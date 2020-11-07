(import (scheme)
        (chicken base)
        (chicken condition)
        (chicken module)
        (only (chicken tcp) tcp-listen)
        (chicken time)
        (chicken time posix)
        (chicken io)
        (chicken pathname)
        (chicken process)
        (chicken process-context)
        (chicken process-context posix)
        (chicken file posix)
        (only (srfi 13) string-null? string-join)
        (srfi 4)
        (args)
        (fmt)
        (tcp-server)
        (uri-generic))

(include "mime-types.scm")

(define buffer-size 4096)

(define log-timestamp-format "%Y-%m-%d %H:%M:%SZ")

(define server-uid #f)
(define server-gid #f)

;;;; Logging

(define (write-log msg . objs)
  (let* ((tv (seconds->utc-time (current-seconds)))
         (stamp (time->string tv log-timestamp-format)))
    (parameterize ((current-output-port (current-error-port)))
      (if (pair? objs)
          (fmt #t "[" stamp "] " msg " | irritants: " objs nl)
          (fmt #t "[" stamp "] " msg nl)))))

;;;; Gemini

;; Snarfed from Kooda's geminid.
(define status-codes
  '((input                       . 10)
    (sensitive-input             . 11)
    (success                     . 20)
    (redirect                    . 30)
    (redirect-temporary          . 30)
    (redirect-permanent          . 31)
    (temporary-failure           . 40)
    (server-unavailable          . 41)
    (cgi-error                   . 42)
    (proxy-error                 . 43)
    (slow-down                   . 44)
    (permanent-failure           . 50)
    (not-found                   . 51)
    (gone                        . 52)
    (proxy-request-refused       . 53)
    (bad-request                 . 59)
    (client-certificate-required . 60)
    (certificate-not-authorised  . 61)
    (certificate-not-valid       . 62)))

(define mime-type-fallback "application/octet-stream")

(define (extension-mime-type ext)
  (cond ((assoc ext mime-types) => cdr)
        (else mime-type-fallback)))

(define (status->integer s)
  (cond ((assv s status-codes) => cdr)
        (else (error "unknown status" s))))

;; Read and validate a Gemini request.
(define (read-request)
  (let ((line (read-line)))
    (cond ((eof-object? line)
           (write-log "empty request")
           #f)
          ((> (string-length line) 1024)
           (write-log "overlong request")
           #f)
          (else line))))

(define (write-response-header status meta)
  (let ((code (status->integer status)))
    (display code)
    (write-char #\space)
    (write-string meta)
    (write-string "\r\n")
    (unless (and (>= code 20) (< code 30))
      (flush-output)
      #;(close-output-port (current-output-port)))))

(define (serve-failure path)
  (write-log "serve file failed" path)
  (write-response-header 'not-found "File not found"))

(define (serve-file root-path ps)
  (let ((path (if (null? ps)
                  root-path
                  (string-join (cons root-path (cdr ps)) "/"))))
    (cond ((regular-file? path) (serve-regular-file path))
          ((directory? path)
           (serve-regular-file (make-pathname path "index.gmi")))
          (else (serve-failure path)))))

;; Write all data from port to the current output.
(define (write-all port)
  (let* ((buffer (make-u8vector buffer-size))
         (read-bytes (lambda () (read-u8vector! #f buffer port))))
    (let lp ((k (read-bytes)))
      (write-u8vector buffer (current-output-port) 0 k)
      (unless (< k buffer-size)
        (lp (read-bytes))))))

(define (serve-regular-file path)
  (call-with-current-continuation
   (lambda (k)
     (let ((port
            (condition-case (open-input-file path)
              ((exn file) (serve-failure path) (k #f)))))
       (write-response-header 'success
                              (extension-mime-type (pathname-extension path)))
       (write-all port)
       (close-input-port port)))))

(define (simple-handler root-path uri)
  ;; TODO: Validate host.
  (if (not (eqv? (uri-scheme uri) 'gemini))
      (begin
       (write-log "unhandled protocol" (uri-scheme uri))
       (write-response-header 'proxy-request-refused
                              "Unhandled protocol"))
      (serve-file root-path (uri-path uri))))

(define (make-request-handler root-path)
  (lambda ()
    (write-log "got request")
    (and-let* ((line (read-request))
               (uri (uri-reference line)))
      (simple-handler root-path uri))))

;;;; Server

(define (run root-path port)
  (let* ((listener (tcp-listen port))
         (serve (make-tcp-server listener (make-request-handler root-path))))
    (and server-uid (set! (current-user-id) server-uid))
    (and server-gid (set! (current-group-id) server-gid))
    (serve)))

(define (daemon-run)
  (change-directory (root-path))
  ;(unveil (root-path) "r")
  ;(unveil-lock)
  (file-creation-mode 0)
  (close-output-port (current-output-port))
  (close-input-port (current-input-port))
  (run))

(letrec* ((opts (list (args:make-option (D) #:none "Daemonize.")
                      (args:make-option (p) (#:required "<port>")
                       "Listen on <port> (default: 1965)")
                      (args:make-option (h) #:none
                       "Show this text."
                       (usage))))
          (usage (lambda ()
                   (parameterize ((current-output-port (current-error-port)))
                     (display "Usage: agena [options...] <directory>\n")
                     (display (args:usage opts))
                     (exit 1)))))
  (let-values (((cli-opts operands) (args:parse (command-line-arguments) opts)))
    (let ((root-path (cond ((= (length operands) 1) (car operands))
                           (else (usage))))
          (listen-port (cond ((assv 'p cli-opts) => cdr)
                             (else 1965))))
      (cond ((assv 'D cli-opts)
             ;; TODO: Handle fork errors.
             (process-fork (lambda () (daemon-run root-path listen-port)))
             (exit 0))
            (else (run root-path listen-port))))))
