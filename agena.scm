(import (scheme)
        (chicken base)
        (chicken module)
        (only (chicken io) read-line)
        (only (chicken pathname) make-pathname pathname-extension)
        (only (chicken process-context) current-directory)
        (only (chicken file posix) regular-file?)
        (only (srfi 13) string-join)
        (srfi 4)
        (fmt)
        (uri-generic))

(include "mime-types.scm")

(define buffer-size 4096)

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
  (display "listening...\n")
  (let ((line (read-line)))
    (when (eof-object? line)
      (error "empty request"))
    (when (> (string-length line) 1024)
      (error "request too long" line))
    line))

(define (write-response-header status meta)
  (fmt #t (status->integer status) " " meta "\r\n"))

(define (serve-file path)
  (if (regular-file? path)
      (serve-regular-file path)
      (write-response-header 'not-found "File not found")))

;; Write all data from port to the current output.
(define (write-all port)
  (let* ((buffer (make-u8vector buffer-size))
         (read-bytes (lambda () (read-u8vector! #f buffer port))))
    (let lp ((k (read-bytes)))
      (write-u8vector buffer (current-output-port) 0 k)
      (unless (< k buffer-size)
        (lp (read-bytes))))))

(define (serve-regular-file path)
  (write-response-header 'success
                         (extension-mime-type (pathname-extension path)))
  (let ((port (open-input-file path)))
    (write-all port)
    (close-input-port port)))

(define (simple-handler uri)
  (if (not (eqv? (uri-scheme uri) 'gemini))
      (write-response-header 'proxy-request-refused
			     "Unhandled protocol")
      (serve-file (make-path (cdr (uri-path uri))))))

(define (make-path ps)
  (string-join (cons (root-path) ps) "/"))

(define root-path
  (make-parameter (make-pathname (current-directory) "root")))
