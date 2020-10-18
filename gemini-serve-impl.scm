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

(define (status->integer s)
  (cond ((assv s status-codes) => cdr)
        (else (error "unknown status" s))))

;; Read and validate a Gemini request.
(define (read-request)
  (let ((line (read-line)))
    (when (eof-object? line)
      (error "empty request"))
    (when (> (length line) 1024)
      (error "request too long" line))
    line))

(define (write-response-header status meta)
  (fmt #t (status->integer status) " " meta "\r\n"))

;; Serve only files.
(define (simple-handler req)
  (if (not (eqv? (uri-scheme req) 'gemini))
      (write-response-header 'proxy-request-refused
			     "Unhandled protocol")
