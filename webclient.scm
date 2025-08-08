(use-modules (web client)
             (web uri)
             (web response)
             (srfi srfi-11)
             (sxml ssax)
             ((sxml xpath) #:select (sxpath))
             (sxml simple)
             (ice-9 threads))

;; Shared hashtable and mutex
(define url-cache (make-hash-table))
(define url-mutex (make-mutex))

(define (thread-safe-set! key value)
  (with-mutex url-mutex
    (hash-set! url-cache key value)))

(define (get-url url)
  (let ((res (with-mutex url-mutex
                (hash-ref url-cache url #f))))
    (if (not res) (async-fetch url))
    (if (eq? res 'pending) #f res)))

(define (async-fetch url)
  (thread-safe-set! url 'pending)
  (call-with-new-thread
   (lambda ()
     (let-values (((response body) (http-get-follow-redirect url 5)))
       (thread-safe-set! url body)))))

(define (get-url-with-proc url proc)
  (let ((res (with-mutex url-mutex
                (hash-ref url-cache url #f))))
    (if (not res) (async-fetch-with-proc url proc))
    (if (eq? res 'pending) #f res)))

(define (async-fetch-with-proc url proc)
  (thread-safe-set! url 'pending)
  (call-with-new-thread
   (lambda ()
     (let-values (((response body) (http-get-follow-redirect url 5)))
       (thread-safe-set! url (proc body))))))

(define (resolve-uri loc-uri base)
  (let ((base-uri (string->uri base)))
    (cond
     ;; Absolute URI already — just return as string
     ((uri? loc-uri)
      location)
     ;; Relative — combine with base
     ((relative-ref? loc-uri)
      (uri->string
       (build-uri
        (uri-scheme base-uri)
        #:host (uri-host base-uri)
        #:port (uri-port base-uri)
        #:path (uri-path loc-uri)    ;; naive, doesn't handle '../' or ''
        #:query (uri-query loc-uri)
        #:fragment (uri-fragment loc-uri))))
     (else
      (error "Could not parse location header" location)))))

(define (http-get-follow-redirect url max)
  (define (helper url redirects-left)
    (if (<= redirects-left 0)
        (error "Too many redirects" url)
        (let-values (((resp body) (http-get url)))
          (let ((code (response-code resp)))
            (cond
             ((and (>= code 300) (< code 400))
              ;; Redirect — follow Location header
              (let ((location (assoc-ref (response-headers resp) 'location)))
                (if location
                    (let ((new-url (resolve-uri location url))) ;; resolve relative URLs
                      (helper new-url (- redirects-left 1)))
                    (error "Redirect without Location header" resp))))
             (else
              ;; Success or error — return as-is
              (values resp body)))))))
  (helper url max))

(define (parse-ssax str)
  (call-with-output-file "/dev/null"
    (lambda (null-port)
      (with-ssax-error-to-port null-port
        (lambda ()
          (ssax:xml->sxml (open-input-string str) '()))))))
