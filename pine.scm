(module pine (serve
              send-response
              send-headers
              send-header
              file-handler
              handles
              handle
              GET
              POST
              PUT
              DELETE
              HEAD)

(import chicken
        scheme
        foreign)

(use
 matchable
 (srfi 18 69)
 utils)

(foreign-declare "#include <fcgiapp.h>")

(define-foreign-type fcgx-request (c-pointer "FCGX_Request"))

(define fcgx-accept
  (foreign-lambda int "FCGX_Accept_r" fcgx-request))

(define fcgx-init
  (foreign-lambda void "FCGX_Init"))

(define fcgx-init-request
  (foreign-lambda* fcgx-request ()
                   "FCGX_Request request;
                    FCGX_InitRequest(&request, 0, 0);
                    C_return(&request);"))

(define fcgx-finish-request
  (foreign-lambda void "FCGX_Finish_r" fcgx-request))

(define fcgx-params
  (foreign-lambda* c-string-list ((fcgx-request request))
                   "C_return(request->envp);"))

(define fcgx-get-param
  (foreign-lambda* c-string ((fcgx-request request)
                             (c-string key))
                   "C_return(FCGX_GetParam(key, request->envp));"))

(define fcgx-print
  (foreign-lambda* void ((fcgx-request request)
                         (c-string data))
                   "FCGX_FPrintF(request->out, data);"))

(define (opt-arg args n default)
  (if (< n (length args))
      (list-ref args n)
      default))

(define (send-header req name value)
  (let ((fmt "~A: ~A\r\n")
        (value (car value)))
    (fcgx-print req (sprintf "~A: ~A\r\n" name value))))

(define (send-headers req headers)
  (hash-table-walk headers (lambda (key val) (send-header req key val))))

(define default-headers
  '(("Status" "200 OK")
    ("Content-Type" "text/html")))

(define (set-default-headers headers default-headers)
  (match default-headers
         [((header value) . default-headers)
          (begin
            ; Don't overwrite existing values
            (if (not (hash-table-exists? headers header))
                (hash-table-set! headers header value))
            (set-default-headers headers default-headers))]
         [() headers]))

(define (send-response . args)
  (let ((req (opt-arg args 0 '()))
        (body (opt-arg args 1 ""))
        (headers (opt-arg args 2 (alist->hash-table default-headers)))
        (headers-included (null? (opt-arg args 2 '()))))
    (begin
      (if (not headers-included)
          (set-default-headers headers default-headers))
      (hash-table-set! headers "Content-Length" `(,(string-length body)))
      (send-headers req headers)
      (fcgx-print req (sprintf "\r\n~A\r\n\r\n" body)))))

(define (route-request req handlers)
  (let ((path (fcgx-get-param req "DOCUMENT_URI"))
        (method (fcgx-get-param req "REQUEST_METHOD")))
    (begin
      (if (and (hash-table-exists? handlers path)
               (hash-table-exists? (hash-table-ref handlers path) method))
          ((hash-table-ref (hash-table-ref handlers path) method) req)
          (send-response req "404 Not found"))
      (fcgx-finish-request req))))

(define (serve-thread handlers)
  (let ((req (fcgx-init-request)))
    (begin
      (if (>= (fcgx-accept req) 0)
          (route-request req handlers)
          (fcgx-finish-request req))
      (serve handlers))))

(define (serve-threads n handlers)
  (if (= n 0)
      '()
      (begin
        (thread-start! (lambda () (serve-thread handlers)))
        (serve-threads (- n 1) handlers))))

(define (serve handlers)
  (begin
    (fcgx-init)
    (serve-threads 10 handlers)
    (define loop-forever (lambda () (loop-forever)))
    (loop-forever)))

; Utils

(define (file-handler page)
  (lambda (req)
    ; TODO: file-caching
    (send-response req (read-all page))))

(define (handle path methods handler)
  (let ((ht (make-hash-table)))
    (begin
      (define (add-handle methods)
        (match methods
               [method (hash-table-set! ht method handler)]
               [() '()]
               [(method . methods)
                (begin
                  (hash-table-set! ht method handler)
                  (add-handle methods))]))
      (add-handle methods)
      `(,path ,ht))))

(define (handles . args)
  (let ((ht (make-hash-table)))
    (begin
      (define (add-handles handles)
        (match handles
               [() '()]
               [((path method-handles) . handles)
                (begin
                  (hash-table-set! ht path method-handles)
                  (add-handles handles))]))
      (add-handles args)
      ht)))

(define GET "GET")
(define POST "POST")
(define PUT "PUT")
(define DELETE "DELETE")
(define HEAD "HEAD")

) ; End module
