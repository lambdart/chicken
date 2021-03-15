;; import modules
(import
  scheme
  apropos
  srfi-1
  srfi-18
  (chicken base)
  (chicken tcp)
  (chicken file)
  (chicken file posix)
  (chicken format)
  (chicken condition)
  (chicken port)
  (chicken string)
  (chicken repl)
  (chicken syntax)
  chicken-doc
  compile-file)

(define (chicken-all-completions prefix)
  (let ((size (string-length prefix)))
    (filter
     (lambda (completions)
       (substring=? prefix completions 0 0 size))
     (map (o symbol->string car)
          (##sys#current-environment)))))

(define (chicken-apropos-completions prefix)
  (let ((completions (apropos-list `(: bos ,prefix) #:macros? #t)))
    (remove
     (lambda (completions)
       (substring-index "#" completions))
     (map symbol->string completions))))

(define (chicken-completions prefix . rest)
  (append (chicken-apropos-completions prefix)
          (chicken-all-completions prefix)))

;; trace

