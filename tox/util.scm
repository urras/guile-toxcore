;;; guile-toxcore
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;;
;;; guile-toxcore is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation, either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; guile-toxcore is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;; for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Utility procedures.
;;
;;; Code:

(define-module (tox util)
  #:use-module (rnrs bytevectors)
  #:export (boolean->number
            one?
            define-enumeration
            hex-string->bytevector
            false-if-negative))

(define (boolean->number true?)
  "Return 1 if TRUE? is #t, 0 otherwise."
  (if true? 1 0))

(define (one? n)
  "Return #t if N is equal to 1, #f otherwise."
  (= n 1))

;; Borrowed from guile-opengl
(define-syntax-rule (define-enumeration enumerator (name value) ...)
  (define-syntax enumerator
    (lambda (x)
      (syntax-case x ()
        ((_)
         #''(name ...))
        ((_ enum) (number? (syntax->datum #'enum))
         #'enum)
        ((_ enum)
         (or (assq-ref '((name . value) ...)
                       (syntax->datum #'enum))
             (syntax-violation 'enumerator "invalid enumerated value"
                               #'enum)))))))

(define (hex-string->bytevector str)
  "Return a newly allocated bytevector of LENGTH bytes containing the binary
representation of the hexadecimal encoded string STR.  The length of STR must
be even."
  (define (read-byte start)
    (string->number
     (string-append "#x" (substring str start (+ start 2)))))

  (let* ((size (/ (string-length str) 2))
         (bv (make-bytevector size)))
    (let loop ((i 0))
      (when (< i size)
        (bytevector-u8-set! bv i (read-byte (* i 2)))
        (loop (1+ i))))
    bv))

(define (false-if-negative n)
  "Return #f is N is negative, or N otherwise."
  (if (negative? n) #f n))
