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
;; Tox API bindings.
;;
;;; Code:

(define-module (tox bindings)
  #:use-module (system foreign))

(define libtoxcore (dynamic-link "libtoxcore"))

(define-syntax-rule (define-tox name return c-name args)
  (define-public name
    (pointer->procedure return (dynamic-func c-name libtoxcore) args)))

(define-tox tox-new
  '* "tox_new" (list uint8))

(define-tox tox-kill
  void "tox_kill" '(*))
