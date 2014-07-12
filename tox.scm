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
;; Tox API.
;;
;;; Code:

(define-module (tox)
  #:use-module (ice-9 format)
  #:use-module (system foreign)
  #:use-module ((tox bindings) #:prefix %)
  #:use-module (tox util)
  #:export (make-tox tox-kill))

(define-wrapped-pointer-type <tox>
  tox? wrap-tox unwrap-tox
  (lambda (tox port)
    (format port "#<<tox> ~x>"
            (pointer-address (unwrap-tox tox)))))

(define* (make-tox #:optional (ipv6-enabled? #t))
  "Return a newly allocated Tox messenger.  IPV6-ENABLED? indicates whether to
create a IPv4 or IPv6 socket.  By default, an IPv6 socket is created."
  (let ((ptr (%tox-new (boolean->number ipv6-enabled?))))
    (if (null-pointer? ptr)
        (error "Failed to create tox instance")
        (wrap-tox ptr))))

(define (tox-kill tox)
  "Free all memory associated with the messenger TOX."
  (%tox-kill (unwrap-tox tox)))
