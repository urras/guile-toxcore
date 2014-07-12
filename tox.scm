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
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-4)
  #:use-module (system foreign)
  #:use-module ((tox bindings) #:prefix %)
  #:use-module (tox util)
  #:export (tox-client-id-size
            tox-friend-address-size
            make-tox tox-kill
            tox? tox-connected?
            tox-do-interval tox-do
            tox-size tox-save tox-load! tox-load
            client-id tox-bootstrap-from-address
            tox-address))

(define tox-client-id-size 32)

(define tox-friend-address-size
  (+ tox-client-id-size (sizeof uint32) (sizeof uint16)))

(define-wrapped-pointer-type <tox>
  tox? wrap-tox unwrap-tox
  (lambda (tox port)
    (format port "#<<tox> ~x>"
            (pointer-address (unwrap-tox tox)))))

(define-syntax-rule (define/unwrap name docstring proc)
  (define (name tox)
    docstring
    (proc (unwrap-tox tox))))

(define* (make-tox #:optional (ipv6-enabled? #t))
  "Return a newly allocated Tox messenger.  IPV6-ENABLED? indicates whether to
create a IPv4 or IPv6 socket.  By default, an IPv6 socket is created."
  (let ((ptr (%tox-new (boolean->number ipv6-enabled?))))
    (if (null-pointer? ptr)
        (error "Failed to create tox instance")
        (wrap-tox ptr))))

(define/unwrap tox-kill
  "Free all memory associated with the messenger TOX."
  %tox-kill)

(define/unwrap tox-do-interval
  "Return the time in milliseconds before tox-do should be called
again for optimal performance."
  %tox-do-interval)

(define/unwrap tox-do
  "The main loop that needs to be run in intervals of tox-do-interval
milliseconds."
  %tox-do)

(define/unwrap tox-size
  "Return the size of the Tox messenger data in bytes.  Useful for
saving state."
  %tox-size)

(define (tox-save tox)
  "Return a uint8 bytevector containing the state of the messenger
TOX."
  (let ((bv (make-u8vector (tox-size tox))))
    (%tox-save (unwrap-tox tox) (bytevector->pointer bv))
    bv))

(define (tox-load! tox state)
  "Load the saved data in the bytevector STATE into the messenger
TOX."
  (or (zero?
       (%tox-load (unwrap-tox tox)
                  (bytevector->pointer state)
                  (bytevector-length state)))
      (error "Failed to load Tox state: " tox)))

(define (tox-load state)
  "Return a newly allocated Tox messenger loaded from the bytevector
STATE."
  (let ((tox (make-tox)))
    (tox-load! tox state)
    tox))

(define (tox-connected? tox)
  "Return #t if the messenger TOX is connected to the DHT, #f
otherwise."
  (one? (%tox-isconnected (unwrap-tox tox))))

(define (tox-client-id id)
  "Return a newly allocated 32 byte long bytevector by transcoding the
hexadecimal string ID."
  (define (read-byte start)
    (string->number
     (string-append "#x" (substring id start (+ start 2)))))

  (unless (= (string-length id) (* tox-client-id-size 2))
    (error "Invalid Tox client ID: " id))

  (let* ((size (/ (string-length id) 2))
         (bv (make-bytevector size)))
    (let loop ((i 0))
      (when (< i size)
        (bytevector-u8-set! bv i (read-byte (* i 2)))
        (loop (1+ i))))
    bv))

(define (tox-bootstrap-from-address tox address ipv6-enabled? port public-key)
  "Resolve ADDRESS into an IP address.  If successful, send a 'get
nodes' request to the given node with IP, PORT, and PUBLIC-KEY to
setup connections.

ADDRESS can be a hostname or an IP address (IPv4 or IPv6).  If
IPV6-ENABLED? is #f, the resolving sticks strictly to IPv4 addresses.
If IPV6-ENABLED? is #t, the resolving procedure looks for IPv6
addresses first, then IPv4 addresses.  PUBLIC-KEY is a 32 byte long
bytevector.

Return #t if ADDRESS could be converted into an IP address, #f
otherwise."
  (one? (%tox-bootstrap-from-address
         (unwrap-tox tox)
         (string->pointer address)
         (boolean->number ipv6-enabled?)
         (htons port)
         (bytevector->pointer public-key))))

(define (tox-address tox)
  "Return bytevector containing the friend address for the messenger
TOX."
  (let ((bv (make-bytevector tox-friend-address-size)))
    (%tox-get-address (unwrap-tox tox) (bytevector->pointer bv))
    bv))
