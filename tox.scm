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
  #:export (tox-friend-add-error
            tox-user-status
            tox-max-name-length tox-max-message-length
            tox-max-status-message-length
            tox-client-id-size tox-friend-address-size
            tox-client-id tox-friend-address
            make-tox tox-kill with-tox
            tox? tox-connected?
            tox-do-interval tox-do
            tox-size tox-save tox-load! tox-load
            tox-bootstrap-from-address
            tox-address
            tox-add-friend tox-add-friend-no-request tox-delete-friend
            tox-friend-number tox-friend-client-id
            tox-friend-connected? tox-friend-exists?))

(define-enumeration tox-friend-add-error
  (too-long -1)
  (no-message -2)
  (own-key -3)
  (already-sent -4)
  (unknown -5)
  (bad-checksum -6)
  (set-new-no-spam -7)
  (no-mem -8))

(define-enumeration tox-user-status
  (none 0)
  (away 1)
  (busy 2)
  (invalid 3))

(define tox-max-name-length 128)
(define tox-max-message-length 1368)
(define tox-max-status-message-length 1007)
(define tox-client-id-size 32)
(define tox-friend-address-size
  (+ tox-client-id-size (sizeof uint32) (sizeof uint16)))

(define (tox-client-id id)
  "Return a newly allocated bytevector of length tox-client-id-size by
transcoding the hexadecimal string ID."
  (if (= (string-length id) (* tox-client-id-size 2))
      (hex-string->bytevector id)
      (error "Invalid Tox client ID: " id)))

(define (tox-friend-address address)
  "Return a newly allocated bytevector of length tox-friend-address-size by
transcoding the hexadecimal string ADDRESS."
  (if (= (string-length address) (* tox-friend-address-size 2))
      (hex-string->bytevector address)
      (error "Invalid Tox friend address: " address)))

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
        (error "Failed to create Tox messenger")
        (wrap-tox ptr))))

(define/unwrap tox-kill
  "Free all memory associated with the messenger TOX."
  %tox-kill)

(define-syntax-rule (with-tox tox body ...)
  "Evaluate BODY ... and ensure that memory for the messenger TOX is properly
freed when with-tox returns, be it normally or because of an exception."
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      (let ((results (call-with-values (lambda ()  body ...) list)))
        (tox-kill tox)
        (apply values results)))
    (lambda ()
      (tox-kill tox))))

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

(define (tox-add-friend tox address message)
  "Add a friend identified by the bytevector ADDRESS to the messenger TOX.
Additionally, send a friend request containing the string MESSAGE.  ADDRESS
must be tox-friend-address-size bytes long.  Return a friend number on
success, otherwise return a negative number that corresponds to an error code
enumerated in tox-friend-add-error."
  (let ((m (string->utf8 message)))
    (%tox-add-friend (unwrap-tox tox)
                    (bytevector->pointer address)
                    (bytevector->pointer m)
                    (bytevector-length m))))

(define (tox-add-friend-no-request tox client-id)
  "Add a friend identified by the bytevector CLIENT-ID to the messenger TOX
without sending a friend request.  Return the friend ID if successful, or #f
otherwise."
  (false-if-negative
   (%tox-add-friend-norequest (unwrap-tox tox)
                              (bytevector->pointer client-id))))

(define (tox-delete-friend tox friend-number)
  "Remove the friend identified by FRIEND-NUMBER from the messenger TOX.
Return #t if successful, #f otherwise."
  (zero? (%tox-del-friend (unwrap-tox tox) friend-number)))

(define (tox-friend-number tox client-id)
  "Return the friend number associated with the bytevector CLIENT-ID in the
messenger TOX, or #f if no such friend exists."
  (false-if-negative
   (%tox-get-friend-number (unwrap-tox tox)
                           (bytevector->pointer client-id))))

(define (tox-friend-client-id tox friend-number)
  "Return a bytevector containing the public key associated with FRIEND-NUMBER
in the messenger TOX, or #f if no such friend exists."
  (let* ((bv (make-bytevector tox-client-id-size))
         (result (%tox-get-client-id (unwrap-tox tox)
                                     friend-number
                                     (bytevector->pointer bv))))
    (if (negative? result) #f bv)))

(define (tox-friend-connected? tox friend-number)
  "Return #t if friend identified by FRIEND-NUMBER is online, #f otherwise."
  (zero? (%tox-get-friend-connection-status (unwrap-tox tox) friend-number)))

(define (tox-friend-exists? tox friend-number)
  "Return #t if friend identified by FRIEND-NUMBER exists, #f otherwise."
  (one? (%tox-friend-exists (unwrap-tox tox) friend-number)))
