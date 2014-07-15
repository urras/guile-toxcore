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
            tox-friend-request-hook tox-message-hook tox-action-hook
            tox-name-change-hook tox-status-message-hook tox-status-hook
            tox-typing-hook tox-read-receipt-hook tox-online-hook
            tox? tox-connected?
            tox-do-interval tox-do
            tox-size tox-save tox-load! tox-load
            tox-bootstrap-from-address
            tox-address
            tox-add-friend tox-add-friend-no-request tox-delete-friend
            tox-friend-number tox-friend-client-id
            tox-friend-connected? tox-friend-exists?
            tox-send-message tox-send-action
            set-tox-name tox-name tox-friend-name
            set-tox-status set-tox-status-message
            tox-status-message tox-friend-status-message
            tox-status tox-friend-status
            tox-friend-last-online
            set-tox-friend-typing tox-friend-typing?
            set-tox-send-receipts
            tox-friend-count tox-online-friend-count
            tox-friend-list))

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
  tox? %wrap-tox unwrap-tox
  (lambda (tox port)
    (format port "#<<tox> ~x>"
            (pointer-address (unwrap-tox tox)))))

(define-syntax-rule (define-tox-hook name)
  (define name (make-hook 3)))

(define-tox-hook tox-friend-request-hook)

(define friend-request-callback
  (procedure->pointer
   void
   (lambda (tox public-key message length user-data)
     (run-hook tox-friend-request-hook
               (%wrap-tox tox)
               (pointer->bytevector public-key tox-client-id-size)
               (utf8-pointer->string message length)))
   (list '* '* '* uint16 '*)))

(define-tox-hook tox-message-hook)

(define friend-message-callback
  (procedure->pointer
   void
   (lambda (tox friend-number message length user-data)
     (run-hook tox-message-hook
               (%wrap-tox tox)
               friend-number
               (utf8-pointer->string message length)))
   (list '* int32 '* uint16 '*)))

(define-tox-hook tox-action-hook)

(define friend-action-callback
  (procedure->pointer
   void
   (lambda (tox friend-number action length user-data)
     (run-hook tox-action-hook
               (%wrap-tox tox)
               friend-number
               (utf8-pointer->string action length)))
   (list '* int32 '* uint16 '*)))

(define-tox-hook tox-name-change-hook)

(define name-change-callback
  (procedure->pointer
   void
   (lambda (tox friend-number name length user-data)
     (run-hook tox-name-change-hook
               (%wrap-tox tox)
               friend-number
               (utf8-pointer->string name length)))
   (list '* int32 '* uint16 '*)))

(define-tox-hook tox-status-message-hook)

(define status-message-callback
  (procedure->pointer
   void
   (lambda (tox friend-number message length user-data)
     (run-hook tox-status-message-hook
               (%wrap-tox tox)
               friend-number
               (utf8-pointer->string message length)))
   (list '* int32 '* uint16 '*)))

(define-tox-hook tox-status-hook)

(define user-status-callback
  (procedure->pointer
   void
   (lambda (tox friend-number status user-data)
     (run-hook tox-status-hook
               (%wrap-tox tox)
               friend-number
               status))
   (list '* int32 uint8 '*)))

(define-tox-hook tox-typing-hook)

(define typing-change-callback
  (procedure->pointer
   void
   (lambda (tox friend-number typing user-data)
     (run-hook tox-typing-hook
               (%wrap-tox tox)
               friend-number
               (one? typing)))
   (list '* int32 uint8 '*)))

(define-tox-hook tox-read-receipt-hook)

(define read-receipt-callback
  (procedure->pointer
   void
   (lambda (tox friend-number receipt user-data)
     (run-hook tox-read-receipt-hook
               (%wrap-tox tox)
               friend-number
               receipt))
   (list '* int32 int32 '*)))

(define-tox-hook tox-online-hook)

(define connection-status-callback
 (procedure->pointer
     void
     (lambda (tox friend-number status user-data)
       (run-hook tox-online-hook
                 (%wrap-tox tox)
                 friend-number
                 (one? status)))
     (list '* int32 uint8 '*)))

(define (wrap-tox pointer)
  ;; Register all callback functions.
  (%tox-callback-friend-request pointer
                                friend-request-callback
                                %null-pointer)
  (%tox-callback-friend-message pointer
                                friend-message-callback
                                %null-pointer)
  (%tox-callback-friend-action pointer
                               friend-action-callback
                               %null-pointer)
  (%tox-callback-name-change pointer
                             name-change-callback
                             %null-pointer)
  (%tox-callback-status-message pointer
                                status-message-callback
                                %null-pointer)
  (%tox-callback-user-status pointer
                             user-status-callback
                             %null-pointer)
  (%tox-callback-typing-change pointer
                               typing-change-callback
                               %null-pointer)
  (%tox-callback-read-receipt pointer
                              read-receipt-callback
                              %null-pointer)
  (%tox-callback-connection-status pointer
                                   connection-status-callback
                                   %null-pointer)
  (%wrap-tox pointer))

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

(define (tox-load tox state)
  "Load the saved data in the bytevector STATE into the messenger
TOX."
  (or (zero?
       (%tox-load (unwrap-tox tox)
                  (bytevector->pointer state)
                  (bytevector-length state)))
      (error "Failed to load Tox state: " tox)))

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
without sending a friend request.  Return the friend number if successful, or
#f otherwise."
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
  (one? (%tox-get-friend-connection-status (unwrap-tox tox) friend-number)))

(define (tox-friend-exists? tox friend-number)
  "Return #t if friend identified by FRIEND-NUMBER exists, #f otherwise."
  (one? (%tox-friend-exists (unwrap-tox tox) friend-number)))

(define (tox-send tox send send-with-id friend-number message id)
  (let* ((tox (unwrap-tox tox))
         (message (string->utf8 message))
         (ptr (bytevector->pointer message))
         (length (bytevector-length message)))
    (false-if-zero
     (if id
         (send-with-id tox friend-number id ptr length)
         (send tox friend-number ptr length)))))

(define* (tox-send-message tox friend-number message #:optional (id #f))
  "Send the string MESSAGE to the friend identified by FRIEND-NUMBER in the
messenger TOX.  Optionally, a message ID may be given.  If omitted, an id is
automatically generated.  MESSAGE length may not exceed
tox-max-message-length.

Return the message id on success, #f otherwise."
  (tox-send tox
            %tox-send-message
            %tox-send-message-withid
            friend-number
            message
            id))

(define* (tox-send-action tox friend-number action #:optional (id #f))
  "Send the string ACTION to the friend identified by FRIEND-NUMBER in the
messenger TOX.  Optionally, a message ID may be given.  If omitted, an id is
automatically generated.  MESSAGE length may not exceed
tox-max-message-length.

Return the message id on success, #f otherwise."
  (tox-send tox
            %tox-send-action
            %tox-send-action-withid
            friend-number
            action
            id))

(define (set-tox-name tox name)
  "Use the nickname NAME for the messenger TOX."
  (let ((n (string->utf8 name)))
    (if (zero? (%tox-set-name (unwrap-tox tox)
                              (bytevector->pointer n)
                              (bytevector-length n)))
        *unspecified*
        (error "Invalid nickname: " name))))

(define (tox-name tox)
  "Return the nickname for the messenger TOX."
  (let* ((name (make-bytevector tox-max-name-length))
         (length (%tox-get-self-name (unwrap-tox tox)
                                     (bytevector->pointer name))))
    (if (positive? length)
        (utf8->string (bytevector-slice name 0 length))
        (error "Failed to get nickname"))))

(define (tox-friend-name tox friend-number)
  "Return the nickname of the friend identified by FRIEND-NUMBER for the
messenger TOX."
  (let* ((name (make-bytevector tox-max-name-length))
         (length (%tox-get-name (unwrap-tox tox)
                                friend-number
                                (bytevector->pointer name))))
    (if (positive? length)
        (utf8->string (bytevector-slice name 0 length))
        (error "Failed to get nickname for friend number: " friend-number))))

(define (set-tox-status tox status)
  "Set the user status for the messenger TOX to STATUS."
  (when (negative? (%tox-set-user-status (unwrap-tox tox) status))
    (error "Invalid user status: " status)))

(define (set-tox-status-message tox message)
  "Set the status message for the messenger TOX to the string MESSAGE."
  (let ((m (string->utf8 message)))
    (when (negative?
           (%tox-set-status-message (unwrap-tox tox)
                                    (bytevector->pointer m)
                                    (bytevector-length m)))
      (error "Invalid status message: " message))))

(define (tox-status-message tox)
  "Return the status message for the messenger TOX."
  (let* ((message (make-bytevector tox-max-status-message-length))
         (length (%tox-get-self-status-message (unwrap-tox tox)
                                               (bytevector->pointer message)
                                               tox-max-status-message-length)))
    (if (positive? length)
        (utf8->string (bytevector-slice message 0 length))
        (error "Failed to get status message"))))

(define (tox-friend-status-message tox friend-number)
  "Return the status message for the friend identified by FRIEND-NUMBER i the
messenger TOX."
  (let* ((message (make-bytevector tox-max-status-message-length))
         (length (%tox-get-status-message (unwrap-tox tox)
                                          friend-number
                                          (bytevector->pointer message)
                                          tox-max-status-message-length)))
    (if (positive? length)
        (utf8->string (bytevector-slice message 0 length))
        (error "Failed to get status message for friend number: "
               friend-number))))

(define/unwrap tox-status
  "Return the user status code for the messenger TOX."
  %tox-get-self-user-status)

(define (tox-friend-status tox friend-number)
  "Return the user status code for the friend identified by FRIEND-NUMBER in
the messenger TOX."
  (%tox-get-user-status (unwrap-tox tox) friend-number))

(define (tox-friend-last-online tox friend-number)
  "Return the timestamp of the last time the friend identified by
FRIEND-NUMBER was seen online, or 0 if never seen."
  (let ((result (%tox-get-last-online (unwrap-tox tox) friend-number)))
    (if (negative? result)
        (error "Invalid friend number: " friend-number)
        result)))

(define (set-tox-friend-typing tox friend-number typing?)
  "Set the typing flag for the friend identified by FRIEND-NUMBER in the
messenger TOX."
  (if (zero? (%tox-set-user-is-typing (unwrap-tox tox) friend-number typing?))
      *unspecified*
      (error "Invalid friend number: " friend-number)))

(define (tox-friend-typing? tox friend-number)
  "Return #t if the friend identified by FRIEND-NUMBER in the messenger TOX is
typing, or #f otherwise."
  (one? (%tox-get-is-typing (unwrap-tox tox) friend-number)))

(define (set-tox-send-receipts tox friend-number send-receipts?)
  "Set whether to send receipts to the friend identified by FRIEND-NUMBER in
the messenger TOX.  SEND-RECEIPTS? should be either #t of #f."
  (%tox-set-sends-receipts (unwrap-tox tox)
                           friend-number
                           (boolean->number send-receipts?)))

(define/unwrap tox-friend-count
  "Return the number of friends in the friend list for the messenger TOX."
  %tox-count-friendlist)

(define/unwrap tox-online-friend-count
  "Return the number of online friends in the friend list for the messenger
TOX."
  %tox-get-num-online-friends)

(define (tox-friend-list tox)
  "Return a list of all friend numbers for the messenger TOX."
  (let* ((length (tox-friend-count tox))
         (bv (make-s32vector length)))
    (%tox-get-friendlist (unwrap-tox tox)
                         (bytevector->pointer bv)
                         length)
    (bytevector->sint-list bv (native-endianness) (sizeof int32))))
