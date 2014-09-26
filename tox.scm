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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4)
  #:use-module (system foreign)
  #:use-module ((tox bindings) #:prefix %)
  #:use-module (tox util)
  #:export (tox-friend-add-error
            tox-user-status tox-chat-event tox-file-control
            tox-max-name-length tox-max-message-length
            tox-max-status-message-length
            tox-client-id-size tox-friend-address-size
            tox-client-id tox-friend-address
            make-tox tox-kill with-tox
            tox-friend-request-hook tox-message-hook tox-action-hook
            tox-name-change-hook tox-status-message-hook tox-status-hook
            tox-typing-hook tox-read-receipt-hook tox-online-hook
            tox-group-invite-hook tox-group-message-hook
            tox-group-action-hook tox-group-peer-hook
            tox-file-send-request-hook tox-file-control-hook tox-file-data-hook
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
            tox-friend-count tox-online-friend-count
            tox-friend-list
            set-tox-nospam tox-nospam
            tox-add-group-chat tox-delete-group-chat
            tox-group-peer-name tox-invite-friend
            tox-join-group-chat
            tox-group-send-message tox-group-send-action
            tox-group-peer-count tox-group-peer-names
            tox-group-chat-count tox-group-chat-list))

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

(define-enumeration tox-chat-event
  (peer-add 0)
  (peer-delete 1)
  (peer-name 2))

(define-enumeration tox-file-control
  (accept 0)
  (pause 1)
  (kill 2)
  (finished 3)
  (resume-broken 4))

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

;;;
;;; Hooks
;;;

(define-syntax define-tox-hook
  (syntax-rules ()
    ((_ name)
     (define name (make-hook 3)))
    ((_ name arity)
     (define name (make-hook arity)))))

(define-syntax-rule (define-tox-callback (name (type arg) ...) body ...)
  (define name
    (procedure->pointer void
                        (lambda (arg ...)
                          body ...)
                        (list type ...))))

(define-tox-hook tox-friend-request-hook)

(define-tox-callback (friend-request-callback ('* tox) ('* public-key)
                                              ('* message) (uint16 length)
                                              ('* user-data))
  (run-hook tox-friend-request-hook
            (%wrap-tox tox)
            (pointer->bytevector public-key tox-client-id-size)
            (utf8-pointer->string message length)))

(define-tox-hook tox-message-hook)

(define-tox-callback (friend-message-callback ('* tox) (int32 friend-number)
                                              ('* message) (uint16 length)
                                              ('* user-data))
  (run-hook tox-message-hook
            (%wrap-tox tox)
            friend-number
            (utf8-pointer->string message length)))

(define-tox-hook tox-action-hook)

(define-tox-callback (friend-action-callback ('* tox) (int32 friend-number)
                                             ('* action) (uint16 length)
                                             ('* user-data))
  (run-hook tox-action-hook
            (%wrap-tox tox)
            friend-number
            (utf8-pointer->string action length)))

(define-tox-hook tox-name-change-hook)

(define-tox-callback (name-change-callback ('* tox) (int32 friend-number)
                                           ('* name) (uint16 length)
                                           ('* user-data))
  (run-hook tox-name-change-hook
            (%wrap-tox tox)
            friend-number
            (utf8-pointer->string name length)))

(define-tox-hook tox-status-message-hook)

(define-tox-callback (status-message-callback ('* tox) (int32 friend-number)
                                              ('* message) (uint16 length)
                                              ('* user-data))
  (run-hook tox-status-message-hook
            (%wrap-tox tox)
            friend-number
            (utf8-pointer->string message length)))

(define-tox-hook tox-status-hook)

(define-tox-callback (user-status-callback ('* tox) (int32 friend-number)
                                           (uint8 status) ('* user-data))
  (run-hook tox-status-hook
            (%wrap-tox tox)
            friend-number
            status))

(define-tox-hook tox-typing-hook)

(define-tox-callback (typing-change-callback ('* tox) (int32 friend-number)
                                             (uint8 typing) ('* user-data))
  (run-hook tox-typing-hook
            (%wrap-tox tox)
            friend-number
            (one? typing)))

(define-tox-hook tox-read-receipt-hook)

(define-tox-callback (read-receipt-callback ('* tox) (int32 friend-number)
                                            (int32 receipt) ('* user-data))
  (run-hook tox-read-receipt-hook
            (%wrap-tox tox)
            friend-number
            receipt))

(define-tox-hook tox-online-hook)

(define-tox-callback (connection-status-callback ('* tox) (int32 friend-number)
                                                 (uint8 status) ('* user-data))
 (run-hook tox-online-hook
           (%wrap-tox tox)
           friend-number
           (one? status)))

(define-tox-hook tox-group-invite-hook)

(define-tox-callback (group-invite-callback ('* tox) (int friend-number)
                                            ('* group-public-key)
                                            ('* user-data))
  (run-hook tox-group-invite-hook
            (%wrap-tox tox)
            friend-number
            (pointer->bytevector group-public-key tox-friend-address-size)))

(define-tox-hook tox-group-message-hook 4)

(define-tox-callback (group-message-callback ('* tox) (int group-number)
                                             (int peer-number)
                                             ('* message) (uint16 length)
                                             ('* user-data))
  (run-hook tox-group-message-hook
            (%wrap-tox tox)
            group-number
            peer-number
            (utf8-pointer->string message length)))

(define-tox-hook tox-group-action-hook 4)

(define-tox-callback (group-action-callback ('* tox) (int group-number)
                                            (int peer-number)
                                            ('* action) (uint16 length)
                                            ('* user-data))
  (run-hook tox-group-action-hook
            (%wrap-tox tox)
            group-number
            peer-number
            (utf8-pointer->string action length)))

(define-tox-hook tox-group-peer-hook 4)

(define-tox-callback (group-peer-callback ('* tox) (int group-number)
                                          (int peer-number)
                                          (int event) ('* user-data))
  (run-hook tox-group-peer-hook
            (%wrap-tox tox)
            group-number
            peer-number
            event))

(define-tox-hook tox-file-send-request-hook 5)

(define-tox-callback (file-send-request-callback ('* tox) (int32 friend-number)
                                                 (uint8 file-number)
                                                 (uint64 file-size)
                                                 ('* file-name)
                                                 (uint16 file-name-length)
                                                 ('* user-data))
  (run-hook tox-file-send-request-hook
            (%wrap-tox tox)
            friend-number
            file-number
            file-size
            (utf8-pointer->string file-name file-name-length)))

(define-tox-hook tox-file-control-hook 6)

(define-tox-callback (file-control-callback ('* tox) (int32 friend-number)
                                            (uint8 mode) (uint8 file-number)
                                            (uint8 event)
                                            ('* data) (uint16 length)
                                            ('* user-data))
  (run-hook tox-file-control-hook
            (%wrap-tox tox)
            friend-number
            file-number
            (if (zero? mode) 'receive 'send)
            event
            (pointer->bytevector data length)))

(define-tox-hook tox-file-data-hook 4)

(define-tox-callback (file-data-callback ('* tox) (int32 friend-number)
                                         (uint8 file-number)
                                         ('* data) (uint16 length)
                                         ('* user-data))
  (run-hook tox-file-data-hook
            (%wrap-tox tox)
            friend-number
            file-number
            (bytevector->pointer data length)))

(define (wrap-tox pointer)
  (define-syntax-rule (register-callbacks ((set-callback callback) ...))
    (begin
      (set-callback pointer callback %null-pointer)
      ...))

  (register-callbacks
   ((%tox-callback-friend-request friend-request-callback)
    (%tox-callback-friend-message friend-message-callback)
    (%tox-callback-friend-action friend-action-callback)
    (%tox-callback-name-change name-change-callback)
    (%tox-callback-status-message status-message-callback)
    (%tox-callback-user-status user-status-callback)
    (%tox-callback-typing-change typing-change-callback)
    (%tox-callback-read-receipt read-receipt-callback)
    (%tox-callback-connection-status connection-status-callback)
    (%tox-callback-group-invite group-invite-callback)
    (%tox-callback-group-message group-message-callback)
    (%tox-callback-group-action group-action-callback)
    (%tox-callback-group-namelist-change group-peer-callback)
    (%tox-callback-file-send-request file-send-request-callback)
    (%tox-callback-file-control file-control-callback)
    (%tox-callback-file-data file-data-callback)))
  (%wrap-tox pointer))

;;;
;;; Core API
;;;

(define-syntax-rule (define/unwrap name docstring proc)
  (define (name tox)
    docstring
    (proc (unwrap-tox tox))))

(define (make-tox)
  "Return a newly allocated Tox messenger object."
  (let ((ptr (%tox-new %null-pointer)))
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

(define (tox-bootstrap-from-address tox address port public-key)
  "Resolve ADDRESS into an IP address.  If successful, send a 'get
nodes' request to the given node with IP, PORT, and PUBLIC-KEY to
setup connections.

ADDRESS can be a hostname or an IP address.
PUBLIC-KEY is a 32 byte long bytevector.

Return #t if ADDRESS could be converted into an IP address, #f
otherwise."
  (one? (%tox-bootstrap-from-address
         (unwrap-tox tox)
         (string->pointer address)
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

(define (tox-send tox send friend-number message)
  (let* ((tox (unwrap-tox tox))
         (message (string->utf8 message))
         (ptr (bytevector->pointer message))
         (length (bytevector-length message)))
    (false-if-zero
     (send tox friend-number ptr length))))

(define (tox-send-message tox friend-number message)
  "Send the string MESSAGE to the friend identified by FRIEND-NUMBER in the
messenger TOX.  MESSAGE length may not exceed tox-max-message-length.

Return the message id on success, #f otherwise."
  (tox-send tox
            %tox-send-message
            friend-number
            message))

(define (tox-send-action tox friend-number action)
  "Send the string ACTION to the friend identified by FRIEND-NUMBER in the
messenger TOX.  MESSAGE length may not exceed tox-max-message-length.

Return the message id on success, #f otherwise."
  (tox-send tox
            %tox-send-action
            friend-number
            action))

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

(define* (tox-friend-name tox friend-number
                          #:optional #:key (anonymous "Anonymous"))
  "Return the nickname of the friend identified by FRIEND-NUMBER for the
messenger TOX, or ANONYMOUS if they have no nickname."
  (let* ((name (make-bytevector tox-max-name-length))
         (length (%tox-get-name (unwrap-tox tox)
                                friend-number
                                (bytevector->pointer name))))
    (if (positive? length)
        (utf8->string (bytevector-slice name 0 length))
        anonymous)))

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

(define (set-tox-nospam tox nospam)
  "Set the 'nospam' part of the address for the messenger TOX."
  (%tox-set-nospam (unwrap-tox tox) nospam))

(define/unwrap tox-nospam
  "Return the 'nospam' part of the address for the messenger TOX."
  %tox-get-nospam)

;;;
;;; Group Chat
;;;

(define (tox-add-group-chat tox)
  "Create a new group chat for the messenger TOX and return the group number,
or #f on failure."
  (let ((result (%tox-add-groupchat (unwrap-tox tox))))
    (if (negative? result) #f result)))

(define (tox-delete-group-chat tox group-number)
  "Remove the group chat identified by GROUP-NUMBER from the messsenger TOX.
Return #t on success, #f otherwise."
  (zero? (%tox-del-groupchat (unwrap-tox tox) group-number)))

(define (tox-group-peer-name tox group-number peer-number)
  "Return the name of the peer identified by PEER-NUMBER in the group
identified by GROUP-NUMBER for the messenger TOX, or #f if there is no such
group or peer."
  (let* ((bv (make-bytevector tox-max-name-length))
         (length (%tox-group-peername (unwrap-tox tox)
                                      group-number
                                      peer-number
                                      (bytevector->pointer bv))))
    (if (negative? length) #f (utf8->string (bytevector-slice bv 0 length)))))

(define (tox-invite-friend tox friend-number group-number)
  "Invite the friend identified by FRIEND-NUMBER to join the group identified
by GROUP-NUMBER.  Return #t on success, or #f if the friend or group is
invalid."
  (zero? (%tox-invite-friend (unwrap-tox tox) friend-number group-number)))

(define (tox-join-group-chat tox friend-number public-key)
  "Join the group identified by bytevector PUBLIC-KEY that the friend
identified by FRIEND-NUMBER for the messenger TOX.  Return the group number on
success, of #f otherwise."
  (false-if-negative
   (%tox-join-groupchat (unwrap-tox tox)
                        friend-number
                        (bytevector->pointer public-key))))

(define (group-send proc tox group-number message)
  (zero?
   (let ((bv (string->utf8 message)))
     (proc (unwrap-tox tox)
           group-number
           (bytevector->pointer bv)
           (bytevector-length bv)))))

(define (tox-group-send-message tox group-number message)
  "Send the string MESSAGE to the group identified by GROUP-NUMBER in the
messenger TOX.  Return #t on success, or #f if no such group exists or the
message is invalid."
  (group-send %tox-group-message-send tox group-number message))

(define (tox-group-send-action tox group-number action)
  "Send the string MESSAGE to the group identified by GROUP-NUMBER in the
messenger TOX.  Return #t on success, or #f if no such group exists or the
message is invalid."
  (group-send %tox-group-action-send tox group-number action))

(define (tox-group-peer-count tox group-number)
  "Return the number of peers in the group identified by GROUP-NUMBER in the
messenger TOX."
  (let ((result (%tox-group-number-peers (unwrap-tox tox) group-number)))
    (if (negative? result)
        (error "Invalid group number: " group-number)
        result)))

(define (tox-group-peer-names tox group-number)
  "Return a list of peer names for the group identified by GROUP-NUMBER in the
messenger TOX."
  (let* ((length (tox-group-peer-count tox group-number))
         (names (make-u8vector (* length tox-max-name-length)))
         (lengths (make-u16vector length))
         (result-length
          (%tox-group-get-names (unwrap-tox tox)
                                group-number
                                (bytevector->pointer names)
                                (bytevector->pointer lengths)
                                length)))
    (if (negative? result-length)
        (error "Invalid group number: " group-number)
        (map (lambda (i)
               (let ((start (* i tox-max-name-length))
                     (length (u16vector-ref lengths i)))
                 (utf8->string
                  (bytevector-slice names start (+ start length)))))
             (iota result-length)))))

(define/unwrap tox-group-chat-count
  "Return the number of group chats in the messenger TOX."
  %tox-count-chatlist)

(define (tox-group-chat-list tox)
  "Return a list of chat IDs in the messenger TOX."
  (let* ((length (tox-group-chat-count tox))
         (bv (make-s32vector length))
         (result-length
          (%tox-get-chatlist (unwrap-tox tox)
                             (bytevector->pointer bv)
                             length)))
    (take (bytevector->sint-list bv (native-endianness) (sizeof int))
          result-length)))

;;;
;;; TODO: File Transfer
;;;
