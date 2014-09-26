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
;; Tox group chat API.
;;
;;; Code:

(define-module (tox group-chat)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4)
  #:use-module (system foreign)
  #:use-module ((tox bindings) #:prefix %)
  #:use-module (tox util)
  #:use-module (tox)
  #:export (tox-add-group-chat tox-delete-group-chat
            tox-group-peer-name tox-invite-friend
            tox-join-group-chat
            tox-group-send-message tox-group-send-action
            tox-group-peer-count tox-group-peer-names
            tox-group-chat-count tox-group-chat-list))

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
