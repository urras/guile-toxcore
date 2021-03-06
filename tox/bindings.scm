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
  '* "tox_new" '(*))

(define-tox tox-kill
  void "tox_kill" '(*))

(define-tox tox-do-interval
  uint32 "tox_do_interval" '(*))

(define-tox tox-do
  void "tox_do" '(*))

(define-tox tox-size
  uint32 "tox_size" '(*))

(define-tox tox-save
  void "tox_save" '(* *))

(define-tox tox-load
  int "tox_load" (list '* '* uint32))

(define-tox tox-isconnected
  int "tox_isconnected" '(*))

(define-tox tox-bootstrap-from-address
  int "tox_bootstrap_from_address" (list '* '* uint16 '*))

(define-tox tox-get-address
  void "tox_get_address" '(* *))

(define-tox tox-add-friend
  int32 "tox_add_friend" (list '* '* '* uint16))

(define-tox tox-add-friend-norequest
  int32 "tox_add_friend_norequest" '(* *))

(define-tox tox-get-friend-number
  int32 "tox_get_friend_number" '(* *))

(define-tox tox-get-client-id
  int "tox_get_client_id" (list '* int32 '*))

(define-tox tox-del-friend
  int "tox_del_friend" (list '* int32))

(define-tox tox-get-friend-connection-status
  int "tox_get_friend_connection_status" (list '* int32))

(define-tox tox-friend-exists
  int "tox_friend_exists" (list '* int32))

(define-tox tox-send-message
  uint32 "tox_send_message" (list '* int32 '* uint32))

(define-tox tox-send-action
  uint32 "tox_send_action" (list '* int32 '* uint32))

(define-tox tox-set-name
  int "tox_set_name" (list '* '* uint16))

(define-tox tox-get-self-name
  uint16 "tox_get_self_name" (list '* '*))

(define-tox tox-get-name
  int "tox_get_name" (list '* int32 '*))

(define-tox tox-get-name-size
  int "tox_get_name_size" (list '* int32))

(define-tox tox-get-self-name-size
  int "tox_get_self_name_size" '(*))

(define-tox tox-set-status-message
  int "tox_set_status_message" (list '* '* uint16))

(define-tox tox-set-user-status
  int "tox_set_user_status" (list '* uint8))

(define-tox tox-get-status-message-size
  int "tox_get_status_message_size" (list '* int32))

(define-tox tox-get-self-status-message-size
  int "tox_get_self_status_message_size" '(*))

(define-tox tox-get-status-message
  int "tox_get_status_message" (list '* int32 '* uint32))

(define-tox tox-get-self-status-message
  int "tox_get_self_status_message" (list '* '* uint32))

(define-tox tox-get-user-status
  uint8 "tox_get_user_status" (list '* int32))

(define-tox tox-get-self-user-status
  uint8 "tox_get_self_user_status" '(*))

(define-tox tox-get-last-online
  uint64 "tox_get_last_online" (list '* int32))

(define-tox tox-set-user-is-typing
  int "tox_set_user_is_typing" (list '* int32 uint8))

(define-tox tox-get-is-typing
  uint8 "tox_get_is_typing" (list '* int32))

(define-tox tox-count-friendlist
  uint32 "tox_count_friendlist" '(*))

(define-tox tox-get-num-online-friends
  uint32 "tox_get_num_online_friends" '(*))

(define-tox tox-get-friendlist
  uint32 "tox_get_friendlist" (list '* '* uint32))

(define-tox tox-callback-friend-request
  void "tox_callback_friend_request" '(* * *))

(define-tox tox-callback-friend-message
  void "tox_callback_friend_message" '(* * *))

(define-tox tox-callback-friend-action
  void "tox_callback_friend_action" '(* * *))

(define-tox tox-callback-name-change
  void "tox_callback_name_change" '(* * *))

(define-tox tox-callback-status-message
  void "tox_callback_status_message" '(* * *))

(define-tox tox-callback-user-status
  void "tox_callback_user_status" '(* * *))

(define-tox tox-callback-typing-change
  void "tox_callback_typing_change" '(* * *))

(define-tox tox-callback-read-receipt
  void "tox_callback_read_receipt" '(* * *))

(define-tox tox-callback-connection-status
  void "tox_callback_connection_status" '(* * *))

(define-tox tox-get-nospam
  uint32 "tox_get_nospam" '(*))

(define-tox tox-set-nospam
  void "tox_set_nospam" (list '* uint32))

(define-tox tox-callback-group-invite
  void "tox_callback_group_invite" '(* * *))

(define-tox tox-callback-group-message
  void "tox_callback_group_message" '(* * *))

(define-tox tox-callback-group-action
  void "tox_callback_group_action" '(* * *))

(define-tox tox-callback-group-namelist-change
  void "tox_callback_group_namelist_change" '(* * *))

(define-tox tox-add-groupchat
  int "tox_add_groupchat" '(*))

(define-tox tox-del-groupchat
  int "tox_del_groupchat" (list '* int))

(define-tox tox-group-peername
  int "tox_group_peername" (list '* int int '*))

(define-tox tox-invite-friend
  int "tox_invite_friend" (list '* int32 int))

(define-tox tox-join-groupchat
  int "tox_join_groupchat" (list '* int32 '*))

(define-tox tox-group-message-send
  int "tox_group_message_send" (list '* int '* uint32))

(define-tox tox-group-action-send
  int "tox_group_action_send" (list '* int '* uint32))

(define-tox tox-group-number-peers
  int "tox_group_number_peers" (list '* int))

(define-tox tox-group-get-names
  int "tox_group_get_names" (list '* int '* '* int))

(define-tox tox-count-chatlist
  uint32 "tox_count_chatlist" '(*))

(define-tox tox-get-chatlist
  uint32 "tox_get_chatlist" (list '* '* uint32))

(define-tox tox-callback-file-send-request
  void "tox_callback_file_send_request" '(* * *))

(define-tox tox-callback-file-control
  void "tox_callback_file_control" '(* * *))

(define-tox tox-callback-file-data
  void "tox_callback_file_data" '(* * *))

(define-tox tox-new-file-sender
  int "tox_new_file_sender" (list '* int32 uint64 '* uint16))

(define-tox tox-file-send-control
  int "tox_file_send_control" (list '* int32 uint8 uint8 uint8 '* uint16))

(define-tox tox-file-send-data
  int "tox_file_send_data" (list '* int32 uint8 '* uint16))

(define-tox tox-file-data-size
  int "tox_file_data_size" (list '* int32))

(define-tox tox-file-data-remaining
  uint64 "tox_file_data_remaining" (list '* int32 uint8 uint8))
