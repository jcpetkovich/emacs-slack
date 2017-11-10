;;; slack-file-comment-message.el ---                -*- lexical-binding: t; -*-

;; Copyright (C) 2017

;; Author:  <yuya373@yuya373>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'eieio)
(require 'slack-file-message)

(defclass slack-file-comment-message (slack-file-message)
  ((comment :initarg :comment :initform nil)
   (comment-id :initarg :comment-id :type string)))

(defmethod slack-message-set-file-comment ((m slack-file-comment-message) payload)
  (let* ((file-id (plist-get (plist-get payload :file) :id))
         (comment (plist-get payload :comment))
         (file-comment (slack-file-comment-create comment file-id)))
    (oset m comment file-comment)
    (oset m comment-id (oref file-comment id))
    m))

(defmethod slack-file-comment-id ((m slack-file-comment-message))
  (oref m comment-id))

(defmethod slack-message-sender-name ((m slack-file-comment-message) team)
  (slack-user-name (oref (oref m comment) user) team))

(defmethod slack-message-sender-id ((m slack-file-comment-message))
  (oref (oref m comment) user))

(defmethod slack-message-star-added ((m slack-file-comment-message))
  (slack-message-star-added (oref m comment)))

(defmethod slack-message-star-removed ((m slack-file-comment-message))
  (slack-message-star-removed (oref m comment)))

(defmethod slack-message-star-api-params ((m slack-file-comment-message))
  (slack-message-star-api-params (oref m comment)))

(defmethod slack-reaction-delete ((this slack-file-comment-message) reaction)
  (slack-reaction-delete (oref this comment) reaction))

(defmethod slack-reaction-push ((this slack-file-comment-message) reaction)
  (slack-reaction-push (oref this comment) reaction))

(defmethod slack-reaction-find ((this slack-file-comment-message) reaction)
  (slack-reaction-find (oref this comment) reaction))

(defmethod slack-message-reactions ((this slack-file-comment-message))
  (slack-message-reactions (oref this comment)))

(defmethod slack-message-get-param-for-reaction ((m slack-file-comment-message))
  (cons "file_comment" (oref (oref m comment) id)))

(defmethod slack-file-id ((m slack-file-comment-message))
  (slack-file-id (oref m comment)))

(defmethod slack-find-file-comment ((this slack-file-comment-message) team)
  (slack-if-let* ((file (slack-find-file this team)))
      (slack-find-file-comment file (oref this comment-id))))

(defmethod slack-starred-p ((this slack-file-comment-message) team)
  (slack-if-let* ((comment (slack-find-file-comment this team)))
      (oref comment is-starred)))

(defmethod slack-message-to-string ((this slack-file-comment-message) team)
  (with-slots (permalink name id page) (oref this file)
    (with-slots (comment) (oref this comment)
      (let* ((face '(:underline t))
             (text (format "commented on %s <%s|open in browser>\n%s"
                           (slack-file-link-info (oref (oref this file) id) name)
                           permalink
                           (format "“ %s" (slack-message-unescape-string comment
                                                                          team))))
             (header (slack-message-header-to-string this team))
             (reactions (slack-message-reaction-to-string this)))
        (slack-format-message header text reactions)))))

(defmethod slack-message-get-user-id ((m slack-file-comment-message))
  (oref (oref m comment) user))

(defmethod slack-message-edit-type ((_m slack-file-comment-message))
  'edit-file-comment)

(defmethod slack-message-get-text ((m slack-file-comment-message))
  (oref (oref m comment) comment))

(defmethod slack-message-changed--copy ((this slack-file-comment-message) other)
  (when (slack-file-comment-message-p other)
    (let ((changed (call-next-method)))
      (with-slots ((old-comment comment) text) this
        (let ((new-comment (oref other comment)))
          (unless (string= (oref old-comment comment) (oref new-comment comment))
            (oset old-comment comment (oref new-comment comment))
            (setq changed t))))
      changed)))


(provide 'slack-file-comment-message)
;;; slack-file-comment-message.el ends here
