;;; slack-file-comment.el ---                        -*- lexical-binding: t; -*-

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
(require 'slack-file-message)

(define-derived-mode slack-edit-file-comment-mode
  fundamental-mode
  "Slack Edit File Comment"
  ""
  (slack-buffer-enable-emojify))

(defvar slack-edit-file-comment-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-c") #'slack-file-comment-edit-commit)
    keymap))

(defclass slack-file-comment (slack-user-message)
  ((id :initarg :id)
   (file-id :initarg :file_id :type string)
   (created :initarg :created)
   (timestamp :initarg :timestamp)
   (user :initarg :user)
   (is-intro :initarg :is_intro)
   (comment :initarg :comment)
   (channel :initarg :channel)
   (reactions :initarg :reactions type list)
   (is-starred :initarg :is_starred :type boolean :initform nil)))

(defclass slack-file-mention-message (slack-file-message)
  ((user :initarg :user :initform nil)))

(defmethod slack-reaction-delete ((this slack-file-comment) reaction)
  (with-slots (reactions) this
    (setq reactions
          (slack-reaction--delete reactions reaction))))

(defmethod slack-reaction-push ((this slack-file-comment) reaction)
  (push reaction (oref this reactions)))

(defmethod slack-reaction-find ((this slack-file-comment) reaction)
  (slack-reaction--find (oref this reactions) reaction))

(defmethod slack-message-reactions ((this slack-file-comment))
  (oref this reactions))

(defmethod slack-message-reaction-to-string ((m slack-file-comment))
  (let ((reactions (oref m reactions)))
    (when reactions
      (slack-format-reactions reactions))))

(defmethod slack-equalp ((c slack-file-comment) other)
  (string= (oref c id) (oref other id)))

(defmethod slack-merge ((old slack-file-comment) new)
  (with-slots (comment reactions) old
    (setq comment (oref new comment))
    (setq reactions (slack-merge-list reactions (oref new reactions)))))

(defmethod slack-message-body ((comment slack-file-comment) team)
  (slack-message-unescape-string (oref comment comment) team))

(defmethod slack-message-to-string ((comment slack-file-comment) team)
  (let ((header (slack-message-header-to-string comment team))
        (body (slack-message-body-to-string comment team))
        (reactions (slack-message-reaction-to-string comment)))
    (propertize (slack-format-message header body reactions)
                'file-comment-id (oref comment id))))

(defmethod slack-file-id ((file-comment slack-file-comment))
  (oref file-comment file-id))

(defmethod slack-message-star-added ((this slack-file-comment))
  (oset this is-starred t))

(defmethod slack-message-star-removed ((this slack-file-comment))
  (oset this is-starred nil))

(defmethod slack-message-star-api-params ((this slack-file-comment))
  (cons "file_comment" (oref this id)))

(defun slack-file-comment-edit-commit ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer)
            (message (buffer-substring-no-properties (point-min) (point-max))))
      (slack-buffer-send-message buf message)))

(defun slack-file-comment-process-star-api (url team)
  (slack-if-let* ((file-id slack-current-file-id)
            (file-comment-id (slack-get-file-comment-id)))
      (slack-with-file file-id team
        (slack-with-file-comment file-comment-id file
          (slack-message-star-api-request url
                                          (list (slack-message-star-api-params file-comment))
                                          team)))))

(defmethod slack-ts ((this slack-file-comment))
  (number-to-string (oref this timestamp)))

(provide 'slack-file-comment)
;;; slack-file-comment.el ends here
