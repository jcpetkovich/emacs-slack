;;; slack-file-message.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2017  南優也

;; Author: 南優也 <yuyaminami@minamiyuuya-no-MacBook.local>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'eieio)
(require 'slack-message)

(defclass slack-file-message (slack-message)
  ((file-id :initarg :file-id :type string)
   (file :initarg :file)))

(defmethod slack-message-file ((this slack-file-message) team)
  (slack-file-find (oref this file-id) team))

(defmethod slack-message-starred-p ((m slack-file-message))
  (oref (oref m file) is-starred))

(defmethod slack-file-comment-id ((m slack-file-message))
  (slack-file-comment-id (oref m comment)))

(defmethod slack-file-channel ((m slack-file-message))
  (oref m channel))

(defmethod slack-file-id ((m slack-file-message))
  (slack-file-id (oref m file)))

(defmethod slack-message-reactions ((this slack-file-message))
  (slack-message-reactions (oref this file)))

(defmethod slack-reaction-find ((m slack-file-message) reaction)
  (slack-reaction-find (oref m file) reaction))

(defmethod slack-message-set-file ((m slack-file-message) payload team)
  (let ((file (slack-file-create (plist-get payload :file))))
    (oset m file file)
    (oset m file-id (oref file id))
    (slack-file-set-channel file (plist-get payload :channel))
    (slack-file-pushnew file team)
    m))

(provide 'slack-file-message)
;;; slack-file-message.el ends here
