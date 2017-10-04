;;; moodle.el --- Grade Moodle assignments offline

;; Author: Titus von der Malsburg <malsburg@posteo.de>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; URL: http://github.com/tmalsburg/moodle.el
;; Version: 0.0.1
;; Package-Requires: ((pcsv "1.3.7") (s "1.9.0") (dash "2.6.0") (cl-lib "0.5") (google-this "1.10"))
;; Prefix: moodle-
;; Separator: -

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

;; Grade Moodle assignments offline in the comfort of your personal
;; Emacs setup.
;;
;; See the github page for details:
;;
;;    https://github.com/tmalsburg/moodle.el

;;; Install:

;; Put this file in a directory included in your load path or install
;; helm-bibtex from MELPA (preferred).  Then add the following in your
;; Emacs startup file:
;;
;;     (require 'moodle)
;;

;;; Code:

(require 'pcsv)
(require 'button)
(require 'google-this)

(define-minor-mode moodle-display-record-mode
    "Minor mode to simulate buffer local keybindings."
    :init-value nil
    :lighter "moodle-record"
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map (kbd "C-c C-c")   'moodle-wrap-up)
              (define-key map (kbd "C-c r")     'moodle-revert-record)
              (define-key map (kbd "<M-left>")  'moodle-save-and-previous-record)
              (define-key map (kbd "<M-right>") 'moodle-save-and-next-record)
              map))

(define-minor-mode moodle-display-solution-mode
    "Minor mode to simulate buffer local keybindings."
    :init-value nil
    :lighter "moodle-solution"
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map (kbd "C-c C-c")   'moodle-wrap-up)
              (define-key map (kbd "C-c r")     'moodle-revert-record)
              (define-key map (kbd "C-c g")     'moodle-google-this-region)
              (define-key map (kbd "C-c .")     'moodle-google-all-sentences)
              (define-key map (kbd "<M-left>")  'moodle-save-and-previous-record)
              (define-key map (kbd "<M-right>") 'moodle-save-and-next-record)
              map))

(defun moodle-google-this-region ()
  (interactive)
  (google-this-region t t))

(defun moodle-google-all-sentences ()
  (interactive)
  (cl-loop
   for sentence in (split-string (buffer-string) "\\. ")
   do
   (let ((sentence (s-trim sentence)))
     (unless (or (string= sentence "")
                 (> 20 (length sentence)))
       (google-this-string t sentence t)))))

(defun moodle-save-record ()
  (with-current-buffer "*moodle-view-record*"
    (let* ((lines    (split-string (buffer-string) "\n"))
           (score    (let ((line (nth 4 lines)))
                       (string-match "^Grade: +\\([0-9,]*\\) +" line)
                       (match-string-no-properties 1 line)))
           (feedback (nth 1 (split-string (buffer-string) "\n\nFeedback:"))))
      (setf (elt (elt moodle-records (1+ moodle-current-record-index)) 5) score)
      (setf (elt (elt moodle-records (1+ moodle-current-record-index)) 11) (s-trim feedback)))))

(defun moodle-revert-record ()
  (interactive)
  (moodle-show-record))

(defun moodle-save-and-next-record ()
  (interactive)
  (moodle-save-record)
  (setq moodle-current-record-index (mod (1+ moodle-current-record-index) (1- (length moodle-records))))
  (moodle-show-record))

(defun moodle-save-and-previous-record ()
  (interactive)
  (moodle-save-record)
  (setq moodle-current-record-index (mod (1- moodle-current-record-index) (1- (length moodle-records))))
  (moodle-show-record))

(defun moodle-wrap-up ()
  (interactive)
  (moodle-save-record)
  (moodle-write-csv moodle-records moodle-assignment-fname)
  (kill-buffer "*moodle-view-record*")
  (delete-window (get-buffer-window "*moodle-view-solution*")))

(defun moodle-write-csv (records fname)
  (with-temp-buffer
    (cl-loop
     for record in records
     do
     (progn
       (insert
        (s-join "," (--map (if (or (s-contains? "," it)
                                   (s-contains? "\n" it)
                                   (s-contains? "\"" it))
                               (format "\"%s\"" (s-replace "\"" "\"\"" it))
                             it) record)))
       (insert "\n")))
    (write-file fname)))

(defun moodle-display-record (line assignment-id)
  (with-current-buffer (get-buffer-create "*moodle-view-record*")
    (erase-buffer)
    (let ((url (format "https://moodle2.uni-potsdam.de/mod/assign/view.php?id=%s&action=grader" assignment-id)))
      (insert (format "Student %d/%d - " (1+ moodle-current-record-index) (1- (length moodle-records))))
      (insert-text-button
       url
       'action (lambda (x) (browse-url (button-get x 'url)))
       'url url)
      (insert "\n\n"))
    (insert
     (s-format
      "$1 ($2, $3):\n\nGrade: $5 (out of $6)\n\nFeedback:\n\n$11"
      'elt
      line))
    (moodle-display-record-mode)))

(defun moodle-html2text ()
  "Replacement for standard html2text using shr."
  (interactive)
  (let ((dom (libxml-parse-html-region (point-min) (point-max)))
        (shr-width fill-column)
	(shr-inhibit-images t)
  	(shr-bullet " "))
     (erase-buffer)
     (shr-insert-document dom)
     (goto-char (point-min))))

(defun moodle-display-solution (record)
  (with-current-buffer (get-buffer-create "*moodle-view-solution*")
    (erase-buffer)
    (insert (nth 9 record))
    (moodle-html2text)
    (moodle-display-solution-mode)))

(defun moodle-show-record ()
  (let ((record (nth (1+ moodle-current-record-index) moodle-records))
        (window (selected-window)))
    (moodle-display-record record moodle-assignment-id)
    (moodle-display-solution record)
    (select-window window)))

(defun moodle-init-grading (fname)
  (setq moodle-assignment-fname     fname)
  (setq moodle-assignment-id        (car (last (split-string fname "[-.]") 2)))
  (setq moodle-records              (pcsv-parse-file fname))
  (setq moodle-current-record-index 0)
  (switch-to-buffer (get-buffer-create "*moodle-view-record*"))
  (switch-to-buffer-other-window (get-buffer-create "*moodle-view-solution*"))
  (moodle-show-record))

;;;###autoload
(defun moodle-grade-assignment (fname)
  (interactive "fAssignment CSV file: ")
  (moodle-init-grading fname))

(provide 'moodle)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; moodle.el ends here
