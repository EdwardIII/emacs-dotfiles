;;; label-mode.el --- Handle .lbl Label files        -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Edward

;; Author: Edward <edward@edward-hp>
;; Keywords: lisp

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

(define-derived-mode label-mode text-mode "Label"
  "Major mode for editing translation files written in the LABEL format.
Based on \"text-mode\", but enables spell checking by default.

Turning on Label mode runs the normal hook `label-mode-hook'.")

(add-hook 'label-mode-hook (apply-partially 'message "Entering label mode"))
(add-hook 'label-mode-hook (apply-partially 'flyspell-mode 1))
(add-hook 'label-mode-hook #'flyspell-buffer)

(defun ep/is-label-definition? (start-pos _ __)
  "Is START-POS on a line that is a label defintion?
Label definitions look like ~~SOME_LABEL~~"
  (save-excursion
    (goto-char start-pos)
    (let ((line (thing-at-point 'line t)))
      (and  (string-prefix-p "~~" line) (string-suffix-p "~~\n" line)))))

(defun ep/flycheck-ignore-label-contents ()
  "Ignore the contents of lines that are just ~~LABEL_DEFINITIONS~~."
  (add-hook 'flyspell-incorrect-hook #'ep/is-label-definition? nil t))
(add-hook 'label-mode-hook #'ep/flycheck-ignore-label-contents)

(defun ep/labels-font-lock ()
  "Font highlighting for ~~LABELs~~."
  (font-lock-add-keywords nil
                          '(("\\(~~.*~~\\)" . 'font-lock-keyword-face))))

(add-hook 'label-mode-hook #'ep/labels-font-lock)
(add-to-list 'auto-mode-alist '("\\.lbl\\'" . label-mode))
(provide 'label-mode)
;;; label-mode.el ends here
