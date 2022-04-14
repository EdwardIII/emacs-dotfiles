;(defun setup-tide-mode ()
;  (interactive)
;  (tide-setup)
;  (flycheck-mode +1)
;  (setq flycheck-check-syntax-automatically '(save mode-enabled))
;  (eldoc-mode +1)
;  (tide-hl-identifier-mode +1)
;  ;; company is an optional dependency. You have to
;  (company-mode +1))
;
;;; aligns annotation to the right hand side
;(setq company-tooltip-align-annotations t)
;
;(setq tide-format-options '(
;			    :insertSpaceAfterFunctionKeywordForAnonymousFunctions t
;										  :placeOpenBraceOnNewLineForFunctions nil
;										  :indentSize 2
;										  :tabSize 2
;										  :placeOpenBraceOnNewLineForFunctions nil
;										  :placeOpenBraceOnNewLineForControlBlocks nil))
;
;(setq typescript-indent-level 2)
;
;;; formats the buffer before saving
;(add-hook 'before-save-hook 'tide-format-before-save)
;
;(add-hook 'typescript-mode-hook #'setup-tide-mode)
;
;(provide 'init-tide) ;;; 
;
