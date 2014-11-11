;; jinja2-mode は SGML継承
(setq-default sgml-indent-step 4)
(setq-default sgml-basic-offset 4)
(setq-default sgml-indent-line-num 4)
(defun my-sgml-mode-hook ()
  ;(xslt-process-mode)
  (make-variable-buffer-local 'adaptive-fill-regexp)
  (setq tab-width 4
        indent-tabs-mode nil
        sgml-indent-step 4
        sgml-indent-data t
        adaptive-fill-regexp nil
        ispell-skip-sgml t
        )
  ; (auto-fill-mode t)
  ; increase fillcolumn
  (set-fill-column 90)
  (sgml-electric-tag-pair-mode t)
)

(add-hook 'sgml-mode-hook 'my-sgml-mode-hook)
(add-hook 'jinja2-mode-hook 'my-sgml-mode-hook)
;; (defun local-sgml-mode-hook
;;   (setq indent-tabs-mode nil
;;         next-line-add-newlines nil
;;         standard-indent 4
;;         sgml-indent-data t)
;;   (set-fill-column 80)
;;   (sgml-electric-tag-pair-mode t)
;;   (auto-fill-mode t))
;; (add-hook 'jina2-mode
;;           '(lambda () (local-psgml-mode-hook)))
(autoload 'jina2-mode "jinja2-mode" "Major mode for editing Jinja2 code." t)
(setq auto-mode-alist 
      (append '(("\\.html?$" . jinja2-mode)
                ) auto-mode-alist))


;; keys
(global-set-key "\M-n" 'sgml-skip-tag-forward)
(global-set-key "\M-p" 'sgml-skip-tag-backward)
