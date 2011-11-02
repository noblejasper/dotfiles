(require 'org)
(add-hook 'org-mode-hook 'howm-mode)
(add-to-list 'auto-mode-alist '("\\.howm$" . org-mode))
(setq howm-view-title-header "*") ;; ← howm のロードより前に書くこと

;; キー割当の重複を避ける (お好みで)
(setq howm-prefix "\C-c") ;; howm のキーを「C-c , □」から「C-z □」に変更