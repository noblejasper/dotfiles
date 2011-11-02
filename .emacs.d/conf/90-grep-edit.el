(require 'grep)
(require 'grep-edit)

(defadvice grep-edit-change-file (around inhibit-read-only activate)
  ""
  (let ((inhibit-read-only t))
    ad-do-it))
;; (progn (ad-disable-advice 'grep-edit-change-file 'around 'inhibit-read-only) (ad-update 'grep-edit-change-file)) 

(defun my-grep-edit-setup ()
  (define-key grep-mode-map '[up] nil)
  (define-key grep-mode-map "\C-c\C-c" 'grep-edit-finish-edit)
  (message (substitute-command-keys "\\[grep-edit-finish-edit] to apply changes."))
  (set (make-local-variable 'inhibit-read-only) t)
  )
(add-hook 'grep-setup-hook 'my-grep-edit-setup t)