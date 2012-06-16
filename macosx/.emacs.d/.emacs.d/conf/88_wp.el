;;=======================================================================
;; @ wp-emacs（weblogger）
;;=======================================================================
(require 'weblogger)
(global-set-key "\C-c\C-w" 'weblogger-start-entry)  ; weblogger起動（C-c C-w）

(defun my-weblogger-send-entry (&optional arg)
  (interactive)
  (save-buffer)
  (set-buffer-modified-p t)
  (weblogger-save-entry nil arg)
  (my-weblogger-quit))

(defun my-weblogger-quit ()
  (interactive)
  (when (y-or-n-p "Do you want to quit weblogger-entry? ")
    (bury-buffer)))

(add-hook 'weblogger-start-edit-entry-hook
          '(lambda ()
             (define-key weblogger-entry-mode-map (kbd "C-x C-s") nil)
             (define-key weblogger-entry-mode-map (kbd "C-c n")   'weblogger-next-entry)
             (define-key weblogger-entry-mode-map (kbd "C-c p")   'weblogger-prev-entry)
             (define-key weblogger-entry-mode-map (kbd "C-c c")   'weblogger-start-entry)
             (define-key weblogger-entry-mode-map (kbd "C-c C-c") 'my-weblogger-send-entry)
             (define-key weblogger-entry-mode-map (kbd "C-c C-k") 'my-weblogger-quit)
             (zencoding-mode t)                     ; zencoding-mode
             ;; (yas/minor-mode t)                     ; YASnippet マイナーモードを有効
             (auto-fill-mode -1)))                  ; 自動改行をOFF