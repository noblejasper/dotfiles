(when (require 'anything nil t)
  ;; default keymap
  (global-set-key "\C-xb" 'anything)
  (define-key anything-map (kbd "C-M-n") 'anything-next-source)
  (define-key anything-map (kbd "C-M-p") 'anything-previous-source)
  (define-key anything-map (kbd "L") 'anything-execute-persistent-action)


  (setq
   anything-idle-delay 0.3
   anything-input-idle-delay 0.2
   ;anything-candidate-number-limit 100
   anything-quick-update t
   anything-enable-digit-shortcuts t
   anything-enable-shortcuts 'alphabet)

  (defun anything-kill-ring ()
    (interactive)
    (anything 'anything-c-source-kill-ring nil nil nil nil "*anything kill ring*"))
  (global-set-key (kbd "M-y") 'anything-kill-ring)

  (when (require 'ac-anything nil t)
    (define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-anything))
  (when (require 'anything-config nil t)
    (setq anything-su-or-sudo "sudo"))
  (require 'anything-match-plugin nil t)
  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (require 'anithing-migemo nil t))
  (when (require 'anything-complete nil t)
    (anything-lisp-complete-symbol-set-timer 150))
  (require 'anything-show-completion nil t)
  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))
  (when (require 'descbinds-anything nil t)
    (descbinds-anything-install))
  (require 'anything-match-plugin)
  ;; (anything-read-string-mode 0)
  (define-key global-map (kbd "M-x")
    (lambda ()
      (interactive)
      (anything-other-buffer
       '(anything-c-source-extended-command-history anything-c-source-emacs-commands)
       "*anything emacs commands*")))
  (setq anything-sources (list anything-c-source-buffers+
                               anything-c-source-recentf
                               anything-c-source-bookmarks
                               anything-c-source-file-cache
                               anything-c-source-man-pages
                               anything-c-source-file-name-history
                               anything-c-source-calculation-result
                               anything-c-source-locate
                               anything-c-source-complex-command-history
                               anything-c-source-emacs-commands
                               anything-c-source-emacs-functions
                               anything-c-source-buffer-not-found
                               anything-c-source-files-in-current-dir+)))

(when (require 'anything-c-moccur nil t)
  (setq
   anything-c-moccur-anything-idle-delay 0.1
   lanything-c-moccur-highlight-info-line-flag t
   anything-c-moccur-enable-auto-look-flag t
   anything-c-moccur-enable-initial-pattern t)
  (global-set-key (kbd "C-M-o") 'anything-c-moccur-occur-by-moccur))
