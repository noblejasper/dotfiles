(setq debug-on-error t)

;; When opened from Desktep entry, PATH won't be set to shell's value.
(let ((path-str
       (replace-regexp-in-string
        "\n+$" "" (shell-command-to-string "echo $PATH"))))
  (setenv "PATH" path-str)
  (setq exec-path (nconc (split-string path-str ":") exec-path)))

(let*
    ((user-emacs-directory
      (substring (or load-file-name "~/.emacs.d/init.el") 0 -7))
     (conf-list (list
                 "init.el"
                 "exec-path.el"
                 "el-get.el"
                 "cc-mode.el"
                 ;; "flymake.el"
                 "midnight.el"
                 "misc.el"
                 "keys.el"
                 )))
  (progn
    (dolist (conf conf-list)
      (load (concat user-emacs-directory "conf/" conf)))
    (and (or (equal window-system 'ns) (equal window-system 'mac))
         (dolist (conf (list "cocoa-init.el"
                             "cocoa-el-get.el"
                             "cocoa-theme.el"
                             "cocoa-font.el"
                             "cocoa-server.el"
                             ))
           (load (concat user-emacs-directory "conf/" conf))))
    (and (null window-system)
         (dolist (conf (list "nw-init.el"))
           (load (concat user-emacs-directory "conf/" conf))))))

