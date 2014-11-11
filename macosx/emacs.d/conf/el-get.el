(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

(setq
 el-get-dir (concat user-emacs-directory "el-get")
 el-get-verbose t
 el-get-user-package-directory (concat user-emacs-directory "conf/init")
 el-get-generate-autoloads nil)

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch el-get-install-skip-emacswiki-recipes)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path (concat user-emacs-directory "recipes"))

(el-get 'sync
        '(
          ;; el-get
          powerline
          powerline-themes
          anything
          ;; anything-project
          anything-c-moccur
          ;; auto-save-buffers-enhanced
          cperl-mode
          perl-completion
          markdown-mode
          clmemo
          ee
          git-gutter
          open-junk-file
          ;; emacs-w3m
          auto-complete
          ;; ddskk
          sequential-command
          sequential-command-config
          ce-scroll
          elscreen
          smartchr
          yasnippet
          jinja2-mode
          python-mode
          howm
          anything-howm
          ;; migemo
          rfringe
          ;; flymake-for-csharp
          csharp-mode
          hiwin
          zencoding-mode
          open-junk-file
          ;; exec-path-from-shell
          ;; dash
          ;; f
          flycheck
          dash
          omnisharp
          tabbar
          sense-region
          ag
          coffee-mode
          json
          php-mode
          mmm-mode
          ))
