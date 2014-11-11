(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda()
            (setq markdown-command "mdown") ;; sudo npm install gh-markdown-cli -g
            (define-key markdown-mode-map (kbd "C-i") 'markdown-cycle)))
(add-hook 'markdown-mode-hook 'howm-mode)

;; marked + markdown-modeの組み合わせが素晴らしい - UNIX的なアレ http://wadap.hatenablog.com/entry/2013/09/15/155342
(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command 
   (format "open -a /Applications/Marked.app %s" 
           (shell-quote-argument (buffer-file-name)))))
(global-set-key "\C-cm" 'markdown-preview-file)
