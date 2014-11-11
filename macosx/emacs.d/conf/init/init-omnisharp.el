(require 'omnisharp)
(setq omnisharp-server-executable-path (expand-file-name "/Users/jasper/git/Omnisharp/server/OmniSharp/bin/Debug/OmniSharp.exe"))
(add-hook 'csharp-mode-hook
            #'(lambda ()
                (omnisharp-mode)
                (ac-common-setup)
                (auto-complete-mode)))
(global-auto-complete-mode t)

(defun my-omnisharp-start (sln)
  (interactive "fOpen sln: ")
  (omnisharp-start-omnisharp-server (expand-file-name sln)))
