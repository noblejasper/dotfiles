(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;; http://amt.ty.land.to/OpenNote/Pymacs.html
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

;; http://sheephead.homelinux.org/2009/05/27/1281/
;; (require 'pysmell)
;; (add-hook 'python-mode-hook (lambda () (pysmell-mode 1)))

;; (defvar ac-source-pysmell
;;   '((candidates
;;      . (lambda ()
;;          (require 'pysmell)
;;          (pysmell-get-all-completions))))
;;   "Source for PySmell")

;; (add-hook 'python-mode-hook
;;           '(lambda ()
;;              (set
;;               (make-local-variable 'ac-sources)
;;               (append ac-sources '(ac-source-pysmell)))))
