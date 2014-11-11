(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))


;; optionally, define and register a mode-hook function. To do so, use
;; something like this in your .emacs file:
;;
(require 'flymake)
;; (defun my-csharp-mode-fn ()
;;   "function that runs when csharp-mode is initialized for a buffer."
;;   (turn-on-auto-revert-mode)
;;   (setq indent-tabs-mode nil)
;;   (require 'yasnippet)
;;   (yas/minor-mode-on)
;;   (require 'rfringe)
;;   )
;; (add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)

;; (defvar my-csharp-default-compiler nil)
;; (setq my-csharp-default-compiler "/Users/jasper/bin/mono @@FILE@@")
;; (defun my-csharp-get-value-from-comments (marker-string line-limit)
;;   my-csharp-default-compiler)
;; (add-hook 'csharp-mode-hook (lambda ()
;;                               (if my-csharp-default-compiler
;;                                   (progn
;;                                     (fset 'orig-csharp-get-value-from-comments
;;                                           (symbol-function 'csharp-get-value-from-comments))
;;                                     (fset 'csharp-get-value-from-comments
;;                                           (symbol-function 'my-csharp-get-value-from-comments))))
;;                               (flymake-mode)))

