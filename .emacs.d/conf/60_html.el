;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(require 'zencoding-mode)
(add-hook 'html-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes
(define-key zencoding-mode-keymap [s-return] 'zencoding-expand-line)
(define-key zencoding-mode-keymap [C-return] 'find-file)

;; html-helper-mode
;; http://www.santafe.edu/~nelson/tools/
;; http://www.santafe.edu/~nelson/tools/html-helper-mode.el
;; http://www.santafe.edu/~nelson/tools/tempo.el
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons 
                       '("\\.html?$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons 
                       '("\\.thtml?$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons 
                       '("\\.xhtml?$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons 
                       '("\\.tpl?$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons 
                       '("\\.tmpl?$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons 
                       '("\\.ctp?$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons 
                       '("\\.mt?$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons 
                       '("\\.tt?$" . html-mode) auto-mode-alist))

;; change sequence face
(make-face 'my-sequence-face)
(set-face-foreground 'my-sequence-face "blue")
(set-face-background 'my-sequence-face "bisque")
(setq html-tt-sequence-face 'my-sequence-face)
