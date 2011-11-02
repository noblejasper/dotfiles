;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; php-mode
(load-library "php-mode")
(require 'php-mode)
(setq auto-mode-alist
      (cons '("\\.php\\'" . php-mode) auto-mode-alist))
(setq php-mode-force-pear t)
(add-hook 'php-mode-user-hook
          (lambda ()
            (require 'php-completion)
            (php-completion-mode t)
            (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)))
;; (add-hook  'php-mode-hook
;;            (lambda ()
;;              (when (require 'auto-complete nil t)
;;                (make-variable-buffer-local 'ac-sources)
;;                (add-to-list 'ac-sources 'ac-source-php-completion)
;;                ;; if you like patial match,
;;                ;; use `ac-source-php-completion-patial' instead of `ac-source-php-completion'.
;;                (add-to-list 'ac-sources 'ac-source-php-completion-patial)
;;                (auto-complete-mode t))))
;; (add-hook 'php-mode-user-hook
;;           '(lambda ()
;;              (setq php-search-url "http://www.phppro.jp/")
;;              (setq php-manual-url "http://www.phppro.jp/phpmanual")
;; ;             (setq php-manual-path "/usr/local/share/php/doc/html/")
;;              )
;;           (setq c-basic-offset 2)
;;           (setq indent-tabs-mode nil)
;;           )
(global-set-key [C-:] 'php-complete-function)
