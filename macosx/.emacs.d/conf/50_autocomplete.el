(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/Users/noblejasper/.emacs.d/elisp/ac-dict")
(ac-config-default)

(global-auto-complete-mode t)

(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

(setq ac-dwim t)
(setq ac-auto-start 2)
(setq ac-auto-show-menu t)
(setq ac-quick-help-delay 0.1)
(setq ac-delay 0)
;(setq ac-auto-show-menu 0)
(setq popup-use-optimized-column-computation nil)
(ac-config-default)

;; 日本語入力中にonだとうざいので切る
(defadvice ac-on-post-command
  (around check-whether-input-type-is-japanese activate)
  (or current-input-method ad-do-it))

;; python
(require 'ac-python)
;; この設定しないとpython-modeで自動的にauto-completeが起動しない
(add-to-list 'ac-modes 'python-2-mode)
