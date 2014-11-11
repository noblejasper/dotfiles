;; C-hでバックスペース
(global-set-key "\C-h" 'delete-backward-char)

;; 折り返しトグル
(defun toggle-truncate-lines ()
  "折り返し表示をトグル動作します."
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))
(global-set-key "\C-c\C-l" 'toggle-truncate-lines)

;; C-t でother-window、分割されてなかったら分割
(defun other-window-or-split () ; http://d.hatena.ne.jp/rubikitch/20100210/emacs
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(define-key global-map (kbd "C-t") 'other-window-or-split)

;; M-i でdabbrev
(define-key global-map (kbd "M-i") 'dabbrev-expand)

;; Meta+g でgoto-line
(global-set-key "\M-g" 'goto-line)

;; find-grep
(setq mac-command-modifier 'super)
(global-set-key (kbd "s-r") 'find-grep)

;; C-x k は current-buffer 専用にしちゃう
(defun kill-current-buffer ()
  (interactive)
  (kill-buffer))
(define-key global-map (kbd "C-x k") 'kill-current-buffer)

; window
(global-set-key [M-right] 'split-window-horizontally)
(global-set-key [M-left] 'split-window-horizontally)
(global-set-key [M-up] 'split-window-vertically)
(global-set-key [M-down] 'delete-other-windows)

;; windmove
;; http://hovav.net/elisp/
(require 'windmove)
(windmove-default-keybindings)

;; (setq cua-rectangle-mark-key [C-M-return])
;; (cua-mode t)
;; (setq cua-enable-cua-keys nil)
(global-unset-key [C-return])
(global-set-key [C-return] 'find-file)
; Ctrl+Meta+delete で現在開いてるバッファを消す
(global-set-key [C-M-backspace]
    '(lambda() (interactive)(kill-buffer (buffer-name))))

(defun find-tag-next ()
  (interactive)
  (find-tag last-tag t))

;;(global-set-key (kbd "C-M-.") 'find-tag-next)
(global-set-key (kbd "M-,")   'find-tag-other-window)
(global-set-key (kbd "M-.")   'find-tag-next)
(global-set-key (kbd "C-c ,") 'anything-c-etags-select)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [基本] トラックパッド用のスクロール設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scroll-down-with-lines ()
  "" (interactive) (scroll-down 3))
(defun scroll-up-with-lines ()
  "" (interactive) (scroll-up 3))
(global-set-key [wheel-up] 'scroll-down-with-lines)
(global-set-key [wheel-down] 'scroll-up-with-lines)
(global-set-key [double-wheel-up] 'scroll-down-with-lines)
(global-set-key [double-wheel-down] 'scroll-up-with-lines)
(global-set-key [triple-wheel-up] 'scroll-down-with-lines)
(global-set-key [triple-wheel-down] 'scroll-up-with-lines)
