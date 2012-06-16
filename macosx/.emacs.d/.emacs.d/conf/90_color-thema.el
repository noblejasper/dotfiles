;; customize 保存用ファイル
(setq custom-file "~/.emacs.d/conf/cocoa-emacs-99customize.el")

;; color-theme
(require 'color-theme)
(load "nobjas-color-theme")
(nobjas-color-theme)
;(require 'color-theme-inkpot)
(color-theme-initialize)
;(color-theme-inkpot)
;(color-theme-arjen)
;(color-theme-calm-forest)

(setq default-frame-alist
      (append (list
               ;;; default color
               '(background-color . "black") ; 背景色
               '(foreground-color . "white")  ; 文字色
               ;;; cursor
               '(cursor-color . "snow4") ; カーソル色
               '(cursor-type . box)     ; カーソル形状
               ;; '(cursor-height . 4)     ; カーソルの高さ
               ;;; mouse cursor
               '(mouse-color . "white") ; マウスカーソル色
               ;;; border
               '(border-color . "yellow") ; 縁の色
               ;;; scroll bar
               '(vertical-scroll-bars . nil) ; スクロールバー
               ;;; size
               '(width . 83)  ; 横幅(桁数)
               '(height . 53) ; 高さ(行数)
               ;;; location
               '(left . 0) ; 左上隅 x 座標
               '(top . 0)    ; 左上隅 y 座標
               )
              default-frame-alist))

;; 色の設定
(global-font-lock-mode t)
(transient-mark-mode t)
(set-face-bold-p 'modeline nil)
;; http://norainu.net/mt/archives/2007/05/emacs_22.html
(set-face-foreground 'font-lock-string-face  "RoyalBlue3")
(set-face-foreground 'font-lock-doc-face "LightGoldenrod2")
(set-face-foreground 'font-lock-keyword-face "turquoise3")
(set-face-foreground 'font-lock-function-name-face "Yellow")
(set-face-foreground 'font-lock-builtin-face "DodgerBlue")
(set-face-foreground 'font-lock-type-face "gold2")
(set-face-foreground 'font-lock-warning-face "yellow")
(set-face-foreground 'font-lock-builtin-face "goldenrod")
(set-face-background 'highlight "yellow")
(set-face-background 'lazy-highlight "gold2")
(set-face-foreground 'lazy-highlight "black")
(set-face-foreground 'highlight "black")
(set-face-background 'region "RosyBrown1")
(set-face-foreground 'region "blue")
(set-face-foreground 'modeline "yellow")
(set-face-background 'modeline "grey19")
(set-face-foreground 'font-lock-comment-delimiter-face "grey47")
(set-face-foreground 'font-lock-comment-face "grey47")

;; 非アクティブなモードラインの（Emacs22 の時だけ設定）
(set-face-foreground 'mode-line-inactive "grey51")
(set-face-background 'mode-line-inactive "grey19")
;; ミニバッファ (22 の時だけ設定）
(set-face-foreground 'minibuffer-prompt "cyan1")

;; sense-region
;; (sense-region-face ((t (:background "#3333ff"))))

;;; cursor 位置の face を調べる関数
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))
