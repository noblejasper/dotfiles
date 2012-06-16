;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; シンタックスハイライトを有効にする
(global-font-lock-mode t)

;; 対応する括弧をハイライト表示
(show-paren-mode t)

;; いろんな括弧をハイライト表示
;; http://www.fan.gr.jp/~ring/Meadow/meadow.html#mic-paren.el
;; (autoload 'paren-activate "mic-paren" nil t)
;; (setq paren-match-face 'bold paren-sexp-mode t)

;; リージョンをハイライト表示
(setq transient-mark-mode t)

;; 対話置換でマッチした箇所をハイライト
(setq query-replace-highlight t)

;; 色の設定
(global-font-lock-mode t)
(setq-default transient-mark-mode t)
(require 'font-lock)
(set-face-bold-p 'modeline nil)

;; search している単語を highlight する
(setq search-highlight t)
