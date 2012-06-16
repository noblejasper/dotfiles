;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; shell-mode
;; パスワードを隠す
;; http://www.namazu.org/~tsuchiya/elisp/#shell-mode
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; エスケープシーケンスを正しく処理する
;; http://www.namazu.org/~tsuchiya/elisp/#shell-mode
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)