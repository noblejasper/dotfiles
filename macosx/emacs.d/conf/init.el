;; スタートアップ時のメッセージを抑制
(setq inhibit-startup-message t)

;; ヴィジュアルベル無効
(setq visible-bell nil)

;; ビープ音も無効
(setq ring-bell-function '(lambda ()))

;; 行数、列数を表示
(line-number-mode t)
(column-number-mode t)

;; バックアップしない
(setq make-backup-files nil)

;; 自動保存したファイルを削除する
(setq delete-auto-save-files t)

;; 自動セーブしない
(setq auto-save-default nil)

;; リージョンをC-hで削除
(delete-selection-mode 1)

;; インデントはスペースで
(setq-default indent-tabs-mode nil)
;; タブ幅は4
(setq-default tab-width 4)

;; 対応する括弧を光らせる
(show-paren-mode 1)

;; % で対応する括弧に移動
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(define-key global-map (kbd "%") 'match-paren)

;; 色つける
(require 'font-lock)
(global-font-lock-mode t)
(setq-default transient-mark-mode t)

;; utf-8優先
(prefer-coding-system 'utf-8)

;; 一行ずつスクロール
(setq scroll-step 1)

;; 縦分割とかでも行を折り返す
(setq truncate-partial-width-windows nil)

;; recenf-mode
(recentf-mode t)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; カーソル点滅
(blink-cursor-mode t)
(setq blink-cursor-interval 0.15)

;; アクティブでないバッファではカーソルを出さない
(setq cursor-in-non-selected-windows nil)

;; 時刻の表示( 曜日 月 日 時間:分 )
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time-mode t)

(setq undo-limit 600000)
(setq undo-strong-limit 700000)

(setq whitespace-style
      '(tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(require 'whitespace)
(global-whitespace-mode 1)
(set-face-foreground 'whitespace-space "LightSlateGray")
(set-face-background 'whitespace-space "Black")
(set-face-foreground 'whitespace-tab "LightSlateGray")
(set-face-background 'whitespace-tab "Black")

;; http://www.namazu.org/~tsuchiya/elisp/#chmod
(defun make-file-executable ()
  "Make the file of this buffer executable, when it is a script source."
  (save-restriction
    (widen)
    (if (string= "#!" (buffer-substring-no-properties 1 (min 3 (point-max))))
        (let ((name (buffer-file-name)))
          (or (equal ?. (string-to-char (file-name-nondirectory name)))
              (let ((mode (file-modes name)))
                (set-file-modes name (logior mode (logand (/ mode 4) 73)))
                (message (concat "Wrote " name " (+x)"))))))))
(add-hook 'after-save-hook 'make-file-executable)

;; local-variables safe なのは自動で適用
(setq enable-local-variables :safe)

;; start server
(require 'server)
(unless (server-running-p)
  (server-start))

;; 一行ずつスクロール
(setq scroll-step 1)

(defalias 'yes-or-no-p 'y-or-n-p)
