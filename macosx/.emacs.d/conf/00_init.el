;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; OSを判別
(defvar run-linux
  (equal system-type 'gnu/linux))
(defvar run-darwin
  (equal system-type 'darwin))

;; Windowシステムを判別
(defvar run-cli
  (equal window-system nil))
(defvar run-cocoa
  (equal window-system 'ns))
(defvar run-carbon
  (equal window-system 'mac))

;; スタートアップ時のメッセージを抑制
(setq inhibit-startup-message t)

;; start server
(server-start)

; emacsclient で Buffer `hogehoge' still has clients; kill it? (yes or no) とかいわれるのがうざいのをなおす
; http://aki.issp.u-tokyo.ac.jp/itoh/hiChangeLog/html/2007-04.html#2007-04-09-1
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; backup & autosave は temporary に
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; メニューバーとツールバーoff
(tool-bar-mode 0)
(if (or run-cocoa run-carbon) (menu-bar-mode 1) (menu-bar-mode 0)) ; GUIでは表示

;; ヴィジュアルベル無効
;; (setq visible-bell nil)

;; ビープ音も無効
;; (setq ring-bell-function '(lambda ()))

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

;; C-hでバックスペース
(global-set-key "\C-h" 'delete-backward-char)

;; スクロールバー非表示
; (scroll-bar-mode nil)

;; フリンジ(左右の折り返し表示領域)はいらない
(fringe-mode 'none)

;; 色つける
(setq-default transient-mark-mode t)
(require 'font-lock)

;; utf-8優先
(prefer-coding-system 'utf-8)

;; tamago
;; (setq default-input-method 'japanese-egg-anthy)

;; 物理行単位でスクロール
;; http://www.bookshelf.jp/soft/meadow_31.html#SEC418
(load "ce-scroll.el")
(setq ce-smooth-scroll nil)
;; 一行ずつスクロール
(setq scroll-step 1)

;; iswitchb & uniquify
(iswitchb-mode 1)
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; 縦分割とかでも行を折り返す
(setq truncate-partial-width-windows nil)

;; カーソル点滅しないように
(blink-cursor-mode t)
(setq blink-cursor-interval 0.15)

;; アクティブでないバッファではカーソルを出さない
(setq cursor-in-non-selected-windows nil)

;; recenf-mode
(recentf-mode t)

;; 時刻の表示( 曜日 月 日 時間:分 )
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time-mode t)

;; ;; ref: WEB+DB PRESS vol.58 p.78
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
;; (set-face-background 'show-paren-match-face nil)
;; (set-face-underline-p 'show-paren-match-face "red")
;; (set-face-background 'show-paren-mismatch-face "#ff0000")
;; (set-face-underline-p 'show-paren-mismatch-face nil)

;;
;; redo+
;;   ref:「Emacsテクニックバイブル」 p.123
;;
;; (require 'redo+)
;; (global-unset-key [C-M-/])
;; (global-set-key [C-M-/] 'redo)
;; (setq undo-no-redo t) ; 過去のundoがredoされないようにする
;; 大量のundoに耐えられるようにする
(setq undo-limit 600000)
(setq undo-strong-limit 700000)


;;
;; sense-region
;;   ref:「Emacsテクニックバイブル」 p.124
;;
;; http://blog.livedoor.jp/k1LoW/archives/65030864.html
(require 'sense-region)
(sense-region-on)

;;
;; migemo
;;   ref:「Emacsテクニックバイブル」 p.113
;;
;; (require 'migemo)

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

;; http://d.hatena.ne.jp/m2ym/20110120/1295524932
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

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