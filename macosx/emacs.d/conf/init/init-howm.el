(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-hook 'markdown-mode-hook
          (lambda()
            (setq markdown-command "mdown") ;; sudo npm install gh-markdown-cli -g
            (define-key markdown-mode-map (kbd "C-i") 'markdown-cycle)))
(add-hook 'markdown-mode-hook 'howm-mode)
(add-to-list 'auto-mode-alist '("\\.howm$" . markdown-mode))

;; howm + Markdown + Plack | nDiki http://www.naney.org/diki/d/2014-03-17-howm-Markdown-Plack.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+nDiki+(nDiki)
(setq howm-view-title-header "#") ;; ← howm のロードより前に書くこと
;; (setq howm-view-title-regexp (format "^\\(%s|=\\)\\( +\\(.*\\)\\|\\)$"
;;                                      (regexp-quote howm-view-title-header)))
;; (setq howm-view-title-regexp-pos 3)
;; (setq howm-view-title-regexp-grep (format "^(%s|=) +"
;;                                           (regexp-quote howm-view-title-header)))


;; marked + markdown-modeの組み合わせが素晴らしい - UNIX的なアレ http://wadap.hatenablog.com/entry/2013/09/15/155342
(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command 
   (format "open -a /Applications/Marked.app %s" 
           (shell-quote-argument (buffer-file-name)))))
(global-set-key "\C-cv" 'markdown-preview-file)

;; キー割当の重複を避ける (お好みで)
(setq howm-prefix "\C-c") ;; howm のキーは「C-c ？」

(require 'howm)
(setq howm-menu-lang 'ja)
;;(global-set-key "\C-c,," 'howm-menu)
(mapc
 (lambda (f)
   (autoload f
     "howm" "Hitori Otegaru Wiki Modoki" t))
 '(howm-menu howm-list-all howm-list-recent
             howm-list-grep howm-create
             howm-keyword-to-kill-ring))

;; 「最近のメモ」一覧時にタイトル表示
(setq howm-list-recent-title t)
;; 全メモ一覧時にタイトル表示
(setq howm-list-all-title t)
;; メニューを 2 時間キャッシュ
(setq howm-menu-expiry-hours 2)

;; howm の時は auto-fill で
;; (add-hook 'howm-mode-on-hook 'auto-fill-mode)
; disable auto-fill-mode
(add-hook 'howm-mode-hook '(lambda () (auto-fill-mode -1)))

;; RET でファイルを開く際, 一覧バッファを消す
;; C-u RET なら残る
(setq howm-view-summary-persistent nil)

;; メニューの予定表の表示範囲
;; 10 日前から
(setq howm-menu-schedule-days-before 10)
;; 3 日後まで
(setq howm-menu-schedule-days 3)

;; howm のファイル名
;; 以下のスタイルのうちどれかを選んでください
;; で，不要な行は削除してください
;; 1 メモ 1 ファイル (デフォルト)
(setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.howm")
;; 1 日 1 ファイルであれば
;(setq howm-file-name-format "%Y/%m/%Y-%m-%d.howm")

(setq howm-view-grep-parse-line
      "^\\(\\([a-zA-Z]:/\\)?[^:]*\\.howm\\):\\([0-9]*\\):\\(.*\\)$")
;; 検索しないファイルの正規表現
(setq howm-excluded-file-regexp
      "/\\.#\\|[~#]$\\|\\.bak$\\|/CVS/\\|\\.doc$\\|\\.pdf$\\|\\.ppt$\\|\\.xls$")

;; いちいち消すのも面倒なので
;; 内容が 0 ならファイルごと削除する
(if (not (memq 'delete-file-if-no-contents after-save-hook))
    (setq after-save-hook
          (cons 'delete-file-if-no-contents after-save-hook)))
(defun delete-file-if-no-contents ()
  (when (and
         (buffer-file-name (current-buffer))
         (string-match "\\.howm" (buffer-file-name (current-buffer)))
         (= (point-min) (point-max)))
    (delete-file
     (buffer-file-name (current-buffer)))))

;; http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?SaveAndKillBuffer
;; C-cC-c で保存してバッファをキルする
(defun my-save-and-kill-buffer ()
  (interactive)
  (when (and
         (buffer-file-name)
         (string-match "\\.howm"
                       (buffer-file-name)))
    (save-buffer)
    (kill-buffer nil)))
(eval-after-load "howm"
  '(progn
     (define-key howm-mode-map
       "\C-c\C-c" 'my-save-and-kill-buffer)))

;; メニューを自動更新しない
(setq howm-menu-refresh-after-save nil)
;; 下線を引き直さない
(setq howm-refresh-after-save nil)
