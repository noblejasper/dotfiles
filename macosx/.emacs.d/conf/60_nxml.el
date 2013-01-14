;; (setq auto-mode-alist
;;       (append '(
;;                 ;;("\\.\\(html\\|xhtml\\|shtml\\|tpl\\)\\'" . xml-mode)
;;                 ("\\.\\(html\\|xhtml\\|shtml\\|tpl\\)\\'" . nxml-mode)
;;                 ("\\.php\\'" . php-mode)
;;                 )
;;               auto-mode-alist))

(load "rng-auto.el" 't)
(add-hook 'nxml-mode-hook
          (lambda ()
            ;; 更新タイムスタンプの自動挿入
            (setq time-stamp-line-limit 10000)
            (if (not (memq 'time-stamp write-file-hooks))
                (setq write-file-hooks
                      (cons 'time-stamp write-file-hooks)))
            (setq time-stamp-format "%3a %3b %02d %02H:%02M:%02S %:y %Z")
            (setq time-stamp-start "Last modified:[ \t]")
            (setq time-stamp-end "$")
            ;;
            (setq auto-fill-mode -1)
            (setq nxml-slash-auto-complete-flag t)      ; スラッシュの入力で終了タグを自動補完
            (setq nxml-child-indent 4)                  ; タグのインデント幅
            (setq nxml-attribute-indent 4)              ; 属性のインデント幅
            (setq indent-tabs-mode nil)
            (setq nxml-bind-meta-tab-to-complete-flag t) 
            (setq nxml-slash-auto-complete-flag t)      ; </の入力で閉じタグを補完する
            (setq nxml-sexp-element-flag t)             ; C-M-kで下位を含む要素全体をkillする
            (setq nxml-char-ref-display-glyph-flag nil) ; グリフは非表示
            (setq tab-width 4)))
