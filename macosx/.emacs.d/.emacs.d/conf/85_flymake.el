; flymake
; http://d.hatena.ne.jp/nyaasan/20071216/p1 (in Japanese)
; http://d.hatena.ne.jp/nushio/20071201 (in Japanese)
; http://www.emacswiki.org/cgi-bin/wiki/PythonMode
(require 'flymake)

;; GUIの警告は表示しない
;; (setq flymake-gui-warnings-enabled nil)

;; 全てのファイルで flymakeを有効化
;; (add-hook 'find-file-hook 'flymake-find-file-hook)

(defun flymake-show-and-sit ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (progn
    (let* ( (line-no             (flymake-current-line-no) )
            (line-err-info-list  (nth 0 (flymake-find-err-info 
                                         flymake-err-info line-no)))
            (count               (length line-err-info-list))
            )
      (while (> count 0)
        (when line-err-info-list
          (let* ((file       (flymake-ler-file (nth (1- count) 
                                                    line-err-info-list)))
                 (full-file  (flymake-ler-full-file (nth (1- count)
                                                         line-err-info-list)))
                 (text (flymake-ler-text (nth (1- count) line-err-info-list)))
                 (line       (flymake-ler-line (nth (1- count) 
                                                    line-err-info-list))))
            (message "[%s] %s" line text)
            )
          )
        (setq count (1- count)))))
  (sit-for 60.0)
  )
(global-set-key "\C-cd"
                'flymake-show-and-sit)


;; エラーメッセージをポップアップ表示
(defun flymake-popup-err-message ()
  "Display a menu with errors/warnings for current line if it has errors and/or warnings."
  (interactive)
  (let* ((line-no (flymake-current-line-no))
         (line-err-info-list 
          (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (menu-data
          (flymake-make-err-menu-data line-no line-err-info-list)))
    (if menu-data
      (popup-tip (mapconcat '(lambda (e) (nth 0 e))
                            (nth 1 menu-data)
                            "\n")))))

;; python
(defun flymake-python-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "pyflakes" (list local-file))))

(defconst flymake-allowed-python-file-name-masks '(("\\.py$" flymake-python-init)))
(defvar flymake-python-err-line-patterns '(("\\(.*\\):\\([0-9]+\\):\\(.*\\)" 1 2 nil 3)))
 
(defun flymake-python-load ()
  (interactive)
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-python-file-name-masks))
  (setq flymake-err-line-patterns flymake-python-err-line-patterns)
  (flymake-mode t))
(add-hook 'python-mode-hook '(lambda () (flymake-python-load)))

(defun next-flymake-error ()
  (interactive)
  (flymake-goto-next-error)
  (let ((err (get-char-property (point) 'help-echo)))
    (when err
      (message err))))
(defun prev-flymake-error ()
  (interactive)
  (flymake-goto-prev-error)
  (let ((err (get-char-property (point) 'help-echo)))
    (when err
      (message err))))

(global-set-key (kbd "M-p") 'prev-flymake-error)
(global-set-key (kbd "M-n") 'next-flymake-error)
;; python
(global-set-key "\C-cc" 'flymake-display-err-menu-for-current-line)
(global-set-key "\C-cd" 'flymake-popup-err-message)