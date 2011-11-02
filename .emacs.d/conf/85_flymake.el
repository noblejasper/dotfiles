; flymake
; http://d.hatena.ne.jp/nyaasan/20071216/p1 (in Japanese)
; http://d.hatena.ne.jp/nushio/20071201 (in Japanese)
; http://www.emacswiki.org/cgi-bin/wiki/PythonMode
;; (require 'flymake)

;; (defun flymake-show-and-sit ()
;;   "Displays the error/warning for the current line in the minibuffer"
;;   (interactive)
;;   (progn
;;     (let* ( (line-no             (flymake-current-line-no) )
;;             (line-err-info-list  (nth 0 (flymake-find-err-info 
;;                                          flymake-err-info line-no)))
;;             (count               (length line-err-info-list))
;;             )
;;       (while (> count 0)
;;         (when line-err-info-list
;;           (let* ((file       (flymake-ler-file (nth (1- count) 
;;                                                     line-err-info-list)))
;;                  (full-file  (flymake-ler-full-file (nth (1- count)
;;                                                          line-err-info-list)))
;;                  (text (flymake-ler-text (nth (1- count) line-err-info-list)))
;;                  (line       (flymake-ler-line (nth (1- count) 
;;                                                     line-err-info-list))))
;;             (message "[%s] %s" line text)
;;             )
;;           )
;;         (setq count (1- count)))))
;;   (sit-for 60.0)
;;   )
;; (global-set-key "\C-cd"
;;                 'flymake-show-and-sit)

; You must prepare epylint by hand
; See also http://www.emacswiki.org/cgi-bin/wiki/PythonMode
;; (when (load "flymake" t)
;;       (defun flymake-pylint-init ()
;;         (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                            'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;           (list "epylint" (list local-file))))
    
;;       (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pylint-init)))


;; (defun flymake-pylint-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                      'flymake-create-temp-inplace))
;;          (local-file (file-relative-name
;;                       temp-file
;;                       (file-name-directory buffer-file-name))))
;;     (list "epylint" (list local-file))))
;; (push '("\\.py\\'" flymake-pylint-init) flymake-allowed-file-name-masks)

;; (defconst flymake-allowed-python-file-name-masks '(("\\.py$" flymake-pylint-init)
;;                                                ))

;; (defun flymake-python-load ()
;;   (interactive)
;;   (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
;;     (setq flymake-check-was-interrupted t))
;;   (ad-activate 'flymake-post-syntax-check)
;;   (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-python-file-name-masks))
;;   (setq flymake-err-line-patterns flymake-err-line-patterns)
;;   (flymake-mode t))

;; (defun flymake-python-load ()
;;   (interactive)
;;   (push '("\\.py\\'" flymake-pylint-init) flymake-allowed-file-name-masks)
;;   (flymake-mode t))

;; (add-hook 'python-mode-hook
;;           '(lambda ()
;;              (flymake-python-load t)))

;; (defun next-flymake-error ()
;;   (interactive)
;;   (flymake-goto-next-error)
;;   (let ((err (get-char-property (point) 'help-echo)))
;;     (when err
;;       (message err))))
;; (global-set-key "\M-e" 'next-flymake-error)
