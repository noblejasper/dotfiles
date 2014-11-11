(require 'flymake)
(require 'perl-completion)

;(setq plcmp-debug t)
;(setq flymake-log-level 3)


;; disable GUI warnings
(setq flymake-gui-warnings-enabled t)

(defun flymake-perl-init ()
  (plcmp-with-set-perl5-lib
   (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                        'flymake-create-temp-inplace))
          (local-file  (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name)))
          (perl5lib (split-string (or (getenv "PERL5LIB") "") ":"))
          (args '("-wc")))
     (progn
       (dolist (lib perl5lib)
         (unless (equal lib "")
           (add-to-list 'args (concat "-I" lib) t)))
       (add-to-list 'args local-file t)
       (list "perl" args)))))

(setq flymake-allowed-file-name-masks
      (cons '("\\.\\(t\\|p[ml]\\|psgi\\)$"
              flymake-perl-init
              flymake-simple-cleanup
              flymake-get-real-file-name)
            flymake-allowed-file-name-masks))

(add-hook 'cperl-mode-hook
          '(lambda () (flymake-mode t)))

;; python
(defun flymake-python-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    ;; (epy-setup-checker "~/bin/check_pyflakes %f")
    (list "/usr/local/bin/pyflakes" (list local-file))))

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
