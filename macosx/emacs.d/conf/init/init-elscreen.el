;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; elscreen
(load "elscreen" "ElScreen" t)
(elscreen-start)
;; (define-key global-map (kbd "C-z") nil)
;; (setq elscreen-prefix-key "\C-z")
(global-set-key [f12] 'elscreen-next)
(global-set-key [f11] 'elscreen-previous)
(global-set-key [f10] 'elscreen-create)

(defun my-tabbar-select-tab-by-number (n)
  "Select Nth tab."
  (interactive "p")
  (let* ((tabset (tabbar-current-tabset t))
         (tab (tabbar-selected-tab tabset))
         previous)
    (when (and tabset (numberp n) (<= 1 n))
      (while (setq previous (tabbar-tab-next tabset tab t))
        (setq tab previous))
      (loop for i from 1 below n
            do (setq tab (tabbar-tab-next tabset tab))
            unless (tabbar-tab-next tabset tab) return nil)
      (tabbar-click-on-tab tab))))

(loop for i from 1 to 9
      for fn = (intern (format "my-tabbar-select-tab-%d" i))
      do
      (fset fn `(lambda () (interactive) (my-tabbar-select-tab-by-number ,i)))
      (global-set-key (read-kbd-macro (format "s-%d" i)) fn))
