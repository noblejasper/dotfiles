;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(defun increment-string-as-number (number)
  "Replace progression string of the position of the cursor
by string that added NUMBER.
Interactively, NUMBER is the prefix arg.

examle:
  At the cursor string \"12\"

  M-x increment-string-as-number        ;; replaced by \"13\"
  C-u 10 M-x increment-string-as-number ;; replaced by \"22\"

  At the cursor string \"-12\"

  M-x increment-string-as-number         ;; replaced by \"-11\"
  C-u 100 M-x increment-string-as-number ;; replaced by \"88\""
  (interactive "P")
  (let ((col (current-column))
        (p (if (integerp number) number 1)))
    (skip-chars-backward "-0123456789")
    (or (looking-at "-?[0123456789]+")
        (error "No number at point"))
    (replace-match
     (number-to-string (+ p (string-to-number (match-string 0)))))
    (move-to-column col)))
(defun decrement-string-as-number (number)
  (interactive "P")
  (increment-string-as-number (if number (- number) -1)))
(global-set-key (kbd "C-+") 'increment-string-as-number)
(global-set-key (kbd "C--") 'decrement-string-as-number)

(global-set-key "\C--" 'decrement-string-as-number)
(global-set-key "\C--" 'decrement-string-as-number)


