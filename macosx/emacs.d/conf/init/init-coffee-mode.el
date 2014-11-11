;; This gives you a tab of 4 spaces
(custom-set-variables '(coffee-tab-width 4))

;; coffeescript
(custom-set-variables
 '(coffee-tab-width 4)
 '(coffee-args-compile '("-c" "--bare")))

(eval-after-load "coffee-mode"
  '(progn
     (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)
     (define-key coffee-mode-map (kbd "C-j") 'coffee-newline-and-indent)))
