;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(set-default-font "M+2VM+IPAG circle-24")
(set-face-font 'variable-pitch "M+2VM+IPAG circle-24")
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  '("M+2VM+IPAG circle" . "unicode-bmp")
)

;; Font
;; (setq my-font "-apple-M+1VM+IPAG-medium-r-normal--20-*-*-*-*-*-fontset-hiramaru")
;; (setq mac-allow-anti-aliasing t)
;; (if (= emacs-major-version 22)
;;     (require 'carbon-font))
;; (set-default-font my-font)
;; (add-to-list 'default-frame-alist `(font . ,my-font))
;; (when (= emacs-major-version 23)
;;   (set-fontset-font
;;    (frame-parameter nil 'font)
;;    'japanese-jisx0208
;;    '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
;;   (setq face-font-rescale-alist
;;     '(("^-apple-M+1VM+IPAG.*" . 1.2)
;;       (".*osaka-bold.*" . 1.2)
;;       (".*osaka-medium.*" . 1.2)
;;       (".*courier-bold-.*-mac-roman" . 1.0)
;;       (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
;;       (".*monaco-bold-.*-mac-roman" . 0.9)
;;       ("-cdac$" . 1.3))))

;; ; フォント設定
;; (when window-system
;;   (progn
;;     (set-default-font "M+1VM+IPAG circle-24")))
;;     (set-fontset-font (frame-parameter nil 'font)
;;                       'japanese-jisx0208
;;                       '("M+1VM+IPAG circle-24" . "unicode-bmp"))))
