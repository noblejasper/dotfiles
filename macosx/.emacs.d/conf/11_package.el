;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
(when (require 'package nil t)
  ;; Add the original Emacs Lisp Package Archive
  (add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize))
;; (when (require 'package nil t)
;;   (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;   (package-initialize))