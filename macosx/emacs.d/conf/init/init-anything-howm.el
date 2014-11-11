(require 'anything-howm)

(setq ah:recent-menu-number-limit 600)
(global-set-key (kbd "C-2") 'ah:menu-command)
(global-set-key (kbd "C-3") 'ah:cached-howm-menu)

(add-to-list 'anything-sources 'anything-c-source-buffers+-howm-title)
