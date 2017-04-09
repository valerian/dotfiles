;; windmove
;(when (fboundp 'windmove-default-keybindings)
;    (windmove-default-keybindings 'control))

;; smex
(use-package smex)
(global-set-key [(meta x)] (lambda ()
                             (interactive)
                             (or (boundp 'smex-cache)
                                 (smex-initialize))
                             (global-set-key [(meta x)] 'smex)
                             (smex)))

(global-set-key [(shift meta x)] (lambda ()
                                   (interactive)
                                   (or (boundp 'smex-cache)
                                       (smex-initialize))
                                   (global-set-key [(shift meta x)] 'smex-major-mode-commands)
                                   (smex-major-mode-commands)))

;; neotree
(use-package neotree)
(global-set-key [f8] 'neotree-toggle)

;; bracketed paste
(use-package bracketed-paste)
(bracketed-paste-enable)

