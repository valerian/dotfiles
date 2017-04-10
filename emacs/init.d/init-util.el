;; windmove
;(when (fboundp 'windmove-default-keybindings)
;    (windmove-default-keybindings 'control))

;; smex
(req-package smex)
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
(req-package neotree
  :bind ([f8] . neotree-toggle))

;; bracketed paste
(req-package bracketed-paste
  :defer t
  :config (bracketed-paste-enable))

(provide 'init-util)
