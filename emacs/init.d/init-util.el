;; undo tree
(req-package undo-tree
  :config
  (global-undo-tree-mode)
  (global-set-key (kbd "C-.") 'undo-tree-redo))

;; line numbers mode
(req-package nlinum
  :config
  (global-nlinum-mode 1)
  (global-set-key (kbd "C-x l") 'nlinum-mode))

;; smex
(req-package smex
  :config
  (global-set-key
   [(meta x)]
   (lambda ()
     (interactive)
     (or (boundp 'smex-cache)
         (smex-initialize))
     (global-set-key [(meta x)] 'smex)
     (smex)))
  (global-set-key
   [(shift meta x)]
   (lambda ()
     (interactive)
     (or (boundp 'smex-cache)
         (smex-initialize))
     (global-set-key [(shift meta x)] 'smex-major-mode-commands)
     (smex-major-mode-commands))))

;; neotree
(req-package neotree
  :defer t
  :bind ([f8] . neotree-toggle))

;; bracketed paste
(req-package bracketed-paste
  :defer t
  :config (bracketed-paste-enable))

(provide 'init-util)
