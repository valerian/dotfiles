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
  :config (bracketed-paste-enable))

;; move region with M-up and M-down
(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(global-set-key (kbd "M-<up>") 'move-region-up)
(global-set-key (kbd "M-<down>") 'move-region-down)


(provide 'init-util)

