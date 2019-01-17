;; cheat sheet
(req-package cheatsheet
  :config
  (global-set-key (kbd "<f1>") 'cheatsheet-show)
  (define-key cheatsheet-mode-map (kbd "<f1>") 'kill-buffer-and-window)
  (define-key cheatsheet-mode-map (kbd "C-g") 'kill-buffer-and-window))

;; prevent backspace issues with xterm
(keyboard-translate ?\C-h ?\C-?)

;; rotating auto files backup within home subfolder
(make-directory "~/.emacs_backups/" t)
(setq
   backup-by-copying t      ; don't clobber symlinks
   make-backup-files t
   backup-directory-alist
    '((".*" . "~/.emacs_backups/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 12
   kept-old-versions 4
   version-control t)       ; use versioned backups

;; no auto-save
(setq auto-save-default nil)

;; no startup message
(setq inhibit-startup-message t)

;; truncate line display when too long (prevents buffer horizontal scrolling)
(setq-default truncate-partial-width-windows nil)

;; no limit to number of open buffers
(setq buffers-menu-max-size nil)

;; force using spaces in place of tabs
(setq-default indent-tabs-mode nil)

;; prefer utf8
(prefer-coding-system 'utf-8-unix)

;; no bell sound
(setq ring-bell-function 'ignore)

;; replace yes-no prompts with y-n
(fset 'yes-or-no-p 'y-or-n-p)

;; display column number on status bar
(column-number-mode t)

;; smart menu completion
(ido-mode t)

;; force vertical split when starting emacs with several files
(setq split-height-threshold nil)
(setq split-width-threshold 200)

;; show trailing whitespaces
(setq show-trailing-whitespace t)

;; hide menu bar
(menu-bar-mode -1)

;; highlight matching parentheses
(show-paren-mode 1)

;; kill current buffer
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

;; same as "C-x o" but backwards
(global-set-key (kbd "C-x p") 'previous-multiframe-window)

;; org-mode tweaks
(setq org-startup-indented t)

;; copy region uses current line if no region
(put 'kill-ring-save 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))

;; kill region uses current line if no region
(put 'kill-region 'interactive-form      
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))

;; comment or uncomment region
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

;; toggle xterm mouse mode
(global-set-key (kbd "C-x m") 'xterm-mouse-mode)
(cheatsheet-add :group "Utils" :key "M-x m" :description "Toggle xterm mouse mode.")

;; windmove
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)

;; easily switch buffers
(global-set-key (kbd "C-S-<left>")  'previous-buffer)
(global-set-key (kbd "C-S-<right>") 'next-buffer)
(global-unset-key (kbd "C-x <C-left>"))
(global-unset-key (kbd "C-x <C-right>"))
(cheatsheet-add :group "Utils" :key "C-S-<left>" :description "Previous buffer.")
(cheatsheet-add :group "Utils" :key "C-S-<right>" :description "Next buffer.")

(provide 'init-core)
