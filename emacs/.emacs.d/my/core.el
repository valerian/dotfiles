;; load path
(add-to-list 'load-path "~/.emacs.d/load/")

;; package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) 

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

;; autocomplete menus
(icomplete-mode 99)

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

;; line numbers mode
(global-linum-mode 1)

;; same as "C-x o" but backwards
(global-set-key (kbd "C-x p") 'previous-multiframe-window)

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
