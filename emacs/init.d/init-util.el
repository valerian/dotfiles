;; cheat sheet
(req-package cheatsheet
  :config
  (global-set-key (kbd "<f1>") 'cheatsheet-show)
  (define-key cheatsheet-mode-map (kbd "<f1>") 'kill-buffer-and-window)
  (define-key cheatsheet-mode-map (kbd "C-g") 'kill-buffer-and-window)
  (cheatsheet-add :group "Utils" :key "C-S-<left>" :description "Previous buffer.")
  (cheatsheet-add :group "Utils" :key "C-S-<right>" :description "Next buffer.")
  (cheatsheet-add :group "Utils" :key "M-x m" :description "Toggle xterm mouse mode.")
  (cheatsheet-add :group "Core" :key "M-<up>" :description "Move region or current line up.")
  (cheatsheet-add :group "Core" :key "M-<down>" :description "Move region or current line down.")
)

;; smart-mode-line
(req-package smart-mode-line
  :defer 1
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'dark)
  :config
  (sml/setup))

;; ws-butler (an unobtrusive way to trim spaces from end of line)
(req-package ws-butler
  :config
  (ws-butler-global-mode 1))

;; volatile-highlight (highlight recent changes)
(req-package volatile-highlights
  :defer 1
  :config
  (volatile-highlights-mode 1))

;; smex
(req-package smex
  :config
  (smex-initialize))

;; ivy mode
(req-package ivy
  :after smex
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (define-key ivy-mode-map (kbd "M-x") 'counsel-M-x)
  (define-key ivy-mode-map (kbd "C-x C-f") 'counsel-find-file)
  ;(define-key ivy-mode-map (kbd "<f1> f") 'counsel-describe-function)
  ;(define-key ivy-mode-map (kbd "<f1> v") 'counsel-describe-variable)
  ;(define-key ivy-mode-map (kbd "<f1> l") 'counsel-find-library)
  ;(define-key ivy-mode-map (kbd "<f2> i") 'counsel-info-lookup-symbol)
  ;(define-key ivy-mode-map (kbd "<f2> u") 'counsel-unicode-char)
  )

;; projectile mode
(setq projectile-ignored-extension-list
      '("editorconfig" "eslintignore" "eslintrc" "git" "gitignore" "projectile" "sailsrc" "tern-port"
        "tern-project" "tmp" "meta" "png" "jpg" "tga" "ttf" "TTF" "asset"))
(setq projectile-ignored-path-list
      '("Assets/PostProcessing/" "Assets/Plugins/"))
(req-package projectile
  :after (ivy)
  :config
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-git-command
        (concat
         "git ls-files -zco --exclude-standard | sed -r 's/[^\\x0]*?\\.("
         (string-join projectile-ignored-extension-list "|")
         ")\\x0//g' | sed -r 's/($|\\x0)("
         (replace-regexp-in-string "/" "\\\\/" (string-join projectile-ignored-path-list "|"))
         ")[^\\x0]*//g'"))
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(req-package counsel-projectile
  :after (ivy projectile)
  :config
  (counsel-projectile-mode 1)
  (global-set-key (kbd "C-x C-b") 'counsel-projectile))

;; tabs bar
(req-package tabbar
  :defer 1
  :config
  (tabbar-mode)
  (set-face-attribute
   'tabbar-default nil
   :background "gray20"
   :foreground "gray50")
  (set-face-attribute
   'tabbar-unselected nil
   :background "gray30"
   :foreground "white")
  (set-face-attribute
   'tabbar-selected nil
   :background "gray75"
   :foreground "black")
  (set-face-attribute
   'tabbar-highlight nil
   :background "white"
   :foreground "black"
   :underline nil)
  (set-face-attribute
   'tabbar-button nil)
  (set-face-attribute
   'tabbar-separator nil
   :background "gray20")
  (custom-set-variables
   '(tabbar-separator (quote (1)))))

;; undo tree
(req-package undo-tree
  :after (cheatsheet)
  :config
  (global-undo-tree-mode)
  (global-set-key (kbd "C-.") 'undo-tree-redo)
  (cheatsheet-add :group "Utils" :key "C-x u" :description "Visualize the undo tree."))

;; line numbers mode
(defun my-nlinum-mode-hook ()
  (when nlinum-mode
    (setq-local nlinum-format
                (concat "%" (number-to-string
                             ;; Guesstimate number of buffer lines.
                             (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
                        "d│"))))
(req-package nlinum
  :after (cheatsheet)
  :config
  (global-nlinum-mode 1)
  (global-set-key (kbd "C-x l") 'nlinum-mode)
  (cheatsheet-add :group "Utils" :key "C-x l" :description "Toggle nlinum-mode (Line Numbers).")
  (add-hook 'nlinum-mode-hook #'my-nlinum-mode-hook))

;; neotree
(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find project root."))))
(req-package neotree
  :after (cheatsheet projectile)
  :defer 1
  :bind (([f8] . neotree-toggle)
         ([f9] . neotree-project-dir))
  :config
  (setq neo-window-width 40)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-hidden-regexp-list '("^\\." "\\.meta$" "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.o$"))
  (cheatsheet-add :group "Utils" :key "[f8]" :description "Toggle neotree side menu.")
  (cheatsheet-add :group "Utils" :key "[f9]" :description "Toggle neotree side menu, with projectile's project root."))

;; multiple cursors
(req-package multiple-cursors
  :after (cheatsheet)
  :config
  (global-set-key (kbd "C-x C-=") 'mc/edit-lines)
  (global-set-key (kbd "C-=") 'mc/mark-next-like-this-symbol)
  (global-set-key (kbd "C--") 'mc/mark-previous-like-this-symbol)
  (global-set-key (kbd "C-+") 'mc/mark-all-like-this)
  (cheatsheet-add :group "Multiple Cursors" :key "C-x C-=" :description "Add cursor for all lines in region.")
  (cheatsheet-add :group "Multiple Cursors" :key "C-=" :description "Add cursor for next similar to symbol or region.")
  (cheatsheet-add :group "Multiple Cursors" :key "C--" :description "Add cursor for previous similar to symbol or region.")
  (cheatsheet-add :group "Multiple Cursors" :key "C-+" :description "Add cursor for all similar to region."))

;; bracketed paste
(req-package bracketed-paste
  :defer 2
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

