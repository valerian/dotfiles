;; c-style indentation defaults
(setq c-default-style "k&r"
      c-basic-offset 4)

;; flycheck
(req-package flycheck)

;; company-mode
(req-package company
  :after (cheatsheet)
  :init
  (setq company-tooltip-align-annotations t)
  (global-set-key (kbd "M-'") 'company-complete)
  (cheatsheet-add :group "Utils" :key "M-'" :description "Autocomplete menu (company-mode).")
)

;; web-mode
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-quoting t)
  (setq web-mode-enable-auto-opening t)
  (company-mode +1)
  (emmet-mode +1))
(req-package web-mode
  :mode ("\\.phtml\\'" . web-mode)
  :mode ("\\.tpl\\.php\\'" . web-mode)
  :mode ("\\.[agj]sp\\'" . web-mode)
  :mode ("\\.as[cp]x\\'" . web-mode)
  :mode ("\\.erb\\'" . web-mode)
  :mode ("\\.mustache\\'" . web-mode)
  :mode ("\\.djhtml\\'" . web-mode)
  :mode ("\\.html?\\'" . web-mode)
  :mode ("\\.tsx\\'" . web-mode)
  :mode ("\\.css\\'" . web-mode)
  :config
  (add-hook 'web-mode-hook 'my-web-mode-hook))

;; emmet mode
(req-package emmet-mode
  :defer 1
  :after (web-mode cheatsheet)
  :config
  (cheatsheet-add :group "HTML" :key "C-j" :description "Expand Emmet snippet."))

;; Javascript
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(req-package js2-mode
  :mode ("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (js2-refactor-mode +1)
            (define-key js2-refactor-mode-map (kbd "C-x C-r") 'js2r-rename-var)
            (cheatsheet-add :group "Javascript" :key "C-x C-r" :description "Refactor rename variable.")
            (js2r-add-keybindings-with-prefix "C-c C-r")
            (add-to-list 'company-backends 'company-tern)
            (tern-mode)
            (company-mode)
            (electric-pair-mode +1)
            ))
(req-package js2-refactor
  :defer 1
  :after js2-mode)
(req-package company-tern
  :defer t
  :after company)

;; Javascript: React
(req-package rjsx-mode
  :mode ("\\.jsx\\'" . rjsx-mode))
(add-hook 'rjsx-mode-hook
          (lambda ()
            (emmet-mode +1)
            (add-to-list 'company-backends 'company-tern)
            (tern-mode)
            (company-mode)
            (electric-pair-mode +1)
            ))


;; Typescript
(req-package typescript-mode
  :mode ("\\.ts\\'" . typescript-mode))
(req-package tide
  :after (typescript-mode company))
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)
            ;(electric-pair-mode +1)
            (electric-layout-mode +1)
            (tide-hl-identifier-mode +1)
            (company-mode +1)
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)
            (global-set-key (kbd "M-;") 'tide-rename-symbol)))
(custom-set-variables
 '(typescript-indent-level 2))
(setq tide-format-options
      '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
        :placeOpenBraceOnNewLineForFunctions nil
        :insertSpaceBeforeAndAfterBinaryOperators nil
        :tabSize 4
        :indentSize 2))

;; Angular 2
(req-package ng2-mode
  :mode ("\\.module.ts\\'" . ng2-ts-mode)
  :mode ("\\.component.ts\\'" . ng2-ts-mode)
  :mode ("\\.service.ts\\'" . ng2-ts-mode)
  :mode ("\\.component.html\\'" . ng2-html-mode))

;; SCSS - SASS
(req-package sass-mode
  :mode ("\\.sass\\'" . sass-mode))
(req-package scss-mode
  :mode ("\\.scss\\'" . scss-mode)
  :after company
  :config
  (add-hook 'scss-mode-hook
            (lambda ()
              (setq-default scss-compile-at-save nil)
              (electric-pair-mode +1)
              (company-mode +1)
              (add-to-list 'write-file-functions 'delete-trailing-whitespace))))

;; Python
(req-package python-mode
  :mode ("\\.py\\'" . python-mode))

;; php
(req-package php-mode
  :mode ("\\.php\\'" . php-mode))
(add-hook 'php-mode-hook
          (lambda ()
            (define-key php-mode-map (kbd "C-.") nil)))

;; yaml
(req-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

;; paradox
(req-package paradox
  :ensure f
  :after company
  :mode ("/\\(STELLARIS\\|stellaris\\|Stellaris\\).*/common/.*\\.txt\\'" . paradox-mode))
(add-hook 'paradox-mode-hook
          (lambda ()
            (company-mode +1)))
(req-package smali-mode
  :ensure f
  :mode ("\\.smali\\'" . smali-mode))

;; Markdown
(req-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Csharp
(req-package csharp-mode
  :mode ("\\.cs\\'" . csharp-mode)
  :after (company omnisharp flycheck)
  :config
  (add-hook 'csharp-mode-hook
            (lambda ()
              (add-to-list 'company-backends #'company-omnisharp)
              (omnisharp-mode +1)
              (company-mode +1)
              (flycheck-mode +1))))
(req-package omnisharp
  :mode ("\\.csproj\\'" . omnisharp-mode)
  :config
  (if (eq system-type 'cygwin)
      (setq omnisharp-server-executable-path "/cygdrive/c/Bin/omnisharp-mono/run.sh"))
      ;;#!/bin/sh
      ;;exec /cygdrive/c/Program\ Files/Unity/Editor/Data/MonoBleedingEdge/bin/mono.exe "C:\\Bin\\omnisharp-mono\\OmniSharp.exe" "$@"
  (add-hook 'csharp-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
              (local-set-key (kbd "C-c C-c") 'recompile))))

(provide 'init-code)
