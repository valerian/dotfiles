;; flycheck
(use-package flycheck)


;; company-mode
(use-package company)
(use-package auto-complete)
(global-set-key (kbd "M-'") 'company-complete)
(setq company-tooltip-align-annotations t)


;; web-mode
(use-package web-mode)
(use-package emmet-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-quoting t)
  (setq web-mode-enable-auto-opening t)
  (emmet-mode +1)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (tide-setup)
              (flycheck-mode +1)
              (setq flycheck-check-syntax-automatically '(save mode-enabled))
              (eldoc-mode +1)
              (company-mode-on))
            )
          )


;; Javascript
(setq js-indent-level 2)


;; Typescript
(use-package typescript-mode)
(use-package tide)
(global-set-key (kbd "M-;") 'tide-rename-symbol)
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)
            (electric-pair-mode +1)
            (electric-layout-mode +1)
            (tide-hl-identifier-mode +1)
            (company-mode +1)
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(custom-set-variables
 '(typescript-indent-level 2))
(setq tide-format-options
      '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
        :placeOpenBraceOnNewLineForFunctions nil
        :insertSpaceBeforeAndAfterBinaryOperators nil
        :tabSize 4
        :indentSize 2))


;; Angular 2
(use-package ng2-mode)
(add-to-list 'auto-mode-alist '("\\.module.ts\\'" . ng2-ts-mode))


;; SCSS - SASS
(use-package sass-mode)
(use-package scss-mode)
(use-package flymake-sass)
(add-hook 'scss-mode-hook
          (lambda ()
            (setq-default scss-compile-at-save nil)
            ;(flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (electric-pair-mode +1)
            (electric-layout-mode +1)
            (company-mode +1)
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))


;; Python
(use-package company-jedi)
(use-package jedi)
(add-hook 'python-mode-hook
          (lambda ()
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (jedi:setup)
            (company-jedi +1)
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))


