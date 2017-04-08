(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'sanityinc-tomorrow-bright t)

(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (propertize (format (format "%%%dd|" w) line) 'face 'linum)))
(setq linum-format 'linum-format-func)

(custom-set-variables
 '(custom-safe-themes
   (quote
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "a0df8eb250b90cce1fb3215c3d5c3b0c7e23a0fbd086f58cdcb9da45bba69781" "d69a0f6d860eeff5ca5f229d0373690782a99aee2410a3eed8a31332a7101f1e" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" default))))

(custom-set-faces
 '(linum ((t (:background "#000000" :foreground "#333333" :underline nil :slant normal))))
 '(web-mode-html-tag-face ((t (:foreground "color-32")))))
