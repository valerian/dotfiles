;; load path
(add-to-list 'load-path "~/.emacs.d/init.d/")

;; package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) 

;(require 'benchmark-init)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(unless (package-installed-p 'req-package)
  (package-refresh-contents)
  (package-install 'req-package))

(require 'use-package)
(require 'req-package)

(require 'init-core)
(require 'init-theme)
(require 'init-util)
(require 'init-code)

(req-package-finish)
