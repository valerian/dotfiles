;; load path
(add-to-list 'load-path (expand-file-name "init.d/" (file-name-directory load-file-name)))

;; disable loading "default.el" at startup
(setq inhibit-default-init t)

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
(setq use-package-always-ensure t)
(require 'req-package)

(require 'init-libraries)

(require 'init-core)

(require 'init-util)
(require 'init-code)
(require 'init-theme)

;; key bindings for AZERTY keyboards
;(require 'init-azerty)

(req-package-finish)
