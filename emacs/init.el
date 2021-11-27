(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; load path
(add-to-list 'load-path (expand-file-name "init.d/" (file-name-directory load-file-name)))

;; disable loading "default.el" at startup
(setq inhibit-default-init t)

;; package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; disable installed packages tracking (it's polluting init.el for no valid reason)
(defun package--save-selected-packages (&rest opt) nil)

;; benchmark
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
