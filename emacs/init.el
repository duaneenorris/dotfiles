(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/")
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Add load paths

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/include/")
(add-to-list 'load-path "~/src/snap_dev_tools/den-dotfiles/")

;; Set customize to point to my stuff
(setq custom-file "~/.emacs.d/include/custom.el")
(load custom-file);; Custom set variables

;; Packages

(defvar myPackages
  '(req-package
    elpy
    flycheck
    magit
    py-autopep8
    powerline
    zoom-window
    irony
    company
    company-irony
    company-irony-c-headers
    flycheck-irony
    cc-mode
    clang-format
    undo-tree
    salt-mode
    dumb-jump
    dired-subtree
    dired-ranger
    dired-narrow
    f
    s
    dash
    ))

;; Use this line to update packages without checking signatures
;;(setq package-check-signature nil)

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; Load some env vars from a file
;; setenv-file requires f, s, and dash
(require 'setenv-file)
(setenv-file (expand-file-name "~/.emacs.d/emacs-env"))

(require 'req-package)
(req-package company
   :config
   (progn
     (add-hook 'after-init-hook 'global-company-mode)
     (global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
     (setq company-idle-delay 0)))

;; Setup the server
(load "server")
(unless (server-running-p) (server-start))

(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
(require 'tramp)

;; Include some magit enhancements
(require 'den-custom-magit "den-magit")

;; Functions
(require 'den-functions "den-functions")

;; dired setup
(require 'den-custom-dired "den-dired")

;; C++ and irony
(require 'den-cpp-irony "den-cpp-irony")

;; Python setup - elpy
(require 'den-custom-python "den-python")

;; Setup flycheck
(require 'den-custom-flycheck "den-flycheck")

;; Setup other programming stuff (Bitbake, major modes, etc)
(require 'den-programming "den-prog")

;; Setup other stuff
;; General configs, Ediff, backups, key bindings, diminish, powerline, etc.
(require 'den-config "den-general-config")
