;;; -*- lexical-binding: t -*-
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

;; Packages

(defvar commonPackages
  '(req-package
    flycheck
    magit
    powerline
    zoom-window
    undo-tree
    salt-mode
    dumb-jump
    dired-subtree
    dired-ranger
    dired-narrow
    ))

;; Use this line to update packages without checking signatures
(setq package-check-signature nil)

;; Make sure common packages are installed
(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      commonPackages)

(require 'req-package)
;; (req-package company
;;    :config
;;    (progn
;;      (add-hook 'after-init-hook 'global-company-mode)
;;      (global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
;;      (setq company-idle-delay 0)))

(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
(require 'tramp)

;; Include some magit enhancements
;;(require 'den-custom-magit "den-magit")

;; Functions
;;(require 'den-functions "den-functions")

;; source: https://github.com/MatthewZMD/.emacs.d
(defun edit-this-with-sudo ()
  "Either open the file currently opened or selected in dired with `sudo' privilege."
  (interactive)
  (let ((buffer-file (buffer-file-name)))
    (if buffer-file
        (progn
          (kill-buffer (buffer-name))
          (find-file (concat "/sudo:root@" (system-name) ":" buffer-file)))
      (dolist (file (dired-get-marked-files))
        (find-file (concat "/sudo:root@" (system-name) ":" file))))))

;; dired setup
;; (require 'den-custom-dired "den-dired")

;; Setup flycheck
(require 'den-robot-flycheck "den-robot-flycheck")

;; Setup other programming stuff (Bitbake, major modes, etc)
;; (require 'den-programming "den-prog")

;; Setup other stuff
;; General configs, Ediff, backups, key bindings, diminish, powerline, etc.
(require 'den-robot-config "den-robot-config")

;; (require 'which-key)
;; (which-key-mode)
