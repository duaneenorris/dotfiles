;; General config stuff

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(load-theme 'wombat)
(setq confirm-kill-emacs 'y-or-n-p)
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(require 'saveplace)
(setq-default save-place t)
(setq require-final-newline t)
(setq column-number-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq set-mark-command-repeat-pop t)
(setq mark-ring-max 5)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(setq show-paren-context-when-offscreen t)

; Set large file threshold to 100MB
(setq large-file-warning-threshold 100000000)

;; Key bindings
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x p") 'py-autopep8-buffer)
(global-set-key (kbd "C-x c") 'copy-line)
(global-set-key (kbd "C-M-z") 'zoom-window-zoom)
(global-set-key (kbd "C-x #") 'comment-region)
;; Setup CTRL-Arrow scrolling to preserve the cursor position, but scroll the window.
(global-unset-key [C-up])
(global-set-key [C-up]   '(lambda () "Scroll text down, don't move cursor." (interactive) (scroll-down 1)))

(global-unset-key [C-down])
(global-set-key [C-down] '(lambda () "Scroll text up in place, don't move cursor." (interactive) (scroll-up 1)))

;; Setup some ediff stuff
(setq ediff-split-window-function 'split-window-horizontally)
(defvar my-ediff-last-windows nil)

(defun my-store-pre-ediff-winconfig ()
  (setq my-ediff-last-windows (current-window-configuration)))

(defun my-restore-pre-ediff-winconfig ()
  (set-window-configuration my-ediff-last-windows))

(add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
(add-hook 'ediff-quit-hook #'my-restore-pre-ediff-winconfig)

;; Setup backups and versioning
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(require 'powerline)
(powerline-center-theme)

(when (display-graphic-p)
  (setq desktop-save                t
        desktop-load-locked-desktop nil
        desktop-auto-save-timeout   120)
  (desktop-save-mode 1))

(global-undo-tree-mode)

;; Turn on HideShow
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Setup diminish
(require 'diminish)
(diminish 'undo-tree-mode)
(diminish 'flycheck-mode)
(diminish 'company-mode)
(diminish 'which-key-mode)
(diminish 'hs-minor-mode)

(provide 'den-config)
