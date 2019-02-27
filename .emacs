(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/")
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(ido-ignore-buffers (quote ("\\`*" "\\` ")))
 '(indent-tabs-mode nil)
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "orange red")))))
(defvar myPackages
  '(elpy
    flycheck
    magit
    py-autopep8
    powerline
    zoom-window))

(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
(require 'tramp)

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

(elpy-enable)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode))
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
(setq elpy-rpc-ignored-buffer-size 204800)
 ;;(elpy-use-ipython)

(load-theme 'wombat)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(require 'saveplace)
(setq-default save-place t)
(setq require-final-newline t)
(setq column-number-mode t)

(setq c-default-style "linux"
      c-basic-offset 4)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; setup copy line
  (defun copy-line (arg)
    "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
    (interactive "p")
    (let ((beg (line-beginning-position))
          (end (line-end-position arg)))
      (when mark-active
        (if (> (point) (mark))
            (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
          (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
      (if (eq last-command 'copy-line)
          (kill-append (buffer-substring beg end) (< end beg))
        (kill-ring-save beg end)))
    (kill-append "\n" nil)
    (beginning-of-line (or (and arg (1+ arg)) 2))
    (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))


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

;; (load "server")
;; (unless (server-running-p) (server-start))
;; setup files ending in “.launch” to open in xml-mode

(add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))

;; (require 'py-autopep8)
;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=99"))

;; Setup backups and versioning
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(when (display-graphic-p)
  (setq desktop-save                t
        desktop-load-locked-desktop nil
        desktop-auto-save-timeout   120)
  (desktop-save-mode 1))
;; (setq backup-directory-alist
;;           `((".*" . ,temporary-file-directory)))
;; (setq auto-save-file-name-transforms
;;           `((".*" ,temporary-file-directory t)))

;; ;; Keep them for a week
;; (message "Deleting old backup files...")
;; (let ((week (* 60 60 24 7))
;;       (current (float-time (current-time))))
;;   (dolist (file (directory-files temporary-file-directory t))
;;     (when (and (backup-file-name-p file)
;;                (> (- current (float-time (fifth (file-attributes file))))
;;                   week))
;;       (message "%s" file)
;;       (delete-file file))))

(require 'powerline)
(powerline-center-theme)
(set-face-attribute 'mode-line nil
                    :foreground "LightGrey"
                    :background "firebrick"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :foreground "black"
                    :background "firebrick4"
                    :box nil)
(setq confirm-kill-emacs 'y-or-n-p)
