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
(add-to-list 'load-path "~/dotfiles/")
(add-to-list 'load-path "~/src/snap_dev_tools/den-dotfiles/")

;; Custom set variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(ido-ignore-buffers '("\\`*" "\\` "))
 '(indent-tabs-mode nil)
 '(irony-extra-cmake-args
   '("-DLIBCLANG_LIBRARY=/usr/lib/llvm-3.5/lib/libclang.so -DLIBCLANG_INCLUDE_DIR=/usr/lib/llvm-3.5/include/"))
 '(package-selected-packages
   '(salt-mode gnu-elpa-keyring-update undo-tree zoom-window req-package py-autopep8 powerline magit flycheck-irony elpy el-get company-irony-c-headers company-irony clang-format))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "orange red")))))

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
    ))

;; Use this line to update packages without checking signatures
;;(setq package-check-signature nil)

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

(require 'req-package)
(req-package company
   :config
   (progn
     (add-hook 'after-init-hook 'global-company-mode)
     (global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
     (setq company-idle-delay 0)))

(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
(require 'tramp)

(load-theme 'wombat)

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

;; Functions

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(defun magit-submodule-update-recursive ()
  (interactive)
  (magit-run-git-async "submodule" "update" "--init" "--recursive"))

;; Add magit submodule Update all
(eval-after-load "magit" '(transient-append-suffix 'magit-submodule "f"
  '("U" "Update all (recursively)" magit-submodule-update-recursive)))

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
        (find-file (concat "/sudo:root@" (system-name) ":" file))))))(add-hook 'irony-mode-hook 'my-irony-mode-hook)

;; dired setup
(require 'dired-x)
(require 'dired)
;; allow dired to delete or copy dir
(setq dired-recursive-copies (quote always)) ; “always” means no asking
(setq dired-recursive-deletes (quote top)) ; “top” means ask once
;; Turn on Target Split window
(setq dired-dwim-target t)
;; Use the same buffer for viewing.  ENTER and ^
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory
(put 'dired-find-alternate-file 'disabled nil)
(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))
(use-package dired-ranger
  :ensure t
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))

;;narrow dired to match filter
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
    (filename (buffer-file-name)))
    (if (not filename)
    (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
      (message "A buffer named '%s' already exists!" new-name)
    (progn
      (rename-file filename new-name 1)
      (rename-buffer new-name)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil))))))

;; C++ mode setup - Irony, Company

(defun my-c++-mode-hook ()
  (local-set-key (kbd "C-x p") 'clang-format-buffer)
  (local-set-key (kbd "M-p") 'clang-format-region))

(require 'clang-format)
(require 'google-c-style)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Check buffer on save, new line and immediately after anbling flycheck-mode
(setq flycheck-check-syntax-automatically '(mode-enabled save new-line idle-change)) ;; new-line also possible
(setq flycheck-idle-change-delay 1)
;; Add include paths
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-include-path
                           (list (expand-file-name "/opt/ros/kinetic/include/")))))

;; Use company-mode with Irony
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; Add support for completing C/C++ headers
(require 'company-irony-c-headers)
(eval-after-load 'company '(add-to-list 'company-backends 'company-irony))

;; Set cppcheck standard to C++11
(setq irony-additional-clang-options '("-std=c++11"))

(req-package irony
  :config
  (progn
    (unless (irony--find-server-executable) (call-interactively #'irony-install-server))

    ;; Use compilation database first, clang_complete as fallback.
    (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
                                                    irony-cdb-clang-complete))

    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    )
)

;; This is the line to add to the irony-install-server cmake so it will compile
;;-DLIBCLANG_LIBRARY=/usr/lib/llvm-3.5/lib/libclang.so -DLIBCLANG_INCLUDE_DIR=/usr/lib/llvm-3.5/include/
;; Be sure you have installed clang llvm
;; sudo apt install clang llvm llvm-3.8-dev

(req-package company-irony
    :require company irony
    :config
    (progn
      (eval-after-load 'company '(add-to-list 'company-backends 'company-irony))))

(req-package flycheck-irony
    :require flycheck irony
    :config
    (progn
      (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))

(setq c-default-style "ellemtel"
      c-basic-offset 4)
(defun ROS-c-mode-hook()
  (setq c-basic-offset 2)
  (setq indent-tabs-mode nil)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'case-label '+)
  (c-set-offset 'brace-list-open 0)
  (c-set-offset 'brace-list-intro '+)
  (c-set-offset 'brace-list-entry 0)
  (c-set-offset 'member-init-intro 0)
  (c-set-offset 'statement-case-open 0)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-cont-nonempty '+)
  (c-set-offset 'arglist-close '+)
  (c-set-offset 'template-args-cont '+))
(add-hook 'c-mode-common-hook 'ROS-c-mode-hook)

;;; In order to get namespace indentation correct, .h files must be opened in C++ mode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))


;; Python setup - elpy

;; If elpy starts failing on startup, uninstall and re-install

(elpy-enable)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  )

(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i --simple-prompt")
(setq elpy-rpc-ignored-buffer-size 204800)
(setq elpy-rpc-python-command "python3")
;;(elpy-use-ipython)
;;(setq elpy-rpc-backend "jedi")

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; (require 'py-autopep8)
;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=99"))


;; Setup major modes based on file types
(add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.sls\\'" . salt-mode))
(add-to-list 'auto-mode-alist '("\\.j2\\'" . salt-mode))


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

;; General config stuff
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

;; (load "server")
;; (unless (server-running-p) (server-start))
(global-undo-tree-mode)
(dumb-jump-mode)
;;(smooth-scrolling-mode 1))

;; Setup diminish
(require 'diminish)
(diminish 'undo-tree-mode)
(diminish 'elpy-mode)
