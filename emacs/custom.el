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
 '(lsp-keymap-prefix "s-x")
 '(package-selected-packages
   '(clang-format company company-irony company-irony-c-headers
                  company-jedi dash dired-narrow dired-ranger
                  dired-subtree dumb-jump el-get f flycheck
                  flycheck-irony git-commit gnu-elpa-keyring-update
                  highlight-indentation irony jedi
                  keychain-environment lsp-jedi magit magit-section
                  powerline py-autopep8 req-package salt-mode
                  transient treemacs treemacs-icons-dired
                  treemacs-magit treemacs-projectile undo-tree
                  with-editor yasnippet zoom-window))
 '(show-paren-mode t)
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist '(("" . "~/.saves"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 113 :width normal))))
 '(cursor ((t (:background "orange red"))))
 '(dired-broken-symlink ((t (:background "black" :foreground "red" :weight bold))))
 '(mode-line ((t (:background "firebrick" :foreground "light gray"))))
 '(mode-line-inactive ((t (:background "firebrick4" :foreground "black"))))
 '(powerline-active0 ((t (:inherit mode-line :background "red"))))
 '(powerline-inactive0 ((t (:inherit mode-line-inactive :background "firebrick")))))
