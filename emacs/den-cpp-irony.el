;; C++ mode setup - Irony, Company

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)

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

(provide 'den-cpp-irony)
