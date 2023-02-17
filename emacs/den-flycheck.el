(require 'flycheck)
(require 'elpy)

;; Check buffer on save, new line and immediately after anbling flycheck-mode
(setq flycheck-check-syntax-automatically '(mode-enabled save new-line idle-change)) ;; new-line also possible
(setq flycheck-idle-change-delay 1)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  )
(setq flycheck-flake8rc "/home/duane/dotfiles/flake8")
(use-package flycheck
  :config
  (setq-default flycheck-disabled-checkers '(python-pylint)))

(provide 'den-custom-flycheck)
