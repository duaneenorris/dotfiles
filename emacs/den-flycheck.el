(require 'flycheck)

(use-package flycheck
  :hook (c++-mode . flycheck-mode)
  :hook (c-mode . flycheck-mode)
  :hook (python-mode . flycheck-mode)
  ;; Add flake8 to flycheck in python-mode when using lsp
  :hook (lsp-managed-mode .
            (lambda ()
              (when (derived-mode-p 'python-mode)
                (setq my-flycheck-local-cache '((next-checkers . (python-flake8)))))))
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save new-line idle-change)) ;; new-line also possible
  (setq flycheck-idle-change-delay 1)
  (setq flycheck-flake8rc "/home/duane/dotfiles/flake8"))

(provide 'den-custom-flycheck)
