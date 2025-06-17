(require 'flycheck)

;; From https://github.com/flycheck/flycheck/issues/1974#issuecomment-1343495202
(flycheck-define-checker python-ruff
  "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
  :command ("ruff"
            "check"
            "--output-format=concise"
            (eval (when buffer-file-name
                    (concat "--stdin-filename=" buffer-file-name)))
            "-")
  :standard-input t
  :error-filter (lambda (errors)
                  (let ((errors (flycheck-sanitize-errors errors)))
                    (seq-map #'flycheck-flake8-fix-error-level errors)))
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" (optional column ":") " "
            (id (one-or-more (any alpha)) (one-or-more digit)) " "
            (message (one-or-more not-newline))
            line-end))
  :modes python-mode)

(use-package flycheck
  :hook (c++-mode . flycheck-mode)
  :hook (c-mode . flycheck-mode)
  :hook (python-mode . flycheck-mode)
  ;; Add flake8 to flycheck in python-mode when using lsp
  :hook (lsp-managed-mode .
            (lambda ()
              (when (derived-mode-p 'python-mode)
                (setq my-flycheck-local-cache '((next-checkers . (python-ruff)))))))
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save new-line idle-change)) ;; new-line also possible
  (setq flycheck-idle-change-delay 1)
  (setq flycheck-flake8rc "/home/duane/dotfiles/flake8")
  (setq flycheck-python-ruff-config "/home/duane/dotfiles/ruff/ruff.toml")
  (add-to-list 'flycheck-checkers 'python-ruff))

(provide 'den-custom-flycheck)
