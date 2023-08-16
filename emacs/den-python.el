;; Python setup

(use-package lsp-jedi
  :ensure t
  :hook (python-mode . lsp-mode))

(setq read-process-output-max (* 3 1024 1024)) ;; 3mb
(setq gc-cons-threshold 100000000)

(require 'yasnippet)
(yas-reload-all)
(add-hook 'python-mode-hook #'yas-minor-mode)

(require 'find-file-in-project)
(setq ffip-prefer-ido-mode t)

(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i --simple-prompt")

;; (require 'py-autopep8)
;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=99"))

(provide 'den-custom-python)
