;; Python setup - elpy

;; If elpy starts failing on startup, uninstall and re-install

(require 'elpy)

(require 'find-file-in-project)
(setq ffip-prefer-ido-mode t)

(elpy-enable)

(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i --simple-prompt")
(setq elpy-rpc-ignored-buffer-size 204800)
(setq elpy-rpc-python-command "python3")
;;(elpy-use-ipython)
(setq elpy-rpc-backend "jedi")
(define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition-or-rgrep)

;; (require 'py-autopep8)
;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=99"))

(provide 'den-custom-python)
