;; Bitbake
(require `bitbake)
(add-hook 'bitbake-mode-hook (lambda () (setq-local tab-width 4)))
(add-to-list 'auto-mode-alist '("\\.bb\\'" . bitbake-mode))
(add-to-list 'auto-mode-alist '("\\.bbclass\\'" . bitbake-mode))

;; Setup major modes based on file types
(add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.sls\\'" . salt-mode))
(add-to-list 'auto-mode-alist '("\\.j2\\'" . salt-mode))

(provide 'den-programming)
