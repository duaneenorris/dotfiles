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
        (find-file (concat "/sudo:root@" (system-name) ":" file))))))

; source: https://elpy.readthedocs.io/en/latest/customization_tips.html#jumping-to-assignment
;; (defun elpy-goto-definition-or-rgrep ()
;;   "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
;;     (interactive)
;;     (ring-insert find-tag-marker-ring (point-marker))
;;     (condition-case nil (elpy-goto-definition)
;;         (error (elpy-rgrep-symbol
;;                    (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))

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

;; Add buffer local Flycheck checkers after LSP for different major modes.
;; https://github.com/flycheck/flycheck/issues/1762
(defvar-local my-flycheck-local-cache nil)
(defun my-flycheck-local-checker-get (fn checker property)
  ;; Only check the buffer local cache for the LSP checker, otherwise we get
  ;; infinite loops.
  (if (eq checker 'lsp)
      (or (alist-get property my-flycheck-local-cache)
          (funcall fn checker property))
    (funcall fn checker property)))
(advice-add 'flycheck-checker-get
            :around 'my-flycheck-local-checker-get)

(provide 'den-functions)
