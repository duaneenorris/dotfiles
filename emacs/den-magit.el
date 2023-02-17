(require 'magit)
(defun magit-submodule-update-recursive ()
  (interactive)
  (magit-run-git-async "submodule" "update" "--init" "--recursive" "-j" "32"))

;; Add magit submodule Update all
(eval-after-load "magit" '(transient-append-suffix 'magit-submodule "f"
  '("U" "Update all (recursively)" magit-submodule-update-recursive)))

;; Add magit fetch in parallel
(eval-after-load "magit" '(transient-insert-suffix 'magit-fetch "-p"
  '("-j" "Fetch in parallel" "--jobs=32")))

;; Add magit pull in parallel
(eval-after-load "magit" '(transient-append-suffix 'magit-pull "-f"
  '("-j" "Fetch in parallel" "--jobs=32")))

;; Add magit pull --prune
(eval-after-load "magit" '(transient-insert-suffix 'magit-pull "-r"
  '("-p" "Prune" "--prune")))

(provide 'den-custom-magit)
