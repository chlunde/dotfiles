;;; straight-update.el --- Update straight packages

;;; Commentary:

;;; Code:
(defun show-updates ()
  "Show available update for packages."
  (interactive)
  (let ((repo-root "~/.emacs.d/straight/repos/"))
	;; (straight-fetch-all)
	(with-current-buffer (get-buffer-create "*straight-update*")
	  (switch-to-buffer "*straight-update*")
	  (erase-buffer)
	  (org-mode)
	  (dolist (repo (directory-files repo-root))
		(unless (string-prefix-p "." repo)
		(let ((news (shell-command-to-string (concat "git -C " repo-root repo " shortlog --pretty=short  master..origin/master"))))
		  (when (> (length news) 0)
			(insert "# " repo "\n")
			(insert (replace-regexp-in-string "^\\(\\w\\)" "* \\1" news))
			(insert "#+BEGIN_SRC emacs-lisp
(straight-pull-package \"" (replace-regexp-in-string "\\.el$" "" repo) "\")
#+END_SRC\n")
			(previous-line)
			(org-hide-block-toggle t)
			(next-line)
			
			(insert "#+BEGIN_SRC diff\n")
			(insert (shell-command-to-string (concat "git -C " repo-root repo " diff master..origin/master")))
			(insert "#+END_SRC\n")
			(previous-line)
			(ignore-errors
			  (org-hide-block-toggle t))
			(next-line)

			))))
	)))

;(replace-regexp-in-string "^\\(\\w\\)" "* \\1" "foo\nbar\n  blatti")

;;; straight-update.el ends here
