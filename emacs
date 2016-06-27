;; -*- emacs-lisp -*-

;;; Packages
(when (>= emacs-major-version 24)
  (require 'package)
  (setq quelpa-update-melpa-p nil)

  (defvar chl/package-selected-packages
	'(yasnippet
	  company
	  company-go
	  diff-hl
	  evil
	  flx-ido
	  flycheck
	  go-eldoc
	  go-mode
	  go-rename
	  markdown-mode
	  projectile
	  quelpa
	  restclient
	  web-mode
	  yaml-mode))

  (add-to-list 'package-archives
   '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  ;; Install missing packages now
  (dolist (p chl/package-selected-packages)
	(when (not (package-installed-p p))
	  (package-install p))))

(if (or (> emacs-major-version 24) (and (= emacs-major-version 24) (> emacs-minor-version 3)))
	(when (not (package-installed-p 'magit))
	  (package-install 'magit))
  ;; Fall back to 1.4.2 for emacs 24.3 (RHEL7), magit 2.x requires 24.4
  (progn
	(quelpa '(git-commit-mode :repo "magit/git-modes" :commit "3423997a89f63eb4c8a4ce495928bc5951767932" :fetcher github))
	(quelpa '(git-rebase-mode :repo "magit/git-modes" :commit "3423997a89f63eb4c8a4ce495928bc5951767932" :fetcher github))
	(quelpa '(magit :repo "magit/magit" :commit "1.4.2" :fetcher github :files ("*.el" (:exclude "magit-autoloads.el"))))))


;;; Global modes
(when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(menu-bar-mode 0)
(prefer-coding-system 'utf-8)
(global-font-lock-mode t)               ; fontify when possible
(column-number-mode t)
(line-number-mode t)
(ido-mode)
(flx-ido-mode)
(fset 'yes-or-no-p 'y-or-n-p)           ; Be consistent!
(show-paren-mode t)                     ; Highlight matching paren
(auto-compression-mode t)               ; Decompress gz-files etc.
(windmove-default-keybindings)
(winner-mode 1)
(global-auto-revert-mode)
(auto-fill-mode)
;(electric-pair-mode)

;;; Extra packages installed
(global-diff-hl-mode)
(global-flycheck-mode)
(require 'company)
(global-company-mode)
(require 'eldoc)

;;; Props
(setq user-full-name "Carl Henrik Lunde"
      user-mail-address (concat "chlunde" "@" "ping.uio.no"))

(setq require-final-newline 'ask
      show-trailing-whitespace t
      mouse-yank-at-point t ; Middle-mouse-button-paste at point, not mouse
      inhibit-startup-message t
      visible-bell t)

(setq-default tab-width 4)

(setq compilation-window-height 8)

(setq diff-switches "-u")               ; Unified diffs
(require 'ediff)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq backup-by-copying t               ; don't clobber symlinks
      backup-directory-alist (list (cons "." (expand-file-name "~/.backups"))) ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 200
      kept-old-versions 50
      vc-follow-symlinks t
      version-control t)                ; use versioned backups

(setq gc-cons-threshold 15000000)       ; I have enough ram ;)

(defadvice backward-page (after chl-page-start-at-top activate)
  "Recenter window with page break at top."
  (recenter 0))

(defadvice forward-page (after chl-page-start-at-top activate)
  "Recenter window with page break at top."
  (recenter 0))

(defun indent-or-complete ()
  (interactive)
  (if (and (fboundp 'company-complete-common) (looking-at "\\_>"))
	  (company-complete-common)
	(indent-according-to-mode)))


;;; Keyboard bindings
(global-set-key (kbd "TAB") 'indent-or-complete)
;(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "<f6>") 'next-error)

(defun re-fontify ()
  (interactive)
  (font-lock-fontify-buffer)
  (recenter-top-bottom))
(global-set-key (kbd "C-l") 're-fontify)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-variable-tag ((t (:foreground "color-45" :weight bold))))
 '(diff-added ((t (:inherit diff-changed :background "color-22"))))
 '(diff-file-header ((t (:background "brightmagenta" :weight bold))))
 '(diff-header ((t (:background "magenta"))))
 '(diff-removed ((t (:inherit diff-changed :background "color-52"))))
 '(ediff-even-diff-A ((t (:background "gray30"))) t)
 '(ediff-even-diff-B ((t (:background "gray30"))) t)
 '(ediff-odd-diff-A ((t (:background "gray30"))) t)
 '(ediff-odd-diff-B ((t (:background "gray30"))) t)
 '(flycheck-error ((t (:underline (:color "Red1" :style wave) :weight bold))))
 '(magit-section-highlight ((t (:background "color-236"))) t)
 '(secondary-selection ((t (:background "color-237" :foreground "#f6f3e8"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "brightmagenta"))))
 '(web-mode-html-tag-face ((t (:foreground "color-213")))))


;;; Go mode
(defun chl/go-mode ()
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'after-save-hook #'recompile)

  (setq-default gofmt-command "goimports")
  (set (make-local-variable 'company-backends) '(company-go))
  (require 'company-go)                                ; load company mode go backend

  (require 'projectile)
  (local-set-key (kbd "C-x C-f") 'projectile-find-file)
  (subword-mode)
  (go-eldoc-setup)
  (load-file "$GOPATH/src/golang.org/x/tools/cmd/guru/go-guru.el")
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  (setq compilation-always-kill t)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
		   (concat (if (projectile-project-root)
					   (concat "cd " (projectile-project-root) ";")
					 "")
				   "GOGC=800 go build -i -v && go test -v -test.short ./... && go vet")))

  (local-set-key (kbd "M-.") 'godef-jump-other-window))

(eval-after-load 'go-mode '(add-hook 'go-mode-hook 'chl/go-mode))

;;;
(add-hook 'yaml-mode-hook
		  (lambda ()
			(add-to-list 'write-file-functions 'delete-trailing-whitespace)
			(define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode)))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))
(flycheck-add-mode 'javascript-eslint 'web-mode)

(defun chl/js-extract-with-imports ()
  "Extract region (React component) to a new file, including any imports in the current file"
  (interactive)

  (let ((selected (buffer-substring-no-properties (mark) (point))))
	(if (eq nil (string-match "^const \\([A-Z][A-Za-z]+\\)\s*=" selected))
		(message "Didn't match ^const Function = ...")
	  (let ((component (match-string 1 selected))
			(imports))
		(save-excursion
		  (kill-region (mark) (point))
		  (goto-char 1)
		  (while (search-forward-regexp "^import .*" nil t 1)
			(push (match-string 0) imports))
		  (insert "\nimport " component " from './" component "'")
		  (find-file-other-window (concat component ".js"))
		  (dolist (import imports)
			(insert import "\n"))
		  (insert "\n")
		  (yank)
		  (insert "\n" "export default " component "\n")
		  (set-buffer-modified-p t))))))

;(setq web-mode-content-types-alist
;  '(("jsx"  . ".*\\.js\\'")))
;(setq web-mode-content-types-alist
;  '(("jsx" . "\\.js[x]?\\'")))

(add-hook 'web-mode-hook
		  (lambda ()
			(when (equal "javascript" web-mode-content-type)
			  (web-mode-set-content-type "jsx"))
			(message web-mode-content-type)
			(setq indent-tabs-mode nil)
			(add-to-list 'write-file-functions 'delete-trailing-whitespace)
			(local-set-key (kbd "C-x C-f") 'projectile-find-file)
			(local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'window-setup-hook
		  (lambda ()
			(set-background-color "black")
			(set-foreground-color "white")))

(defun chl/use-projectile-if-git ()
  (when (or (file-exists-p ".git")
			(file-exists-p "../.git")
			(file-exists-p "../../.git")
			(file-exists-p "../../../.git"))
	(local-set-key (kbd "C-x C-f") 'projectile-find-file)))

(add-hook 'find-file-hook 'chl/use-projectile-if-git)
(add-hook 'dired-mode-hook 'chl/use-projectile-if-git)

;;;
(if window-system
	;; GUI settings
	(progn
	  (set-cursor-color "red")
	  (setq initial-frame-alist
			'((width . 102)
			  (height . 54))))

  ;; Terminal settings
  (set-face-attribute 'eldoc-highlight-function-argument nil
					  :underline t :foreground "green"
					  :weight 'bold)

  (diff-hl-margin-mode))
