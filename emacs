;; -*- emacs-lisp -*-

;;; Packages
(when (>= emacs-major-version 24)
  (require 'package)
  (setq quelpa-update-melpa-p nil)

  (defvar chl/package-selected-packages
	'(yasnippet
	  expand-region
	  restclient
	  diff-hl
	  company
	  company-go
	  go-eldoc
	  flycheck
	  go-mode
	  yaml-mode
	  quelpa
	  multiple-cursors))

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
(when (fboundp 'tool-base-more) (tool-bar-mode nil))
(prefer-coding-system 'utf-8)
(global-font-lock-mode t)               ; fontify when possible
(column-number-mode t)
(line-number-mode t)
(ido-mode)
(fset 'yes-or-no-p 'y-or-n-p)           ; Be consistent!
(show-paren-mode t)                     ; Highlight matching paren
(auto-compression-mode t)               ; Decompress gz-files etc.
(windmove-default-keybindings)
(winner-mode 1)
(global-auto-revert-mode)
(auto-fill-mode)
(electric-pair-mode)

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
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq backup-by-copying t               ; don't clobber symlinks
      backup-directory-alist '(("." . "/home/chlunde/.backups")) ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 200
      kept-old-versions 50
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
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "<f6>") 'next-error)


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
 '(ediff-even-diff-A ((t (:background "gray30"))))
 '(ediff-even-diff-B ((t (:background "gray30"))))
 '(ediff-odd-diff-A ((t (:background "gray30"))))
 '(ediff-odd-diff-B ((t (:background "gray30"))))
 '(magit-section-highlight ((t (:background "color-236"))))
 '(secondary-selection ((t (:background "color-237" :foreground "#f6f3e8")))))


;;; Go mode
(defun chl/go-mode ()
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'after-save-hook #'recompile)

  (setq-default gofmt-command "goimports")
  (set (make-local-variable 'company-backends) '(company-go))
  (require 'company-go)                                ; load company mode go backend

  (subword-mode)
  (go-eldoc-setup)
  (load-file "$GOPATH/src/golang.org/x/tools/cmd/guru/go-guru.el")

  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))

  (local-set-key (kbd "M-.") 'godef-jump-other-window))

(eval-after-load 'go-mode '(add-hook 'go-mode-hook 'chl/go-mode))

;;;
(add-hook 'yaml-mode-hook
		  (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode)))

;;;
(if window-system
	;; GUI settings
	(progn
	  (set-background-color "black")
	  (set-foreground-color "white")
	  (set-cursor-color "red")
	  (setq initial-frame-alist
			'((width . 102)
			  (height . 54))))

  ;; Terminal settings
  (set-face-attribute 'eldoc-highlight-function-argument nil
					  :underline t :foreground "green"
					  :weight 'bold)

  (menu-bar-mode 0) ; no mouse anyway

  ;; This is a dirty hack that I accidentally stumbled across:
  ;;  initializing "rxvt" first and _then_ "xterm" seems
  ;;  to make the colors work... although I have no idea why.
  (tty-run-terminal-initialization (selected-frame) "rxvt")
  (tty-run-terminal-initialization (selected-frame) "xterm")

  (diff-hl-margin-mode)
  (load-theme 'wombat))
