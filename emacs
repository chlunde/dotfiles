;;; dot-emacs --- .emacs -*- emacs-lisp -*-
;; -*- lexical-binding: t; -*-
;;; Commentary:
;; None

;;; Code:

(setq gc-cons-threshold 15000000)
;; (setq debug-on-error t)

;; Configure basic look and feel first, to avoid flickering
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)

;(setq-default comp-deferred-compilation-deny-list nil)
;(add-hook 'window-setup-hook
;          (lambda ()
;            (set-background-color "black")
;            (set-foreground-color "white")))
;(frame-parameters)
(when (window-system)
  (set-cursor-color "red")
  (setq-default cursor-type '(hbar . 5))
  ;(set-frame-font "Go Mono-15")
  ;;(set-frame-font "iA Writer Mono V-14")
  (set-frame-font "Iosevka Term Slab Light-18")
  )

(setq-default warning-suppress-types '((comp)))

(when (< emacs-major-version 25)
  (message "too old"))

;; Emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
;; Only install google-noto-emoji-color-fonts - not BW
(when (fboundp 'set-fontset-font) (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append))

;;; Bootstrap straight.el

(setq-default straight-check-for-modifications '(check-on-save find-when-checking))

(defvar bootstrap-version)
(setq native-comp-deferred-compilation-deny-list nil)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/da48615f293960d37829a2d8cb1f0bb9d0524a7a/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Packages
(straight-use-package 'use-package)
(setq package-enable-at-startup nil)

;; When configuring a feature with `use-package', also tell
;; straight.el to install a package of the same name, unless otherwise
;; specified using the `:straight' keyword.
(setq straight-use-package-by-default t)

(eval-when-compile
  (require 'use-package))

(use-package server
  :straight nil
  :config
  (add-hook 'server-switch-hook #'raise-frame)
  (unless (server-running-p)
    (server-start)))

(use-package diminish :straight t)

(setq vc-handled-backends '(Git))

(setq-default use-package-always-defer t)

(use-package acme-theme)
(load-theme 'acme t)

(use-package vertico
  :init
  (vertico-mode +1)
  (setq completion-styles '(substring flex partial-completion)))

(defun chl-project-go (dir)
  (let ((override (locate-dominating-file dir "go.mod")))
    (if override
	(cons 'vc override)
      nil)))

(require 'project)
(add-hook 'project-find-functions #'chl-project-go)

(setq consult-project-root-function
	  (lambda ()
		(ignore-errors
		  (project-root (project-current)))))

(defun chl/fzf-projects ()
  "Fuzzy find projects."
  (interactive)
  (consult-find (concat (getenv "HOME") "/src")))
;  (let ((counsel-fzf-cmd "find -L src/ go/src/ git/ -maxdepth 3 -name vendor -a  -prune -o -name node_modules -prune -o -name \".*\" -a -prune -o -type d -a -print | fzf -f \"%s\""))
;	(counsel-fzf "" (getenv "HOME") "")))


(defun chl/fzf-git ()
  "Fuzzy find on the closest git repository."
  (interactive)
  (consult-find (magit-toplevel)))

(defun chl/treemacs-current-project ()
  "Ensure current project is in treemacs, toggle treemacs"
  (interactive)
  (unless (eq major-mode 'treemacs-mode)
	(treemacs-add-and-display-current-project)))

(global-set-key (kbd "s-/") #'chl/treemacs-current-project)
(global-set-key (kbd "C-2") #'chl/fzf-git)
(global-set-key (kbd "C-c 2") #'chl/fzf-git) ; alias for emacs -nw
(global-set-key (kbd "C-3") #'chl/fzf-projects)
(global-set-key (kbd "C-c 3") #'chl/fzf-projects)

(global-set-key (kbd "C-c ! l") #'flymake-show-buffer-diagnostics)

(global-set-key (kbd "C-x b") #'consult-buffer)
(global-set-key (kbd "C-c f") #'consult-imenu)
(global-set-key (kbd "C-c y") #'consult-yank-from-kill-ring)
(global-set-key (kbd "M-g M-g") #'consult-goto-line)

(use-package dumb-jump)
(global-set-key (kbd "M-.") #'dumb-jump-go)

;(use-package dockerfile-mode)

;(use-package groovy-mode) ; jenkinsfile

(use-package consult)

(recentf-mode)
(global-set-key (kbd "C-4") #'consult-ripgrep)
(global-set-key (kbd "C-c 4") #'consult-ripgrep)

(global-set-key (kbd "C-5") #'consult-line)
(global-set-key (kbd "C-c 5") #'consult-line)

;; https://sam217pa.github.io/2016/09/11/nuclear-power-editing-via-ivy-and-ag/
;; C-4, C-c C-o; C-x C-q (ivy-wgrep-change-to-wgrep-mode)

(require 'cl-lib)

(defun shell-at-dir (dir)
  "Open a shell at DIR.
If a shell buffer visiting DIR already exists, show that one."
  (interactive (list default-directory))
  (let ((buf (car (cl-remove-if-not
                   (lambda (it)
                     (with-current-buffer it
                       (and (derived-mode-p 'shell-mode)
                            (equal (expand-file-name default-directory) (expand-file-name dir)))))
                   (buffer-list)))))
    (if buf
		(progn
		  (if (eq (selected-window) (get-buffer-window buf))
			  (pop-to-buffer (other-buffer (current-buffer) t))
			(display-buffer buf 'in-previous-window)
			(select-window (get-buffer-window buf))))
      (shell (generate-new-buffer-name "*shell*")))))

(defun chl/shell-project-root ()
  "Open shell in project root."
  (interactive)
  (shell-at-dir (project-root (project-current))))

(global-set-key (kbd "C-t") #'chl/shell-project-root)

;(use-package markdown-mode)


(use-package company
  :defer t
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;;; Global modes
(prefer-coding-system 'utf-8)
(global-font-lock-mode t)               ; fontify when possible
(column-number-mode t)
(line-number-mode t)

(fset 'yes-or-no-p 'y-or-n-p)           ; Be consistent!
(show-paren-mode t)                     ; Highlight matching paren
(auto-compression-mode t)               ; Decompress gz-files etc.
(windmove-default-keybindings)
(global-auto-revert-mode)
(auto-fill-mode)
(delete-selection-mode t)
(savehist-mode t) ; Save minibuffer history (for compile command etc.)

;;; Extra packages installed
(use-package diff-hl
  :defer 0
  :config
  (global-diff-hl-mode)

  (set-face-background 'diff-hl-insert "#336633")
  (set-face-background 'diff-hl-delete "#663333")
  (set-face-background 'diff-hl-change "blue2")

  (unless (window-system)
	(diff-hl-margin-mode)))

(use-package yaml-mode
  :config
  ;;(add-hook 'yaml-mode-hook
  ;;(lambda ()
  ;; (add-hook 'write-file-functions #'delete-trailing-whitespace t t)
  ;; (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

  (add-hook 'yaml-mode-hook
			(lambda ()
			  (add-hook 'write-file-functions #'delete-trailing-whitespace t t)

	          ;;; avoid issue where previous line is re-indenten on newline
			  (define-key yaml-mode-map "\C-m" 'newline-and-indent)
			  (when (fboundp 'electric-indent-mode) (electric-indent-mode -1))))

  ;;(add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
  ;;(setq-default highlight-indent-guides-method 'character)
  ;;(setq-default highlight-indent-guides-auto-character-face-perc 30)
  ;;(setq-default highlight-indent-guides-character ?\│)
  )

;;; Props
(setq user-full-name "Carl Henrik Lunde"
      user-mail-address (concat "chlunde" "@" "ifi.uio.no"))

(setq require-final-newline 'ask
      show-trailing-whitespace t
      mouse-yank-at-point t ; Middle-mouse-button-paste at point, not mouse
      inhibit-startup-message t
      visible-bell t)

(setq-default tab-width 4)

(setq diff-switches "-u")               ; Unified diffs

(use-package ediff
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(setq backup-by-copying t               ; don't clobber symlinks
      backup-directory-alist (list (cons "." (expand-file-name "~/.backups"))) ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 200
      kept-old-versions 50
      vc-follow-symlinks t
      version-control t)                ; use versioned backups

(setq org-log-done t)
(setq org-agenda-files '("todo.org"))

(define-key global-map (kbd "C-c A") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      `(("t" "TODO" entry (file+headline "todo.org" "Tasks")
         ,(concat "* %? (%a) :capture:\n"
                  "<%<%Y-%m-%d %a %H:00>>\n"
                  "/Entered on/ %U\n"))))

(org-babel-do-load-languages 'org-babel-load-languages
							 '((shell . t)))

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
(global-set-key (kbd "TAB") #'indent-or-complete)
(global-set-key (kbd "<f5>") #'recompile)
(global-set-key (kbd "<f6>") #'next-error)
(global-set-key (kbd "C-x |") #'next-error) ; C-x | and C-x ` - same key in US/NO layout
(global-set-key (kbd "C-x ,") #'comment-line) ; C-x C-; unavailable in terminal
(global-set-key (kbd "C-x j") #'avy-goto-char-timer)

(define-key isearch-mode-map [(control return)] #'isearch-exit-other-end)

(defun isearch-exit-other-end ()
  "Exit isearch, at the opposite end of the string."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))

(defun move-to-char (arg char)
  "Move to and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive (list (prefix-numeric-value current-prefix-arg)
             (read-char "Go to char: " t)))
  (search-forward (char-to-string char) nil nil arg)
  (goto-char (match-beginning 0)))

(global-set-key (kbd "M-i") #'move-to-char)

(defun re-fontify ()
  (interactive)
  (font-lock-ensure)
  (recenter-top-bottom))
(global-set-key (kbd "C-l") #'re-fontify)

(use-package magit
  :defer 1
  :commands magit-toplevel
  :bind ("C-x g" . magit-status))

(use-package magit-todos)

(defun chl/compilation-small-font-size ()
  "Use a condensed tiny font for the compilation window."
  (setq buffer-face-mode-face '(:family "DejaVu Sans Mono" :height 120 :width semi-condensed))
  (buffer-face-mode))

(add-hook 'compilation-mode-hook #'chl/compilation-small-font-size)

;(setq compilation-window-height 12)
(defun chl/compilation-shrink-maybe (buf status)
  "Shrink compilation window for `BUF'.  `STATUS' is unused.
Make sure it's no larger than need, and at most as high as
specified by `compilation-window-height'."
  (let ((win (get-buffer-window buf)))
    (when win
      (set-window-text-height win compilation-window-height)
      (shrink-window-if-larger-than-buffer win))))

;(add-hook 'flycheck-after-syntax-check-hook #'chl/auto-flycheck-win)

;; (flymake-mode t)						
(display-battery-mode t)

;; (defun chl/auto-flycheck-win ()
;;   (interactive)
;;   (when (flymake-running-backends)
;;     (flymake-show-buffer-diagnostics)))

(require 'rx)

;; Configure `display-buffer' behaviour for some special buffers.
(setq display-buffer-alist
      `(
		;; keep help, elisp source, etc in the same window
		;; on the right side if we need to create it
		(,(rx bos
			  (or (seq (or "*Apropos*" "*Help*" "*helpful" "*info*" "*Summary*" "magit:") (0+ not-newline))
				  (seq (0+ not-newline) (or ".el.gz" ".el") string-end)))
		 (display-buffer-reuse-window display-buffer-in-side-window)
		 (side            . right)
		 (window-width    . fit-window-to-buffer)
		 (reusable-frames . visible)
		 (slot            . 0)
         (mode apropos-mode help-mode helpful-mode Info-mode Man-mode elisp-mode magit-mode dired-mode))
		;; magit -> dired: reuse dired window
;		(,(rx bos (seq "magit:" (0+ not-newline)))
;		 (display-buffer-same-window display-buffer-reuse-window)
;		 (inhibit-same-window . nil)
;		 (mode dired-mode magit-status-mode))
        ;; Put REPLs and error lists into the bottom side window
        (,(rx bos (or "*Flymake diagnostics"
                      "*compilation"      ; Compilation buffers
                      "*Warnings*"        ; Emacs warnings
                      "*shell"            ; Shell window
					  "*Backtrace*"
                      ))
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (side            . bottom)
         (reusable-frames . visible)
         (window-height   . 0.22))
        ;; Let `display-buffer' reuse visible frames for all buffers.  This must
        ;; be the last entry in `display-buffer-alist', because it overrides any
        ;; later entry with more specific actions.
        ("." nil (reusable-frames . visible))))

(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-highlight-symbol-face ((t (:inherit bold :underline (:color "lightgreen" :style wave :position nil))))))


;;; Go mode
(use-package go-mode
  :init
  (add-hook 'go-mode-hook 'chl/go-mode))

(use-package gotest)

(require 'project)

(defun chl/go-build-root (dir)
  (interactive)
  (when-let ((root (locate-dominating-file dir "go.mod")))
	(cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(cl-defmethod project-root ((project (head eglot-project)))
  (cdr project))

(use-package typescript-mode)

(add-hook 'project-find-functions #'chl/go-build-root)

(use-package yasnippet)

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
             '((typescript-mode) "typescript-language-server" "--stdio"))
  )
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)

(use-package tree-sitter)
(use-package tree-sitter-langs)
(add-hook 'typescript-mode-hook #'tree-sitter-mode)
(add-hook 'typescript-mode-hook #'tree-sitter-hl-mode)


(defun my-project-try-tsconfig-json (dir)
  (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
    (cons 'eglot-project found)))

(add-hook 'project-find-functions
          'my-project-try-tsconfig-json nil nil)

(setq-default eglot-workspace-configuration
    '((:gopls .
        ((staticcheck . t)
         (matcher . "CaseSensitive")))))

(defun chl/go-mode ()
  "Customize go-mode."
  (interactive)
  ;; Set up before-save hooks to format buffer and add/delete imports.
  ;; Make sure you don't have other gofmt/goimports hooks enabled.
  (add-hook 'after-save-hook #'recompile t t)
  (add-hook 'write-file-functions #'delete-trailing-whitespace t t)
  (add-hook 'prog-mode-hook 'turn-on-auto-fill t t)

  ;; The depth of -10 places this before eglot's willSave notification,
  ;; so that that notification reports the actual contents that will be saved.
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t)

  (define-key go-mode-map (kbd "C-c C-t") 'go-test-current-test)

  (diminish 'eldoc-mode)

  ;(flyspell-prog-mode)
  ;(diminish 'flyspell-mode)

  (subword-mode)
  (diminish 'subword-mode)

  (setq compilation-always-kill t)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           (concat
			"cd " (project-root (project-current)) ";\n"
			;; Don't build the Go project using go build
			(if (file-exists-p "Makefile")
				"make"
			  "go build ."
			  )
			" && cd " default-directory " && go build .")))

  (add-to-list 'compilation-search-path default-directory)

  (local-set-key (kbd "M-.") #'xref-find-definitions))

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

(add-to-list 'auto-mode-alist '("/inventory" . conf-mode) 'append)

(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(add-hook 'web-mode-hook
          (lambda ()
            (when (equal "javascript" web-mode-content-type)
              (web-mode-set-content-type "jsx"))
            (message web-mode-content-type)
            (setq indent-tabs-mode nil)
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)
            (local-set-key (kbd "RET") 'newline-and-indent)))

;(use-package typescript-mode)

;(use-package kotlin-mode)

(with-eval-after-load 'subr-x
  (setq-default mode-line-buffer-identification
                '(:eval (format-mode-line (propertized-buffer-identification (or (when-let* ((buffer-file-truename buffer-file-truename)
                                                                                             (prj (cdr-safe (project-current)))
                                                                                             (prj-parent (file-name-directory (directory-file-name (expand-file-name prj)))))
                                                                                   (concat (file-relative-name (file-name-directory buffer-file-truename) prj-parent) (file-name-nondirectory buffer-file-truename)))
                                                                                 "%b"))))))

(when (window-system)
  (set-face-attribute 'eldoc-highlight-function-argument nil
                      :underline t :foreground "green"
                      :weight 'bold))

;(use-package esup)

;;; emacs ends here
