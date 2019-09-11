;;; dot-emacs --- .emacs -*- emacs-lisp -*-
;; -*- lexical-binding: t; -*-
;;; Commentary:
;; None

;;; Code:

(setq gc-cons-threshold 15000000)

;; Configure basic look and feel first, to avoid flickering
(when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(menu-bar-mode 0)

(add-hook 'window-setup-hook
          (lambda ()
            (set-background-color "black")
            (set-foreground-color "white")))

(when (window-system)
  (set-cursor-color "red")
  ;;(set-frame-font "Go Mono-13")
  (set-frame-font "Iosevka Term Slab Light-13"))

(when (< emacs-major-version 25)
  (message "too old"))

;;; Bootstrap straight.el
(setq-default straight-check-for-modifications '(check-on-save find-when-checking))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/4d2c3ab92c83a6d2a45ba8ef391dd01d555178fc/install.el"
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

;; (straight-use-package
;;'(el-patch :type git :host github :repo "your-name/el-patch"))

(eval-when-compile
  (require 'use-package))

(setq-default use-package-always-defer t)

(use-package evil)

(use-package flx-ido
  :demand
  :config
  (flx-ido-mode t))

(use-package fzf
  :commands fzf/start
  :init
  ;; less problems with fzf
  (setq-default term-term-name "vt100"))

(defun chl/fzf-projects ()
  "Fuzzy find projects."
  (interactive)
  (fzf/start (getenv "HOME")
			 "(find -L /git ~/src ~/go/src/ -maxdepth 3 \\( -path '*/\\.*' -o -fstype 'dev' -o -fstype 'proc' \\) -prune -o -type d -print 2> /dev/null; find -L /git ~/src ~/go/src/ -maxdepth 3 -type d -print 2> /dev/null) | grep -v /vendor/ | grep -v /node_modules/"))

(defun chl/fzf-git ()
  "Fuzzy find on the closest git repository."
  (interactive)
  (fzf/start (magit-toplevel)))

(global-set-key (kbd "C-2") #'chl/fzf-git)
(global-set-key (kbd "C-c 2") (kbd "C-2")) ; alias for emacs -nw
(global-set-key (kbd "C-3") #'chl/fzf-projects)
(global-set-key (kbd "C-c 3") (kbd "C-3"))

(defun chl/file-in-parent (fn)
  (or (file-exists-p fn)
      (file-exists-p (concat "../" fn))
      (file-exists-p (concat "../../" fn))
      (file-exists-p (concat "../../../" fn))))

;; (defun chl/use-fzf-if-git ()
;;   (when (chl/file-in-parent ".git")
;;     (local-set-key (kbd "C-x C-f") #'chl/fzf-git)))

;; (add-hook 'find-file-hook #'chl/use-fzf-if-git)
;; (add-hook 'dired-mode-hook #'chl/use-fzf-if-git)

(use-package dockerfile-mode)

(use-package groovy-mode) ; jenkinsfile

(use-package swiper)

(use-package counsel)

(global-set-key (kbd "C-4") #'counsel-rg)
(global-set-key (kbd "C-c 4") #'counsel-rg)

;; https://sam217pa.github.io/2016/09/11/nuclear-power-editing-via-ivy-and-ag/
;; C-4, C-c C-o; C-x C-q (ivy-wgrep-change-to-wgrep-mode)

(defun shell-pop-projectroot (orig-fun &rest args)
  (let ((default-directory (car (project-roots (project-current)))))
	(apply orig-fun args)))

(use-package shell-pop
  :config
  (setq shell-pop-universal-key "C-t")
  (advice-add 'shell-pop :around #'shell-pop-projectroot)
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  :bind ("C-t" . shell-pop))

(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package flycheck
  :defer 1
  :config
  (global-flycheck-mode)
  (when (not (window-system))
	(set-face-attribute 'flycheck-error nil :foreground "red"))

  (setq-default flycheck-shellcheck-follow-sources nil) ; not supported in epel
  (setq-default flycheck-disabled-checkers '(go-golint go-build
				go-test go-errcheck go-unconvert go-megacheck
				javascript-jshint))
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  ;;(setq flycheck-global-modes '(rjsx-mode emacs-lisp-mode))
  ;;https://github.com/flycheck/flycheck/issues/1129#issuecomment-319600923
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))

(use-package company
  :defer t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;;; Global modes
(prefer-coding-system 'utf-8)
(global-font-lock-mode t)               ; fontify when possible
(column-number-mode t)
(line-number-mode t)
(ido-mode)
(ido-everywhere t)

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
  (unless (window-system)
	(diff-hl-margin-mode)))

(use-package yaml-mode
  :config
  ;;(add-hook 'yaml-mode-hook
  ;;(lambda ()
  ;; (add-hook 'write-file-functions #'delete-trailing-whitespace t t)
  ;; (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
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

(defun chl/compilation-small-font-size ()
  "Use a condensed tiny font for the compilation window."
  (setq buffer-face-mode-face '(:family "DejaVu Sans Moo" :height 80 :width semi-condensed))
  (buffer-face-mode))

(add-hook 'compilation-mode-hook #'chl/compilation-small-font-size)

(setq compilation-window-height 9)
(defun chl/compilation-shrink-maybe (buf status)
  "Shrink compilation window for `BUF'.  `STATUS' is unused.
Make sure it's no larger than need, and at most as high as
specified by `compilation-window-height'."
  (let ((win (get-buffer-window buf)))
    (when win
      (set-window-text-height win compilation-window-height)
      (shrink-window-if-larger-than-buffer win))))

(add-hook 'compilation-finish-functions #'chl/compilation-shrink-maybe)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-variable-tag ((t (:foreground "color-45" :weight bold))))
 '(diff-file-header ((t (:background "brightmagenta" :weight bold))))
 '(diff-header ((t (:background "magenta"))))
 '(ediff-even-diff-A ((t (:background "gray30"))))
 '(ediff-even-diff-B ((t (:background "gray30"))))
 '(ediff-odd-diff-A ((t (:background "gray30"))))
 '(ediff-odd-diff-B ((t (:background "gray30"))))
 '(go-guru-hl-identifier-face ((t (:inherit nil :underline (:color "green" :style wave)))))
 '(lsp-face-highlight-textual ((t (:weight bold))))
 '(magit-section-highlight ((t (:background "color-236"))))
 '(markdown-code-face ((t (:inherit fixed-pitch :background "gray20"))))
 '(secondary-selection ((t (:background "color-237" :foreground "#f6f3e8"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "brightmagenta"))))
 '(web-mode-html-tag-face ((t (:foreground "color-213")))))


;;; Go mode
(use-package go-mode
  :init
  (add-hook 'go-mode-hook 'chl/go-mode))

(use-package gotest)

(defun chl/go-build-root ()
  (interactive)
  (let ((cur default-directory)
        (last-go-directory default-directory)
        root default-directory)
    (while (let ((is-git-root (file-exists-p (concat cur ".git")))
                 (has-go-files (directory-files cur nil "^.*\\.go$" t)))
             (when has-go-files
			   (setq last-go-directory cur))
			 (when is-git-root
			   (setq root cur))
             (setq cur (file-name-directory (directory-file-name cur)))
             (when (equal cur "/")
			   (setq root last-go-directory))
			 (and (not is-git-root) (not (equal cur "/")))))
    root))

(defun chl/go-mode ()
  "Customize go-mode."
  (interactive)
  ;; (add-hook 'before-save-hook #'gofmt-before-save t t)
  (add-hook 'before-save-hook 'lsp-organize-imports nil t)
  (add-hook 'after-save-hook #'recompile t t)
  (add-hook 'write-file-functions #'delete-trailing-whitespace t t)
  (add-hook 'prog-mode-hook 'turn-on-auto-fill t t)
  (define-key go-mode-map (kbd "C-c C-t") 'go-test-current-test)

;  (setq-default gofmt-command "goimports")

  (flyspell-prog-mode)
  (subword-mode)
  ;;(go-eldoc-setup)

  (when (string-match "/\\(github.com/[^/]+/[^/]+\\)/" buffer-file-name)
    (setq bug-reference-bug-regexp "\\([Bb]ug ?#?\\|[Pp]atch ?#\\|RFE ?#\\|PR [a-z-+]+/\\|#\\)\\([0-9]+\\(?:#[0-9]+\\)?\\)")
    (set (make-local-variable 'bug-reference-url-format)
         (concat "https://" (match-string-no-properties 1 buffer-file-name) "/issues/%s"))
    (bug-reference-prog-mode))

  (when (file-exists-p "~/opt/gotoolpath/src/github.com/stapelberg/expanderr/lisp/go-expanderr.el")
	(load "~/opt/gotoolpath/src/github.com/stapelberg/expanderr/lisp/go-expanderr.el")
	(local-set-key (kbd "C-c C-e") #'go-expanderr))
  (setq compilation-always-kill t)
  (setq go "go")
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           (concat
			"cd " (chl/go-build-root) ";\n"
			;; Don't build the Go project using go build
            (if (string-prefix-p "/home/chlunde/src/go/" (chl/go-build-root))
				"(cd ~/src/go/src; GOROOT_BOOTSTRAP=~/opt/go ./make.bash --no-clean) "
			  (concat
			   (if (file-exists-p "Makefile")
				   "make"
				 (concat "GOGC=800 " go " build -v && " go " test -v ."))
			   "&& staticcheck $(type errfilt &> /dev/null && errfilt || echo .)")))))
  ;;| grep " (file-name-nondirectory (buffer-file-name)) "

  (local-set-key (kbd "M-.") #'lsp-find-definition))

(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-enable-links nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-use-childframe nil))

(add-hook 'go-mode-hook #'lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package company-lsp
  :commands company-lsp)

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

;;; emacs ends here
