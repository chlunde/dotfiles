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

(when (>= 27 emacs-major-version)
  (global-tab-line-mode 1)
  (add-to-list 'tab-line-exclude-modes 'compilation-mode))

(add-hook 'window-setup-hook
          (lambda ()
            (set-background-color "black")
            (set-foreground-color "white")))

(when (window-system)
  (set-cursor-color "red")
  (setq-default cursor-type '(hbar . 5))
  ;;(set-frame-font "Go Mono-13")
  (set-frame-font "iA Writer Mono V-12")
  ;(set-frame-font "Iosevka Term Slab Light-13")
  )

(when (< emacs-major-version 25)
  (message "too old"))

;; Emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
;; Only install google-noto-emoji-color-fonts - not BW
(when (fboundp 'set-fontset-font) (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append))

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

(use-package server
  :straight nil
  :config
  (add-hook 'server-switch-hook #'raise-frame)
  (unless (server-running-p)
    (server-start)))

(use-package diminish :straight t)

(setq vc-handled-backends '(Git))

(setq-default use-package-always-defer t)

(use-package tramp
  :config
  ;; get magit to use git from SCL
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package selectrum
  :init
  (selectrum-mode +1)
  (setq completion-styles '(flex substring partial-completion)))

(use-package evil)

(defun chl/fzf-projects ()
  "Fuzzy find projects."
  (interactive)
  (let ((counsel-fzf-cmd "find -L src/ go/src/ git/ -maxdepth 3 -name vendor -a  -prune -o -name node_modules -prune -o -name \".*\" -a -prune -o -type d -a -print | fzf -f \"%s\""))
	(counsel-fzf "" (getenv "HOME") "")))

(defun chl/fzf-git ()
  "Fuzzy find on the closest git repository."
  (interactive)
  (counsel-fzf "" (magit-toplevel) (magit-toplevel)))

(global-set-key (kbd "C-2") #'chl/fzf-git)
(global-set-key (kbd "C-c 2") #'chl/fzf-git) ; alias for emacs -nw
(global-set-key (kbd "C-3") #'chl/fzf-projects)
(global-set-key (kbd "C-c 3") #'chl/fzf-projects)

(defun chl/file-in-parent (fn)
  (or (file-exists-p fn)
      (file-exists-p (concat "../" fn))
      (file-exists-p (concat "../../" fn))
      (file-exists-p (concat "../../../" fn))))

(use-package dockerfile-mode)

(use-package groovy-mode) ; jenkinsfile

(use-package swiper)

(use-package counsel)

;(put 'dired-find-alternate-file 'disabled nil)
;(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)))

(global-set-key (kbd "C-4") #'counsel-rg)
(global-set-key (kbd "C-c 4") #'counsel-rg)

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
                            (equal default-directory dir))))
                   (buffer-list)))))
    (if buf
        (display-buffer buf 'in-previous-window)
      (shell (generate-new-buffer-name "*shell*")))))

(defun chl/shell-project-root ()
  "Open shell in project root."
  (interactive)
  (shell-at-dir (car (project-roots (project-current)))))

(global-set-key (kbd "C-t") #'chl/shell-project-root)

(use-package markdown-mode
  :config
;  (set-face-background 'markdown-code-face nil)
  ;(setq markdown-fontify-code-blocks-natively t)
  )

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

(use-package company-quickhelp)
(use-package company-terraform)

(use-package terraform-mode
  :config
  (company-terraform-init)
  (company-quickhelp-mode)
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
  :mode ("\\.tf\\'" . terraform-mode))

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

(add-hook 'flycheck-after-syntax-check-hook #'chl/auto-flycheck-win)

(defun chl/auto-flycheck-win ()
  (interactive)
  (when flycheck-current-errors
    (flycheck-list-errors)))

  ;; (save-window-excursion
  ;; 	(when (get-buffer-window "*compilation*")
  ;; 	  (when (not (eq (get-buffer-window "*Flycheck errors*")
  ;; 					 (window-in-direction 'right (get-buffer-window "*compilation*"))))
  ;; 		(when (get-buffer-window "*Flycheck errors*")
  ;; 		  (delete-window (get-buffer-window "*Flycheck errors*")))
  ;; 		(unless (window-in-direction 'right (get-buffer-window "*compilation*"))
  ;; 		  (split-window (get-buffer-window "*compilation*") nil 'right nil))
  ;; 		(select-window (get-buffer-window "*compilation*"))
  ;; 		(select-window (window-in-direction 'right))
  ;; 		(display-buffer (get-buffer "*Flycheck errors*") )))))





(require 'rx)

;; Configure `display-buffer' behaviour for some special buffers.
;; see http://www.lunaryorn.com/2015/04/29/the-power-of-display-buffer-alist.html
;; and https://github.com/lunaryorn/.emacs.d/blob/2233f7dc277453b7eaeb447b00d8cb8d72435318/init.el#L420-L439
(setq display-buffer-alist
      `(
		(,(rx bos (or (or "*Apropos*" "*Help*" "*helpful" "*info*" "*Summary*" ) (0+ not-newline))
			  ".el.gz")
		 (display-buffer-reuse-window display-buffer-in-side-window)
		 (side            . right)
		 (window-width    . fit-window-to-buffer)
		 (reusable-frames . visible)
		 (slot            . 0)
         (mode apropos-mode help-mode helpful-mode Info-mode Man-mode elisp-mode))
        ;; Put REPLs and error lists into the bottom side window
        (,(rx bos (or "*Flycheck errors*" ; Flycheck error list
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


;(add-to-list 'display-buffer-alist             '("*Apropos*" display-buffer-same-window))
;(add-to-list 'display-buffer-alist             '("*Help*" display-buffer-same-window))


;(setq split-height-threshold 35)
;(add-hook 'compilation-finish-functions #'chl/compilation-shrink-maybe)

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
        (root nil))
    (while (let ((is-git-root (file-exists-p (concat cur ".git")))
				 (is-go-root (file-exists-p (concat cur "go.mod"))))
             (when (directory-files cur nil "^.*\\.go$" t)
			   (setq last-go-directory cur))
			 (when (or is-go-root is-git-root)
			   (setq root cur))
             (setq cur (file-name-directory (directory-file-name cur)))
			 (and (not root)
				  (not (equal cur "/")))))
	(or root last-go-directory)))

(defun chl/go-mode ()
  "Customize go-mode."
  (interactive)
  ;; Set up before-save hooks to format buffer and add/delete imports.
  ;; Make sure you don't have other gofmt/goimports hooks enabled.
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (add-hook 'after-save-hook #'recompile t t)
  (add-hook 'write-file-functions #'delete-trailing-whitespace t t)
  (add-hook 'prog-mode-hook 'turn-on-auto-fill t t)
  (define-key go-mode-map (kbd "C-c C-t") 'go-test-current-test)

  (diminish 'eldoc-mode)
  (diminish 'lsp-mode)

  (flyspell-prog-mode)
  (diminish 'flyspell-mode)

  (subword-mode)
  (diminish 'subword-mode)

  (setq compilation-always-kill t)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           (concat
			"cd " (chl/go-build-root) ";\n"
			;; Don't build the Go project using go build
			(if (file-exists-p "Makefile")
				"make"
			  "go build ./...")
			" && cd " default-directory " && go test .")))

  (add-to-list 'compilation-search-path default-directory)

  (local-set-key (kbd "M-.") #'lsp-find-definition))

(use-package lsp-mode
  :commands lsp
  :config
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
	 ("gopls.staticcheck" t t)))
  (setq lsp-file-watch-threshold 5000)
  (setq lsp-enable-links nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-use-childframe t))

(add-hook 'go-mode-hook #'lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (set-face-background 'lsp-ui-doc-background nil)
  ;; (set-face-background 'fixed-pitch nil)
  ;; (set-face-foreground 'fixed-pitch nil)
  )

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

(use-package vue-mode
  :mode "\\.vue\\'"
  :config
  (add-hook 'vue-mode-hook #'lsp))

(use-package typescript-mode)

(use-package kotlin-mode)

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
