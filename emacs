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

;;; Packages
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "https://stable.melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq-default use-package-always-defer t
              use-package-always-ensure t)

(use-package flx-ido)
(use-package fzf)
(use-package key-chord)
(use-package markdown-mode)
(use-package projectile)

(use-package flycheck
  :defer 1
  :config
  (global-flycheck-mode)
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

(use-package company-go
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-go)))

;;; Global modes
(prefer-coding-system 'utf-8)
(global-font-lock-mode t)               ; fontify when possible
(column-number-mode t)
(line-number-mode t)
(ido-mode)
(ido-everywhere t)
(flx-ido-mode t)
(fset 'yes-or-no-p 'y-or-n-p)           ; Be consistent!
(show-paren-mode t)                     ; Highlight matching paren
(auto-compression-mode t)               ; Decompress gz-files etc.
(windmove-default-keybindings)
(global-auto-revert-mode)
(auto-fill-mode)
(delete-selection-mode t)

;;; Extra packages installed
(use-package diff-hl
  :init
  (global-diff-hl-mode)
  (unless (window-system)
	(diff-hl-margin-mode)))

(use-package yaml-mode)

;(add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
(setq-default highlight-indent-guides-method 'character)
(setq-default highlight-indent-guides-auto-character-face-perc 30)
(setq-default highlight-indent-guides-character ?\│)

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
  (progn
    (setq ediff-split-window-function 'split-window-horizontally)
    (setq ediff-window-setup-function 'ediff-setup-windows-plain)))

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
(key-chord-mode 1)
(key-chord-define-global "fd" #'fzf)
(defun fzf-home ()
  (interactive)
  (fzf-directory "~"))
(key-chord-define-global "er" #'fzf-home)

(defun fzf-ffap ()
  (interactive)
  (setq fzf/args (concat "-x --color bw --print-query -q " (word-at-point)))
  (fzf-directory "~")
  (setq fzf/args "-x --color bw --print-query"))

(key-chord-define-global "fp" #'fzf-ffap)

;; less problems with fzf
(setq-default term-term-name "vt100")

(defun re-fontify ()
  (interactive)
  (font-lock-ensure)
  (recenter-top-bottom))
(global-set-key (kbd "C-l") #'re-fontify)

(use-package magit
  :defer 1
  :ensure t
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
 '(flycheck-error ((t (:underline (:color "Red1" :style wave) :weight bold))))
 '(go-guru-hl-identifier-face ((t (:inherit nil :underline (:color "green" :style wave)))))
 '(magit-section-highlight ((t (:background "color-236"))))
 '(markdown-code-face ((t (:inherit fixed-pitch :background "gray20"))))
 '(secondary-selection ((t (:background "color-237" :foreground "#f6f3e8"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "brightmagenta"))))
 '(web-mode-html-tag-face ((t (:foreground "color-213")))))


;;; Go mode
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

(use-package go-eldoc
  :ensure t
  :defer 1
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-guru)
(use-package go-rename)

(use-package go-mode
  :init
  (add-hook 'go-mode-hook 'chl/go-mode))

(defun chl/go-mode ()
  (interactive)
  "Customize go-mode."
  (add-hook 'before-save-hook #'gofmt-before-save t t)
  (add-hook 'after-save-hook #'recompile t t)
  (add-hook 'write-file-functions #'delete-trailing-whitespace t t)
  (add-hook 'prog-mode-hook 'turn-on-auto-fill t t)

  (setq-default gofmt-command "goimports")
  (set (make-local-variable 'company-backends) '(company-go company-files))

  ;(setq projectile-globally-ignored-directories
;		(append projectile-globally-ignored-directories '("vendor")))


  (local-set-key (kbd "C-x C-f") 'projectile-find-file)
  (set (make-local-variable 'projectile-require-project-root) nil)

  (flyspell-prog-mode)
  (subword-mode)
  ;(go-eldoc-setup)

  (when (string-match "/\\(github.com/[^/]+/[^/]+\\)/" buffer-file-name)
    (setq bug-reference-bug-regexp "\\([Bb]ug ?#?\\|[Pp]atch ?#\\|RFE ?#\\|PR [a-z-+]+/\\|#\\)\\([0-9]+\\(?:#[0-9]+\\)?\\)")
    (set (make-local-variable 'bug-reference-url-format)
         (concat "https://" (match-string-no-properties 1 buffer-file-name) "/issues/%s"))
    (bug-reference-prog-mode))

  (unless (featurep 'go-guru)
    (dolist (guru '("$GOPATH/src/golang.org/x/tools/cmd/guru/go-guru.el"
                    "~/opt/gotoolpath/src/golang.org/x/tools/cmd/guru/go-guru.el"))
      (when (file-exists-p guru)
        (add-to-list 'load-path (file-name-directory (directory-file-name guru)))
        (require 'go-guru))))
  (when (file-exists-p "~/opt/gotoolpath/src/github.com/stapelberg/expanderr/lisp/go-expanderr.el")
	(load "~/opt/gotoolpath/src/github.com/stapelberg/expanderr/lisp/go-expanderr.el")
	(local-set-key (kbd "C-c C-e") #'go-expanderr))
  (go-guru-hl-identifier-mode)
  (setq compilation-always-kill t
        compilation-auto-jump-to-first-error t)
  (setq go (if (chl/file-in-parent "go.mod")
			   "vgo"
			 "go"))
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           (concat "cd " (chl/go-build-root) ";\n"
				   ;; Don't build the Go project using go build
                   (if (string-prefix-p "/home/chlunde/src/go/" (chl/go-build-root))
					   "(cd ~/src/go/src; GOROOT_BOOTSTRAP=~/opt/go ./make.bash --no-clean) && "
					 (concat "if [[ -f Makefile ]]; then make; else GOGC=800 " go " build -i -v; fi && "
							 go " test -v . && "))
                   "megacheck $(type errfilt 2> /dev/null && errfilt || echo .)"
                   )))
  ;;| grep " (file-name-nondirectory (buffer-file-name)) "

  (local-set-key (kbd "M-.") #'go-guru-definition))

(eval-after-load 'go-mode '(add-hook 'go-mode-hook #'chl/go-mode))

;;;
;(add-hook 'yaml-mode-hook
;          (lambda ()
;            (add-hook 'write-file-functions #'delete-trailing-whitespace t t)
;            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

(setq markdown-fontify-code-blocks-natively t)

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
            (local-set-key (kbd "C-x C-f") 'projectile-find-file)
            (local-set-key (kbd "RET") 'newline-and-indent)))

(defun chl/file-in-parent (fn)
  (or (file-exists-p fn)
      (file-exists-p (concat "../" fn))
      (file-exists-p (concat "../../" fn))
      (file-exists-p (concat "../../../" fn))))

(defun chl/use-projectile-if-git ()
  (when (chl/file-in-parent ".git")
    (local-set-key (kbd "C-x C-f") 'projectile-find-file)))

(add-hook 'find-file-hook #'chl/use-projectile-if-git)
(add-hook 'dired-mode-hook #'chl/use-projectile-if-git)


;(require 'undo-tree)
;(global-undo-tree-mode t)
;(setq undo-tree-visualizer-diff t)

(when (window-system)
  (set-face-attribute 'eldoc-highlight-function-argument nil
                      :underline t :foreground "green"
                      :weight 'bold))

;;; emacs ends here
