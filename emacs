;; -*- emacs-lisp -*-

;;; Packages
(when (< emacs-major-version 25)
  (message "too old"))

(require 'package)

(defvar chl/package-selected-packages
  '(company
    company-go
    diff-hl
    flx-ido
    flycheck
    go-eldoc
    go-mode
    go-guru
    go-rename
    magit
    markdown-mode
    projectile
    undo-tree
    restclient
    web-mode
    yaml-mode))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Install missing packages now
(dolist (p chl/package-selected-packages)
  (when (not (package-installed-p p))
    (package-install p t)))

;;; Global modes
(when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(menu-bar-mode 0)
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
(winner-mode 1)
(global-auto-revert-mode)
(auto-fill-mode)
(delete-selection-mode t)

;;; Extra packages installed
(global-diff-hl-mode)
(global-flycheck-mode)
(setq-default flycheck-shellcheck-follow-sources nil) ; not supported in epel

(add-hook 'after-init-hook 'global-company-mode)

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
(eval-after-load 'ediff
  '(progn
     (setq ediff-split-window-function 'split-window-horizontally)
     (setq ediff-window-setup-function 'ediff-setup-windows-plain)))

(setq backup-by-copying t               ; don't clobber symlinks
      backup-directory-alist (list (cons "." (expand-file-name "~/.backups"))) ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 200
      kept-old-versions 50
      vc-follow-symlinks t
      version-control t)                ; use versioned backups

(setq gc-cons-threshold 15000000)       ; I have enough ram ;)

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

(defun re-fontify ()
  (interactive)
  (font-lock-fontify-buffer)
  (recenter-top-bottom))
(global-set-key (kbd "C-l") #'re-fontify)

(global-set-key (kbd "C-x g") #'magit-status)

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
 '(magit-section-highlight ((t (:background "color-236"))) t)
 '(secondary-selection ((t (:background "color-237" :foreground "#f6f3e8"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "brightmagenta"))))
 '(web-mode-html-tag-face ((t (:foreground "color-213")))))


;;; Go mode
(defun chl/go-build-root ()
  (interactive)
  (let ((cur default-directory)
        (last-go-directory default-directory)
        root)
    (while (let ((is-git-root (file-exists-p (concat cur ".git")))
                 (has-go-files (directory-files cur nil "^.*\\.go$" t)))
             (when has-go-files (setq last-go-directory cur))

             (setq cur (file-name-directory (directory-file-name cur)))
             (not (when (or is-git-root (equal cur "/"))
                    (setq root last-go-directory)))))
    root))

(defun chl/go-mode ()
  "Hook for go mode customizations."
  (require 'company)
  (require 'company-go)
  (require 'projectile)

  (add-hook 'before-save-hook #'gofmt-before-save t t)
  (add-hook 'after-save-hook #'recompile t t)
  (add-hook 'write-file-functions #'delete-trailing-whitespace t t)

  (setq-default gofmt-command "goimports")
  (set (make-local-variable 'company-backends) '(company-go company-files))

  (local-set-key (kbd "C-x C-f") 'projectile-find-file)
  (set (make-local-variable 'projectile-require-project-root) nil)

  (flyspell-prog-mode)
  (subword-mode)
  (go-eldoc-setup)

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

  (setq compilation-always-kill t
        compilation-auto-jump-to-first-error t)

  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           (concat (if (chl/go-build-root)
                       (concat "cd " (chl/go-build-root) ";")
                     "")
                   "go vet | grep " (file-name-nondirectory (buffer-file-name)) " ; "
				   ;; Don't build the Go project using go build
                   (if (string-prefix-p "/home/chlunde/src/go/" (chl/go-build-root))
					   "(cd ~/src/go/src; GOROOT_BOOTSTRAP=~/opt/go ./make.bash --no-clean) && "
					 (concat "GOGC=800 go build -i -v ./... && "
							 "go test -v -test.short ./... && "))
                   "gometalinter --deadline=10s ./... -s vendor --dupl-threshold=150 -e 'Subprocess launching with partial path.' | grep -v testdata/ | grep " (file-name-nondirectory (buffer-file-name))
                   )))

  (local-set-key (kbd "M-.") #'go-guru-definition))

(eval-after-load 'go-mode '(add-hook 'go-mode-hook #'chl/go-mode))

;;;
(add-hook 'yaml-mode-hook
          (lambda ()
            (add-hook 'write-file-functions #'delete-trailing-whitespace t t)
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

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

(add-hook 'find-file-hook #'chl/use-projectile-if-git)
(add-hook 'dired-mode-hook #'chl/use-projectile-if-git)


(require 'undo-tree)
(global-undo-tree-mode t)
(setq undo-tree-visualizer-diff t)

(when (window-system)
    ;; GUI settings
    (progn
      (set-cursor-color "red")
      (setq initial-frame-alist
            '((width . 102)
              (height . 54))))

  ;; Terminal settings
  (require 'eldoc)
  (set-face-attribute 'eldoc-highlight-function-argument nil
                      :underline t :foreground "green"
                      :weight 'bold)

  (diff-hl-margin-mode)

  ;;(set-default-font "Fira Mono-13:weight=light")
  (set-default-font "Go Mono-13")

  (when nil
	(add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
	;; This works when using emacs without server/client
	(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
	;; I haven't found one statement that makes both of the above situations work, so I use both for now

	(defconst fira-code-font-lock-keywords-alist
	  (mapcar (lambda (regex-char-pair)
				`(,(car regex-char-pair)
				  (0 (prog1 ()
					   (compose-region (match-beginning 1)
									   (match-end 1)
									   ;; The first argument to concat is a string containing a literal tab
									   ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
			  '(
				("\\(!=\\)"                    #Xe10e)
				("\\(&&\\)"                    #Xe131)
				("\\(||\\)"                    #Xe132)
				("\\(<=\\)"                    #Xe141)
				("\\(<-\\)"                    #Xe152)
				("\\(<=\\)"                    #Xe157)
				("\\(->\\)"                    #Xe114)
				("[^=]\\(:=\\)"                #Xe10c))))

	(defun add-fira-code-symbol-keywords ()
	  (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

	(add-hook 'prog-mode-hook
			  #'add-fira-code-symbol-keywords)))
