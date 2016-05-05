;; -*- emacs-lisp -*-
;; $Id: .emacs,v 1.13 2005/03/30 22:08:58 chlunde Exp $

(when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))
(tool-bar-mode 0)
;(transient-mark-mode t)                 ; highlight when mark
(global-font-lock-mode t)               ; fontify when possible
(column-number-mode t)
(line-number-mode t)
;(blink-cursor-mode 0)
(menu-bar-mode t)
(iswitchb-mode t)                       ; incremental buffer search
(icomplete-mode t)                      ; lots of incremental complete
(setq icomplete-show-key-bindings t)
(fset 'yes-or-no-p 'y-or-n-p)           ; Be consistent!
(show-paren-mode t)                     ; Highlight matching paren
(auto-compression-mode t)               ; Decompress gz-files etc.
(windmove-default-keybindings)

(setq user-full-name "Carl Henrik Lunde"
      user-mail-address (concat "chlunde" "@" "ping.uio.no"))

(setq require-final-newline 'ask
      show-trailing-whitespace t
      mouse-yank-at-point t ; Middle-mouse-button-paste at point, not mouse
;      inhibit-startup-message t
      tags-revert-without-query t
      visible-bell t)

(setq indent-tabs-mode nil
      c-basic-offset 8
      tab-width 8)

(prefer-coding-system 'utf-8)

(setq calendar-week-start-day 1
      european-calendar-style t)

(setq compilation-window-height 7)

(setq diff-switches "-u")               ; Unified diffs

(setq backup-by-copying t               ; don't clobber symlinks
      backup-directory-alist '(("." . "/home/chlunde/.backups")) ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 200
      kept-old-versions 50
      version-control t)                ; use versioned backups

(setq gc-cons-threshold 15000000	; I have enough ram ;)
      utf-translate-cjk-mode nil)	; I don't read CJK anyway

(defadvice backward-page (after chl-page-start-at-top activate)
  "Recenter window with page break at top"
  (recenter 0))

(defadvice forward-page (after chl-page-start-at-top activate)
  "Recenter window with page break at top"
  (recenter 0))

(global-set-key "\M-g" 'goto-line)

(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "<f6>") 'next-error)

(set-cursor-color "red")

;; Weeeird, this has inhibit-startup-message <= t as side effect -- WTF?
(server-start)

(when window-system
  (set-default-font "-*-terminus-medium-r-*-*-14-*-*-*-*-*-*-*")
;;  (set-default-font "-*-fixed-bold-r-*-*-13-*-*-*-*-80-iso10646-1") Haavard misliker denne
  (set-background-color "black")
  (set-foreground-color "white"))
