;; -----------------------------------------------
;; Basic customizations
;; -----------------------------------------------

; Disable beep
(setq visible-bell t)
; Hide scroll bars
(set-scroll-bar-mode nil)
; Hide toolbar
(tool-bar-mode -1)
; Display line numbers
(setq line-number-mode t)
; Display column numbers
(setq column-number-mode t)
; Don't blink the cursor
(blink-cursor-mode 0)
; Turn ido mode on
(ido-mode t)
; Turn on flexible matching
(setq ido-enable-flex-matching t)
; Wrap at 79 columns by default
(setq-default fill-column 79)
; Highlight open/closing parens
(show-paren-mode 1)
; Newline at end of file
(setq require-final-newline t)
; Highlight trailing whitespace
(setq show-trailing-whitespace t)
; Highlight empty lines at end of file
(setq-default indicate-empty-lines t)
; Cause tab key to indent 4 places
(setq c-basic-offset 4)
; Interpret tab char as 4 places
(setq tab-width 4)
; Insert spaces instead of tabs
(setq-default indent-tabs-mode nil)


;; ----------------------------------------------
;; Advanced customizations
;; ----------------------------------------------

(defun scroll-up-one-line()
    (interactive)
    (scroll-up 1))

(defun scroll-down-one-line()
    (interactive)
    (scroll-down 1))

(global-set-key (kbd "C-.") 'scroll-down-one-line)
(global-set-key (kbd "C-,") 'scroll-up-one-line)


(global-set-key (kbd "C-c m") 'comment-or-uncomment-region)


(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;; ----------------------------------------------
;; Mac specific settings
;; ----------------------------------------------

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))


;; ----------------------------------------------
;; Package Configuration
;; ----------------------------------------------

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

(add-to-list 'load-path "~/.emacs.d/use-package/")

(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)


;; --------------------------------------------
;; use-package config
;; --------------------------------------------

(use-package ag
  :config
  (setq ag-reuse-buffers 't)
  (global-set-key (kbd "C-m") 'ag))

(use-package fiplr
  :config
  (global-set-key (kbd "C-x f") 'fiplr-find-file))

(use-package magit)

(use-package neotree)
