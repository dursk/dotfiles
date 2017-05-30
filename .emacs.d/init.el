;;; init.el --- Summary

;;; Commentary:

;;; Code:

;; -----------------------------------------------
;; Basic customizations
;; -----------------------------------------------

;; Disable welcome screen
(setq inhibit-startup-message t)
;; Get rid of scratch buffer message
(setq initial-scratch-message "")
;; Disable beep
(setq visible-bell t)
;; Hide scroll bars
(set-scroll-bar-mode nil)
;; Hide toolbar
(tool-bar-mode -1)
;; Display line numbers
(setq line-number-mode t)
;; Display column numbers
(setq column-number-mode t)
;; Highlight open/closing parens
(show-paren-mode 1)
;; Newline at end of file
(setq require-final-newline 1)
;; Highlight empty lines at end of file
(setq-default indicate-empty-lines 1)
;; Interpret tab char as 4 places
(setq tab-width 4)
;; Insert spaces instead of tabs
(setq-default indent-tabs-mode nil)
;; Switch windows
(global-set-key (kbd "M-o") 'other-window)
;; Treat snake-case as one word
(global-superword-mode t)
;; Kill line command
(global-set-key "\C-cd" 'kill-whole-line)

;; Fonts
(set-face-attribute 'default nil :family "Monaco")
(set-face-attribute 'default nil :height 130)


;; Fix gross El Capitan rendering issue.
;; http://stuff-things.net/2015/10/05/emacs-visible-bell-work-around-on-os-x-el-capitan/
(setq visible-bell nil)
(setq ring-bell-function 'ignore)


;; ----------------------------------------------
;; Ido mode
;; ----------------------------------------------

;; Turn ido mode on
(ido-mode t)
;; Flex matching for ido
(defvar ido-enable-flex-matching)
(setq ido-enable-flex-matching t)
;; Ido everywhere
(defvar ido-everywhere)
(setq ido-everywhere t)
;; Don't prompt for creating a new buffer
(defvar ido-create-new-buffer)
(setq ido-create-new-buffer 'always)


;; ----------------------------------------------
;; Whitespace mode
;; ----------------------------------------------

(require 'whitespace)
(setq whitespace-line-column 79)
(setq whitespace-style '(face lines-tail trailing))

(global-whitespace-mode 1)


;; ----------------------------------------------
;; Org mode
;; ----------------------------------------------

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (list "~/org/q.org"))

(add-hook 'org-mode-hook (lambda() (setq-local whitespace-line-column 100)))


;; ----------------------------------------------
;; Flyspell Mode (on mac: brew install ipsell)
;; ----------------------------------------------

;; Enable spell checking for all text-modes
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; Spell checking in comments & strings
(add-hook 'python-mode-hook (lambda () (flyspell-prog-mode)))


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


;; Later on I rebind C-i to call 'ag.
;; This unbinds TAB from C-i.
(global-set-key [tab] 'indent-for-tab-command)


;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))


;; Stop asking me about files changed on disk when I switch branches!
(global-auto-revert-mode t)


;; css mode for .scss
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-hook 'css-mode-hook (lambda() (setq tab-width 2)))
(add-hook 'css-mode-hook (lambda() (setq css-indent-offset 2)))


;; C-c C-k to copy line
(defun copy-line (arg)
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (message "%d lines copied" arg))

(global-set-key "\C-c\C-k" 'copy-line)


;; C-c C-y to cut line
(defun cut-line (arg)
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'cut-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end)))
    (delete-region beg end))
  (message "%d lines copied" arg))

(global-set-key "\C-c\C-y" 'cut-line)


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

(add-to-list 'load-path "~/.emacs.d/vendor/use-package/")

(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

(load "~/.emacs.d/vendor/fireplace/fireplace")


;; --------------------------------------------
;; Packages
;; --------------------------------------------

(use-package ace-jump-mode
  :config
  (define-key global-map (kbd "C-c SPC") 'ace-jump-mode))

(use-package ag
  :config
  (setq ag-reuse-buffers 't)
  (global-set-key (kbd "C-i") 'ag))

(use-package auto-complete
  :config
  (global-auto-complete-mode t))

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

(use-package fiplr
  :config
  (global-set-key (kbd "C-x f") 'fiplr-find-file))

(use-package fish-mode)

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package go-mode
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 4))))

;; Run the following command:
;; M-x jedi:install-server RET
;; on a new machine.
(use-package jedi
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
  (setq js2-basic-offset 2))

(use-package markdown-mode)

(use-package powerline
  :config
  (powerline-center-theme))

(use-package web-mode
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (setq tab-width 2))))

(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package neotree)

(use-package smex
  :config
  (global-set-key (kbd "M-x") 'smex))

(if (display-graphic-p)
  (use-package sublime-themes
    :config
    (load-theme 'brin t)))

(use-package yaml-mode)

(provide 'init)
;;; init.el ends here
