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
(global-set-key (kbd "M-k") 'kill-whole-line)
;; Put all backup (~) files in a single dir
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Fonts
(set-face-attribute 'default nil :family "Monaco")
(set-face-attribute 'default nil :height 140)

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

(defun xah-show-kill-ring ()
  "Insert all `kill-ring' content in a new buffer.
URL `http://ergoemacs.org/emacs/emacs_show_kill_ring.html'
Version 2017-06-19"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (progn
      (switch-to-buffer $buf)
      (funcall 'fundamental-mode)
      (setq buffer-offer-save t)
      (dolist (x kill-ring )
        (insert x "\n--------------------------------------------------\n\n"))
      (goto-char (point-min)))))


;; ----------------------------------------------
;; Mac specific settings
;; ----------------------------------------------

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))


;; ----------------------------------------------
;; Package Configuration
;; ----------------------------------------------

(require 'package)
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
	    ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;;(add-to-list 'load-path "~/.emacs.d/vendor/use-package/")
  (require 'use-package))

(setq use-package-always-ensure t)

(load "~/.emacs.d/vendor/ember-mode/ember-mode")
(load "~/.emacs.d/vendor/fireplace/fireplace")
(load "~/.emacs.d/vendor/key-chord/key-chord")
(key-chord-mode 1)

(key-chord-define-global "xc" `save-buffer)
(key-chord-define-global "fd" `ido-find-file)
(key-chord-define-global "df" `ido-find-file)
(key-chord-define-global "jk" `ido-switch-buffer)
(key-chord-define-global "kj" `ido-switch-buffer)

;; tide setup (typescript mode)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))
;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
      (lambda ()
        (when (string-equal "tsx" (file-name-extension buffer-file-name))
          (setup-tide-mode))))
(setq tide-format-options '(:indentSize 2 :tabSize 2))

;; --------------------------------------------
;; Packages
;; --------------------------------------------

(use-package ag
  :config
  (setq ag-reuse-buffers 't)
  (setq ag-arguments '("--ignore-dir" "dist" "--ignore-dir" "node_modules" "--ignore-dir" "@better"))
  (global-set-key (kbd "C-i") 'ag))

(use-package auto-complete
  :config
  (global-auto-complete-mode t))

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  (key-chord-define-global "w3" 'er/expand-region)
  (key-chord-define-global "3w" 'er/expand-region))

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

(use-package markdown-mode)

(use-package powerline
  :config
  (powerline-center-theme))

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (add-hook 'web-mode-hook
            (lambda ()
              (setq tab-width 2))))

(use-package rjsx-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (setq js-switch-indent-offset 2)
  (setq js-indent-level 2))

(use-package smex
  :config
  (global-set-key (kbd "M-x") 'smex))

(use-package solidity-mode)

(if (display-graphic-p)
  (use-package sublime-themes
    :config
    (load-theme 'brin t)))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook
  (typescript-mode . tide-setup)
  (typescript-mode . tide-hl-identifier-mode)
  (before-save . tide-format-before-save)  )

(use-package transpose-mark)

(use-package yaml-mode)

(use-package python-black
  :demand t
  :after python
  :config (add-hook 'python-mode-hook (lambda () (python-black-on-save-mode))))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(python-black yaml-mode transpose-mark tide web-mode use-package sublime-themes solidity-mode smex rjsx-mode powerline markdown-mode go-mode flycheck fish-mode fiplr expand-region exec-path-from-shell auto-complete ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
