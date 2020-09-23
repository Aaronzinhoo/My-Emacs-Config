;;; package --- Summary:
;;; -*- lexical-binding: t -*-
;;; Commentary:
;;;init.el --- Emacs configuration

;;; Code:
;; startup defaults
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq package-enable-at-startup nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil
      ad-redefinition-action 'accept
      calendar-latitude 33.916403
      calendar-longitude -118.352575
      create-lockfiles nil
      select-enable-clipboard t
      inhibit-startup-screen t)

;; load custom faces and vars for packages
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file) (load custom-file))
;; create backup in emacs folder "backup"
(defvar home-directory (expand-file-name "~/.emacs.d/"))
(defvar backup-dir (concat home-directory "backups/"))
(defvar autosave-dir (concat home-directory "autosave/"))
;; make dirs for saving and backing up
(if (not (file-exists-p backup-dir))
    (make-directory backup-dir))
(if (not (file-exists-p autosave-dir))
    (make-directory autosave-dir))
(setq make-backup-files t
      backup-by-copying t ;; safest method to backup
      delete-old-versions t ;; delete excess backups
      delete-by-moving-to-trash t
      kept-old-versions 0
      kept-new-versions 10
      auto-save-default t
      backup-directory-alist `((".*" . ,(concat user-emacs-directory "backups")))
      auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "autosave") t)))

;; improve startup performance
;; disable double buffering if on Windows
(if (and (getenv "PATH") (string-match-p "Windows" (getenv "PATH")))
    (setq default-frame-alist
          (append default-frame-alist '((inhibit-double-buffering . t)))))

;;; custom functions
(defun my-minibuffer-setup-hook ()
  "Make startup faster by increasing garbage can disposal."
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  "Set the garbage can threshold back to default value."
  (setq gc-cons-threshold 800000))
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
(defcustom ccm-vpos-init '(round (window-text-height) 2)
  "This is the screen line position where the cursor initially stays."
  :group 'centered-cursor
  :tag "Vertical cursor position"
  :type '(choice (const :tag "Center" (round (window-text-height) 2))
                 (const :tag "Golden ratio" (round (* 21 (window-text-height)) 34))
                 (integer :tag "Lines from top" :value 10)
                 (const :tag "2 Lines above center" (- (round (window-text-height) 2) 2))))
(defun post-func-recenter (&rest args)
  "Recenter display after func using ARGS as input."
  (recenter))
(defun pop-local-mark-ring ()
    "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
URL `http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html'
Version 2016-04-04"
    (interactive)
    (set-mark-command t))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; load packages and repos
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" home-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously  "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;;; Quick Defaults
(setq-default
 ;; make indent commands use space only (never tab character)
 indent-tabs-mode nil
 tab-width 4
 tab-stop-list (number-sequence 4 120 4)
 scroll-preserve-screen-position t
 scroll-conservatively 10000)
(setq ring-bell-function nil)
(global-hl-line-mode t)
(global-display-line-numbers-mode t)
(column-number-mode t) ;; enable column numbers globally
(global-visual-line-mode t) ;; cause lines to wrap
(setq scroll-bar-mode nil) ;;remove the scroll bar
(put 'erase-buffer 'disabled nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(defalias 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;;; Packages
(use-package s :straight t)
(use-package cl :straight t)
(use-package gh :straight t)
(use-package async :straight t)
(use-package f :straight t)
(use-package pcre2el :straight t)
(use-package bind-key :straight t)
(use-package dash
  :config
  (dash-enable-font-lock))
(use-package xref
  :straight t)
(use-package frame-local
  :straight t)
(use-package compdef
  :straight t)
;; show matching parens by highlighting parens
(use-package delight
  :defer t)
(use-package beginend
  :defer 2
  :hook (after-init . beginend-global-mode)
  :config
  (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
    (diminish mode)))
(use-package paren
  :init
  (setq show-paren-mode t)
  :config
  (setq blink-matching-paren t)
  (setq show-paren-style 'parenthesis))
;; only use agency when windows detected
(cond ((string-equal system-type "windows-nt")
       (progn
         (use-package ssh-agency
           :defer t)))) ;; only needed for windows
(use-package xclip
  :straight t
  :init
  (defun wsl-copy (start end)
    "Copy currently selected text to the Windows clipboard"
    (interactive "r")
    (let ((default-directory "/mnt/c/"))
      (shell-command-on-region start end "clip.exe")))
  (defun wsl-paste ()
    "Paste contents of Windows clipboard to buffer"
    (interactive)
    (let ((coding-system-for-read 'dos)
          (default-directory "/mnt/c/" ))
      (insert (shell-command-to-string
               "powershell.exe -command 'Get-Clipboard'"))))
  :config
  (xclip-mode t)
  (global-set-key (kbd "C-c C-w") 'wsl-copy)
  (global-set-key (kbd "C-c C-y") 'wsl-paste))
;; sets up emacs process with keychain environment variables
(use-package keychain-environment
  :straight t
  :config
  (keychain-refresh-environment))
(use-package auto-package-update
  :defer 10
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))
;; preview line for goto-line
(use-package goto-line-preview
  :commands (goto-line-preview)
  :config
  (global-set-key [remap goto-line] 'goto-line-preview))

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))
(use-package diff-hl
  :straight t
  :hook (prog-mode . diff-hl-mode)
  :config
  (diff-hl-margin-mode t)
  (diff-hl-flydiff-mode t)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
(use-package hl-todo :straight t)
(use-package magit-todos
  :straight t)
(use-package magit
  :defer 5
  :diminish
  :bind ("M-s" . 'magit-status)
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status)
  (setq magit-completing-read-function 'ivy-completing-read))
(use-package better-defaults
  :defer 1)
(use-package grep
  :defer t)
;; Ripgrep
(use-package rg
  :defer 2
  :config
  (global-set-key (kbd "C-M-g") 'rg)
  (global-set-key (kbd "C-M-d") 'rg-dwim))
(use-package hungry-delete
  :straight t
  :config
  (global-hungry-delete-mode))
(use-package powerline
  :straight t
  :init
  (powerline-vc 'center))
(use-package dired-narrow
  :bind (("C-c C-n" . dired-narrow)))
(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)
              ("<tab>" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle)))
(setq dired-listing-switches "-lXGh --group-directories-first"
      dired-dwim-target t)
(use-package diredfl
  :after dired
  :config
  (diredfl-global-mode 1))
(use-package dired-recent
  :config
  (dired-recent-mode  1))
(use-package recentf
  :defer 1
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (setq recentf-max-saved-items 25))
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-c I" . crux-find-user-init-file))
  :init
  (defun aaronzinho-delete-line ()
    "Delete from current position to end of line without pushing to `kill-ring'."
    (interactive)
    (delete-region (point) (line-end-position)))
  (defun aaronzinho-delete-whole-line ()
    "Delete whole line without pushing to kill-ring."
    (interactive)
    (delete-region (line-beginning-position) (line-end-position)))
  (defun crux-smart-delete-line ()
    "Kill to the end of the line and kill whole line on the next call."
    (interactive)
    (let ((orig-point (point)))
      (move-end-of-line 1)
      (if (= orig-point (point))
          (aaronzinho-delete-whole-line)
        (goto-char orig-point)
        (aaronzinho-delete-line))))
  :config
  (global-set-key (kbd "C-k") 'crux-smart-delete-line))
(use-package windmove
  :bind*
  (("M-<right>" . windmove-right)
   ("M-<left>" . windmove-left)
   ("M-<up>" . windmove-up)
   ("M-<down>" . windmove-down)))
(use-package diminish
  :straight t)
(use-package beacon
  :defer 2
  :diminish
  :config
  (setq beacon-color "#111FFF")
  (beacon-mode 1))
(use-package which-key
  :defer 2
  :diminish
  :config
  (which-key-mode t))
(use-package default-text-scale
  :defer 2
  :config
  (global-set-key (kbd "C-M-=") 'default-text-scale-increase)
  (global-set-key (kbd "C-M--") 'default-text-scale-decrease))
(use-package eldoc
  :diminish eldoc-mode)
(use-package flycheck
  :straight t
  :commands flycheck-mode
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (flycheck-add-mode 'json-jsonlint 'json-mode)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode))
(use-package aggressive-indent
  :straight t
  :diminish
  :config
  (global-aggressive-indent-mode 1)
  (append aggressive-indent-excluded-modes '(web-mode html-mode python-mode)))
(use-package expand-region ;; use to highlight more characters with each use
  :straight t
  :bind ("M-2" . 'er/expand-region)
  :init
  (defun er/add-rjsx-mode-expansions ()
    (make-variable-buffer-local 'er/try-expand-list)
    (setq er/try-expand-list (append
                              er/try-expand-list
                              '(er/mark-html-attribute
                                er/mark-inner-tag
                                er/mark-outer-tag))))
  :config
  (delete-selection-mode 1)
  (er/enable-mode-expansions 'rjsx-mode 'er/add-rjsx-mode-expansions))
(use-package all-the-icons
  :defer t)
(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode)
  :init
  (setq company-box-enable-icon (display-graphic-p))
  :config
  (setq company-box-backends-colors nil))
(use-package lsp-mode
  :defer t
  :hook (((c-mode        ; clangd
          c-or-c++-mode  ; clangd
          java-mode      ; eclipse-jdtls
          go-mode
          ) . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook 'lsp-format-buffer)
    (add-hook 'before-save-hook 'lsp-organize-imports)
    (setq lsp-gopls-staticcheck t)
    (setq lsp-eldoc-render-all t)
    (setq lsp-gopls-complete-unimported t))
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-completion-enable t)
  (setq lsp-ui-doc-enable nil)
  (setq read-process-output-max (* 1024 1024)) ;;1MB
  (add-hook 'go-mode-hook 'lsp-go-install-save-hooks))
(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode)
(use-package company-posframe
  :after company
  :diminish company-posframe-mode
  :init (company-posframe-mode t)
  :config
  (setq company-posframe-show-indicator nil)
  (setq company-posframe-show-metadata nil))
(use-package company
  :defer 1
  :diminish company-mode
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-common-or-cycle)
        ("<backtab>" . company-select-previous))
  :init
  (defun company-preview-if-not-tng-frontend (command)
    "`company-preview-frontend', but not when tng is active."
    (unless (and (eq command 'post-command)
                 company-selection-changed
                 (memq 'company-tng-frontend company-frontends))
      (company-preview-frontend command)))
  (setq company-idle-delay 0.0
        company-echo-delay 0 ;; remove annoying blinking
        company-tooltip-flip-when-above t
        company-tooltip-limit 15
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-show-numbers nil
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-transformers '(company-sort-by-backend-importance
                               company-sort-prefer-same-case-prefix
                               company-sort-by-occurrence))
  ;; (setq company-auto-complete #'my-company-visible-and-explicit-action-p)
  (setq company-frontends
        '(
          company-preview-if-not-tng-frontend
          company-pseudo-tooltip-unless-just-one-frontend
          company-preview-if-just-one-frontend
          company-echo-metadata-frontend))
  ;; -----------------------------------------------------------------
  :config
  (global-company-mode t))

(use-package company-quickhelp
  :after company
  :init
  (company-quickhelp-mode t)
  (setq company-quickhelp-delay 0.1)
  )
;; use if only on terminal
(use-package company-quickhelp-terminal
  :defer t
  ;; :init
  ;; (with-eval-after-load 'company-quickhelp
  ;;   (company-quickhelp-terminal-mode 1)
  )
(use-package company-shell
  :straight t
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env company-files)))
(use-package company-jedi
  :commands (jedi:goto-definition jedi-mode company-jedi)
  :bind (:map jedi-mode-map
              ("M-d" . jedi:goto-definition)
              ("M-b" . jedi:goto-definition-pop-marker)))
(use-package smex
  :straight t)
;; IF NEW MACHINE USE M-x all-the-icons-install-fonts
(use-package ivy
  :straight t
  :diminish ivy-mode
  :bind* (("M-x" . counsel-M-x)
          ("C-x b" . ivy-switch-buffer)
          ("C-s" . swiper-isearch)
          ("C-r" . swiper-isearch)
          ("C-x C-f" . counsel-find-file)
          ("C-x C-r" . counsel-recentf)
          ("C-c r" . ivy-resume)
          ("C-c C-r" . counsel-rg)
          ("M-q" . counsel-yank-pop)
          :map ivy-minibuffer-map
          ("C-j" . ivy-immediate-done))
  :config
  (ivy-mode t)
  (setq ivy-extra-directories nil)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy))
(use-package all-the-icons-ivy-rich
  :after (ivy all-the-icons)
  :config (all-the-icons-ivy-rich-mode t))
(use-package ivy-rich
  :after (ivy all-the-icons-ivy-rich)
  :init
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  :config
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode)
          icon))))
  ;; ;; All the icon support to ivy-rich
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-switch-buffer-icon (:width 2))
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))))
  (setq ivy-rich-path-style 'abbrev)
  (ivy-rich-mode 1))
(use-package counsel
  :after (ivy)
  :straight t
  :requires (smex ivy swiper))
(use-package prescient
  :straight t)
(use-package ivy-prescient
  :straight t
  :config
  (setq ivy-prescient-enable-sorting t)
  (setq ivy-prescient-enable-filtering t))
(use-package company-prescient
  :after company
  :config
  (company-prescient-mode t))
(use-package swiper
  :requires (ivy)
  :bind ("M-t" .  swiper-thing-at-point)
  :config
  (setq swiper-action-recenter t))
(use-package ag
  :defer 3)
(use-package avy
  :bind* ("M-SPC" . avy-goto-char)
  :config
  (advice-add 'avy-goto-char :after 'post-func-recenter))
(use-package electric
  :config
  (electric-pair-mode 1))
(use-package multiple-cursors
  :bind (("M-3" . 'mc/mark-next-like-this)
         ("M-1" . 'mc/mark-previous-like-this)
         ("M-m" . 'mc/mark-all-like-this)
         :map mc/keymap
         ("M-h" . 'mc-hide-unmatched-lines-mode)
         ("M-s n" . 'mc/skip-to-next-like-this)
         ("M-s p" . 'mc/skip-to-previous-like-this))
  :hook ((prog-mode . multiple-cursors-mode)
         (text-mode . multiple-cursors-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Yaml editing support and JSON
;; json-mode => json-snatcher json-refactor
(use-package yaml-mode
  :mode (("\\.ya?ml$" . yaml-mode)))
;; use json-mode from https://github.com/joshwnj/json-mode for json instead of js-mode or js2-mode
(use-package json-mode
  :mode ("\\.json" . json-mode)
  :config
  (setq js-indent-level 4))
(use-package dotenv-mode
  :defer t
  :mode ("\\.env\\'" . dotenv-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WEB-DEV CONFIG
(use-package simple-httpd
  :defer t)
(use-package skewer-mode
  :defer t)
(use-package restclient
  :defer t
  :mode ("\\.http\\'" . restclient-mode))
(use-package add-node-modules-path
  :hook ((js2-mode . add-node-modules-path)
         (json-mode . add-node-modules-path))
  :config
  (eval-after-load 'rjsx-mode
    (add-node-modules-path)))
(use-package emmet-mode
  :hook (web-mode . emmet-mode))
(use-package company-web
  :defer t
  :hook (web-mode . (lambda ()
                      (add-to-list 'company-backends 'company-css)
                      (add-to-list 'company-backends 'company-web-html)
                      (add-to-list 'company-backends 'company-web-slim))))
(use-package web-mode
  :defer t
  :hook (html-mode . web-mode)
  :mode (("\\.css\\$" . web-mode)
         ("\\.html\\$" . web-mode))
  :custom
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-attr-indent-offset 2)
  (web-mode-attr-value-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-opening t)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-auto-expanding t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-current-element-highlight t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; JS/react config
;; completetion: tide+company
;; refactor: js-prettier
;; syntax: flycheck
;; linter: flycheck
;; for React development use (setq create-lockfiles nil) to avoid crashes
;; packages needed:
;;     npm install prettier
;;     npm install eslint --save-dev
;;     npx eslint --init
;;     npm install --save typescript
;;     npm install --save @types/browserify
;;     tsc --init
(defun setup-tide-mode ()
    "Basic setup for tide mode."
    (interactive)
    (tide-setup)
    (if (file-exists-p (concat tide-project-root "node_modules/typescript/bin/tsserver"))
        (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver"))
    (tide-hl-identifier-mode t)
    (local-set-key (kbd "C-c d") 'tide-documentation-at-point))
(use-package tide
  :diminish
  :after (rjsx-mode)
  :requires flycheck
  :hook ((rjsx-mode . setup-tide-mode)))
(use-package prettier-js
  :after (rjsx-mode json-mode markdown-mode)
  :hook ((markdown-mode . prettier-js-mode)
         (json-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode))
  :config
  (setq prettier-js-args '("--trailing-comma" "all"
                           "--tab-width" "4"
                           "--bracket-spacing" "false")))
(use-package js-comint
  :defer t
  :init
  (defun inferior-js-mode-hook-setup ()
    (add-hook 'comint-output-filter-functions 'js-comint-process-output))
  :config
  (add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t)
  (add-hook 'js2-mode-hook
            (lambda ()
              (local-set-key (kbd "\C-c !") 'run-js)
              (local-set-key (kbd "\C-c\C-r") 'js-send-region)
              (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
              (local-set-key (kbd "C-c b") 'js-send-buffer)
              (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)))
  (setq inferior-js-program-command "node"))
(use-package rjsx-mode
  :mode (("\\.js\\'" . rjsx-mode)
         ("\\.tsx\\'" . rjsx-mode))
  :config
  (setq js2-mode-show-strict-warnings nil)
  (setq js-indent-level 2)
  (add-hook 'js2-mode-hook #'prettier-js-mode)
  (add-hook 'js2-mode-hook #'setup-tide-mode))

(use-package exec-path-from-shell
  :defer 5
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (push "HISTFILE" exec-path-from-shell-variables))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PYTHON CONFIG
;; PYTHON VERSION NEEDS TO BE ADJUSTED PER SETUP
;; linter/refractor: black
;; syntax on-the-fly: flycheck
;; style: flake8
;; completion: company
;; install black, flake8 ipython, jedi, rope, autopep8, yapf
(use-package python
  :defer t
  :delight " Py"
  :mode ("\\.py" . python-mode)
  :init
  (eval-after-load 'python
    (lambda ()
      (defun python-shell-completion-native-try ()
        "Return non-nil if can trigger native completion."
        (let ((python-shell-completion-native-enable t)
              (python-shell-completion-native-output-timeout
               python-shell-completion-native-try-output-timeout))
          (python-shell-completion-native-get-completions
           (get-buffer-process (current-buffer))
           nil "_")))))
  :config
  (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
  (setq python-indent-offset 4))
(use-package pyenv-mode
  :hook (elpy-mode . pyenv-mode)
  :bind
  ("C-x p e" . pyenv-activate-current-project)
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/"))
(use-package pyenv-mode-auto
  :after pyenv-mode)
;; MAY HAVE TO CHANGE PYTHON PATH
;; INSTALL PYENV, VIRTUALENVWRAPPER to be used by elpy
(use-package elpy
  :defer t
  :diminish ""
  :init (with-eval-after-load 'python (elpy-enable))
  :hook (elpy-mode . flycheck-mode)
  :config
  (add-to-list 'process-coding-system-alist '("elpy" . (utf-8 . utf-8)))
  (setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
  (setq elpy-shell-starting-directory 'current-directory)
  ;;use flake8
  (setq python-check-command "flake8")
  ;;replace flycheck with flymake
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-modules (delq 'elpy-module-company elpy-modules ))
  ;; use jedi for completetions
  (defun company-elpy-setup ()
    (add-to-list 'company-backends 'elpy-company-backend))
  (add-hook 'python-mode-hook 'company-elpy-setup))
(use-package blacken
  :after elpy
  :delight " Bl"
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-line-length '80))

;; Golang Setup
(use-package go-mode
  :straight t
  :mode ("\\.go\\'" . go-mode)
  :init
  ;;Smaller compilation buffer
  (setq compilation-window-height 14)
  (defun my-compilation-hook ()
    (when (not (get-buffer-window "*compilation*"))
      (save-selected-window
        (save-excursion
          (let* ((w (split-window-vertically))
                 (h (window-height w)))
            (select-window w)
            (switch-to-buffer "*compilation*")
            (shrink-window (- h compilation-window-height)))))))
  (defun my-go-mode-hook ()
    ;; Customize compile command to run go build
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v -o ./main")))
  (setq compilation-read-command nil)
  :bind (("M-," . compile)
         ("M-." . godef-jump)
         ("M-*" . pop-tag-mark))
  :config
  (setq compilation-scroll-output t)
  (add-hook 'compilation-mode-hook 'my-compilation-hook)
  (add-hook 'go-mode-hook 'yas-minor-mode)
  (add-hook 'go-mode-hook 'my-go-mode-hook))
;; ----------------------------------------------------------------
(use-package moe-theme
  :straight (:host github
                   :repo "kuanyui/moe-theme.el"
                   :branch "dev")
  :config
  (require 'moe-theme-switcher)
  (setq moe-theme-highlight-buffer-id t)
  (powerline-moe-theme))

;; Helpful Defualt keys
;; C-h k <key> -> describe what key is binded to
;; M-DEL del backward one word

;;CUSTOM EMACS BUILT-IN KEYS
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-l") 'forward-char)
;;(global-set-key (kbd "M-q") 'yank)
(global-set-key (kbd "M-4") 'pop-local-mark-ring)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;;; init.el ends here
