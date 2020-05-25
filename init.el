;;; package --- Summary:
;;; -*- lexical-binding: t -*-
(setq-default lexical-binding t)
;;; Commentary:
; init.el --- Emacs configuration

;; INSTALL PACKAGES
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; improve startup performance
(defun my-minibuffer-setup-hook ()
  "Make startup faster by increasing garbage can disposal."
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  "Set the garbage can threshold back to default value."
  (setq gc-cons-threshold 800000))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(progn
  ;; make indent commands use space only (never tab character)
  (setq-default indent-tabs-mode nil)
  ;; emacs 23.1 to 26, default to t
  ;; if indent-tabs-mode is t, it means it may use tab, resulting mixed space and tab
  )
(defcustom ccm-vpos-init '(round (window-text-height) 2)
  "This is the screen line position where the cursor initially stays."
  :group 'centered-cursor
  :tag "Vertical cursor position"
  :type '(choice (const :tag "Center" (round (window-text-height) 2))
                 (const :tag "Golden ratio" (round (* 21 (window-text-height)) 34))
                 (integer :tag "Lines from top" :value 10)
                 (const :tag "2 Lines above center" (- (round (window-text-height) 2) 2))))
(setq x-select-enable-clipboard t)
(setq ad-redefinition-action 'accept) ;;silence ad-handle redefs
;; --------------------------------------


;;; Functions
(defun pop-local-mark-ring ()
    "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
URL `http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html'
Version 2016-04-04"
    (interactive)
    (set-mark-command t))
(global-set-key (kbd "M-4") 'pop-local-mark-ring)


;;; Code:
(auto-complete-mode -1) ;; using company so disable
(use-package ssh-agency) ;; only needed for windows
(use-package auto-package-update
  :defer 10
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))
(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))
(use-package moe-theme)
(use-package moe-theme-switcher
  :load-path "~/.emacs.d/elpa/moe-theme-20200216.1927/")
(use-package beginend
  :diminish ""
  :init
  (beginend-global-mode))
(use-package magit
  :defer 5
  :diminish
  :bind ("M-s" . 'magit-status)
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status)
  (setq magit-completing-read-function 'ivy-completing-read))
(use-package better-defaults)
(use-package hungry-delete
  :config
  (global-hungry-delete-mode))
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
;; (add-hook 'dired-mode-hook 'dired-hide-details-mode)
(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode 1))
(use-package dired-recent
  :ensure t
  :config
  (dired-recent-mode  1))
(use-package recentf
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
  :defer t)
(use-package beacon
  :diminish
  :config
  (setq beacon-color "#111FFF")
  (beacon-mode 1))
(use-package which-key
  :diminish
  :config
  (which-key-mode t))
(use-package default-text-scale
  :config
  (global-set-key (kbd "C-M-=") 'default-text-scale-increase)
  (global-set-key (kbd "C-M--") 'default-text-scale-decrease))
(use-package flycheck
  :init (global-flycheck-mode))
(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))
(use-package company
  :diminish company-mode
  :custom
  (company-dabbrev-ignore-case nil)
  :init
  ;; set pop up behaviour like autocomplete
  (defun company-complete-common-or-cycle-if-tooltip-visible-or-complete-selection ()
      "Complete common or cycle if appropriate, or complete selection.
Insert selection if only preview is showing or only one candidate,
otherwise complete common or cycle."
      (interactive)
      (if (and (company-tooltip-visible-p) (> company-candidates-length 1))
          (call-interactively 'company-complete-common-or-cycle)
            (call-interactively 'company-complete-selection)))
  (defun my-company-visible-and-explicit-action-p ()
    (and (company-tooltip-visible-p)
         (company-explicit-action-p)))
  (defun company-ac-setup ()
    "Sets up `company-mode' to behave similarly to `auto-complete-mode'."
    (setq company-require-match nil)
    (setq company-auto-complete #'my-company-visible-and-explicit-action-p)
    (setq company-frontends '(company-echo-metadata-frontend
                              company-pseudo-tooltip-unless-just-one-frontend-with-delay
                              company-preview-frontend))
    (define-key company-active-map [tab]
      'company-complete-common-or-cycle-if-tooltip-visible-or-complete-selection)
    (define-key company-active-map (kbd "TAB")
      'company-complete-common-or-cycle-if-tooltip-visible-or-complete-selection))
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-transformers '(company-sort-by-backend-importance
                               company-sort-prefer-same-case-prefix
                               company-sort-by-occurrence))
  ;; -----------------------------------------------------------------
  :config
  (global-company-mode 1)
  (set-face-attribute
     'company-preview
     nil
     :background (face-attribute 'company-preview-common :background))
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (company-ac-setup))
(use-package company-quickhelp
  :defer t
  :init (with-eval-after-load 'company
          (company-quickhelp-mode)
          (setq company-quickhelp-delay 1.5)
          (setq company-quickhelp-color-background "#FFF08A")))
(use-package company-quickhelp-terminal
  :defer t
  :init
  (with-eval-after-load 'company-quickhelp
    (company-quickhelp-terminal-mode 1)))
(use-package company-jedi
  :defer t
  :commands (jedi:goto-definition jedi-mode company-jedi)
  :bind (("M-d" . jedi:goto-definition)
         ("M-b" . jedi:goto-definition-pop-marker))
  :config
  (defun enable-jedi()
    (setq-local company-backends
                (append '(company-jedi) company-backends)))
  (with-eval-after-load 'company
          (add-hook 'python-mode-hook 'enable-jedi))
  (setq jedi:complete-on-dot t))
;; ---------------------------------------------------

(use-package expand-region ;; use to highlight more characters with each use
  :bind ("M-2" . 'er/expand-region)
  :config
  (delete-selection-mode 1))
(use-package smex)
(use-package all-the-icons)
(use-package ivy-rich
  :after ivy
  :config
  ;; (defun ivy-rich-switch-buffer-icon (candidate)
  ;;   (with-current-buffer
  ;;       (get-buffer candidate)
  ;;     (let ((icon (all-the-icons-icon-for-mode major-mode)))
  ;;       (if (symbolp icon)
  ;;           (all-the-icons-icon-for-mode 'fundamental-mode)
  ;;         icon))))

  ;; ;; All the icon support to ivy-rich
  ;; (setq ivy-rich-display-transformers-list
  ;;       '(ivy-switch-buffer
  ;;         (:columns
  ;;          ((ivy-rich-switch-buffer-icon (:width 2))
  ;;           (ivy-rich-candidate (:width 30))
  ;;           (ivy-rich-switch-buffer-size (:width 7))
  ;;           (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
  ;;           (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
  ;;           (ivy-rich-switch-buffer-project (:width 15 :face success))
  ;;           (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
  ;;          :predicate
  ;;          (lambda (cand) (get-buffer cand)))))
  ;; Add custom icons for various modes that can break ivy-rich
  (add-to-list 'all-the-icons-mode-icon-alist '(dashboard-mode all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-dsilver))
  (add-to-list 'all-the-icons-mode-icon-alist '(ess-mode all-the-icons-fileicon "R" :face all-the-icons-lblue))
  (setq ivy-rich-path-style 'abbrev)
  (ivy-rich-mode 1))
(use-package counsel
  :after smex
  :requires (smex ivy swiper))
(use-package prescient)
(use-package ivy-prescient
  :after counsel
  :config
  (setq ivy-prescient-enable-sorting t)
  (setq ivy-prescient-enable-filtering t))
(use-package company-prescient
  :config
  (company-prescient-mode t))
(use-package ivy :demand
  :diminish ivy-mode
  :bind* (("M-x" . counsel-M-x)
          ("C-x b" . ivy-switch-buffer)
          ("C-s" . swiper-isearch)
          ("C-r" . swiper-isearch)
          ("C-x C-f" . counsel-find-file)
          ("C-x C-r" . counsel-recentf))
  :config
  (ivy-mode 1)
  (setq ivy-extra-directories nil)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))
(use-package swiper
  :requires (ivy)
  :bind ("M-." .  swiper-thing-at-point)
  :config
  (setq swiper-action-recenter t))
(use-package ag)
(use-package avy
  :bind* ("M-SPC" . avy-goto-char))
(use-package electric
  :config
  (electric-pair-mode 1))
(use-package multiple-cursors
  :bind (("M-3" . 'mc/mark-next-like-this)
         ("M-1" . 'mc/mark-previous-like-this)
         ("M-m" . 'mc/mark-all-like-this))
  :hook ((prog-mode . multiple-cursors-mode)
         (text-mode . multiple-cursors-mode)))

;; JAVASCRIPT/REACT CONFIG
;; completion: auto-complete
;; refactor: js2-refactor-mode
;; syntax: js2-mode
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (define-key js-mode-map (kbd "M-.") nil)
  (js2-imenu-extras-mode t))
(use-package js2-refactor
  :defer t
  :hook (js2-mode . js2-refactor-mode))
(use-package xref-js2
  :defer t)

;; PYTHON CONFIG
;; linter/refractor: black
;; syntax on-the-fly: flycheck
;; style: flake8
;; completion: company
(use-package python
  :defer t
  :mode ("\\.py" . python-mode)
  :config
  (setq python-shell-interpreter "python")
  (setq py-python-command "python")
  (setq python-indent-offset 4))
(use-package elpy
  :defer t
  :diminish ""
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-rpc-python-command "python")
  (setq elpy-shell-starting-directory 'current-directory)
  ;;use flake8
  (setq python-check-command "flake8")
  ;;replace flycheck with flymake
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  ;;use black to autoformat code before saving
  (add-hook 'elpy-mode-hook (lambda ()
                              (add-hook 'before-save-hook
                                        'elpy-black-fix-code nil t))))
;; ----------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-dabbrev-ignore-case nil)
 '(package-selected-packages
   (quote
    (dired-recent diredfl ivy-rich all-the-icons beginend default-text-scale company-prescient ivy-prescient prescient benchmark-init flx company-quickhelp-terminal crux company-box company-quickhelp counsel ace-jump ace-jump-mode diminish flycheck auto-package-update electric-pair-mode moe-theme-switcher electric-pair ssh-agency jedi moe-theme bind-map rjsx-mode ag company-tern impatient-mode company-jedi smex idle-highlight-in-visible-buffers-mode idle-highlight-mode magit async git-commit list-packages-ext use-package image+ gnu-elpa-keyring-update magithub pylint python-black multiple-cursors material-theme elpy better-defaults python))))

(show-paren-mode 1)
(setq show-paren-style 'expression)
(setq inhibit-startup-screen t)
(put 'erase-buffer 'disabled nil)
;; (sublimity-mode 1)
(global-hl-line-mode t)
(global-linum-mode 1) ;; enable line numbers globally
(column-number-mode 1) ;; enable column numbers globally
(global-visual-line-mode 1) ;; cause lines to wrap
(scroll-bar-mode -1) ;;remove the scroll bar
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)
;; regular cursor (only works in GUI)
(setq cursor-type '(bar . 5))
(setq-default blink-cursor-blinks 0)
(setq scroll-preserve-screen-position t
      scroll-conservatively 10000)
(defun post-func-recenter (&rest args)
  "Recenter display after func using ARGS as input."
  (recenter))
(advice-add 'avy-goto-char :after 'post-func-recenter)
;; create backup in emacs folder "backup"
(defvar backup-dir (expand-file-name "~/.emacs.d/emacs_backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir))
      auto-save-list-file-prefix autosave-dir
      auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;;CUSTOM HOT KEY

;;CUSTOM EMACS BUILT-IN KEYS
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-q") 'yank)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; -----------------------------------------------------------

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white"))))
 '(ivy-current-match ((t (:background "black" :foreground "brightcyan"))))
 '(ivy-minibuffer-match-highlight ((t (:background "color-27" :foreground "brightwhite"))))
 '(magit-diff-base-highlight ((t (:inherit magit-section-highlight :background "blue" :foreground "#fce94f" :weight bold))))
 '(swiper-background-match-face-2 ((t (:background "lightgray" :foreground "color-235"))))
 '(swiper-match-face-2 ((t (:background "color-21" :foreground "#eeeeee" :weight bold)))))

;;; init.el ends here
