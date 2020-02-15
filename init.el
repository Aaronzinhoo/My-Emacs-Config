;;; package --- Summary:
; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
;;; Code;
(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/moe-theme.el/")
(add-to-list 'load-path "~/.emacs.d/moe-theme.el/")
(add-to-list 'load-path "~/.emacs.d/sublimity/")
(add-to-list 'load-path "~/.emacs.d/Emacs-imagex/")

(require 'moe-theme)
(require 'better-defaults)
(require 'moe-theme-switcher)
(use-package expand-region) ;; use to highlight more characters with each use
(require 'sublimity)
(require 'sublimity-scroll)
(require 'multiple-cursors)
(require 'company)
(require 'company-tern)
(use-package js2-mode)
(use-package js2-refactor)
(use-package xref-js2)
(require 'pylint)
(use-package ag)
(use-package zoom)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rjsx-mode zoom ag tern-auto-complete company-tern impatient-mode company-jedi smex idle-highlight-in-visible-buffers-mode idle-highlight-mode magit async company git-commit list-packages-ext use-package image+ gnu-elpa-keyring-update magithub pylint python-black multiple-cursors material-theme elpy flycheck better-defaults python)))
 '(python-shell-interpreter "python3")
 '(show-paren-mode t)
 '(zoom-mode t nil (zoom))
 '(zoom-size (quote (0.618 . 0.618))))


(electric-pair-mode 1)
(sublimity-mode 1)
(delete-selection-mode 1)
(global-linum-mode t) ;; enable line numbers globally
(global-company-mode t)
(eval-after-load 'image '(require 'image+))
(eval-after-load 'image+ '(imagex-auto-adjust-mode 1))
(add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))


;; CUSTOM HOT KEYS
(global-set-key (kbd "M-2") 'er/expand-region)
(global-set-key (kbd "M-s") 'magit-status)
;;MOVE TO WINDOW  
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)
(global-set-key (kbd "S-<left>") 'windmove-left)
;; select mult lines
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;;customize which instances changed
(global-set-key (kbd "M->") 'mc/mark-next-like-this)
(global-set-key (kbd "M-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-q") 'yank)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(defvar myPackages
  '(better-defaults
    flycheck
    elpy))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------

(elpy-enable)
(menu-bar-mode 1)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook 'global-company-mode)
;;(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-backends 'company-tern)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq flycheck-python-flake8-executable "flake8")
(setq elpy-rpc-python-command "python3")
(eval-after-load 'image+' '(imagex-global-sticky-mode 1))

(define-key js-mode-map (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)
(add-hook 'js2-mode-hook (lambda ()
                           (auto-complete-mode t)
                           (tern-mode t)
                           (company-mode t)
                           (js2-imenu-extras-mode t)
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
                           (js2-refactor-mode t)))
(eval-after-load 'tern
  '(progn
     (use-package tern-auto-complete)
     (tern-ac-setup)))

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-module-hook 'flycheck-mode))
(defun my/python-mode-hook ()
  ;; loading my python setup
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook #'my/python-mode-hook)
(use-package elpy
             :config
             (setq python-check-command "flake8")
             (setq elpy-shell-starting-directory 'current-directory))
(setq moe-theme-highlight-buffer-id t)

;; init.el ends here

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
