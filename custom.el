(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(company-dabbrev-ignore-case nil)
 '(org-structure-template-alist
   (quote
    (("py" . "src python")
     ("js" . "src js")
     ("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse"))))
 '(package-selected-packages
   (quote
    (keychain-environment company company-eclim go-mode lsp-ui frame-local pyenv-mode-auto pyenv-mode rg delight blacken powerline powermo company-posframe company-postframe company-box simpleclip all-the-icons-ivy-rich tide flycheck exec-path-from-shell company-web skewer-mode simple-httpd js2 prettier-js rjsx json-snatcher json-reformat js-comint web-mode emmet-mode add-node-modules-path yaml-mode lsp-mode dired-recent diredfl ivy-rich all-the-icons beginend default-text-scale company-prescient ivy-prescient prescient benchmark-init flx company-quickhelp-terminal crux company-quickhelp counsel ace-jump ace-jump-mode diminish auto-package-update electric-pair-mode moe-theme-switcher electric-pair ssh-agency jedi moe-theme bind-map rjsx-mode ag company-tern impatient-mode company-jedi smex idle-highlight-in-visible-buffers-mode idle-highlight-mode magit async git-commit list-packages-ext use-package image+ gnu-elpa-keyring-update magithub pylint python-black multiple-cursors material-theme elpy better-defaults python)))
 '(web-mode-attr-indent-offset 2)
 '(web-mode-attr-value-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-auto-closing t)
 '(web-mode-enable-auto-expanding t)
 '(web-mode-enable-auto-opening t)
 '(web-mode-enable-auto-pairing t)
 '(web-mode-enable-css-colorization t)
 '(web-mode-enable-current-column-highlight t)
 '(web-mode-enable-current-element-highlight t)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-subtree-depth-1-face ((t (:background "slate blue"))))
 '(ivy-minibuffer-match-face-2 ((t (:background "steel blue" :foreground "#eeeeee" :weight bold))))
 '(swiper-background-match-face-2 ((t (:background "steel blue" :foreground "gainsboro"))))
 '(swiper-match-face-1 ((t (:background "light cyan" :foreground "dim gray" :weight bold))))
 '(swiper-match-face-2 ((t (:background "light coral" :foreground "white smoke" :weight bold)))))
(provide 'custom)
;;; custom.el ends here
