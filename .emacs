(require 'package)
(add-to-list 'package-archives
      '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(setq install-packages-list
      '(ggtags auto-complete yasnippet counsel smartparens
	       hc-zenburn-theme elpy ivy swiper magit magit-rockstar))
(dolist (package install-packages-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Global UI settings
(tool-bar-mode -1)
(setq inhibit-startup-message t)
(load-theme 'hc-zenburn)
(global-linum-mode) ;; show line numbers for any file
(add-hook 'before-save-hook 'my-prog-nuke-trailing-whitespace)
(defun my-prog-nuke-trailing-whitespace ()
  ;; remove trailing spaces when saving
  ;; in a mode derived from prog mode
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)))

;; Backup settings
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

;; Global Key bindings
(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "C->") 'end-of-buffer)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-v") 'scroll-up)
(global-set-key (kbd "M-v") 'scroll-down)

(defun new-line-below-current ()
    (interactive)
  (let (pos (point))
    (end-of-line)
    (newline-and-indent)))
(global-set-key (kbd "<C-return>") 'new-line-below-current)

(defun new-line-above-current()
  (interactive)
  (let (pos (point)
			(beginning-of-line)
			(newline-and-indent))))
(global-set-key (kbd "C-M-<return>") 'new-line-above-current)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c C-w C-t") 'tabify)
(global-set-key (kbd "C-c C-w C-u") 'untabify)

;; Autocomplete setup
(ac-config-default)

;; Swiper setup
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

;; C/C++ packages setup
(require 'ggtags)
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
	      (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g d") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                            ("* ||\n[i]" "RET"))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
