(require 'package)
(add-to-list 'package-archives
      '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(setq install-packages-list
      '(ggtags auto-complete yasnippet counsel
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
(setq-default tab-width 4)
(require 'ggtags)
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
	      (ggtags-mode 1))))
(setq c-default-style "linux")

(define-key ggtags-mode-map (kbd "C-c g d") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
