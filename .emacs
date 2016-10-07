(require 'package)
(add-to-list 'package-archives
      '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(setq install-packages-list
      '(ggtags auto-complete yasnippet
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
(defun new-line-below-current ()
    (interactive)
  (let (pos (point))
    (end-of-line)
    (newline-and-indent)))
(global-set-key (kbd "<C-return>") 'new-line-below-current)
(global-set-key (kbd "C-x g") 'magit-status)

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
