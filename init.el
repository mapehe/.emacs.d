(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)
(global-linum-mode 1) ; always show line numbers
(setq inhibit-startup-screen t)

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
             ("melpa" . "https://melpa.org/packages/")
             ("marmalade" . "https://marmalade-repo.org/packages/")
             ("melpa-stable" . "https://stable.melpa.org/packages/")
             ("elpy" . "https://jorgenschaefer.github.io/packages/")))
(package-initialize)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; load evil
(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config ;; tweak evil after loading it
  (evil-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (avy helm-projectile projectile helm-ag helm-swoop helm-descbinds helm magit evil use-package))))

(defun toggle-evilmode ()
  (interactive)
  (if (bound-and-true-p evil-local-mode)
    (progn
      ; go emacs
      (evil-local-mode (or -1 1))
      (undo-tree-mode (or -1 1))
      (set-variable 'cursor-type 'bar)
    )
    (progn
      ; go evil
      (evil-local-mode (or 1 1))
      (set-variable 'cursor-type 'box)
    )
  )
)

(use-package magit
  :ensure t
  :bind (("C-c m" . magit-status)))

(defun magit-section-highlight-less (section _)
  (magit-section-case
    ((untracked unstaged staged unpushed unpulled hunk file)
     ;; ^ You will likely have to add more here,
     ;; `magit-describe-section' is your friend.
     (magit-section-make-overlay (magit-section-start   section)
                                 (magit-section-content section)
                                 'magit-section-highlight)
  t)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-added ((t (:foreground "#149914" :background nil :inherit nil))))
 '(magit-diff-removed ((t (:foreground "#991414" :background nil :inherit nil)))))

(add-hook 'magit-section-highlight-hook 'magit-section-highlight-less)

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("C-SPC" . helm-dabbrev)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list))
  :bind (:map helm-map
	      ("M-i" . helm-previous-line)
	      ("M-k" . helm-next-line)
	      ("M-I" . helm-previous-page)
	      ("M-K" . helm-next-page)
	      ("M-h" . helm-beginning-of-buffer)
	      ("M-H" . helm-end-of-buffer))
  :config (progn
	    (setq helm-buffers-fuzzy-matching t)
            (helm-mode 1)))
(use-package helm-descbinds
  :ensure t
  :bind ("C-h b" . helm-descbinds))

(use-package helm-files
  :bind (:map helm-find-files-map
	      ("M-i" . nil)
	      ("M-k" . nil)
	      ("M-I" . nil)
	      ("M-K" . nil)
	      ("M-h" . nil)
	      ("M-H" . nil)))
(use-package helm-swoop
  :ensure t
  :bind (("M-m" . helm-swoop)
	 ("M-M" . helm-swoop-back-to-last-point))
  :init
  (bind-key "M-m" 'helm-swoop-from-isearch isearch-mode-map))

(use-package helm-ag
  :ensure helm-ag
  :bind ("M-p" . helm-projectile-ag)
  :commands (helm-ag helm-projectile-ag)
  :init (setq helm-ag-insert-at-point 'symbol
	      helm-ag-command-option "--path-to-ignore ~/.agignore"))

(use-package projectile
  :ensure t
  :bind (("C-c p p" . projectile-switch-project))
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package helm-projectile
  :ensure t
  :bind ("C-c p f" . helm-projectile-find-file)
  :config
  (helm-projectile-on))

(use-package atom-one-dark-theme
  :ensure t
  :config (load-theme 'atom-one-dark t))

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char))

(defun back-frame ()
  (interactive)
  (other-frame -1))

(defun new-make ()
  (interactive)
  (select-frame (make-frame))
  (funcall #'toggle-frame-fullscreen)
  (toggle-scroll-bar -1)
  )

(global-set-key (kbd "M-<right>") 'other-frame)
(global-set-key (kbd "M-<left>") 'back-frame)
(global-set-key (kbd "M-<up>") 'new-make)
(global-set-key (kbd "M-<down>") 'delete-frame)
(global-set-key [(control x) (k)] 'kill-this-buffer)
