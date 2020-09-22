(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)
(toggle-scroll-bar -1)
(global-linum-mode 1) ; always show line numbers
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq echo-keystrokes 0.1)
(setq inhibit-startup-screen t)
(setq-default word-wrap t)

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(defun back-frame ()
  (interactive)
  (other-frame -1))

(defun new-make ()
  (interactive)
  (select-frame (make-frame))
  (funcall #'toggle-frame-fullscreen)
  (toggle-scroll-bar -1)
  )


(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" .
                          ;; "https://elpa.zilongshanren.com/melpa/"
                          "https://melpa.org/packages/"
                          )
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;; load evil
(use-package evil
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
 '(counsel-find-file-ignore-regexp "\\.DS_Store\\|.git")
 '(package-selected-packages
   (quote
    (scalariform js2-mode prettier-js elpy auctex acutex ace-window counsel-projectile counsel company-lsp yasnippet lsp-ui lsp-mode sbt-mode scala-mode atom-one-dark-theme helm-files company tide typescript-mode web-mode company-quickhelp avy helm-projectile projectile helm-ag helm-swoop helm-descbinds helm magit evil use-package))))

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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-added ((t (:foreground "#149914" :background nil :inherit nil))))
 '(magit-diff-removed ((t (:foreground "#991414" :background nil :inherit nil)))))

(use-package helm
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
  :bind (:map helm-find-files-map
	      ("TAB" . helm-execute-persistent-action))
  :config (progn
	    (setq helm-buffers-fuzzy-matching t)
            (helm-mode 1)))

(use-package helm-descbinds
  :bind ("C-h b" . helm-descbinds))

(use-package helm-swoop
  :bind (("M-m" . helm-swoop)
	 ("M-M" . helm-swoop-back-to-last-point))
  :init
  (bind-key "M-m" 'helm-swoop-from-isearch isearch-mode-map))

(use-package projectile
  :ensure t
  :commands (projectile-find-file projectile-switch-project)
  :diminish projectile-mode
  :init
  (use-package helm-projectile
    :ensure t
    :bind (("C-c p f" . helm-projectile-find-file)
           ("C-c p s s" . helm-projectile-ack)
           ("C-c p p" . helm-projectile-switch-project)))
  :config
  (projectile-global-mode))

(use-package atom-one-dark-theme
  :config (load-theme 'atom-one-dark t))

(use-package avy
  :bind ("M-s" . avy-goto-char))

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
 
(use-package company
  :config
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))
 
(use-package company-quickhelp
  :init
  (company-quickhelp-mode 1)
  (use-package pos-tip))
 
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2
 
        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
	web-mode-enable-auto-indentation nil
        )
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
		(setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode))
 
(use-package typescript-mode
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode))
 
(use-package tide
  :init
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))
 
(use-package css-mode
  :config

(setq css-indent-offset 2))
;; Enable scala-mode and sbt-mode
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook (scala-mode . lsp)
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui)

;; lsp-mode supports snippets, but in order for them to work you need to use yasnippet
;; If you don't want to use snippets set lsp-enable-snippet to nil in your lsp-mode settings
;;   to avoid odd behavior with snippets and indentation
(use-package yasnippet)

;; Add company-lsp backend for metals
(use-package company-lsp)

(use-package ace-window
  :config
  (setq aw-leading-char-style 'path)
  (setq aw-keys '(?h ?t ?u ?e))
  (setq aw-scope 'frame)
  )

(use-package auctex
  :defer t
  :ensure t
  :config
  (setq TeX-auto-save t))

(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-mode)))

(use-package scalariform
  :defer t
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode)) ;; Override?
(setq web-mode-content-types-alist
  '(("jsx"  . "/some/react/path/.*\\.js[x]?\\'")))

(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "M-<right>") 'other-frame)
(global-set-key (kbd "M-<left>") 'back-frame)
(global-set-key (kbd "M-<up>") 'new-make)
(global-set-key (kbd "M-<down>") 'delete-frame)
(global-set-key [(control x) (k)] '(lambda () (interactive) (kill-buffer (current-buffer))))
