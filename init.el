;; Set up package management
(setq straight-bootstrap-file (expand-file-name "var/straight/repos/straight.el/bootstrap.el" user-emacs-directory)
      straight-installer-url "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el")

(setq straight-base-dir (expand-file-name "var/" user-emacs-directory))

(defvar bootstrap-version)
(let ((bootstrap-version 5))
  (unless (file-exists-p straight-bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously straight-installer-url 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load straight-bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(setq use-package-hook-name-suffix nil)
(straight-use-package 'use-package)

;; PACKAGES

;; Core
(use-package diminish)

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-shell-name (executable-find "zsh"))
  (setq exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-initialize))

(use-package no-littering)

(use-package emacs
  :init
  (progn
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (fringe-mode 0)
    (menu-bar-mode -1)
    (set-face-attribute 'default nil :font "Inconsolata" :height 200)
    (fset 'yes-or-no-p 'y-or-n-p))
  
  :config
  (setq visible-bell t
	inhibit-startup-message t
	auto-save-default nil
	make-backup-files nil
	frame-resize-pixelwise t
	browse-url-browser-function 'xwidget-webkit-browse-url
	custom-file (no-littering-expand-etc-file-name "custom.el"))
  
  :hook
  (prog-mode-hook . subword-mode))

(use-package windmove
  :bind
  (("s-<left>" . windmove-left)
   ("s-<right>" . windmove-right)
   ("s-<up>" . windmove-up)
   ("s-<down>" . windmove-down)))

(use-package which-key
  :diminish
  :hook
  (after-init-hook . which-key-mode))

(use-package avy
  :bind
  (("C-'" . avy-goto-char-2)))

(use-package projectile
  :bind
  (("s-p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '("~/Projects/perimeter" "~/Projects/personal")))

(use-package docker
  :bind
  (("s-d" . docker)))

(use-package selectrum
  :hook
  (after-init-hook . selectrum-mode)
  (selectrum-mode-hook . selectrum-prescient-mode))

(use-package selectrum-prescient
  :config
  (prescient-persist-mode +1))

;; Visuals

(use-package doom-themes
  :config
  (load-theme 'doom-nord-light))

;; Programming utilities

(use-package company
  :diminish
  :hook (after-init-hook . global-company-mode))

(use-package company-box
  :diminish
  :hook (company-mode-hook . company-box-mode))

(use-package lsp-mode
  :commands lsp
  :init
  (add-to-list 'exec-path (expand-file-name "~/.local/bin"))
  
  :config
  (setq lsp-clients-elixir-server-executable "elixir-ls")

  :bind
  (("M-?" . lsp-find-references))
  
  :hook
  (lsp-mode-hook . lsp-headerline-breadcrumb-mode)
  (lsp-mode-hook . lsp-ui-mode)
  (lsp-mode-hook . yas-minor-mode))

(use-package lsp-ui)

(use-package vterm
  :config
  (defun drgmr/project-vterm (&rest args)
    "Opens a terminal on the project root, if any."
    (interactive)
    (let ((project-root (projectile-project-root)))
      (setenv "PROOT" (projectile-project-root))
      (vterm)
      (vterm-send-string (concat "cd " project-root))
      (vterm-send-return)
      (vterm-clear)))
  :bind
  (("C-x t" . drgmr/project-vterm)))

(use-package magit
  :bind
  (("C-x g" . magit-status)))

;; Languages

(use-package elixir-mode
  :hook
  (elixir-mode-hook . lsp))

(use-package exunit)

(use-package erlang)

(use-package yaml-mode)

;; Other utilities

