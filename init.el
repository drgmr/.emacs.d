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
(setq use-package-hook-name-suffix nil
      use-package-enable-imenu-support t)
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
    (set-face-attribute 'default nil :font "Inconsolata" :height 250)
    (fset 'yes-or-no-p 'y-or-n-p))

  :config
  (progn
    (defun drgmr/apply-theme (appearance)
      "Load theme, taking current system APPEARANCE into consideration."
      (mapc #'disable-theme custom-enabled-themes)
      (pcase appearance
	('light (load-theme 'modus-operandi t))
	('dark (load-theme 'modus-vivendi t))))

    (setq visible-bell t
	  inhibit-startup-message t
	  auto-save-default nil
	  make-backup-files nil
	  frame-resize-pixelwise t
	  browse-url-browser-function 'xwidget-webkit-browse-url
	  custom-file (no-littering-expand-etc-file-name "custom.el")
	  scheme-program-name "csi -:c"))
  
  :hook
  (prog-mode-hook . subword-mode)
  (ns-system-appearance-change-functions . drgmr/apply-theme)

  :bind
  (("C-?" . undo-redo)))

(use-package windmove
  :bind
  (("s-<left>" . windmove-left)
   ("s-<right>" . windmove-right)
   ("s-<up>" . windmove-up)
   ("s-<down>" . windmove-down)))

(use-package marginalia
  :init
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package which-key
  :diminish
  :config
  (setq which-key-popup-type 'minibuffer)
  :hook
  (after-init-hook . which-key-mode))

(use-package avy
  :bind
  (("C-\"" . avy-goto-char-2)))

(use-package projectile
  :config
  (setq projectile-project-search-path '("~/Projects/perimeter" "~/Projects/personal")
	projectile-switch-project-action 'magit-status)

  :bind
  (("s-p" . projectile-command-map)))

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

(use-package doom-themes)

(use-package doom-modeline
  :config
  (progn
    (setq doom-modeline-height 1)
    (set-face-attribute 'mode-line nil :height 180)
    (set-face-attribute 'mode-line-inactive nil :height 180))
  :hook
  (after-init-hook . doom-modeline-mode))

;; Programming utilities

(use-package company
  :diminish
  :hook (after-init-hook . global-company-mode))

(use-package company-box
  :diminish
  :hook (company-mode-hook . company-box-mode))

(use-package eglot
  :commands eglot)

(use-package vterm
  :config
  (progn
    (defun drgmr/vterm-name (project)
      (concat "<vterm @ " project ">"))

    (defun drgmr/switch-to-project-vterm (&rest args)
      "Switches to a terminal on the project root, creating one if needed."
      (interactive)
      (let ((possible-buffer-name (drgmr/vterm-name (projectile-project-name))))
	(cond
	 ((get-buffer possible-buffer-name)
	  (switch-to-buffer possible-buffer-name))
	 (t
	  (drgmr/new-project-vterm)))))

    (defun drgmr/new-project-vterm (&rest args)
      "Creates a new term on the project's root."
      (interactive)
      (let* ((project-root (projectile-project-root))
	     (project-name (projectile-project-name)))
	(setenv "PROOT" project-root)
	(vterm (generate-new-buffer-name (drgmr/vterm-name project-name)))
	(vterm-send-string (concat "cd " project-root))
	(vterm-send-return)
	(vterm-clear))))
  :bind
  (("C-x t t" . drgmr/switch-to-project-vterm)
   ("C-x t n" . drgmr/new-project-vterm)))

(use-package magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

  :bind
  (("C-x g" . magit-status)))

(use-package ctrlf
  :init
  (ctrlf-mode))

(use-package imenu-list
  :config
  (setq imenu-list-focus-after-activation t)
  :bind
  (("C-'" . imenu-list-smart-toggle)))

;; Languages

(use-package elixir-mode)

(use-package exunit
  :hook
  (elixir-mode-hook . exunit-mode))

(use-package erlang)

(use-package yaml-mode)

(use-package rust-mode)

(use-package gleam-mode
  :straight (:host github :repo "gleam-lang/gleam-mode"))

(use-package lfe-mode)

(use-package clojure-mode)

(use-package cider)

(use-package zig-mode)

;; Other utilities

(use-package ledger-mode
  :config
  (setq ledger-flymake-be-explicit t
	ledger-flymake-be-pedantic t
	ledger-report-use-strict t))

(use-package markdown-mode
  :commands
  (markdown-mode gfm-mode)

  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))

  :init
  (setq markdown-command "multimarkdown"))

(use-package org-mode
  :config
  (setq org-catch-invisible-edits 'error
	org-agenda-files '("~/Projects/roam")
	org-pretty-entities t
	org-pretty-entities-include-sub-superscripts t
	org-use-sub-superscripts t))

(use-package org-roam
  :config
  (setq org-roam-db-location "~/Projects/roam/"))

(use-package ripgrep)
