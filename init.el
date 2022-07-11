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
    (set-face-attribute 'default nil :font "Berkeley Mono" :height 215)
    (set-face-attribute 'fixed-pitch nil :font "Berkeley Mono")
    (fset 'yes-or-no-p 'y-or-n-p)
    (put 'downcase-region 'disabled nil)
    (global-visual-line-mode))

  :config
  (progn
    (defun drgmr/apply-theme (appearance)
      "Load theme, taking current system APPEARANCE into consideration."
      (mapc #'disable-theme custom-enabled-themes)
      (pcase appearance
	('light (load-theme 'kaolin-light t))
	('dark (load-theme 'kaolin-aurora t))))
    (defun drgmr/add-list-to-list (dst src)
      "Similar to `add-to-list', but accepts a list as 2nd argument"
      (set dst (append (eval dst) src)))

    (setq load-prefer-newer t
          visible-bell t
	  inhibit-startup-message t
	  auto-save-default nil
	  make-backup-files nil
	  frame-resize-pixelwise t
	  browse-url-browser-function 'xwidget-webkit-browse-url
	  custom-file (no-littering-expand-etc-file-name "custom.el")
	  scheme-program-name "csi -:c"
	  whitespace-style '(face trailing)
          css-indent-offset 2)
    (setq-default indent-tabs-mode nil))

  :hook
  (prog-mode-hook . subword-mode)
  (prog-mode-hook . whitespace-mode)
  (before-save-hook . whitespace-cleanup)
  (ns-system-appearance-change-functions . drgmr/apply-theme)

  :bind
  (("C-?" . undo-redo)))

(use-package windmove
  :bind
  (("s-<left>" . windmove-left)
   ("s-<right>" . windmove-right)
   ("s-<up>" . windmove-up)
   ("s-<down>" . windmove-down)))

(use-package org
  :config
  (progn
    (setq org-catch-invisible-edits 'error
	  org-agenda-files '("~/Projects/roam")
	  org-pretty-entities t
	  org-pretty-entities-include-sub-superscripts t
	  org-use-sub-superscripts t
          org-return-follows-link t
          org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
    (org-babel-do-load-languages
     'org-babel-load-languages
     (append org-babel-load-languages
	     '((scheme . t)
	       (dot . t)))))
  :hook
  (org-mode-hook . org-indent-mode))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Projects/roam/")
  (org-roam-complete-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain
      "\n\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :config
  (org-roam-setup)
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n t" . org-roam-dailies-goto-today)
   ("C-c n y" . org-roam-dailies-goto-yesterday)))

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
  (setq projectile-project-search-path '("~/Projects" "~/quicklisp/local-projects")
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

(use-package hungry-delete
  :config
  (setq hungry-delete-join-reluctantly t)
  :hook
  (prog-mode-hook . hungry-delete-mode))

;; Visuals

(use-package doom-themes)

;; Programming utilities

(use-package company
  :diminish
  :hook (after-init-hook . global-company-mode))

(use-package company-box
  :diminish
  :hook (company-mode-hook . company-box-mode))

(use-package eglot
  :commands eglot
  :config
  (progn
    (add-to-list 'eglot-server-programs '(elixir-mode "elixir-ls"))
    (add-to-list 'eglot-server-programs '(erlang-mode "erlang_ls"))
    (add-to-list 'eglot-server-programs '((swift-mode :language-id "swift") "xcrun" "sourcekit-lsp")))
  :bind
  (("s-=" . eglot-format)))

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

(use-package elixir-mode
  :hook
  (elixir-mode-hook . eglot-ensure))

(use-package exunit
  :hook
  (elixir-mode-hook . exunit-mode))

(use-package flycheck-credo
  :hook
  (elixir-mode-hook . flycheck-mode))

(use-package erlang)

(use-package yaml-mode)

(use-package rust-mode)

;; (use-package gleam-mode
;;   :straight (:host github :repo "gleam-lang/gleam-mode"))

(use-package tree-sitter-indent)

(use-package lfe-mode)

(use-package clojure-mode)

(use-package cider)

;; (use-package slime
;;   :config
;;   (setq inferior-lisp-program "sbcl"))

(use-package sly
  :config
  (setq inferior-lisp-program "sbcl")
  :hook
  (lisp-mode-hook . paredit-mode))

(use-package paredit
  :hook
  (common-lisp-mode-hook . paredit-mode)
  (clojure-mode-hook . paredit-mode))

(use-package zig-mode
  :hook
  (zig-mode-hook . eglot-ensure))

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

(use-package ripgrep)

(use-package org-tree-slide
  :custom
  (org-image-actual-width nil))

(use-package geiser-chicken)

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :hook
  (after-init-hook . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-contrib)

(use-package ghub
  :config
  (setq auth-sources '("~/.authinfo")))

(use-package forge
  :after magit)

(use-package protobuf-mode)

(use-package yasnippet
  :hook
  (prog-mode-hook . yas-minor-mode))

(use-package yasnippet-snippets)

(use-package persp-mode)

(use-package swift-mode
  :hook
  (swift-mode-hook . eglot-ensure))

(use-package kaolin-themes)

;; ((evil telephone-line-evil-tag-segment)
;;  (accent telephone-line-vc-segment telephone-line-erc-modified-channels-segment telephone-line-process-segment)
;;  (nil telephone-line-projectile-segment telephone-line-buffer-segment))

;; ((nil telephone-line-flycheck-segment telephone-line-misc-info-segment)
;;  (accent telephone-line-major-mode-segment)
;;  (evil telephone-line-airline-position-segment))

(use-package telephone-line
  :hook
  (after-init-hook . telephone-line-mode)
  :config
  (progn
    (setq telephone-line-lhs
          '((evil . (telephone-line-major-mode-segment))
            (accent . (telephone-line-vc-segment))
            (nil . (telephone-line-projectile-buffer-segment)))
          telephone-line-rhs
          '((nil . (telephone-line-misc-info-segment))
            (accent . (telephone-line-airline-position-segment))))))
