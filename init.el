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
      use-package-enable-imenu-support t
      use-package-always-demand t)
(straight-use-package 'use-package)

;; PACKAGES

;; Core
(use-package no-littering)

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-shell-name (executable-find "zsh"))
  (setq exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-initialize))

(use-package kaolin-themes)

(use-package emacs
  :init
  (progn
    (put 'downcase-region 'disabled nil)
    (put 'magit-clean 'disabled nil)
    (global-visual-line-mode)
    (tooltip-mode -1)
    (set-face-attribute 'default nil :font "Berkeley Mono" :height 215)
    (set-face-attribute 'fixed-pitch nil :font "Berkeley Mono"))

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
          css-indent-offset 2
          gc-cons-threshold 100000000
          read-process-output-max (* 1024 1024)
          mac-right-option-modifier 'none)
    (setq-default indent-tabs-mode nil
                  indicate-empty-lines t))

  :hook
  (prog-mode-hook . subword-mode)
  (prog-mode-hook . whitespace-mode)
  (before-save-hook . whitespace-cleanup)
  (ns-system-appearance-change-functions . drgmr/apply-theme)

  :bind
  (("C-?" . undo-redo)
   ("s-k" . kill-this-buffer)))

(use-package diminish)

(use-package windmove
  :bind
  (("s-<left>" . windmove-left)
   ("s-<right>" . windmove-right)
   ("s-<up>" . windmove-up)
   ("s-<down>" . windmove-down)))

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-sql-indent-offset 2
        web-mode-enable-current-element-highlight t
        web-mode-start-tag-regexp "<\\([[:alpha:]][[:alnum:].:_-\.]*\\|>\\)"
        web-mode-tag-regexp "</?\\([[:alpha:]][[:alnum:].:_-\.]*\\)"))

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
    (add-to-list 'org-link-frame-setup '(file . find-file))
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

(use-package projectile
  :config
  (progn
    (setq projectile-project-search-path '("~/Projects" "~/quicklisp/local-projects")
	  projectile-switch-project-action 'magit-status)
    (add-to-list 'projectile-other-file-alist '("ex" "html.heex"))
    (add-to-list 'projectile-other-file-alist '("html.heex" "ex")))

  :hook
  (after-init-hook . projectile-global-mode)

  :bind
  (("s-p" . projectile-command-map)))

(use-package docker
  :bind
  (("C-x d" . docker)))

(use-package kubernetes
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600)
  :bind
  (("C-x k" . kubernetes-overview)))

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

;; Programming utilities

(use-package company
  :diminish
  :hook (after-init-hook . global-company-mode))

(use-package company-box
  :diminish
  :hook (company-mode-hook . company-box-mode))

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
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-bury-buffer-function #'magit-restore-window-configuration)

  :bind
  (("C-x g" . magit-status)))

(use-package ctrlf
  :init
  (ctrlf-mode))

;; Languages

(use-package elixir-mode
  :config
  (yas-define-snippets 'elixir-mode
                       '(("debug" "IO.inspect(binding(), label: \"#{__ENV__.module}.#{elem(__ENV__.function, 0)}/#{elem(__ENV__.function, 1)}\")")
                         ("pry" "require IEx; IEx.pry()"))))

(use-package exunit
  :hook
  (elixir-mode-hook . exunit-mode))

(use-package flycheck-credo
  :hook
  (elixir-mode-hook . flycheck-mode))

(use-package erlang)

(use-package yaml-mode)

(use-package rust-mode)

(use-package tree-sitter-indent)

(use-package lfe-mode)

(use-package clojure-mode)

(use-package cider)

(use-package sly
  :config
  (setq inferior-lisp-program "sbcl")
  :hook
  (lisp-mode-hook . paredit-mode))

(use-package paredit
  :hook
  (common-lisp-mode-hook . paredit-mode)
  (clojure-mode-hook . paredit-mode))

(use-package zig-mode)

;; Other utilities

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
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-contrib)

(use-package protobuf-mode)

(use-package yasnippet
  :hook
  (prog-mode-hook . yas-minor-mode)
  :config
  (add-to-list 'company-backends 'company-yasnippet))

(use-package yasnippet-snippets)

(use-package swift-mode)

(use-package envrc
  :config
  (envrc-global-mode))

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

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-file-watch-threshold nil)
  :hook
  ((elixir-mode-hook . lsp-deferred)
   (swift-mode-hook . lsp-deferred)
   (zig-mode-hook . lsp-deferred)
   (rust-mode-hook . lsp-deferred)
   (lsp-mode-hook . lsp-enable-which-key-integration)
   (lsp-mode-hook . lsp-ui-mode)))

(use-package lsp-ui
  :commands (lsp-ui-mode lsp-ui-imenu)
  :config
  (setq lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t)
  :bind
  (("C-'" . lsp-ui-imenu)))

(use-package dap-mode)

(require 'dap-elixir)
(require 'dap-erlang)
(require 'dap-gdb-lldb)
(require 'dap-cpptools)

(dap-register-debug-template "Rust::GDB Run Configuration"
                             (list :type "gdb"
                                   :request "launch"
                                   :name "GDB::Run"
                                   :gdbpath "rust-gdb"
                                   :target nil
                                   :cwd nil))
(use-package wgsl-mode
  :straight (:host github :repo "acowley/wgsl-mode"))
