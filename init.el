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

(use-package straight
  :custom
  (straight-built-in-pseudo-packages '(emacs nadvice python image-mode project flymake xref)))

(use-package no-littering)

(use-package emacs
  :init
  (progn
    (put 'downcase-region 'disabled nil)
    (put 'magit-clean 'disabled nil)
    (global-visual-line-mode)
    (tooltip-mode -1)
    (set-face-attribute 'default nil :font "Berkeley Mono" :height 215)
    (set-face-attribute 'fixed-pitch nil :font "Berkeley Mono")
    (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend))

  :config
  (progn
    (defun drgmr/apply-theme (appearance)
      "Load theme, taking current system APPEARANCE into consideration."
      (mapc #'disable-theme custom-enabled-themes)
      (pcase appearance
      	('light (load-theme 'modus-operandi-tinted t))
      	('dark (load-theme 'modus-vivendi-tinted t))))

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
      	  whitespace-style '(face trailing)
          css-indent-offset 2
          gc-cons-threshold 100000000
          read-process-output-max (* 1024 1024)
          mac-right-option-modifier 'none
          js-indent-level 2
          native-comp-async-report-warnings-errors 'silent)

    (setq-default indent-tabs-mode nil
                  indicate-empty-lines t)

    (setq treesit-language-source-alist
          '((elixir . ("https://github.com/elixir-lang/tree-sitter-elixir.git" "v0.2.0"))
            (heex . ("https://github.com/phoenixframework/tree-sitter-heex.git" "v0.6.0")))
          major-mode-remap-alist
          '((elixir-mode . elixir-ts-mode)
            (heex-mode . heex-ts-mode))))

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
    (setq projectile-project-search-path '("~/Projects")
      	  projectile-switch-project-action 'magit-status
          projectile-kill-buffers-filter 'kill-only-files)
    (add-to-list 'projectile-other-file-alist '("ex" "html.heex"))
    (add-to-list 'projectile-other-file-alist '("html.heex" "ex")))

  :hook
  (after-init-hook . projectile-global-mode)

  :bind
  (("s-p" . projectile-command-map)))

(use-package docker
  :bind
  (("C-x d" . docker)))

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

(use-package elixir-mode)

(use-package exunit
  :hook
  (elixir-ts-mode-hook . exunit-mode))

(use-package erlang)

(use-package yaml-mode)

(use-package tree-sitter-indent)

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

(use-package protobuf-mode)

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

(use-package eglot
  :hook
  (elixir-ts-mode-hook . eglot-ensure))
