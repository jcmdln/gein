;; /etc/emacs/default.el

;;;
;;; Startup
;;;

(when (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))

(setq initial-scratch-message     nil
      inhibit-splash-screen       t
      inhibit-startup-buffer-menu t
      custom-file                 "~/.emacs.d/custom.el")

;; Environment Variables
(setenv "EDITOR"         "emacsclient")
(setenv "GIT_EDITOR"     "emacsclient")
(setenv "GOPATH"         (getenv "GOPATH"))
(setenv "GOBIN"          (getenv "GOBIN"))
(setenv "MANPATH"        (getenv "MANPATH"))
(setenv "PATH"           (getenv "PATH"))
(setenv "PROMPT_COMMAND" "")
(setenv "SHELL"          (getenv "SHELL"))
(setenv "TERM"           (getenv "TERM"))


;;;
;;; Appearance
;;;

(load-theme 'tango-dark)

(set-face-attribute
 'default nil
 :family "Monospace" :weight 'normal
 :width 'normal      :height 96)


;;;
;;; Normalization
;;;

(setq auto-window-vscroll                 nil
      c-basic-offset                      2
      column-number-mode                  t
      cperl-indent-level                  2
      indent-tabs-mode                    nil
      js-indent-level                     2
      prefer-coding-system                'utf-8
      require-final-newline               t
      save-interprogram-paste-before-kill t
      scroll-conservatively               101
      scroll-down-aggressively            0.0
      scroll-margin                       0
      scroll-preserve-screen-position     1
      scroll-step                         1
      scroll-up-aggressively              0.0
      select-enable-primary               nil
      set-default-coding-systems          'utf-8
      set-language-environment            "UTF-8"
      set-locale-environment              "en_US.UTF-8"
      show-paren-delay                    0
      tab-width                           2
      visible-bell                        t)

(global-visual-line-mode t)
(show-paren-mode         t)
(fset 'yes-or-no-p       'y-or-n-p)


;;;
;;; Input
;;;

(setq mouse-wheel-follow-mouse      't
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount     '(3 ((shift) . 3))
      mouse-yank-at-point           t
      xterm-mouse-mode              t)

;; Buffers
(global-set-key (kbd "C-x x")           'kill-buffer-and-window)
(global-set-key (kbd "<C-tab>")         'next-buffer)
(global-set-key (kbd "<C-iso-lefttab>") 'previous-buffer)
(global-set-key (kbd "M--")             (lambda() (interactive)
                                          (split-window-vertically)
                                          (other-window 1 nil)
                                          (switch-to-next-buffer)))
(global-set-key (kbd "M-=")             (lambda() (interactive)
                                          (split-window-horizontally)
                                          (other-window 1 nil)
                                          (switch-to-next-buffer)))

;; Comment or uncomment the highlighted region
(global-set-key (kbd "C-c c")           'comment-or-uncomment-region)

;; Mouse
(global-set-key (kbd "<mouse-4>")       (lambda() (interactive)
                                          (scroll-down 3)))
(global-set-key (kbd "<mouse-5>")       (lambda() (interactive)
                                          (scroll-up 3)))

;; Movement
(global-set-key (kbd "<M-down>")        'windmove-down)
(global-set-key (kbd "<M-left>")        'windmove-left)
(global-set-key (kbd "<M-right>")       'windmove-right)
(global-set-key (kbd "<M-up>")          'windmove-up)
(global-set-key (kbd "C-c <down>")      'windmove-down)
(global-set-key (kbd "C-c <left>")      'windmove-left)
(global-set-key (kbd "C-c <right>")     'windmove-right)
(global-set-key (kbd "C-c <up>")        'windmove-up)


;;;
;;; Package Management
;;;

(require 'package)
(setq package-user-dir "~/.emacs.d/pkg/"
      package-archives
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 3)
        ("GNU ELPA"     . 2)
        ("MELPA"        . 1)))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;;;
;;; Utilities
;;;

(use-package async
  :config (async-bytecomp-package-mode '(all)))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package auto-dictionary)

(use-package company
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'text-mode-hook 'company-mode)
  (setq company-tooltip-limit  20
        company-idle-delay     0.3
        company-echo-delay     0
        company-begin-commands '(self-insert-command)))

;; (use-package company-ansible)
;; (use-package company-emoji)
(use-package company-go)

(use-package company-irony
  :config (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :config
  (add-to-list 'company-backends
               '(company-irony-c-headers company-irony)))

;; (use-package company-jedi)
;; (use-package company-php)
;; (use-package company-rtags)
;; (use-package company-shell)
;; (use-package company-web)

(use-package counsel
  :bind (("<f1> f"  . counsel-describe-function)
         ("<f1> l"  . counsel-find-library)
         ("<f1> v"  . counsel-describe-variable)
         ("<f2> i"  . counsel-info-lookup-symbol)
         ("<f2> u"  . counsel-unicode-char)
         ("C-S-o"   . counsel-rhythmbox)
         ("C-c g"   . counsel-git)
         ("C-c j"   . counsel-git-grep)
         ("C-c l"   . counsel-ag)
         ("C-r"     . counsel-minibuffer-history)
         ("C-x C-f" . counsel-find-file)
         ("C-x l"   . counsel-locate)
         ("M-x"     . counsel-M-x)))

(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'diff-hl-mode)
  (add-hook 'text-mode-hook 'diff-hl-mode))

(use-package eww-lnum
  :config
  (define-key eww-mode-map "f" 'eww-lnum-follow)
  (define-key eww-mode-map "F" 'eww-lnum-universal))

(use-package flycheck
  :config (add-hook 'prog-mode-hook 'flycheck-mode))

(use-package flyspell
  :config
  (add-hook 'flyspell-mode-hook (auto-dictionary-mode 1))
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook     'flyspell-prog-mode)
  (add-hook 'text-mode-hook     'flyspell-mode))

(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (add-hook 'ibuffer-hook
            (lambda()
              (ibuffer-auto-mode             t)
              (ibuffer-do-sort-by-alphabetic t)
              (ibuffer-do-sort-by-major-mode t))))

(use-package irony
  :config
  (add-hook 'c++-mode-hook   'irony-mode)
  (add-hook 'c-mode-hook     'irony-mode)
  (add-hook 'objc-mode-hook  'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (define-key irony-mode-map [remap completion-at-point] 'counsel-irony)
  (define-key irony-mode-map [remap complete-symbol]     'counsel-irony)
  (setq irony-additional-clang-options '("-std=c++14")))

(use-package ivy
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t))

(use-package linum
  :config
  (add-hook 'lisp-mode-hook 'linum-mode)
  (add-hook 'prog-mode-hook 'linum-mode)
  (add-hook 'text-mode-hook 'linum-mode))

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package rainbow-delimiters
  :config
  (add-hook 'markdown-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook     'rainbow-delimiters-mode)
  (add-hook 'text-mode-hook     'rainbow-delimiters-mode))

;; (use-package smartparens
;;   :config
;;   (add-hook 'markdown-mode-hook 'smartparens-mode)
;;   (add-hook 'prog-mode-hook     'smartparens-mode)
;;   (add-hook 'text-mode-hook     'smartparens-mode)
;;   (setq sp-highlight-pair-overlay     nil
;;         sp-highlight-wrap-overlay     nil
;;         sp-highlight-wrap-tag-overlay nil))

(use-package server
  :bind ("C-x C-c" . server-stop)
  :config (unless (server-running-p)(server-start))

  (defun server-kill()
    "Delete current Emacs server, then kill Emacs"
    (interactive)
    (if (y-or-n-p "Kill Emacs without saving? ")
        (kill-emacs)))

  (defun server-stop()
    "Prompt to save buffers, then kill Emacs."
    (interactive)
    (if (y-or-n-p "Quit Emacs? ")
        (save-buffers-kill-emacs)))

  (defun server-update()
    "Refresh package contents, then update all packages."
    (interactive)
    (package-initialize)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-utils-upgrade-all)))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package undo-tree
  :config (global-undo-tree-mode))

;; (use-package xclip
;;   :config (xclip-mode 1))


;;;
;;; Applications
;;;

;; (use-package circe
;;   :config
;;   (if (file-exists-p "~/.emacs.d/circe.el")
;;       (load-file "~/.emacs.d/circe.el"))
;;   (require 'circe-chanop)
;;   (enable-circe-color-nicks)
;;   (defun my-circe-set-margin() (setq left-margin-width 9))
;;   (setf (cdr (assoc 'continuation fringe-indicator-alist)) nil)

;;   (setq circe-default-part-message ""
;;         circe-default-quit-message ""
;;         circe-format-server-topic  "*** Topic: {userhost}: {topic-diff}"
;;         circe-reduce-lurker-spam   t
;;         circe-use-cycle-completion t
;;         lui-fill-type              nil
;;         lui-flyspell-alist         '((".*" "american"))
;;         lui-flyspell-p             t
;;         lui-time-stamp-format      "%H:%M:%S"
;;         lui-time-stamp-position    'left-margin)

;;   (defun my-lui-setup()
;;     (setq fringes-outside-margins t
;;           left-margin-width 9
;;           word-wrap t
;;           wrap-prefix ""))

;;   (defun my-circe-prompt()
;;     (lui-set-prompt
;;      (concat (propertize
;;               (concat (buffer-name) ":")
;;               'face 'circe-prompt-face) " ")))

;;   (defun my-circe-message-option-chanserv (nick user host command args)
;;     (when (and (string= "ChanServ" nick)
;;                (string-match "^\\[#.+?\\]" (cadr args)))
;;       '((dont-display . t))))

;;   (add-hook 'circe-chat-mode-hook           'my-circe-prompt)
;;   (add-hook 'circe-message-option-functions 'my-circe-message-option-chanserv)
;;   (add-hook 'lui-mode-hook (lambda() (my-lui-setup) (my-circe-set-margin))))

;; (use-package elfeed
;;   :bind ("C-x w" . elfeed)
;;   :config
;;   (setq elfeed-search-filter "@1-week-ago +unread "
;;         url-queue-timeout 30)
;;   (if (file-exists-p "~/.emacs.d/elfeed.el")
;;       (load-file "~/.emacs.d/elfeed.el")))

(use-package eshell
  :config
  (setq eshell-cmpl-cycle-completions     nil
        eshell-error-if-no-glob           t
        eshell-hist-ignoredups            t
        eshell-history-size               4096
        eshell-prefer-lisp-functions      t
        eshell-save-history-on-exit       t
        eshell-scroll-to-bottom-on-input  nil
        eshell-scroll-to-bottom-on-output nil
        eshell-scroll-show-maximum-output nil
        eshell-prompt-regexp              "^[^#$\n]*[#$] "
        eshell-prompt-function
        (lambda nil
          (concat
           "[" (user-login-name) "@" (system-name) " "
           (if (string= (eshell/pwd) (getenv "HOME"))
               "~" (eshell/basename (eshell/pwd))) "]"
           (if (= (user-uid) 0) "# " "$ ")))

        eshell-visual-commands
        '("alsamixer" "atop" "htop" "less" "mosh" "nano" "ssh" "tail"
          "top" "vi" "vim" "watch" ))

  (defun eshell/clear()
    (interactive)
    (recenter 0))

  (defun eshell-new()
    "Open a new instance of eshell."
    (interactive)
    (eshell 'N)))

(use-package eww
  :config
  (setq browse-url-browser-function 'eww-browse-url
        shr-blocked-images          "")

  (defun eww-toggle-images()
    "Toggle blocking images in eww."
    (interactive)
    (if (bound-and-true-p shr-blocked-images)
        (setq shr-blocked-images nil)
      (setq shr-blocked-images ""))
    (eww-reload))

  (defun eww-new()
    "Open a new instance of eww."
    (interactive)
    (let ((url (read-from-minibuffer "Enter URL or keywords: ")))
      (switch-to-buffer (generate-new-buffer "*eww*"))
      (eww-mode)
      (eww url))))

;; (use-package gist)

;; (use-package gnus
;;   :config
;;   (gnus-add-configuration
;;    '(article
;;      (horizontal 1.0
;;                  (vertical 25 (group 1.0))
;;                  (vertical 1.0
;;                            (summary 0.25 point)
;;                            (article 1.0)))))
;;   (gnus-add-configuration
;;    '(summary
;;      (horizontal 1.0
;;                  (vertical 25  (group 1.0))
;;                  (vertical 1.0 (summary 1.0 point))))))

(use-package magit)

;; (use-package nov
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; (use-package pdf-tools)
;; (use-package ranger)
;; (use-package realgud)

(use-package scratch
  :config
  (defun scratch-new()
    "Open a new scratch buffer."
    (interactive)
    (switch-to-buffer (generate-new-buffer "*scratch*"))
    (lisp-mode)))


;;;
;;; Language Modes
;;;

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'c-mode-hook
          (lambda()
            (add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
            (setq indent-tabs-mode 1)))

(add-hook 'c++-mode-hook
          (lambda()
            (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
            (setq indent-tabs-mode 1)))

(add-hook 'lisp-mode-hook
          (lambda()
            (add-hook 'before-save-hook
                      (lambda()
                        (untabify (point-min)(point-max))))))

(add-hook 'text-mode-hook
          (lambda()
            (add-hook 'before-save-hook
                      (lambda()
                        (untabify (point-min)(point-max))))))

;; (use-package ahk-mode)
;; (use-package android-mode)
;; (use-package angular-mode)
;; (use-package ansible-vault)
;; (use-package apache-mode)
(use-package cmake-mode)
;; (use-package coffee-mode)
;; (use-package csharp-mode)
;; (use-package cuda-mode)
;; (use-package d-mode)
;; (use-package dart-mode)
;; (use-package docker-compose-mode)
;; (use-package dockerfile-mode)
(use-package dotenv-mode)
;; (use-package es-mode)
;; (use-package fsharp-mode)
(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

;; (use-package go-mode
;;   :config
;;   (add-hook 'before-save-hook 'gofmt-before-save)
;;   (add-hook 'go-mode-hook
;;             (lambda()
;;               (setq tab-width        4
;;                     indent-tabs-mode 1)
;;               (set (make-local-variable 'company-backends) '(company-go))
;;               (company-mode t))))

;; (use-package gradle-mode)
;; (use-package json-mode)
;; (use-package jsx-mode)
;; (use-package less-css-mode)
;; (use-package lua-mode)
;; (use-package markdown-mode)
;; (use-package markdown-preview-mode)
;; (use-package meson-mode)
;; (use-package nginx-mode)
;; (use-package ninja-mode)
;; (use-package npm-mode)
;; (use-package php-mode)
;; (use-package protobuf-mode)
;; (use-package python-mode)
;; (use-package qml-mode)
;; (use-package rust-mode)
;; (use-package sass-mode)
;; (use-package swift-mode)
;; (use-package systemd)
;; (use-package typescript-mode)
;; (use-package vue-mode)
;; (use-package yaml-mode)
