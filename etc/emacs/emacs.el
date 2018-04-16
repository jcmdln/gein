;; /etc/emacs/default.el

;; When Emacs is opened we should first adjust the appearance to what we
;; desire so that while configurations are being parsed we do not see the
;; default theme. We will disable various tool bars before they
;; initialize, then set the theme and font appearance.

(when (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))

(load-theme 'tango-dark)

(set-face-attribute
 'default nil
 :family "Monospace" :weight 'normal
 :width 'normal      :height 96)


;; In this section we will set some ideal default behaviors such as
;; inhibiting any unwanted initial buffers, forcing utf-8 encoding,
;; showing column numbers in the mode line, enabling the visual-bell
;; which disables the audible bell, and moving custom configuration
;; to another file as to not append this to the bottom of this file.

(setq
 ;; Misc
 initial-scratch-message nil
 inhibit-splash-screen t
 inhibit-startup-buffer-menu t
 custom-file "~/.emacs.d/custom.el"

 ;; Encoding
 prefer-coding-system 'utf-8
 set-default-coding-systems 'utf-8
 set-language-environment "UTF-8"
 set-locale-environment "en_US.UTF-8")


;; Whenever possible, we should not load custom configurations until
;; after Emacs has finished initializing. This section relies on the
;; 'after-init-hook' to accomplish this.

(add-hook
 'after-init-hook
 (lambda()
   (add-hook 'before-save-hook 'delete-trailing-whitespace)

   (add-hook 'lisp-mode-hook
             (lambda()
               (add-hook 'before-save-hook
                         (lambda() (untabify (point-min)(point-max))))
               (linum-mode)))

   (add-hook 'prog-mode-hook
             (lambda()
               (linum-mode)))

   (add-hook 'text-mode-hook
             (lambda()
               (add-hook 'before-save-hook
                         (lambda() (untabify (point-min)(point-max))))
               (linum-mode)))

   (add-hook 'c-mode-hook
             (lambda()
               (add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
               (setq indent-tabs-mode 1)))

   (add-hook 'c++-mode-hook
             (lambda()
               (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
               (setq indent-tabs-mode 1)))

   (add-hook 'python-mode-hook
             (lambda()
               (add-hook 'before-save-hook
                         (lambda() (untabify (point-min)(point-max))))))

   (setq
    ;; Cut/Paste
    require-final-newline t
    save-interprogram-paste-before-kill t
    select-enable-primary nil

    ;; Display
    column-number-mode t
    visible-bell t

    ;; Indentation
    c-basic-offset 4
    cperl-indent-level 4
    js-indent-level 4
    tab-width 4
    indent-tabs-mode nil

    ;; Mouse
    mouse-wheel-follow-mouse 't
    mouse-wheel-progressive-speed nil
    mouse-wheel-scroll-amount '(3 ((shift) . 3))
    mouse-yank-at-point t

    ;; Scrolling
    auto-window-vscroll nil
    scroll-conservatively 101
    scroll-down-aggressively 0.0
    scroll-margin 0
    scroll-preserve-screen-position 1
    scroll-step 1
    scroll-up-aggressively 0.0)

   ;; Shorter y/n prompts
   (fset 'yes-or-no-p 'y-or-n-p)

   ;; Globally enable word-wrap
   (global-visual-line-mode t)

   ;; Highlight matching parenthesis
   (show-paren-mode t)

   ;; Enable mouse support in terminal
   (xterm-mouse-mode t)

   ;; Environment Variables
   (setenv "EDITOR"         "emacsclient")
   (setenv "GIT_EDITOR"     "emacsclient")
   (setenv "GOPATH"         (getenv "GOPATH"))
   (setenv "MANPATH"        (getenv "MANPATH"))
   (setenv "PATH"           (getenv "PATH"))
   (setenv "PROMPT_COMMAND" "")
   (setenv "SHELL"          (getenv "SHELL"))
   (setenv "TERM"           (getenv "TERM"))

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

   ;; Autoload custom file
   (if (file-exists-p custom-file)
       (load custom-file)
     (write-region "" nil custom-file))))


;; Package management in Emacs has a few hiccups and this section is
;; meant to mitigate them. We will set the repositories to use and the
;; order in which to use them so that we are using stable packages
;; whenever possible. Additionally, we will install use-package if it is
;; not already present and set 'ensure' on all future use-package
;; declarations.

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


;; This section contains use-package definitions for any package that
;; currently ships with Emacs. If the package does not exist such as
;; with older versions, they will be installed. Extra consideration has
;; been taken to only apply settings when a package is used as to reduce
;; the time needed to initialize Emacs.

(use-package async
  :config (async-bytecomp-package-mode '(all)))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package auto-dictionary)

(use-package eshell
  :config
  (setq eshell-cmpl-cycle-completions nil
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-history-size 4096
        eshell-prefer-lisp-functions t
        eshell-save-history-on-exit t
        eshell-scroll-to-bottom-on-input nil
        eshell-scroll-to-bottom-on-output nil
        eshell-scroll-show-maximum-output nil

        eshell-visual-commands
        '("alsamixer" "htop" "top" "nano" "vi" "vim" "less" "ssh" "tail"
          "watch")

        eshell-prompt-regexp "^[^#$\n]*[#$] "

        eshell-prompt-function
        (lambda nil
          (concat
           "[" (user-login-name) "@" (system-name) " "
           (if (string= (eshell/pwd) (getenv "HOME"))
               "~" (eshell/basename (eshell/pwd)))
           "]"
           (if (= (user-uid) 0) "# " "$ "))))

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
        shr-blocked-images "")

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

(use-package gnus
  :config
  (gnus-add-configuration
   '(article
     (horizontal 1.0
                 (vertical 25 (group 1.0))
                 (vertical 1.0
                           (summary 0.25 point)
                           (article 1.0)))))
  (gnus-add-configuration
   '(summary
     (horizontal 1.0
                 (vertical 25  (group 1.0))
                 (vertical 1.0 (summary 1.0 point))))))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (add-hook 'ibuffer-hook
            (lambda()
              (ibuffer-do-sort-by-alphabetic)
              (ibuffer-do-sort-by-major-mode)
              (ibuffer-auto-mode t))))

(use-package scratch
  :config
  (defun scratch-new()
    "Open a new scratch buffer."
    (interactive)
    (switch-to-buffer (generate-new-buffer "*scratch*"))
    (lisp-mode)))

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

(use-package xclip
  :config (xclip-mode 1))


;; This section defines additional packages which typically do not ship
;; with emacs that will be installed and configured when issued.

(use-package circe
  :config
  (if (file-exists-p "~/.emacs.d/circe.el")
      (load-file "~/.emacs.d/circe.el"))

  (setq circe-default-part-message ""
        circe-default-quit-message ""
        circe-format-server-topic "*** Topic change by {userhost}: {topic-diff}"
        circe-reduce-lurker-spam t
        circe-use-cycle-completion t
        lui-fill-type nil
        lui-flyspell-alist '((".*" "american"))
        lui-flyspell-p t
        lui-time-stamp-format "%H:%M:%S"
        lui-time-stamp-position 'left-margin)

  (require 'circe-chanop)
  (enable-circe-color-nicks)

  (defun my-circe-set-margin() (setq left-margin-width 9))
  (setf (cdr (assoc 'continuation fringe-indicator-alist)) nil)

  (defun my-lui-setup()
    (setq fringes-outside-margins t
          left-margin-width 9
          word-wrap t
          wrap-prefix ""))

  (defun my-circe-prompt()
    (lui-set-prompt
     (concat (propertize
              (concat (buffer-name) ">")
              'face 'circe-prompt-face) " ")))

  (defun my-circe-message-option-chanserv (nick user host command args)
    (when (and (string= "ChanServ" nick)
               (string-match "^\\[#.+?\\]" (cadr args)))
      '((dont-display . t))))

  (add-hook 'circe-chat-mode-hook 'my-circe-prompt)
  (add-hook 'circe-message-option-functions 'my-circe-message-option-chanserv)
  (add-hook 'lui-mode-hook (lambda() (my-lui-setup) (my-circe-set-margin))))

(use-package cmake-ide
  :config (cmake-ide-setup))

(use-package cmake-mode)

(use-package company
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'text-mode-hook 'company-mode))

(use-package company-irony
  :config (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :config
  (add-to-list 'company-backends
               '(company-irony-c-headers company-irony)))

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

(use-package dockerfile-mode)

(use-package elfeed
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-search-filter "@1-week-ago +unread "
        url-queue-timeout 30)

  (if (file-exists-p "~/.emacs.d/elfeed.el")
      (load-file "~/.emacs.d/elfeed.el")))

(use-package eww-lnum
  :config
  (define-key eww-mode-map "f" 'eww-lnum-follow)
  (define-key eww-mode-map "F" 'eww-lnum-universal))

(use-package flycheck
  :config (add-hook 'prog-mode-hook 'flycheck-mode))

(use-package flymake)

(use-package flyspell
  :config
  (add-hook 'flyspell-mode-hook (auto-dictionary-mode 1))
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode))

(use-package gist)

(use-package gitconfig-mode)

(use-package go-mode
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook
            (lambda()
              (setq tab-width 4
                    indent-tabs-mode 1))))

(use-package haml-mode)

(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

  (setq highlight-indent-guides-method 'character))

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

(use-package json-mode)

(use-package less-css-mode)

(use-package lua-mode)

(use-package magit)

(use-package meson-mode)

(use-package nginx-mode)

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package nov
  :config (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package pdf-tools)

(use-package php-mode)

(use-package rainbow-delimiters
  :config
  (add-hook 'markdown-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook     'rainbow-delimiters-mode)
  (add-hook 'text-mode-hook     'rainbow-delimiters-mode))

(use-package realgud)

(use-package rjsx-mode)

(use-package ranger)

(use-package scss-mode)

(use-package smartparens
  :config
  (add-hook 'markdown-mode-hook 'smartparens-mode)
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (add-hook 'text-mode-hook 'smartparens-mode)

  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package systemd)

(use-package undo-tree
  :config (global-undo-tree-mode))

(use-package yaml-mode)
