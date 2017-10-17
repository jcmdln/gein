;; /etc/emacs/default.el

;; Encoding
(setq prefer-coding-system 'utf-8
      set-default-coding-systems 'utf-8
      set-language-environment "UTF-8"
      set-locale-environment "en_US.UTF-8")

;; Disable menubar, scrollbar, and toolbar before they initialize
(when (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))

;; Startup
(setq initial-scratch-message nil
      inhibit-splash-screen t
      inhibit-startup-buffer-menu t
      column-number-mode t
      visible-bell t)

;; Set theme to 'tango-dark'
(load-theme 'tango-dark)

;; Set font as 9pt normal monospace
(set-face-attribute
 'default nil
 :family "Monospace" :weight 'normal
 :width 'normal      :height 96)

;; Startup Hooks
(add-hook 'lisp-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'text-mode-hook 'linum-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Autosave/Backups
(setq auto-save-default nil
      auto-save-file-name-transforms `((".*" "~/.emacs.d/backup/" t))
      backup-directory-alist `((".*" . "~/.emacs.d/backup/"))
      create-lockfiles nil
      delete-by-moving-to-trash t
      delete-old-versions t
      delete-selection-mode t
      kept-new-versions 2
      vc-follow-symlinks t
      vc-make-backup-files t
      version-control t)

;; Cut/Paste
(setq require-final-newline t
      save-interprogram-paste-before-kill t
      select-enable-primary nil)

;; Indentation
(setq c-basic-offset 2
      cperl-indent-level 2
      js-indent-level 2
      indent-tabs-mode nil
      tab-width 2)

;; Mouse
(setq mouse-wheel-follow-mouse 't
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(3 ((shift) . 3))
      mouse-yank-at-point t)

;; Scrolling
(setq auto-window-vscroll nil
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

;; Enable Emacs mouse commands
(xterm-mouse-mode t)

;; Autoload custom file on start
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file)
  (write-region "" nil custom-file))


;;; Input ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c c")           'comment-or-uncomment-region)
(global-set-key (kbd "C-x C-b")         'ibuffer)
(global-set-key (kbd "C-x x")           'kill-buffer-and-window)
(global-set-key (kbd "<C-tab>")         'next-buffer)
(global-set-key (kbd "<C-iso-lefttab>") 'previous-buffer)
(global-set-key (kbd "<M-down>")        'windmove-down)
(global-set-key (kbd "<M-left>")        'windmove-left)
(global-set-key (kbd "<M-right>")       'windmove-right)
(global-set-key (kbd "<M-up>")          'windmove-up)
(global-set-key (kbd "C-c <down>")      'windmove-down)
(global-set-key (kbd "C-c <left>")      'windmove-left)
(global-set-key (kbd "C-c <right>")     'windmove-right)
(global-set-key (kbd "C-c <up>")        'windmove-up)

(global-set-key (kbd "M--")
                (lambda() (interactive)
                  (split-window-vertically)
                  (other-window 1 nil)
                  (switch-to-next-buffer)))

(global-set-key (kbd "M-=")
                (lambda() (interactive)
                  (split-window-horizontally)
                  (other-window 1 nil)
                  (switch-to-next-buffer)))

(global-set-key (kbd "<mouse-4>") (lambda() (interactive)(scroll-down 4)))
(global-set-key (kbd "<mouse-5>") (lambda() (interactive)(scroll-up 4)))


;;; Package Manager ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Install 'use-package' if not present
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)


;;; Built-in Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package async
  :config (async-bytecomp-package-mode  '(all)))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package auto-dictionary)

(use-package elfeed
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-search-filter "@1-week-ago +unread "
        url-queue-timeout 30)
  (if (file-exists-p "~/.emacs.d/elfeed.el")
      (load-file "~/.emacs.d/elfeed.el")))

(use-package eshell
  :config
  (setq eshell-cmpl-cycle-completions nil
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-history-size 4096
        eshell-prefer-lisp-functions t
        eshell-save-history-on-exit t
        eshell-scroll-to-bottom-on-input 'all)

  (setq eshell-prompt-function
        (lambda nil
          (concat
           "[" (user-login-name) "@" (system-name) " "
           (if (= (length (eshell/pwd)) (length (getenv "HOME")))
               "~" (eshell/basename (eshell/pwd))) "]"
               (if (= (user-uid) 0) "# " "$ ")))
        eshell-prompt-regexp "^[^#$\n]*[#$] ")

  (defun append-to-list (list-var elements)       "Append ELEMENTS to the end of LIST-VAR"
         (set list-var (append (symbol-value list-var) elements)))

  (add-hook
   'eshell-mode-hook
   (lambda()
     (add-hook 'eshell-mode-hook 'eshell-exports)
     (append-to-list 'eshell-visual-commands
                     '("alsamixer" "htop" "top" "nano" "vi" "vim" "less" "ssh"
                       "tail" "watch"))

     (eshell/alias "clear" "eshell-clear")
     (eshell/alias "cp" "cp -ip $1 $2")
     (eshell/alias "cr" "cp -ipr $1 $2")
     (eshell/alias "hurl" "curl --fail --location --remote-name-all --progress-bar $*")
     (eshell/alias "df" "df -h $*")
     (eshell/alias "di" "df -hi $*")
     (eshell/alias "free" "free -h $*")
     (eshell/alias "l" "ls -hSC --color=auto --group-directories-first $*")
     (eshell/alias "la" "ls -ahSC --color=auto --group-directories-first $*")
     (eshell/alias "ll" "ls -halS --color=auto --group-directories-first $*")
     (eshell/alias "ls" "ls -hSC --color=auto --group-directories-first $*")
     (eshell/alias "mkcd" "mkdir -vp $1; cd $1")
     (eshell/alias "mkdir" "mkdir -vp $*")
     (eshell/alias "rf" "rm -rf $*")
     (eshell/alias "rm" "rm -i $*")
     (eshell/alias "rr" "rm -ir $*")))

  (defun eshell-clear()
    "Clear the eshell buffer"
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (defun eshell-exports()
    "Store desired environment variables for eshell."
    (setenv "EDITOR"         "emacsclient")
    (setenv "GIT_EDITOR"     "emacsclient")
    (setenv "MANPATH"        (getenv "MANPATH"))
    (setenv "PATH"           (getenv "PATH"))
    (setenv "PROMPT_COMMAND" "")
    (setenv "SHELL"          (getenv "SHELL"))
    (setenv "TERM"           (getenv "TERM")))

  (defun eshell-new()
    "Open a new instance of eshell."
    (interactive)
    (eshell 'N)))

(use-package eww
  :config
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
      (eww url)))

  (defun eww-open-url-in-new()
    "Open link under cursor in new instance of eww"
    (interactive)
    (let ((url (get-text-property (point) 'shr-url)))
      (print url)
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
  :config
  (add-hook 'ibuffer-hook
            (lambda()
              (ibuffer-do-sort-by-alphabetic)
              (ibuffer-do-sort-by-major-mode))))

(use-package scratch
  :config
  (defun scratch-new()
    "Open a new scratch buffer."
    (interactive)
    (switch-to-buffer (generate-new-buffer "*scratch*"))
    (lisp-mode)))

(use-package server
  :config
  (unless (server-running-p)
    (server-start))

  (defun server-reinstall()
    "Remove packages, then run server-reload"
    (interactive)
    (if (file-exists-p "~/.emacs.d/pkg")
        (delete-directory "~/.emacs.d/pkg" t))
    (server-reload))

  (defun server-kill()
    "Delete current Emacs server, then kill Emacs"
    (interactive)
    (server-force-delete)
    (kill-emacs))

  (defun server-reload()
    "Reload init file"
    (interactive)
    (if (file-exists-p "~/.emacs.d/init.el")
        (load-file "~/.emacs.d/init.el")
      (if (file-exists-p "~/.emacs")
          (load-file "~/.emacs")))
    (eshell-exports))

  (defun server-stop()
    "Prompt to save buffers, then kill Emacs."
    (interactive)
    (save-buffers-kill-emacs))

  (defun server-update()
    "Refresh package contents, then update all packages."
    (interactive)
    (package-initialize)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-utils-upgrade-all))

  (global-set-key (kbd "C-x C-c")
                  (lambda() (interactive)
                    (if (y-or-n-p "Quit Emacs? ")
                        (server-stop)))))

(use-package xclip
  :config (xclip-mode 1))


;;; Extras ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package circe
  :config
  (add-hook 'circe-chat-mode-hook 'my-circe-prompt)
  (add-hook 'circe-message-option-functions 'my-circe-message-option-chanserv)
  (add-hook 'lui-mode-hook
            (lambda()
              (my-lui-setup)
              (my-circe-set-margin)))

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
              'face 'circe-prompt-face)
             " ")))

  (defun my-circe-message-option-chanserv (nick user host command args)
    (when (and (string= "ChanServ" nick) (string-match "^\\[#.+?\\]" (cadr args)))
      '((dont-display . t)))))

(use-package company
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'text-mode-hook 'company-mode))

(use-package counsel
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f"  . counsel-describe-function)
         ("<f1> v"  . counsel-describe-variable)
         ("<f1> l"  . counsel-find-library)
         ("<f2> i"  . counsel-info-lookup-symbol)
         ("<f2> u"  . counsel-unicode-char)
         ("C-c g"   . counsel-git)
         ("C-c j"   . counsel-git-grep)
         ("C-c l"   . counsel-ag)
         ("C-x l"   . counsel-locate)
         ("C-S-o"   . counsel-rhythmbox))
  :config (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

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

(use-package flymake)

(use-package flyspell
  :config
  (add-hook 'flyspell-mode-hook (auto-dictionary-mode 1))
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-mode)
  (add-hook 'text-mode-hook 'flyspell-mode))

(use-package go-mode
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook
            (lambda()
              (setq tab-width 4
                    indent-tabs-mode 1))))

(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character))

(use-package ivy
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t))

(use-package magit)

(use-package nov
  :config (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package ranger)

(use-package rainbow-delimiters
  :config
  (add-hook 'markdown-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook     'rainbow-delimiters-mode)
  (add-hook 'text-mode-hook     'rainbow-delimiters-mode))

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

(use-package undo-tree
  :config (global-undo-tree-mode))
