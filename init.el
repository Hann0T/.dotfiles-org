(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" ."https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Init use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))

(setq inhibit-startup-message t)
(tooltip-mode -1)
(set-fringe-mode 10)

(add-to-list 'default-frame-alist '(alpha-background . 90))

;; is making everything slow (see with the profiler)
;;(use-package doom-modeline
;;  :ensure t
;;  :init (doom-modeline-mode 1)
;;  :custom ((doom-modeline-height 15)))

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers 'relative)

;; disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package doom-themes
  :init (load-theme 'doom-tokyo-night t))

(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 240)

(use-package dashboard
  :ensure t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :custom 
  (dashboard-modify-heading-icons '((recents . "file-text")
				      (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

(use-package org)
(setq org-ellipsis " ▾"
      org-startup-folded 'content
      org-cycle-separator-lines 2
      org-fontify-quote-and-verse-blocks t)

;; Indent org-mode buffers for readability
(add-hook 'org-mode-hook #'org-indent-mode)

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.config/emacs/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))
;; get tangled

(use-package org-tempo
  :ensure nil
  :demand t
  :config
  (dolist (item '(("sh" . "src sh")
                  ("el" . "src emacs-lisp")
                  ("li" . "src lisp")
                  ("sc" . "src scheme")
                  ("ts" . "src typescript")
                  ("py" . "src python")
                  ("yaml" . "src yaml")
                  ("json" . "src json")
                  ("einit" . "src emacs-lisp :tangle emacs/init.el")
                  ("emodule" . "src emacs-lisp :tangle emacs/modules/dw-MODULE.el")))
    (add-to-list 'org-structure-template-alist item)))

(add-hook 'org-mode-hook 'org-indent-mode)
  (use-package org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; (use-package org-bullets
;;   :after org
;;   :hook (org-mode . org-bullets-mode)
;;   :custom
;;   (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package general
  :config
  (general-create-definer hann0t/leader-keys
    :keymaps '(normal)
    :prefix "SPC"
    :global-prefix "SPC")

  (hann0t/leader-keys
    "SPC"  '(project-find-file :which-key "telescope")
    ","  '(counsel-ibuffer :which-key "list buffers")
    "."  '(counsel-find-file :which-key "find file")
    "rn"  '(lsp-rename :which-key "lsp rename")
    "rr"  '(lsp-find-references :which-key "lsp find references")
    "gg"  '(magit :which-key "magit")
    "sd"  '(project-find-regexp :which-key "project find regex")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  :bind (("C-h" . lsp-ui-doc-glance)
      ("C-SPC" . completion-at-point))
)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

;; lsp and tsserver was making the jit-lock-function pretty slow
;; https://www.reddit.com/r/emacs/comments/1bwe92d/comment/ky5jk31/
(setq font-lock-maximum-decoration 2)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (evil-set-undo-system 'undo-redo)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;;(use-package hydra)
;;
;;(defhydra hydra-text-scale (:timeout 4)
;;  "scale text"
;;  ("j" text-scale-increase "in")
;;  ("k" text-scale-decrease "out")
;;  ("f" nil "finished" :exit t))
;;
;;(hann0t/leader-keys
;;  "ts" '(hydra-text-scale/body :which-key "scale text"))

;;(use-package projectile
;;  :diminish projectile-mode
;;  :config (projectile-mode)
;;  :custom ((projectile-completion-system 'ivy))
;;  :bind-keymap
;;  ("C-c p" . projectile-command-map)
;;  :init
;;  (setq projectile-project-search-path '(("~/Personal" . 1) ("~/Work" . 1)))
;;  (setq projectile-switch-project-action #'projectile-dired))

;;(use-package counsel-projectile
;;  :config (counsel-projectile-mode))

(use-package project
    ;;:bind (
    ;;    ("C-f" . project-switch-project))
)
;; try to bind C-f to project-switch-project
;; try to create harpoon with project

(use-package magit
   :custom
   (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(delete-selection-mode 1)    ;; You can select text and delete it by typing.
(electric-indent-mode -1)    ;; Turn off the weird indenting that Emacs does by default.
(electric-pair-mode 1)       ;; Turns on automatic parens pairing
;; The following prevents <> from auto-pairing when electric-pair-mode is on.
;; Otherwise, org-tempo is broken when you try to <s TAB...
(add-hook 'org-mode-hook (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
(global-auto-revert-mode t)  ;; Automatically show changes if the file has changed
(global-display-line-numbers-mode 1) ;; Display line numbers
(global-visual-line-mode t)  ;; Enable truncated lines
(menu-bar-mode -1)           ;; Disable the menu bar 
(scroll-bar-mode -1)         ;; Disable the scroll bar
(tool-bar-mode -1)           ;; Disable the tool bar
(setq org-edit-src-content-indentation 0) ;; Set src block automatic indent to 0 instead of 2.
