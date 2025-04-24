;; Core settings
(setq ;; Yes, this is Emacs
      inhibit-startup-message t

      ;; Make it easy to cycle through previous items in the mark ring
      set-mark-command-repeat-pop t

      ;; Prevents start at text mode and load some other packages
      initial-major-mode 'fundamental-mode

      ;; Don't warn on large files
      large-file-warning-threshold nil

      ;; Follow symlinks to VC-controlled files without warning
      vc-follow-symlinks t

      ;; Don't warn on advice
      ad-redefinition-action 'accept

      ;; Revert Dired and other buffers
      global-auto-revert-non-file-buffers t

      ;; Disable GUI prompts
      use-dialog-box nil

      ;; Silence compiler warnings as they can be pretty disruptive
      native-comp-async-report-warnings-errors nil)

;; Core modes
(repeat-mode 1)                ;; Enable repeating key maps
(menu-bar-mode 0)              ;; Hide the menu bar
(tool-bar-mode 0)              ;; Hide the tool bar
(savehist-mode 1)              ;; Save minibuffer history
(scroll-bar-mode 0)            ;; Hide the scroll bar
(xterm-mouse-mode 1)           ;; Enable mouse events in terminal Emacs
(display-time-mode 1)          ;; Display time in mode line / tab bar
(fido-vertical-mode 1)         ;; Improved vertical minibuffer completions
(column-number-mode 1)         ;; Show column number on mode line
(tab-bar-history-mode 1)       ;; Remember previous tab window configurations
(auto-save-visited-mode 1)     ;; Auto-save files at an interval
(global-visual-line-mode 1)    ;; Visually wrap long lines in all buffers
(global-auto-revert-mode 1)    ;; Refresh buffers with changed local files

;; Tabs to spaces
(setq-default indent-tabs-mode nil tab-width 2)

(delete-selection-mode 1)    ;; You can select text and delete it by typing.
(electric-indent-mode -1)    ;; Turn off the weird indenting that Emacs does by default.

(electric-pair-mode 1)       ;; Turns on automatic parens pairing
;; The following prevents <> from auto-pairing when electric-pair-mode is on.
;; Otherwise, org-tempo is broken when you try to <s TAB...
(add-hook 'org-mode-hook (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(setq org-edit-src-content-indentation 0) ;; Set src block automatic indent to 0 instead of 2.

;; Display line numbers in programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq display-line-numbers-mode 'relative)

;; Make icomplete slightly more convenient
(keymap-set icomplete-fido-mode-map "M-h" 'icomplete-fido-backward-updir)
(keymap-set icomplete-fido-mode-map "TAB" 'icomplete-force-complete)

;; Delete trailing whitespace before saving buffers
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Match completion substrings that may be out of order
(defun dw/override-fido-completion-styles ()
  (setq-local completion-styles '(basic substring partial-completion emacs22)))
(add-hook 'icomplete-minibuffer-setup-hook 'dw/override-fido-completion-styles)

(setopt completions-detailed t
        completions-format 'vertical
        completion-auto-select t)

(setopt tab-always-indent 'complete
        completion-styles '(basic partial-completion substring flex)
        completion-ignore-case t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-flex-nospace t
        completion-show-help nil
        completions-detailed t
        completions-group t
        completion-auto-help 'visible
        completion-auto-select 'second-tab
        completions-header-format nil
        completions-format 'vertical  ;'one-column
        completions-max-height 10)

(keymap-set minibuffer-local-map "C-p" #'minibuffer-previous-completion)
(keymap-set minibuffer-local-map "C-n" #'minibuffer-next-completion)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" ."https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Init use-package on non-linux platforms
;;(unless (package-installed-p 'use-package)
;;  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; debug packages load time, defer, etc
;; use `use-package-report` to see the report
;;(setq use-package-compute-statistics t)
;;(setq use-package-minimum-reported-time 0.01)

(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))
;; dont backup files opened by sudo or doas
(setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not
              (let ((method (file-remote-p name 'method)))
                (when (stringp method)
                  (member method '("su" "sudo" "doas"))))))))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(tooltip-mode -1)
;; padding
(set-fringe-mode 10)

(add-to-list 'default-frame-alist '(alpha-background . 90))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))


;; defer the doom theme
;; is not that useful because it loads right after the startup and looks weird
;;(add-hook 'emacs-startup-hook
;;          (lambda () (load-theme 'doom-tokyo-night t)))

(use-package doom-themes
  :init (load-theme 'doom-tokyo-night t))

(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-20"))
(setq frame-inhibit-implied-resize t)

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

;; Clean up the mode line
(setq-default mode-line-format
              '("%e" "  "
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                mode-line-format-right-align
                "  "
                (project-mode-line project-mode-line-format)
                " "
                (vc-mode vc-mode)
                "  "
                ;;mode-line-modes
                mode-line-misc-info
                "  ")
              project-mode-line t
              mode-line-buffer-identification '(" %b")
              mode-line-position-column-line-format '(" %l:%c"))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :commands (org-mode))
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

(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
      (python . t))))

  (use-package org-tempo
    :ensure nil
    :after org
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



(setq org-clock-sound "~/Music/sfx/bell-notification.wav")

(use-package general
  :config
  (general-create-definer hann0t/leader-keys
    :keymaps '(normal)
    :prefix "SPC"
    :global-prefix "SPC")

  (hann0t/leader-keys
    "SPC"  '(project-find-file :which-key "telescope")
    ","  '(switch-to-buffer :which-key "switch to buffer")
    "b"  '(ibuffer :which-key "list buffers")
    "."  '(find-file :which-key "find file")
    "rn"  '(lsp-rename :which-key "lsp rename")
    "rr"  '(lsp-find-references :which-key "lsp find references")
    "gg"  '(magit :which-key "magit")
    "ps"  '(project-find-regexp :which-key "project find regex")
    "tt" '(org-timer-set-timer :which-key "set timer")))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  :bind (:map lsp-mode-map
      ("C-h" . lsp-ui-doc-glance)
      ("TAB" . completion-at-point)
      ("C-SPC" . completion-at-point))
)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

;; lsp and tsserver was making the jit-lock-function pretty slow
;; https://www.reddit.com/r/emacs/comments/1bwe92d/comment/ky5jk31/
(setq font-lock-maximum-decoration 2)

;; remove the auto shit|imports? the thing that mess up my imports in tsserver

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package php-mode
    :mode "\\.php\\'"
    :hook (php-mode . lsp-deferred)
)

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("b" . dired-up-directory))
  :config
  (setq dired-listing-switches "-alv --group-directories-first"
        ;;dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        dired-dwim-target 'dired-dwim-target-next
        dired-hide-details-hide-symlink-targets nil
        dired-kill-when-opening-new-dired-buffer t
        delete-by-moving-to-trash t))

;;(use-package evil
;;  :init
;;  (setq evil-want-integration t)
;;  (setq evil-want-keybinding nil)
;;  (setq evil-want-C-u-scroll t)
;;  (setq evil-want-C-i-jump nil)
;;  :config
;;  (evil-mode 1)
;;  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;;  (evil-set-undo-system 'undo-redo)

;;  ;; Use visual line motions even outside of visual-line-mode buffers
;;  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
;;  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;;  (evil-set-initial-state 'messages-buffer-mode 'normal)
;;  (evil-set-initial-state 'dashboard-mode 'normal))

;;(use-package evil-collection
;;  :after evil
;;  :config
;;  (evil-collection-init))

(use-package project
     :bind (
         ("C-c f" . project-switch-project))
)

;;(defun my/project-switched-advice (&rest _)
;;  (message "Switched to project: %s" (project-root (project-current t)))
;;  ;; Put your custom logic here
;;)

;; (advice-add 'project-switch-project :after #'my/project-switched-advice)

;;(defun my/project-switched-advice (&rest _)
  ;;(message "Switched to project: %s" (project-root (project-current t)))
  ;;;; Put your custom logic here
;;)

;; (advice-add 'project-switch-project :after #'my/project-switched-advice)

(use-package magit
  :bind (("C-c g" . magit))
  :commands (magit)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)
  (meow-leader-define-key
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-motion-define-key
   ;; custom keybinding for motion state
   '("<escape>" . ignore))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-line)
   '("E" . meow-goto-line)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-till)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-search)
   '("t" . meow-right)
   '("T" . meow-right-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-save)
   '("X" . meow-sync-grab)
   '("y" . meow-yank)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

 (use-package meow
   :demand t)
 (meow-setup)
 (meow-global-mode 1)

(defface siren-tab-bar-tab
`((t :inherit 'tab-bar-tab
        :foreground ,(face-attribute 'font-lock-keyword-face :foreground nil t)))
"Face for active tab in tab-bar."
:group 'siren-tab-bar)

(defface siren-tab-bar-tab-hint
`((t :inherit 'siren-tab-bar-tab
        :foreground ,(face-attribute 'tab-bar-tab-inactive :foreground nil t)))
"Face for active tab hint in tab-bar."
:group 'siren-tab-bar)

(defface siren-tab-bar-tab-inactive
`((t :inherit 'tab-bar-tab-inactive
        :foreground ,(face-attribute 'font-lock-comment-face :foreground nil t)))
"Face for inactive tab in tab-bar."
:group 'siren-tab-bar)

(defface siren-tab-bar-tab-hint-inactive
`((t :inherit 'siren-tab-bar-tab-inactive
        :foreground ,(face-attribute 'tab-bar-tab-inactive :foreground nil t)))
"Face for inactive tab hint in tab-bar."
:group 'siren-tab-bar)

(defun siren-tab-bar-tab-name-format-default (tab i)
(let* ((current-p (eq (car tab) 'current-tab))
        (tab-face (if current-p
                        'siren-tab-bar-tab
                    'siren-tab-bar-tab-inactive))
        (hint-face (if current-p
                        'siren-tab-bar-tab-hint
                    'siren-tab-bar-tab-hint-inactive)))
    (concat (propertize (if tab-bar-tab-hints (format "  %d:" (- i 1)) "  ")
                        'face hint-face)
            (propertize
            (concat
            (alist-get 'name tab)
            (or (and tab-bar-close-button-show
                        (not (eq tab-bar-close-button-show
                                (if current-p 'non-selected 'selected)))
                        tab-bar-close-button)
                "")
            "  ")
            'face tab-face))))

(use-package tab-bar
  :custom
    (tab-bar-close-button-show nil)
    (tab-bar-new-button-show nil)
    (tab-bar-history-limit 25)
    (tab-bar-new-tab-choice "*scratch*")
    (tab-bar-show 1)
    (tab-bar-tab-hints t)
    (tab-bar-format `(tab-bar-format-tabs-groups
                    ,(if (eq system-type 'darwin)
                         'tab-bar-notch-spacer
                       'tab-bar-separator)))
    (tab-bar-tab-name-format-function #'siren-tab-bar-tab-name-format-default)
  :bind (
    ("C-c c" . tab-bar-new-tab)
    ("C-c x" . tab-bar-close-tab)
    ("C-c n" . tab-bar-switch-to-next-tab)
    ("C-c p" . tab-bar-switch-to-prev-tab)
  ))

;; Make sure ripgrep is used everywhere
(setq xref-search-program 'ripgrep
      grep-command "rg -nS --noheading")

(use-package vterm
  :commands (vterm)
  :ensure t)

(defun efs/display-startup-time ()
   (message "Emacs loaded in %s with %d garbage collections and %d features loaded."
            (format "%.2f seconds"
                    (float-time
                      (time-subtract after-init-time before-init-time)))
            gcs-done (length features)))

;;(add-hook 'emacs-startup-hook #'efs/display-startup-time)
