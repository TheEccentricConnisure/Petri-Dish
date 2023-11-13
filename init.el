;; -*- lexical-binding: t; -*-

  ;; The loading of the Vibraniums
        (add-to-list 'load-path "~/.config/emacs/vibraniums/")
        ;; The Elpaca Package Manager
        (require 'elpaca-setup)

(let ((file-name-handler-alist nil)) "~/.config/emacs/init.el")

;; Automatically tangle our config.org config file when we save it
(defun dr/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.config/emacs/README.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'dr/org-babel-tangle-config)))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (R . t)))
(push '("conf-unix" . conf-unix) org-src-lang-modes)

(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

(setq backup-directory-alist '((".*" . "~/.config/emacs/.trash")))

(setq user-full-name "Ivan Pereira"
      user-mail-address "ivan.pereira@mailfence.com")

;; Disable line numbers for some modes
 (dolist (mode '(
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set the default font
    (set-face-attribute 'default nil
		     :font "Input Sans"
		     :height 180
		     :weight 'medium)
   (set-face-attribute 'variable-pitch nil
    :font "Input Sans"
    :height 180
    :weight 'medium)
    (set-face-attribute 'fixed-pitch nil
    :font "Input Mono"
    :height 180
    :weight 'medium)
  ;; Makes commented text and keywords italics.
  ;; This is working in emacsclient but not emacs.
  ;; Your font must have an italic face available.
  (set-face-attribute 'font-lock-comment-face nil
    :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil
    :slant 'italic)

  ;; This sets the default font on all graphical frames created after restarting Emacs.
  ;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
  ;; are not right unless I also add this method of setting the default font.
  ;;(add-to-list 'default-frame-alist '(font . "Input Mono-11"))

  ;; Uncomment the following line if line spacing needs adjusting.
;  (setq-default line-spacing 0.12)

    ;; Enable line numbers
    (global-display-line-numbers-mode t)

  (use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package dashboard
          :config
          (dashboard-setup-startup-hook)
        (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))))
      ;; Set the title
      (setq dashboard-banner-logo-title "Welcome to Forgers Board")
      ;; Set the banner
;;    (setq dashboard-startup-banner 'logo)
      (setq dashboard-startup-banner "/home/Dr.Eccentric/Pictures/DP/CosmoDoc-modified.png")
      ;; Value can be
      ;; - nil to display no banner
      ;; - 'official which displays the official emacs logo
      ;; - 'logo which displays an alternative emacs logo
      ;; - 1, 2 or 3 which displays one of the text banners
      ;; - "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer
      ;; - a cons of '("path/to/your/image.png" . "path/to/your/text.txt")

      ;; Content is not centered by default. To center, set
      (setq dashboard-center-content t)

      ;; To disable shortcut "jump" indicators for each section, set
      (setq dashboard-show-shortcuts t)
  (setq dashboard-display-icons-p t) ;; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package

(use-package ef-themes
  :demand t
  :config
   (load-theme 'ef-elea-dark :no-confirm))

(use-package doom-modeline
   :demand t
:init (doom-modeline-mode 1)
 :custom ((doom-modeline-height 30)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
    :init (which-key-mode)
    :diminish which-key-mode
    :config
    (setq which-key-idle-delay 1))

(use-package smartparens-config
      :elpaca (smartpares-config :host github :repo "Fuco1/smartparens")
      :config
     (smartparens-global-mode t)
     ;; Customize smartparens behavior for ~
(sp-pair "~" "~" :trigger "~"))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package evil
  :ensure t
  :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (evil-mode 1))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init))

(use-package evil-tutor)

(use-package restart-emacs
        :demand t
            :elpaca (restart-emacs :host github :repo "iqbalansari/restart-emacs")
        :config
        (setq restart-emacs-restore-frame t)
        :bind
("C-c r r" . restart-emacs))

(defun my-reload-emacs ()
    "Reload Emacs by re-evaluating the init file."
    (interactive)
    (load-file user-init-file))

(global-set-key (kbd "C-c s") 'my-reload-emacs)

(global-set-key (kbd "C-c f e") 'open-my-config)

(defun open-my-config ()
  "Open README.org ."
  (interactive)
  (find-file "~/.config/emacs/README.org"))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package dired-open
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package neotree
        :ensure t
        :config
        (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
)

(use-package general
   :config
   (general-evil-setup)
    (general-create-definer leader-key
        :states '(normal insert visual emacs)
        :keymaps 'override
        :prefix "SPC" ;; set leader
        :global-prefix "C-SPC");; access leader in insert mode
(require 'hashmap)
 )



(use-package vertico
:custom
(vertico-cycle t)
  :config
(vertico-mode 1))

(use-package ivy
  :ensure t)
 ; :config
 ; (ivy-mode 1))

(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :config
  (all-the-icons-ivy-rich-mode 1))

(use-package marginalia
:custom
(marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
:init
(marginalia-mode))

(use-package orderless
:init
;; Tune the global completion style settings to your liking!
;; This affects the minibuffer and non-lsp completion at point.
(setq completion-styles '(orderless partial-completion basic)
      completion-category-defaults nil
      completion-category-overrides nil))

(use-package counsel
  :bind (
        ;("M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("M-r" . 'counsel-minibuffer-history)))

(setq savehist-mode t)

(use-package company
  :config
  (global-company-mode))

(use-package fuzzy)

(use-package langtool
  :elpaca (langtool :host github :repo "mhayashi1120/Emacs-langtool")
  :init
  (setq langtool-language-tool-jar "~/.config/emacs/LanguageTool/languagetool-commandline.jar")
  (setq langtool-default-language "en-GB"))

(use-package ivy-bibtex
    :ensure t
    :config
    (setq bibtex-completion-bibliography '("~/Documents/Articles/bibliotext/dummy.bib")) ; Add the path to your .bib file
    (setq bibtex-completion-library-path '("~/Documents/Articles/Medicine") ) ; Add the path to your PDFs or attach files
    (setq bibtex-completion-notes-path "~/Documents/wORG/Org-ROAM/Alexandria/") ; Add the path to your notes directory

    ;; Use Ivy for BibTeX selection
    (setq bibtex-completion-cite-prompt-for-optional-arguments nil)
    (ivy-bibtex-ivify-action ivy-bibtex-open-external ivy-bibtex)
    (setq bibtex-completion-additional-search-fields '(keywords))
    (setq bibtex-completion-notes-template-multiple-files
          (concat
          "#+TITLE: ${title}\n"
          "#+ROAM_KEY: cite:${=key=}\n"))
    )

  ;; Set a keybinding for Ivy BibTeX
;  (global-set-key (kbd "C-c C-b") 'ivy-bibtex)

(use-package citar
  :custom
  (org-cite-global-bibliography '("~/bib/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))
  
    (use-package citar-org-roam
    :after (citar org-roam)
    :config (citar-org-roam-mode))

(use-package org-ref
  :ensure t
  :config
  ;; Customize your Org-Ref settings here
  )

(require 'organizer)

(use-package ess
    :init (require 'ess-site)  ;; I don't know how else to get this working...
    :commands R
    :config
    (setq ess-default-style 'RStudio-))

(setq org-startup-indented t
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-startup-with-inline-images t)

(use-package org-sticky-header
    :elpaca (org-sticky-header :host github :repo "alphapapa/org-sticky-header")
    :after (org)
  :ensure t
  :after org
  :hook (org-mode . org-sticky-header-mode)
  :config
  (setq org-sticky-header-full-path 'full)
  (setq org-sticky-header-outline-path-separator " ❱ ")
  (setq org-sticky-header-face-list
        '((header-line . (:inherit mode-line :background "gray90" :foreground "black" :box nil)))))

(use-package org-remark
  :bind (;; :bind keyword also implicitly defers org-remark itself.
         ;; Keybindings before :map is set for global-map.
         ("C-c n m" . org-remark-mark)
         ("C-c n l" . org-remark-mark-line)
         :map org-remark-mode-map
         ("C-c n o" . org-remark-open)
         ("C-c n ]" . org-remark-view-next)
         ("C-c n [" . org-remark-view-prev)
         ("C-c n r" . org-remark-remove)
         ("C-c n d" . org-remark-delete))
  ;; Alternative way to enable `org-remark-global-tracking-mode' in
  ;; `after-init-hook'.
  ;; :hook (after-init . org-remark-global-tracking-mode)
  :init
  ;; It is recommended that `org-remark-global-tracking-mode' be
  ;; enabled when Emacs initializes. Alternatively, you can put it to
  ;; `after-init-hook' as in the comment above
  (org-remark-global-tracking-mode +1)
  :config
  (use-package org-remark-info :after info :config (org-remark-info-mode +1))
  (use-package org-remark-eww  :after eww  :config (org-remark-eww-mode +1))
  (use-package org-remark-nov  :after nov  :config (org-remark-nov-mode +1)))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(define-abbrev global-abbrev-table "<m" "* Thyroid\n** Embryology\n** Anatomy** Physiology Functions\n** Pathology** Clinical Parameters to look out for\n** Pharmacology")
(setq-default abbrev-mode t)

(setq org-directory "~/Documents/wORG/Colloquy")
(setq org-journal-dir "~/Documents/wORG/MyPersonal/My-Microsome")
(setq org-roam-directory "~/Documents/wORG/Org-ROAM/Alexandria")

(use-package org-superstar
  :config
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))
(setq
	   ;; Edit settings
	   org-auto-align-tags nil
	   org-tags-column 0
	   org-catch-invisible-edits 'show-and-error
	   org-special-ctrl-a/e t
	   org-insert-heading-respect-content t

	   ;; Org styling, hide markup etc.
	   org-hide-emphasis-markers t
	   org-pretty-entities t
	   org-ellipsis "…"

	   ;; Agenda styling
	   org-agenda-tags-column 0
	   org-agenda-block-separator ?─
	   org-agenda-time-grid
	   '((daily today require-timed)
	     (800 1000 1200 1400 1600 1800 2000)
	     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
	   org-agenda-current-time-string
	   "⭠ now ─────────────────────────────────────────────────")

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package olivetti
        :ensure t
        :hook
        (org-mode . olivetti-mode)
        :config
        (olivetti-set-width 160)
)

(use-package pdf-tools)

(use-package org-present
  :ensure t
  :config
  (setq org-present-text-scale 3
        org-present-mode-hook
        (lambda ()
          (org-present-big)
          (org-display-inline-images)
          (org-present-hide-cursor)
          (org-present-read-only))))

(setq  org-journal-date-prefix "#+TITLE:"
       org-journal-time-prefix "*  "
       org-journal-date-format "%A, %F"
       org-journal-file-format "%F.org")

;; Org-roam Template ---------------------------------------------------------------------------------------------------
;;       (setq org-roam-capture-templates
;;             `(("d" "default" plain "%?"
;;                 :target (file+head "${slug}.org" "#+title: ${title}")
;;                 :unnarrowed t)))
(setq org-roam-capture-templates
      `(("d" "Default" plain "%?"
         :target (file+head "${slug}.org" "#+title: ${title}\n#+created: %u\n#+last_modified: %U\n#+ROAM_TAGS: ${tag}")
         :unnarrowed t)
        ("r" "Roam Note" plain "%?"
         :target (file+head "${slug}.org" "#+title: ${title}\n#+created: %u\n#+last_modified: %U\n#+ROAM_TAGS: ${tag}\n\n* Thing that I have understood\n\n* Thing that I have 50-50% Confidence\n\n* Thing that I blew past my head and need to review\n\n* Research Article\n\n* Devil's Advocate Corner\n\n")
         :unnarrowed t)))

(use-package org-roam
          :elpaca (org-roam :host github :repo "org-roam/org-roam"
                     :files (:defaults "extensions/*") )
          :init
        (setq org-roam-v2-ack t)
       (org-roam-db-autosync-mode)
       (require 'org-roam-protocol)
       :config
(setq org-id-location "~/Documents/wORG/Org-ROAM/Alexandria" )
      (setq org-fold-catch-invisible-edits t)
)

; (setq org-roam-db-location "~/Documents/wORG/Org-ROAM/Alexandria")

(use-package org-roam-ui
:after (org-roam)
    :elpaca
    (roam-ui :host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
            org-roam-ui-follow t
            org-roam-ui-update-on-save t
            org-roam-ui-open-on-start t))

(setq org-roam-db-node-include-function
      (lambda ()
        (not (member "ATTACH" (org-get-tags)))))

(setq org-todo-keywords '((sequence "IDEA(i)" "PLAN(p)" "|" "TODO(t)" "In-Progress(r)" "|" "DONE(d)" "CANCELLED(c)" "|" "DEFERRED(f)")))

;; TODO: org-todo-keyword-faces
(setq org-todo-keyword-faces
      '(("IDEA" . (:foreground "DeepSkyBlue2" :weight bold))
        ("PLAN" . (:foreground "orange red" :weight bold))
        ("TODO" . (:foreground "HotPink2" :weight bold))
        ("In-Progress" . (:foreground "MediumPurple3" :weight bold))
        ("DONE" . (:foreground "LimeGreen" :weight bold))
        ("CANCELLED" . (:foreground "red3" :weight bold))
        ("DEFERRED" . (:foreground "DarkOrange2" :weight bold))))

(use-package org-recur
  :demand t)

;; Customize the variable org-refile-targets to specify the refile targets.
;; The example below sets it to refile headlines in the current buffer,
;; as well as in the "~/path/to/destination.org" file.
(setq org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :maxlevel . 3)
                           ("~/Documents/wORG/My-Personal/Transmogrify/Niflheim.org" :maxlevel . 3)))

;; Optionally, set org-refile-use-outline-path to t to show the full outline path in the completion.
(setq org-refile-use-outline-path t)

;; Optionally, set org-outline-path-complete-in-steps to t for better completion.
(setq org-outline-path-complete-in-steps t)

;; Optionally, set org-refile-allow-creating-parent-nodes to t to allow creating non-existing parent nodes.
(setq org-refile-allow-creating-parent-nodes 'confirm)

(use-package origami
    :ensure t
    :config
    (global-origami-mode))

(use-package org-heatmap
    :elpaca (org-heatmap :host github :repo "Elilif/org-heatmap")
  :config
(add-hook 'org-mode-hook #'org-heatmap-mode))

(setq org-highest-priority ?A       ; Highest priority is 'A'
      org-lowest-priority ?D        ; Lowest priority is 'D'
      org-default-priority ?C)      ; Default priority is 'C'

(setq org-priority-faces
      '((?A . (:foreground "red" :weight bold :height 1.2))    ; Highest priority: ❗
        (?B . (:foreground "orange" :weight bold :height 1.2)) ; Priority 'B': ⬆
        (?C . (:foreground "yellow" :weight bold :height 1.2)) ; Priority 'C': ⬇
        (?D . (:foreground "green" :weight bold :height 1.2))) ; Lowest priority: ☕
        )

(use-package org-fancy-priorities
     :ensure t
     :hook (org-mode . org-fancy-priorities-mode))

(setq org-fancy-priorities-list '((?A . "❗")
					(?B . "⬆")
					(?C . "⬇")
					(?D . "☕")))

(setq org-caldav-url "https://cloud.disroot.org/remote.php/dav/calendars/xanaus")
(setq org-caldav-calendar-id "transmogrify")
(setq org-caldav-inbox "~/Documents/wORG/My-Personal/Transmogrify/Sync.org")
(setq org-caldav-backup-file "~/Documents/My-Personal/Transmogrify/Niflheiem.org")
(setq org-icalendar-timezone "IST")
(setq org-caldav-files '("~/Documents/wORG/My-Personal/Transmogrify/Hammurabi's-Code.org""~/Documents/wORG/My-Personal/Transmogrify/Stone-Tablet.org"))
(setq org-icalendar-include-todo 'all
    org-caldav-sync-todo t)

(use-package org-journal
  :config
  (setq org-journal-dir "~/Documents/wORG/My-Personal/My-Microsome"))

(use-package elfeed
         :ensure t)

 (use-package elfeed-score
   :ensure t
   :config
   (elfeed-score-enable))
; (require 'zotearo)

(use-package eat
:elpaca (eat 
      :host codeberg
      :repo "akib/emacs-eat"
      :files ("*.el" ("term" "term/*.el") "*.texi"
              "*.ti" ("terminfo/e" "terminfo/e/*")
              ("terminfo/65" "terminfo/65/*")
              ("integration" "integration/*")
              (:exclude ".dir-locals.el" "*-tests.el"))))

(setq custom-file "~/.config/emacs/custom.el")
  (when (file-exists-p custom-file)
    (load custom-file))
;; Local Variables:
;; no-byte-compile: t
;; End:
