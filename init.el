;; -*- lexical-binding: t; -*-

; The loading of the Vibraniums
        (add-to-list 'load-path "~/.config/emacs/vibraniums/" "~/.config/emacs/vibraniums/spacemacs_module_for_doom/")
        ;; The Elpaca Package Manager
        (require 'elpaca-setup)

(let ((file-name-handler-alist nil)) "~/.config/emacs/init.el")

;; Automatically tangle our config.org config file when we save it
(defun dr/org-babel-tangle-config ()
    (when (string-equal (buffer-file-name)
                        (expand-file-name "~/.config/emacs/README.org"))
      ;; Dynamic scoping to the rescue
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle)
              (load-file "~/.config/emacs/init.el")
        )))

 (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'dr/org-babel-tangle-config nil 'make-it-local)))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (R . t)
    (scheme . t)
    ))
(push '("conf-unix" . conf-unix) org-src-lang-modes)

(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

(setq backup-directory-alist '((".*" . "~/.config/emacs/.trash")))

;; Disable line numbers for some modes
 (dolist (mode '(
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq default-frame-alist '((undecorated . t)))
(add-to-list 'default-frame-alist '(internal-border-width . 5))
(add-to-list 'default-frame-alist '(drag-internal-border . 1))

(use-package fontaine)
 (elpaca-wait)
   (setq fontaine-latest-state-file
       (locate-user-emacs-file "fontaine-latest-state.eld"))
   (setq fontaine-presets
       '((tiny
          :default-family "Liberation Mono"
          :default-height 130)
         (small
          :default-family "Liberation Mono"
          :default-height 150)
         (regular
          :default-height 160)
         (medium
          :default-height 170)
         (large
          ;:default-weight semilight
          :default-height 190
          :bold-weight extrabold)
         (presentation
          ;:default-weight semilight
          :default-height 190
          :bold-weight extrabold)
         (jumbo
          ;:default-weight semilight
          :default-height 240
          :bold-weight extrabold)
         (t
          ;; I keep all properties for didactic purposes, but most can be
          ;; omitted.  See the fontaine manual for the technicalities:
          ;; <https://protesilaos.com/emacs/fontaine>.
:default-family "Liberation Mono"
          :default-weight regular
          :default-height 160
          :fixed-pitch-family nil ; falls back to :default-family
          :fixed-pitch-weight nil ; falls back to :default-weight
          :fixed-pitch-height 1.0
          :fixed-pitch-serif-family nil ; falls back to :default-family
          :fixed-pitch-serif-weight nil ; falls back to :default-weight
          :fixed-pitch-serif-height 1.0
          :variable-pitch-family "Liberation Monos"
          :variable-pitch-weight nil
          :variable-pitch-height 1.0
          :bold-family nil ; use whatever the underlying face has
          :bold-weight bold
          :italic-family nil
          :italic-slant italic
          :line-spacing nil)))

 ;; Recover last preset or fall back to desired style from
 ;; `fontaine-presets'.
 (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

 ;; The other side of `fontaine-restore-latest-preset'.
 (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

 ;; fontaine does not define any key bindings.  This is just a sample that
 ;; respects the key binding conventions.  Evaluate:
 ;;

;; Enable line numbers
(global-display-line-numbers-mode t)

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package nerd-icons)

(use-package dashboard
            :config
            (dashboard-setup-startup-hook)
          (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))))
        ;; Set the title
        (setq dashboard-banner-logo-title "Welcome to Forgers Board")
        ;; Set the banner
  ;;    (setq dashboard-startup-banner 'logo)
        (setq dashboard-startup-banner "~/Pictures/DP/CosmoDoc-modified.png")
        ;; Value can b
        ;; - nil to display no banner
        ;; - 'official which displays the official emacs logo
        ;; - 'logo which displays an alternative emacs logo
        ;; - 1, 2 or 3 which displays one of the text banners
        ;; - "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer
        ;; - cons of '("path/to/your/image.png" . "path/to/your/text.txt")

        ;; Content is not centered by default. To center, set
        (setq dashboard-center-content t)
(add-hook 'dashboard-setup-startup-hook (lambda () (display-line-numbers-mode 0)))
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

(use-package bufler
  :elpaca (bufler :fetcher github :repo "alphapapa/bufler.el"
                  :files (:defaults (:exclude "helm-bufler.el"))))

(use-package smartparens
      :elpaca (smartparens :host github :repo "Fuco1/smartparens")
      :config
     (smartparens-global-mode t) )
     ;; Customize smartparens behavior for ~
;(sp-pair "~" "~" :trigger "~"))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package meow
     :demand
     :config
(require 'meow-qwerty)
(meow-setup)
(meow-global-mode 1)
(setq meow-expand-exclude-mode-list (remove 'org meow-expand-exclude-mode-list))
(setq meow-use-clipboard t)
)

(global-set-key (kbd "C-c c e") 'open-my-config)
    (defun open-my-config ()
      "Open README.org ."
      (interactive)
      (find-file "~/.config/emacs/README.org"))
(global-set-key (kbd "C-x b") 'bufler-switch-buffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "C-c b") 'org-switchb) ;this key-binding is used to solely switch between the org buffers
(global-set-key (kbd "C-c o c i") 'org-clock-in)
(global-set-key (kbd "C-c o c o") 'org-clock-out)
(global-set-key (kbd "C-c o n") 'my-create-org-file)

;; Function to create a new org file
(defun my-create-org-file ()
  "Create a new org file with a prompt for the file name."
  (interactive)
  (let ((org-file-name (read-file-name "Enter org file name: ")))
    (find-file (concat org-file-name ".org"))
    (insert "#+TITLE: " (file-name-base org-file-name) "\n\n")
    (org-mode)))

;; You can add more custom keybindings or configurations below if needed.

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c o a") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

(setq org-capture-templates
      '(("a" "Appointment" entry (file+headline "~/Documents/wORG/My-Personal/Transmogrify/appointment.org" "Tasks")
         "* PLAN %?\nSCHEDULED: %^T\n")
        ("b" "Birthday" entry (file+headline "~/Documents/wORG/My-Personal/Transmogrify/birthdays.org" "Birthdays")
         "* PLAN %?\nSCHEDULED: %^T\n")
        ("g" "GTD Task" entry (file+headline "~/Documents/wORG/My-Personal/Transmogrify/GTD.org" "Tasks")
         "* PLAN %?\nSCHEDULED: %^T\n")
        ("p" "Project" entry (file+headline "~/Documents/wORG/My-Personal/Transmogrify/Stone-Tablet.org" "Projects")
         "* PLAN %?\nSCHEDULED: %^T\n")
        ;; Add more templates as needed
        ))

(global-set-key (kbd "C-c r n") 'org-roam-capture)
(global-set-key (kbd "C-c r f") 'org-roam-node-find)
(global-set-key (kbd "C-c r m") 'completion-at-point)
(global-set-key (kbd "C-c r a t") 'org-roam-tag-add)
(global-set-key (kbd "C-c r a r") 'org-roam-ref-add)

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
(add-hook 'find-file-hook (lambda () (display-line-numbers-mode 0))))

(use-package vertico
:custom
(vertico-cycle t)
  :config
(vertico-mode 1))

(use-package corfu
    :demand t
    ;; Optional customizations
    :custom
    (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
    (corfu-auto t)                 ;; Enable auto-completion
    ;; (corfu-separator ?\s)          ;; Orderless field separator
    ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
    ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
    ;; (corfu-preview-current nil)    ;; Disable current candidate preview
    ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
    ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
    ;; (corfu-scroll-margin 5)        ;; Use scroll margin
    (corfu-auto-delay 0.0)          ;; Enable auto-completion
    ;; Enable Corfu only for certain modes.
    ;; :hook ((prog-mode . corfu-mode)
    ;;        (shell-mode . corfu-mode)
    ;;        (eshell-mode . corfu-mode))

    ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
    ;; be used globally (M-/).  See also the customization variable
    ;; `global-corfu-modes' to exclude certain modes.
    :init
    (global-corfu-mode)
    :bind
    (:map corfu-map
          ("TAB" . corfu-next)
          ([backtab] . corfu-previous)))

  
(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

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

(use-package consult
:ensure t)

(setq savehist-mode t)

(use-package langtool
  :elpaca (langtool :host github :repo "mhayashi1120/Emacs-langtool")
  :init
  (setq langtool-language-tool-jar "~/.config/emacs/LanguageTool/languagetool-commandline.jar")
  (setq langtool-default-language "en-GB"))

(use-package ob-mermaid
  :ensure t
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((mermaid . t))))

(use-package org-journal)

(setq org-startup-indented t
          org-pretty-entities t
          org-hide-emphasis-markers t
          org-startup-with-inline-images t)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))

(use-package org-modern
    :elpaca(org-modern :host github :repo "minad/org-modern")
    :init
(with-eval-after-load 'org (global-org-modern-mode))
     )

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
    "◀── now ─────────────────────────────────────────────────")

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

(setq org-roam-db-node-include-function
      (lambda ()
        (not (member "ATTACH" (org-get-tags)))))

(setq org-directory "~/Documents/wORG/Colloquy")
(setq org-journal-dir "~/Documents/wORG/MyPersonal/My-Microsome")
(setq org-roam-directory "~/Documents/wORG/Org-ROAM/Alexandria")

(use-package org-remark
    :elpaca (org-remark :host github :repo "nobiot/org-remark")
    ;; Alternative way to enable `org-remark-global-tracking-mode' in
    ;; `after-init-hook'.
    ;; :hook (after-init . org-remark-global-tracking-mode)
    :init
    ;; It is recommended that `org-remark-global-tracking-mode' be
    ;; enabled when Emacs initializes. Alternatively, you can put it to
    ;; `after-init-hook' as in the comment above
    (org-remark-global-tracking-mode 1))
(elpaca-wait)
     (require 'org-remark-info)
       (org-remark-info-mode 1)

     (require 'org-remark-eww)
       (org-remark-eww-mode 1)
; Optional if you would like to highlight EPUB books via nov.el
(with-eval-after-load 'nov
  (org-remark-nov-mode +1))
   ;  (require 'org-remark-nov)
    ;   (org-remark-nov-mode 1)

(use-package org-roam-timestamps
  :after org-roam
  :config (org-roam-timestamps-mode))
(setq org-roam-timestamps-timestamp-parent-file t)
(setq org-roam-timestamps-remember-timestamps t)
(setq org-roam-timestamps-minimum-gap 3600)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("mer" . "src mermaid"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(define-abbrev global-abbrev-table "m" "* Thyroid\n** Embryology\n** Anatomy** Physiology Functions\n** Pathology** Clinical Parameters to look out for\n** Pharmacology")
(setq-default abbrev-mode t)

(use-package org-side-tree)

(use-package olivetti
        :ensure t
        :hook
        (org-mode . olivetti-mode)
        :init
        (setq olivetti-body-width 140)
)

(use-package org-noter)

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))
  (use-package pdf-tools)
(use-package djvu)

(use-package consult-org-roam
   :ensure t
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key "M-.")
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c r c f" . consult-org-roam-file-find)
   ("C-c r c b" . consult-org-roam-backlinks)
   ("C-c r c l" . consult-org-roam-forward-links)
   ("C-c r c s" . consult-org-roam-search))

(setq org-agenda-start-on-weekday 0) ; 0 for Sunday, 1 for Monday, and so on
  (setq org-log-done t)
(setq org-agenda-files '("~/Documents/wORG/My-Personal/Transmogrify"))

(setq org-todo-keywords '((sequence "IDEA(i)" "PLAN(p)" "SCHEDULE(s)" "|" "TODO(t)" "In-Progress(r)" "DONE(d)" "|" "CANCELLED(c)" "DEFERRED(f)")))

(use-package svg-tag-mode)
(use-package svg-lib
  :elpaca (svg-lib :host github :repo "rougier/svg-lib")
  :demand t
  )

(setq svg-tag-keywords
      '(("IDEA(i)" . ((lambda (tag) (svg-tag-make "IDEA"))))
        ("PLAN(p)" . ((lambda (tag) (svg-tag-make "PLAN"))))
        ("SCHEDULE(s)" . ((lambda (tag) (svg-tag-make "SCHEDULE"))))
        ("TODO(t)" . ((lambda (tag) (svg-tag-make "TODO"))))
        ("In-Progress(r)" . ((lambda (tag) (svg-tag-make "In-Progress"))))
        ("DONE(d)" . ((lambda (tag) (svg-tag-make "DONE"))))
        ("CANCELLED(c)" . ((lambda (tag) (svg-tag-make "CANCELLED"))))
        ("DEFERRED(f)" . ((lambda (tag) (svg-tag-make "DEFERRED"))))))

;(setq svg-tag-tags (append svg-tag-tags svg-tag-keywords))

(defun org-agenda-show-svg ()
  (let* ((case-fold-search nil)
         (keywords (mapcar #'svg-tag--build-keywords svg-tag--active-tags))
         (keyword (car keywords)))
    (while keyword
      (save-excursion
        (while (re-search-forward (nth 0 keyword) nil t)
          (overlay-put (make-overlay
                        (match-beginning 0) (match-end 0))
                       'display  (nth 3 (eval (nth 2 keyword)))) ))
      (pop keywords)
      (setq keyword (car keywords)))))
(add-hook 'org-agenda-finalize-hook #'org-agenda-show-svg)

(setq org-highest-priority ?A       ; Highest priority is 'A'
      org-lowest-priority ?D        ; Lowest priority is 'D'
      org-default-priority ?C)      ; Default priority is 'C'

(use-package org-recur)

(setq org-journal-date-prefix "#+TITLE:"
       org-journal-time-prefix "*  "
       org-journal-date-format "%A, %F"
       org-journal-file-format "%F.org")

(setq org-roam-capture-templates
      `(("d" "Default" plain "%?"
         :target (file+head "${slug}.org" "#+title:${title}\n#+filetags: ${tag}\n#+OPTIONS: toc:nil timestamp:t")
         :unnarrowed t)
        ("r" "Roam Note" plain "%?"
         :target (file+head "${slug}.org" "#+title: ${title}\n#+filetags: ${tag}\n#+OPTIONS: toc:nil timestamp:t\n\n* Thing that I have understood\n\n* Thing that I have 50-50% Confidence\n\n* Thing that I blew past my head and need to review\n\n* Research Article\n\n* Devil's Advocate Corner\n\n")
         :unnarrowed t)))

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
