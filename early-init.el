(setq package-enable-at-startup nil)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
    (setq inhibit-startup-message t)
    (setq initial-scratch-message nil)
        (defun dr/display-startup-time ()
          (message "Emacs loaded in %s with %d garbage collections."
                   (format "%.2f seconds"
                           (float-time
                             (time-subtract after-init-time before-init-time)))
                   gcs-done))

        (add-hook 'emacs-startup-hook #'dr/display-startup-time)

   (setq inhibit-startup-message t)
    (scroll-bar-mode -1)        ; Disable visible scrollbar
    (tool-bar-mode -1)          ; Disable the toolbar
    (tooltip-mode -1)           ; Disable tooltips
    (set-fringe-mode 10)        ; Give some breathing room
    (menu-bar-mode -1)            ; Disable the menu bar

    ;; Set up the visible bell
    (setq visible-bell t)
    (column-number-mode)
    (global-display-line-numbers-mode t)

(setq byte-compile-warnings '(cl-functions))
