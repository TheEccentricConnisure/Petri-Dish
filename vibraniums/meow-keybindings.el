(setq meow-leader-key "SPC"
      meow-leader-idle-delay 0.3) ;; Adjust the delay as needed

      (meow-leader-define-key
      "r" '(:ignore t :wk "roam")
      "r i" '(org-roam-ui-mode :wk "Obsidian")
      "r n" '(:ignore t :wk "Notes")
       "r n n" '(org-roam-capture :wk "new note")
      "r n f" '(org-roam-node-find :wk "Find the Note")
     "r n c" '(completion-at-point :wk "Mapping Nodes")
    )
  (meow-leader-define-key
    "o" '(:ignore t :wk "Org")
    "o a" '(org-agenda :wk "Org-Agenda") )
;; provide the required ID
(provide 'meow-keybindings)
