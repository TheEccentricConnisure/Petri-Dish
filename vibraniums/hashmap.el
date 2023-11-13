(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'dr/org-babel-tangle-config)))

(leader-key
  "c" '(:ignore t :wk "config") 
  "c r" 'my-reload-emacs
  "c s" 'restart-emacs
  "c e" '((lambda () (interactive) (find-file "~/.config/emacs/README.org")) :wk "Edit emacs config"))

(leader-key
  "f"'(:ignore t :wk "Filling")
  "f /" '(find-file :wk "Find file")
  "f r" '(org-refile :wk "REfile")
)

(leader-key
  "s" '(:ignore t :wk "Shell") 
  "s t" '(eshell :wk "Eshell Launcher"))

(leader-key
  "w" '(:ignore t :wk "Window") 
  "w d" '(delete-window :wk "Kill The current Window")
  "w c" '(delete-other-windows :wk "Kill the Other window")
  "w s v" '(split-window-right :wk "Split Window on Right")
  "w s h" '(split-window-below :wk "Split Window Below"))

(leader-key
  "b" '(:ignore t :wk "Buffer")
  "b b" '(switch-to-buffer :wk "Switch buffer")
  "b i" '(ibuffer :wk "Ibuffer")
  "b k" '(kill-this-buffer :wk "Kill this buffer")
  "b n" '(next-buffer :wk "Next buffer")
  "b p" '(previous-buffer :wk "Previous buffer")
  "b r" '(revert-buffer :wk "Reload buffer")
  "b h" '(dashboard-open :wk "Home Buffer")
  "b N" '(evil-buffer-new :wk "Open New Buffer"))

(leader-key
    "r" '(:ignore t :wk "roam")
    "r i" '(org-roam-ui-mode :wk "Obsidian")
    "r n" '(:ignore t :wk "Notes")
    "r n n" '(org-roam-capture :wk "new note")
    "r n f" '(org-roam-node-find :wk "Find the Note")
    "r n m" '(completion-at-point :wk "Mapping Nodes")
    )

(leader-key
"n" '(:ignore t :wk "org-remark")
"nm" '(org-remark-mark :wk "Mark")
"nl" '(org-remark-mark-line :wk "Mark Line")
"no" '(org-remark-open :wk "Open")
"n]" '(org-remark-view-next :wk "View Next")
"n[" '(org-remark-view-prev :wk "View Prev")
"nr" '(org-remark-remove :wk "Remove")
"nd" '(org-remark-delete :wk "Delete"))

(leader-key
   "z" '(:ignore t :wk "zotero")
   "z p" '('my/download-pdf-and-file-organize :wk "organise")
  )

(leader-key
  "o" '(:ignore t :wk "Org")
  "o t" '(org-timmer :wk "start timer")
  "o a" '(org-agenda :wk "Org-Agenda") )

(provide 'hashmap)
