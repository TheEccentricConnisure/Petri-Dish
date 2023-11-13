;; Initialize package.el
(require 'package)
(setq package-archives '(("GNU ELPA" . "https://elpa.gnu.org/packages/")
                         ("MELPA" . "https://melpa.org/packages/")
                         ("Org" . "https://orgmode.org/elpa/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
;; provide the required ids
(provide 'packaging)
