;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
;(package! some-package)
;(package! another-package
;  :recipe (:host github :repo "username/repo"))
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))
;(package! builtin-package :disable t)
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))
;(package! builtin-package :recipe (:branch "develop"))
(package! academic-phrases)
(package! company-bibtex)
(package! rainbow-mode)
(package! typo)
(package! flycheck-vale)
(package! zop-to-char)
(package! exec-path-from-shell)
(package! org-web-tools)
(package! org-projectile)
(package! plan9-theme)
(package! darktooth-theme)
(package! org-ql)
(package! org-sidebar)
(package! company-prescient)
(package! hl-sentence)
(package! sublime-themes)
;; disabled
(package! hl-line :disable t)
