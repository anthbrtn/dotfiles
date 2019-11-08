;;; .doom.d/config.el -*- lexical-binding: t; -*-
;; TODO: Flyspell setup, LSP setup, synosaurus setup
;; eyebrowse setup (easy window configuration)
;; focus mode (focus.el), which desaturates inactive paragraphs
;; org-habit, which gives my agenda a weekly habit (that's intense)

;; ###### INITIALIZATION #####

;; first, we set up the packages that our config is going to call
;; the doom automatic package setup gives us some predefined stuff, but for the sake
;; of literacy we are going to call anything for whom we're configuring with use-package.
;; any other packages that have been installed by us also need to be called with
;; use-package.

;; this package allows us to use predefined snippets
;; the snippets still need to be set up
(use-package! yasnippet-snippets)

;; we want autocompletion and we use company-mode to get it
;; this is a doom module, but the setup for backends is finicky
(use-package! company)

;; we need to load any extra packages for company autocompletion framework
;; this package allows for our autocompletion framework to search for bibtex keys
(use-package! company-bibtex)

;; this package is a prose linter for things like passive voice
(use-package! writegood-mode)

;; ivy is a completion framework that is already loaded by doom
;; but we have some configuration and additions to do

;; this function searches for bibtex
;; entries and inserts them
(use-package! ivy-bibtex)

;; this package, when called from M-x, provides preset phrases to get over mental
;; blocks when writing academic papers etc
(use-package! academic-phrases)

;; this package integrates pandoc with emacs. It's not fully set up yet
(use-package! pandoc-mode)

;; this highlights hex colour strings etc as the colour they represent
(use-package! rainbow-mode)

;; this package has a few things from sublime like minimap and smooth scrolling
(use-package! sublimity)

;; this is org-journal, which lets me quickly add and track a journal with SPC-j
(use-package! org-journal)

;; automatic completion of quote marks, and cycling em-dashes
(use-package! typo)

;; INITIALIZATION CONFIGURATION
;; this makes it feel personalized and also lets org do some autofilling
(setq user-full-name "Anthony Burton"
      user-mail-address "anthbrtn@gmail.com")

;; Explicitly load evil-surround so that extra pairs can be loaded in time
(require 'evil-surround)

;; Doom Emacs doesn't play along nicely with the noninteractive Emacs daemon,
;; but server functionality is still very useful
(require 'server)
(unless (server-running-p)
  (server-start))

;; now we can initialize the completion framework itself
;; this enables us to company-mode completion popups for every file
(require 'company)
;; this makes us able to configure what company mode does on initialization
(add-hook 'after-init-hook 'global-company-mode)

;; this sets up company to autocomplete suggestions that it comes up with


;; this increases the garbage collector threshold, speeding startup
;; grabbed from https://github.com/jamiecollinson/dotfiles/blob/master/config.org/
(setq gc-cons-threshold 10000000)
;; Restore after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 1000000)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

;; store backups and auto-saves in \temporary-file-directory (default: /tmp)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; don't assume that sentences have two spaces after a period
;; this helps with evil key navigation and makes \) a more useful key
(setq sentence-end-double-space nil)

;; for dired, the default directory browser, set it human readable
(setq-default dired-listing-switches "-alh")

;; when a buffer is changed, automatically change any other buffer with that file
(global-auto-revert-mode t)

;; display the current time
(display-time-mode t)

;; KEYBINDINGS
;; an example of a keybinding config
;; define-key defines the key, map argument takes the keymap, and zoom-in is a func
;; (define-key global-map (kbd "C-1") 'zoom-in)
;; these keybindings set { and } to move buffers
(define-key evil-normal-state-map (kbd "{") 'evil-next-buffer)
(define-key evil-normal-state-map (kbd "}") 'evil-prev-buffer)

;; if the current buffer can't be saved as user, ask automatically if it
;; should be saved with sudo
(defun ph/sudo-file-name (filename)
  "Prepend '/sudo:root@`system-name`:' to FILENAME if appropriate.
This is, when it doesn't already have a sudo-prefix."
  (if (not (or (string-prefix-p "/sudo:root@localhost:"
                                filename)
               (string-prefix-p (format "/sudo:root@%s:" system-name)
                                filename)))
      (format "/sudo:root@%s:%s" system-name filename)
    filename))

(defun ph/sudo-save-buffer ()
  "Save FILENAME with sudo if the user approves."
  (interactive)
  (when buffer-file-name
    (let ((file (ph/sudo-file-name buffer-file-name)))
      (if (yes-or-no-p (format "Save file as %s ? " file))
          (write-file file)))))

(advice-add 'save-buffer :around
            '(lambda (fn &rest args)
               (when (or (not (buffer-file-name))
                         (not (buffer-modified-p))
                         (file-writable-p (buffer-file-name))
                         (not (ph/sudo-save-buffer)))
                 (call-interactively fn args))))

;; remember the place that my cursor last was when opening a file
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; TEXT MODE
;; modes designed for writing markup and text - plaintext, md, and org

;; to call our prose linter
(add-hook 'text-mode-hook 'writegood-mode)
;; this enables soft wrapping at 80 columns
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
;; this enables pandoc integration
(add-hook 'text-mode-hook 'pandoc-mode)
;; this sets up typo.el for quotations and em dash cycling
(typo-global-mode 1)
(add-hook 'text-mode-hook 'typo-mode)


;; ORG MODE
;; an organizational tool with its own markup language. Like markdown-plus

;; this tells the org agenda buffer to read from my todo file
(setq org-agenda-files '("~/org/todo.org"))

;; this sets the default directory for any built-in org mode file creation
(setq org-directory "~/nextcloud/")

;; this sets the default notes file (called from M-x) to a file inside the above folder
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; this sets the org agenda, when called, to open in a side window and not
;; take over the current buffer
(setq org-agenda-window-setup 'current-window)

;; this is configuring org-journal, a journalling subsystem for org
;; setting up the journalling directory
(customize-set-variable 'org-journal-dir "~/nextcloud/journal/")
(customize-set-variable 'org-journal-date-format "%A, %d %B %Y")

;; creating my own keymaps for this, so now SPC-j gets me to my journal setup
;; this uses the doom map function, which is different from the builtin
;; kbd setup. either will work, but noting the difference is important for my learning
(map! :leader
      (:prefix ("j" . "journal") ;; org-journal bindings
        :desc "Create new journal entry" "j" #'org-journal-new-entry
        :desc "Open previous entry" "p" #'org-journal-open-previous-entry
        :desc "Open next entry" "n" #'org-journal-open-next-entry
        :desc "Search journal" "s" #'org-journal-search-forever))

(setq org-list-allow-alphabetical t)


;; PDF MODE
;; disable linum mode, which causes problems when viewing pdfs
(add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))
;; set cursor to be visible when reading pdfs (original value nil, can just
;; delete the following line if it doesn't work; want to see if I can see where
;; I'm navigating and highlighting PDFs)
(setq-hook! 'pdf-view-mode-hook evil-normal-state-cursor (list bar))

;; automatically add an annotation when highlighting
;; if this works as I think it might, it could be a pain in the ass
(setq pdf-annot-activate-created-annotations t)

;; AUTOCOMPLETION with COMPANY-MODE
;; this sets all text-modes to use our bibtex citekeys completion as default
(set-company-backend! 'text-mode 'company-bibtex 'company-ispell 'company-abbrev 'company-dabbrev 'company-files 'company-yasnippet)
;; these settings are self-explanatory
(setq company-minimum-prefix-length 1
      company-idle-delay 0.022
      ;; this sets up what to look for in completing strings
      completion-styles '(partial-completion initials)
      company-frontends ;; configures behaviour of popup
      '(company-pseudo-tooltip-unless-just-one-frontend ;; this calls the tooltip
        company-preview-frontend ;; this displays all completions inline
        company-echo-metadata-frontend ) ;; not totally sure!
      company-auto-complete t ;; this sets us up to space hit and complete. might be buggy
      )

;; ACADEMIC FUNCTIONS

;; this ensures that our bibtex autocompletion is set. it might be redundant; not sure
(add-to-list 'company-backends 'company-bibtex)
;; this sets the default bib file for company-bibtex to search for keys
(setq company-bibtex-bibliography
'"/home/anthony/Documents/academic/prose/.config/bibliography.bib"
  )
;; this changes org citations for company-bibtex to use pandoc style, instead of
;; org-ref style (which seems like a messy package I don't want to install)
(setq company-bibtex-org-citation-regex "-?@")

(setq bibtex-completion-bibliography
      '("~/Documents/academic/prose/.config/bibliography.bib"))
;; when inserting after search, tell it how to format the citation.
;; org-mode is changed from its default "ebib:" frame because it's fairly useless
(setq bibtex-completion-format-citation-functions
  '((org-mode      . bibtex-completion-format-citation-pandoc-citeproc)
    (latex-mode    . bibtex-completion-format-citation-cite)
    (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
    (default       . bibtex-completion-format-citation-default)))

;if I grab a pdf through ivy-bibtex, it'll automatically download here
; just need to make sure to add to zotero
(setq bibtex-completion-library-path "~/Downloads/") 

;;  ####### UI ######

;; LINES
;; this might be redundant, because of the text-mode hook I call above. But experience so far shows me they aren't opening.
(global-visual-line-mode 1)

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
;; (load-theme 'doom-dracula t)
;; (load-theme 'doom-one t)
;; (load-theme 'doom-one-light t)
(load-theme 'doom-molokai t)
;; (load-theme 'doom-nord-light t)
;; (load-theme 'doom-spacegrey t)

;; smooth scroll and minimap from sublimity package
;; find settings at https://github.com/zk-phi/sublimity
(require 'sublimity)
(sublimity-mode 1)
;; smooth scroll settings
(setq sublimity-scroll-weight 10
      sublimity-scroll-drift-length 5)
;; minimap settings
(setq sublimity-map-size 20)
(setq sublimity-map-fraction 0.3)
(setq sublimity-map-text-scale -7)
;; set how long idle for minimap to show up (it's a performance thing)
(sublimity-map-set-delay 5)


;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;; FONT
(setq display-line-numbers-type 'relative)
(setq doom-font (font-spec :family "SF Mono"
                           ;; :width 'semi-condensed
                           :weight 'semi-light
                           :size 14)
      doom-variable-pitch-font (font-spec :family "Roboto"
                                          :size 16))

;; don't highlight colour names/words, just hex strings
(setq rainbow-x-colors nil)
;; enable only for programming modes, not text modes
(add-hook 'prog-mode-hook 'rainbow-mode)
