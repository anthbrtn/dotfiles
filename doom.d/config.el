;;; config.el -*- lexical-binding: t; -*-
(setq user-full-name "Anthony G. Burton"
      user-mail-address "ab@anthbrtn.com")

(setq doom-font (font-spec :family "Roboto Mono" :weight 'semilight))
(setq doom-variable-pitch-font (font-spec :family "Breeze Sans" :size 30))
(setq doom-theme 'mccarthy
      ;; doom-city-lights-brighter-comments 't
      ;; doom-city-lights-brighter-modeline 't
      ;; doom-city-lights-padded-modeline 't
      ;; doom-city-lights-comment-bg 't
      ;; doom-tomorrow-day-padded-modeline t
      doom-dark+-padded-modeline 't)
(setq! +biblio-pdf-library-dir "~/library/"
       +biblio-default-bibliography-files '("~/library/references.bib")
       +biblio-notes-path "~/documents/notes")

(setq org-directory "~/documents/journal/")
(use-package! org
  :init
  (setq org-startup-indented t
      org-adapt-indentation t
      org-hide-leading-stars nil
      org-agenda-files '("~/documents/journal/work.org"
                         "~/documents/journal/personal.org"
                         "~/documents/journal/refile.org")
      org-directory "~/documents/journal/"
      org-confirm-babel-evaluate nil
      org-html-use-infojs 'when-configured
      org-log-done 'time
      org-fontify-quote-and-verse-blocks nil
      org-fontify-whole-heading-line nil
      org-src-fontify-natively t
      org-ellipsis "  "
      org-startup-folded t
      org-archive-location "~/documents/journal/archive.org::* From %s"
      )
  (add-hook! 'org-mode-hook
           #'+word-wrap-mode
           #'typo-mode
           #'org-superstar-mode
  )
  (remove-hook! 'org-mode-hook
  ;; #'auto-fill-mode
  #'display-line-numbers-mode)

  :config
  (setq truncate-lines nil)
  (setq-default line-spacing 5)
  (setq-local fill-column 10000)
  (visual-fill-column-mode)
  (setq visual-fill-column-center-text 't)
  ;; org-todo settings
  (setq org-todo-keywords
      (quote ((sequence "TODO(t!)"  "NEXT(n!)" "|" "DONE(d!)")
	      (sequence "SCHEDULED(s)" "REPEAT(r)" "WAIT(w!)"  "|"  "PAUSED(P@/!)" "CANCELLED(c@/!)" )
	      (sequence "PROJECT(p!)" "IDEA(i!)" "MAYBE(y!)" "STAGED(s!)" "WORKING(k!)" "|" "USED(u!/@)")
)))
  )
(use-package! org-superstar
  :after org
  :config
  (setq org-superstar-headline-bullets-list '("")
        org-superstar-special-todo-items 't
        org-superstar-todo-bullet-alist '(
                                          ("DONE" . ?)
                                          ("PROJECT" . ?)
                                          ("CANCELLED" . ?)
                                          ("WAIT" . ?);; use wait for waiting for a date
                                          ("PAUSED" . ?);; use hold for other's people work holding up
                                          ("TODO" . ?)
                                          ("SCHEDULED" . ?)
                                          ("[x]" . ?)
                                          )
        )
  )
(use-package! org-agenda
  :init
  (setq org-agenda-block-separator nil
        org-agenda-start-with-log-mode t
        org-agenda-files '("~/documents/journal/work.org"
                           "~/documents/journal/personal.org"
                           "~/documents/journal/refile.org")
        org-agenda-span 7
        org-agenda-start-day "-1d"
        org-agenda-compact-blocks t
        org-agenda-timegrid-use-ampm t
        org-agenda-skip-deadline-if-done 't
        org-agenda-skip-timestamp-if-done 't
        org-agenda-skip-scheduled-if-done 't
        org-icalendar-combined-agenda-file "~/documents/journal/org.ics"
        org-agenda-custom-commands
        '(("n" "Agenda and all TODOs"
          ((agenda "")
           (alltodo ""))
          nil
          ("~/code/startpage/index.html")))
  )
  )

(use-package! org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/documents/notes/")
  :config
  (setq org-roam-capture--file-name-default ""
        org-roam-capture-templates
  '(("d" "default" plain (function org-roam-capture--get-point)
     "%?"
     :file-name "${slug}"
     :head "#+TITLE: ${title}\n#+ROAM_TAGS: "
     :unnarrowed t)
    )
  )
)

(use-package org-projectile
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (progn
    (setq org-projectile-projects-file
          "~/documents/journal/work.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates))
  :ensure t)

(use-package! org-agenda
  :init
  (setq org-agenda-block-separator nil
        org-agenda-start-with-log-mode t
        org-agenda-sort-notime-is-late t
        org-agenda-todo-ignore-with-date t)
  )

(use-package! lsp
  :config
  (setq lsp-flycheck-live-reporting t)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  )

(use-package! anaconda-mode
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning 'nil
        )
  (add-to-list 'python-shell-completion-native-disabled-interpreters "ipython"))

(use-package! org-gcal
  :config
  (setq org-gcal-client-id "358262195125-majds3utoqeg8i3nmbjt7df1uopbdmj6.apps.googleusercontent.com"
      org-gcal-client-secret "2r9WZLEcvmbzV3_MOTkCgsJE"
      org-gcal-file-alist '(("3kej37buoqh2t633scnjhcatak@group.calendar.google.com" .  "~/documents/journal/org/academic.org")
                ("anthbrtn@gmail.com" . "~/documents/journal/org/personal.org")
                            )
      )
  )

(setq +python-ipython-repl-args '("-i" "--simple-prompt" "--no-color-info"))
(setq +python-jupyter-repl-args '("--simple-prompt"))

(use-package! zop-to-char
  :config
  (global-set-key [remap zap-to-char] 'zop-up-to-char)
  )

(use-package! deft
  :config
  (setq deft-directory "~/documents/notes/"
        deft-new-file-format ""
      )
  )

(use-package! company
  :init
  (setq company-tooltip-align-annotations t)
  :config
  (set-company-backend! 'org-mode 'company-org-roam 'company-bibtex)
  (set-company-backend! 'markdown-mode 'company-bibtex 'company-dabbrev)
  (set-company-backend! :derived 'prog-mode 'company-files)
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2)
)

(use-package! company-prescient
  :config
  (add-hook! 'company-mode 'company-prescient-mode)
  )

(use-package! company-bibtex
  :config
  (setq company-bibtex-bibliography "/home/anthony/library/references.bib"
      company-bibtex-org-citation-regex "-?@"
      )
)
(use-package! ivy-bibtex
  :config
  (setq bibtex-completion-pdf-field "file"
      bibtex-completion-bibliography '("~/library/references.bib")
      bibtex-completion-cite-default-command "@"
      bibtex-completion-notes-path "~/documents/notes/"
      bibtex-completion-notes-extension ".org"
      bibtex-completion-format-citation-functions '(
      (org-mode . bibtex-completion-format-citation-pandoc-citeproc)
      (latex-mode    . bibtex-completion-format-citation-cite)
      (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
      (default       . bibtex-completion-format-citation-default)
      )
      )
  )

(use-package! org-journal
  :config
  (setq org-journal-dir "~/documents/journal"
      org-journal-file-type 'yearly
      org-journal-file-format "%Y%m%d.org"
      org-journal-time-format "%A %D: %R"
      )
  )

(use-package! writeroom-mode
  :config
  (setq writeroom-border-width 0
        )
  )
;; (use-package! mu4e
;;               :config
;;               (setq ;; mu4e-compose-pre-hook nil
;;                     mu4e-attachment-dir "home/anthony/Downloads"
;;                     mu4e-maildir "/home/anthony/.local/share/mail"
;;                     mu4e-get-mail-command "mbsync -a -c ~/.config/mbsyncrc"
;;                     ;; message-sendmail-extra-arguments '("--read-envelope-from")
;;                     message-send-mail-function 'message-send-mail-with-sendmail
;;                     send-mail-function 'smtpmail-send-it
;;                     mu4e-context-policy 'pick-first
;;                     mu4e-compose-context-policy 'always-ask
;;                     sendmail-program "/usr/bin/msmtp"
;;                     mu4e-bookmarks
;;                     (quote
;;                       (("maildir:/ryerson/INBOX OR maildir:/gmail/INBOX OR maildir:/personal/INBOX" "Universal inbox" 105)
;;                        ("maildir:/personal/Listservs" "Mailing lists" 108)
;;                        ("flag:unread AND flag:inbox AND NOT flag:trashed" "Unread inbox messages" 117)
;;                        ("date:today..now AND NOT flag:trashed" "Today's messages" 116)
;;                        ("date:7d..now" "Last 7 days" 119)
;;                        ("mime:image/*" "Messages with images" 112)))))


;; system
(setq auto-save-default t)
;; UI
(when window-system
  (set-frame-size (selected-frame) 120 50))
(auto-insert-mode t)
(blink-cursor-mode t)
(setq sentence-end-double-space nil)
(add-hook 'prog-mode-hook 'rainbow-mode)
(global-auto-revert-mode t)
(save-place-mode t)
(setq uniquify-buffer-name-style 'forward)
(setq frame-resize-pixelwise t)
;; (fringe-mode '(50 . 50))
;; (setq display-line-numbers-type 'nil)
;; (setq-default mode-line-format
;;               (list
;;                (propertize (make-string 50 ?\ )
;;                            'face `(:box nil)
;;                            )
;;                )
;;               )
;; (setq-default header-line-format
;;               (list
;;                             'display '(raise 0.0)
;;                             (propertize " %+%b | %c:%l " 'face `(:weight bold
;;                                                                  :box nil)
;;                                         )
;;                             )
;;               )
;; (set-face-attribute 'header-line nil
;;                     :underline (face-foreground 'default)
;;                     :overline nil
;;                     :background (face-background 'default)
;;                     :box nil)
;; (set-face-attribute 'mode-line nil
;;                     :overline nil
;;                     :box nil
;;                     :foreground (face-background 'default)
;;                     :background (face-background 'default)
;;                     )
;; (set-face-attribute 'org-document-title nil)
;; (set-face-attribute 'fringe nil
;;                     :underline (face-foreground 'default)
;;                     :overline nil
;;                     :box nil
;;                     :foreground (face-background 'default)
;;                     :background (face-background 'default)
;;                     )

;; text mode

;; keybindings
(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "M-j"))
(global-unset-key (kbd "M-k"))
(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-q"))
(map!
  (:when (featurep! :ui workspaces)
         :g "M-q"   #'+workspace/close-window-or-workspace
         :g "M-n"   #'+workspace/new
         :g "M-1"   #'+workspace/switch-to-0
         :g "M-2"   #'+workspace/switch-to-1
         :g "M-3"   #'+workspace/switch-to-2
         :g "M-4"   #'+workspace/switch-to-3
         :g "M-5"   #'+workspace/switch-to-4
         :g "M-6"   #'+workspace/switch-to-5
         :g "M-7"   #'+workspace/switch-to-6
         :g "M-8"   #'+workspace/switch-to-7
         :g "M-9"   #'+workspace/switch-to-8
         :g "M-0"   #'+workspace/switch-to-final
         )
  )
(map! :map vterm-mode-map
      :gnvim "M-h" nil
      :gnvim "M-j" nil
      :gnvim "M-k" nil
      :gnvim "M-l" nil
      :gnvim "M-q" nil
      )
(map! :map org-mode-map
      :gnvim "M-h" nil
      :gnvim "M-j" nil
      :gnvim "M-k" nil
      :gnvim "M-l" nil
      )
(map! :map markdown-mode-map
      :gnvim "M-h" nil
      :gnvim "M-j" nil
      :gnvim "M-k" nil
      :gnvim "M-l" nil
      )
(map! :leader
      :gnvm "f z" #'counsel-fzf
      :gnvm "f r" #'counsel-buffer-or-recentf
      :gnvm "o p" #'counsel-org-agenda-headlines
      :gnvm "n n" #'counsel-org-capture
      )
(map!   :map global-map
        "M-h"   #'evil-window-left
        "M-j"   #'evil-window-down
        "M-k"   #'evil-window-up
        "M-l"   #'evil-window-right
        "M-;"   #'+evil-window-vsplit-a
        "M-w"   #'kill-current-buffer
        "M-<Tab>"   #'next-buffer
        "C-c c" #'counsel-org-capture
        :nvm "j"     #'evil-next-visual-line
        :nvm "k"     #'evil-previous-visual-line
        )
(map! :leader
      (:prefix ("e" . "mu4e for email")
               :desc "Open mu4e main page" "e" #'=mu4e
               :desc "Store email in todo list" "x" #'org-mu4e-store-and-capture
               :desc "Convert region to roam note" "r" #'org-roam-insert-replace-region-with-link-and-follow ()
               ))

;; the evil-org package takes over all of these keybindings above, so I need to disable "additional" in this alist
(setq evil-org-key-theme (quote (navigation insert textobjects calendar)))

;; org capture templates
(setq org-capture-templates
      '(
        ("a"              ; key
         "work"            ; name
         entry             ;type
         (file "~/documents/journal/work.org") ; target
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n" ; template for entry
         :empty-lines 1    ; properties
         :created t        ; properties
         )
        ("t"              ; key
         "quick todo"            ; name
         entry             ;type
         (file "~/documents/journal/refile.org") ; target
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n" ; template for entry
         :empty-lines 1    ; properties
         :created t        ; properties
         )
        ("j" "Journal entry"
         entry
         (file "~/documents/journal/journal.org")
         "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
         )
        ("p"              ; key
         "personal"            ; name
         entry             ;type
         (file "~/documents/journal/personal.org") ; target
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n" ; template for entry
         :empty-lines 1    ; properties
         :created t        ; properties
         )
        ("w"
         "web capture"
         entry
         (file+headline (lambda () (concat org-directory "/refile.org"))
                        "Web captures")
         "* %a\nCaptured On: %U\nWebsite: %l\n\n%i\n%?"
        )
        )
    )

;; custom functions
(defun arrayify (start end quote)
    "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
    (interactive "r\nMQuote: ")
    (let ((insertion
           (mapconcat
           (lambda (x) (format "%s%s%s" quote x quote))
           (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))
(defun imenu-rescan ()
   (interactive)
   (imenu--menubar-select imenu--rescan-item))
(defun org-roam-insert-replace-region-with-link-and-follow ()
(interactive )
(let ((title (buffer-substring (mark) (point)) )
        (top (current-buffer)))
    (org-roam-find-file title)
    (let ((target-file (buffer-file-name (buffer-base-buffer)))
        (note-buffer (current-buffer)))
    (switch-to-buffer top nil t)
    (kill-region (mark) (point))
    (insert (concat "[[" target-file "][" title "]]"))
    (switch-to-buffer note-buffer nil t)
    (save-buffer))))

;; org-roam tags functions
;; from https://gist.github.com/d12frosted/4a55f3d072a813159c1d7b31c21bac9a
(defun +org-notes-tags-read ()
  "Return list of tags as set in the buffer."
  (org-roam--extract-tags-prop (buffer-file-name (buffer-base-buffer))))

(defun +org-notes-tags-delete ()
  "Delete a tag from current note."
  (interactive)
  (unless (+org-notes-buffer-p)
    (user-error "Current buffer is not a note"))
  (let* ((tags (+org-notes-tags-read))
         (tag (completing-read "Tag: " tags nil 'require-match)))
    (+org-buffer-prop-set
     "ROAM_TAGS"
     (combine-and-quote-strings (delete tag tags)))
    (org-roam-db--update-tags)))

(defun +org-notes-tags-add ()
  "Add a tag to current note."
  (interactive)
  (unless (+org-notes-buffer-p)
    (user-error "Current buffer is not a note"))
  (let* ((tags (seq-uniq
                (+seq-flatten
                 (+seq-flatten
                  (org-roam-db-query [:select tags :from tags])))))
         (tag (completing-read "Tag: " tags)))
    (when (string-empty-p tag)
      (user-error "Tag can't be empty"))
    (+org-buffer-prop-set
     "ROAM_TAGS"
     (combine-and-quote-strings (seq-uniq (cons tag (+org-notes-tags-read)))))
    (org-roam-db--update-tags)))

(defun +org-notes-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-equal (expand-file-name (file-name-as-directory org-roam-directory))
                     (file-name-directory buffer-file-name))))

(defun +seq-flatten (list-of-lists)
  "Flatten LIST-OF-LISTS."
  (apply #'append list-of-lists))

(defun +org-buffer-prop-set (name value)
  "Set a buffer property called NAME to VALUE."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (if (re-search-forward (concat "^#\\+" name ": \\(.*\\)") (point-max) t)
        (replace-match (concat "#+" name ": " value))
      ;; find the first line that doesn't begin with ':' or '#'
      (let ((found))
        (while (not (or found (eobp)))
          (beginning-of-line)
          (if (or (looking-at "^#")
                  (looking-at "^:"))
              (line-move 1 t)
            (setq found t)))
        (insert "#+" name ": " value "\n")))))
