;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq doom-leader-key "SPC"
      doom-localleader-key "SPC m"
      doom-font (font-spec :family "Fira Mono Medium" :size 14)
      doom-big-font (font-spec :family "Fira Mono Medium" :size 19)
      doom-scratch-buffer-major-mode 'emacs-lisp-mode

      ;; initial-major-mode 'emacs-lisp-mode ; makes startup a little bit slow
      frame-resize-pixelwise t
      vc-follow-symlinks t
      inhibit-compacting-font-caches t
      make-backup-files nil
      create-lockfiles nil
      backward-delete-char-untabify-method 'untabify
      calendar-week-start-day 1

      projects-directory "~/Projects"
      magit-repository-directories `((,doom-private-dir . 0)
                                     (,doom-emacs-dir . 0)
                                     (,projects-directory . 1))
      magit-save-repository-buffers nil)

(when window-system
  (setq frame-parameters '((left . 0.5) (top . 0.5)
                           (width . 0.7) (height . 0.9)))
  (dolist (fp frame-parameters)
    (add-to-list 'default-frame-alist fp)))

;; GC tweaks
(add-hook 'focus-out-hook #'garbage-collect)

;; WSL tweaks
(when (file-directory-p "/mnt/c/Windows/System32/cmd.exe")
  ;; https://adam.kruszewski.name/2017/09/emacs-in-wsl-and-opening-links/
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic))))

(def-package! reverse-im
  :config
  (reverse-im-activate "russian-computer")
  (after! evil
    ;; cyrillic tweaks
    (define-key evil-normal-state-map (kbd "C-х") #'evil-force-normal-state)
    (define-key evil-insert-state-map (kbd "C-х") #'evil-normal-state)
    (define-key evil-visual-state-map (kbd "C-х") #'evil-exit-visual-state)))

(setq org-ellipsis " ▼ " ; "…"
      org-hide-emphasis-markers nil ; hide markup elements, e.g. * *, / /, _ _
      org-list-allow-alphabetical t
      org-log-into-drawer t
      org-startup-indented t
      org-pretty-entities t
      org-edit-src-content-indentation 0
      org-src-window-setup 'current-window
      org-tags-column 0
      org-agenda-tags-column 0
      org-directory "~/Dropbox/Org"
      org-default-inbox-file (concat org-directory "/inbox.org")
      org-default-todo-file (concat org-directory "/todo.org")
      org-default-notes-file (concat org-directory "/notes.org")
      org-agenda-files `(,org-default-todo-file ,org-default-inbox-file)
      org-archive-location (concat org-directory "/old/archive.org" "::* From %s")
      org-bullets-bullet-list '("#")
      evil-org-special-o/O '(item table-row)
      evil-org-key-theme '(textobjects insert navigation heading))

(def-package! org-expiry
  :after org
  :config (org-expiry-insinuate))

(def-package! google-translate
  :commands (google-translate-at-point google-translate-at-point-reverse)
  :init
  (setq google-translate-default-target-language "ru"
        google-translate-default-source-language "en"))

(def-package! link-hint
  :commands link-hint-open-link)

(def-package! olivetti
  :commands olivetti-mode
  :config
  (setq olivetti-body-width 100))

(map! :leader
      (:prefix "h"
        :desc "Translate"         :n "t" #'google-translate-at-point
        :desc "Translate reverse" :n "T" #'google-translate-at-point-reverse)
      (:prefix "t"
        :desc "Olivetti"          :n "o" #'olivetti-mode)
      (:prefix "o"
        :desc "Link"              :n "l" #'link-hint-open-link))
