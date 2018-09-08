;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq doom-leader-key "SPC"
      doom-localleader-key ","
      doom-font (font-spec :family "Fira Mono Medium" :size 14)
      doom-big-font (font-spec :family "Fira Mono Medium" :size 19)
      doom-scratch-buffer-major-mode 'emacs-lisp-mode)

(setq initial-major-mode 'emacs-lisp-mode
      frame-resize-pixelwise t
      vc-follow-symlinks t
      inhibit-compacting-font-caches t
      make-backup-files nil
      create-lockfiles nil
      backward-delete-char-untabify-method 'untabify
      calendar-week-start-day 1)

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

(setq projects-directory "~/Projects")

(setq magit-repository-directories `((,doom-private-dir . 0)
                                     (,doom-emacs-dir . 0)
                                     (,projects-directory . 1))
      magit-save-repository-buffers nil)

(def-package! reverse-im
  :config
  (reverse-im-activate "russian-computer")
  (after! evil
    ;; cyrillic tweaks
    (define-key evil-normal-state-map (kbd "C-х") 'evil-force-normal-state)
    (define-key evil-insert-state-map (kbd "C-х") 'evil-normal-state)
    (define-key evil-visual-state-map (kbd "C-х") 'evil-exit-visual-state)))

(setq org-ellipsis "…"
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
        org-bullets-bullet-list '("#"))

(add-to-list 'evil-org-special-o/O 'item)

;; TODO integrate with doom-emacs bindings
;; (def-package! org-expiry)

;; (after! org
;;   (org-expiry-insinuate))

;; FIXME evil-snipe? M-t?
;; (after! evil-org
;;   (setq evil-org-key-theme `(textobjects
;;                              navigation
;;                              additional
;;                              insert
;;                              todo)))

(def-package! google-translate
  :config
  (setq google-translate-default-target-language "ru"
        google-translate-default-source-language "en")
  (map! :leader
        :prefix "h"
        :n "t" #'google-translate-at-point
        :n "T" #'google-translate-at-point-reverse))

(def-package! link-hint
  :config
  (map! :leader
        :prefix "o"
        :desc "Open link" :n "l" #'link-hint-open-link))

(def-package! olivetti
  :config
  (setq-default olivetti-body-width 100)
  (map! :leader
        :prefix "t"
        :desc "Olivetti mode" :n "o" #'olivetti-mode))
