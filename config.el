;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Fira Mono Medium" :size 14))

(def-package! reverse-im
  :config
  (reverse-im-activate "russian-computer"))

(def-package! docker)
(def-package! docker-tramp)
(def-package! dockerfile-mode)

(after! docker
  (set-evil-initial-state! 'docker-container-mode 'emacs)
  (set-evil-initial-state! 'docker-image-mode 'emacs)
  (set-evil-initial-state! 'docker-network-mode 'emacs)
  (set-evil-initial-state! 'docker-volume-mode 'emacs)
  (set-evil-initial-state! 'docker-machine-mode 'emacs))

(map!
 (:leader
   (:prefix "d"
    :desc "Docker":n            "." #'docker
    :desc "Docker Containers":n "c" #'docker-containers
    :desc "Docker Images":n     "i" #'docker-images
    :desc "Docker Networks":n   "n" #'docker-networks
    :desc "Docker Volumes":n    "v" #'docker-volumes
    :desc "Docker Machines":n   "m" #'docker-machines)))
