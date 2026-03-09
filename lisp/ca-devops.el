;; -*- lexical-binding: t; -*-

(use-package company-terraform)

(use-package kubernetes
  :defer t)
(use-package ssh-tunnels
  :defer t)
(use-package ssh
  :defer t)
(use-package ssh-agency)
(use-package ssh-config-mode)

(use-package terraform-mode)

(use-package terraform-doc)

(use-package docker
  :defer t)
(use-package docker-api
  :defer t)
(use-package docker-cli
  :defer t)
(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package kdl-mode)

(provide 'ca-devops)
