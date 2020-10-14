(set-frame-font "-CTDB-Fira Code-normal-normal-normal-*-13-*-*-*-d-0-iso10646-1")

(setq wakatime-api-key "24d12bde-c72c-4ca8-bf97-c2964aa0d2d2")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-jdk-src-paths '("/usr/lib/jvm/java-11-openjdk/lib/src.zip"))
 '(custom-safe-themes
   '("f490984d405f1a97418a92f478218b8e4bcc188cf353e5dd5d5acd2f8efd0790" "35c096aa0975d104688a9e59e28860f5af6bb4459fd692ed47557727848e6dfe" "2d035eb93f92384d11f18ed00930e5cc9964281915689fa035719cab71766a15" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "24714e2cb4a9d6ec1335de295966906474fdb668429549416ed8636196cb1441" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "4980e5ddaae985e4bae004280bd343721271ebb28f22b3e3b2427443e748cd3f" "eb0a314ac9f75a2bf6ed53563b5d28b563eeba938f8433f6d1db781a47da1366" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(helm-completion-style 'emacs)
 '(package-selected-packages
   '(wordnut define-word synosaurus rfc-mode wc-mode browse-at-remote docker-compose-mode javadoc-lookup writeroom-mode writegood-mode muse dumb-jump wakatime-mode lsp-ui cargo toml-mode company-racer racer rust-mode yari rubocop robe helm-robe which-key yafolding yasnippet-snippets use-package typo tern-auto-complete tern terraform-mode sx solarized-theme slime smart-mode-line-powerline-theme skewer-mode rbenv ruby-electric racket-mode psc-ide pytest purescript-mode protobuf-mode poet-theme outline-magic olivetti ob-typescript ob-sql-mode ob-rust ob-prolog ob-lfe ob-ipython ob-http ob-go ob-diagrams ob-elixir magit-gitflow magit-annex kibit-helper kubernetes jinja2-mode intero impatient-mode highlight-indent-guides highlight-blocks hindent helm-hoogle helm flycheck-swift flycheck-stack flycheck-rust flycheck-perl6 flycheck-ocaml flycheck-mix flycheck-joker flycheck-ghcmod flycheck-clj-kondo flycheck-cask fancy-narrow expand-region emamux ejc-sql dracula-theme docker diff-hl company-web company-erlang company-dict company-cabal color-theme-solarized clojure-mode-extra-font-locking cljsbuild-mode cljr-helm auto-highlight-symbol ansible-vault ansible-doc alchemist zenburn-theme yaml-mode web-mode virtualenvwrapper virtualenv tuareg textile-mode sos smartparens smart-mode-line sly-company sly rinari rebox2 rainbow-mode rainbow-delimiters puppet-mode psvn powerline pony-mode pomodoro php-mode persistent-scratch paradox p4 ox-reveal org-present org-jekyll org-gcal org-bullets offlineimap notmuch noctilux-theme nix-mode nginx-mode monky minimap memory-usage markdown-mode malabar-mode magit log4j-mode less-css-mode ledger-mode kanban js2-mode js-comint jedi inf-clojure indent-guide htmlize hi2 heroku helm-swoop helm-projectile helm-spotify helm-make helm-google helm-gitlab helm-git-files helm-git helm-ghc helm-flyspell helm-flycheck helm-dired-recent-dirs helm-dired-history helm-company helm-clojuredocs helm-chrome helm-cider helm-aws helm-ag hackernews groovy-mode graphviz-dot-mode google-translate google-contacts golint gitlab git-gutter git-commit git-annex gitconfig gist flycheck-mypy flycheck-purescript flycheck-pos-tip flycheck-pony flycheck-haskell flycheck-elm flycheck-elixir flycheck-cython flycheck-clojure flycheck find-file-in-repository feature-mode evil eval-in-repl ess erlang ensime emms emmet-mode elm-yasnippets elm-mode elixir-yasnippets elixir-mode elein ein edit-server ebib dpaste dockerfile-mode dired-details dired+ d-mode cython-mode csv-mode csharp-mode company-shell company-restclient company-lua company-jedi company-inf-ruby company-go company-ghci company-ghc company-ansible company color-theme color-moccur cmake-mode clojure-cheatsheet clj-refactor cider-spy cider-profile cider-eval-sexp-fu cider-decompile cider cask-mode cask c-eldoc browse-kill-ring bookmark+ bitlbee beacon batch-mode autopair auctex ascii arduino-mode apache-mode ansible android-mode ag adoc-mode ack 4clojure))
 '(paradox-github-token t)
 '(safe-local-variable-values
   '((cider-cljs-repl-types
      (edge "(do (require 'dev-extras) ((resolve 'dev-extras/cljs-repl)))"))
     (cider-repl-init-code "(dev)")
     (cider-ns-refresh-after-fn . "dev-extras/resume")
     (cider-ns-refresh-before-fn . "dev-extras/suspend")
     (cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")
     (ca-cleanup-is-enabled . t)
     (ruby-compilation-executable . "ruby")
     (ruby-compilation-executable . "ruby1.8")
     (ruby-compilation-executable . "ruby1.9")
     (ruby-compilation-executable . "rbx")
     (ruby-compilation-executable . "jruby")))
 '(smartparens-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#DCDCCC" :background "#3F3F3F")))))
