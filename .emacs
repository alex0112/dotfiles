;; Personal emacs configuration of Alex Larsen (kingsfoil)
;;
;; All color configuration in this file designed to be used with the iTerm theme "Sea Shells" found here:
;; https://github.com/mbadolato/iTerm2-Color-Schemes#seashells
;;
;; If you build a man a fire, you keep him warm for one night. But if you light a man on fire: you keep him warm for the rest of his life.
;;    -- Terry Pratchett

; ----------------------------- Meta ----------------------------- ;
(setq-default indent-tabs-mode nil) ;; never use tabs. More trouble than they're worth.
(menu-bar-mode -1)                  ;;  (Turn off the top menu bar. No one has ever used it in the history of emacs -nw)

; ----------------------------- Use straight.el for package management ----------------------------- ;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Notes:
;; call `(straight-pull-recipe-repositories)` on occasion (or whenever this init file is run on a new system)
;; This will ensure that we've fetched melpa, the emacs package mirror etc.


; ----------------------------- Package Listings (be sure to say what they do!) ----------------------------- ;

;; Easy for global overrides of default keys. The current keymap will be reorganized into the first galactic empire.
(straight-use-package 'bind-key) ;; We're gonna need it for the remainder of the file.

;; Notes:
;;   https://github.com/priyadarshan/bind-key
;;   Use bind-key* to override everything inc. minor mode rebindings.

;; A minimalist mode line
(straight-use-package 'feebleline)
(feebleline-mode t)

;; Notes:
;;   https://github.com/tautologyclub/feebleline

;; Avy: Because moving the cursor around is for plebs.
;;  Avy allows for jumping around inside a file by creating labels at distinict points around a buffer or set of buffers.
;;  An enterprising programmer at this point can jump between those points as a means of rapid navigation
(straight-use-package 'avy)

(bind-key* "M-j" 'avy-goto-line)
(bind-key* "C-M-j" 'avy-goto-char)
(bind-key* "C-j" 'avy-goto-word-0)

;; Notes:
;;   https://github.com/abo-abo/avy


;; lsp-mode. Because no one wants fast emacs amirite?
;; Lang server support, hooks, etc. go here.

(straight-use-package 'lsp-mode)

;; Notes:
;;   https://github.com/emacs-lsp/lsp-mode/
;;   https://emacs-lsp.github.io/lsp-mode/


;; Yasnippet
;; My snippet? Yes, Yasnippet.
;;
;; Nice templating system for various languages

(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(yas-global-mode 1)

;; Notes:
;;   https://github.com/joaotavora/yasnippet



; ----------------------------- Custom keybindings: ----------------------------- ;

;; Prefer `bind-key` over `global-set-key`, because... it hates me?
(bind-key* (kbd "M-e e") (find-file "~/.emacs")) ;; quickly open the config file.

;; Madness??? THIS. IS. EMACS!


; ----------------------------- CUSTOM VARS ----------------------------- ;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(achievements-mode t)
 '(custom-safe-themes
   '("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "60940e1f2fa3f4e61e7a7ed9bab9c22676aa25f927d5915c8f0fa3a8bf529821" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(package-selected-packages
   '(dumb-jump flycheck-credo company-jedi minimap web-mode realgud wc-goal-mode wc-mode rust-mode smooth-scroll smooth-scrolling sublimity doom-modeline zone-rainbow zone-nyan yasnippet writeroom-mode writegood-mode web-narrow-mode vterm use-package smbc smart-mode-line-powerline-theme smart-mode-line-atom-one-dark-theme selectric-mode reveal-in-osx-finder restart-emacs rainbow-fart php-mode nyan-mode multi-web-mode mood-line memento-mori markdown-preview-mode markdown-mode+ make-color major-mode-icons major-mode-hydra lsp-ui lsp-ivy ivy-prescient ivy-emoji hide-mode-line helm haskell-mode graphql-mode graphql gitignore-mode flymake-elixir flymake-easy flycheck-elixir exunit elixir-mode darkroom dap-mode counsel company-prescient company-lsp company-box all-the-icons-ivy-rich all-the-icons-ivy achievements 0blayout))
 '(sml/mode-width (if (eq (powerline-current-separator) 'arrow) 'right 'full))
 '(sml/pos-id-separator
   '(""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " " 'display
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   'powerline-active1 'powerline-active2)))
     (:propertize " " face powerline-active2)))
 '(sml/pos-minor-modes-separator
   '(""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " " 'display
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   'powerline-active1 'sml/global)))
     (:propertize " " face sml/global)))
 '(sml/pre-id-separator
   '(""
     (:propertize " " face sml/global)
     (:eval
      (propertize " " 'display
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   'sml/global 'powerline-active1)))
     (:propertize " " face powerline-active1)))
 '(sml/pre-minor-modes-separator
   '(""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " " 'display
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   'powerline-active2 'powerline-active1)))
     (:propertize " " face powerline-active1)))
 '(sml/pre-modes-separator (propertize " " 'face 'sml/modes))
 '(window-divider-default-right-width 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:background "red" :foreground "white"))))
 '(avy-lead-face-0 ((t (:background "green" :foreground "white"))))
 '(avy-lead-face-2 ((t (:background "red" :foreground "white"))))
 '(elixir-atom-face ((t (:inherit nil :foreground "brightred"))))
 '(elixir-attribute-face ((t (:inherit (default font-lock-preprocessor-face) :foreground "brightcyan"))))
 '(font-lock-builtin-face ((t (:foreground "brightwhite"))))
 '(font-lock-comment-face ((t (:foreground "color-208"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "color-76"))))
 '(font-lock-function-name-face ((t (:foreground "yellow"))))
 '(font-lock-keyword-face ((t (:foreground "brightblue"))))
 '(font-lock-string-face ((t (:foreground "color-34"))))
 '(font-lock-type-face ((t (:foreground "magenta"))))
 '(font-lock-variable-name-face ((t (:foreground "yellow"))))
 '(highlight ((t (:background "color-17"))))
 '(isearch ((t (:background "color-16" :foreground "color-46"))))
 '(isearch-fail ((t (:background "color-16" :foreground "color-124"))))
 '(ivy-minibuffer-match-face-1 ((t (:background "color-136"))))
 '(lazy-highlight ((t (:background "color-34"))))
 '(markdown-markup-face ((t (:inherit shadow :foreground "brightmagenta" :slant normal :weight normal))))
 '(markdown-pre-face ((t (:inherit (markdown-code-face font-lock-constant-face) :foreground "color-46"))))
 '(menu ((t (:background "brightyellow" :foreground "color-16" :inverse-video t))))
 '(minibuffer-prompt ((t (:foreground "magenta"))))
 '(minimap-active-region-background ((t (:extend t :background "blue"))))
 '(mode-line ((t (:background "grey75" :foreground "black" :height 0.9))))
 '(mood-line-status-info ((t (:inherit default :foreground "color-27"))))
 '(package-description ((t (:foreground "brightwhite" :slant italic))))
 '(package-name ((t (:foreground "brightblue" :underline nil))))
 '(package-status-available ((t (:foreground "yellow"))))
 '(package-status-installed ((t (:inherit nil :foreground "color-41" :underline (:color "color-41" :style wave)))))
 '(region ((t (:extend t :background "black"))))
 '(vertical-border ((t (:foreground "black" :width condensed))))
 '(web-mode-block-delimiter-face ((t (:inherit font-lock-preprocessor-face :foreground "brightred"))))
 '(web-mode-builtin-face ((t (:inherit font-lock-builtin-face :foreground "color-105"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "yellow"))))
 '(web-mode-html-tag-face ((t (:foreground "magenta")))))
