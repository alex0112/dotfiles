;; Personal emacs configuration of Alex Larsen (kingsfoil)
;;
;; All color configuration in this file designed to be used with the iTerm theme "Sea Shells" found here:
;; https://github.com/mbadolato/iTerm2-Color-Schemes#seashells
;
;; If you build a man a fire, you keep him warm for one night. But if you light a man on fire: you keep him warm for the rest of his life.
;;    -- Terry Pratchett

; ----------------------------- Meta ----------------------------- ;
(setq-default indent-tabs-mode nil) ;; never use tabs. More trouble than they're worth.
(menu-bar-mode -1)                  ;;  (Turn off the top menu bar. No one has ever used it in the history of emacs -nw)
(setq vc-follow-symlinks t)

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

;; Use the use package macro when calling (use-package ...)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Notes:
;; Since you always forget: https://jeffkreeftmeijer.com/emacs-straight-use-package
;;
;; call `(straight-pull-recipe-repositories)` on occasion (or whenever this init file is run on a new system)
;; This will ensure that we've fetched melpa, the emacs package mirror etc.

; ----------------------------- Package Listings (be sure to say what they do!) ----------------------------- ;


(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
)

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-t" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("M-i" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

;; Orderless: powerful completion style
;; Very optional---you can get fzf like completions----config can be
;; extensive (of course---it's Emacs)
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

;; Consult: Misc. enhanced commands
(use-package consult
  :ensure t
  :bind (
         ;; Drop-in replacements
         ("C-x b" . consult-buffer)     ; orig. switch-to-buffer
         ("M-y"   . consult-yank-pop)   ; orig. yank-pop
         ;; Searching
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)       ; Alternative: rebind C-s to use
         ("M-s s" . consult-line)       ; consult-line instead of isearch, bind
         ("M-s L" . consult-line-multi) ; isearch to M-s s
         ("M-s o" . consult-outline)
         ;; Isearch integration
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
         ("M-s l" . consult-line)            ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)      ; needed by consult-line to detect isearch
         )
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

(use-package embark
  :ensure t
  :demand t
  :after avy
  :bind (("C-c a" . embark-act))        ; bind this to an easy key to hit
  :init
  ;; Add the option to run embark when using avy
  (defun bedrock/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark))

(use-package embark-consult
  :ensure t)

;; Easy for global overrides of default keys. The current keymap will be reorganized into the first galactic empire.
(straight-use-package 'bind-key) ;; We're gonna need it for the remainder of the file.

;; Notes:
;;   https://github.com/priyadarshan/bind-key
;;   Use bind-key* to override everything inc. minor mode rebindings.

;; Use olivetti as a minor mode for writing
(straight-use-package 'olivetti)

;; Notes:
;;   https://github.com/rnkn/olivetti
;;   Emacs minor mode to automatically balance window margins


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
;; (straight-use-package 'lsp-mode)

 (use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ;("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config)
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)


  ;; comment to disable rustfmt on save
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for rust-analyzer integration

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, using clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
  ;; effect on open projects.
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; Notes:
;; The rust stuff here was cribbed from: https://github.com/rksm/emacs-rust-config/blob/master/init.el
;; which is what is built in the tutorial: https://robert.kra.hn/posts/rust-emacs-setup/
;;   https://github.com/emacs-lsp/lsp-mode/
;;   https://emacs-lsp.github.io/lsp-mode/

;; Yasnippet
;; My snippet? Yes, Yasnippet.
;;
;; Nice templating system for various languages
(straight-use-package 'yasnippet)
(yas-global-mode 1)

;; Notes:
;;   https://github.com/joaotavora/yasnippet

;; Rainbow Delimiters
(straight-use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode) ;; use in programming modes

;; Notes:
;;   This one highlights parens etc. with differing colors
;;   in order to make it more apparent who matches with whom.

;; Flycheck
(straight-use-package 'flycheck)
;; Notes:
;;   https://www.flycheck.org/en/latest/user/installation.html

;; Elixir
(straight-use-package 'elixir-mode)
;; Notes:
;;   It's a major mode for Elixir. Duh.

;; Rust
;; (use-package rustic
;;   :ensure
;;   :bind (:map rustic-mode-map
;;               ("M-j" . lsp-ui-imenu)
;;               ("M-?" . lsp-find-references)
;;               ("C-c C-c l" . flycheck-list-errors)
;;               ("C-c C-c a" . lsp-execute-code-action)
;;               ("C-c C-c r" . lsp-rename)
;;               ("C-c C-c q" . lsp-workspace-restart)
;;               ("C-c C-c Q" . lsp-workspace-shutdown)
;;               ("C-c C-c s" . lsp-rust-analyzer-status)
;;               ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
;;               ("C-c C-c d" . dap-hydra)
;;               ("C-c C-c h" . lsp-ui-doc-glance))
;;   :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  ;; (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

;; (defun rk/rustic-mode-hook ()
;;   ;; so that run C-c C-c C-r works without having to confirm, but don't try to
;;   ;; save rust buffers that are not file visiting. Once
;;   ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
;;   ;; no longer be necessary.
;;   (when buffer-file-name
;;     (setq-local buffer-save-without-query t))
;;   (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for rust-analyzer integration
;; Notes:
;;   See https://github.com/brotzeit/rustic (it's an alternative to rust-mode)
;;   This was recommended by the tutorial here: https://robert.kra.hn/posts/rust-emacs-setup/

;; Nushell
(straight-use-package
 '(nushell-mode :type git :host github :repo "azzamsa/emacs-nushell"))
;; Notes:
;;   Major mode for nushell scripts. Do I gotta spell it out?

;; JSX Mode
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode)) ;; Ensure that we load for typescript as well
(setq js-indent-level 2) ;; Honestly this should be true for all js files. Idk why emacs doesn't have a sane default here.
(use-package rjsx-mode
  :config
  ;;(setq-local indent-line-function 'js-jsx-indent-line) ;; Ignore the package default for indentation. I hate it.

  (define-key rjsx-mode-map "<" nil) ;; Not a huge fan of these either.
  (define-key rjsx-mode-map (kbd "C-d") nil) ;; Seriously just let me type.
  (define-key rjsx-mode-map ">" nil) ;; If I really want magic symbols popping up out of nowhere I have yasnippet
  )
;; Notes:
;;   Sometimes you gotta write react. This is a nice jsx major mode;
;;   https://github.com/felipeochoa/rjsx-mode

;; Just in Time Spell Checker (jit-spell)
;; (use-package jit-spell
;;   :config
;;     (define-key jit-spell-mode-map (kbd "C-m") 'jit-spell-correct-word) ;; bind C-; to spell check
;;     ;; (bind-key* (kbd "C-;") (jit-spell-correct-word))
;;   )
;; (add-hook 'text-mode-hook 'jit-spell-mode)
;; (add-hook 'prog-mode-hook 'jit-spell-mode)
;; Notes:
;;; Doesn't really work. Not using.
;;;  https://github.com/astoff/jit-spell

;; Credo
(straight-use-package 'flycheck-credo)
(eval-after-load 'flycheck
  '(flycheck-credo-setup))
(add-hook 'elixir-mode-hook 'flycheck-mode)
;; Notes:
;;   Use flycheck to report linter errors from credo on the fly
;;   https://github.com/aaronjensen/flycheck-credo

;; Alchemist.el (not using for the time being)
;;;; (straight-use-package 'alchemist)
;; Notes:
;;   https://github.com/tonini/alchemist.el

;; HCL. Like hydrochloric acid.
(straight-use-package 'hcl-mode)
;; Notes:
;;   https://github.com/syohex/emacs-hcl-mode

;; Docker? I 'ardly know 'er!
; -- removed temporarily --
;; Notes:
;;   https://github.com/spotify/dockerfile-mode

;; GitLab CI YAML files:
(straight-use-package 'gitlab-ci-mode)

;; Notes:
;;   https://gitlab.com/joewreschnig/gitlab-ci-mode/

;; Markdown
;;(straight-use-package 'markdown-preview-eww)
(straight-use-package 'grip-mode)
;; Notes:
;;

;; Fuzzy Finder
(use-package fzf
  :bind
    ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))


;; Ripgrep. As in, RIP grep, you will have a fond place in our hearts, always. Rust is the future.
;;  -- removed temporarily --
;; Notes:
;;  Did you know the original version of grep was written by Ken Thompson (as in K&R) in PDP-11 assembly? Legendary.

;; It would be *dumb* not to *jump* around with ripgrep. I mean c'mon.
(straight-use-package 'dumb-jump)
;; Notes:
;;  https://github.com/jacktasia/dumb-jump

;; Because why not?
;; Run a nyan cat with M-x zone-nyan-preview
(use-package zone-nyan)
;; Notes:
;;  https://github.com/emacsmirror/zone-nyan

;; Vue
(use-package vue-mode)

(add-hook 'mmm-mode-hook ;; mmm-mode comes with the ugliest background colors known to the human race.
          (lambda ()     ;; turn them off.
            (set-face-background 'mmm-default-submode-face nil)))
;; Notes:
;;   https://github.com/AdamNiederer/vue-mode

; ----------------------------- Custom keybindings: ----------------------------- ;

;; Prefer `bind-key` over `global-set-key`, because... it hates me?
(bind-key* (kbd "M-e e") (find-file (expand-file-name "~/.emacs"))) ;; quickly open the config file.

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
 '(warning-suppress-types '((comp) (comp)))
 '(window-divider-default-right-width 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-black ((t (:background "cadetblue" :foreground "black"))))
 '(avy-lead-face ((t (:background "red" :foreground "white"))))
 '(avy-lead-face-0 ((t (:background "green" :foreground "white"))))
 '(avy-lead-face-2 ((t (:background "red" :foreground "white"))))
 '(elixir-atom-face ((t (:inherit nil :foreground "deepskyblue"))))
 '(elixir-attribute-face ((t (:inherit (default font-lock-preprocessor-face) :foreground "skyblue1"))))
 '(font-lock-builtin-face ((t (:foreground "dodgerblue1"))))
 '(font-lock-comment-face ((t (:foreground "orangered"))))
 '(font-lock-constant-face ((t (:foreground "indianred"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "green"))))
 '(font-lock-function-name-face ((t (:foreground "yellow"))))
 '(font-lock-keyword-face ((t (:foreground "cornflowerblue"))))
 '(font-lock-string-face ((t (:foreground "green"))))
 '(font-lock-type-face ((t (:foreground "lightskyblue"))))
 '(font-lock-variable-name-face ((t (:foreground "darkorange"))))
 '(highlight ((t (:background "color-17"))))
 '(isearch ((t (:background "color-16" :foreground "color-46"))))
 '(isearch-fail ((t (:background "color-16" :foreground "color-124"))))
 '(ivy-minibuffer-match-face-1 ((t (:background "color-136"))))
 '(lazy-highlight ((t (:background "color-34"))))
 '(lv-separator ((t (:background "darkslategray"))))
 '(markdown-markup-face ((t (:inherit shadow :foreground "mistyrose" :slant normal :weight normal))))
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
 '(region ((t (:extend t :background "gray1"))))
 '(vertical-border ((t (:foreground "black" :width condensed))))
 '(web-mode-block-delimiter-face ((t (:inherit font-lock-preprocessor-face :foreground "brightred"))))
 '(web-mode-builtin-face ((t (:inherit font-lock-builtin-face :foreground "color-105"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "yellow"))))
 '(web-mode-html-tag-face ((t (:foreground "magenta")))))
