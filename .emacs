;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If you build a man a fire, you keep him warm for one night. But if you light a man on fire: you keep him warm for the rest of his life. ;;
;;     -- Terry Pratchett                                                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs takes a lifetime to learn: so the sooner you start, the longer it will take. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Personal emacs configuration of Alex Larsen (kingsfoil) - using with emacs 30.1 compiled on Linux (as of 2026)

;; -----------------------------------------------------------------------------------------

;; All color configuration in this file designed to be used with the iTerm theme "Sea Shells" found here:
;; https://github.com/mbadolato/iTerm2-Color-Schemes#seashells
;; (though I'm on kitty/linux these days so all recent configurations reflect that, where applicable)
;; TODO: turn this into a custom theme file https://emacsfodder.github.io/emacs-theme-editor/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp cons cell review                                            ;;
;; car => head                                                      ;;
;; cons => construct linked list element                            ;;
;; cdr => tail (rest)                                               ;;
;;                                                                  ;;
;; (cons 1 2) => produces a cons cell with head:1 tail:2            ;;
;; '(1 . 2)   => syntactic sugar for the same thing (`.` => `cons`) ;;
;;                                                                  ;;
;; '(...)     => list, not function call                            ;;
;; (...)      => function call                                      ;;
;; '(1 2 3)   => linked list (cons cells) of (1 . (2 . (3 . nil)))  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; General package import reference:                                                                 ;;
;; (use-package package-name                                                                            ;;
;;   :defer t ;; explicitly state that the editor should load before the package (faster startup times) ;;
;;   :bind (                                                                                            ;;
;;          ("C-c f" . (s-expr)) ;; bind these keys on package load                                     ;;
;;          )                                                                                           ;;
;;   :config ;; call these functions on package load (at least I think that's when they're called)      ;;
;;   (s-expr)                                                                                           ;;
;;   (another s-expr)                                                                                   ;;
;;   (etc.)                                                                                             ;;
;;   )                                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; What in the name of sanity is going on with completion?                                        ;;
;; - vertico: strictly minibuffer completion UI that handles display of candidates                   ;;
;; - corfu: strictly at-point completion UI; vertico for the middle of the buffer                    ;;
;; - orderless: works for both, but focused just on how candidates are filtered/sorted given a query ;;
;; - dabbrev: sources completions for the completion-at-point system                                 ;;
;; - cape: special completion-at-point functions (“capf”)                                            ;;
;;   (courtesy of the derk of vars)                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tips for myself:
;; - `C-h m`               => show all major/minor modes and their simple help pages
;; - `C-h b`               => show all current key bindings
;; - `C-h ?` or `M-x help` => open help buffer to explore
;; - `C-h o`               => describe symbol at point (use this when you don't know what an elisp thing is doing)

;; --- TODO: Read these later ---
;; - https://systemcrafters.net/emacs-from-scratch/the-best-default-settings/
;; - https://karthinks.com/software/batteries-included-with-emacs/
;; - https://eshelyaron.com/posts/2023-11-17-completion-preview-in-emacs.html
;; - https://github.com/company-mode/company-mode/wiki/Writing-backends
;; - https://emacsredux.com/blog/2026/03/03/expreg-expand-region-reborn/

;; ----------------------------- TODO: Look into these later ----------------------------- ;
;;
;; * RG/FZF/BAT Integration
;;   - Try fzf with bat previews (--preview 'bat --color=always --style=numbers {}')
;;   - Configure ripgrep (rg) with project-wide search
;;   - Simple bat integration for quick file previews
;;
;; * Navigation Enhancements
;;   - Avy for faster in-buffer movement
;;   - Citre/CTags for jumping to definitions
;;   - ace-window for faster window navigation
;;   - treemacs for project structure visualization
;;
;; * Code Intelligence
;;   - tree-sitter for better syntax highlighting
;;   - dtrt-indent for smart indentation detection
;;   - embark+consult-ripgrep (embark pulls up a menu buffer and you can edit matches directly in that buffer, Ashton showed me Jan 2026)
;;
;; * Documentation Tools
;;   - Quick documentation lookup for symbols under cursor
;;   - Enhanced eldoc integration
;;
;; * Non-Intrusive Completion
;;   - Ghost text completion (like zsh's) => use `preview-completion-mode` (done)
;;   - Lightweight alternative to heavy LSP-based solutions
;;
;; * Visual Enhancements
;;   - rainbow-delimiters for bracket highlighting (done)
;;   - beacon to flash cursor on movement (Ashton suggests Pulsar instead)
;;   - xterm-color for proper ANSI colors in shell output
;;   - vundo for visual undo tree
;;   - color-identifiers-mode for variable highlighting
;;
;; * Productivity Boosters
;;   - One-key project dashboard
;;   - Snippet expansion showcases
;;
;; Remember: Focus on terminal compatibility, speed, and minimal resource usage. Editor should get out of the way.

; ----------------------------- Meta ----------------------------- ;
(setq-default indent-tabs-mode nil) ;; never use tabs. More trouble than they're worth.
(menu-bar-mode -1)                  ;;  (Turn off the top menu bar. No one has ever used it in the history of `emacs -nw`)
(setq vc-follow-symlinks t)
(setq inhibit-startup-screen t)     ;; don't load the startup screen (should open the scratch buffer instead)

;; Get selection/copy/paste in order
(xterm-mouse-mode 1)             ;; let emacs decide about where cursor highlights should go with split buffers (emacs highlight region not terminal)
(setq select-enable-clipboard t) ;; kill/yank to/from the system clipboard instead of a separate kill ring (at least for now, I may go back)
(setq select-enable-primary t)   ;; sync with the X11 primary selection (i.e. the system-wide clipboard)
;; ^^^ to actually enforce the above behavior, enable xclip-mode vvv
;; (require 'xclip)
;; (xclip-mode 1)
;; (turn-on-xclip)


;; Completion preview mode (the best complete)
(global-completion-preview-mode t)
(bind-key* "M-n" 'completion-preview-next-candidate)
(bind-key* "M-p" 'completion-preview-prev-candidate)

; ----------------------------- Packages/Modes (be sure to say what they do!) ----------------------------- ;
(setq use-package-always-ensure t) ;; treat everyting like it has `:ensure t` set

(with-eval-after-load 'package ;; use melpa
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(setq byte-compile-warnings '(not obsolete)) ;; warnings? where we're going we don't need *warnings*
(setq warning-suppress-log-types '((comp) (bytecomp)))

;; Let emacs and xclip talk to one another TODO FIXME
;; (use-package xclip
;;   :config (xclip-mode 1))
;; Notes:
;;   - xclip.el (on emacs wiki)

;; use all the icons! (pretty icons)
;; (use-package all-the-icons
;;   :if (display-graphic-p))
;; Notes:
;;   https://github.com/domtronn/all-the-icons.el


;; Orderless: powerful completion style 
;; Use this to make consult-line match anything anywhere on the line fuzzily
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))
;; Note:
;;   Makes Orderless Fuzzy
;;   TODO: Go look at configs for this. Sometimes it's a bit insensible

;; Lua Major Mode
(use-package lua-mode)
;; Note:
;;   https://github.com/immerrr/lua-mode

;; Consult: Search and navigate via completing-read
(use-package consult
  :ensure t
  :bind (;; Drop-in replacements
         ("C-x b" . consult-buffer)     ; orig. switch-to-buffery
         ("M-y"   . consult-yank-pop)   ; orig. yank-pop
         ;; Searching
         ("C-c r" . consult-ripgrep)    ; ripgrep through files and use consult-line to walk through them. Very nice.
         ("C-s" . consult-line)         ; replace default search binding with consult line, which searches in mini-buffer in a fancy way
         ;;("M-f" . consult-imenu)        ; in supported major modes, use the consult minibuffer to hop between definitions (like functions)
         ("C-x r b" . consult-bookmark-narrow)
         ))
;; Notes:
;;   https://github.com/minad/consult

;; Code Folding (I blame the Advanstaff kronos_employee_sync.pm module for this one. Grrrr)
(use-package origami
  :defer t)
;; Notes:
;;  

;; Mini map (useful for egregiously long nested files)
(use-package demap
  :defer t

  :bind (
         ("M-m" . demap-toggle)
         )

  :config
  (face-spec-set 'demap-minimap-font-face
                 `((t   :inherit    unspecified
                        :family     "minimap"
                        :height     10)))
  (setq demap-minimap-window-side  'left)
  (setq demap-minimap-window-width 15)
  )
;; Notes:
;;   https://github.com/emacsmirror/demap

;; Major mode for Terraform/OpenTofu
(use-package terraform-mode)
;; Notes:
;;   https://github.com/hcl-emacs/terraform-mode

;; Major mode for the pollen templating system (racket)
(use-package pollen-mode
  :defer t)
;; Notes:
;;   https://docs.racket-lang.org/pollen/

;; VERTical Interactive COmpletion (Vertico provides a vertical completion UI based on the default completion system)
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

;; Edit files remotely over ssh
(use-package tramp)
;; Notes:
;;  In the immortal words of the windmill: "I'm not a huge fan." Purportedly there are other solutions to this, worth checking out.

;; COmpletion in Region FUnction 
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
  :ensure t
  :init
  (global-corfu-mode))
;; Notes:
;;   - Does autocompletion in that ugly little popup that I still need to customize the colors for.
;;   - TODO: see if there's not a better way to integrate it with (completion-preview-mode)
;;   https://github.com/minad/corfu

;; Corfu popup on terminal (TODO: re-evaluate, might be redundant with completion-preview)
(use-package
  corfu-terminal
  :ensure t
  :config 
  (corfu-terminal-mode +1)
  )
;; Notes:
;;   - https://codeberg.org/akib/emacs-corfu-terminal

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

;; Completion At Point Extensions which can be used in combination with Corfu, Company etc.
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

;; Easy for global overrides of default keys. The current keymap will be reorganized into the first galactic empire.
(use-package bind-key) ;; We're gonna need it for the remainder of the file.

;; Notes:
;;   https://github.com/priyadarshan/bind-key
;;   Use bind-key* to override everything inc. minor mode rebindings.

;; Use olivetti as a minor mode for writing
(use-package olivetti :ensure t)

;; Notes:
;;   https://github.com/rnkn/olivetti
;;   Emacs minor mode to automatically balance window margins

;; Hide and show scopes (good for deeply nested codebases)
;; (hs-minor-mode)
;; (bind-key* "M-h" hs-toggle-hiding)
;; Notes:
;;   I enabled this one specifically to deal with the kronos_employee_sync.pm file at Advanstaff in May of 2026. I hate that file.
;;   https://www.nongnu.org/emacsdoc-fr/manuel/hideshow.html <= docs on different ways to use this (good short reference)
;; Update: As of May 2026 this doesn't appear to work for either c-mode, perl-mode, or cperl ode. But *does* work for elisp. Grrr. There are some alternatives to consider here like oragami, search around for them and come back to this as a TODO.

;;; Feebleline: A minimalist mode line
(use-package feebleline)
(feebleline-mode t) ;; turn it on

;; Notes:
;;   https://github.com/tautologyclub/feebleline

;;; Avy: Because moving the cursor around is for plebs.
;;  Avy allows for jumping around inside a file by creating labels at distinict points around a buffer or set of buffers.
;;  An enterprising programmer at this point can jump between those points as a means of rapid navigation


(use-package avy
  :bind (
         ("M-j"   . 'avy-goto-line)
         ("C-M-j" . 'avy-goto-char)
         ;;("C-j"   . 'avy-goto-word-1)
         ("C-j"   . 'avy-goto-char-timer)
  ))

;; (bind-key* "M-j" 'avy-goto-line)
;; (bind-key* "C-M-j" 'avy-goto-char)
;; ;;(bind-key* "C-j" 'avy-goto-word-1)
;; (bind-key* "C-j" 'avy-goto-char-timer)

;; Notes:
;;   https://github.com/abo-abo/avy

;; lsp-mode. Because no one wants fast emacs amirite?
;; (edit: for the reasons I joke about, I don't use a langserver)
;; Lang server support, hooks, etc. go here.
;; (use-package lsp-mode) 


;;; Pathologically Eclectic Rubbish Lister <3
;; Use cperl-mode instead of plain perl-mode
(add-to-list 'auto-mode-alist '("\\.pm\\'" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pl\\'" . cperl-mode))
;; Notes:
;;   - The beginning
;;   - https://xkcd.com/519/ (perhaps the most specifically relatable xkcd for me)

;;; Rust!
(use-package rust-mode)

;; Notes:
;;  I used to have the whole uncommented rustic mode thing working but it was driving me up the wall.
;; TODO FIXME

 ;; (use-package rustic
 ;;  :ensure
 ;;  :bind (:map rustic-mode-map
 ;;              ;("M-j" . lsp-ui-imenu)
 ;;              ("M-?" . lsp-find-references)
 ;;              ("C-c C-c l" . flycheck-list-errors)
 ;;              ("C-c C-c a" . lsp-execute-code-action)
 ;;              ("C-c C-c r" . lsp-rename)
 ;;              ("C-c C-c q" . lsp-workspace-restart)
 ;;              ("C-c C-c Q" . lsp-workspace-shutdown)
 ;;              ("C-c C-c s" . lsp-rust-analyzer-status)
 ;;              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
 ;;              ("C-c C-c d" . dap-hydra)
 ;;              ("C-c C-c h" . lsp-ui-doc-glance))
 ;;  :config)
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)


  ;; comment to disable rustfmt on save
  ;; (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)

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

;; (use-package lsp-mode
;;   :ensure
;;   :commands lsp
;;   :custom
;;   ;; what to use when checking on-save. "check" is default, using clippy
;;   (lsp-rust-analyzer-cargo-watch-command "clippy")
;;   (lsp-eldoc-render-all t)
;;   (lsp-idle-delay 0.6)
;;   ;; This controls the overlays that display type and other hints inline. Enable
;;   ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
;;   ;; effect on open projects.
;;   (lsp-rust-analyzer-server-display-inlay-hints t)
;;   (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
;;   (lsp-rust-analyzer-display-chaining-hints t)
;;   (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
;;   (lsp-rust-analyzer-display-closure-return-type-hints t)
;;   (lsp-rust-analyzer-display-parameter-hints nil)
;;   (lsp-rust-analyzer-display-reborrow-hints nil)
;;   :config
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; Notes:
;; The rust stuff here was cribbed from: https://github.com/rksm/emacs-rust-config/blob/master/init.el
;; which is what is built in the tutorial: https://robert.kra.hn/posts/rust-emacs-setup/
;;   https://github.com/emacs-lsp/lsp-mode/
;;   https://emacs-lsp.github.io/lsp-mode/

;;; Just mode (for Justfiles)
(use-package just-mode)
;; Notes:
;;   https://github.com/leon-barrett/just-mode.el

;; go-mode
(use-package go-mode)
;; Notes:
;;   https://github.com/dominikh/go-mode.el?tab=readme-ov-file

;;; Yasnippet
;; My snippet? Yes, Yasnippet.
;;
;; Nice templating system for various languages
(use-package yasnippet)
(yas-global-mode 1)

;; Notes:
;;   https://github.com/joaotavora/yasnippet

;;; Rainbow Delimiters
(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode) ;; use in programming modes
;; Notes:
;;   This one highlights parens etc. with differing colors
;;   in order to make it more apparent who matches with whom.

;;; Flycheck
(use-package flycheck)
;; Notes:
;;   https://www.flycheck.org/en/latest/user/installation.html

;;; Elixir
(use-package elixir-mode)
;; Notes:
;;   It's a major mode for Elixir. Duh.

;;; Rust (old)
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

;;; Nushell
(use-package nushell-mode)
;; Notes:
;;   Major mode for nushell scripts. Do I gotta spell it out?

;;; JSX Mode
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


;;; Typescript Mode
(use-package typescript-mode)

;; Notes:
;;   https://github.com/emacs-typescript/typescript.el


;;; Just in Time Spell Checker (jit-spell)
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

;;; Credo
(use-package flycheck-credo)
(eval-after-load 'flycheck
  '(flycheck-credo-setup))
(add-hook 'elixir-mode-hook 'flycheck-mode)
;; Notes:
;;   Use flycheck to report linter errors from credo on the fly
;;   https://github.com/aaronjensen/flycheck-credo

;;; Alchemist.el (not using for the time being)
;; (use-package alchemist)
;; Notes:
;;   https://github.com/tonini/alchemist.el

;;; HCL. Like hydrochloric acid.
(use-package hcl-mode)
;; Notes:
;;   https://github.com/syohex/emacs-hcl-mode

;;; Docker
(use-package dockerfile-mode)
;; Notes:
;;   https://github.com/spotify/dockerfile-mode

;;; GitLab CI YAML files:
(use-package gitlab-ci-mode)

;; Notes:
;;   https://gitlab.com/joewreschnig/gitlab-ci-mode/

;;; Markdown
;;(use-package markdown-preview-eww)
;;(use-package grip-mode)
(use-package markdown-mode)
;; Notes:
;;   - I find most markdown modes to be less than satisfying. TODO: find a better solution here

;;; Fuzzy Finder
(use-package fzf
  :bind
    ;; TODO: set keybinds!
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


;;; Ripgrep. As in, RIP grep, you will have a fond place in our hearts, always. Rust is the future.
;;  -- removed temporarily --
;; Notes:
;;  Did you know the original version of grep was written by Ken Thompson (as in K&R) in PDP-11 assembly? Legendary.

;;; It would be *dumb* not to *jump* around with ripgrep.
(use-package dumb-jump)
;; Notes:
;;  https://github.com/jacktasia/dumb-jump

;;; Extremely important feature
;; Run a nyan cat with M-x zone-nyan-preview
(use-package zone-nyan)
;; Notes:
;;  https://github.com/emacsmirror/zone-nyan

;;; Vue
(use-package vue-mode)

(add-hook 'mmm-mode-hook ;; mmm-mode comes with the ugliest background colors known to the human race.
          (lambda ()     ;; turn them off.
            (set-face-background 'mmm-default-submode-face nil)))
;; Notes:
;;   https://github.com/AdamNiederer/vue-mode

;; (use-package python-mode)

;; (use-package bluetooth)

;;; dtrt-indent: guess indentation levels based off the current file
(use-package dtrt-indent)
(dtrt-indent-global-mode 1)
;; Notes:
;;   https://github.com/jscheid/dtrt-indent


;;; Evil Mode TODO: give a more serious attempt at exploring this someday
;; "Don't be evil." We can use this motto here because it is not otherwise in use. Ahem.
;;(use-package evil)

;; Enable Evil
;;(require 'evil)
;;(evil-mode 1)

;; Notes:
;;  https://github.com/emacs-evil/evil

; ----------------------------- Custom keybindings: ----------------------------- ;

;; Prefer `bind-key` over `global-set-key`, because... it hates me?
(bind-key* (kbd "M-e e") #'(lambda () (find-file "~/.emacs"))) ;; quickly open the config file.
 
;; Madness??? THIS. IS. EMACS!

; ----------------------------- CUSTOM VARS ----------------------------- ;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(achievements-mode t)
 '(custom-safe-themes
   '("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279"
     "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d"
     "60940e1f2fa3f4e61e7a7ed9bab9c22676aa25f927d5915c8f0fa3a8bf529821"
     "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
     default))
 '(package-selected-packages nil)
 '(sml/mode-width (if (eq (powerline-current-separator) 'arrow) 'right 'full))
 '(sml/pos-id-separator
   '("" (:propertize " " face powerline-active1)
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
   '("" (:propertize " " face powerline-active1)
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
   '("" (:propertize " " face sml/global)
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
   '("" (:propertize " " face powerline-active2)
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
 '(tramp-default-method "ssh")
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
 '(region ((t (:extend t :background "#27260c"))))
 '(vertical-border ((t (:foreground "black" :width condensed))))
 '(vertico-current ((t (:extend t :underline t))))
 '(web-mode-block-delimiter-face ((t (:inherit font-lock-preprocessor-face :foreground "brightred"))))
 '(web-mode-builtin-face ((t (:inherit font-lock-builtin-face :foreground "color-105"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "yellow"))))
 '(web-mode-html-tag-face ((t (:foreground "magenta")))))
