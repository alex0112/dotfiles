;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(menu-bar-mode -1)

;; Package archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
             ("org" . "https://orgmode.org/elpa/")
             ("melpa" . "https://melpa.org/packages/")))
;;("melpa" . "http://melpa.milkbox.net/packages/")))
;;             ("marmalade" . "http://marmalade-repo.org/packages/")))

(global-display-line-numbers-mode)
(mood-line-mode)

;; == Use ivy with company mode ==
(use-package ivy
  :diminish (ivy-mode . "") ; prevents "ivy" from showing up in the mode line
  :ensure t
  :demand
  :bind
   (
    ("M-x" . counsel-M-x)     ;; enable counsel mode (adds regex matching etc)
    ("C-s" . swiper)          ;; show swiper matches using ivy
    ("C-r" . swiper-backward) ;; same but backwards search
   )
  :config
  (ivy-mode 1)
  ;n(ivy-prescient-mode) ;; optional, show most recent selections at top
  (setq ivy-height 5) ;; optional, set the number of suggestions to n
  (setq ivy-use-selectable-prompt t) ;; optional, use user input rather than suggestion where there is not an exact match (e.g. in the subset of another word) 
  )

(use-package company
  :diminish (company-mode . "")
  :ensure t
  :pin melpa
  :config
  (global-company-mode)
  (company-prescient-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package counsel
  :config
  (ivy-configure 'counsel-M-x :initial-input "") ;; optional, ivy will suggest from the beginning of the string (^) by default.  This sets the inital string to empty ("")
  )

(require 'dap-elixir)
(use-package lsp-mode
    :commands lsp
    :ensure t
    :diminish lsp-mode
    :hook
    (elixir-mode . lsp)
    :config
    (setq lsp-clients-elixir-server-executable "/Users/alexlarsen/lsp/elixir-ls-0.5.0/release/language_server.sh"))

(use-package avy
  :ensure t
  :bind
  (("C-c j" . avy-goto-line)
   ("C-M-j" . avy-goto-char)
   ("C-j" . avy-goto-line)
;   ("C-J" . avy-goto-word-0)
;   ("C-x j" . avy-goto-char)
;   ("C-x J" . avy-goto-char-2)
   ))

(use-package    feebleline
  :ensure       t
  :config       (setq feebleline-msg-functions
                      '((feebleline-line-number         :post "" :fmt "%5s")
                        (feebleline-column-number       :pre ":" :fmt "%-2s")
                        (feebleline-file-directory      :face feebleline-dir-face :post "")
                        (feebleline-file-or-buffer-name :face font-lock-keyword-face :post "")
                        (feebleline-file-modified-star  :face font-lock-warning-face :post "")
                        (feebleline-git-branch          :face feebleline-git-face :pre " : ")
                        (feebleline-project-name        :align right)))
  (setq feebleline-use-legacy-settings t)
  (feebleline-mode 1))



;; ;; Custom Keybindings
;; (define-key global-map (kbd "C-x C-r") 'revert-buffer)


;; ;; == Do not edit by hand ==
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    (quote
;;     (js-react-redux-yasnippets rjsx-mode company-lsp company-prescient show-css rust-mode avy elfeed elixir-mode lsp-elixir lsp-ivy lsp-mode poly-markdown jekyll-modes ivy-prescient use-package markdown-preview-mode markdown-mode+ counsel-spotify counsel))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(achievements-mode t)
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "60940e1f2fa3f4e61e7a7ed9bab9c22676aa25f927d5915c8f0fa3a8bf529821" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(feebleline-mode t nil (feebleline))
 '(package-selected-packages
   (quote
    (rust-mode smooth-scroll smooth-scrolling sublimity doom-modeline zone-rainbow zone-nyan yasnippet writeroom-mode writegood-mode web-narrow-mode vterm use-package smbc smart-mode-line-powerline-theme smart-mode-line-atom-one-dark-theme selectric-mode reveal-in-osx-finder restart-emacs rainbow-fart php-mode nyan-mode multi-web-mode mood-line memento-mori markdown-preview-mode markdown-mode+ make-color major-mode-icons major-mode-hydra lsp-ui lsp-ivy ivy-prescient ivy-emoji hide-mode-line helm haskell-mode graphql-mode graphql gitignore-mode flymake-elixir flymake-easy flycheck-elixir feebleline exunit elixir-mode darkroom dap-mode counsel company-prescient company-lsp company-box all-the-icons-ivy-rich all-the-icons-ivy achievements 0blayout)))
 '(sml/mode-width
   (if
       (eq
	(powerline-current-separator)
	(quote arrow))
       (quote right)
     (quote full)))
 '(sml/pos-id-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
		  (quote display)
		  (funcall
		   (intern
		    (format "powerline-%s-%s"
			    (powerline-current-separator)
			    (car powerline-default-separator-dir)))
		   (quote powerline-active1)
		   (quote powerline-active2))))
     (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
		  (quote display)
		  (funcall
		   (intern
		    (format "powerline-%s-%s"
			    (powerline-current-separator)
			    (cdr powerline-default-separator-dir)))
		   (quote powerline-active1)
		   (quote sml/global))))
     (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
    (""
     (:propertize " " face sml/global)
     (:eval
      (propertize " "
		  (quote display)
		  (funcall
		   (intern
		    (format "powerline-%s-%s"
			    (powerline-current-separator)
			    (car powerline-default-separator-dir)))
		   (quote sml/global)
		   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " "
		  (quote display)
		  (funcall
		   (intern
		    (format "powerline-%s-%s"
			    (powerline-current-separator)
			    (cdr powerline-default-separator-dir)))
		   (quote powerline-active2)
		   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes)))
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
 '(font-lock-comment-face ((t (:foreground "red"))))
 '(font-lock-function-name-face ((t (:foreground "yellow"))))
 '(font-lock-keyword-face ((t (:foreground "brightblue"))))
 '(font-lock-string-face ((t (:foreground "color-28"))))
 '(font-lock-type-face ((t (:foreground "color-27"))))
 '(font-lock-variable-name-face ((t (:foreground "yellow"))))
 '(highlight ((t (:background "color-17"))))
 '(isearch ((t (:background "color-16" :foreground "color-46"))))
 '(isearch-fail ((t (:background "color-16" :foreground "color-124"))))
 '(ivy-minibuffer-match-face-1 ((t (:background "color-136"))))
 '(lazy-highlight ((t (:background "color-34"))))
 '(menu ((t (:background "brightyellow" :foreground "color-16" :inverse-video t))))
 '(minibuffer-prompt ((t (:foreground "magenta"))))
 '(mode-line ((t (:background "grey75" :foreground "black" :height 0.9))))
 '(mood-line-status-info ((t (:inherit default :foreground "color-27"))))
 '(package-description ((t (:foreground "brightwhite" :slant italic))))
 '(package-name ((t (:foreground "brightblue" :underline nil))))
 '(package-status-available ((t (:foreground "yellow"))))
 '(package-status-installed ((t (:inherit nil :foreground "color-41" :underline (:color "color-41" :style wave)))))
 '(vertical-border ((t (:foreground "black" :width condensed))))
 '(web-mode-block-delimiter-face ((t (:inherit font-lock-preprocessor-face :foreground "brightred"))))
 '(web-mode-builtin-face ((t (:inherit font-lock-builtin-face :foreground "color-105"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "yellow"))))
 '(web-mode-html-tag-face ((t (:foreground "magenta")))))
