;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Package archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
             ("org" . "https://orgmode.org/elpa/")
             ("melpa" . "http://melpa.milkbox.net/packages/")
             ("marmalade" . "http://marmalade-repo.org/packages/")))

;; == Use ivy with company mode ==
(use-package ivy
  ;; :diminish (ivy-mode . "") ; prevents "ivy" from showing up in the mode line
  ;; :ensure t
  :demand
  :bind
   (
    ("M-x" . counsel-M-x)     ;; enable counsel mode (adds regex matching etc)
    ("C-s" . swiper)          ;; show swiper matches using ivy
    ("C-r" . swiper-backward) ;; same but backwards search
   )
  :config
  (ivy-mode 1)
  (ivy-prescient-mode) ;; optional, show most recent selections at top
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

(use-package counsel
  :config
  (ivy-configure 'counsel-M-x :initial-input "") ;; optional, ivy will suggest from the beginning of the string (^) by default.  This sets the inital string to empty ("")
  )

(use-package lsp-mode
  :hook ((elixir-mode . lsp-deferred) (python-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config (setq lsp-clients-elixir-server-executable "~/prog/utils/langservers/elixir-ls/release/language_server.sh")
  :bind (("C-c C-d" . lsp-describe-thing-at-point)) ;; define a custom binding for this mode
  )

(use-package avy
  :ensure t
  :bind
  (("C-c j" . avy-goto-line)
   ("C-M-j" . avy-goto-char)
   ("C-c J" . avy-goto-word-0)
   ("C-x j" . avy-goto-char)
   ("C-x J" . avy-goto-char-2)
   ))


;; Custom Keybindings
(define-key global-map (kbd "C-x C-r") 'revert-buffer)




;; == Do not edit by hand ==
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (js-react-redux-yasnippets rjsx-mode company-lsp company-prescient show-css rust-mode avy elfeed elixir-mode lsp-elixir lsp-ivy lsp-mode poly-markdown jekyll-modes ivy-prescient use-package markdown-preview-mode markdown-mode+ counsel-spotify counsel))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
