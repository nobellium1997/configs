;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(package-initialize)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-acario-dark)
;; (setq evil-normal-state-cursor '(box "black")
;;       evil-insert-state-cursor '(bar "black")
;;       evil-visual-state-cursor '(hollow "black"))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Make company faster
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)

;; Set org-agenda-files
(setq org-agenda-files '("/home/nobel/Notes/WorkNotes"))

;; Function for opening tmux in the current directory/project
(defun open-term ()
  (interactive)
  (if (not (string= (projectile-project-name) "-"))
      (call-process "tmux" nil 0 nil "new-window" "-c" (projectile-project-root) "-n" (projectile-project-name))
      (call-process "tmux" nil 0 nil "new-window" "-c" default-directory "-n" "Emacs")))
  ;(call-process "tmux" nil 0 nil "kill-window" "-a"))

(map! :leader "o e" 'ansi-term)
(map! :leader "t z" 'centered-window-mode)

;; Set neo-window to be bigger
(setq neo-window-fixed-size nil)
(setq neo-window-width 45)

;; Custom file open locations
(defun goto-notes ()
  (interactive)
  (counsel-find-file "~/Notes"))
(map! :leader "f n" 'goto-notes)

(defun goto-home ()
  (interactive)
  (counsel-find-file "~"))
(map! :leader "f h" 'goto-home)

(defun goto-rest ()
  (interactive)
  (counsel-find-file "~/Work/Rest"))
(map! :leader "f m" 'goto-rest)

(defun goto-work ()
  (interactive)
  (counsel-find-file "~/Work"))
(map! :leader "f w" 'goto-work)

;; Disable company mode in the following modes
(defun jpk/eshell-mode-hook ()
  (company-mode -1))
(add-hook 'eshell-mode-hook 'jpk/eshell-mode-hook)
(add-hook 'org-mode-hook 'jpk/eshell-mode-hook)

;; Set vterm default shell
;(setq vterm-shell "/usr/bin/fish")

;; Force so-long mode for certain file types
(add-to-list 'auto-mode-alist (cons (rx ".html" eos) 'so-long-mode))
;; (add-to-list 'auto-mode-alist (cons (rx ".json" eos) 'so-long-mode))

;; Dired mappings
(map! :leader "f d" 'fd-dired)

;; Magit blame style
(setq magit-blame-echo-style 'margin)

;; Map scrolling to easier to reach keys
(map! :nv "J" 'evil-scroll-line-down)
(map! :nv "K" 'evil-scroll-line-up)
(map! "C-h" '+workspace/switch-left)
(map! "C-l" '+workspace/switch-right)
(map! :nv "U" 'undo-tree-redo)

;; Map jump to <C-i>
(map! :nv "C-i" 'evil-jump-forward)
(map! :nv "C-o" 'evil-jump-backward)

;; Make pop up buffers persist
(setq persp-autokill-buffer-on-remove nil)

;; Add xml-mode to csproj files
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . xml-mode))

;; Set modeline color
(set-face-background 'mode-line "#332E42")

;; Set realtive line numbers
(setq display-line-numbers-type 'relative)
