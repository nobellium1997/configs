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
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'gruvbox)

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

;; DAP MODE
(require 'dap-python)
(require 'dap-netcore)

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; Custom company mappings
(setq company-idle-delay 0)
(setq company-tooltip-limit 5)
(setq company-minimum-prefix-length 3)

;; Set org-agenda-files
(setq org-agenda-files '("/home/nobel/Notes"))

;; No line numbers for extra cool buffers
(setq display-line-numbers-type nil)

;; Custom evil settings
(setq evil-move-beyond-eol t)

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

(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (or (looking-at-p "[[:space:]]*$") (looking-at-p "^[[:space:]]*[[:ascii:]][[:space:]]*$"))))

(defun jump-to-same-indent (direction)
  (interactive "P")
  (let ((start-indent (current-indentation)))
    (while
      (and (not (bobp))
           (zerop (forward-line (or direction 1)))
           (or (current-line-empty-p)
               (> (current-indentation) start-indent)))))
  (back-to-indentation))

;; Hotkeys
(map! "C-\\" 'er/expand-region)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(map! :map evil-snipe-local-mode-map :vnm "s" nil :vnm "S" nil)
(map! :nv "s" 'avy-goto-char-timer)
(map! :nv "\/" 'swiper)
(map! :nv "?" 'swiper-backward)
(map! :leader :nv "[" 'backward-up-list)
(map! :leader :nv "]" 'down-list)
(map! :nv "[[" '(lambda () (interactive) (jump-to-same-indent -1)))
(map! :nv "]]" 'jump-to-same-indent)
(map! :nvi "C--" 'er/contract-region)
(map! :nv "C-r" 'undo-fu-only-redo)

;; Dired mappings
(map! :leader "f d" 'fd-dired)

;; Magit blame style
(setq magit-blame-echo-style 'margin)

;; Disable smart-parens
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;; Add xml-mode to csproj files
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . xml-mode))

;; Set default search to google
(setq eww-search-prefix "https://www.google.com/search?q=")
