;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(package-initialize)
(load "exwm-edit")

;; (add-hook 'exwm-edit-compose-hook '+word-wrap-mode)

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
(setq company-idle-delay nil)
(setq company-tooltip-limit 5)
(setq company-minimum-prefix-length 3)

;; Set org-agenda-files
(setq org-agenda-files '("/home/nobel/Notes"))

;; No line numbers for extra cool buffers
(setq display-line-numbers-type nil)

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
(map! :map evil-snipe-local-mode-map :vnm "s" nil :vnm "S" nil)
(map! :nv "s" 'avy-goto-char-timer)
(map! :leader :nv "[" 'backward-up-list)
(map! :leader :nv "]" 'down-list)
(map! :nv "[[" '(lambda () (interactive) (jump-to-same-indent -1)))
(map! :nv "]]" 'jump-to-same-indent)
(map! :nvi "C--" 'er/contract-region)
(map! :nv "C-r" 'undo-fu-only-redo)

;; EOL config
(setq evil-move-beyond-eol nil)

;; Dired mappings
(map! :leader "f d" 'fd-dired)

;; Disable smart-parens
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
(remove-hook 'doom-first-input-hook #'which-key-mode)

;; Add xml-mode to csproj files
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . xml-mode))

;; Set default search to google
(setq eww-search-prefix "https://www.google.com/search?q=")

;; EXWM processes
(defun mutemic()
  (interactive)
  (start-process "" nil "/home/nobel/Scripts/mutemic.sh"))

(defun cycle-display()
  (interactive)
  (start-process "" nil "/home/nobel/Scripts/cycle_display.sh"))

(defun cycle-outputs()
  (interactive)
  (start-process "" nil "/home/nobel/Scripts/cycle_outputs.sh"))

(defun flameshot()
  (interactive)
  (start-process "" nil "flameshot" "gui"))

(defun toggle-trackpad()
  (interactive)
  (start-process "" nil "/home/nobel/Scripts/toggle_trackpad.sh"))

(defun toggle-audio()
  (interactive)
  (start-process "" nil "playerctl" "play-pause"))

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun switch-to-firefox ()
  (interactive)
  (switch-to-buffer "google-chrome"))

(defun split-and-focus-left ()
  (interactive)
  (split-window-right)
  (redisplay)
  (windmove-right)
  (+ivy/switch-buffer))

;; EXWM configs
;; Disable menu-bar, tool-bar and scroll-bar to increase the usable space.
(evil-set-initial-state 'exwm-mode 'emacs)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Set char mode by default
(setq exwm-manage-configurations '((t char-mode t)))

;; Also shrink fringes to 1 pixel.
(fringe-mode 1)

(require 'exwm)
(require 'exwm-randr)

(exwm-randr-enable)

(start-process "" nil "/home/nobel/.screenlayout/dual.sh")
(start-process "" nil "polybar" "panel" "-r")

(setq exwm-randr-workspace-monitor-plist '(3 "DP-1" 6 "DP-1" 7 "DP-1" 8 "DP-1" 9 "DP-1" 0 "DP-1"))
(setq mouse-autoselect-window t
      focus-follows-mouse t)

;; Set the initial number of workspaces (they can also be created later).
(setq exwm-workspace-number 10)

;; Allow all buffers in any workspace
(setq exwm-layout-show-all-buffers t)
(setq exwm-workspace-show-all-buffers t)

(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-instance-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

;; These keys should always pass through to Emacs
(setq exwm-input-prefix-keys
      '(?\M-x))

;; Global keybindings can be defined with `exwm-input-global-keys'.
;; Here are a few examples:
(setq exwm-input-global-keys
      `(
        ([?\s-r] . exwm-reset)
        ([?\s-h] . windmove-left)
        ([?\s-l] . windmove-right)
        ([?\s-k] . windmove-up)
        ([?\s-j] . windmove-down)
        ([?\s-v] . split-and-focus-left)
        ([?\s-s] . split-window-below)
        ([?\s-b] . +ivy/switch-buffer)
        ([?\s-w] . mutemic)
        ([?\s-z] . cycle-display)
        ([?\s-x] . cycle-outputs)
        ([?\s-q] . delete-window)
        ([?\s-d] . kill-current-buffer)
        ([?\s-f] . delete-other-windows)
        ([?\s-u] . winner-undo)
        ;; ([?\s-p] . previous-buffer)
        ;; ([?\s-n] . next-buffer)
        ([?\s-a] . flameshot)
        ([?\s-p] . toggle-audio)
        ([?\s-o] . switch-to-last-buffer)
        ([?\s-e] . toggle-trackpad)
        ([?\s-g] . switch-to-firefox)
        ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
        ;; Bind "s-&" to launch applications ('M-&' also works if the output
        ;; buffer does not bother you).
        ([?\s-i] . (lambda (command)
		     (interactive (list (read-shell-command "$ ")))
		     (start-process-shell-command command nil command)))))

;; To add a key binding only available in line-mode, simply define it in
;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

(defun efs/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun efs/send-polybar-exwm-workspace ()
  (efs/send-polybar-hook "exwm-workspace" 1))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'efs/send-polybar-exwm-workspace)

;; Do not forget to enable EXWM. It will start by itself when things are
;; ready.  You can put it _anywhere_ in your configuration.
(exwm-enable)
