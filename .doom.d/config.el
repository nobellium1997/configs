;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(package-initialize)
(load "exwm-edit")

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
(setq doom-theme 'dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq exwm-edit-split t)

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

;; Custom company mappings
(setq company-dabbrev-downcase 0)
(setq company-idle-delay nil)
(setq company-tooltip-limit 5)

;; Set org-agenda-files
(setq org-agenda-files '("/home/nobel/Notes"))

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

;; Hotkeys
(map! "M-n" 'avy-goto-char-timer)
(map! "C-S-p" 'scroll-down-line)
(map! "C-S-n" 'scroll-up-line)
(map! "C-M-;" 'down-list)
(map! "C-\\" 'er/expand-region)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

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

(defun suspend()
  (interactive)
  (start-process "" nil "systemctl" "suspend"))

(defun toggle-trackpad()
  (interactive)
  (start-process "" nil "/home/nobel/Scripts/toggle_trackpad.sh"))

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

;; Dired mappings
(map! :leader "f d" 'fd-dired)

;; Magit blame style
(setq magit-blame-echo-style 'margin)

;; Add xml-mode to csproj files
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . xml-mode))

;; Set default search to google
(setq eww-search-prefix "https://www.google.com/search?q=")

;; Syntax highlighting eww
(require 'cl-lib)

(defun eww-tag-pre (dom)
  (let ((shr-folding-mode 'none)
        (shr-current-font 'default))
    (shr-ensure-newline)
    (insert (eww-fontify-pre dom))
    (shr-ensure-newline)))

(defun eww-fontify-pre (dom)
  (with-temp-buffer
    (shr-generic dom)
    (let ((mode (eww-buffer-auto-detect-mode)))
      (when mode
        (eww-fontify-buffer mode)))
    (buffer-string)))

(defun eww-fontify-buffer (mode)
  (delay-mode-hooks (funcall mode))
  (font-lock-default-function mode)
  (font-lock-default-fontify-region (point-min)
                                    (point-max)
                                    nil))

(defun eww-buffer-auto-detect-mode ()
  (let* ((map '((ada ada-mode)
                (awk awk-mode)
                (c c-mode)
                (cpp c++-mode)
                (clojure clojure-mode lisp-mode)
                (csharp csharp-mode java-mode)
                (css css-mode)
                (dart dart-mode)
                (delphi delphi-mode)
                (emacslisp emacs-lisp-mode)
                (erlang erlang-mode)
                (fortran fortran-mode)
                (fsharp fsharp-mode)
                (go go-mode)
                (groovy groovy-mode)
                (haskell haskell-mode)
                (html html-mode)
                (java java-mode)
                (javascript javascript-mode)
                (json json-mode javascript-mode)
                (latex latex-mode)
                (lisp lisp-mode)
                (lua lua-mode)
                (matlab matlab-mode octave-mode)
                (objc objc-mode c-mode)
                (perl perl-mode)
                (php php-mode)
                (prolog prolog-mode)
                (python python-mode)
                (r r-mode)
                (ruby ruby-mode)
                (rust rust-mode)
                (scala scala-mode)
                (shell shell-script-mode)
                (smalltalk smalltalk-mode)
                (sql sql-mode)
                (swift swift-mode)
                (visualbasic visual-basic-mode)
                (xml sgml-mode)))
         (language (language-detection-string
                    (buffer-substring-no-properties (point-min) (point-max))))
         (modes (cdr (assoc language map)))
         (mode (cl-loop for mode in modes
                        when (fboundp mode)
                        return mode)))
    (message (format "%s" language))
    (when (fboundp mode)
      mode)))

(setq shr-external-rendering-functions
      '((pre . eww-tag-pre)))

;; EXWM configs
;; Disable menu-bar, tool-bar and scroll-bar to increase the usable space.
;; (evil-set-initial-state 'exwm-mode 'emacs)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Also shrink fringes to 1 pixel.
(fringe-mode 1)

;; Turn on `display-time-mode' if you don't use an external bar.
(setq display-time-default-load-average nil)
(display-time-mode t)
(display-battery-mode t)

;; You are strongly encouraged to enable something like `ido-mode' to alter
;; the default behavior of 'C-x b', or you will take great pains to switch
;; to or back from a floating frame (remember 'C-x 5 o' if you refuse this
;; proposal however).
;; You may also want to call `exwm-config-ido' later (see below).
;; (ido-mode 1)
(require 'exwm)

;; Fix problems with Ido (if you use it).
(require 'exwm-config)
;; (exwm-config-ido)

;; Set the initial number of workspaces (they can also be created later).
(setq exwm-workspace-number 10)

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
      '(?\M-x
        ?\C-c
        ?\C-x
        ?\M-\ ))

;; Global keybindings can be defined with `exwm-input-global-keys'.
;; Here are a few examples:
(setq exwm-input-global-keys
      `(
        ;; Bind "s-r" to exit char-mode and fullscreen mode.
        ([?\s-r] . exwm-reset)
        ([?\s-i] . exwm-input-release-keyboard)
        ;; Bind "s-w" to switch workspace interactively.
        ;; ([?\s-w] . exwm-workspace-switch)
        ;; ([?\s-H] . +evil/window-move-left)
        ;; ([?\s-L] . +evil/window-move-right)
        ;; ([?\s-K] . +evil/window-move-up)
        ;; ([?\s-J] . +evil/window-move-down)
        ([?\s-j] . ace-window)
        ([?\s-v] . split-window-right)
        ([?\s-s] . split-window-below)
        ([?\s-b] . ido-switch-buffer)
        ([?\s-w] . mutemic)
        ([?\s-z] . cycle-display)
        ([?\s-x] . cycle-outputs)
        ([?\s-q] . delete-window)
        ([?\s-d] . kill-current-buffer)
        ([?\s-f] . delete-other-windows)
        ([?\s-u] . winner-undo)
        ([?\s-p] . previous-buffer)
        ([?\s-n] . next-buffer)
        ([?\s-a] . flameshot)
        ([?\s-g] . suspend)
        ([?\s-h] . switch-to-last-buffer)
        ([?\s-T] . toggle-trackpad)
        ([?\s-t] . +term/toggle)
        ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
        ;; Bind "s-&" to launch applications ('M-&' also works if the output
        ;; buffer does not bother you).
        ([?\s-&] . (lambda (command)
		     (interactive (list (read-shell-command "$ ")))
		     (start-process-shell-command command nil command)))
        ;; Bind "s-<f2>" to "slock", a simple X display locker.
        ([s-f2] . (lambda ()
		    (interactive)
		    (start-process "" nil "/usr/bin/slock")))))

;; To add a key binding only available in line-mode, simply define it in
;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

;; The following example demonstrates how to use simulation keys to mimic
;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
;; and DEST is what EXWM actually sends to application.  Note that both SRC
;; and DEST should be key sequences (vector or string).
(setq exwm-input-simulation-keys
      '(
        ;; movement
        ([?\C-\S-a] . [S-home])
        ([?\C-\S-e] . [S-end])
        ([?\C-\S-n] . [S-down])
        ([?\C-\S-p] . [S-up])
        ([?\C-\S-f] . [S-right])
        ([?\C-\S-b] . [S-left])
        ([?\C-g] . [escape])
        ([?\M-<] . [C-home])
        ([?\M->] . [C-end])
        ([?\M-F] . [S-C-right])
        ([?\M-B] . [S-C-left])
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
        ([?\C-s] . [C-f])
        ([?\M-d] . [C-S-right delete])
        ([?\C-d] . [delete])
        ;; cut/paste.
        ;; ([?\C-w] . [C-backspace])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])))

;; Do not forget to enable EXWM. It will start by itself when things are
;; ready.  You can put it _anywhere_ in your configuration.
(exwm-enable)
