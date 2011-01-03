;;;;;;;;;;;;;;;;;;;;
;; GENERAL SETTINGS
;;;;;;;;;;;;;;;;;;;;

;; Set window title
(setq frame-title-format '(buffer-file-name "%f - Emacs" "Emacs"))

;; Prevent splash screen
(setq inhibit-splash-screen t)

;; Set fringe mode
(fringe-mode '(0 . 1))

;; Prevent leftover backup turds
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Stop asking me to type "yes" or "no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Make initial scratch mode usable
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)

;; Set tab width
(setq tab-width 4)

;; Set fill column
(setq-default fill-column 80)

;; Set whitespace mode to highlight column 80+ chars
(setq whitespace-style '(lines-tail)
      whitespace-line-column 80)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Enable mouse modes
(xterm-mouse-mode t)
(mouse-wheel-mode t)

;; Enable line numbers
(global-linum-mode t)
(setq linum-format "%d ")

;; Enable paren mode
(show-paren-mode t)
(setq show-paren-delay 0)

;; Enable CUA mode
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; Enable column number mode
(column-number-mode t)

;; Winner mode
(winner-mode t)

;;;;;;;;;;;;;;;;;;;;
;; Hooks
;;;;;;;;;;;;;;;;;;;;

;; Enable PHP mode
(autoload 'php-mode "php-mode.el" "Php mode." t)
(add-to-list 'auto-mode-alist '("/*.\.php[345]?$" . php-mode))

;; Turn on visual line mode for text mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Discard trailing whitespace on file save
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Set default tramp mode protocol
(setq tramp-default-method "ssh")

;; Integrate emacs and X clipboards
(setq x-select-enable-clipboard t)

;; Enable copy/paste in daemon mode and terminal
(unless window-system
  ;; Callback for when user cuts
  (defun xsel-cut-function (text &optional push)
    ;; Insert text to temp-buffer, and "send" content to xsel stdin
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--input")))
  ;; Call back for when user pastes
  (defun xsel-paste-function()
    (let ((xsel-output (shell-command-to-string "xsel --output")))
      (unless (string= (car kill-ring) xsel-output)
	xsel-output )))
  ;; Attach callbacks to hooks
  (setq interprogram-cut-function 'xsel-cut-function)
  (setq interprogram-paste-function 'xsel-paste-function)
  )

;; Daemon mode settings
(if 'server-mode
    (progn
      (setq default-frame-alist '((vertical-scroll-bars)
                                  (left-fringe . 0)
                                  (right-fringe . 1)
                                  ; (cursor-color . "#64a8d8")
                                  (initial-major-mode . 'text-mode)
                                  ))
      (menu-bar-mode 0)
      (tool-bar-mode 0)
      (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
      ))
