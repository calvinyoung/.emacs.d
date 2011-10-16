;;;;;;;;;;;;;;;;;;;;;;;
;; Enable autopair mode
;;;;;;;;;;;;;;;;;;;;;;;
(require 'autopair)
(autopair-global-mode t)

;;;;;;;;;;;;;;;;;;;;;;
;; Enable redo
;;;;;;;;;;;;;;;;;;;;;;
(require 'redo)
(global-set-key (kbd "C-M-/") 'redo)    ; for window-system
(global-set-key (kbd "C-M-_") 'redo)    ; for termminal

;;;;;;;;;;;;;;;;;;;;;;;
;; Enable autocomplete
;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/autocomplete/ac-dict")
(setq ac-use-comphist nil)
(ac-config-default)

;; Keys
(setq ac-auto-start nil)
(ac-set-trigger-key "TAB")
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; Appearance
(set-face-background 'ac-candidate-face "#141414")
(set-face-foreground 'ac-candidate-face "#f6f3e8")
(set-face-background 'ac-selection-face "#64a8d8")

;;;;;;;;;;;;;;;;;;;;;;;
;; Enable markdown-mode
;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;
;; Enable scss-mode
;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'scss-mode "scss-mode"
   "Major mode for editing SCSS files" t)
(add-to-list 'auto-mode-alist '("\\.scsstt$" . scss-mode))
