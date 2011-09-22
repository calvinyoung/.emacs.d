(add-hook 'html-mode-hook
     (lambda ()
       ;; Default indentation is usually 2 spaces, changing to 4.
       (setq sgml-basic-offset 4)))

;; Set html-mode as default for .htmltt files
(setq auto-mode-alist (cons '("\\.htmltt$" . html-mode) auto-mode-alist))

;; Set js-mode as default for .jstt files
(setq auto-mode-alist (cons '("\\.jstt$" . js-mode) auto-mode-alist))

;; Set css-mode as default for .scsstt files
(setq auto-mode-alist (cons '("\\.scsstt$" . css-mode) auto-mode-alist))

;; Set html-mode as default for .jqt files
(setq auto-mode-alist (cons '("\\.jqt$" . html-mode) auto-mode-alist))
