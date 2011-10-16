(add-hook 'html-mode-hook
     (lambda ()
       ;; Default indentation is usually 2 spaces, changing to 4.
       (setq sgml-basic-offset 4)))

;; Set html-mode as default for .htmltt files
(add-to-list 'auto-mode-alist '("\\.htmltt$" . html-mode))

;; Set js-mode as default for .jstt files
(add-to-list 'auto-mode-alist '("\\.jstt$" . js-mode))

;; Set html-mode as default for .jqt files
(add-to-list 'auto-mode-alist '("\\.jqt$" . html-mode))
