(add-hook 'html-mode-hook
     (lambda ()
       ;; Default indentation is usually 2 spaces, changing to 4.
       (setq sgml-basic-offset 4)))

;; Set html-mode as default for mako files
(setq auto-mode-alist (cons '("\\.mako$" . html-mode) auto-mode-alist))