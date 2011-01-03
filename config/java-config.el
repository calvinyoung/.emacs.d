;; Enable java mode hook
(add-hook 'java-mode-hook 'java-mode-hook)

;; Compile the current Java buffer
(defun javac-current()
  (interactive)
  (compile (concat "javac " buffer-file-name)))

;; Define Java specific hook
(defun java-mode-hook ()
  (local-set-key [C-f7] 'javac-current)
  )