;;;;;;;;;;;;;;;;;;;;
;; LOAD CONFIG FILES
;;;;;;;;;;;;;;;;;;;;

;; Add all config files in my ~/.emacs.d directory
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
    (let ((default-directory (file-name-as-directory dir)))
        (add-to-list 'load-path dir)
            (normal-top-level-add-subdirs-to-load-path)))
            (add-subdirs-to-load-path "~/.emacs.d")

;; Load general config settings
(load-library "general-config")

;; Load c configs
(load-library "c-config")

;; Load java configs
(load-library "java-config")

;; Load global key bindings
(load-library "keybindings")

;; Load color scheme
(load-library "color-theme-wombat")
(color-theme-wombat)

;; Initialize plugins
(load-library "init-plugins")

(put 'downcase-region 'disabled nil)
