;; Enable auto indenting
(global-set-key (kbd "RET") 'newline-and-indent)

;; Easier buffer list
(global-set-key "\C-x\C-b" 'bs-show)
;; (global-set-key "\C-xn" 'bs-cycle-next)
;; (global-set-key "\C-xp" 'bs-cycle-previous)
(setq bs-default-configuration "files-and-scratch")

;; Custom kill chords
(global-set-key "\C-w" 'backward-kill-word)

;; Execute extended command
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Compile command
(global-set-key [Cn-f11] 'compile)
(global-set-key [S-f11] 'kill-compilation)
(global-set-key [f11] 'next-error)

;; Easier goto line
(global-set-key "\M-g" 'goto-line)

;; Behave like vim's open line commands
(global-set-key "\C-\M-o" 'open-previous-line)
(global-set-key "\C-o" 'open-next-line)

;; Behave like vim's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Behave like vim's O command
(defun open-previous-line (arg)
  "Open a new line before the current one.
See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause
them to autoindent.")

;; Map some copy chords
(global-set-key "\C-xl" 'mark-line)
(global-set-key "\C-cw" 'copy-word)
(global-set-key "\C-cl" 'copy-line)

;; Mark line without selection
(defun mark-line (&optional arg)
  "Marks a line from start of indentation to end"
  (interactive "p")
  (beginning-of-line)
  (cua-set-mark)
  (end-of-line))

;; Copy word without selection
(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (let ((beg (progn
               (if (looking-back "[a-zA-Z0-9]" 1)
                   (backward-word 1))
               (point)))
        (end (progn
               (forward-word arg)
               (point))))
    (copy-region-as-kill beg end)))

;; Copy line without selection
(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line "
   (interactive "P")
   (let ((beg (line-beginning-position))
         (end (line-end-position arg)))
     (copy-region-as-kill beg end)))

;; Use custom comment function
(global-set-key "\M-;" 'comment-dwim-line)

;; Change default behavior of comment-dwim
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at
the end of the line, then comment current line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position)
                                   (line-end-position))
    (comment-dwim arg)))

;; Define custom backward-kill-word function
(defun my-backward-kill-word (&optional arg)
  "Replacement for the backward-kill-word command
If the region is active, then invoke kill-region.  Otherwise, use the
following custom backward-kill-word procedure.
If the previous word is on the same line, then kill the previous word.
Otherwise, if the previous word is on a prior line, then kill to the
beginning of the line.  If point is already at the beginning of the line,
then kill to the end of the previous line.

With argument ARG and region inactive, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (kill-region (mark) (point))
    (let (count)
      (dotimes (count arg)
        (if (bolp)
            (delete-backward-char 1)
          (kill-region (max (save-excursion (backward-word)(point))
                            (line-beginning-position))
                       (point)))))))

(define-key (current-global-map) [remap backward-kill-word]
  'my-backward-kill-word)

;; Define custom minor mode keys
(defvar my-keys-map (make-keymap) "my-keys keymap.")

(define-minor-mode my-keys
  "A minor mode so that my key settings override annoying major modes."
  t nil 'my-keys-map)
;; (define-key my-keys-map "\C-\M-p" (lambda() (interactive) (scroll-down 5)))

;; Scroll screen up and down
(define-key my-keys-map "\C-\M-p" (lambda() (interactive) (scroll-down 5)))
(define-key my-keys-map "\C-\M-n" (lambda() (interactive) (scroll-up 5)))

;; Move up and down by 5 lines with M-n and M-p
(define-key my-keys-map "\M-n" (lambda() (interactive) (next-line 10)))
(define-key my-keys-map "\M-p" (lambda() (interactive) (previous-line 10)))

;; Move between windows more easily
(define-key my-keys-map "\M-h" 'windmove-left)
(define-key my-keys-map "\M-j" 'windmove-down)
(define-key my-keys-map "\M-k" 'windmove-up)
(define-key my-keys-map "\M-l" 'windmove-right)

;; Resize windows
(define-key my-keys-map "\C-\M-h" 'enlarge-window-horizontally)
(define-key my-keys-map "\C-\M-j" 'enlarge-window)
(define-key my-keys-map "\C-\M-k" 'shrink-window)
(define-key my-keys-map "\C-\M-l" 'shrink-window-horizontally)

;; Recover overwritten key bindings
(define-key my-keys-map "\C-j" 'indent-new-comment-line)
(define-key my-keys-map "\C-x\M-l" 'downcase-word)
(define-key my-keys-map "\C-x\M-k" 'kill-sentence)

;; Rotate windows counter-clockwise
(define-key my-keys-map "\C-xr" 'rotate-windows)
(defun rotate-windows ()
  "Rotate your windows counter-clockwise"
  (interactive)
  (cond ((not (> (count-windows) 1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))
                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))
                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))
