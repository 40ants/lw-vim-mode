(in-package :vim)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro wrap-editor-function (symbol)
    (let ((editor-symbol (intern symbol :editor)))
      `(defun ,symbol (&rest rest)
	 (apply ',editor-symbol rest))))
  (defmacro wrap-editor-macro (symbol)
    (let ((editor-symbol (intern symbol :editor)))
      `(defmacro ,symbol (&rest rest)
	 `(,',editor-symbol ,@rest))))
  (editor:setup-indent 'wrap-list-with 1)
  (defmacro wrap-list-with (wrapper &rest list)
    `(progn
       ,@(loop for s in list
               collect `(,wrapper ,s)))))

#+cells
(defmodel point ()
  ((e-point :initarg :e-point :accessor e-point-of)
   (buffer :initarg :buffer :accessor buffer-of)))

#+cells
(defmodel buffer ()
  ((e-buffer :initarg :e-buffer :accessor e-buffer-of)
   (point :initarg :point :accessor point-of)))

; (error "fix me")
; (defun current-point ()
;   ())

;;;; Not just importing these symbols is a deliberate design choice,
;;;; on the assumption that I may well need to wrap some of these guys
;;;; more extensively.  Arguably I should just import them and cross
;;;; that bridge when I come to it, and maybe I'll eventually change,
;;;; but for now I'm just gonna leave it.

(wrap-list-with wrap-editor-macro
  buffer-minor-mode
  buffer-modified
  buffer-writable
  collect-undo
  defcommand
  def-ed-var
  defmode
  next-character
  save-excursion
  use-buffer
  with-point)

(wrap-list-with wrap-editor-function
  argument-digit-command
  back-to-indentation-command
  backward-character-command
  backward-form-command
  backward-list-command
  backward-sentence-command
  beginning-of-buffer-command
  beginning-of-line-command
  bind-key
  bottom-of-window-command
  buffer-mode-names
  buffer-pathname
  buffers-end
  buffers-start
  character-offset
  clear-undo-command
  copy-point
  count-lines
  current-buffer
  current-point
  current-window
  delete-indentation-command
  delete-next-character-command
  delete-point
  delete-previous-character-command
  editor-error
  end-of-buffer-command
  end-of-line-command
  find-file-command
  find-regexp-pattern
  font-lock-apply-highlight
  font-lock-fontify-buffer-command
  font-lock-unfontify-region
  forward-character-command
  forward-form-command
  forward-list-command
  forward-sentence-command
  gesture-to-simple-char
  goto-line-command
  hl-off-command
  hl-on-command
  insert-string
  invoke-hook
  isearch-backward-regexp-command
  isearch-forward-regexp-command
  kill-region-command
  line-end
  line-offset
  line-start
  make-fsa
  move-point
  move-to-window-line-command
  new-line-command
  next-line-command
  open-line-command
  point<
  point<=
  point>=
  point/=
  point-buffer
  points-to-string
  previous-line-command
  prompt-for-character*
  prompt-for-prefix
  refresh-screen-command
  regexp-forward-search-command
  regexp-reverse-search-command
  regular-expression-search
  revert-buffer-command
  same-line-p
  save-all-files-command
  save-region-command
  scroll-window-down-command
  scroll-window-up-command
  sentence-offset
  set-current-mark
  set-mark-command
  top-of-window-command
  undo-command
  un-kill-command
  update-buffer-window
  )
