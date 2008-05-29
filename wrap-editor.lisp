(in-package :vim)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro wrap-editor (symbol)
    (let ((editor-symbol (intern symbol :editor)))
      `(defmacro ,symbol (&rest rest)
         `(,',editor-symbol ,@rest))))
  (defmacro wrap-list (&rest list)
    `(progn
       ,@(loop for s in list
               collect `(wrap-editor ,s)))))


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

(wrap-list 
  argument-digit-command
  back-to-indentation-command
  backward-character-command
  backward-form-command
  backward-list-command
  beginning-of-buffer-command
  beginning-of-line-command
  bind-key
  bottom-of-window-command
  buffer-minor-mode
  buffer-mode-names
  buffer-pathname
  buffers-end
  buffers-start
  character-offset
  clear-undo-command
  collect-undo
  copy-point
  count-lines
  current-buffer
  current-point
  current-window
  def-ed-var
  defcommand
  defmode
  delete-indentation-command
  delete-next-character-command
  delete-previous-character-command
  delete-point
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
  gesture-to-simple-char
  goto-line-command
  insert-string
  invoke-hook
  kill-region-command
  line-end
  line-offset
  line-start
  make-fsa
  move-point
  move-to-window-line-command
  new-line-command
  next-character
  next-line-command
  open-line-command
  point/=
  point<
  point<=
  point>=
  point-buffer
  points-to-string
  previous-line-command
  prompt-for-character*
  prompt-for-prefix
  refresh-screen-command
  regular-expression-search
  revert-buffer-command
  same-line-p
  save-all-files-command
  save-excursion
  scroll-window-down-command
  scroll-window-up-command
  set-current-mark
  top-of-window-command
  update-buffer-window
  use-buffer
  with-point
  )
