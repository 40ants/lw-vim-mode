(in-package :vim)

; Copied from fontlock.lisp in the editor sources
(defmacro with-hidden-font-lock-changes (buffer &body body)
  (declare (ignorable buffer))
  ; obviously this is wrong
  `(progn ,@body)
  #+nil
  (rebinding (buffer)
    (with-unique-names (modified writable)
      `(let ((editor::*dont-undo* t)
             (,modified (buffer-modified ,buffer))
             (,writable (shiftf (buffer-writable ,buffer) t)))
         (declare (special *dont-undo*))
         (unwind-protect
             (progn ,@body)
           (unless ,modified
             (setf (buffer-modified ,buffer) nil))
           (setf (buffer-writable ,buffer) ,writable))))))

(defmacro in-vim-command (&body body)
  `(progn
     (unless *vim-in-command*
       (invoke-hook 'vim-before-top-level-command-hooks))
     (invoke-hook 'vim-before-command-hooks)
     (let ((was-in-command *vim-in-command*)
           (*vim-in-command* t))
       (unwind-protect 
           (progn ,@body)
         (invoke-hook 'vim-after-command-hooks)
         (unless was-in-command
           (invoke-hook 'vim-after-top-level-command-hooks))))))

(editor:setup-indent 'def-vim-char-attribute 1)
(defmacro def-vim-char-attribute (type set)
  `(setf (gethash ,type *vim-default-char-attributes*) ,set))


(editor:setup-indent 'vim-point-after 0)
(defmacro vim-point-after (&body body)
  `(with-point ((point (current-point)))
     ,@body
     (prog1 (copy-point (current-point) :temporary)
       (move-point (current-point) point))))
