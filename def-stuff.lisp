(in-package :vim)

(editor:setup-indent 'def-vim-change 4)
(defmacro def-vim-change (name lambda-list command-doc function-doc &body body)
  (let ((p (car lambda-list))
        (change (gensym))
        (default-repeat (gensym)))
    `(defcommand ,name ,lambda-list ,command-doc ,function-doc
       (in-vim-command
         (setf w-vim-collecting-count nil)
         (let ((,default-repeat ,p))
           (flet ((,change (,p)
                    (unless ,p (setf ,p ,default-repeat))
                    ,@body))
             (setf *vim-last-action* #',change)
             (,change nil)))))))

(editor:setup-indent 'def-vim-move 6)
(defmacro def-vim-move (name lambda-list linewise incl/excl command-doc function-doc 
                             &body body)
  (unless (member incl/excl '(:inclusive :exclusive))
    (error "Invalid incl/excl flag ~A in def-vim-move ~A; should be ~A or ~A~%"
           incl/excl name :inclusive :exclusive))
  (let ((p (car lambda-list))
        (move (gensym))
        (default-repeat (gensym)))
    `(defcommand ,name ,lambda-list ,command-doc ,function-doc
       (in-vim-command
         (setf w-vim-collecting-count nil)
         (let ((,default-repeat
                (when (or *vim-repeat-multiplier* ,p)
                  (* (or *vim-repeat-multiplier* 1)
                     (or ,p 1)))))
           (flet ((,move (,p)
                    (unless ,p (setf ,p ,default-repeat))
                    (setf b-vim-linewise ,linewise
                          (exclusive) ,(eql incl/excl :exclusive)
                          b-vim-repeat-insertion-count 0)
                    (prog1 (progn ,@body)
                      ; FIXME: Need to call this when we move the cursor via the mouse, too.
                      (mark-start-insert))))
             (if (vim-operator-pending-mode-p)
               (finish-pending-motion #',move)
               (,move nil))))))))

(editor:setup-indent 'def-vim-movement-pending 4)
(defmacro def-vim-movement-pending (name lambda-list command-doc function-doc &body body)
  `(defcommand ,name (p) ,command-doc ,function-doc
     (in-vim-command
       (setf w-vim-collecting-count nil
             *vim-repeat-multiplier* p
             *vim-pending-action* (lambda ,lambda-list
                                    ,@body)
             b-vim-movement-pending-ending-mode "Vim Command"
             b-vim-movement-pending ,name)
       ; FIXME: (save-modes)
       (setf (buffer-minor-mode (current-buffer) "Vim Command") nil)
       (setf (buffer-minor-mode (current-buffer) "Vim Operator Pending") t))))

(editor:setup-indent 'def-start-insert 4)
(defmacro def-start-insert (name lambda-list command-doc function-doc &body body)
  (let ((p (car lambda-list)))
    `(defcommand ,name ,lambda-list ,command-doc ,function-doc
       (setf b-vim-repeat-insertion-count (or ,p 1)
             b-vim-repeat-insert-start (lambda () ,@body))
       (funcall b-vim-repeat-insert-start)
       (start-insert-mode))))

