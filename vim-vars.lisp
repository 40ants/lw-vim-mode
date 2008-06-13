(in-package :vim)

(defun vim-var-key (name type)
  (cons name type))
(defun vim-var-lookup (name type #| &optional create |#)
  (let* ((key (vim-var-key name type))
         (var (gethash key *vim-vars*)))
    (cond (var var)
          #+nil
          (create (setf (gethash key *vim-vars*)
                        (make-instance 'vim-var :name name :type type)))
          (t (error "vim-var-lookup: vim-var ~S of type ~S not found" name type)))))

(defun vim-var-selector (type)
  (multiple-value-bind (selector present) (gethash type *vim-var-selectors*)
    (if present
      (funcall selector)
      (error "vim-var-selector: unknown selector: ~S" type))))

(defun vim-var (name type)
  (let ((var (vim-var-lookup name type)))
    (if var
      (let ((selector (vim-var-selector type)))
        (multiple-value-bind (selected-val present)
            (gethash selector (values-of var))
          (if present
            selected-val
            (setf (gethash selector (values-of var))
                  (funcall (init-func-of var))))))
      (error "vim-var: vim-var ~S of type ~S not found" name type))))

(defun vim-var-set (name type value)
  (setf (gethash (vim-var-selector type)
                 (values-of (vim-var-lookup name type)))
        value))
(defsetf vim-var vim-var-set)

(defmacro def-vim-var-definer (definer-name type selector)
  `(progn
     (setf (gethash ,type *vim-var-selectors*) (lambda () ,selector))
     (editor:setup-indent ',definer-name 2)
     (defmacro ,definer-name (name init-code &optional (doc "Vim variable"))
       `(progn
          (setf (gethash (vim-var-key ',name ,,type) *vim-vars*)
                (make-instance 'vim-var
                               :name ',name
                               :type ,,type
                               :values (make-hash-table)
                               :init-func (lambda () ,init-code)
                               :doc ,doc))
          (eval-when (:compile-toplevel :load-toplevel :execute)
            (define-symbol-macro ,name
              (vim-var ',name ,,type)))))))

(def-vim-var-definer def-vim-var :global t)
(def-vim-var-definer def-vim-buffer-var :buffer (current-buffer))
(def-vim-var-definer def-vim-window-var :window (current-window))

(def-vim-buffer-var b-vim-point-before-movement (copy-point (current-point))
  "The point before a movement command.")
(def-vim-buffer-var b-vim-exclusive t
  "Defines whether the current movement command is exclusive")
(def-vim-buffer-var b-vim-linewise t
  "Defines whether the current movement command is linewise.")
(def-vim-buffer-var b-vim-movement-pending ""
  "Stores the name of the pending command, e.g. for cw, stores \"Vim Change Motion\".")
(def-vim-buffer-var b-vim-movement-pending-ending-mode "Vim Command"
  "Stores the default mode to be in after ending a pending-motion command.  Used to differentiate
between dw and cw.")

; These next three should probably be Window vars not Buffer vars, but
; the interactions are weird.  For now the answer is "don't do that
; then".
(def-vim-buffer-var b-vim-insert-start-point nil
  "(current-point) when insert starts in a given buffer.")
(def-vim-buffer-var b-vim-repeat-insertion-count 0
  "Saved the 3 in 3ifoo<esc>.")
(def-vim-buffer-var b-vim-repeat-insert-start (constantly t)
  "When repeating an insert, you need to also repeat the I or a or A or whatever.
This stores what to do.")

(def-vim-var v+hlsearch t
  "Are searches highlighted?")
#+nil ; I don't use this just yet.
(def-vim-var v+search-expr nil
  "String to highlight")

(def-vim-window-var w-vim-collecting-count nil
  "True if we're collecting a count.
We must know if we're collecting a count so we can process #\0 (zero) correctly.")

(def-ed-var 'vim-before-top-level-command-hooks :global t nil
  "Hooks called before starting a top-level Vim command.")
(def-ed-var 'vim-after-top-level-command-hooks :global t nil
  "Hooks called after ending a top-level Vim command.")
(def-ed-var 'vim-before-command-hooks :global t nil
  "Hooks called before starting any Vim command.")
(def-ed-var 'vim-after-command-hooks :global t nil
  "Hooks called after ending any Vim command.")

(defun copy-hash-table (source)
  (let ((dest (make-hash-table :test (hash-table-test source)
                               :size (hash-table-size source))))
    (loop for key being the hash-keys of source using (hash-value value) do
          (setf (gethash key dest) value))
    dest))

(def-vim-buffer-var b-vim-char-attributes
    (copy-hash-table *vim-default-char-attributes*))
