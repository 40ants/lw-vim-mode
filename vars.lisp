(in-package :vim)

(defparameter *vim-repeat-multiplier* nil
  "The repeat multiplier, e.g. the 2 in 2d4j.")
(defparameter *vim-pending-action* (constantly nil)
  "Stores a closure to run after the next movement command.  Used to implement things
like dj.")
(defparameter *vim-last-action* #'identity
  "Stores a closure to run to repeat the previous command.  Used to implement the . command.")
(defparameter *vim-last-find-char* nil
  "Stores direction and leave-before-ness of the last f/F/t/T command.")
(defparameter *bound-vim-commands* nil)

(defparameter *vim-vars* (make-hash-table :test #'equal)
  "Holds the vim variables.")
(defparameter *vim-var-selectors* (make-hash-table)
  "Selector code for vim-vars.")
(defvar *vim-in-command* nil
  "T if currently running a Vim command.")
(defvar *vim-last-inserted-text* nil
  "A copy of the text most recently inserted.")

(defvar *font-lock-inverse-face*
  (editor:make-face 'font-lock-inverse-face
                    :if-exists t
                    :inverse-p t))

(defvar *vim-default-char-attributes* (make-hash-table))
