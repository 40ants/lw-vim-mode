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

;; I'd really like a font that says "change the background to yellow but leave
;; the foreeground alone.  Using :foreground nil doesn't seem to work.  Oh Well.
(defvar *font-lock-highlight-search-face*
  (editor:make-face 'font-lock-highlight-search-face
                    :if-exists :overwrite
                    :background :yellow))

(defvar *vim-default-char-attributes* (make-hash-table))

;; FIXME: This is really a hack.  To interact correctly with the Emacs kill-ring,
;; we need a ring of these, too.
(defvar *vim-saved-linewise-status* :linewise
  "Stores the linewise status of the last yanked text.")
