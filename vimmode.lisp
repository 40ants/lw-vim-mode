(in-package :cl-user)

; To compile this in a stand-alone Lisp w/out loading the environment, use
; lw -build =(print '(compile-file "vimmode")')
; (The =() is zsh-specific though).  You could also say
; echo '(compile-file "vimmode")' > /tmp/build ; lw -build /tmp/build

#+nil
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cells))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro compile-load (file)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (compile-file-if-needed ,file :load t))))

(compile-load "packages")
(compile-load "wrap-editor")
(compile-load "vars")
(compile-load "classes")
(compile-load "vim-vars")
(compile-load "def-stuff")
(compile-load "macros")
(compile-load "functions")
(compile-load "commands")
(compile-load "bindings")

; Convenient scratchpad ...

#+nil
(bind-key "Vim Test" #(#\C-\, #\C-\.))

#+nil
(defcommand "Vim Test" (p) "" ""
  (format t "'~A'~%" (current-word)))
