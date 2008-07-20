(in-package :vim)

(defun xlat-mode-abbrev (mode-abbrev)
  (ecase mode-abbrev
    (:c "Vim Command")
    (:i "Vim Insert")
    (:o "Vim Operator Pending")
    (:v "Vim Visual")))

(defmethod bind-vim ((name string) key (mode string))
  (pushnew key *bound-vim-commands*)
  ; (format t "~&~S bound to ~S in ~S mode~%" name key mode)
  (bind-key name key :mode mode))
(defmethod bind-vim ((name string) (keys list) (mode string))
  (when (eql 'quote (car keys))
    (error "Quoted list in bind-vim: ~S" keys))
  (dolist (key keys)
    (bind-vim name key mode)))
(defmethod bind-vim ((name string) (keys string) (mode string))
  (bind-vim name (make-array (list (length keys)) :initial-contents keys) mode))
(defmethod bind-vim (name keys (mode symbol))
  (bind-vim name keys (xlat-mode-abbrev mode)))
(defmethod bind-vim (name keys (mode-list list))
  (dolist (mode mode-list)
    (bind-vim name keys mode)))

(defun bind-vim-movement (name arg &key insert-too)
  (bind-vim name arg '("Vim Command" "Vim Operator Pending"))
  (when insert-too
    (bind-vim name arg "Vim Insert")))
(defun bind-vim-command (name arg)
  (bind-vim name arg "Vim Command"))
(defun bind-vim-insert (name arg)
  (bind-vim name arg "Vim Insert"))
(defun bind-vim-ci (name arg)
  (bind-vim name arg '("Vim Command" "Vim Insert")))
(defun bind-vim-table (table)
  (mapcar (lambda (list)
            (loop with command = (first list)
                  for (keys modes) on (rest list) by #'cddr do
                  (bind-vim command keys modes)))
          table)
  t ; return value not meaningful
  )

(defun add-return (&rest list)
  (apply #'concatenate 'string (mapcar #'string (append list '(#\Return)))))

;; Enter / Exit the modes
(bind-key "All Vim" #\sh-c-escape :global :emacs)
(bind-key "All Vim" #(#\c-\, #\c-\,) :global :emacs)

;; Note to all: #\c-w is read as Ctrl-W, that is Ctrl-Shift-w.  If you want
;; Ctrl-w (lowercase-w) you need to say #\c-\w.
(bind-vim-table
 `(; control
   ("Exit Vim Mode" (#\sh-c-escape #(#\c-\, #\c-\,)) :c)
   ("Vim Insert Mode" #\i :c)
   ("Vim Append" #\a :c)
   ("Vim Append at EOL" #\A :c)
   ("Vim Command Mode" (#\c-[ #\Escape) :i)
   ("Illegal" (#\c-[ #\Escape) :c)

   ; movement
   ("Vim Previous Line" (#\k #\-) (:c :o)
                        (#\up #\c-\p) (:c :i :o))
   ("Vim Previous Screen Line" "gk" (:c :o))
   ("Vim Next Line" (#\j #\return) (:c :o)
                    (#\down #\c-\j #\c-\n) (:c :i :o))
   ("Vim Next Screen Line" "gj" (:c :o))
   ("Vim Backward Character" (#\h #\c-\h #\backspace) (:c :o)
                             #\left (:c :i :o))
   ("Vim Forward Character" (#\l #\space) (:c :o)
                            #\right (:c :i :o))
   ("Vim Forward Word" #\w (:c :o)
                       #\c-right (:c :i :o))
   ("Vim Forward Word End" #\e (:c :o))
   ("Vim Forward BIGWORD" #\W (:c :o))
   ("Vim Forward BIGWORD End" #\E (:c :o))
   ("Vim Backward Word" #\b (:c :o)
                        #\c-left (:c :i :o))
   ("Vim Backward Word End" "ge" (:c :o))
   ("Vim Backward BIGWORD" #\B (:c :o))
   ("Vim Backward BIGWORD End" "gE" (:c :o))
   ("Vim Goto Line or End of File" #\G (:c :o))
   ("Vim Top of Window" #\H (:c :o))
   ("Vim Bottom of Window" #\L (:c :o))
   ("Vim Move to Window Line" #\M (:c :o))
   ("Vim Goto Line or Top of Buffer" "gg" (:c :o))
   ; ("Vim Forward Form" #\) (:c :o))
   ; ("Vim Backward Form" #\( (:c :o))
   ("Vim Forward Sentence" #\) (:c :o))
   ("Vim Backward Sentence" #\( (:c :o))
   ("Vim Forward List" #\c-\) (:c :o))
   ("Vim Backward List" #\c-\( (:c :o))
   ("Vim Beginning of Line" ; for #\0 see Vim Beginning of Line or Collect Count
                            #\home (:c :i :o))
   ("Vim End of Line" #\$ (:c :o)
                      #\end (:c :i :o))
   ("Back to Indentation" #\^ (:c :o))

   ("Beginning of Defun" "[\\" (:c :o))
   ("End of Defun" "]\\" (:c :o))

   ("Vim To Column N" #\| (:c :o))

   ; Visual mode
   ("Vim Visual Mode" #\v :c)

   ; yank / put
   ; ("Save Region" #\y :c)
   ("Vim Put Before" #\P :c)
   ("Vim Put After" #\p :c)

   ; scrolling
   ("Scroll Window Down" #\c-\f (:c :o))
   ("Scroll Window Up" #\c-\b (:c :o))
   ("Vim Scroll Window Up" (#\c-\y #\sh-up) (:c :i :o))
   ("Vim Scroll Window Down" (#\c-\e #\sh-down) (:c :i :o))
   ("Vim Scroll Line to Top of Window" ("zt" #(#\z #\Return)) (:c :o))
   ("Vim Scroll Line to Middle of Window" ("zz" "z.") (:c :o))
   ("Vim Scroll Line to Bottom of Window" ("zb" "z-") (:c :o))

   ; Repeating
   ("Vim Repeat" #\. :c)
   ("Vim Argument Digit" (#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (:c :o))
   ("Vim Beginning of Line or Collect Count" #\0 #| zero |# (:c :o))

   ; changes
   ("Vim Delete Next Character" #\x :c)
   ("Vim Delete Previous Character" #\X :c)
   ("Vim Replace Character" #\r :c)
   ("Vim Append Text at End of Line" #\A :c)
   ("Vim Insert Text at Beginning of Line" #\I :c)
   ("Vim Delete Motion" #\d :c)
   ("Vim Change Motion" #\c :c)
   ("Vim Yank Motion" #\y :c)
   ("Vim Kill To End Of Line" #\D :c)
   ("Vim Yank Line" #\Y :c)
   ("Vim Move Over Whole Line" (#\d #\y) :o) ; used for "dd" and "yy"
   ("Vim Move Over Inner Word" "iw" :o)
   ("Vim Move Over A Word" "aw" :o)
   ("Vim Move Over Inner BigWord" "iW" :o)
   ("Vim Move Over A BigWord" "aW" :o)
   ("Vim Open Line Down" #\o :c)
   ("Vim Open Line Up" #\O :c)
   ("Vim Undo" #\u :c)

   ("Lowercase Word" "~w" :c)
   ; ("Lowercase Region" "vu" :c)
   ; ("Uppercase Region" "vU" :c)

   ("Indent Rigidly" ">>" :c)
   ("Vim Join Lines" #\J :c)

   ("Vim Insert Character Above Cursor" #\c-\y :i)
   ("Vim Insert Character Below Cursor" #\c-\e :i)

   ; buffer / window manipulation
   ("New Buffer" ":n" :c)

   ("New Window" #(#\c-\w #\n) :c)
   ("Next Ordinary Window" #(#\c-\w #\j) :c)
   ("Previous Window" #(#\c-\w #\k) :c)
   ("Delete Window" #(#\c-\w #\c) :c)

   ("List Buffers" ,(add-return ":ls") :c)
   ("Circulate Buffers" ,(add-return ":bn") :c)
   ("Select Buffer" ":b " :c)

   ; search
   ("Vim ISearch Forward Regexp" #\/ (:c :o))
   ("Vim ISearch Backward Regexp" #\? (:c :o))
   ("Vim Find Next" #\n (:c :o))
   ("Vim Find Previous" #\N (:c :o))
   ("List Matching Lines" ":gp" :c) ; not in Vim, exactly
   ("Delete Matching Lines" ":gd" :c) ; not in Vim
   ("Vim Regexp Forward Search" #\c-\/ (:c :o))
   ("Vim Regexp Reverse Search" #\c-\? (:c :o))
   ("Vim Find Char Right" #\f (:c :o))
   ("Vim Find Char Left" #\F (:c :o))
   ("Vim Find Till Char Right" #\t (:c :o))
   ("Vim Find Till Char Left" #\T (:c :o))
   ("Vim Repeat Last Find Char" #\; (:c :o))
   ("Vim Repeat Last Find Char Opposite" #\, (:c :o))

   ; File manipulation
   ("Save File" ,(add-return ":w") :c)
   ("Vim Save All Files" ,(add-return ":wa") :c)
   ("Vim Save All Files And Exit" ,(add-return ":wqa") :c)
   ("Write File" ":w " :c)
   ("Wfind file" ":e " :c)
   ("Vim Revert Buffer" ":e!" :c)
   ("Insert File" ":r " :c)
   ("Vim Goto File" "gf" :c)

   ; tagging
   ("Find Source" #\c-\] :c)
   ("Continue Tags Search" ,(add-return ":tn") :c)
   ("View Source Search" ,(add-return ":ts") :c)
   ))

;;; counts; may change later; for now just use Argument Digit
; (bind-vim-command "Argument Digit" '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
; (bind-vim-command "What Cursor Position" '(#(#\g #\Ctrl-\Meta-\g)))
; (bind-vim-command "vim beginning of line or collect count"  #\0)

(defun restore-default-highlighting ()
  ;; There may be a better way to do this.  Like remembering all the
  ;; highlighting we delete and replace, and restoring it.  But this seems
  ;; to work pretty well and pretty quickly, on the small files I've edited
  ;; so far.
  (font-lock-fontify-buffer-command nil))

(defun highlight-search ()
  (when editor::*last-search-string*
    (restore-default-highlighting)
    (let* ((buffer (current-buffer))
           (start (buffers-start buffer))
           (end (buffers-end buffer)))
      ;; For some reason the highlighting doesn't show up unless I use this macro. ???
      (with-hidden-font-lock-changes buffer
        (with-point ((point start :temporary)
                     (form-start start :temporary))
          (let ((pattern (make-fsa editor::*last-search-string*)))
            (loop (let ((length (find-regexp-pattern point pattern t end)))
                    (unless length
                      (return))
                    (move-point form-start point)
                    (character-offset point length)
                    (font-lock-unfontify-region form-start point)
                    (font-lock-apply-highlight form-start point *font-lock-highlight-search-face*)))))
        )
      )))

;;; Not Vim
(bind-vim-table
  '(("Evaluate Defun" ",x" :c)
    ("Evaluate Defun In Listener" ",l" :c)))

;;; Vim Insert mode
; (bind-vim-insert "Indent New Line" #\Return)
; (bind-vim-insert "New Line" #\Newline)
; (bind-vim-insert "New Line" #\Return)

;;; Larry's favorite mappings
(bind-vim-table
  '(("Vim Save All Files" #\c-F12 (:c :i))
    ("List Buffers" #\F7 (:c :i))
    ("Circulate Buffers" #\c-F10 :c)))

;;; Ignore everything not defined

(defun vim-command-char-p (char &optional (seq *bound-vim-commands*))
  (find-if (lambda (item)
             (or (eql char item)
                 (and (not (characterp item))
                      (not (keywordp item))
                      (vim-command-char-p char item))))
           seq))

(defmethod vim-ignore ((char character))
  (unless (vim-command-char-p char)
    ; (format t "Ignoring ~S~%" char)
    (bind-key "Illegal" char :mode "Vim Command")))
(defmethod vim-ignore ((char integer))
  (vim-ignore (code-char char)))

(loop for char across "abcdefghijklmnopqrstuvwxyz0123456789~`!@#$%^&*()_-+=[]{}\\|;':\",./<>?"
      do (vim-ignore char)
      if (both-case-p char)
      do (vim-ignore (char-upcase char)))

; Neither of these work; both expect Major Modes.  :(
; (pushnew "Vim Command" *default-new-buffer-modes*)
; (pushnew "Vim Command" (variable-value "Default Modes"))

#+nil
(progn
(defun foo (buffer enter)
  (format t "buffer is ~S, enter is ~S~%" buffer enter))

(add-hook 'foo 'vim-insert-mode-hook)
(add-global-hook vim-insert-mode-hook 'foo)
(remove-global-hook vim-insert-mode-hook 'foo)
) ; progn

(defadvice (editor::incremental-search-somehow after :after) (direction)
  (declare (ignorable direction))
  (when v+hlsearch
    (highlight-search)))
