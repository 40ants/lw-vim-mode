(in-package :vim)

(defun linewise ()
  b-vim-linewise)

(defun save-linewise-status ()
  (setf *vim-saved-linewise-status* b-vim-linewise))

(defun exclusive ()
  b-vim-exclusive)

(defun inclusive ()
  (not b-vim-exclusive))

(defun (setf linewise) (val)
  (setf b-vim-linewise val))

(defun (setf exclusive) (val)
  (setf b-vim-exclusive val))

(defun (setf inclusive) (val)
  (setf b-vim-exclusive (not val)))

(defun just-blanks (start end)
  "Returns T if the characters from start to end are spaces."
  (loop for ch across (points-to-string start end)
        if (not (eql ch #\Space))
        do (return nil)
        finally (return t)))

(defun blanks-before (point)
  (with-point ((bol point :temporary))
    (line-start bol)
    (just-blanks bol point)))

(defun blanks-after (point)
  (with-point ((eol point :temporary))
    (line-end eol)
    (just-blanks point eol)))

(defun vim-window-line ()
  (with-point ((top (current-point)))
    (save-excursion
      (top-of-window-command nil)
      (move-point top (current-point)))
    (count-lines top (current-point))))

(defun scroll-line-to-place (line-num place)
  (when line-num
    (goto-line-command line-num))
  (refresh-screen-command (case place
                            (:top 0)
                            (:middle nil)
                            (:bottom -1))))

(def-vim-char-attribute :whitespace '(#\Space #\Return #\Newline #\Tab))
(def-vim-char-attribute :keyword
  (loop for ch across "-_0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        collect ch))
(def-vim-char-attribute :filename
  (loop for ch across "0123456789/\\.-_+,#$%{}[]:abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!~="
        collect ch))

(defmethod vim-char-attribute ((type symbol) &optional (point (current-point)))
  (member (next-character point) (gethash type b-vim-char-attributes)))
(defmethod vim-char-attribute ((test function) &optional (point (current-point)))
  (funcall test (next-character point)))
(defmethod vim-char-attribute ((types list) &optional (point (current-point)))
  (let* ((invert-p (eql (car types) :not))
         (types (if invert-p (cdr types) types)))
    (loop for type in types
          if (vim-char-attribute type point)
          return (not invert-p)
          finally return invert-p)))

(defun vim-find-attribute (forward attributes &optional (point (current-point)))
  (with-point ((started-at point))
    (let ((offset (if forward 1 -1)))
      (loop while (and (not (vim-char-attribute attributes point))
                       (character-offset point offset)))
      (and (point/= started-at point)
           (vim-char-attribute attributes point)))))

(defun xor (x y)
  (or (and x (not y))
      (and y (not x))))

(defun white-p (point)
  (vim-char-attribute :whitespace point))

(defun skip-white (point forward)
  (if (white-p point)
    (vim-find-attribute forward '(:not :whitespace) point)
    t))

(defun skip-non-white (point forward)
  (if (white-p point)
    t
    (vim-find-attribute forward :whitespace point)))

;; FIXME: optimize this a bit so we're not creating several new functions
;; every time we call with-move.  Discussion: the only way to do that
;; that I can see is change all the functions in the LABELS to macros.
;; I think, for now, I won't worry about it.
(defmacro with-move ((forward point &key (word-type :keyword)) &body body)
  (rebinding (forward point word-type)
    (with-unique-names (limit limit-test)
      `(block with-move
         (let ((,limit nil)
               (,limit-test (if ,forward #'point<= #'point>=)))
           (macrolet ((must (&body body)
                        `(let ((res (and ,@body)))
                           (if (and res (test-limit))
                             res
                             (return-from with-move nil)))))
             (labels ((set-point (new-point)
                        (setf ,point new-point))
                      (set-direction (new-direction)
                        (setf ,forward new-direction))
                      (test-limit ()
                        (or (not ,limit)
                            (funcall ,limit-test ,point ,limit)))
                      (line-limit (point)
                        (if ,forward
                          (line-end point)
                          (line-start point)))
                      (set-limit (limit-point)
                        (setf ,limit limit-point))
                      (go-forward ()
                        (setf ,forward t))
                      (u-turn ()
                        (setf ,forward (not ,forward)))
                      (invert (list)
                        (if (eql (car list) :not)
                          (cdr list)
                          (cons :not list)))
                      (skip-current ()
                        (loop for attrib in (list '(:whitespace)
                                                  (list ,word-type)
                                                  (list :not :whitespace ,word-type))
                              until (vim-char-attribute attrib ,point)
                              finally return (must (vim-find-attribute ,forward (invert attrib) ,point))))
                      (fix-endp (endp)
                        (if endp
                          (must (character-offset ,point (if ,forward -1 1)))
                          t))
                      (boundary (&optional endp)
                        (must (skip-current)
                              (fix-endp endp)))
                      (skip (&rest attribute)
                        (let ((inverted-attribute (invert attribute)))
                          (or (vim-char-attribute inverted-attribute ,point)
                              (must (vim-find-attribute ,forward inverted-attribute ,point)))))
                      (bump ()
                        (must (character-offset ,point (if ,forward 1 -1))))
                      (unbump ()
                        (must (character-offset ,point (if ,forward -1 1))))
                      (find (attribute &optional endp)
                        (skip (invert attribute))
                        (fix-endp endp))
                      (go-here ()
                        (move-point (current-point) ,point)))
               ,@body)))))))

(defgeneric vim-offset (n type forward point &key &allow-other-keys))
(defmethod vim-offset (count (type (eql :word)) forward point &key end (word-type :keyword))
  (setf count (or count 1))
  (loop for n below count
        while
        ; This code highlights that e & b are inverses of each other, and
        ; w and ge are inverses of each other.  That is, e & b do the same
        ; things in opposite directions; same for w and ge.
        (with-move (forward point :word-type word-type)
          (cond ((xor forward end)
                 (boundary)
                 (skip :whitespace))
                (t (bump)
                   (skip :whitespace)
                   (boundary :end))))
        finally return (= n count)))

(defmethod vim-offset (count (type (eql :bigword)) forward point &key end)
  (flet ((move ()
           (with-move (forward point)
             (cond ((xor forward end)
                    (skip :not :whitespace)
                    (skip :whitespace))
                   (t (bump)
                      (skip :whitespace)
                      (skip :not :whitespace)
                      (unbump))))))
    (setf count (or count 1))
    (loop for n below count
          while (move)
          finally return (= n count))))

(defmethod vim-offset (count (type (eql :sentence)) forward point &key &allow-other-keys)
  (unless count (setf count 1))
  (sentence-offset point (if forward count (- count))))

(defun current-word (&optional (word-type :keyword) (point (current-point)))
  (unless (listp word-type)
    (setf word-type (list word-type)))
  (with-point ((start point)
               (end point))
    (with-move (t start)
      (find word-type)
      (u-turn)
      (find (invert word-type) :end)
      (move-point end start)
      (go-forward)
      (set-point end)
      (find (invert word-type))
      (points-to-string start end))))

(defun move-over-word (p type end)
  (vim-offset 1 type nil b-vim-begin-pending-motion)
  (vim-offset (or p 1) type t (current-point) :end end))

(defun vim-read-a-character ()
  (gesture-to-simple-char
   (prompt-for-character* "Character: " :ignored)))

(defun vim-find-char (forward p &key ch leave-before save)
  (let (update-window)
    (unless ch (setf ch (vim-read-a-character)
                     update-window t))
    (when save
      (setf *vim-last-find-char* (list forward ch leave-before (exclusive))))
    (setf p (or p 1))
    (with-point ((point (current-point))
                 (limit (current-point)))
      (with-move (forward point)
        (set-limit (line-limit limit))
        (bump)
        (loop count (equal ch (next-character point)) into count
              if (= count p)
              return t
              else do (bump))
        (when leave-before (unbump))
        (go-here)))
    (when update-window
      (update-buffer-window (current-window)))
    ))

(defun vim-repeat-last-find (p &key invert)
  (if p
    (destructuring-bind (forward ch leave-before exclusive) *vim-last-find-char*
      (cond (invert
             (setf (exclusive) (not exclusive))
             (vim-find-char (not forward) p :ch ch :leave-before leave-before))
            (t (setf (exclusive) exclusive)
               (vim-find-char forward p :ch ch :leave-before leave-before))))
    (editor-error "No character to find.")))

(defun finish-pending-motion (move)
  (let ((saved-vim-movement-pending b-vim-movement-pending))
    (flet ((command (p)
             (setf b-vim-movement-pending saved-vim-movement-pending)
             (move-point b-vim-point-before-movement (current-point))
             (move-point b-vim-begin-pending-motion (current-point))
             (let ((offset (point-column b-vim-point-before-movement)))
               (funcall move p)
               (unless (exclusive)
                 (character-offset (current-point) 1))
               (funcall *vim-pending-action*
                        b-vim-begin-pending-motion
                        (current-point))
               (if (point-buffer b-vim-point-before-movement) ; point deleted?
                 (move-point (current-point) b-vim-point-before-movement) ; no
                 (progn
                   ; FIXME: Extend with-move to do this more elegantly
                   (let ((point (current-point)))
                     (with-point ((limit point))
                       (line-start point)
                       (line-end limit)
                       (loop for count below offset
                             while (point< point limit)
                             do (character-offset point 1))
                       (when (point> point limit)
                         (character-offset point -1))))
                   ; ; make a new point
                   (setf b-vim-point-before-movement (copy-point (current-point))))))))
      (command nil)
      (setf *vim-last-action* #'command
            *vim-repeat-multiplier* nil
            b-vim-movement-pending nil)
      ; FIXME: (restore-modes)
      (setf (buffer-minor-mode (current-buffer) "Vim Operator Pending") nil)
      (setf (buffer-minor-mode (current-buffer) b-vim-movement-pending-ending-mode) t)
      )))

(defun mode-p (mode)
  (let ((buffer (current-buffer)))
    (and buffer
         (find mode (buffer-mode-names buffer) :test #'string=))))

(defun vim-command-mode-p ()
  (mode-p "Vim Command"))
(defun vim-operator-pending-mode-p ()
  (mode-p "Vim Operator Pending"))
(defun vim-insert-mode-p ()
  (mode-p "Vim Insert"))

(defun start-insert-mode ()
  (setf (buffer-minor-mode (current-buffer) "Vim Insert") t)
  (setf (buffer-minor-mode (current-buffer) "Vim Command") nil))

; FIXME: Need to call this when we move the cursor via the mouse, too.
(defun mark-start-insert ()
  (when (vim-insert-mode-p)
    (when (and b-vim-insert-start-point
               (point-buffer b-vim-insert-start-point))
      (delete-point b-vim-insert-start-point))
    (setf b-vim-insert-start-point (copy-point (current-point) :insert-after))))

(defun setup-vim-insert-mode (buffer)
  (use-buffer buffer
    (mark-start-insert)))

; FIXME: The way I do repeats in the rest of the code is fairly clean.
; The way I do it for repeated inserts is pretty gross by comparison.
; I wanted to "make it work, then make it pretty."  It's not pretty
; yet.  I think to make it pretty I'd need to invoke a recursive edit,
; which could be "interesting", because I don't think the current code
; is recursive-edit-proof.  Not that making it recursive-edit- proof
; would be bad or anything, it just isn't yet.  I think.  :)
; FIXME: the undo for the inserted text is funky.
(defun finish-repeated-insert ()
  ; If somebody else modifies our buffer, especially if they delete
  ; the text that b-vim-insert-start-point points to, then
  ; b-vim-insert-start-point is deleted too, so we need to make sure
  ; it's valid.
  (when (point-buffer b-vim-insert-start-point)
    (setf *vim-last-inserted-text* (points-to-string b-vim-insert-start-point (current-point)))
    (let ((default-repeat b-vim-repeat-insertion-count))
    ; Save the action so it can be repeated later by the . command.
      (setf *vim-last-action*
            (lambda (p)
              (unless p (setf p default-repeat)) ; reminder: 0 is non-nil!  :)
              (when (> p 0)
                (funcall b-vim-repeat-insert-start)
                (insert-string (current-point)
                               (apply #'concatenate 'string
                                      (loop for n below p
                                            collect *vim-last-inserted-text*))))))
      (funcall *vim-last-action* (1- b-vim-repeat-insertion-count)))))

(defun setup-vim-command-mode (buffer)
  (use-buffer buffer
    ; I thought I needed these at one point, but now I don't think so.  Leaving
    ; for a while, just in case.
    (setf ; *vim-pending-action* (constantly nil)
          ; *vim-last-action* #'identity
     *vim-repeat-multiplier* nil)
    )
  (setf editor::*meta-prefix-gesture-spec* (sys::make-gesture-spec
                                    (char-code #\C-Escape)
                                    sys:gesture-spec-control-bit))
  ; fixme: can't seem to have the meta-prefix-gesture and the interrupt key be the same key
  #+nil
  (set-interrupt-keys '(#\escape)))

; Note: Emacs's kill-region-command does not include the character at the "end" mark.  I
; think this just reflects a difference in approach.  To Emacs a point lies *between*
; two characters, so if you have 123^456^789 it deletes the "456".  To Vim a point lies *on*
; a character, so if you have
; 123456789
;    ^ ^
; an "exclusive" motion d2l deletes the "45".
(defun vim-action-over-motion (action begin end)
  ;; Make sure begin and end are distinct objects, different from each other
  ;; and different from (current-point).
  (with-point ((begin begin :before-insert)
               (end end))
    ; (format t "~&starting Vim Delete Motion: begin is ~S, end is ~S~%" begin end)
    (when (point< end begin)
      (rotatef end begin))
    ; (format t "begin is ~S, end is ~S~%" begin end)
    (when (and (not (linewise))
               (not (same-line-p begin end))
               (blanks-before begin)
               (blanks-after end))
      (setf (linewise) t))
    (when (linewise)
      (line-start begin)
      (line-end end)
      (character-offset end 1))
    ; (format t "point is ~S, begin is ~S, end is ~S~%" (current-point) begin end)
    (move-point (current-point) begin)
    (set-current-mark end)
    (funcall action nil)))
