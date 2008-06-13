(in-package :vim)

;; Modes
(defmode "Vim Command" :setup-function 'setup-vim-command-mode)
(defmode "Vim Insert" :setup-function 'setup-vim-insert-mode)
(defmode "Vim Operator Pending")
(defmode "Vim Visual")

(defvar *orig-meta-prefix-gesture-spec* editor::*meta-prefix-gesture-spec*)
(defvar *orig-interrupt-keys* '(#\c-g))

(defcommand "All Vim" (p)
     "Put all buffers in Vim Command mode."
     "Put all buffers in Vim Command mode."
  (declare (ignore p))
  (dolist (b editor::*buffer-list*)
    (when (buffer-pathname b)
      (use-buffer b
        (vim-command-mode-command nil)))))

(defcommand "Vim Command Mode" (p)
     "Start Vim Command mode"
     "Start Vim Command mode"
  (declare (ignore p))
  (when (vim-insert-mode-p)
      ; Finish the repeat count on the i command itself, e.g. 3ix<esc> => xxx.  This
      ; doesn't work from inside copy-inserted-text.  I think setf of buffer-minor-mode
      ; locks the buffer, which I guess maybe makes sense.
    (finish-repeated-insert))
  (setf (buffer-minor-mode (current-buffer) "Vim Insert") nil)
  (setf (buffer-minor-mode (current-buffer) "Vim Operator Pending") nil)
  (setf (buffer-minor-mode (current-buffer) "Vim Command") t))

(defcommand "Vim Mode" (p) "" ""
  (vim-command-mode-command p))

(def-start-insert "Vim Insert Mode" (p) "" ""
  (declare (ignore p))
  t)

(defcommand "Vim Visual Mode" (p) "" ""
  (setf (buffer-minor-mode (current-buffer) "Vim Operator Pending") nil)
  (setf (buffer-minor-mode (current-buffer) "Vim Command") t)
  (setf (buffer-minor-mode (current-buffer) "Vim Visual") t)
  ; from Barry Wilkes via Edi Weitz's lw-add-ons
  (set-mark-command p)
  (hl-on-command p))

(defcommand "Exit Vim Mode" (p)
     "Exit Vim Command Mode"
     "Exit Vim Command Mode"
  (declare (ignore p))
  (dolist (mode '("Vim Command" "Vim Operator Pending" "Vim Insert"))
    (setf (buffer-minor-mode (current-buffer) mode) nil))
  (setf editor::*meta-prefix-gesture-spec* *orig-meta-prefix-gesture-spec*)
  #+nil
  (set-interrupt-keys *orig-interrupt-keys*))

(def-start-insert "Vim Append" (p) "" ""
  (declare (ignore p))
  (forward-character-command 1))

(def-start-insert "Vim Append at EOL" (p) "" ""
  (end-of-line-command p))

(defcommand "Vim Scroll Window Down" (p) "" ""
  (scroll-window-down-command (or p 1)))

(defcommand "Vim Scroll Window Up" (p) "" ""
  (scroll-window-up-command (or p 1)))

; Modified from Line to Top of Window from filecoms.lisp in the editor sources.
(defcommand "Vim Scroll Line to Top of Window" (p) "" ""
  (scroll-line-to-place p :top))

(defcommand "Vim Scroll Line to Middle of Window" (p) "" ""
  (scroll-line-to-place p :middle))

(defcommand "Vim Scroll Line to Bottom of Window" (p) "" ""
  (scroll-line-to-place p :bottom))

(defcommand "Vim Repeat" (p) "" ""
  (funcall *vim-last-action* p))

(def-start-insert "Vim Append Text at End of Line" (p) "" ""
  (declare (ignore p))
  (end-of-line-command nil))

(def-start-insert "Vim Insert Text at Beginning of Line" (p) "" ""
  (declare (ignore p))
  (back-to-indentation-command nil))

(def-start-insert "Vim Open Line Down" (p) "" ""
  (declare (ignore p))
  (line-end (current-point))
  (new-line-command nil))

(def-start-insert "Vim Open Line Up" (p) "" ""
  (declare (ignore p))
  (line-start (current-point))
  (open-line-command nil))

(defcommand "Vim Save All Files" (p) "" ""
  (declare (ignore p))
  (save-all-files-command t))

(defcommand "Vim Save All Files and Exit" (p) "" ""
  (declare (ignore p))
  (save-all-files-command t)
  (lispworks-tools::confirm-quit-lispworks))

;; This isn't really the right way (i.e. the Vim way) to do a
;; setting like this.  Oh well.
#+nil
(defcommand "Vim Toggle hlsearch" (p) "" ""
  (setq v+hlsearch
        (cond ((not p) (not v+hlsearch))
              ((plusp p) t)
              (t nil)))
  (if v+hlsearch
    (highlight-search)
    (font-lock-fontify-buffer-command nil)))

; (defcommand "Vim Jump To Match" (p) "Currently only works on ()" ""
;   (declare (ignore p))

(def-vim-move "Vim Next Line" (p) :linewise :inclusive "" ""
  (next-line-command p))
(def-vim-move "Vim Next Screen Line" (p) nil :inclusive "" ""
  (next-line-command p))
(def-vim-move "Vim Previous Line" (p) :linewise :inclusive "" ""
  (previous-line-command p))
(def-vim-move "Vim Previous Screen Line" (p) nil :inclusive "" ""
  (previous-line-command p))
(def-vim-move "Vim Backward Character" (p) nil :exclusive "" ""
  (backward-character-command p))
(def-vim-move "Vim Forward Character" (p) nil :exclusive "" ""
  (forward-character-command p))

(def-vim-move "Vim Forward Word" (p) nil :exclusive "" ""
  (let ((end nil))
    (when (and (white-p (current-point))
               (string= b-vim-movement-pending "Vim Change Motion"))
      (setf end t
            (exclusive) nil))
    (vim-offset p :word t (current-point) :end end)))
(def-vim-move "Vim Forward Word End" (p) nil :inclusive "" ""
  (vim-offset p :word t (current-point) :end t))
(def-vim-move "Vim Backward Word" (p) nil :exclusive "" ""
  (vim-offset p :word nil (current-point)))
(def-vim-move "Vim Backward Word End" (p) nil :inclusive "" ""
  (vim-offset p :word nil (current-point) :end t))

(def-vim-move "Vim Forward BIGWORD" (p) nil :exclusive "" ""
  (let ((end nil))
    (when (and (white-p (current-point))
               (string= b-vim-movement-pending "Vim Change Motion"))
      (setf end t
            (exclusive) nil))
    (vim-offset p :bigword t (current-point) :end end)))
(def-vim-move "Vim Forward BIGWORD End" (p) nil :exclusive "" ""
  (vim-offset p :bigword t (current-point) :end t))
(def-vim-move "Vim Backward BIGWORD" (p) nil :inclusive "" ""
  (vim-offset p :bigword nil (current-point)))
(def-vim-move "Vim Backward BIGWORD End" (p) nil :inclusive "" ""
  (vim-offset p :bigword nil (current-point) :end t))

(def-vim-move "Vim Forward Sentence" (p) nil :exclusive "" ""
  (vim-offset p :sentence t (current-point)))

(def-vim-move "Vim Backward Sentence" (p) nil :exclusive "" ""
  (vim-offset p :sentence nil (current-point)))

(def-vim-move "Vim Goto Line or End of File" (p) :linewise :inclusive "" ""
  (if p
    (goto-line-command p)
    (end-of-buffer-command nil))
  (back-to-indentation-command p))

(def-vim-change "Vim Delete Next Character" (p) "" ""
  (delete-next-character-command p))
(def-vim-change "Vim Delete Previous Character" (p) "" ""
  (delete-previous-character-command p))

(def-vim-move "Vim Top of Window" (p) :linewise :inclusive "" ""
  (top-of-window-command p))
(def-vim-move "Vim Bottom of Window" (p) :linewise :inclusive "" ""
  (bottom-of-window-command p))
(def-vim-move "Vim Move to Window Line" (p) :linewise :inclusive "" ""
  (move-to-window-line-command p))

(def-vim-move "Vim Forward Form" (p) nil :exclusive "" ""
  (forward-form-command p))
(def-vim-move "Vim Backward Form" (p) nil :exclusive "" ""
  (backward-form-command p))
(def-vim-move "Vim Forward List" (p) nil :exclusive "" ""
  (forward-list-command p))
(def-vim-move "Vim Backward List" (p) nil :exclusive "" ""
  (backward-list-command p))

(def-vim-move "Vim Beginning of Line" (p) nil :exclusive "" ""
  (beginning-of-line-command p))
(def-vim-move "Vim End of Line" (p) nil :exclusive "" ""
  (end-of-line-command p))
(def-vim-move "Vim Goto Line or Top of Buffer" (p) :linewise :exclusive "" ""
  (if p
    (goto-line-command p)
    (beginning-of-buffer-command p))
  (back-to-indentation-command p))

(def-vim-move "Vim Move Over Whole Line" (p) :linewise :exclusive "" ""
  (line-start b-vim-point-before-movement)
  (when (and p (> p 1))
    (line-offset (current-point) (1- p)))
  (line-end (current-point)))

(def-vim-move "Vim Move Over Inner Word" (p) nil :inclusive "" ""
  (move-over-word p :word t))

(def-vim-move "Vim Move Over A Word" (p) nil :exclusive "" ""
  (move-over-word p :word nil))

(def-vim-move "Vim Move Over Inner BigWord" (p) nil :inclusive "" ""
  (move-over-word p :bigword t))

(def-vim-move "Vim Move Over A BigWord" (p) nil :exclusive "" ""
  (move-over-word p :bigword nil))

(defcommand "Vim Revert Buffer" (p) "" ""
  (revert-buffer-command p)
  (clear-undo-command p))

(def-vim-movement-pending "Vim Delete Motion" (begin end) "" ""
  (save-linewise-status)
  (vim-action-over-motion #'kill-region-command begin end))

(def-vim-movement-pending "Vim Change Motion" (begin end) "" ""
  (vim-action-over-motion #'kill-region-command begin end)
  (setf b-vim-movement-pending-ending-mode "Vim Insert"))

(def-vim-movement-pending "Vim Yank Motion" (begin end) "" ""
  (save-linewise-status)
  (vim-action-over-motion #'save-region-command begin end))

;; Basically maps D to d$
(def-vim-change "Vim Kill To End Of Line" (p) "" ""
  (save-linewise-status)
  (vim-delete-motion-command nil)
  (vim-end-of-line-command p))

(def-vim-change "Vim Yank Line" (p) "" ""
  (save-linewise-status)
  (vim-yank-motion-command nil)
  (vim-move-over-whole-line-command p))

(def-vim-change "Vim Put Before" (p) "" ""
  (when *vim-saved-linewise-status*
    (line-start (current-point)))
  (save-excursion
   (un-kill-command p)))

(def-vim-change "Vim Put After" (p) "" ""
  (when *vim-saved-linewise-status*
    (line-offset (current-point) 1 0))
  (save-excursion
   (un-kill-command p)))

(defcommand "Vim Argument Digit" (p) "" ""
  (setf w-vim-collecting-count t)
  (argument-digit-command p))

(defcommand "Vim Beginning of Line or Collect Count" (p) "" ""
  (if w-vim-collecting-count
    (prompt-for-prefix 1 (* p 10))
    (vim-beginning-of-line-command p)))

(def-vim-change "Vim Replace Character" (p) "" ""
  (let ((ch (vim-read-a-character)))
    (with-point ((point (current-point))
                 (end (current-point)))
      ;; Only proceed if there *are* p more characters in the buffer.
      (if (character-offset end (or p 1))
        (progn
          (collect-undo (current-buffer)
            (loop for n below (or p 1) do
                  (setf (next-character point) ch)
                  (character-offset point 1)))
          (character-offset point -1))
        (editor-error)))))

;; FIXME: implement going to the p'th file in the path.  First, need to
;; implement path, though.  :)
(defcommand "Vim Goto File" (p) "" ""
  (declare (ignorable p))
  (let ((file (current-word :filename)))
    ; (format t "file is ~A~%" file)
    ; #+nil
    (when file
      (find-file-command nil (pathname file)))))

(def-vim-move "Vim Find Char Right" (p) nil :inclusive "" ""
  (vim-find-char t p :save t))

(defcommand "Doesnt Work" (p)
  (prompt-for-character* "Character: " :ignored)
  (end-of-line-command p)
  )

(def-vim-move "Vim Find Char Left" (p) nil :exclusive "" ""
  (vim-find-char nil p :save t))

(def-vim-move "Vim Find Till Char Right" (p) nil :inclusive "" ""
  (vim-find-char t p :leave-before t :save t))

(def-vim-move "Vim Find Till Char Left" (p) nil :exclusive "" ""
  (vim-find-char nil p :leave-before t :save t))

(def-vim-move "Vim Repeat Last Find Char" (p) nil :exclusive "" ""
  (vim-repeat-last-find p))

(def-vim-move "Vim Repeat Last Find Char Opposite" (p) nil :exclusive "" ""
  (vim-repeat-last-find p :invert t))

(defun insert-character-at-offset (offset)
  (with-point ((start (current-point))
               (end (current-point)))
    (when (line-offset start offset)
      (move-point end start)
      (character-offset end 1)
      (insert-string (current-point)
                     (points-to-string start end)))))

(defcommand "Vim Insert Character Above Cursor" (p) "" ""
  (declare (ignore p))
  (insert-character-at-offset -1))

(defcommand "Vim Insert Character Below Cursor" (p) "" ""
  (declare (ignore p))
  (insert-character-at-offset 1))

(def-vim-move "Vim To Column N" (p) nil :exclusive "" ""
  (let ((point (current-point)))
    (with-point ((end point))
      (line-start point)
      (line-end end)
      (loop for n below (1- (or p 1))
            while (point< point end)
            do (character-offset point 1)))))

(def-vim-change "Vim Join Lines" (p) "" ""
  (collect-undo (current-buffer)
    (dotimes (n (or p 1))
      (delete-indentation-command 1))))

(def-vim-move "Vim ISearch Forward Regexp" (p) nil :exclusive "" ""
  (isearch-forward-regexp-command p))

(def-vim-move "Vim ISearch Backward Regexp" (p) nil :exclusive "" ""
  (isearch-backward-regexp-command p))

(def-vim-move "Vim Regexp Forward Search" (p) nil :exclusive "" ""
  (regexp-forward-search-command p))

(def-vim-move "Vim Regexp Reverse Search" (p) nil :exclusive "" ""
  (regexp-reverse-search-command p))

(def-vim-move "Vim Find Next" (p) nil :exclusive "" ""
  (dotimes (n (or p 1))
    (regular-expression-search (current-point) editor::*last-search-string*)))

(def-vim-move "Vim Find Previous" (p) nil :exclusive "" ""
  (dotimes (n (or p 1))
    (regular-expression-search (current-point) editor::*last-search-string* :forwardp nil)))

;; Technically this is not a "change" and should not use def-vim-change.
;; FIXME: tried a SAVE-EXCURSION around the undo, but that's not really right.  LW undo
;; leaves the point after the undone text; Vim leaves the point before the undone text.
(defcommand "Vim Undo" (p) "" ""
  (undo-command p))
