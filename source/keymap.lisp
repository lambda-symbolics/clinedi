;;;; -- Programmable Keymaps --

(in-package #:clinedi)

(defclass keymap ()
  ((parent
    :initarg :parent
    :initform nil
    :reader keymap-parent
    :type (or null keymap)
    :documentation "Optional fallback keymap consulted after local bindings.")
   (binding-table
    :initform (make-hash-table :test #'equal)
    :reader keymap--binding-table
    :type hash-table
    :documentation "Local event-to-command bindings compared with EQUAL."))
  (:documentation
   "A programmable mapping from semantic input events to command designators.

Keymaps may inherit from a parent. A lookup checks an exact local event first,
then the head of a local compound event, and finally repeats those checks in
the parent chain."))

(defun make-keymap (&key parent bindings)
  "Create a keymap with optional PARENT and local BINDINGS.

PARENT must be another keymap or NIL. BINDINGS is an association list whose
entries are dotted EVENT-to-COMMAND pairs. Events and command values are
retained as application-owned objects."
  (unless (or (null parent) (typep parent 'keymap))
    (error 'type-error
           :datum parent
           :expected-type '(or null keymap)))
  (let ((keymap (make-instance 'keymap :parent parent)))
    (dolist (binding bindings)
      (unless (consp binding)
        (error 'type-error :datum binding :expected-type 'cons))
      (keymap-bind keymap (first binding) (rest binding)))
    keymap))

(defun keymap-bindings (keymap)
  "Return a detached association list of KEYMAP's local bindings.

The list and its binding conses are fresh. Event keys and command values remain
opaque and are not copied. Parent bindings are not included."
  (check-type keymap keymap)
  (loop for event being the hash-keys of (keymap--binding-table keymap)
          using (hash-value command)
        collect (cons event command)))

(defun keymap-bind (keymap event command)
  "Bind EVENT locally to COMMAND in KEYMAP and return KEYMAP.

An exact compound event can override its event-head binding. COMMAND remains
opaque to the keymap and is interpreted by the component performing lookup."
  (check-type keymap keymap)
  (setf (gethash event (keymap--binding-table keymap)) command)
  keymap)

(defun keymap-unbind (keymap event)
  "Remove KEYMAP's local binding for EVENT and return KEYMAP.

Removing a local binding reveals a matching parent binding, when present."
  (check-type keymap keymap)
  (remhash event (keymap--binding-table keymap))
  keymap)

(defun keymap--local-lookup (keymap event)
  "Return KEYMAP's local command and whether EVENT or its head was found."
  (multiple-value-bind (command present-p)
      (gethash event (keymap--binding-table keymap))
    (if (or present-p (not (consp event)))
        (values command present-p)
        (gethash (first event) (keymap--binding-table keymap)))))

(defun keymap-lookup (keymap event)
  "Look up EVENT in KEYMAP and return COMMAND and a found flag.

Each map checks the exact event before the head of a compound event. A parent
is consulted only when neither local form is bound. A binding whose command is
NIL is therefore distinct from a missing binding and masks its parent."
  (check-type keymap keymap)
  (loop for current = keymap then (keymap-parent current)
        while current
        do (multiple-value-bind (command present-p)
               (keymap--local-lookup current event)
             (when present-p
               (return (values command t))))
        finally (return (values nil nil))))

(defun copy-keymap (keymap)
  "Return a detached copy of KEYMAP and its complete parent chain.

Every keymap and binding table in the returned chain is fresh. Event keys and
command values remain opaque and are retained."
  (check-type keymap keymap)
  (let ((copy (make-keymap
               :parent (and (keymap-parent keymap)
                            (copy-keymap (keymap-parent keymap))))))
    (maphash (lambda (event command)
               (keymap-bind copy event command))
             (keymap--binding-table keymap))
    copy))

(defun default-line-editor-keymap ()
  "Return a fresh keymap containing every standard line-editor binding."
  (make-keymap
   :bindings
   '((:insert . :insert)
     (:paste . :paste)
     (:insert-newline . :insert-newline)
     (:line . :line)
     (:left . :left)
     (:right . :right)
     (:word-left . :word-left)
     (:word-right . :word-right)
     (:home . :home)
     (:end . :end)
     (:backspace . :backspace)
     (:delete . :delete)
     (:history-previous . :history-previous)
     (:history-next . :history-next)
     (:kill-to-end . :kill-to-end)
     (:kill-line . :kill-line)
     (:kill-word . :kill-word)
     (:kill-word-right . :kill-word-right)
     (:complete . :complete)
     (:complete-previous . :complete-previous)
     (:up . :up)
     (:down . :down)
     (:submit . :submit)
     (:interrupt . :interrupt)
     (:end-of-input . :end-of-input)
     (:stream-end . :stream-end)
     (:escape . :escape)
     (:clear-screen . :clear-screen))))
