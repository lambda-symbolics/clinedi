(in-package #:clinedi)

;;;; -- Editor snapshots --

(defstruct (line-editor-state (:constructor line-editor--make-state))
  "An opaque snapshot of editor text, cursor and history traversal."
  text cursor history index stash stash-cursor)

(defun line-editor-history-navigating-p (editor)
  "Return true when EDITOR is traversing history."
  (not (null (slot-value editor 'history-index))))

(defun line-editor-snapshot (editor)
  "Copy EDITOR's text, cursor and complete history traversal for later restoration."
  (line-editor--make-state
   :text (copy-seq (line-editor-text editor))
   :cursor (line-editor-cursor editor)
   :history (map 'list #'copy-seq (slot-value editor 'history))
   :index (slot-value editor 'history-index)
   :stash (let ((stash (slot-value editor 'history-stash)))
            (and stash (copy-seq stash)))
   :stash-cursor (slot-value editor 'history-stash-cursor)))

(defun line-editor-restore (editor state)
  "Restore a snapshot in EDITOR without exposing its private history representation.

STATE is reusable. Restore history through EDITOR's configured limit, adjusting
the traversal index when an older prefix is discarded. If the recalled entry
falls outside that limit, restore the visible text without history traversal."
  (check-type state line-editor-state)
  (let* ((history (line-editor-state-history state))
         (discarded (max 0 (- (length history) (line-editor-history-limit editor))))
         (index (line-editor-state-index state)))
    (line-editor-set-text editor (copy-seq (line-editor-state-text state))
                          :cursor (line-editor-state-cursor state))
    (setf (slot-value editor 'history)
          (line-editor--make-history history (line-editor-history-limit editor)))
    (when (and index (>= index discarded))
      (setf (slot-value editor 'history-index) (- index discarded)
            (slot-value editor 'history-stash)
            (copy-seq (line-editor-state-stash state))
            (slot-value editor 'history-stash-cursor)
            (line-editor-state-stash-cursor state))))
  editor)


(defun line-editor-replace-history (editor entries)
  "Replace bounded history, restoring the saved draft when traversal is active."
  (let* ((history (line-editor--make-history entries (line-editor-history-limit editor)))
         (navigating-p (line-editor-history-navigating-p editor))
         (text (copy-seq (if navigating-p (slot-value editor 'history-stash)
                            (line-editor-text editor))))
         (cursor (if navigating-p (slot-value editor 'history-stash-cursor)
                     (line-editor-cursor editor))))
    (line-editor-set-text editor text :cursor cursor)
    (setf (slot-value editor 'history) history))
  editor)
