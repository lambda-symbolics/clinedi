;;;; -- Test package --

(defpackage #:clinedi/tests
  (:use #:cl)
  (:import-from #:clinedi
                #:grapheme-next-boundary
                #:grapheme-previous-boundary
                #:grapheme-boundary-at-or-after
                #:grapheme-cell-width
                #:text-cell-width
                #:text-cell-prefix
                #:text-cell-window
                #:wrap-text
                #:sanitize-text
                #:*presentation-enabled*
                #:ansi-colorize
                #:ansi-cursor-hide
                #:ansi-cursor-show
                #:ansi-cursor-up
                #:ansi-cursor-column
                #:ansi-clear-below
                #:ansi-clear-line-right
                #:semantic-prompt-marker-sequence
                #:ansi-strip
                #:ansi-display-width
                  #:wrap-styled-text
                #:make-keymap
                #:copy-keymap
                #:keymap-parent
                #:keymap-bindings
                #:keymap-bind
                #:keymap-unbind
                #:keymap-lookup
                #:default-line-editor-keymap
                #:make-line-editor
                #:line-editor-create
                #:line-editor-text
                #:line-editor-cursor
                #:line-editor-history
                #:line-editor-history-match-function
                #:line-editor-keymap
                #:line-editor-set-text
                #:line-editor-clear
                #:line-editor-add-history
                #:line-editor-command-for-event
                #:line-editor-execute-command
                #:line-editor-handle-event
                #:line-editor-move-vertical
                #:line-editor-render
                #:make-selector
                #:selector-items
                #:selector-selection
                #:selector-visible-count
                #:selector-arrangement
                #:selector-column-count
                #:selector-set-items
                #:selector-selected-item
                #:selector-move
                #:selector-window
                #:selector-arrange
                #:selector-handle-event
                #:enable-bracketed-paste
                #:disable-bracketed-paste
                #:read-event
                #:screen-position
                #:screen-window
                #:write-display
                #:render-line
                #:print-candidates
                #:split-prompt
                #:make-live-region
                #:live-region-columns
                #:live-region-maximum-rows
                #:live-region-row-count
                #:live-region-cursor-row
                #:live-region-cursor-column
                #:live-region-cursor-visible-p
                #:live-region-set-cursor-visible
                #:live-region-visible-p
                #:live-region-present
                #:live-region-append-and-present
                #:live-region-append
                #:live-region-suspend
                #:live-region-resume
                #:live-region-dismiss
                #:live-region-resize
                #:call-with-live-region-suspended
                #:edit-line)
  (:export #:run-tests))

(in-package #:clinedi/tests)

(defvar *test-failures* nil
  "Descriptions of failures from the current test run.")

(defun check-equal (name expected actual)
  "Record a failure under NAME unless EXPECTED and ACTUAL are EQUAL."
  (unless (equal expected actual)
    (push (format nil "~a: expected ~s, got ~s" name expected actual)
          *test-failures*))
  (values))

(defun check-true (name value)
  "Record a failure under NAME unless VALUE is true."
  (unless value
    (push (format nil "~a: expected a true value" name) *test-failures*))
  (values))
