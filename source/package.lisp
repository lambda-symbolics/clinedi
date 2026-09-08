;;;; -- Package definition --

(defpackage #:clinedi
  (:use #:cl)
  (:export
   #:terminal
   #:stream-terminal
   #:posix-terminal
   #:stream-terminal-create
   #:terminal-rows
   #:terminal-columns
   #:terminal-interactive-p
   #:terminal-styled-p
   #:terminal-started-p
   #:terminal-set-dimensions
   #:terminal-start
   #:terminal-stop
   #:terminal-read-event
   #:terminal-input-ready-p
   #:terminal-write
   #:terminal-flush
   #:stream-terminal-input-stream
   #:stream-terminal-output-stream
   #:stream-terminal-input-file-descriptor
   #:stream-terminal-pending-input-stream
   #:stream-terminal-saved-terminal-mode
   #:terminal-capture-input-mode
   #:terminal-activate-input-mode
   #:terminal-restore-input-mode
   #:terminal-file-descriptor-size
   #:terminal-error
   #:terminal-error-message
   #:terminal-error-operation
   #:terminal-error-cause
   #:read-paste-burst
   #:terminal-read-editing-event
   #:terminal-bracketed-paste-enable-sequence
   #:terminal-bracketed-paste-disable-sequence
   #:terminal-keyboard-enhancement-enable-sequence
   #:terminal-keyboard-enhancement-disable-sequence
   #:*terminal-default-rows*
   #:*terminal-default-columns*
   #:*terminal-escape-character*
   #:*terminal-escape-delay-seconds*
   #:*terminal-unbracketed-paste-coalesce-seconds*
   #:*terminal-unbracketed-paste-maximum-characters*

   #:line-editor-state #:line-editor-history-navigating-p
   #:line-editor-snapshot #:line-editor-restore #:line-editor-replace-history
   #:selection-session #:make-selection-session
   #:selection-session-selector #:selection-session-query
   #:selection-session-selected-id #:selection-session-select-id
   #:selection-session-replace-items #:selection-session-handle-event
   #:run-selection-session
   ;; Identity
   #:*clinedi-version*

   ;; Unicode text geometry
   #:grapheme-next-boundary
   #:grapheme-previous-boundary
   #:grapheme-boundary-at-or-after
   #:grapheme-cell-width
   #:text-cell-width
   #:text-cell-prefix
   #:text-cell-window
   #:wrap-text
   #:sanitize-text

   ;; ANSI presentation
   #:*presentation-enabled*
   #:ansi-colorize
   #:ansi-reverse-video
   #:ansi-cursor-up
   #:ansi-cursor-down
   #:ansi-cursor-column
   #:ansi-cursor-hide
   #:ansi-cursor-show
   #:ansi-clear-below
   #:ansi-clear-line-right
   #:ansi-clear-screen
   #:semantic-prompt-marker-sequence
   #:ansi-strip
   #:ansi-display-width
   #:wrap-styled-text
   #:wrap-styled-editor-text

   ;; Programmable keymaps
   #:keymap
   #:make-keymap
   #:copy-keymap
   #:keymap-parent
   #:keymap-bindings
   #:keymap-bind
   #:keymap-unbind
   #:keymap-lookup
   #:default-line-editor-keymap

   ;; Incremental editing
   #:line-editor
   #:make-line-editor
   #:line-editor-create
   #:line-editor-text
   #:*default-word-delimiters*
   #:line-editor-cursor
   #:line-editor-word-delimiter-mode-p
   #:line-editor-word-delimiters
   #:line-editor-toggle-word-delimiter-mode
   #:line-editor-history
   #:line-editor-history-limit
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

   ;; Candidate selection
   #:selector
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

   ;; Input and layout
   #:enable-keyboard-enhancement
   #:disable-keyboard-enhancement
   #:enable-bracketed-paste
   #:disable-bracketed-paste
   #:read-event
   #:screen-position
   #:screen-window
   #:write-display
   #:render-line
   #:print-candidates
   #:split-prompt

   ;; Scrollback-safe live application regions
   #:live-region
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
   #:with-live-region-suspended

   ;; Blocking frontend
   #:edit-line)
  (:documentation
   "Portable editing state, Unicode terminal geometry and line input."))

(in-package #:clinedi)

(defparameter *clinedi-version* "0.1.0"
  "The version of the loaded Clinedi system.")
