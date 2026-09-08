# Clinedi

Clinedi is a portable Common Lisp line editor for terminal applications. It
separates a Unicode-aware incremental editor from its blocking terminal
frontend, so applications can either feed it semantic events or use it as a
complete interactive input loop.

The editor handles:

- extended grapheme clusters
- terminal-cell layout
- wrapped multiline input
- history navigation
- bracketed paste
- completion presentation
- syntax-highlighting callbacks
- ghost-text suggestions

The application owns shell parsing, completion policy and history persistence.

## Editor snapshots and modal sessions

Use `line-editor-snapshot` and `line-editor-restore` to suspend completion
previews without losing history traversal, its saved draft, or the cursor.
Snapshots are reusable and own their text and history copies. Use
`line-editor-history-navigating-p` to inspect traversal and
`line-editor-replace-history` to replace bounded history while restoring the
original draft. Persist history in the application.

Create a `selection-session` with `make-selection-session`. Supply opaque
`:items`, an `:identity-key`, an `:identity-test`, and a string-valued
`:search-key`. Enable `:search-p` for case-insensitive, whitespace-separated
query terms. Use `selection-session-handle-event` in an existing event loop,
or `run-selection-session` with transport and presentation callbacks.

Read `selection-session-selector` for navigation and viewport state, and
`selection-session-query` for the current query. Replace candidate snapshots
with `selection-session-replace-items`; select a stable designator with
`selection-session-select-id`. Candidate labels may coincide without losing
selection identity.

For the modal loop, supply `:read-event`, `:input-ready-p`, `:poll-interval`,
`:refresh`, `:paint`, and optionally `:call-with-lock`. Query pending resizes
in `:refresh`. Return true after repainting there to omit a duplicate paint.
The loop queries it before readiness and again before event handling.
`:on-event` receives `(event selector)` and returns NIL for default handling,
`:continue`, `(:accept value)`, or `(:cancel)`. Handle `:poll` for background
updates. Keep titles, hints and application-specific actions in the callbacks.
`:on-close` runs on every exit, including a failed `:on-open`.

## Buffered transports and native modes

Use `stream-terminal-create` for buffered stream input and trusted presentation
output. Call `terminal-start`, `terminal-read-event`, `terminal-input-ready-p`,
`terminal-write`, `terminal-flush`, and `terminal-stop`. Plain bursts become one
`:insert` event; multiline bursts become sanitized `:paste` events. Mixed input
retains pending characters between reads. Configure a custom `:event-decoder`
with `(stream &key escape-delay)` and an `:event-prefix-p-function` when an
application-defined prefix must precede multiline-paste classification.
Use `read-paste-burst` to implement literal-paste bindings with explicit idle
and character limits. Choose styling with `:styling-p-function`.

Load the optional `clinedi/posix` system on SBCL, then instantiate
`posix-terminal` with `:input-stream`, `:output-stream`, and
`:input-file-descriptor`. Check the descriptor rather than the stream wrapper
for interactive mode. Read native dimensions with `terminal-file-descriptor-size`.
The transport falls back to line events for non-TTY input. Startup and shutdown
are idempotent. Failed activation rolls back partial protocols and native mode;
shutdown attempts every cleanup and reports the first failure as `terminal-error`.
For another native backend, implement `terminal-capture-input-mode`,
`terminal-activate-input-mode`, and `terminal-restore-input-mode` on a subclass.

## Loading

Clinedi is an ASDF system. It uses
[cl-colorist](https://github.com/lambda-symbolics/cl-colorist) for ANSI text styling
and control-sequence parsing.

```lisp
(ql:quickload :clinedi)
```

For local Quicklisp development, either place the checkout directly below
`~/quicklisp/local-projects/`, or add its parent directory before registering
local projects:

```lisp
(pushnew #P"/root/common-lisp/"
         ql:*local-project-directories*
         :test #'equal)
(ql:register-local-projects)
```

## Incremental editor

```lisp
(let ((editor (clinedi:make-line-editor :history '("git status"))))
  (clinedi:line-editor-handle-event editor '(:insert "echo 猫"))
  (clinedi:line-editor-handle-event editor :left)
  (clinedi:line-editor-text editor))
```

`line-editor-handle-event` accepts semantic editing events and returns an
action plus an optional payload. This API is suitable for event-driven terminal
UIs that own their repaint loop.

- `:end-of-input` represents Ctrl-D and follows the usual delete-or-EOF behavior
- `:stream-end` represents physical stream EOF; handling it returns the
  `:end-of-input` action and keeps partial text
- `:insert-newline` adds an explicit newline
- Arrow events return `:up` or `:down` so event-driven callers can invoke
  `line-editor-move-vertical` with their current terminal width and prompt
  width, falling back to explicit history events when it reports no adjacent
  visual row

Pass `:history-match-function` to the constructor to filter those history
events. The function receives the complete draft captured when traversal begins
and each candidate entry. Down past the newest match restores that draft and
its original cursor. An empty draft traverses every entry.

Pass `:word-delimiter-mode-p t` to make Ctrl-Left, Ctrl-Right and
Ctrl-Backspace stop at delimiters as well as whitespace. The default delimiter
list is `-`, `_`, `/`, `.`, and `:`; override it with `:word-delimiters`.
`line-editor-toggle-word-delimiter-mode` and the built-in
`:toggle-word-delimiter-mode` command switch the mode while an editor is active.

## Programmable keymaps

Clinedi decodes terminal input into semantic events, then resolves each event
through the editor's keymap. `default-line-editor-keymap` returns a fresh map
with the standard behavior, so an application can customize its own copy:

```lisp
(defparameter *application-keymap*
  (clinedi:default-line-editor-keymap))

;; Give Up and Down unconditional history behavior.
(clinedi:keymap-bind *application-keymap* :up :history-previous)
(clinedi:keymap-bind *application-keymap* :down :history-next)

(clinedi:edit-line "> " :keymap *application-keymap*)
```

A binding maps an event to a built-in semantic command, a function, or a
non-keyword fbound symbol. Custom commands receive the editor and the original
event, and return the same action and optional payload pair as
`line-editor-handle-event`. They can call `line-editor-execute-command` to reuse
built-in behavior. `line-editor-command-for-event` exposes resolution separately
for event loops that need to inspect a command before executing it.

Keymaps support parent fallback. For a compound event such as
`(:insert "x")`, lookup checks that exact event, then `:insert`, before moving
to the parent. `keymap-unbind` removes a local binding and reveals its parent;
binding an event to `nil` masks the parent. `copy-keymap` copies every map and
binding table in the parent chain, while `keymap-bindings` returns a detached
snapshot of one map's local entries.

## Candidate selection

`clinedi:selector` is application-neutral navigation and viewport state for
pickers and interactive completions. Candidate values are opaque, so an
application can use strings for file completion, model records for a picker,
or any other values while retaining control of filtering, labels, styling and
acceptance policy.

A selector can arrange candidates vertically or in a row-major grid that
measures candidate cell widths against the available terminal width.

- Arrow keys navigate that geometry
- Tab and Shift-Tab cycle candidates forward and backward
- Enter accepts
- ordinary editing input dismisses the chooser while returning the selected
  value

```lisp
(let ((selector (clinedi:make-selector
                 :items '("source/" "source/main.lisp")
                 :arrangement :grid)))
  (clinedi:selector-arrange selector 80
                           :width-function #'clinedi:text-cell-width)
  (clinedi:selector-handle-event selector :history-next)
  (clinedi:selector-selected-item selector))
```

## Blocking frontend

`clinedi:edit-line` owns key decoding and repainting while delegating terminal
raw mode, terminal size, completion, highlighting and suggestions to callbacks.
This keeps terminal policy and application semantics outside the library. The
terminal-size callback is refreshed while input is active, so wrapped text,
ghost suggestions and completion layouts follow terminal resizes. Pass
`:keymap` to customize command dispatch.

Ambiguous completions open a live selector below the edited text. The default
`:completion-arrangement :grid` fits as many measured columns as the terminal
width permits and collapses to a vertical list in narrow terminals. Callers can
request `:vertical` explicitly.

`live-region-append-and-present` appends and replacement-repaints in one
terminal write and flush for streaming applications. An optional `maximum-rows`
keeps long multiline content inside a cursor-following viewport while retaining
the complete presentation for later repainting. `live-region-resize` reconciles
the painted rows with terminal reflow before retracting them. Pass
`:repaint-p nil` when the application will immediately call
`live-region-present`.

Applications that manage their own presentation can use `clinedi:screen-window`
to obtain grapheme-safe start, end, and cursor indexes for the same bounded
multiline viewport behavior.

## Semantic prompt markers

`clinedi:semantic-prompt-marker-sequence` returns OSC 133 controls for terminal
shell integration:

- `:prompt-start`
- `:input-start`
- `:execution-start`
- `:command-finished`, with an optional nonnegative status defaulting to zero

The application chooses when these trusted controls are written and flushed.

## Tests

```sh
./check
```

Part of the [Lambda Symbolics library shelf](https://www.lambda-symbolics.com/libraries).
