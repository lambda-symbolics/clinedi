;;;; -- ANSI presentation --

(in-package #:clinedi)

(defconstant +escape-character+ (code-char 27)
  "The ASCII escape character used in terminal control sequences.")

(defvar *presentation-enabled* t
  "Whether ANSI presentation helpers may emit terminal control sequences.")

(defun ansi--color-designator (color)
  "Return COLOR when Colorist recognizes it, otherwise basic white."
  (if (or (cl-colorist:color-p color)
          (member color (cl-colorist:basic-color-names)))
      color
      :white))

(defun ansi-colorize (text color &key bold)
  "Wrap TEXT in the SGR sequence for COLOR, optionally BOLD.
Return TEXT unchanged when presentation is disabled."
  (if *presentation-enabled*
      (cl-colorist:paint text
                         :foreground (ansi--color-designator color)
                         :bold bold
                         :level :indexed)
      text))

(defun ansi-reverse-video (text)
  "Wrap TEXT in reverse video, unless presentation is disabled."
  (if *presentation-enabled*
      (cl-colorist:paint text :reverse t :level :basic)
      text))

(defun ansi-cursor-up (lines)
  "Return the sequence moving the cursor LINES up, or an empty string."
  (if (and *presentation-enabled* (plusp lines))
      (format nil "~c[~dA" +escape-character+ lines)
      ""))

(defun ansi-cursor-down (lines)
  "Return the sequence moving the cursor LINES down, or an empty string."
  (if (and *presentation-enabled* (plusp lines))
      (format nil "~c[~dB" +escape-character+ lines)
      ""))

(defun ansi-cursor-column (column)
  "Return the sequence moving the cursor to zero-based COLUMN."
  (if *presentation-enabled*
      (format nil "~c[~dG" +escape-character+ (1+ column))
      ""))

(defun ansi-cursor-hide ()
  "Return the sequence that hides the terminal cursor."
  (if *presentation-enabled*
      (format nil "~c[?25l" +escape-character+)
      ""))

(defun ansi-cursor-show ()
  "Return the sequence that makes the terminal cursor visible."
  (if *presentation-enabled*
      (format nil "~c[?25h" +escape-character+)
      ""))

(defun ansi-clear-below ()
  "Return the sequence clearing from the cursor to the screen end."
  (if *presentation-enabled*
      (format nil "~c[J" +escape-character+)
      ""))

(defun ansi-clear-line-right ()
  "Return the sequence clearing from the cursor to the line end."
  (if *presentation-enabled*
      (format nil "~c[K" +escape-character+)
      ""))

(defun ansi-clear-screen ()
  "Return the sequence clearing the whole screen and homing the cursor."
  (if *presentation-enabled*
      (format nil "~c[H~c[2J" +escape-character+ +escape-character+)
      ""))

(defun semantic-prompt-marker-sequence (marker &optional (status 0))
  "Return one OSC 133 control for semantic prompt MARKER and completion STATUS."
  (check-type status (integer 0))
  (let ((payload
          (ecase marker
            (:prompt-start "A")
            (:input-start "B")
            (:execution-start "C")
            (:command-finished (format nil "D;~D" status)))))
    (format nil "~c]133;~a~c~c"
            +escape-character+
            payload
            +escape-character+
            #\\)))

(defun ansi-strip (string)
  "Remove ANSI control sequences from STRING."
  (cl-colorist:strip-ansi string))

(defun ansi--control-payload-start (string start)
  "Return the index after the introducer of the control beginning at START."
  (if (char= (char string start) +escape-character+)
      (+ start 2)
      (1+ start)))

(defun ansi--sgr-control-p (string start end)
  "Return true when STRING's control between START and END selects graphic rendition."
  (and (>= (- end start) 2)
       (char= (char string (1- end)) #\m)
       (or (and (char= (char string start) +escape-character+)
                (< (1+ start) end)
                (char= (char string (1+ start)) #\[))
           (= (char-code (char string start)) #x9b))))

(defun ansi--sgr-reset-p (string start end)
  "Return true when the SGR control between START and END resets every attribute."
  (let ((parameters-start (ansi--control-payload-start string start))
        (parameters-end (1- end)))
    (or (>= parameters-start parameters-end)
        (and (= (- parameters-end parameters-start) 1)
             (char= (char string parameters-start) #\0)))))

(defun ansi--hyperlink-control-p (string start end)
  "Return true when STRING's control between START and END is an OSC 8 hyperlink."
  (let ((payload-start (ansi--control-payload-start string start)))
    (and (or (and (char= (char string start) +escape-character+)
                  (< (1+ start) end)
                  (char= (char string (1+ start)) #\]))
             (= (char-code (char string start)) #x9d))
         (< (1+ payload-start) end)
         (char= (char string payload-start) #\8)
         (char= (char string (1+ payload-start)) #\;))))

(defun ansi--hyperlink-close-p (string start end)
  "Return true when the OSC 8 control between START and END ends a hyperlink.

A hyperlink ends when its URI is empty. The URI follows the second semicolon
and runs to the string terminator, which is ESC backslash, BEL or C1 ST."
  (let* ((payload-start (ansi--control-payload-start string start))
         (terminator-start (if (and (>= (- end start) 2)
                                    (char= (char string (1- end)) #\\)
                                    (char= (char string (- end 2)) +escape-character+))
                               (- end 2)
                               (1- end)))
         (separator (position #\; string :start (+ payload-start 2) :end end)))
    (or (null separator)
        (>= (1+ separator) terminator-start))))

(defun ansi--visible-slices (string ranges)
  "Return STRING's styled slices for RANGES, one string per (START END) pair.

START and END index ANSI-stripped characters and the ranges ascend. Every slice
stands alone: it opens with the graphic rendition and hyperlink in force at its
first visible character, keeps the controls that occur inside it, and closes
whatever it leaves open. Rows therefore paint correctly alone or in sequence
without one row's controls being copied into every other row, and STRING is
scanned once however many ranges there are."
  (let ((slices nil)
        (active-sgr nil)
        (active-hyperlink nil)
        (index 0)
        (visible-index 0)
        (length (length string)))
    (labels ((note-control (start end)
               "Track the presentation state a control between START and END leaves."
               (cond
                 ((ansi--sgr-control-p string start end)
                  (if (ansi--sgr-reset-p string start end)
                      (setf active-sgr nil)
                      (push (subseq string start end) active-sgr)))
                 ((ansi--hyperlink-control-p string start end)
                  (setf active-hyperlink
                        (if (ansi--hyperlink-close-p string start end)
                            nil
                            (subseq string start end))))))

             (open-slice (slice)
               "Write the state in force so the slice starts as STRING would."
               (dolist (control (reverse active-sgr))
                 (write-string control slice))
               (when active-hyperlink
                 (write-string active-hyperlink slice)))

             (close-slice (slice)
               "Neutralize whatever the slice leaves open."
               (when active-hyperlink
                 (format slice "~c]8;;~c\\" +escape-character+ +escape-character+))
               (when active-sgr
                 (format slice "~c[0m" +escape-character+)))

             (scan-to (target slice)
               "Consume STRING up to visible TARGET, copying into SLICE when given."
               (loop while (and (< index length) (< visible-index target))
                     do (let ((control-end (cl-colorist:ansi-control-end string index)))
                          (if control-end
                              (progn
                                (note-control index control-end)
                                (when slice
                                  (write-string string slice
                                                :start index :end control-end))
                                (setf index control-end))
                              (progn
                                (when slice
                                  (write-char (char string index) slice))
                                (incf visible-index)
                                (incf index)))))))
      (loop for (start end) in ranges
            do (push (if (= start end)
                         ""
                         (with-output-to-string (slice)
                           (scan-to start nil)
                           (open-slice slice)
                           (scan-to end slice)
                           (close-slice slice)))
                     slices)))
    (nreverse slices)))

(defun ansi--visible-slice (string start end)
  "Return STRING's standalone styled slice for visible characters START to END."
  (first (ansi--visible-slices string (list (list start end)))))

(defun wrap-styled-text (text display maximum-cells)
  "Return TEXT and trusted styled DISPLAY as paired word-wrapped rows.

DISPLAY must have exactly TEXT as its ANSI-stripped visible content. Every
returned pair contains a plain row followed by its styled equivalent. Explicit
newlines, including empty and trailing lines, become row boundaries. Wrapping
prefers spaces, preserves graphemes and retains the presentation of every
visible character."
  (check-type text string)
  (check-type display string)
  (check-type maximum-cells integer)
  (unless (string= text (ansi-strip display))
    (error "Styled text does not preserve its plain visible content."))
  (let ((ranges (unicode--wrap-text-ranges text maximum-cells)))
    (loop for (start end) in ranges
          for slice in (ansi--visible-slices display ranges)
          collect (list (subseq text start end) slice))))

(defun ansi-display-width (string)
  "Return the number of visible terminal cells STRING occupies."
  (text-cell-width (ansi-strip string)))
