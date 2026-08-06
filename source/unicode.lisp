;;;; -- Unicode terminal geometry --

(in-package #:clinedi)

#+ccl
(defconstant +unicode-lc-ctype-mask+
  #+netbsd 4
  #-netbsd 1
  "NEWLOCALE mask selecting the LC_CTYPE category.")

#+ccl
(defvar *unicode-cell-locale-active* nil
  "True while the current thread is inside a terminal-width query.")

#+(and ccl netbsd)
(defvar *unicode-cell-locale* nil
  "Private locale used by the current NetBSD terminal-width query.")


;;; Character properties

(defun unicode--control-code-p (code)
  "True when CODE is a C0, DEL or C1 control code."
  (or (< code 32)
      (<= 127 code 159)))

(defun unicode--zero-width-code-p (code)
  "True when CODE is a terminal-formatting character with no cells."
  ;; Common Lisp has no portable terminal-width database. These are the
  ;; control, format, variation-selector and tag ranges that libc may report as
  ;; unprintable even though they contribute zero cells to adjacent text.
  (or (unicode--control-code-p code)
      (= code #x034f)
      (= code #x061c)
      (= code #x115f)
      (= code #x1160)
      (= code #x17b4)
      (= code #x17b5)
      (= code #x180e)
      (= code #x200b)
      (<= #x200c code #x200f)
      (<= #x202a code #x202e)
      (<= #x2060 code #x206f)
      (= code #x3164)
      (<= #xfe00 code #xfe0f)
      (= code #xfeff)
      (= code #xffa0)
      (<= #x1bca0 code #x1bca3)
      (<= #x1d173 code #x1d17a)
      (<= #xe0000 code #xe01ef)))

(defun unicode--wide-code-p (code)
  "True when CODE is in a stable wide-character fallback range."
  ;; This compact wcwidth-style table is only a fallback when an implementation
  ;; or libc cannot classify a character. It covers Hangul, CJK, fullwidth
  ;; forms, emoji and the supplementary CJK planes.
  (or (<= #x1100 code #x115f)
      (= code #x2329)
      (= code #x232a)
      (and (<= #x2e80 code #xa4cf) (/= code #x303f))
      (<= #xac00 code #xd7a3)
      (<= #xf900 code #xfaff)
      (<= #xfe10 code #xfe19)
      (<= #xfe30 code #xfe6f)
      (<= #xff00 code #xff60)
      (<= #xffe0 code #xffe6)
      (<= #x1f300 code #x1faff)
      (<= #x20000 code #x3fffd)))

(defun unicode--regional-indicator-p (character)
  "True when CHARACTER is a regional-indicator flag component."
  (<= #x1f1e6 (char-code character) #x1f1ff))

(defun unicode--emoji-modifier-p (character)
  "True when CHARACTER is an emoji skin-tone modifier."
  (<= #x1f3fb (char-code character) #x1f3ff))

(defun unicode--variation-selector-p (character)
  "True when CHARACTER is a Unicode variation selector."
  (let ((code (char-code character)))
    (or (<= #xfe00 code #xfe0f)
        (<= #xe0100 code #xe01ef))))

(defun unicode--variation-selector-16-p (character)
  "True when CHARACTER requests emoji presentation."
  (= (char-code character) #xfe0f))

(defun unicode--zero-width-joiner-p (character)
  "True when CHARACTER joins adjacent emoji or script glyphs."
  (= (char-code character) #x200d))

(defun unicode--extended-pictographic-p (character)
  "True when CHARACTER is in a commonly supported emoji-symbol range."
  (let ((code (char-code character)))
    ;; Unicode grapheme rule GB11 needs the Extended_Pictographic property, but
    ;; Common Lisp exposes no portable query for it. The BMP entries enumerate
    ;; emoji symbols and the supplementary ranges cover emoji blocks. Regional
    ;; indicators and skin-tone modifiers live inside those broad ranges but
    ;; have different grapheme properties, so they are explicitly excluded.
    (and (not (unicode--regional-indicator-p character))
         (not (unicode--emoji-modifier-p character))
         (or (= code #x00a9)
             (= code #x00ae)
             (= code #x203c)
             (= code #x2049)
             (= code #x2122)
             (= code #x2139)
             (<= #x2194 code #x2199)
             (<= #x21a9 code #x21aa)
             (<= #x231a code #x231b)
             (= code #x2328)
             (= code #x23cf)
             (<= #x23e9 code #x23f3)
             (<= #x23f8 code #x23fa)
             (= code #x24c2)
             (<= #x25aa code #x25ab)
             (= code #x25b6)
             (= code #x25c0)
             (<= #x25fb code #x25fe)
             (<= #x2600 code #x27bf)
             (<= #x2934 code #x2935)
             (<= #x2b05 code #x2b07)
             (<= #x2b1b code #x2b1c)
             (= code #x2b50)
             (= code #x2b55)
             (= code #x3030)
             (= code #x303d)
             (= code #x3297)
             (= code #x3299)
             (<= #x1f000 code #x1faff)
             (<= #x1fc00 code #x1fffd)))))

(defun unicode--prepend-p (character)
  "True when CHARACTER has the grapheme Prepend property."
  (let ((code (char-code character)))
    ;; GraphemeBreakProperty.txt has a small, sparse Prepend class. Keeping its
    ;; ranges here avoids an implementation-specific Unicode dependency.
    (or (<= #x0600 code #x0605)
        (= code #x06dd)
        (= code #x070f)
        (<= #x0890 code #x0891)
        (= code #x08e2)
        (= code #x0d4e)
        (= code #x110bd)
        (= code #x110cd)
        (<= #x111c2 code #x111c3)
        (= code #x1193f)
        (= code #x11941)
        (= code #x11a3a)
        (<= #x11a84 code #x11a89)
        (= code #x11d46)
        (= code #x11f02)
        (<= #x13430 code #x1343f))))

(defun unicode--hangul-class (character)
  "Return CHARACTER's Hangul grapheme class, or NIL."
  (let ((code (char-code character)))
    ;; Hangul syllable composition is algorithmic: leading consonants (L),
    ;; vowels (V), trailing consonants (T), and precomposed LV/LVT syllables.
    (cond ((or (<= #x1100 code #x115f)
               (<= #xa960 code #xa97c))
           ':l)
          ((or (<= #x1160 code #x11a7)
               (<= #xd7b0 code #xd7c6))
           ':v)
          ((or (<= #x11a8 code #x11ff)
               (<= #xd7cb code #xd7fb))
           ':t)
          ((<= #xac00 code #xd7a3)
           (if (zerop (mod (- code #xac00) 28)) ':lv ':lvt))
          (t
           nil))))

#+ccl
(defun unicode--make-cell-locale ()
  "Create a private UTF-8 LC_CTYPE locale for CCL width queries."
  (dolist (name '("C.UTF-8" "C.utf8" "en_US.UTF-8" "")
                (ccl:%null-ptr))
    (ccl::with-utf-8-cstr (encoded name)
      (let ((locale
              (ccl:external-call "newlocale"
                                 :int +unicode-lc-ctype-mask+
                                 :address encoded
                                 :address (ccl:%null-ptr)
                                 :address)))
        (unless (ccl:%null-ptr-p locale)
          (return locale))))))

#+(and ccl netbsd)
(defun unicode--call-with-created-cell-locale (function locale)
  "Call FUNCTION with LOCALE bound for NetBSD's WCWIDTH_L, then release it.

NetBSD has locale objects but no USELOCALE. Keeping the locale explicit avoids
mutating process-global state when editor threads measure text concurrently."
  (unwind-protect
       (let ((*unicode-cell-locale-active* t)
             (*unicode-cell-locale* locale))
         (funcall function))
    (ccl:external-call "freelocale"
                       :address locale
                       :void)))

#+(and ccl (not netbsd))
(defun unicode--call-with-created-cell-locale (function locale)
  "Call FUNCTION with the native thread using LOCALE, then release it."
  (let ((previous
          (ccl:external-call "uselocale"
                             :address locale
                             :address)))
    (unwind-protect
         (let ((*unicode-cell-locale-active* t))
           (funcall function))
      (unless (ccl:%null-ptr-p previous)
        (ccl:external-call "uselocale"
                           :address previous
                           :address))
      (ccl:external-call "freelocale"
                         :address locale
                         :void))))

#+ccl
(defun unicode--call-with-cell-locale (function)
  "Call FUNCTION using a private UTF-8 locale for native width queries."
  (if *unicode-cell-locale-active*
      (funcall function)
      (let ((locale (unicode--make-cell-locale)))
        (if (ccl:%null-ptr-p locale)
            (let ((*unicode-cell-locale-active* t))
              (funcall function))
            (unicode--call-with-created-cell-locale function locale)))))

#-ccl
(defun unicode--call-with-cell-locale (function)
  "Call FUNCTION directly on implementations with native Unicode data."
  (funcall function))

(defun unicode--combining-character-p (character)
  "True when CHARACTER is a combining mark recognized by the implementation."
  #+sbcl
  (not (null (member (sb-unicode:general-category character)
                     '(:mn :me)
                     :test #'eq)))
  #+ccl
  (ccl::is-combinable character)
  #-(or ccl sbcl)
  (let ((code (char-code character)))
    (or (<= #x0300 code #x036f)
        (<= #x1ab0 code #x1aff)
        (<= #x1dc0 code #x1dff)
        (<= #x20d0 code #x20ff)
        (<= #xfe20 code #xfe2f))))

(defun unicode--spacing-mark-p (character)
  "True when CHARACTER is a spacing combining mark."
  #+sbcl
  (eq (sb-unicode:general-category character) ':mc)
  #+ccl
  (and (null (unicode--hangul-class character))
       (ccl::is-combinable character)
       t)
  #-(or ccl sbcl)
  nil)

(defun unicode--character-cell-width (character)
  "Return CHARACTER's cell width inside an active width query."
  #+ccl
  (let* ((code (char-code character))
         (width
           #+netbsd
           (if *unicode-cell-locale*
               (ccl:external-call "wcwidth_l"
                                  :unsigned-int code
                                  :address *unicode-cell-locale*
                                  :int)
               (ccl:external-call "wcwidth"
                                  :unsigned-int code
                                  :int))
           #-netbsd
           (ccl:external-call "wcwidth"
                              :unsigned-int code
                              :int)))
    (cond ((not (minusp width))
           width)
          ((unicode--zero-width-code-p code)
           0)
          ((ccl::is-combinable character)
           0)
          ((unicode--wide-code-p code)
           2)
          (t
           1)))
  #+sbcl
  (let ((code (char-code character)))
    (cond ((unicode--zero-width-code-p code)
           0)
          ((unicode--combining-character-p character)
           0)
          ((member (sb-unicode:east-asian-width character)
                   '(:w :f)
                   :test #'eq)
           2)
          ((unicode--wide-code-p code)
           2)
          (t
           1)))
  #-(or ccl sbcl)
  (let ((code (char-code character)))
    (cond ((or (unicode--zero-width-code-p code)
               (unicode--combining-character-p character))
           0)
          ((unicode--wide-code-p code)
           2)
          (t
           1))))

(defun unicode--grapheme-control-p (character)
  "True when CHARACTER forces a grapheme break."
  (unicode--control-code-p (char-code character)))

(defun unicode--grapheme-extend-p (character)
  "True when CHARACTER remains attached to a preceding grapheme."
  (and (null (unicode--hangul-class character))
       (not (unicode--zero-width-joiner-p character))
       (not (unicode--grapheme-control-p character))
       (or (unicode--combining-character-p character)
           (unicode--variation-selector-p character)
           (unicode--emoji-modifier-p character)
           (zerop (unicode--character-cell-width character)))))

(defun unicode--emoji-zwj-join-p (string cluster-start current-index)
  "True when CURRENT-INDEX completes an extended-pictographic ZWJ run."
  (let ((joiner-index (1- current-index)))
    (when (and (>= joiner-index cluster-start)
               (unicode--zero-width-joiner-p
                (char string joiner-index))
               (unicode--extended-pictographic-p
                (char string current-index)))
      (let ((index (1- joiner-index)))
        (loop while (and (>= index cluster-start)
                         (unicode--grapheme-extend-p
                          (char string index)))
              do (decf index))
        (and (>= index cluster-start)
             (unicode--extended-pictographic-p
              (char string index)))))))

(defun unicode--regional-run-length (string cluster-start end)
  "Count consecutive regional indicators ending immediately before END."
  (loop for index downfrom (1- end) to cluster-start
        while (unicode--regional-indicator-p (char string index))
        count 1))

(defun unicode--hangul-no-break-p (previous current)
  "True when PREVIOUS and CURRENT Hangul classes form one grapheme."
  (let ((left (unicode--hangul-class previous))
        (right (unicode--hangul-class current)))
    (not (null
          (or (and (eq left ':l) (member right '(:l :v :lv :lvt)))
              (and (member left '(:lv :v)) (member right '(:v :t)))
              (and (member left '(:lvt :t)) (eq right ':t)))))))

(defun unicode--grapheme-no-break-p
    (string &key cluster-start previous-index current-index)
  "True when STRING has no grapheme break before CURRENT-INDEX."
  (let ((previous (char string previous-index))
        (current (char string current-index)))
    (cond ((and (char= previous #\return)
                (char= current #\newline))
           t)
          ((or (unicode--grapheme-control-p previous)
               (unicode--grapheme-control-p current))
           nil)
          ((unicode--hangul-no-break-p previous current)
           t)
          ((or (unicode--grapheme-extend-p current)
               (unicode--zero-width-joiner-p current)
               (unicode--spacing-mark-p current))
           t)
          ((unicode--prepend-p previous)
           t)
          ((unicode--emoji-zwj-join-p string cluster-start current-index)
           t)
          ((and (unicode--regional-indicator-p previous)
                (unicode--regional-indicator-p current)
                (oddp (unicode--regional-run-length
                       string cluster-start current-index)))
           t)
          (t
           nil))))


;;; Grapheme boundaries and cell widths

(defun grapheme-next-boundary (string start &optional (end (length string)))
  "Return the first extended-grapheme boundary after START.

END limits the examined slice. Combining marks, emoji modifiers, regional-
indicator pairs, variation selectors, keycaps and emoji ZWJ sequences remain
indivisible. START and END are character indexes and must delimit STRING."
  (check-type string string)
  (check-type start (integer 0 *))
  (check-type end (integer 0 *))
  (unless (<= start end (length string))
    (error "Invalid grapheme slice ~d..~d for a string of length ~d."
           start end (length string)))
  (when (>= start end)
    (return-from grapheme-next-boundary end))
  (unicode--call-with-cell-locale
   (lambda ()
     (loop with index = (1+ start)
           while (< index end)
           while (unicode--grapheme-no-break-p
                  string
                  :cluster-start start
                  :previous-index (1- index)
                  :current-index index)
           do (incf index)
           finally (return index)))))

(defun grapheme-previous-boundary (string index)
  "Return the extended-grapheme boundary immediately before INDEX.

When INDEX is inside a grapheme, return that grapheme's start. Indexes beyond
the string are treated as the string end."
  (check-type string string)
  (check-type index (integer 0 *))
  (setf index (min index (length string)))
  (when (zerop index)
    (return-from grapheme-previous-boundary 0))
  (unicode--call-with-cell-locale
   (lambda ()
     (loop with start = 0
           for next = (grapheme-next-boundary string start)
           when (>= next index)
             return start
           do (setf start next)))))

(defun grapheme-boundary-at-or-after (string index)
  "Return STRING's first extended-grapheme boundary at or after INDEX."
  (check-type string string)
  (check-type index (integer 0 *))
  (setf index (min index (length string)))
  (when (zerop index)
    (return-from grapheme-boundary-at-or-after 0))
  (unicode--call-with-cell-locale
   (lambda ()
     (loop with boundary = 0
           while (< boundary (length string))
           do (setf boundary (grapheme-next-boundary string boundary))
           when (>= boundary index)
             return boundary
           finally (return (length string))))))

(defun grapheme-cell-width (string start end)
  "Return the terminal-cell width of one grapheme from START through END.

Emoji selectors and keycaps promote their cluster to two cells. Emoji
modifiers and joined pictographs do not add cells to the base glyph."
  (check-type string string)
  (check-type start (integer 0 *))
  (check-type end (integer 0 *))
  (unless (<= start end (length string))
    (error "Invalid grapheme slice ~d..~d for a string of length ~d."
           start end (length string)))
  (unicode--call-with-cell-locale
   (lambda ()
     (let ((total 0)
           (widest 0)
           (joiner-p nil)
           (emoji-presentation-p nil)
           (keycap-p nil))
       (loop for index from start below end
             for character = (char string index)
             for code = (char-code character)
             for width = (unicode--character-cell-width character)
             do (setf widest (max widest width))
                (cond ((unicode--zero-width-joiner-p character)
                       (setf joiner-p t))
                      ((and (> index start)
                            (unicode--emoji-modifier-p character))
                       nil)
                      (t
                       (incf total width)))
                (when (unicode--variation-selector-16-p character)
                  (setf emoji-presentation-p t))
                (when (= code #x20e3)
                  (setf keycap-p t)))
       (cond ((or keycap-p
                  (and emoji-presentation-p (> end (1+ start))))
              2)
             (joiner-p
              widest)
             (t
              total))))))

(defun text-cell-width (text &key (start 0) (end (length text)))
  "Return the terminal cells occupied by TEXT between START and END.

Newlines, returns and other controls occupy no horizontal cells."
  (check-type text string)
  (check-type start (integer 0 *))
  (check-type end (integer 0 *))
  (unless (<= start end (length text))
    (error "Invalid text slice ~d..~d for a string of length ~d."
           start end (length text)))
  (unicode--call-with-cell-locale
   (lambda ()
     (loop with index = start
           while (< index end)
           for next = (grapheme-next-boundary text index end)
           sum (grapheme-cell-width text index next)
           do (setf index next)))))

(defun text-cell-prefix
    (text maximum-cells &key (start 0) (end (length text)))
  "Return TEXT's longest grapheme-safe slice fitting MAXIMUM-CELLS.

START is moved forward when it falls inside a grapheme. END is moved backward
when necessary, so the returned string never contains a partial grapheme."
  (check-type text string)
  (check-type maximum-cells (integer 0 *))
  (check-type start (integer 0 *))
  (check-type end (integer 0 *))
  (unless (<= start end (length text))
    (error "Invalid text slice ~d..~d for a string of length ~d."
           start end (length text)))
  (unicode--call-with-cell-locale
   (lambda ()
     (let* ((safe-start (grapheme-boundary-at-or-after text start))
            (after-end (grapheme-boundary-at-or-after text end))
            (safe-end (if (= after-end end)
                          end
                          (grapheme-previous-boundary text end))))
       (if (> safe-start safe-end)
           ""
           (let ((index safe-start)
                 (prefix-end safe-start)
                 (used 0))
             (loop while (< index safe-end)
                   for next = (grapheme-next-boundary text index safe-end)
                   for width = (grapheme-cell-width text index next)
                   while (<= (+ used width) maximum-cells)
                   do (incf used width)
                      (setf index next
                            prefix-end next))
             (subseq text safe-start prefix-end)))))))

(defun text-cell-window
    (text cursor maximum-cells &key (left-marker "‹"))
  "Return a grapheme-safe window around CURSOR and its cell offset.

The returned text occupies at most MAXIMUM-CELLS. When content to the left is
clipped, LEFT-MARKER occupies the first available cells. CURSOR is normalized
to a grapheme boundary and the second value is its zero-based cell position in
the returned window."
  (check-type text string)
  (check-type cursor (integer 0 *))
  (check-type maximum-cells integer)
  (check-type left-marker string)
  (let* ((available (max 1 maximum-cells))
         (safe-cursor
           (grapheme-boundary-at-or-after text (min cursor (length text))))
         (start 0))
    (loop while (< start safe-cursor)
          for clipped-p = (plusp start)
          for marker = (and clipped-p
                            (text-cell-prefix left-marker available))
          for marker-width = (if marker (text-cell-width marker) 0)
          while (>= (+ marker-width
                       (text-cell-width text :start start :end safe-cursor))
                    available)
          do (setf start (grapheme-next-boundary text start)))
    (let* ((clipped-p (plusp start))
           (marker (if clipped-p
                       (text-cell-prefix left-marker available)
                       ""))
           (marker-width (text-cell-width marker))
           (content-width (max 0 (- available marker-width)))
           (content (text-cell-prefix text content-width :start start))
           (cursor-column
             (+ marker-width
                (text-cell-width text :start start :end safe-cursor))))
      (values (concatenate 'string marker content)
              (min (1- available) cursor-column)))))


;;; Safe text and wrapping

(defun unicode--normalize-newlines (text)
  "Return TEXT with CR and CRLF line endings normalized to newlines."
  (with-output-to-string (stream)
    (loop with index = 0
          while (< index (length text))
          for character = (char text index)
          do (cond ((char= character #\return)
                    (write-char #\newline stream)
                    (when (and (< (1+ index) (length text))
                               (char= (char text (1+ index)) #\newline))
                      (incf index)))
                   (t
                    (write-char character stream)))
             (incf index))))

(defun sanitize-text
    (text &key (single-line-p nil) (tab-width 4)
               (replacement-character (code-char #xfffd)))
  "Return TEXT without executable terminal control characters.

CR and CRLF become newlines. Newlines remain unless SINGLE-LINE-P is true,
in which case each becomes a space. Tabs expand to TAB-WIDTH spaces. Other
C0, DEL and C1 controls become REPLACEMENT-CHARACTER; NIL removes them."
  (check-type text string)
  (check-type tab-width (integer 0 *))
  (check-type replacement-character (or null character))
  (with-output-to-string (stream)
    (loop for character across (unicode--normalize-newlines text)
          for code = (char-code character)
          do (cond ((char= character #\newline)
                    (write-char (if single-line-p #\space #\newline)
                                stream))
                   ((char= character #\tab)
                    (loop repeat tab-width
                          do (write-char #\space stream)))
                   ((unicode--control-code-p code)
                    (when replacement-character
                      (write-char replacement-character stream)))
                   (t
                    (write-char character stream))))))

(defun unicode--trimmed-row-end (line start end)
  "Return END moved left past ASCII spaces within LINE's row slice."
  (loop while (and (> end start)
                   (char= (char line (1- end)) #\space))
        do (decf end)
        finally (return end)))

(defun unicode--wrap-line-ranges
    (line maximum-cells &key (start 0) (end (length line)))
  "Wrap newline-free LINE slice into grapheme-safe, word-aware index ranges."
  (let ((width (max 1 maximum-cells))
        (ranges nil)
        (line-start start))
    (loop while (< line-start end)
          do (let ((used 0)
                   (row-end line-start)
                   (break-position nil))
               (loop while (< row-end end)
                     for next = (grapheme-next-boundary line row-end end)
                     for grapheme-width =
                           (grapheme-cell-width line row-end next)
                     while (<= (+ used grapheme-width) width)
                     do (incf used grapheme-width)
                        (when (and (= next (1+ row-end))
                                   (char= (char line row-end) #\space))
                          (setf break-position next))
                        (setf row-end next))
               (cond ((= row-end end)
                      (push (list line-start row-end) ranges)
                      (setf line-start row-end))
                     ((and (< row-end end)
                           (char= (char line row-end) #\space))
                      (push (list line-start
                                  (unicode--trimmed-row-end
                                   line line-start row-end))
                            ranges)
                      (setf line-start row-end))
                     ((and break-position (> break-position line-start))
                      (push (list line-start
                                  (unicode--trimmed-row-end
                                   line line-start break-position))
                            ranges)
                      (setf line-start break-position))
                     ((> row-end line-start)
                      (push (list line-start row-end) ranges)
                      (setf line-start row-end))
                     (t
                      (let ((forced-end
                              (grapheme-next-boundary line line-start end)))
                        (push (list line-start forced-end) ranges)
                        (setf line-start forced-end))))
               (loop while (and (< line-start end)
                                (char= (char line line-start) #\space))
                     do (setf line-start
                              (grapheme-next-boundary line line-start end)))))
    (if ranges
        (nreverse ranges)
        (list (list start start)))))

(defun unicode--wrap-line (line maximum-cells)
  "Wrap newline-free LINE into grapheme-safe, word-aware rows."
  (loop for (start end) in (unicode--wrap-line-ranges line maximum-cells)
        collect (subseq line start end)))

(defun unicode--wrap-text-ranges (text maximum-cells)
  "Wrap TEXT into grapheme-safe, word-aware index ranges without newlines."
  (let ((ranges nil)
        (line-start 0))
    (loop
      for newline = (position #\newline text :start line-start)
      for line-end = (or newline (length text))
      do (setf ranges
               (nconc ranges
                      (unicode--wrap-line-ranges
                       text maximum-cells
                       :start line-start
                       :end line-end)))
      if newline
        do (setf line-start (1+ newline))
      else
        do (return ranges))))

(defun wrap-text (text maximum-cells)
  "Return TEXT wrapped as a list of display rows.

Explicit newlines, including empty and trailing lines, become row boundaries.
Wrapping prefers spaces and never splits a grapheme. Every row fits within
MAXIMUM-CELLS except when one indivisible grapheme is itself wider than the
positive effective width. Nonpositive widths use an effective width of one."
  (check-type text string)
  (check-type maximum-cells integer)
  (let ((normalized (unicode--normalize-newlines text))
        (rows nil)
        (start 0))
    (loop
      for newline = (position #\newline normalized :start start)
      for end = (or newline (length normalized))
      do (dolist (row (unicode--wrap-line
                       (subseq normalized start end)
                       maximum-cells))
           (push row rows))
      if newline
        do (setf start (1+ newline))
      else
        do (return (nreverse rows)))))
