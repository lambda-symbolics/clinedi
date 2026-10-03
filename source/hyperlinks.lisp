;;;; -- Web URLs and OSC 8 hyperlinks --

(in-package #:clinedi)

(defparameter *url-schemes* '("https://" "http://")
  "The URL prefixes recognized in text, longest first.")

(defparameter *url-trailing-punctuation* ".,;:!?'\""
  "Characters that end a sentence rather than a URL when they trail one.")

(defun url--scheme-at (text start)
  "Return the end of the URL scheme beginning at START in TEXT, or NIL."
  (loop for scheme in *url-schemes*
        for end = (+ start (length scheme))
        when (and (<= end (length text))
                  (string-equal scheme text :start2 start :end2 end))
          return end))

(defun url--boundary-p (character)
  "Return true when CHARACTER can never belong to a URL."
  (let ((code (char-code character)))
    (and (or (member character '(#\Space #\Tab #\Newline #\Return #\Page
                                 #\< #\> #\" #\` #\| #\{ #\})
                     :test #'char=)
             (< code 32)
             (member code '(#x7f #xa0 #x3000)))
         t)))

(defun url--trim-end (text start end)
  "Return END moved back over trailing punctuation and unbalanced closers."
  (loop
    (when (<= end (1+ start))
      (return end))
    (let ((last (char text (1- end))))
      (cond
        ((find last *url-trailing-punctuation*)
         (decf end))
        ((and (char= last #\))
              (> (count #\) text :start start :end end)
                 (count #\( text :start start :end end)))
         (decf end))
        ((and (char= last #\])
              (> (count #\] text :start start :end end)
                 (count #\[ text :start start :end end)))
         (decf end))
        (t
         (return end))))))

(defun url-ranges (text)
  "Return the (START . END) character ranges of the web URLs in TEXT.

A URL begins with a scheme from *URL-SCHEMES* and runs to whitespace or a
bracketing character, then sheds trailing sentence punctuation and closing
brackets that have no opening partner inside it. A scheme alone is not a URL."
  (check-type text string)
  (let ((ranges nil)
        (index 0)
        (length (length text)))
    (loop
      (when (>= index length)
        (return (nreverse ranges)))
      (let ((scheme-end (url--scheme-at text index)))
        (if scheme-end
            (let ((end (url--trim-end
                        text index
                        (or (position-if #'url--boundary-p text :start scheme-end)
                            length))))
              (if (> end scheme-end)
                  (progn
                    (push (cons index end) ranges)
                    (setf index end))
                  (setf index scheme-end)))
            (incf index))))))

(defun url-at (text offset)
  "Return the URL in TEXT covering character OFFSET, or NIL."
  (loop for (start . end) in (url-ranges text)
        when (and (<= start offset) (< offset end))
          return (subseq text start end)))

(defun web-url-p (value)
  "Return true when VALUE is one complete web URL string."
  (and (stringp value)
       (let ((ranges (url-ranges value)))
         (and (= (length ranges) 1)
              (zerop (car (first ranges)))
              (= (cdr (first ranges)) (length value))))))

(defun hyperlink--target-p (url)
  "Return true when URL fits an OSC 8 payload, meaning printable ASCII only."
  (and (plusp (length url))
       (every (lambda (character)
                (<= 33 (char-code character) 126))
              url)))

(defun ansi-hyperlink (text url)
  "Return TEXT wrapped in an OSC 8 hyperlink to URL.

The visible characters are unchanged, so the result strips back to TEXT.
TEXT is returned unchanged when presentation is disabled or URL is not
printable ASCII; percent-encode other URLs first."
  (check-type text string)
  (check-type url string)
  (if (and *presentation-enabled* (hyperlink--target-p url))
      (format nil "~c]8;;~a~c\\~a~c]8;;~c\\"
              +escape-character+ url +escape-character+
              text
              +escape-character+ +escape-character+)
      text))

(defun ansi-link-urls (text)
  "Return TEXT with every web URL in it wrapped in an OSC 8 hyperlink.

URLs are found by URL-RANGES and linked by ANSI-HYPERLINK, so the visible text
is unchanged and URLs that are not printable ASCII stay plain."
  (check-type text string)
  (let ((ranges (and *presentation-enabled* (url-ranges text))))
    (if (null ranges)
        text
        (with-output-to-string (output)
          (let ((position 0))
            (loop for (start . end) in ranges
                  for url = (subseq text start end)
                  do (write-string text output :start position :end start)
                     (write-string (ansi-hyperlink url url) output)
                     (setf position end))
            (write-string text output :start position))))))
