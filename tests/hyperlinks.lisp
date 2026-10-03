;;;; -- Web URL and hyperlink tests --

(in-package #:clinedi/tests)

(defun run-hyperlink-tests ()
  "Run URL detection and OSC 8 hyperlink regression checks."
  (dolist (case '(("see https://example.com/path."
                   ("https://example.com/path"))
                  ("(https://example.com/a_(b))"
                   ("https://example.com/a_(b)"))
                  ("<https://x.org>, then http://d.e/f?x=1&y=2#frag!"
                   ("https://x.org" "http://d.e/f?x=1&y=2#frag"))
                  ("HTTPS://Up.Case/Path"
                   ("HTTPS://Up.Case/Path"))
                  ("a bare http:// scheme"
                   ())
                  ("[link](https://example.com/wiki_(disambiguation))"
                   ("https://example.com/wiki_(disambiguation)"))
                  ("quoted \"https://q.example/\" text"
                   ("https://q.example/"))
                  ("no links here"
                   ())))
    (destructuring-bind (text expected) case
      (check-equal (format nil "URL detection in ~s" text)
                   expected
                   (loop for (start . end) in (url-ranges text)
                         collect (subseq text start end)))))
  (let ((text "visit https://example.com/doc now"))
    (check-equal "the URL under an offset is returned"
                 "https://example.com/doc"
                 (url-at text 10))
    (check-equal "offsets before a URL return nothing" nil (url-at text 2))
    (check-equal "trailing text is not part of the URL"
                 nil
                 (url-at text (1- (length text)))))
  (check-equal "a complete https URL is a web URL"
               t (web-url-p "https://example.com/a"))
  (check-equal "surrounding text disqualifies a web URL"
               nil (web-url-p "see https://example.com/a"))
  (check-equal "other schemes are not web URLs"
               nil (web-url-p "ftp://example.com/a"))
  (check-equal "non-strings are not web URLs" nil (web-url-p 42))
  (let* ((escape (code-char 27))
         (text "go to https://example.com/x now")
         (linked (ansi-link-urls text)))
    (check-equal "a hyperlink wraps its text in OSC 8 controls"
                 (format nil "~c]8;;https://x~c\\here~c]8;;~c\\"
                         escape escape escape escape)
                 (ansi-hyperlink "here" "https://x"))
    (check-equal "linked URLs keep the visible text"
                 text (ansi-strip linked))
    (check-equal "each URL is linked to itself"
                 (format nil "go to ~a now"
                         (ansi-hyperlink "https://example.com/x"
                                         "https://example.com/x"))
                 linked)
    (check-true "a linked URL reopens on every wrapped row"
                (>= (count-if (lambda (row)
                                (search (format nil "~c]8;;https://example.com/x"
                                                escape)
                                        (second row)))
                              (wrap-styled-text text linked 14))
                    2))
    (check-equal "non-ASCII URLs stay unlinked"
                 "see https://例え.jp/p"
                 (ansi-link-urls "see https://例え.jp/p"))
    (check-equal "an empty target is not a hyperlink"
                 "here" (ansi-hyperlink "here" ""))
    (let ((*presentation-enabled* nil))
      (check-equal "disabled presentation emits no hyperlinks"
                   text (ansi-link-urls text)))))
