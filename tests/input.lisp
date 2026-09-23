;;;; -- Input-decoder tests --

(in-package #:clinedi/tests)

(defun input-test--event (text)
  "Decode one event from TEXT."
  (with-input-from-string (stream text)
    (read-event :stream stream :escape-delay 0)))

(defun input-test--escape-sequence (body)
  "Return an escape-prefixed terminal sequence containing BODY."
  (concatenate 'string (string (code-char 27)) body))

(defun input-test--control-output (function)
  "Return the terminal control written by FUNCTION."
  (let ((stream (make-string-output-stream)))
    (funcall function :stream stream)
    (get-output-stream-string stream)))

(defun run-input-tests ()
  "Run semantic input-decoder regression tests."
  (dolist (case '(("[5~" :page-up)
                  ("[6~" :page-down)
                  ("[1;5H" :scroll-top)
                  ("[7;5~" :scroll-top)
                  ("[1;5F" :scroll-bottom)
                  ("[8;5~" :scroll-bottom)
                  ("[<64;10;20M" (:scroll -1))
                  ("[<65;10;20M" (:scroll 1))
                  ("[<80;10;20M" (:scroll -1))
                  ("[<0;10;20M" :ignore)
                  ("[<64;10;20m" :ignore)
                  ("[<96;10;20M" :ignore)
                  ("[<-64;10;20M" :ignore)
                  ("[<64;0;20M" :ignore)
                  ("[<64;10;20;1M" :ignore)
                  ("[<64;;20M" :ignore)))
    (check-equal (format nil "viewport input ~A" (first case))
                 (second case)
                 (input-test--event (input-test--escape-sequence (first case)))))
  (check-equal "bracketed paste enable control"
               (input-test--escape-sequence "[?2004h")
               (input-test--control-output #'enable-bracketed-paste))
  (check-equal "bracketed paste disable control"
               (input-test--escape-sequence "[?2004l")
               (input-test--control-output #'disable-bracketed-paste))
  (check-equal "printable input event"
               '(:insert "猫")
               (input-test--event "猫"))
  (check-equal "physical stream end is distinct from control-D"
               :stream-end
               (input-test--event ""))
  (check-equal "control-D editing event"
               :end-of-input
               (input-test--event (string (code-char 4))))
  (check-equal "control-B event"
               :left
               (input-test--event (string (code-char 2))))
  (check-equal "raw control-backspace event"
               :kill-word
               (input-test--event (string (code-char 8))))
  (check-equal "arrow-up event"
               :up
               (input-test--event (input-test--escape-sequence "[A")))
  (check-equal "arrow-down event"
               :down
               (input-test--event (input-test--escape-sequence "[B")))
  (check-equal "control-P history event"
               :history-previous
               (input-test--event (string (code-char 16))))
  (check-equal "control-N history event"
               :history-next
               (input-test--event (string (code-char 14))))
  (check-equal "shift-tab event"
               :complete-previous
               (input-test--event (input-test--escape-sequence "[Z")))
  (check-equal "delete event"
               :delete
               (input-test--event (input-test--escape-sequence "[3~")))
    (dolist (case '(("xterm control-left event" "[1;5D" :word-left)
                    ("short control-left event" "[5D" :word-left)
                    ("xterm control-right event" "[1;5C" :word-right)
                    ("short control-right event" "[5C" :word-right)
                    ("xterm alt-left event" "[1;3D" :word-left)
                    ("short alt-left event" "[3D" :word-left)
                    ("xterm alt-right event" "[1;3C" :word-right)
                    ("short alt-right event" "[3C" :word-right)))
      (check-equal (first case)
                   (third case)
                   (input-test--event
                    (input-test--escape-sequence (second case)))))
    (dolist (case '(("meta-b event" "b" :word-left)
                    ("meta-f event" "f" :word-right)
                    ("meta-d event" "d" :kill-word-forward)))
      (check-equal (first case)
                   (third case)
                   (input-test--event
                    (input-test--escape-sequence (second case)))))
    (check-equal "meta-backspace event"
                 :kill-word
                 (input-test--event
                  (input-test--escape-sequence (string (code-char 8)))))
    (check-equal "meta-delete event"
                 :kill-word
                 (input-test--event
                  (input-test--escape-sequence (string (code-char 127)))))
    (check-equal "unknown meta-letter event"
                 :escape
                 (input-test--event
                  (input-test--escape-sequence "x")))
  (check-equal "legacy alt-enter event"
               :insert-newline
               (input-test--event
                (concatenate 'string
                             (string (code-char 27))
                             (string #\return))))
  (dolist (case '(("CSI-u line-feed event" "[10u" :submit)
                  ("CSI-u carriage-return event" "[13u" :submit)
                  ("CSI-u tab event" "[9u" :complete)
                  ("CSI-u shift-tab event" "[9;2u" :complete-previous)
                  ("CSI-u escape event" "[27u" :escape)
                  ("CSI-u backspace event" "[127u" :backspace)
                  ("CSI-u control-D event" "[100;5u" :end-of-input)
                  ("modify-other-keys control-D event"
                   "[27;5;100~" :end-of-input)))
    (check-equal (first case)
                 (third case)
                 (input-test--event
                  (input-test--escape-sequence (second case)))))
  (dolist (code '(10 13))
    (loop for modifier from 2 to 8
          do (check-equal
              (format nil "CSI-u modified Enter code ~D modifier ~D"
                      code modifier)
              :insert-newline
              (input-test--event
               (input-test--escape-sequence
                (format nil "[~D;~Du" code modifier))))))
  (loop for modifier from 2 to 8
        do (check-equal
            (format nil "modify-other-keys Enter modifier ~D" modifier)
            :insert-newline
            (input-test--event
             (input-test--escape-sequence
              (format nil "[27;~D;13~C" modifier #\~)))))
    (dolist (case '(("CSI-u control-backspace with BS" "[8;5u")
                    ("CSI-u control-backspace with DEL" "[127;5u")
                    ("modify-other-keys control-backspace with BS" "[27;5;8~")
                    ("modify-other-keys control-backspace with DEL" "[27;5;127~")
                    ("CSI-u alt-backspace with BS" "[8;3u")
                    ("CSI-u alt-backspace with DEL" "[127;3u")
                    ("modify-other-keys alt-backspace with BS" "[27;3;8~")
                    ("modify-other-keys alt-backspace with DEL" "[27;3;127~")))
      (check-equal (first case)
                   :kill-word
                   (input-test--event
                    (input-test--escape-sequence (second case)))))
    (dolist (case '(("CSI-u alt-b event" "[98;3u" :word-left)
                    ("modify-other-keys alt-b event" "[27;3;98~" :word-left)
                    ("CSI-u alt-f event" "[102;3u" :word-right)
                    ("modify-other-keys alt-f event" "[27;3;102~" :word-right)
                    ("CSI-u alt-d event" "[100;3u" :kill-word-forward)
                    ("modify-other-keys alt-d event" "[27;3;100~" :kill-word-forward)))
      (check-equal (first case)
                   (third case)
                   (input-test--event
                    (input-test--escape-sequence (second case)))))
  (dolist (case '(("CSI-u control-C event" "[99;5u")
                  ("modify-other-keys control-C event" "[27;5;99~")))
    (check-equal (first case)
                 :interrupt
                 (input-test--event
                  (input-test--escape-sequence (second case)))))
  (check-equal "lone escape event"
               :escape
               (input-test--event (string (code-char 27))))
  (check-equal "unknown CSI event"
               :ignore
               (input-test--event (input-test--escape-sequence "[9~")))
  (check-equal "bracketed paste is one event"
               (list :paste (format nil "one~%猫"))
               (input-test--event
                (concatenate 'string
                             (input-test--escape-sequence "[200~")
                             (format nil "one~%猫")
                             (input-test--escape-sequence "[201~"))))
  (values))
