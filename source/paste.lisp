(in-package #:clinedi)

;;;; -- Pasted paths --

;;; Terminals paste a dragged file as its path, quoted or escaped as a shell
;;; would read it, or as a file:// URL. These functions recover the path text.

(defun pasted-path-token (text)
  "Return the one shell word pasted TEXT spells, or NIL.

Single and double quotes group characters and a backslash escapes the next
one, as a POSIX shell reads them. Text that is not exactly one word, or leaves a
quote or escape open, gives NIL."
  (let ((result (make-string-output-stream))
        (quote nil)
        (escaped-p nil))
    (loop for character across (string-trim '(#\Space #\Tab #\Newline #\Return) text)
          do (cond
               (escaped-p
                (write-char character result)
                (setf escaped-p nil))
               ((and (null quote) (char= character #\\))
                (setf escaped-p t))
               ((and (null quote) (member character '(#\' #\")))
                (setf quote character))
               ((and quote (char= character quote))
                (setf quote nil))
               ((and (null quote) (member character '(#\Space #\Tab #\Newline)))
                (return-from pasted-path-token nil))
               (t
                (write-char character result))))
    (and (not quote) (not escaped-p) (get-output-stream-string result))))

(defun paste--percent-decode (text)
  "Return TEXT with its %XX escapes decoded as UTF-8, or NIL when one is malformed."
  (let ((octets (make-array (length text) :element-type '(unsigned-byte 8) :fill-pointer 0))
        (index 0))
    (loop while (< index (length text))
          do (let ((character (char text index)))
               (cond
                 ((char= character #\%)
                  (let ((value (and (<= (+ index 3) (length text))
                                    (ignore-errors
                                     (parse-integer text :start (1+ index) :end (+ index 3)
                                                         :radix 16)))))
                    (unless value
                      (return-from paste--percent-decode nil))
                    (vector-push value octets)
                    (incf index 3)))
                 ((< (char-code character) 128)
                  (vector-push (char-code character) octets)
                  (incf index))
                 (t
                  (return-from paste--percent-decode nil)))))
    (ignore-errors
     #+sbcl (sb-ext:octets-to-string (coerce octets '(vector (unsigned-byte 8)))
                                     :external-format :utf-8)
     #+ccl (ccl:decode-string-from-octets (coerce octets '(vector (unsigned-byte 8)))
                                          :external-format :utf-8)
     #-(or sbcl ccl) (error "Percent-decoding needs SBCL or CCL."))))

(defun pasted-path (text)
  "Return the local path pasted TEXT names, or NIL.

TEXT is one shell word as PASTED-PATH-TOKEN reads it; a file:// URL is
percent-decoded to its path. Whether the path exists is left to the caller."
  (let ((token (pasted-path-token text)))
    (cond
      ((or (null token) (zerop (length token)))
       nil)
      ((and (>= (length token) 7) (string-equal "file://" token :end2 7))
       (let ((path (paste--percent-decode (subseq token 7))))
         (and path (plusp (length path)) path)))
      (t
       token))))
