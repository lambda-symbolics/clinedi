(in-package #:clinedi)

;;;; -- Win32 Console Bindings --

;;; The Windows console equivalent of the POSIX termios adapter. Descriptors
;;; are console HANDLE values, which is also what SBCL's Windows fd-streams
;;; report through SB-SYS:FD-STREAM-FD. Only wide kernel32 entry points are
;;; bound, and nothing here depends on SBCL's internal SB-WIN32 package.

(sb-alien:define-alien-routine ("GetStdHandle" terminal--get-std-handle)
    (sb-alien:signed 64)
  (which (sb-alien:unsigned 32)))

(sb-alien:define-alien-routine ("GetConsoleMode" terminal--get-console-mode)
    sb-alien:int
  (handle (sb-alien:signed 64))
  (mode (* (sb-alien:unsigned 32))))

(sb-alien:define-alien-routine ("SetConsoleMode" terminal--set-console-mode)
    sb-alien:int
  (handle (sb-alien:signed 64))
  (mode (sb-alien:unsigned 32)))

(sb-alien:define-alien-routine ("GetConsoleCP" terminal--get-console-cp)
    (sb-alien:unsigned 32))

(sb-alien:define-alien-routine ("SetConsoleCP" terminal--set-console-cp)
    sb-alien:int
  (code-page (sb-alien:unsigned 32)))

(sb-alien:define-alien-routine ("GetConsoleOutputCP" terminal--get-console-output-cp)
    (sb-alien:unsigned 32))

(sb-alien:define-alien-routine ("SetConsoleOutputCP" terminal--set-console-output-cp)
    sb-alien:int
  (code-page (sb-alien:unsigned 32)))

(sb-alien:define-alien-routine ("GetConsoleScreenBufferInfo"
                                terminal--get-console-screen-buffer-info)
    sb-alien:int
  (handle (sb-alien:signed 64))
  (info (* t)))

(sb-alien:define-alien-routine ("GetLastError" terminal--get-last-error)
    (sb-alien:unsigned 32))

(sb-alien:define-alien-routine ("ReadConsoleW" terminal--read-console)
    sb-alien:int
  (handle (sb-alien:signed 64))
  (buffer (* t))
  (count (sb-alien:unsigned 32))
  (read (* (sb-alien:unsigned 32)))
  (control (* t)))

(sb-alien:define-alien-routine ("PeekConsoleInputW" terminal--peek-console-input)
    sb-alien:int
  (handle (sb-alien:signed 64))
  (records (* t))
  (count (sb-alien:unsigned 32))
  (read (* (sb-alien:unsigned 32))))

(sb-alien:define-alien-routine ("FlushConsoleInputBuffer" terminal--flush-console-input)
    sb-alien:int
  (handle (sb-alien:signed 64)))

(defparameter *terminal-standard-input-handle* #xFFFFFFF6
  "STD_INPUT_HANDLE as the unsigned argument GetStdHandle takes.")

(defparameter *terminal-standard-output-handle* #xFFFFFFF5
  "STD_OUTPUT_HANDLE as the unsigned argument GetStdHandle takes.")

(defparameter *terminal-standard-error-handle* #xFFFFFFF4
  "STD_ERROR_HANDLE as the unsigned argument GetStdHandle takes.")

(defparameter *terminal-console-input-cleared-flags*
  (logior #x1 #x2 #x4 #x8 #x10 #x40)
  "ENABLE_PROCESSED_INPUT, ENABLE_LINE_INPUT, ENABLE_ECHO_INPUT,
ENABLE_WINDOW_INPUT, ENABLE_MOUSE_INPUT, and ENABLE_QUICK_EDIT_MODE, all of
which are cleared for application-managed input.")

(defparameter *terminal-console-input-set-flags*
  (logior #x80 #x200)
  "ENABLE_EXTENDED_FLAGS, which makes the quick-edit change effective, and
ENABLE_VIRTUAL_TERMINAL_INPUT, which delivers keys as escape sequences.")

(defparameter *terminal-console-output-set-flags*
  (logior #x1 #x4)
  "ENABLE_PROCESSED_OUTPUT and ENABLE_VIRTUAL_TERMINAL_PROCESSING. Newline
auto-return stays enabled so a bare line feed behaves as it does under POSIX
output post-processing.")

(defparameter *terminal-utf-8-code-page* 65001
  "The console code page under which input and output are UTF-8 octets.")

(defparameter *terminal-key-event* 1
  "KEY_EVENT, the INPUT_RECORD type carrying a keyboard event.")

(defparameter *terminal-input-record-size* 20
  "The size of one INPUT_RECORD in octets.")

(defparameter *terminal-console-peek-records* 32
  "The number of pending INPUT_RECORDs inspected when asking whether a key waits.")

(defparameter *terminal-console-read-units* 512
  "The UTF-16 units one ReadConsoleW call may return.")


;;;; -- Console Queries --

(defun terminal--console-mode (handle)
  "Return HANDLE's console mode, or NIL when HANDLE is not a console."
  (sb-alien:with-alien ((mode (sb-alien:unsigned 32)))
    (if (zerop (terminal--get-console-mode handle (sb-alien:addr mode)))
        nil
        mode)))

(defun terminal--interactive-file-descriptor-p (file-descriptor)
  "Return true when FILE-DESCRIPTOR is a console handle."
  (and (integerp file-descriptor)
       (not (minusp file-descriptor))
       (not (null (terminal--console-mode file-descriptor)))))

(defun terminal--screen-buffer-size (handle)
  "Return positive window rows and columns of screen buffer HANDLE, or NIL values."
  (sb-alien:with-alien ((info (sb-alien:array (sb-alien:unsigned 8) 24)))
    (let ((sap (sb-alien:alien-sap info)))
      (if (zerop (terminal--get-console-screen-buffer-info handle sap))
          (values nil nil)
          (let ((rows (1+ (- (sb-sys:signed-sap-ref-16 sap 16)
                             (sb-sys:signed-sap-ref-16 sap 12))))
                (columns (1+ (- (sb-sys:signed-sap-ref-16 sap 14)
                                (sb-sys:signed-sap-ref-16 sap 10)))))
            (values (and (plusp rows) rows)
                    (and (plusp columns) columns)))))))

(defun terminal-file-descriptor-size (file-descriptor)
  "Return positive terminal rows and columns for FILE-DESCRIPTOR, or NIL values.

Window geometry lives on the screen buffer, so an input handle is answered
through the standard output or error handle of the same console."
  (loop for handle in (list file-descriptor
                            (terminal--get-std-handle *terminal-standard-output-handle*)
                            (terminal--get-std-handle *terminal-standard-error-handle*))
        do (multiple-value-bind (rows columns)
               (terminal--screen-buffer-size handle)
             (when (and rows columns)
               (return (values rows columns))))
        finally (return (values nil nil))))

(defun terminal-standard-input-file-descriptor ()
  "Return the descriptor of this process's standard input for terminal use."
  (terminal--get-std-handle *terminal-standard-input-handle*))


;;;; -- Terminal Methods --

(defclass win32-terminal (stream-terminal) ()
  (:documentation "An SBCL Windows console terminal with native input-mode management."))

(defclass host-terminal (win32-terminal) ()
  (:documentation "The terminal class with native input-mode management on this host."))

(defmethod terminal-capture-input-mode ((terminal win32-terminal))
  "Capture console modes and code pages only for an actual console descriptor."
  (let ((descriptor (stream-terminal-input-file-descriptor terminal)))
    (when (terminal--interactive-file-descriptor-p descriptor)
      (list :input-mode (terminal--console-mode descriptor)
            :output-mode (terminal--console-mode
                          (terminal--get-std-handle *terminal-standard-output-handle*))
            :input-code-page (terminal--get-console-cp)
            :output-code-page (terminal--get-console-output-cp)))))

(defmethod terminal-activate-input-mode ((terminal win32-terminal))
  "Enter virtual-terminal input without line buffering, echo, or quick edit."
  (let* ((descriptor (stream-terminal-input-file-descriptor terminal))
         (output (terminal--get-std-handle *terminal-standard-output-handle*))
         (input-mode (terminal--console-mode descriptor))
         (output-mode (terminal--console-mode output)))
    (unless input-mode
      (error 'terminal-error :message "Could not inspect console input mode."
             :operation ':start :cause nil))
    (when (zerop (terminal--set-console-mode
                  descriptor
                  (logior (logandc2 input-mode *terminal-console-input-cleared-flags*)
                          *terminal-console-input-set-flags*)))
      (error 'terminal-error
             :message "This console does not support virtual terminal input; use Windows Terminal or a console host with virtual terminal processing."
             :operation ':start :cause nil))
    (when output-mode
      (terminal--set-console-mode
       output (logior output-mode *terminal-console-output-set-flags*)))
    (terminal--set-console-cp *terminal-utf-8-code-page*)
    (terminal--set-console-output-cp *terminal-utf-8-code-page*)
    nil))

(defmethod terminal-restore-input-mode ((terminal win32-terminal) mode)
  "Restore the exact console modes and code pages captured in MODE."
  (let ((descriptor (stream-terminal-input-file-descriptor terminal))
        (output (terminal--get-std-handle *terminal-standard-output-handle*)))
    (terminal--set-console-mode descriptor (getf mode :input-mode))
    (when (getf mode :output-mode)
      (terminal--set-console-mode output (getf mode :output-mode)))
    (terminal--set-console-cp (getf mode :input-code-page))
    (terminal--set-console-output-cp (getf mode :output-code-page))
    nil))


;;;; -- Console Input Stream --

;;; SBCL's Windows runtime reads console handles through its own reader
;;; thread, which was written for cooked line input and drops every carriage
;;; return it receives. In raw virtual-terminal input mode the Enter key is
;;; a lone carriage return, so it vanished before any decoder saw it. This
;;; stream reads the console with ReadConsoleW directly and reports pending
;;; input through PeekConsoleInputW, counting only key-down records that
;;; carry a character, since a key-up or a bare modifier would make
;;; ReadConsoleW block.

(defclass console-input-stream (sb-gray:fundamental-character-input-stream)
  ((handle
    :initarg :handle
    :reader console-input-stream-handle
    :documentation "The console input HANDLE the stream reads.")
   (buffer
    :initform (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)
    :reader console-input-stream-buffer
    :documentation "Characters read from the console and not yet consumed.")
   (position
    :initform 0
    :accessor console-input-stream-position
    :documentation "The index of the next unconsumed character in BUFFER.")
   (unread
    :initform nil
    :accessor console-input-stream-unread
    :documentation "A character handed back with UNREAD-CHAR, returned first.")
   (high-surrogate
    :initform nil
    :accessor console-input-stream-high-surrogate
    :documentation "A UTF-16 high surrogate awaiting its low half across reads.")
   (end-p
    :initform nil
    :accessor console-input-stream-end-p
    :documentation "Whether the console reported the end of its input."))
  (:documentation "A character stream reading a console handle with ReadConsoleW."))

(defun console-input-stream--buffered-p (stream)
  "Return true when STREAM holds a consumed-later character."
  (or (console-input-stream-unread stream)
      (< (console-input-stream-position stream)
         (fill-pointer (console-input-stream-buffer stream)))))

(defun console-input-stream--fill (stream)
  "Read waiting console characters into STREAM's buffer; return false at its end."
  (sb-alien:with-alien ((units (sb-alien:array (sb-alien:unsigned 16) 512))
                        (count (sb-alien:unsigned 32)))
    (setf count 0)
    (let ((status (terminal--read-console (console-input-stream-handle stream)
                                          (sb-alien:alien-sap units)
                                          *terminal-console-read-units*
                                          (sb-alien:addr count)
                                          nil)))
      (when (or (zerop status) (zerop count))
        (setf (console-input-stream-end-p stream) t)
        (return-from console-input-stream--fill nil))
      (let ((buffer (console-input-stream-buffer stream))
            (pending (console-input-stream-high-surrogate stream)))
        (setf (fill-pointer buffer) 0
              (console-input-stream-position stream) 0)
        (dotimes (index count)
          (let ((unit (sb-alien:deref units index)))
            (cond ((<= #xD800 unit #xDBFF)
                   (setf pending unit))
                  ((and pending (<= #xDC00 unit #xDFFF))
                   (vector-push-extend
                    (code-char (+ #x10000 (ash (- pending #xD800) 10) (- unit #xDC00)))
                    buffer)
                   (setf pending nil))
                  (t
                   (setf pending nil)
                   (vector-push-extend (code-char unit) buffer)))))
        (setf (console-input-stream-high-surrogate stream) pending)
        (plusp (fill-pointer buffer))))))

(defun console-input-stream--key-pending-p (stream)
  "Return true when a key-down record carrying a character waits on STREAM's console."
  (sb-alien:with-alien ((records (sb-alien:array (sb-alien:unsigned 8) 640))
                        (count (sb-alien:unsigned 32)))
    (setf count 0)
    (let ((sap (sb-alien:alien-sap records)))
      (and (not (zerop (terminal--peek-console-input (console-input-stream-handle stream)
                                                     sap
                                                     *terminal-console-peek-records*
                                                     (sb-alien:addr count))))
           (loop for index below count
                 for base = (* index *terminal-input-record-size*)
                 thereis (and (= (sb-sys:sap-ref-16 sap base) *terminal-key-event*)
                              (not (zerop (sb-sys:sap-ref-32 sap (+ base 4))))
                              (not (zerop (sb-sys:sap-ref-16 sap (+ base 14))))))))))

(defun console-input-stream--next (stream wait-p)
  "Return STREAM's next character, :EOF at its end, or NIL without WAIT-P when none waits."
  (let ((unread (console-input-stream-unread stream)))
    (when unread
      (setf (console-input-stream-unread stream) nil)
      (return-from console-input-stream--next unread)))
  (cond ((console-input-stream-end-p stream)
         :eof)
        ((and (not (console-input-stream--buffered-p stream))
              (not wait-p)
              (not (console-input-stream--key-pending-p stream)))
         nil)
        ((or (console-input-stream--buffered-p stream)
             (console-input-stream--fill stream))
         (prog1 (char (console-input-stream-buffer stream)
                      (console-input-stream-position stream))
           (incf (console-input-stream-position stream))))
        (t
         :eof)))

(defmethod sb-gray:stream-read-char ((stream console-input-stream))
  (console-input-stream--next stream t))

(defmethod sb-gray:stream-read-char-no-hang ((stream console-input-stream))
  (console-input-stream--next stream nil))

(defmethod sb-gray:stream-unread-char ((stream console-input-stream) character)
  (setf (console-input-stream-unread stream) character)
  nil)

(defmethod sb-gray:stream-listen ((stream console-input-stream))
  (and (not (console-input-stream-end-p stream))
       (or (console-input-stream--buffered-p stream)
           (console-input-stream--key-pending-p stream))))

(defmethod sb-gray:stream-clear-input ((stream console-input-stream))
  (setf (fill-pointer (console-input-stream-buffer stream)) 0
        (console-input-stream-position stream) 0
        (console-input-stream-unread stream) nil)
  (terminal--flush-console-input (console-input-stream-handle stream))
  nil)

(defmethod initialize-instance :after ((terminal win32-terminal) &key)
  "Read an actual console directly, bypassing SBCL's carriage-return-dropping reader."
  (let ((descriptor (stream-terminal-input-file-descriptor terminal)))
    (when (terminal--interactive-file-descriptor-p descriptor)
      (setf (slot-value terminal 'input-stream)
            (make-instance 'console-input-stream :handle descriptor)))))
