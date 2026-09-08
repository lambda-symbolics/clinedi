(in-package #:clinedi)

;;;; -- Terminal Methods --

(defparameter *terminal-window-size-request*
  #+linux #x5413
  #+bsd #x40087468
  #-(or linux bsd) #x5413
  "The platform TIOCGWINSZ ioctl request for reading terminal dimensions.")
(defun terminal-file-descriptor-size (file-descriptor)
  "Return positive terminal rows and columns for FILE-DESCRIPTOR, or NIL values."
  (handler-case
      (sb-alien:with-alien ((size (array sb-alien:unsigned-short 4)))
        (sb-posix:ioctl file-descriptor
                        *terminal-window-size-request*
                        (sb-alien:addr (sb-alien:deref size 0)))
        (let ((rows (sb-alien:deref size 0))
              (columns (sb-alien:deref size 1)))
          (values (and (plusp rows) rows)
                  (and (plusp columns) columns))))
    (sb-posix:syscall-error ()
      (values nil nil))))
(defun terminal--interactive-file-descriptor-p (file-descriptor)
  "Return true when FILE-DESCRIPTOR names an interactive terminal."
  (and (not (minusp file-descriptor))
       (let ((result (sb-unix:unix-isatty file-descriptor)))
         (and result (plusp result)))))
(defun terminal--configure-input-mode (mode)
  "Configure MODE for noncanonical, no-echo, application-managed input."
  (setf (sb-posix:termios-lflag mode)
        (logandc2 (sb-posix:termios-lflag mode)
                  (logior sb-posix:icanon
                          sb-posix:echo
                          sb-posix:isig
                          sb-posix:iexten))
        (sb-posix:termios-iflag mode)
        (logandc2 (sb-posix:termios-iflag mode) sb-posix:ixon))
  (let ((control-characters (sb-posix:termios-cc mode)))
    (setf (aref control-characters sb-posix:vmin) 1
          (aref control-characters sb-posix:vtime) 0))
  mode)


(defclass posix-terminal (stream-terminal) ()
  (:documentation "An SBCL POSIX stream terminal with native input-mode management."))

(defmethod terminal-capture-input-mode ((terminal posix-terminal))
  "Capture termios only for an actual interactive descriptor."
  (let ((descriptor (stream-terminal-input-file-descriptor terminal)))
    (when (terminal--interactive-file-descriptor-p descriptor)
      (handler-case (sb-posix:tcgetattr descriptor)
        (sb-posix:syscall-error (condition)
          (error 'terminal-error :message "Could not inspect terminal input mode."
                 :operation ':start :cause condition))))))

(defmethod terminal-activate-input-mode ((terminal posix-terminal))
  "Disable line buffering, echo, driver signals and software flow control."
  (let ((descriptor (stream-terminal-input-file-descriptor terminal)))
    (sb-posix:tcsetattr descriptor sb-posix:tcsanow
                      (terminal--configure-input-mode (sb-posix:tcgetattr descriptor)))))

(defmethod terminal-restore-input-mode ((terminal posix-terminal) mode)
  "Restore the exact termios MODE captured from TERMINAL."
  (sb-posix:tcsetattr (stream-terminal-input-file-descriptor terminal)
                    sb-posix:tcsanow mode))
