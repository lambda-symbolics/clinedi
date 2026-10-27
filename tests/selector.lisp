;;;; -- Candidate Selector Tests --

(in-package #:clinedi/tests)

(defun run-selector-tests ()
  "Run candidate selection, navigation and viewport regression tests."
  (let ((selector (make-selector :items         '(alpha beta gamma delta)
                                 :visible-count 2)))
    (check-equal "selector owns its candidate container"
                 '(alpha beta gamma delta)
                 (selector-items selector))
    (check-equal "selector begins on the first candidate"
                 'alpha
                 (selector-selected-item selector))
    (multiple-value-bind (items start selection)
        (selector-window selector)
      (check-equal "initial selector window" '(alpha beta) items)
      (check-equal "initial selector window starts at zero" 0 start)
      (check-equal "initial selector selection is local" 0 selection))
    (selector-move selector 2)
    (multiple-value-bind (items start selection)
        (selector-window selector)
      (check-equal "selector window follows forward movement"
                   '(beta gamma)
                   items)
      (check-equal "forward selector window start" 1 start)
      (check-equal "forward local selector index" 1 selection))
    (selector-move selector 2)
    (check-equal "selector movement wraps forward"
                 'alpha
                 (selector-selected-item selector))
    (selector-move selector -1)
    (check-equal "selector movement wraps backward"
                 'delta
                 (selector-selected-item selector))
    (selector-set-items selector '(alpha beta gamma delta))
    (check-equal "equal candidates preserve selection"
                 'delta
                 (selector-selected-item selector))
    (selector-set-items selector '(newer newest))
    (check-equal "changed candidates reset selection"
                 'newer
                 (selector-selected-item selector))
    (multiple-value-bind (action value)
        (selector-handle-event selector :history-next)
      (check-equal "down event changes selector" :changed action)
      (check-equal "movement returns selected value" 'newest value))
    (multiple-value-bind (action value)
        (selector-handle-event selector :complete)
      (check-equal "tab cycles selection" :changed action)
      (check-equal "tab returns cycled opaque value" 'newer value))
    (multiple-value-bind (action value)
        (selector-handle-event selector :complete-previous)
      (check-equal "shift-tab cycles selection backward" :changed action)
      (check-equal "shift-tab returns cycled opaque value" 'newest value))
    (selector-handle-event selector :complete)
    (multiple-value-bind (action value)
        (selector-handle-event selector :submit)
      (check-equal "submission accepts selection" :accept action)
      (check-equal "submission returns opaque value" 'newer value))
    (multiple-value-bind (action value)
        (selector-handle-event selector :escape)
      (check-equal "escape cancels selector" :cancel action)
      (check-equal "cancel has no payload" nil value))
    (multiple-value-bind (action value)
        (selector-handle-event selector '(:insert "x"))
      (check-equal "text input dismisses choosing" :dismiss action)
      (check-equal "dismissal retains selected value" 'newer value)))
  (let ((selector (make-selector :items #() :visible-count 3)))
    (multiple-value-bind (items start selection)
        (selector-window selector)
      (check-equal "empty selector window" nil items)
      (check-equal "empty selector window start" 0 start)
      (check-equal "empty selector has no local selection" nil selection))
    (multiple-value-bind (action value)
        (selector-handle-event selector :submit)
      (check-equal "empty selector cannot accept" :unhandled action)
      (check-equal "empty selector has no accepted value" nil value)))
  (let ((selector (make-selector :items '(nil))))
    (multiple-value-bind (action value)
        (selector-handle-event selector :submit)
      (check-equal "nil is a valid opaque candidate" :accept action)
      (check-equal "nil candidate remains nil" nil value)))
  (let ((selector (make-selector :items         '("a" "bbbb" "cc" "d")
                                 :visible-count 4
                                 :arrangement   :grid)))
    (multiple-value-bind (rows widths)
        (selector-arrange selector 8 :width-function #'length)
      (check-equal "grid fills the widest measured column count"
                   '((0 1) (2 3))
                   rows)
      (check-equal "grid reports measured column widths" '(2 4) widths)
      (check-equal "grid records navigation geometry"
                   2
                   (selector-column-count selector)))
    (multiple-value-bind (action value)
        (selector-handle-event selector :history-next)
      (check-equal "grid down arrow changes selection" :changed action)
      (check-equal "grid down arrow preserves column" "cc" value))
    (multiple-value-bind (action value)
        (selector-handle-event selector :right)
      (check-equal "grid right arrow changes selection" :changed action)
      (check-equal "grid right arrow moves one cell" "d" value))
    (multiple-value-bind (rows widths)
        (selector-arrange selector 7 :width-function #'length)
      (check-equal "narrow grid falls back to vertical"
                   '((0) (1) (2) (3))
                   rows)
      (check-equal "vertical fallback measures one column" '(4) widths)))
  (let ((selector (make-selector :items         '(one two three)
                                 :visible-count 3
                                 :arrangement   :vertical)))
    (multiple-value-bind (rows widths)
        (selector-arrange selector 80 :width-function (lambda (item)
                                                        (length
                                                         (symbol-name item))))
      (check-equal "vertical arrangement remains one item per row"
                   '((0) (1) (2))
                   rows)
      (check-equal "vertical arrangement reports one measured width"
                   '(5)
                   widths)))
  (values))
