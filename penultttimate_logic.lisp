;;------------------------------------------------------------------------------
;;-------------------------- reference 80 column line --------------------------

(setq *print-circle* t)

(defpackage "PENULTTTIMATE"
  (:use "COMMON-LISP")
  (:shadow "DEREF" "LOOP"))
(in-package "PENULTTTIMATE")

;; this will be partially written in a pipettish style because
;; i forgot how to write decent CL (and don't want to)

;; call a variable as if it were a function
;; (needed because i think in terms of a lisp-1)
(set-dispatch-macro-character
 #\# #\*
 (lambda (stream _1 _2)
   (declare (ignore _1 _2))
   `(funcall ,@(let ((x (read stream t nil t)))
                 (if (or (atom x) (eq 'funcall (car x))) (list x) x)))))

(set-macro-character
 #\λ
 (lambda (stream _)
   (declare (ignore _))
   (let ((args nil) (c nil))
     (block nil
       (tagbody
        getchar
          (setq c (read-char stream t nil t))
          (if (eql c #\()
              (progn
                (unread-char c stream)
                (setq args (nreverse args))
                (return))
              (progn
                (setq args (cons (intern (string-upcase (string c))) args))
                (go getchar)))))
     (let ((x (read stream t nil t)))
       `(lambda ,args ,@(if (eq 'progn (car x)) (cdr x) (list x)))))))

(defun id (x) x)
(defun const (x) (lambda (_) (declare (ignore _)) x))
(defun duplist (l) (mapcar λx(if (typep x 'list) (duplist x) x) l))

(defun calltimes (n f)
  (when (> n 0) (cons #*(f n) (calltimes (- n 1) f))))

;; there is definitely a more efficient method to do this
;; but this is easy to write
(defun range (min max)
  (mapcar λx(+ x (- min 1)) (nreverse (calltimes (- max min) #'id))))

(defun split-list (l)
  (cons
   (remove-if
    #'null
    (mapcar λxn(when (evenp n) x) l (range 0 (length l))))
   (remove-if
    #'null
    (mapcar λxn(when (oddp n) x) l (range 0 (length l))))))

(defmacro match (val eq def &body body)
  `(cond
     ,@(mapcar λx(list (list eq val (car x)) (cadr x)) body)
     (t ,def)))

(defmacro mapcar^2 (f l) `(mapcar (lambda (l2) (mapcar ,f l2)) ,l))

;; let mods
(defmacro let-1 (sym val &body body) `(let ((,sym ,val)) ,@body))
(defmacro let-n (syms vals &body body) `(let ,(mapcar #'list syms vals) ,@body))

;; LOOP as it should be: an indefinite loop
(defmacro loop (&body body)
  (let-1 start (gensym)
    `(block nil (tagbody ,start ,@body (go ,start)))))

(defmacro loop-until (cond ret &body body)
  `(loop (when ,cond (return ,ret)) ,@body))
(defmacro loop-while (cond ret &body body)
  `(loop (unless ,cond (return ,ret)) ,@body))

;; i like stacks :3
(defparameter *stack* nil)
(defun s (x) (setq *stack* (cons x *stack*)))
(defun spop () (let ((out (car *stack*))) (setq *stack* (cdr *stack*)) out))
(defmacro with-stack (&body body) `(let ((*stack* nil)) ,@body))

;; since cons cells are passed by reference,
;; this allows you to write functions which take variables in the parent scope
(defun ref (x) (cons x nil))
(defun deref (ref) (car ref))
(defun setref (ref x) (rplaca ref x))

(defun read-input () (read-from-string (read-line)))
(defun get-var (prompt) (format t "~a: " prompt) (finish-output) (read-input))
(defun get-var-optional (default prompt)
  (format t "~a (default ~a): " prompt default) (finish-output)
  (let-1 input (read-line)
    (if (equal input "")
        default
        (read-from-string input))))
(defun get-var! (ref prompt)
  (format t "~a: " prompt) (finish-output)
  (setref ref (read-input)))
(defun get-var-optional! (ref prompt)
  (format t "~a (default ~a): " prompt (deref ref)) (finish-output)
  (let-1 input (read-line)
    (unless (equal input "") (setref ref (read-from-string input)))))

;; should be a struct or something but i'm a cons addict and never
;; bothered to learn how to make data structures with any other tools
(defpackage "POINT"
  (:export "POINT"
           "DUP"
           "X" "Y"
           "SETX" "SETY"
           "+"))
(defun point:point (x y) (cons x y))
(defun point:dup (p) (cons (car p) (cdr p)))
(defun point:x (p) (car p))
(defun point:y (p) (cdr p))
(defun point:setx (p x) (rplaca p x))
(defun point:sety (p y) (rplacd p y))
(defun point:+ (p o)
  (point:point (+ (point:x p) (point:x o)) (+ (point:y p) (point:y o))))
(defun point (x y) (point:point x y)) ;; convenience function

(defun player-id>icon (id)
  (match id eql " "
    (0 "X") (1 "O") (2 "#") (3 "*") (4 ".") (5 "&") (6 "%") (7 "7")))

(defun aset (a x &rest indexes)
  ;; due to the standards writers being lazy, there is no non-setf array setter
  (setf (apply #'aref a indexes) x))
(defun vset (v x index)
  (aset v x index))

(defun make-vector (length &optional &key (initial-contents nil))
  (make-array length :initial-contents initial-contents))
(defun vector-foldr (f b v)
  (do ((i 0 (+ 1 i))
       (val b #*(f (aref v i) val)))
      ((= i (length v)) val)))
(defun vector-map (f v)
  (do ((out (make-vector (length v)))
       (i 0 (+ 1 i)))
      ((= i (length v)) out)
    (vset out #*(f (svref v i)) i)))

(defun array-foldr (f b a)
  (vector-foldr f b (make-array
                     (apply #'* (array-dimensions a)) :displaced-to a)))
(defun array-map-2d (f a)
  ;; i couldn't figure out how to write a generalised array-map,
  ;; but this is currently good enough for our purposes
  (let-1 dim (array-dimensions a)
    (do ((out (make-array dim))
         (x 0 (+ 1 x)))
        ((= x (car dim)) out)
      (do ((y 0 (+ 1 y))) ((= y (cadr dim)))
        (aset out #*(f (aref a x y)) x y)))))
(defun array-2d>list (a)
  ;; considering the mess array-map was looking to be,
  ;; i'm not even going to try to generalise this
  (let-1 dim (array-dimensions a)
    (do ((out nil)
         (y 0 (+ 1 y)))
        ((= y (car dim)) (nreverse out))
      (setq out (cons (do ((row nil)
                           (x 0 (+ 1 x)))
                          ((= x (cadr dim)) (nreverse row))
                        (setq row (cons (aref a x y) row)))
                      out)))))

(defpackage "DEQUE"
  (:export "CONS"
           "PREV" "GET" "NEXT"
           "HEAD" "TAIL"
           "PREPEND" "APPEND"
           "POPL" "POPR"
           "JOIN" "LINK" "BREAK"
           "PRINT"))
;; terminology
;; 'left' and 'right' links - equivalent to 'prev' and 'next' links
;; 'well-formed' deque - a deque where for all elements A and B, either:
;;   - the prev link of A is B and the next link of B is A
;;   - the next link of A is B and the prev link of B is A
;;   - both of the above
;;   - A and B are not linked to each other
;; 'cyclic' deque - a deque where for all elements A,
;;   repeated application of PREV or NEXT returns A
;;
;; warning: deques must be printed via DEQUE:PRINT as the pretty-printer
;; does not handle doubly-linked structures well
(defun deque:cons (x) (vector nil x nil))
(defun deque::is-deque (d) (and d (typep d 'vector) (= (length d) 3)))
(defun deque:prev (d) (when (and (deque::is-deque d) (svref d 0)) (svref d 0)))
(defun deque:get (d) (when (deque::is-deque d) (svref d 1)))
(defun deque:next (d) (when (and (deque::is-deque d) (svref d 2)) (svref d 2)))
(defun deque:head (d)
  ;; warning: does not handle cyclic deques
  (if (deque:prev d) (deque:head (deque:prev d)) d))
(defun deque:tail (d)
  ;; warning: does not handle cyclic deques
  (if (deque:next d) (deque:tail (deque:next d)) d))
(defun deque:prepend (d x)
  (let-n (c d) ((deque:cons x) (deque:head d)) (vset d c 0) (vset c d 2)))
(defun deque:append (d x)
  (let-n (c d) ((deque:cons x) (deque:tail d)) (vset d c 2) (vset c d 0)))
(defun deque:popl (d)
  (let-1 c (deque:head d)
    (vset (deque:next c) nil 0) (vset c nil 2) (deque:get c)))
(defun deque:popr (d)
  (let-1 c (deque:tail d)
    (vset (deque:prev c) nil 2) (vset c nil 0) (deque:get c)))
(defun deque:join (d1 d2)
  ;; warning: can create cyclic deques
  (let-n (left right) ((deque:tail d1) (deque:head d2))
    (vset left right 2)
    (vset right left 0)))
(defun deque:link (d1 d2)
  ;; warning: can create cyclic deques
  (vset d1 d2 2) (vset d2 d1 0))
(defun deque:break (d &optional (left nil))
  ;; warning: requires break point to be between elements;
  ;; breaking at head-prev or tail-next will fail
  (if left
      (let-1 o (deque:prev d)
        (vset o nil 2) (vset d nil 0)
        (cons o d))
      (let-1 o (deque:next d)
        (vset o nil 0) (vset d nil 2)
        (cons d o))))
(defun deque:print (d_)
  (do ((d (deque:head d_) (deque:next d))
       (out "#<deque" (format nil "~a ~:[~s~;~*~a~]" out
                              (deque::is-deque (deque:get d))
                              (deque:get d)
                              (deque:print (deque:get d)))))
      ((null d) (concatenate 'string out ">"))))

(defpackage "BOARD"
  (:export "CREATE"
           "GET" "SET"
           "PRINT"
           "EMPTYP" "FULLP"
           "CHECK-AT-POINT"))
(defun board:create (width height)
  (make-array (list width height) :initial-element -1))
(defun board:get (board p) (aref board (point:x p) (point:y p)))
(defun board:set (board p val)
  (setf (aref board (point:x p) (point:y p)) val))
(defun board:print (board)
  (format nil "~{~{ ~a ~^│~}~%~^~:*~{~*───~^┼~}~%~}"
          (array-2d>list (array-map-2d #'player-id>icon board))))
(defun board:emptyp (board)
  (array-foldr λxa(if (> 0 x) a nil) t board))
(defun board:fullp (board)
  (array-foldr λxa(if (> 0 x) nil a) t board))

(defun board:check-at-point (board width height point win-len)
  (let-1 piece (board:get board point)
    (let-1 l
        (split-list
         (mapcar
          (lambda (offset)
            (let ((p (point:dup point))
                  (tr (lambda (pt) (point:+ pt offset)))
                  (c 0))
              (loop-until
               (or (not (and (< -1 (point:x p) width)
                             (< -1 (point:y p) height)))
                   (not (eql (board:get board p) piece))
                   (= c win-len))
               c
               (setq p (f tr p))
               (setq c (+ 1 c)))))
          (list (point -1 0) (point 1 0)
                (point 0 -1) (point 0 1)
                (point -1 -1) (point 1 1)
                (point 1 -1) (point -1 1))))
      (when (car
             (remove-if
              #'null
              (mapcar λab(> (+ a b) win-len) (car l) (cdr l))))
        piece))))

(defun game ()
  (let* ((width (get-var-optional 3 "board width"))
         (height (get-var-optional 3 "board height"))
         (win-len (get-var-optional 3 "winning path length"))
         (players (get-var-optional 2 "player count"))
         (board (board:create width height))
         (player 0)
         (turn 0))
    (loop
     (format t "~a" (board:print board))
     (let-n (point)
	 ((loop
	   (let-n (x y)
               ((get-var (format nil "~a's x position"
                                 (player-id>icon player)))
		(get-var (format nil "~a's y position"
                                 (player-id>icon player))))
	     (when (and (< -1 x width)
			(< -1 y height)
			(> 0 (board:get board (point x y))))
	       (return (point x y)))
	     (format t "that position is not available, try again~%"))))
       (board:set board point player)
       (when (board:check-at-point
              board width height
              point
              win-len)
         (return (format t "~a~a won!~%turns taken: ~a"
                         (board:print board)
                         (player-id>icon player)
                         (+ 1 turn)))))
     (when (board:fullp board)
       (return (format t "~agame was a tie!" (board:print board))))
     (setq player (+ 1 player))
     (when (= player players) (setq player 0))
     (setq turn (+ 1 turn)))))
