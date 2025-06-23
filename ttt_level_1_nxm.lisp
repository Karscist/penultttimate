;;------------------------------------------------------------------------------
;;-------------------------- reference 80 column line --------------------------

(setq *print-circle* t)

(defpackage "penultttimate-level-1"
  (:use "COMMON-LISP")
  (:shadow "DEREF" "LOOP"))
(in-package "penultttimate-level-1")

;; this will be partially written in a pipettish style because
;; i forgot how to write decent CL (and don't want to)

(defmacro f (function &rest rest) `(funcall ,function ,@rest))

(defun id (x) x)
(defun const (x) (lambda (_) (declare (ignore _)) x))
(defun duplist (l) (mapcar (lambda (x) (if (typep x 'list) (duplist x) x)) l))

(defun calltimes (n f)
  (when (> n 0) (cons (funcall f n) (calltimes (- n 1) f))))

;; there is definitely a more efficient method to do this
;; but this is easy to write
(defun range (min max)
  (mapcar (lambda (x) (- x (+ 1 min))) (nreverse (calltimes (- max min) #'id))))

(defun split-list (l)
  (cons
   (remove-if
    #'null
    (mapcar (lambda (x n) (when (evenp n) x)) l (range 0 (length l))))
   (remove-if
    #'null
    (mapcar (lambda (x n) (when (oddp n) x)) l (range 0 (length l))))))

(defmacro match (val eq def &body body)
  `(cond
     ,@(mapcar (lambda (x) (list (list eq val (car x)) (cadr x))) body)
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
(defun get-var (ref prompt)
  (format t "~a: " prompt)
  (finish-output)
  (setref ref (read-input)))
(defun get-var-optional (ref prompt)
  (format t "~a (default ~a): " prompt (deref ref))
  (finish-output)
  (let-1 input (read-line)
    (unless (equal input "") (setref ref (read-from-string input)))))

;; should be a struct or something but i'm a cons addict and never
;; bothered to learn how to make data structures with any other tools
(defun point (x y) (cons x y))
(defun dup-point (p) (cons (car p) (cdr p)))
(defun point-x (p) (car p))
(defun point-y (p) (cdr p))
(defun point-setx (p x) (rplaca p x))
(defun point-sety (p y) (rplacd p y))
(defun point-+ (p o)
  (point (+ (point-x p) (point-x o)) (+ (point-y p) (point-y o))))

(defun player-id>icon (id)
  (match id eql " "
    (0 "X") (1 "O") (2 "#") (3 "*") (4 ".") (5 "&") (6 "%") (7 "7")))

(defun board-create (width height)
  (duplist (calltimes height (const (calltimes width (const nil))))))
(defun board-get (board p) (nth (point-x p) (nth (point-y p) board)))
(defun board-set (board p val)
  (setf (nth (point-x p) (nth (point-y p) board)) val))
(defun board-print (board)
  (format nil "~{~{ ~a ~^│~}~%~^~:*~{~*───~^┼~}~%~}"
          (mapcar^2 #'player-id>icon board)))

(defun <x< (min x max) (and (> x min) (< x max)))

(defun board-check-at-point (board width height point win-len)
  (let-1 piece (board-get board point)
    (let-1 l
        (split-list
         (mapcar
          (lambda (offset)
            (let ((p (dup-point point))
                  (tr (lambda (pt) (point-+ pt offset)))
                  (c 0))
              (loop-until
               (or (not (and (<x< -1 (point-x p) width)
                             (<x< -1 (point-y p) height)))
                   (not (eql (board-get board p) piece))
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
              (mapcar (lambda (a b) (> (+ a b) win-len)) (car l) (cdr l))))
        piece))))

;; user-facing stuff

(defun get-board-info ()
  (eval ; this is not production code
   `(let-n
        (width height win-len players)
        ,(mapcar (lambda (x) `(ref ,x)) '(3 3 3 2))
      (get-var-optional width "board width")
      (get-var-optional height "board height")
      (get-var-optional win-len "how many aligned pieces needed for a win")
      (get-var-optional players "player count")
      (list width height win-len players))))

(defun game ()
  (let* ((info (get-board-info))
         (width (deref (car info)))
         (height (deref (cadr info)))
         (win-len (deref (caddr info)))
         (players (deref (cadddr info)))
         (board (board-create width height))
         (player 0)
         (turn 0))
    (loop
     (format t (board-print board))
     (let-n (x y) ((ref nil) (ref nil))
       (loop
        (get-var x (format nil "~a's x position" (player-id>icon player)))
        (get-var y (format nil "~a's y position" (player-id>icon player)))
        (when (and (<x< -1 (deref x) width)
                   (<x< -1 (deref y) height)
                   (null (board-get board (point (deref x) (deref y)))))
          (return))
        (format t "that position is not available, try again~%"))
       (board-set board (point (deref x) (deref y)) player)
       (when (board-check-at-point
              board width height
              (point (deref x) (deref y))
              win-len)
         (return (format t "~a won!~%turns taken: ~a"
                         (player-id>icon player) turn))))
     (when (null
            (remove-if
             #'null
             (mapcar (lambda (x) (remove-if-not #'null x)) board)))
       (return (format t "game was a tie!")))
     (setq player (+ 1 player))
     (when (= player players) (setq player 0))
     (setq turn (+ 1 turn)))))
