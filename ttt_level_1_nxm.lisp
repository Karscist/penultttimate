;;------------------------------------------------------------------------------
;;-------------------------- reference 80 column line --------------------------

(setq *print-circle* t)

(defpackage "penultttimate-level-1" (:use "COMMON-LISP") (:shadow "DEREF"))
(in-package "penultttimate-level-1")

;; this will be partially written in a pipettish style because
;; i forgot how to write decent CL (and don't want to)

(defmacro f (function) (list 'funcall function))

(defun const (x) (lambda (_) (declare (ignore _)) x))
(defun duplist (l) (mapcar (lambda (x) (if (typep x 'list) (duplist x) x)) l))

(defun calltimes (n f)
  (when (> n 0) (cons (funcall f n) (calltimes (- n 1) f))))

(defmacro match (val eq def &body body)
  `(cond
     ,@(mapcar (lambda (x) (list (list eq val (car x)) (cadr x))) body)
     (t ,def)))

(defmacro mapcar^2 (f l) `(mapcar (lambda (l2) (mapcar ,f l2)) ,l))

;; let mods
(defmacro let-1 (sym val &body body) `(let ((,sym ,val)) ,@body))
(defmacro let-n (syms vals &body body) `(let ,(mapcar #'list syms vals) ,@body))

;; i like stacks :3
(defparameter *stack* nil)
(defun s (x) (setq *stack* (cons x *stack*)))
(defun spop () (let ((out (car *stack*))) (setq *stack* (cdr *stack*)) out))
(defmacro with-stack (&body body) `(let ((*stack* nil)) ,@body))

;; since cons cells are passed by reference,
;; this allows you to write functions which take variables in the parent scope
(defun ref (x) (cons x nil))
(defun deref (ref) (car ref))
(defun setref (ref x) (setf (car ref) x))

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
(defun point-x (p) (car p))
(defun point-y (p) (cdr p))

(defun board-create (width height)
  (duplist (calltimes height (const (calltimes width (const nil))))))
(defun board-get (board p) (nth (point-x p) (nth (point-y p) board)))
(defun board-set (board p val)
  (setf (nth (point-x p) (nth (point-y p) board)) val))
(defun board-print (board)
  (let ((board-map
         (lambda (x)
           (match x eq " " (0 "X") (1 "O") (2 "#") (3 "*")))))
    (format nil "~{~{ ~a │~}~%~^~:*~{───~^┼~*~}──~%~}"
            (mapcar^2 board-map board))))

;; get board info

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


