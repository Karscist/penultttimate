;;------------------------------------------------------------------------------
;;-------------------------- reference 80 column line --------------------------

(defpackage "penultttimate-level-1" (:use "COMMON-LISP") (:shadow "DEREF"))
(in-package "penultttimate-level-1")

;; this will be partially written in a pipettish style because
;; i forgot how to write decent CL (and don't want to)

(defmacro f (function) (list 'funcall function))

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
(defun modref (ref x) (setf (car ref) x))

(defun read-input () (read-from-string (read-line)))
(defun get-var (ref prompt)
  (format t "~a: " prompt)
  (finish-output)
  (modref ref (read-input)))
(defun get-var-optional (ref prompt)
  (format t "~a (default ~a): " prompt (deref ref))
  (finish-output)
  (let-1 input (read-line)
    (unless (equal input "") (modref ref (read-from-string input)))))

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


