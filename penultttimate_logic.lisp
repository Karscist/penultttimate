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
   (let ((args nil) (c nil) (l nil) (ignored 0) (ign-args nil) (rest nil))
     (block nil
       (tagbody
        getchar
          (setq c (read-char stream t nil t))
          (cond
            ((or (eql c #\space) (eql c #\newline)) (go getchar))
            ((or (eql c #\() (eql c #\.))
             (when (eql c #\() (unread-char c stream))
             (setq args (nreverse args))
             (return))
            ((eql c #\[)
             (tagbody
              nextchar
                (setq c (read-char stream t nil t))
                (if
                 (and (eql c #\]) l)
                 (progn
                   (setq
                    args
                    (cons (intern (string-upcase (coerce (nreverse l) 'string)))
                          args))
                   (setq l nil)
                   (go getchar))
                 (progn
                   (setq l (cons c l))
                   (go nextchar)))))
            ((eql c #\_)
             (let ((x (intern (format nil "_~a" ignored))))
               (setq args (cons x args))
               (setq ign-args (cons x ign-args)))
             (setq ignored (+ 1 ignored))
             (go getchar))
            ((eql c #\*)
             (tagbody
              nextchar
                (setq c (read-char stream t nil t))
                (cond
                  ((eql c #\()
                   (unread-char c stream)
                   (go end))
                  ((eql c #\.) (go end))
                  ((and (null rest) (eql c #\_))
                   (setq ign-args (cons '_rest ign-args))
                   (setq rest '(#\t #\s #\e #\r #\_))
                   (setq c (read-char stream t nil t))
                   (unless (eql c #\.) (unread-char c stream))
                   (go end))
                  (t (setq rest (cons c rest))
                     (go nextchar)))
              end
                (when rest
                  (setq rest
                        (intern
                         (string-upcase (coerce (nreverse rest) 'string))))
                  (setq args (cons rest (cons '&rest args)))
                  (setq args (nreverse args)))))
            (t
             (progn
               (setq args (cons (intern (string-upcase (string c))) args))
               (go getchar))))))
     (let ((x (read stream t nil t)))
       (if (and (typep x 'list) (eq 'progn (car x)))
           (setq x (cdr x)) (setq x (list x)))
       (when ign-args (setq x (cons `(declare (ignore ,@ign-args)) x)))
       `(lambda ,args ,@x)))))

;; let mods
(defmacro let-1 (sym val &body body) `(let ((,sym ,val)) ,@body))
(defmacro let-n (syms vals &body body) `(let ,(mapcar #'list syms vals) ,@body))

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

(defun mapcar-until (f def list)
  (if list
      (let-1 x #*(f (car list))
             (if (cdr x)
                 (car x)
                 (mapcar-until f def (cdr list))))
      def))
(defun match-alist (val eq def alist)
  (mapcar-until
   λx(if #*(eq val (car x)) (cons (cdr x) t) '(()))
   def
   alist))

(defmacro mapcar^2 (f l) `(mapcar λ[l2](mapcar ,f l2) ,l))

;; LOOP as it should be: an indefinite loop
(defmacro loop (&body body)
  (let-1 start (gensym)
    `(block nil (tagbody ,start ,@body (go ,start)))))

(defmacro loop-until (cond ret &body body)
  `(loop (when ,cond (return ,ret)) ,@body))
(defmacro loop-while (cond ret &body body)
  `(loop (unless ,cond (return ,ret)) ,@body))

;; use an alist as a binding environment
(defun geta (alist sym)
  (when alist
    (if (eq (caar alist) sym)
        (cdar alist)
        (geta (cdr alist) sym))))
(defun seta (alist sym val)
  (if (car alist)
    (if (eq (caar alist) sym)
        (rplacd (car alist) val)
        (seta (cdr alist) sym val))
    (rplacd alist (cons (cons sym val) nil))))
(defparameter *env* (list nil))
(defmacro gete (sym) `(geta *env* ',sym))
(defmacro sete (sym val) `(seta *env* ',sym ,val))
(defmacro with-env (&body body) `(let-1 *env* (list nil) ,@body))

(defun make-simple-object-f (messages)
  (let-1 o
    λ[self].
    λm*args.
    (apply (match-alist
            m #'eq nil
            (mapcar λx(cons (car x) #*((cdr x) #*(self self))) messages))
           args)
    #*(o o)))
(defmacro make-simple-object (&body messages)
  `(with-env
       (make-simple-object-f
        (list ,@(mapcar λx.`(cons ',(car x) ,(cadr x)) messages)))))

(defun make-simple-object-data-f (messages data)
  (let-1 o
    λ[self].
    λm*args.
    (apply (match-alist
            m #'eq nil
            (mapcar λx(cons (car x) #*((cdr x) #*(self self) data)) messages))
           args)
    #*(o o)))
(defmacro make-simple-object-data (data &body messages)
  `(make-simple-object-data-f
    (list ,@(mapcar λx.`(cons ',(car x) ,(cadr x)) messages))
    ,data))

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

(defun point (x y)
  (make-simple-object-data (cons x y)
    (dup λ__.λ(point x y))
    (x λ_d.λ(car d))
    (y λ_d.λ(cdr d))
    (setx λsd.λx(progn (rplaca d x) s))
    (sety λsd.λx(progn (rplacd d x) s))
    (+ λs_.λo(point (+ #*(s 'x) #*(o 'x)) (+ #*(s 'y) #*(o 'y))))))

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

(defun deque (x)
  (make-simple-object-data (vector nil x nil)
    (object λ__.λm*_(when (eq m 'type) 'deque))
    (private λ_d.λm*args(match m eq nil
                          ('setprev (vset d (car args) 0))
                          ('setnext (vset d (car args) 2))))
    (get λ_d.λ(svref d 1))
    (set λsd.λx(progn (vset d x 1) s))
    (prev λ_d.λ(svref d 0))
    (next λ_d.λ(svref d 2))
    (head λs_.λ(if #*(s 'prev) #*(#*(s 'prev) 'head) s))
    (tail λs_.λ(if #*(s 'next) #*(#*(s 'next) 'tail) s))
    (prepend λs_.λx(let-n (c h) ((deque x) #*(s 'head))
                     #*(h 'private 'setprev c)
                     #*(c 'private 'setnext h)
                     c))
    (append λs_.λx(let-n (c tl) ((deque x) #*(s 'tail))
                    #*(tl 'private 'setnext c)
                    #*(c 'private 'setprev tl)
                    c))
    (popl λs_.λ(let-1 h #*(s 'head)
                      (let-1 n #*(h 'next)
                             #*(n 'private 'setprev nil)
                             #*(h 'private 'setnext nil)
                             (values h n))))
    (popr λs_.λ(let-1 tl #*(s 'tail)
                      (let-1 p #*(tl 'prev)
                             #*(p 'private 'setnext nil)
                             #*(tl 'private 'setprev nil)
                             (values tl p))))
    (print λs_.λ
           (do ((q #*(s 'head) #*(q 'next))
                (out "#<deque"
                     (let-1 is-deque
                         (and (typep #*(q 'get) 'function)
                              (ignore-errors
                                (eq 'deque #*(#*(q 'get) 'object 'type))))
                       (format nil "~a ~:[~s~;~*~a~]" out
                               is-deque
                               #*(q 'get)
                               (when is-deque #*(#*(q 'get) 'print))))))
               ((null q) (concatenate 'string out ">"))))))

(defun make-board (width height &optional (inner -1))
  (make-simple-object
    (init λs.λ(progn
                (sete board
                      (make-array (list width height) :initial-element inner))
                s))
    (get λ_.λp(aref (gete board) #*(p 'x) #*(p 'y)))
    (set λs.λpx(progn (aset (gete board) x #*(p 'x) #*(p 'y)) s))
    (empty? λ_.λ(array-foldr λx[acc](if (> 0 x) acc nil) t (gete board)))
    (full? λ_.λ(array-foldr λx[acc](if (> 0 x) nil acc) t (gete board)))
    (print
     λ_.λ(format nil "~{~{ ~a ~^│~}~%~^~:*~{~*───~^┼~}~%~}"
                 (array-2d>list (array-map-2d #'player-id>icon (gete board)))))
    (check λs.λp[win-len]
           (let-1 piece #*(s 'get p)
             (let-1 l
                 (split-list
                  (mapcar
                   (lambda (offset)
                     (let ((p #*(p 'dup))
                           (tr (lambda (pt) #*(pt '+ offset)))
                           (c 0))
                       (loop-until
                        (or (not (and (< -1 #*(p 'x) width)
                                      (< -1 #*(p 'y) height)))
                            (not (eql #*(s 'get p) piece))
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
                 piece))))))

(defun game ()
  (let* ((width (get-var-optional 3 "board width"))
         (height (get-var-optional 3 "board height"))
         (win-len (get-var-optional 3 "winning path length"))
         (players (get-var-optional 2 "player count"))
         (board #*((make-board width height) 'init))
         (player 0)
         (turn 0))
    (loop
     (format t "~a" #*(board 'print))
     (let-n (point)
	 ((loop
	   (let-n (x y)
               ((get-var (format nil "~a's x position"
                                 (player-id>icon player)))
		(get-var (format nil "~a's y position"
                                 (player-id>icon player))))
	     (when (and (< -1 x width)
			(< -1 y height)
			(> 0 #*(board 'get (point x y))))
	       (return (point x y)))
	     (format t "that position is not available, try again~%"))))
       #*(board 'set point player)
       (when #*(board 'check point win-len)
             (return (format t "~a~a won!~%turns taken: ~a"
                             #*(board 'print)
                             (player-id>icon player)
                             (+ 1 turn)))))
     (when #*(board 'full?)
           (return (format t "~agame was a tie!" #*(board 'print))))
     (setq player (+ 1 player))
     (when (= player players) (setq player 0))
     (setq turn (+ 1 turn)))))
