;;------------------------------------------------------------------------------
;;-------------------------- reference 80 column line --------------------------

(load "kl.lisp")

(defpackage "PENULTTTIMATE" (:use "KL"))
(in-package "PENULTTTIMATE")

(defun player-id>icon (id)
  (match id eql " "
    (0 "X") (1 "O") (2 "#") (3 "*") (4 ".") (5 "&") (6 "%") (7 "7")))

(defun make-board (width height &optional (inner -1))
  (make-simple-object
    ((board (make-array
             (list width height)
             :initial-element inner)))
    (get λ_.λp(aref board #*(p 'x) #*(p 'y)))
    (path-get
     ;; intrinsically linked to the structure MAKE-NESTED-BOARD produces
     λs(lambda (p &optional d)
         (let-1
             pt #*(p 'popl)
             (print #*(p 'print))
             (if (> #*(p 'length) 0)
                 #*((cdr #*(s 'get pt)) 'path-get p (cdr #*(s 'get pt)))
                 d))))
    (set λs.λpx(progn (aset board x #*(p 'x) #*(p 'y)) s))
    (empty? λ_.λ(array-foldr λx[acc](if (> 0 x) acc nil) t board))
    (full? λ_.λ(array-foldr λx[acc](if (> 0 x) nil acc) t board))
    (print
     λ_.λ(format nil "~{~{ ~a ~^│~}~%~^~:*~{~*───~^┼~}~%~}"
                 (array-2d>list
                  (array-map-2d #'player-id>icon board))))
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
                             (setq p #*(tr p))
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

(defun make-nested-board (width height level)
  (make-board
   width height
   (if (= 0 level)
       -1
       (cons -1 (make-nested-board width height (- level 1))))))

(defun game ()
  (let* ((width (get-var-optional 3 "board width"))
         (height (get-var-optional 3 "board height"))
         (level (get-var-optional 0 "board nesting level"))
         (win-len (get-var-optional 3 "winning path length"))
         (players (get-var-optional 2 "player count"))
         (board (make-nested-board width height level))
         (path (deque))
         (player 0)
         (turn 0))
    (loop
     (when (> level 0)
       (format t "board at path ~a~%" #*(path 'print)))
     (format t "~a~%" #*(board 'print))
     (let-n (point)
	 ((loop
           (when (> level 0) (format t "in board path ~a,~%" #*(path 'print)))
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
