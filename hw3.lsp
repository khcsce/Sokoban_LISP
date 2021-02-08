;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;

;; Helper function used in  goal-test
;; Looks for boxes in each row using the given isBox helppr function
;; recursively called to check all cases.
;; Return: T if no box is found.
;; Return: NIL if the states not at the goal => box found

(defun row-goal(s)
	(if (null s)
		t
		(if (isBox (car s))
			nil
			(row-goal (cdr s))
		)
	)
)

(defun goal-test (s)
	(if (null s)
		t
		(and (row-goal (car s)) (goal-test (cdr s)))
	)
);end defun




#|
(defun row-goal(s)
	(cond ((null s) t)
		((isBox (car s)) nil) ; if a box is found , we are not at the goal
		(t (row-goal(cdr s))) ; else
	)
)


(defun goal-test (s)
	(cond ((null s) t)
		;;((row-goal(car s)) t)
		;;(t (goal-test (cdr s)))
		(t (and (row-goal (car s)) (goal-test (cdr s))))
	)
)|#






; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;

;; IDEA FROM SPEC: (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
;; So we want to create the try-move function


;;Finally, a state is simply a list of rows (top to bottom).
;;For simplicity, in this assignment we will assume that every row contains the same number of columns. We
;;will assume that each state contains exactly one keeper, at least one box, and that the number of goals is
;;greater than or equal to the number of boxes in the state.

;; Function: get-square
;; input: takes in a State S, a row number r, and a column number c.
;; Output: It should return the integer content of state S at square (r,c). 
;; If the square is outside the scope of the
;; problem, return the value of a wall = 1
(defun get-square (S r c)
	;; The functions that you are allowed to use are the same as those allowed in hw1 and hw2 plus: butlast,
	;; nthcdr, count
	;; http://clhs.lisp.se/Body/f_nthcdr.htm
	;; Nevermind spec says nthcdr
	;; and not just nth
	(cond ((null S) wall) ; null State just return wall ; edge case
		; out of bounds
		((< r 0) wall)
		((< c 0) wall)
		; at position (r,c) when r and c both equal to 0
		((and (= r 0) (= c 0)) (caar S))
		; Get to the next row
		((> r 0) (get-square (cdr S) (- r 1) c))
		; Get to the next column recursively
		((> c 0) (get-square (cons (cdar S) (cdr S)) r (- c 1)))
		(t wall) ; defualt case: return value of wall = 1
	)
)


;; Function: set-square-sub 
;; Input: row (r), col (c), v
;; Return: The list with element located replaced with v
(defun set-square-sub (r c v)
;; Explanation (cdr row) is basically the "other " elements 
;; Whiie (car row) is the current element being examined
	(cond ((null r) nil)
		((= c 0) (cons v (cdr r)))
		(t (cons (car r) (set-square-sub (cdr r) (- c 1) v)))
	)
)



;; Function: set-square
;; Input: state S, a row number r, a column number c, and
;; a square content v (integer). 
;; Output: This function should return a new state S’ that is obtained by setting
;; the square (r,c) to value v. 
;; But if (r,c) is invalid, no changes are made.
(defun set-square(S r c v)
	(cond ((null S) nil) ; Spec doesn't specify to return nil or just the S back
		((= r 0) (cons (set-square-sub (car S) c v) (cdr S)))
		(t (cons (car s) (set-square (cdr S) (- r 1) c v)))
	)
)


;; Function: char-status (S r c k t1 t2)
;; Input: Input: state S, a row number r, a column number c, the player k,
;; element that is one move away t1, element that is two moves away t2
;; Returns a new state S'
;; Helper function that is used in try-move

;; Reason for 2 element finding, simplifies  code to check for star and blank

(defun char-status (S r c k t1 t2)
	;; Kept getting errors so use cond clauses instead
	#|(if (isWall t1) ;; towards a wall => invalid!!
		nil
		(if (and (isBox t1) (not (or (isStar t2) (isBlank t2)))) ;; push the box , but move after that is neither a goal or an empty spot
			nil
		)
		;; Move the keeper
		(if (= k keeper)
			(set-square S r c blank)
		)
		(if (= k keeperstar)
			(set-square S r c star)
		)
	)|#
	(cond ((isWall t1) nil)
		((and (isBox t1) (not (or (isStar t2) (isBlank t2)))) 
			nil ;; return nil if second element away is not a goal / empty spot
		)
		((= k keeper)
			(set-square S r c blank)
		)
		((= k keeperstar)
			(set-square S r c star)
		)
	)
)


;; Function: try-move
;; Input : takes in a state S and a move direction D. This function should
;; Return: the state that is the result of moving the keeper in state S in direction D. 
;; NIL returned if the move is invalid (e.g. there is a wall in that direction).
;; updates the content of every square to the right value
;; uses set-square and get-square helper functions
(defun try-move (S D)
  ; get the position of the keeper
  	(let* ((p (getKeeperposition S 0)) (c (car p)) (r (car (cdr p))) (k (get-square S r c)))
		(cond
			((equal D 'UP) ; entity h
				(let ((x (char-status S r c k (get-square S (- r 1) c) (get-square S (- r 2) c))))
					(cond ((isBlank (get-square S (- r 1) c)) (set-square x (- r 1) c keeper))
						((and (isBox (get-square S (- r 1) c)) (isBlank (get-square S (- r 2) c))) (set-square (set-square x (- r 1) c keeper) (- r 2) c box))
						((and (isBox (get-square S (- r 1) c)) (isStar (get-square S (- r 2) c))) (set-square (set-square x (- r 1) c keeper) (- r 2) c boxstar))
						((isStar (get-square S (- r 1) c)) (set-square x (- r 1) c keeperstar))
						((and (isBoxStar (get-square S (- r 1) c)) (isBlank (get-square S (- r 2) c))) (set-square (set-square x (- r 1) c keeperstar) (- r 2) c box))
						((and (isBoxStar (get-square S (- r 1) c)) (isStar (get-square S (- r 2) c))) (set-square (set-square x (- r 1) c keeperstar) (- r 2) c boxstar))
					) ; cond end
				) ; closing let
			);closing equal

			((equal D 'DOWN)
				(let ((x (char-status S r c k (get-square S (+ r 1) c) (get-square S (+ r 2) c))))
					(cond ((isBlank (get-square S (+ r 1) c)) (set-square x (+ r 1) c keeper))
						((and (isBox (get-square S (+ r 1) c)) (isBlank (get-square S (+ r 2) c))) (set-square (set-square x (+ r 1) c keeper) (+ r 2) c box))
						((and (isBox (get-square S (+ r 1) c)) (isStar (get-square S (+ r 2) c))) (set-square (set-square x (+ r 1) c keeper) (+ r 2) c boxstar))
						((isStar (get-square S (+ r 1) c)) (set-square x (+ r 1) c keeperstar))
						((and (isBoxStar (get-square S (+ r 1) c)) (isBlank (get-square S (+ r 2) c))) (set-square (set-square x (+ r 1) c keeperstar) (+ r 2) c box))
						((and (isBoxStar (get-square S (+ r 1) c)) (isStar (get-square S (+ r 2) c))) (set-square (set-square x (+ r 1) c keeperstar) (+ r 2) c boxstar))
					) ; cond end
				) ; closing let
			); closing equal

			((equal D 'LEFT)
				(let ((x (char-status S r c k (get-square S r (- c 1)) (get-square S r (- c 2)))))
					(cond ((isBlank (get-square S r (- c 1))) (set-square x r (- c 1) keeper))
						((and (isBox (get-square S r (- c 1))) (isBlank (get-square S r (- c 2)))) (set-square (set-square x r (- c 1) keeper) r (- c 2) box))
						((and (isBox (get-square S r (- c 1))) (isStar (get-square S r (- c 2)))) (set-square (set-square x r (- c 1) keeper) r (- c 2) boxstar))
						((isStar (get-square S r (- c 1))) (set-square x r c keeperstar))
						((and (isBoxStar (get-square S r (- c 1))) (isBlank (get-square S r (- c 2)))) (set-square (set-square x r (- c 1) keeperstar) r (- c 2) box))
						((and (isBoxStar (get-square S r (- c 1))) (isStar (get-square S r (- c 2)))) (set-square (set-square x r (- c 1) keeperstar) r (- c 2) boxstar))
					) ; cond end
				) ; closing let
			) ; closing 

			((equal D 'RIGHT) 
				(let ((x (char-status S r c k (get-square S r (+ c 1)) (get-square S r (+ c 2)))))
					(cond ((isBlank (get-square S r (+ c 1))) (set-square x r (+ c 1) keeper))
						((and (isBox (get-square S r (+ c 1))) (isBlank (get-square S r (+ c 2)))) (set-square (set-square x r (+ c 1) keeper) r (+ c 2) box))
						((and (isBox (get-square S r (+ c 1))) (isStar (get-square S r (+ c 2)))) (set-square (set-square x r (+ c 1) keeper) r (+ c 2) boxstar))
						((isStar (get-square S r (+ c 1))) (set-square x r (+ c 1) keeperstar))
						((and (isBoxStar (get-square S r (+ c 1))) (isBlank (get-square S r (+ c 2)))) (set-square (set-square x r (+ c 1) keeperstar) r (+ c 2) box))
						((and (isBoxStar (get-square S r (+ c 1))) (isStar (get-square S r (+ c 2)))) (set-square (set-square x r (+ c 1) keeperstar) r (+ c 2) boxstar))
					) ; cond end
				) ; closing let
			)
		) ; outer cond
	); end let
) ; end fun


;; Expected Behavior:
;; The function try-move has to check whether the move is legal. If it is, it will need to update some
;; squares of the current state to reflect the changes. Use set-square to help you do this. According to this
;; strategy, at most three squares need to be updated for any single move (make sure you agree).

(defun next-states (s)
	(cleanUpList (list (try-move s 'UP) (try-move s 'DOWN) (try-move s 'LEFT) (try-move s 'RIGHT)))
)


; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)  ; haha
 	0
)




; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;; This function is admissible since it measures how close the current state
;; is to the solution. If there exists n boxxes in the state S, at least n moves
;; will be needed to move all the n boxes into a goal state. The cost of reaching
;; goal state is not overestimated
(defun h1 (s)
;; The functions that you are allowed to use are the same as those allowed in hw1 and hw2 plus: butlast,
;;  nthcdr, count

;; Use count to simplify process : Don't need to make helper functions that would hav et to count the number
;; of boxes each row 

;; Documentation: https://jtra.cz/stuff/lisp/sclr/count.html
	(cond ((null s) 0)
		(t (+ (count box (first s)) (h1 (rest s))))) ;; count number of misplaced boxes in state

;; https://stackoverflow.com/questions/29907440/difference-between-cdr-car-and-rest-first-and-possible-implementation
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
(defun h105123806 (s)
	(h1 s)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
