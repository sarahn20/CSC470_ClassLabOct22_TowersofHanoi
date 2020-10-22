(define initGame
  (lambda ()
    (list (list 1 2 3)'()'())))

(define FirstTower
  (lambda (gameBoard)
    (car gameBoard)))

(define SecondTower
  (lambda (gameBoard)
    (cadr gameBoard)))

(define ThirdTower
  (lambda (gameBoard)
    (caddr gameBoard)))

(define towerCheck
  (lambda (given-tower source-tower dest-tower)
    (cond
         ((equal? source-tower given-tower)
          (cdr given-tower))
         ((equal? dest-tower given-tower)
          (cons (car source-tower) dest-tower))
         (else given-tower))))

(define transform
  (lambda (num gameBoard)
    (cond
      ((eq? num 1)(FirstTower gameBoard))
      ((eq? num 2)(SecondTower gameBoard))
      ((eq? num 3)(ThirdTower gameBoard)))))


(define makeMove
  (lambda (gameBoard source-tower dest-tower)
    (cond
    ((null? (transform source-tower gameBoard)) #f)
    ((eq? (length (transform dest-tower gameBoard)) 3) #f)
    ((or (null? (transform dest-tower gameBoard))
         (< (car (transform source-tower gameBoard))(car (transform dest-tower gameBoard))))
     (list
      (if (or (eq? source-tower 1)(eq? dest-tower 1))
       (towerCheck (FirstTower gameBoard) (transform source-tower gameBoard) (transform dest-tower gameBoard))
       (FirstTower gameBoard))
      (if (or (eq? source-tower 2)(eq? dest-tower 2))
       (towerCheck (SecondTower gameBoard) (transform source-tower gameBoard) (transform dest-tower gameBoard))
       (SecondTower gameBoard))
      (if (or (eq? source-tower 3)(eq? dest-tower 3))
       (towerCheck (ThirdTower gameBoard) (transform source-tower gameBoard) (transform dest-tower gameBoard))
       (ThirdTower gameBoard))))
    ((> (car (transform source-tower gameBoard))(car (transform dest-tower gameBoard))) #f))))


(define isWinner
  (lambda (gameBoard)
    (eq? (length (ThirdTower gameBoard)) 3)))

(define myGame (initGame))
myGame
(isWinner (makeMove (makeMove (makeMove (makeMove (makeMove (makeMove (makeMove myGame 1 3) 1 2) 3 2) 1 3) 2 1) 2 3) 1 3))
