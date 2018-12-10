;Exercise 3.15.  Draw box-and-pointer diagrams to explain the effect of set-to-wow! on the structures z1 and z2 above.
(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

;done
;Exercise 3.16.  Ben Bitdiddle decides to write a procedure to count the number of pairs in any list structure. ``It's easy,'' he reasons. ``The number of pairs in any structure is the number in the car plus the number in the cdr plus one more to count the current pair.'' So Ben writes the following procedure:

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;Show that this procedure is not correct. In particular, draw box-and-pointer diagrams representing list structures made up of exactly three pairs for which Ben's procedure would return 3; return 4; return 7; never return at all.
;answer: this doesn't check i there are repeats in the list, it doesn't count DISTINCT pairs.
(count-pairs (list (cons 3 4) (cons 3 4) (cons 4 5) (cons 5 6)))
(count-pairs (list 1 2 3))

;Exercise 3.17.  Devise a correct version of the count-pairs procedure of exercise 3.16 that returns the number of distinct pairs in any structure. (Hint: Traverse the structure, maintaining an auxiliary data structure that is used to keep track of which pairs have already been counted.)
(define (count-pairs1 input)
  (let ((seenbe4 '()))
    (define (uncounted inp)
      (if (member? inp seenbe4)
        0
        (begin (set! seenbe4 (cons inp seen)) 1)))
    (define (counter input)
      (if (not (pair? nput)
      0
      (+ (counter (cdr nput)
         (counter (car nput)
         (uncounted nput))))))))
  (counter input))
;Exercise 3.22.  Instead of representing a queue as a pair of pointers, we can build a queue as a procedure with local state. The local state will consist of pointers to the beginning and the end of an ordinary list. Thus, the make-queue procedure will have the form

;(define (make-queue)
;  (let ((front-ptr ...)
;        (rear-ptr ...))
;    <definitions of internal procedures>
;    (define (dispatch m) ...)
;    dispatch))

;Complete the definition of make-queue and provide implementations of the queue operations using this representation.
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    ; empty-queue? is written to align with the way front-ptr
    ; and rear-ptr were given, above
    (define (empty-queue?)
      (if (null? (cons front-ptr rear-ptr))
      #t
      #f))

    ; peek returns the datum at the front of the queue
    ; peek returns #f if the queue is empty
    (define (peek)
      (cond ((empty-queue?) (error "Empty queue.  :-("))
            (else (car front-ptr))))

    ; insert-queue! plays out differently depending on whether the queue
    ; is currently empty or not
    (define (insert-queue! datum)
      (let ((new-node (cons datum '())))
        (if (empty-queue?) 
          (set! rear-ptr new-node)
          (begin (set-car! front-ptr new-node)(set-car! rear-ptr new-mode)))))

    ; delete-queue! has three possibilties:
    ; * empty queue
    ; * one element in queue
    ; * more than one element in queue
    (define (delete-queue!)
      (cond ((empty-queue?) (error "Empty queue.  :-("))
            (else 
                    ; store the datum at the head of the queue
                  (let ((return-value (peek)))
                    ; update the front pointer
                    (set! front-ptr (car rear-ptr))
                    (set! rear-ptr (cdr rear-ptr))
                    ; If there was only one thing in the queue, then the
                    ; rear-ptr will need to be set to nil
                    (if (null? front-ptr) 
                      (set! rear-ptr null))
                    ; Now return the element of the queue (or #f)
                    (begin(empty-queue?) (cons front-ptr rear-ptr))))))

    (define (dispatch message)
      (cond ((eq? message 'insert-queue!) insert-queue!)
            ((eq? message 'delete-queue!) delete-queue!)
            ((eq? message 'peek) peek)
            ((eq? message 'empty?) empty-queue?)))
    dispatch))
(define q1 (make-queue))



;delete table
(define (make-table)
  (cons 'table ()))
(define (empty-table? t) (null? (cdr t)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

        
(define (lookup k t)
  (let ((record (assoc k (cdr t))))
    (cond (record (cdr record))
          (else #f))))
(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (rlookup k t)
  (let ((record (rassoc k (cdr t))))
    (cond (record (car record))
          (else #f))))
(define (rassoc value records)
  (cond ((null? records) #f)
        ((equal? value (cdar records)) (car records))
        (else (rassoc value (cdr records)))))

(define (delete! k t)
  (cond ((empty-table?)#f)
        ((not (memq k t)#f));checks if is in there
        ((equals? (caadr t) k)(set-car! t (caddr t)))
        (else (delete! k (caddr t)))))
;Exercise 3.25.  Generalizing one- and two-dimensional tables, show how to implement a table in which values are stored under an arbitrary number of keys and different values may be stored under different numbers of keys. The lookup and insert! procedures should take as input a list of keys used to access the table.
(define (make-table) 
   (let ((local-table (list '*table*))) 
     
     (define (lookup keys) 
       (define (iter keys table) 
         (let ((subtable (assoc (car keys) (cdr table)))) 
           (if subtable 
               (cond ((null? (cdr keys)) (cdr subtable)) 
                     (else 
                     (iter (cdr keys) subtable))) 
             false))) 
       (iter keys local-table)) 
      
     (define (gen-new-list keys value) 
         (if (null? (cdr keys)) 
             (cons (car keys) value) 
             (list (car keys) (gen-new-list (cdr keys) value)))) 
      
     (define (insert! keys value) 
       (define (iter keys table) 
         (let ((subtable (assoc (car keys) (cdr table)))) 
           (if subtable 
               (cond ((null? (cdr keys)) 
                     (set-cdr! subtable value)) 
                    (else 
                      (iter (cdr keys) subtable))) 
               (set-cdr! table (cons (gen-new-list keys value) (cdr table))))) 
         'ok) 
       (iter keys local-table)) 
    
   (define (dispatch m) 
     (cond ((eq? m 'lookup) lookup) 
           ((eq? m 'insert!) insert!) 
           ((eq? m 'display) (display local-table)) 
           (else (error "Try again")))) 
   dispatch)) 




;Exercise 3.27.  Memoization (also called tabulation) is a technique that enables a procedure to record, in a local table, values that have previously been computed. This technique can make a vast difference in the performance of a program. A memoized procedure maintains a table in which values of previous calls are stored using as keys the arguments that produced the values. When the memoized procedure is asked to compute a value, it first checks the table to see if the value is already there and, if so, just returns that value. Otherwise, it computes the new value in the ordinary way and stores this in the table. As an example of memoization, recall from section 1.2.2 the exponential process for computing Fibonacci numbers:

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;The memoized version of the same procedure is

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

;where the memoizer is defined as

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

;Draw an environment diagram to analyze the computation of (memo-fib 3). Explain why memo-fib computes the nth Fibonacci number in a number of steps proportional to n. Would the scheme still work if we had simply defined memo-fib to be (memoize fib)?
;No, as simply doing (memoize fib) would have all the memoized values in other, temporary enviroments that are scrapped by the garbage collector before they can actually be looked at. It no longer is just memoize fib, but just fib.
