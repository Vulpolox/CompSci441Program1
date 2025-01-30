#lang racket

; Assignment #1

(require racket/random)
(require rackunit)

; --- FILE READING ---------------------------------

; in:  string file name
; out: list of integers
(define (process-file file-name)
  (define raw-data (file->lines file-name))
  (define number-list (string-split (string-join raw-data " ") " "))

  (map string->number number-list))

; --- COMPUTATION ----------------------------------

; in:  takes a hash table and a key
; out: returns a copy of the hash table with the value of the passed key incremented by one
(define (hash-incrementer hash-table key)
  (define old-value (hash-ref hash-table key [lambda () 0])) ; reference to the value found in the original hash ; set to 0 if it doesn't exist
  (hash-set hash-table key (+ old-value 1)))                 ; return updated hash table


; in:  takes a list of integers
; out: returns a frequency hash table
(define (create-frequency-hash lst [hash (make-immutable-hash '())])
                               (if [empty? lst]
                                   hash
                                   [create-frequency-hash (cdr lst) (hash-incrementer hash (car lst))]
                                   ))


; in:  takes an integer value and amount and a list of integers
; out: appends 'amount' # of 'value' to the end of the list and returns it
(define (list-adder value amount [lst '()])
  (let loop ([count amount] [acc lst])
    (if (= count 0)
        acc
        (loop (- count 1) (cons value acc)))
    )
  )


; in:  a list of integers
; out: a sorted list of integers
(define (quicksort lst)
  (if (or (empty? lst) (empty? (rest lst)))
      lst
      (let* ([pivot (first lst)]
             [smaller (filter (λ (x) (< x pivot)) (rest lst))]
             [larger (filter (λ (x) (>= x pivot)) (rest lst))])
        (append (quicksort smaller) (list pivot) (quicksort larger)))))


; in:  a frequency hash
; out: a sorted list
(define (frequency-hash->sorted-list input-hash)

  (displayln "Sorting Keys\n---\n")
  (define sorted-keys (quicksort (hash-keys input-hash)))

  (displayln "Constructing Sorted List w/ Frequency Value Pairs\n---\n")
  (define (helper remaining-keys [output-list '()])

    (if (empty? remaining-keys)
        
        output-list
        
        [let* ([value (first remaining-keys)]
               [new-remaining-keys (rest remaining-keys)]
               [amount (hash-ref input-hash value)]
               [updated-output (list-adder value amount output-list)])
          (helper new-remaining-keys updated-output)]
        )
    )

  (reverse (helper sorted-keys))

  )


;  in: a list
; out: prints elements of list to console
(define (print-list lst)
  (for ([i lst])
    (printf "~a " i)))


; --- UI ---------------------------------------------

; in:  nothing
; out: returns a valid string file name from user input
(define (get-file-name)
  (display "Enter File Name\n")
  (define input (read-line))

  (if (file-exists? input)
      input
      (begin (display "File Does Not Exist\n")
             (get-file-name)))
  )

; in:  takes a function; asks user if they want to continue
; out: if user does, calls function
(define (continue? func)
  (display "\n---\nContinue? Y to Continue, Anything Else to Exit\n")
  (define input (read-line))

  (if [or (equal? input "Y")
          (equal? input "y")]
      (func)
      (display "---\n"))
  )
  

; --- Program ----------------------------------------

(define (execution-loop)
  (define file-name (get-file-name))
  (displayln "Creating Frequency Hash\n---\n")
  (define frequency-hash (create-frequency-hash (process-file file-name)))
  (define sorted-list (frequency-hash->sorted-list frequency-hash))
  (begin (print-list sorted-list)
         (continue? execution-loop))
  )

(execution-loop)


; --- Testing -----------------------------------------

