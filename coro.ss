;; Every Scheme 101 course's "implement coroutines with continuations" assignment
;; Assumptions:
;; - Only one thread interacts with this system at all times
;; - No nonlocal jumps into or out of run-tasks
(library (coro)
  (export wait wait-until yield spawn-task kill-task run-tasks task-count)
  (import (chezscheme))

  (define-record-type task
	(fields
	 name
	 keep-running?
	 pre-hook
	 post-hook
	 (mutable continuation)))

  (define (yield0-fallback _resume)
	(error 'yield "yield0 called with no active coroutine"))
  ;; The yield function for the current coroutine.
  ;; Should be set to the proper continuation on every (re)entry into a coroutine,
  ;; but is provided as a top-level value to avoid having to thread the yield-function
  ;; down into any utility functions, etc.
  ;; When not executing any coroutine, set to yield0-fallback
  (define yield0 yield0-fallback)

  ;; Yield the current coroutine to the next one in line to run
  ;; If this is called outside coroutine context, raises an error.
  (define (yield)
	;; scheduler will resume and provide us with a new yield function
	(define next-yield0 (call/1cc yield0))
	(set! yield0 next-yield0))

  ;; Yields the specified number of times before returning
  (define (wait n)
	(do [(i 0 (add1 i))]
		[(>= i n)]
	  (yield)))

  ;; Yields until (pred) becomes true.
  ;; If pred is already true when called, does not perform any yields.
  (define (wait-until pred)
	(let loop ([satisfied (pred)])
	  (unless satisfied
		(yield)
		(loop (pred)))))

  (define loop-running? #f)
  (define task-queue '())
  (define tasks-to-add '())
  (define tasks-to-remove (make-eq-hashtable))

  ;; Enqueue thunk as a new coroutine to run.
  ;; If called while coroutine loop is being run, the spawned task does not begin
  ;; execution until the next iteration of the loop.
  ;; If thunk returns any value, the coroutine is treated as completed.
  ;;
  ;; keep-running?: Nullary predicate, if it returns false the task is removed
  ;; pre-hook: Called before each time the coroutine resumes.
  ;;  The value it returns is passed to the post-hook
  ;; post-hook: Called after each time the coroutine resumes. Return value is ignored.
  ;;  Also called on the final exit after the coroutine exits normally.
  (define spawn-task
	(case-lambda
	  [(name thunk keep-running?)
	   (spawn-task name thunk keep-running? (lambda () #f) values)]
	  [(name thunk keep-running? pre-hook post-hook)
	   (let* ([task (make-task name keep-running? pre-hook post-hook #f)]
			  [scaffolded-coroutine
			   (lambda (yielder)
				 ;; add initial entry/final exit scaffolding
				 (set! yield0 yielder)
				 (thunk task)
				 (yield0 #f))])
		 (task-continuation-set! task scaffolded-coroutine)
		 (if loop-running?
			 (set! tasks-to-add
				   (cons task tasks-to-add))
			 (set! task-queue
				   (cons task task-queue)))
		 task)]))

  ;; Terminates a task. If called during run-tasks, takes effect only after the current
  ;; run-tasks call ends.
  (define (kill-task task)
	(eq-hashtable-set! tasks-to-remove task #t))

  (define (task-count)
	(+ (length task-queue)
	   (length tasks-to-add)))

  ;; Run one iteration of the task loop
  (define (run-tasks)
	(dynamic-wind
	  (lambda () (set! loop-running? #t))
	  (lambda ()
		;; Run all tasks once, filtering out the ones that no longer need to run
		;; we use fold-left to explicitly get left-to-right iteration order.
		;; scheme does not specify an ordering for map/filter and indeed Chez does not
		;; go in order.
		(define next-task-queue-rev
		  (fold-left (lambda (acc task)
					   (if ((task-keep-running? task))
						   (let ([pre-data ((task-pre-hook task))])
							 (define next-continuation
							   (call/1cc (task-continuation task)))
							 ((task-post-hook task) pre-data)
							 (if (procedure? next-continuation)
								 (let ()
								   (task-continuation-set! task next-continuation)
								   (cons task acc))
								 ;; Not a procedure, task is done.
								 acc))
						   acc))
					 '()
					 task-queue))
		;; Add any new tasks that arose during the iteration and
		;; process pending deletions
		(set! task-queue
			  (append! tasks-to-add
					   (reverse! (remp
								  (lambda (task)
									(eq-hashtable-contains? tasks-to-remove task))
								  next-task-queue-rev))))
		(hashtable-clear! tasks-to-remove)
		(set! tasks-to-add '()))
	  (lambda ()
		(set! loop-running? #f)
		(set! yield0 yield0-fallback)))))
