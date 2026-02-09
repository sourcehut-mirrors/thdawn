;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is derived from the SRFI-171 reference implementation,
;; originally at https://github.com/scheme-requests-for-implementation/srfi-171/
;; It has been modified to suit Chez Scheme and this particular program, thus
;; it may not be of use to other programs.
;; 
;; Copyright 2019 Linus Björnstam
;;
;; You may use this code under either the license in the SRFI document or the
;; license below.
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all source copies.
;; The software is provided "as is", without any express or implied warranties.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(library (srfi171meta)
  (export
   compose
   reduced reduced?
   unreduce
   ensure-reduced
   preserving-reduced

   list-reduce
   vector-reduce
   string-reduce
   bytevector-u8-reduce
   port-reduce
   generator-reduce)
  (import (chezscheme))

  (define compose
	(case-lambda
	  [() values]
	  [(f) f]
	  [(f g)
	   (lambda args
		 (call-with-values (lambda () (apply g args)) f))]
	  [(f g h)
	   (lambda args
		 (call-with-values
			 (lambda () (call-with-values (lambda () (apply h args)) g))
		   f))]
	  [functions
	   (define (make-chain chain thunk)
		 (lambda args
		   (call-with-values (lambda () (apply thunk args)) chain)))
	   (if (null? functions)
		   values
		   (fold-left make-chain (car functions) (cdr functions)))]))

  ;; A reduced value is stops the transduction.
  (define-record-type (<reduced> reduced reduced?)
	(fields val))
  (alias unreduce <reduced>-val)

  ;; helper function which ensures x is reduced.
  (define (ensure-reduced x)
	(if (reduced? x)
		x
		(reduced x)))


  ;; helper function that wraps a reduced value twice since reducing functions (like list-reduce)
  ;; unwraps them. tconcatenate is a good example: it re-uses it's reducer on it's input using list-reduce.
  ;; If that reduction finishes early and returns a reduced value, list-reduce would "unreduce"
  ;; that value and try to continue the transducing process.
  (define (preserving-reduced reducer)
	(lambda (a b)
      (let ((return (reducer a b)))
		(if (reduced? return)
			(reduced return)
			return))))


  ;; This is where the magic tofu is cooked
  (define (list-reduce f identity lst)
	(if (null? lst)
		identity
		(let ((v (f identity (car lst))))
          (if (reduced? v)
              (unreduce v)
              (list-reduce f v (cdr lst))))))

  (define (vector-reduce f identity vec)
	(let ((len (vector-length vec)))
      (let loop ((i 0) (acc identity))
		(if (= i len)
			acc
			(let ((acc (f acc (vector-ref vec i))))
              (if (reduced? acc)
                  (unreduce acc)
                  (loop (+ i 1) acc)))))))

  (define (string-reduce f identity str)
	(let ((len (string-length str)))
      (let loop ((i 0) (acc identity))
		(if (= i len)
			acc
			(let ((acc (f acc (string-ref str i))))
              (if (reduced? acc)
                  (unreduce acc)
                  (loop (+ i 1) acc)))))))

  (define (bytevector-u8-reduce f identity vec)
	(let ((len (bytevector-length vec)))
      (let loop ((i 0) (acc identity))
		(if (= i len)
			acc
			(let ((acc (f acc (bytevector-u8-ref vec i))))
              (if (reduced? acc)
                  (unreduce acc)
                  (loop (+ i 1) acc)))))))

  (define (port-reduce f identity reader port)
	(let loop ((val (reader port)) (acc identity))
      (if (eof-object? val)
          acc
          (let ((acc (f acc val)))
			(if (reduced? acc)
				(unreduce acc)
				(loop (reader port) acc))))))

  (define (generator-reduce f identity gen)
	(let loop ((val (gen)) (acc identity))
      (if (eof-object? val)
          acc
          (let ((acc (f acc val)))
			(if (reduced? acc)
				(unreduce acc)
				(loop (gen) acc)))))))
