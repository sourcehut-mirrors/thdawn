;; Specialized sort operation for bullet rendering:
;; 1. operate upon fxvectors and use fxvector ops for everything
;; 2. temporary buffer is not reallocated between every sort call but is instead
;;    preallocated and reused
;;
;; Derived from the Chez Scheme codebase and used under the Apache 2.0 license (see
;; licenses/Apache-2.0.txt)
;; This code is
;;     Copyright (c) 1998 by Olin Shivers.
;; The terms are: You may do as you please with this code, as long as
;; you do not delete this notice or hold me responsible for any outcome
;; related to its use.
(library (sort)
  (export fxvector-sort-for-bullet-render!)
  (import (chezscheme))

  (define (domerge elt< target v1 v2 l len1 len2)
    (let* ([r1 (fx+ l len1)] [r2 (fx+ r1 len2)])
      (let lp ([i l] [j l] [x (fxvector-ref v1 l)] [k r1] [y (fxvector-ref v2 r1)])
        (if (elt< y x)
            (let ([k (fx+ k 1)])
              (fxvector-set! target i y)
              (if (fx< k r2)
                  (lp (fx+ i 1) j x k (fxvector-ref v2 k))
                  (vblit v1 j target (fx+ i 1) r1)))
            (let ([j (fx+ j 1)])
              (fxvector-set! target i x)
              (if (fx< j r1)
                  (lp (fx+ i 1) j (fxvector-ref v1 j) k y)
                  (unless (eq? v2 target)
                    (vblit v2 k target (fx+ i 1) r2))))))))
  (define (vblit fromv j tov i n)
	(fxvector-copy! fromv j tov i (fx- n j)))
  (define (getrun elt< v l r) ; assumes l < r
    (let lp ([i (fx+ l 1)] [x (fxvector-ref v l)])
      (if (fx= i r)
          (fx- i l)
          (let ([y (fxvector-ref v i)])
            (if (elt< y x) (fx- i l) (lp (fx+ i 1) y))))))
  (define temp-buffer (make-fxvector 2048))
  (define (dovsort! elt< v0)
	(define n (fxvector-length v0))
	(assert (fx= n (fxvector-length temp-buffer)))
    (let ([temp0 temp-buffer])
      (define (recur l want)
        (let lp ([pfxlen (getrun elt< v0 l n)] [v v0] [temp temp0])
          (if (or (fx>= pfxlen want) (fx= pfxlen (fx- n l)))
              (values pfxlen v)
              (let-values ([(outlen outvec) (recur (fx+ l pfxlen) pfxlen)])
                (domerge elt< temp v outvec l pfxlen outlen)
                (lp (fx+ pfxlen outlen) temp v)))))
      (let-values ([(outlen outvec) (recur 0 n)]) outvec)))

  (define (fxvector-sort-for-bullet-render! elt< v)
    (let ([n (fxvector-length v)])
      (unless (fx<= n 1)
        (let ([outvec (dovsort! elt< v)])
          (unless (eq? outvec v)
            (fxvector-copy! outvec 0 v 0 n))))))
  )
