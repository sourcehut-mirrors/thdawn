(define (chapter14 task doremi hazuki aiko)
  (define bossinfo (enm-extras doremi))
  (set! current-chapter 14)
  (play-music (musbundle-naisho-yo-ojamajo music))
  (bossinfo-healthbars-set!
   bossinfo
   (immutable-vector
	;; final
	(make-healthbar 4 1 #xffd700ff #xffd700ff)
	;; survival
	(make-healthbar 4 1 #xffd700ff #xffd700ff)
	;; aiko sp2
	(make-healthbar 4 1 #x00ffffff #x00ffffff)
	;; hazuki sp2
	(make-healthbar 4 1 #xffa500ff #xffa500ff)
	;; doremi sp2
	(make-healthbar 4 1 #xff69fcff #xff69fcff)
	;; group sp2
	(make-healthbar 4 1 #xffd700ff #xffd700ff)
	;; aiko sp1
	(make-healthbar 4 1 #x00ffffff #x00ffffff)
	;; hazuki sp1
	(make-healthbar 4 1 #xffa500ff #xffa500ff)
	;; doremi sp1
	(make-healthbar 4 1 #xff69fcff #xff69fcff)
	;; group sp1
	(make-healthbar 25 0 #xffd700ff #xffd700ff)
	;; group non1
	(make-healthbar -1 0 #xf5f5f5ff #x808080ff)))
  (declare-nonspell doremi 1800 1000)
  (wait-while
   (lambda ()
	 (and (positive? (bossinfo-remaining-timer bossinfo))
		  (positive? (enm-health doremi)))))
  (group-sp1 task doremi hazuki aiko))


(define (group-sp1 task doremi hazuki aiko)
  (define bossinfo (enm-extras doremi))
  (set! current-chapter 15)
  (bossinfo-healthbars-set!
   bossinfo
   (vector-pop (bossinfo-healthbars bossinfo)))
  (declare-spell doremi 2)
  (wait-while
   (lambda ()
	 (and (positive? (bossinfo-remaining-timer bossinfo))
		  (positive? (enm-health doremi)))))
  (common-spell-postlude bossinfo doremi)
  )
