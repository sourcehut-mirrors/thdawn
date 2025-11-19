(define (adjust-bars-non healthbars)
  ;; add upcoming nonspell (replacing the spell that just expired)
  (vector-set! healthbars (sub1 (vlen healthbars))
			   (make-healthbar -1 0 #xf5f5f5ff #x808080ff))
  ;; make upcoming spell bigger and remove its padding
  (let ([sp (vnth healthbars (- (vlen healthbars) 2))])
	(healthbar-width-set! sp 25)
	(healthbar-post-padding-set! sp 0)))

(define (group-non1 task doremi hazuki aiko)
  (define bossinfo (enm-extras doremi))
  (set! current-chapter 14)
  (play-music (musbundle-naisho-yo-ojamajo music))
  (bossinfo-healthbars-set!
   bossinfo
   (vector
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
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi)))))
  (cancel-all #t)
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
  (doremi-non1 task doremi hazuki aiko))

(define (doremi-non1 task doremi hazuki aiko)
  (define bossinfo (enm-extras doremi))
  (set! current-chapter 16)
  (wait 90)
  (adjust-bars-non (bossinfo-healthbars bossinfo))
  (spawn-subtask "hazuki leave"
	(lambda (_)
	  (ease-to ease-out-cubic -100.0 -100.0 30 hazuki)
	  (delete-enemy hazuki))
	(constantly #t)
	task)
  (spawn-subtask "aiko leave"
	(lambda (_)
	  (ease-to ease-out-cubic 100.0 -100.0 30 aiko)
	  (delete-enemy aiko))
	(constantly #t)
	task)
  (declare-nonspell doremi 1800 1000)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi)))))
  (cancel-all #t)
  (doremi-sp1 task doremi))

(define (doremi-sp1 task doremi)
  (define bossinfo (enm-extras doremi))
  (set! current-chapter 17)
  (bossinfo-healthbars-set!
   bossinfo
   (vector-pop (bossinfo-healthbars bossinfo)))
  (declare-spell doremi 3)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi)))))
  (common-spell-postlude bossinfo doremi)
  (hazuki-non1 task doremi))

(define (hazuki-non1 task doremi)
  (define bars (bossinfo-healthbars (enm-extras doremi)))
  (define hazuki
	(spawn-enemy (enmtype boss-hazuki) 100.0 -100.0 500
				 (lambda (task enm)
				   (ease-to ease-out-cubic +middle-boss-x+ +middle-boss-y+
							30 enm))
				 '()
				 (thunk #f)))
  (define bossinfo (blank-bossinfo "Fujiwara Hazuki" #xffa500ff))
  (set! current-chapter 18)
  (spawn-subtask "doremi leave"
	(lambda (_)
	  (ease-to ease-out-cubic -100.0 -100.0 30 doremi)
	  (delete-enemy doremi))
	(constantly #t)
	task)
  (adjust-bars-non bars)
  (bossinfo-healthbars-set! bossinfo bars)
  (enm-extras-set! hazuki bossinfo)
  (declare-nonspell hazuki 1800 1000)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health hazuki))))))
