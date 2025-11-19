(define (adjust-bars-non healthbars)
  ;; add upcoming nonspell (replacing the spell that just expired)
  (vector-set! healthbars (sub1 (vlen healthbars))
			   (make-healthbar -1 0 #xf5f5f5ff #x808080ff))
  ;; make upcoming spell bigger and remove its padding
  (let ([sp (vnth healthbars (- (vlen healthbars) 2))])
	(healthbar-width-set! sp 25)
	(healthbar-post-padding-set! sp 0))
  healthbars)

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
   (thunk
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
		 (positive? (enm-health hazuki)))))
  (cancel-all #t)
  (hazuki-sp1 task hazuki))

(define (hazuki-sp1 task hazuki)
  (define bossinfo (enm-extras hazuki))
  (set! current-chapter 19)
  (bossinfo-healthbars-set!
   bossinfo
   (vector-pop (bossinfo-healthbars bossinfo)))
  (declare-spell hazuki 4)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health hazuki)))))
  (common-spell-postlude bossinfo hazuki)
  (aiko-non1 task hazuki))

(define (aiko-non1 task hazuki)
  (define bars (bossinfo-healthbars (enm-extras hazuki)))
  (define aiko
	(spawn-enemy (enmtype boss-aiko) 100.0 -100.0 500
				 (lambda (task enm)
				   (ease-to ease-out-cubic +middle-boss-x+ +middle-boss-y+
							30 enm))
				 '()
				 (thunk #f)))
  (define bossinfo (blank-bossinfo "Senoo Aiko" #x00ffffff))
  (set! current-chapter 20)
  (spawn-subtask "hazuki leave"
	(lambda (_)
	  (ease-to ease-out-cubic -100.0 -100.0 30 hazuki)
	  (delete-enemy hazuki))
	(constantly #t)
	task)
  (adjust-bars-non bars)
  (bossinfo-healthbars-set! bossinfo bars)
  (enm-extras-set! aiko bossinfo)
  (declare-nonspell aiko 1800 1000)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health aiko)))))
  (cancel-all #t)
  (aiko-sp1 task aiko))

(define (aiko-sp1 task aiko)
  (define bossinfo (enm-extras aiko))
  (set! current-chapter 21)
  (bossinfo-healthbars-set!
   bossinfo
   (vector-pop (bossinfo-healthbars bossinfo)))
  (declare-spell aiko 5)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health aiko)))))
  (common-spell-postlude bossinfo aiko)
  (group-non2 task aiko))

(define (group-non2 task aiko)
  (define bossinfo (blank-bossinfo "Harukaze Doremi" #xff69fcff))
  (define doremi
	(spawn-enemy (enmtype boss-doremi) 100.0 -100.0 500
				 (lambda (task enm)
				   (ease-to ease-out-cubic +middle-boss-x+ +middle-boss-y+
							30 enm))
				 '()
				 (thunk #f)))
  (define hazuki
	(spawn-enemy (enmtype boss-hazuki) -100.0 -100.0 500
				 (lambda (task enm)
				   (ease-to ease-out-cubic +left-boss-x+ +left-boss-y+
							30 enm))
				 '()
				 (thunk #f)))
  (set! current-chapter 22)
  (enm-extras-set! doremi bossinfo)
  (enm-extras-set! hazuki (blank-bossinfo "Fujiwara Hazuki" #xffa500ff))
  (bossinfo-healthbars-set!
   bossinfo
   (adjust-bars-non (bossinfo-healthbars (enm-extras aiko))))
  (bossinfo-healthbars-set! (enm-extras aiko) '#())
  (ease-to ease-out-cubic +right-boss-x+ +right-boss-y+ 30 aiko)
  (declare-nonspell doremi 1800 1000)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi)))))
  (cancel-all #t)
  (group-sp2 task doremi hazuki aiko))

(define (group-sp2 task doremi hazuki aiko)
  (define bossinfo (enm-extras doremi))
  (set! current-chapter 23)
  (bossinfo-healthbars-set!
   bossinfo
   (vector-pop (bossinfo-healthbars bossinfo)))
  (declare-spell doremi 6)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi)))))
  (common-spell-postlude bossinfo doremi)
  (doremi-non2 task doremi hazuki aiko))

(define (doremi-non2 task doremi hazuki aiko)
  (define bossinfo (enm-extras doremi))
  (set! current-chapter 24)
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
  (doremi-sp2 task doremi))

(define (doremi-sp2 task doremi)
  (define bossinfo (enm-extras doremi))
  (set! current-chapter 25)
  (bossinfo-healthbars-set!
   bossinfo
   (vector-pop (bossinfo-healthbars bossinfo)))
  (declare-spell doremi 7)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi)))))
  (common-spell-postlude bossinfo doremi)
  (hazuki-non2 task doremi))

(define (hazuki-non2 task doremi)
  (define bars (bossinfo-healthbars (enm-extras doremi)))
  (define hazuki
	(spawn-enemy (enmtype boss-hazuki) 100.0 -100.0 500
				 (lambda (task enm)
				   (ease-to ease-out-cubic +middle-boss-x+ +middle-boss-y+
							30 enm))
				 '()
				 (thunk #f)))
  (define bossinfo (blank-bossinfo "Fujiwara Hazuki" #xffa500ff))
  (set! current-chapter 26)
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
		 (positive? (enm-health hazuki)))))
  (cancel-all #t)
  (hazuki-sp2 task hazuki))

(define (hazuki-sp2 task hazuki)
  (define bossinfo (enm-extras hazuki))
  (set! current-chapter 27)
  (bossinfo-healthbars-set!
   bossinfo
   (vector-pop (bossinfo-healthbars bossinfo)))
  (declare-spell hazuki 8)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health hazuki)))))
  (common-spell-postlude bossinfo hazuki)
  (aiko-non2 task hazuki))

(define (aiko-non2 task hazuki)
  (define bars (bossinfo-healthbars (enm-extras hazuki)))
  (define aiko
	(spawn-enemy (enmtype boss-aiko) 100.0 -100.0 500
				 (lambda (task enm)
				   (ease-to ease-out-cubic +middle-boss-x+ +middle-boss-y+
							30 enm))
				 '()
				 (thunk #f)))
  (define bossinfo (blank-bossinfo "Senoo Aiko" #x00ffffff))
  (set! current-chapter 28)
  (spawn-subtask "hazuki leave"
	(lambda (_)
	  (ease-to ease-out-cubic -100.0 -100.0 30 hazuki)
	  (delete-enemy hazuki))
	(constantly #t)
	task)
  (adjust-bars-non bars)
  (bossinfo-healthbars-set! bossinfo bars)
  (enm-extras-set! aiko bossinfo)
  (declare-nonspell aiko 1800 1000)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health aiko)))))
  (cancel-all #t)
  (aiko-sp2 task aiko))

(define (aiko-sp2 task aiko)
  (define bossinfo (enm-extras aiko))
  (set! current-chapter 29)
  (bossinfo-healthbars-set!
   bossinfo
   (vector-pop (bossinfo-healthbars bossinfo)))
  (declare-spell aiko 9)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health aiko)))))
  (common-spell-postlude bossinfo aiko)
  (group-non3 task aiko))

(define (group-non3 task aiko)
  (define bossinfo (blank-bossinfo "Harukaze Doremi" #xff69fcff))
  (define doremi
	(spawn-enemy (enmtype boss-doremi) 100.0 -100.0 500
				 (lambda (task enm)
				   (ease-to ease-out-cubic +middle-boss-x+ +middle-boss-y+
							30 enm))
				 '()
				 (thunk #f)))
  (define hazuki
	(spawn-enemy (enmtype boss-hazuki) -100.0 -100.0 500
				 (lambda (task enm)
				   (ease-to ease-out-cubic +left-boss-x+ +left-boss-y+
							30 enm))
				 '()
				 (thunk #f)))
  (set! current-chapter 30)
  (enm-extras-set! doremi bossinfo)
  (enm-extras-set! hazuki (blank-bossinfo "Fujiwara Hazuki" #xffa500ff))
  (bossinfo-healthbars-set!
   bossinfo
   (adjust-bars-non (bossinfo-healthbars (enm-extras aiko))))
  (bossinfo-healthbars-set! (enm-extras aiko) '#())
  (ease-to ease-out-cubic +right-boss-x+ +right-boss-y+ 30 aiko)
  (declare-nonspell doremi 1800 1000)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi)))))
  (cancel-all #t)
  (group-sp3 task doremi hazuki aiko))

(define (group-sp3 task doremi hazuki aiko)
  (define bossinfo (enm-extras doremi))
  (set! current-chapter 31)
  (bossinfo-healthbars-set!
   bossinfo
   (vector-pop (bossinfo-healthbars bossinfo)))
  ;; widen to full width
  (healthbar-width-set! (vnth (bossinfo-healthbars bossinfo)
							  (sub1 (vlen (bossinfo-healthbars bossinfo))))
						-1)
  (declare-spell doremi 10)
  (wait-while
   (thunk (positive? (bossinfo-remaining-timer bossinfo))))
  (common-spell-postlude bossinfo doremi)
  (group-sp4 task doremi hazuki aiko))

(define (group-sp4 task doremi hazuki aiko)
  (define bossinfo (enm-extras doremi))
  (set! current-chapter 32)
  (bossinfo-healthbars-set!
   bossinfo
   (vector-pop (bossinfo-healthbars bossinfo)))
  (wait 90)
  ;; widen to full width
  (healthbar-width-set! (vnth (bossinfo-healthbars bossinfo)
							  (sub1 (vlen (bossinfo-healthbars bossinfo))))
						-1)
  (declare-spell doremi 11)
  (wait-while
   (thunk
	(and (positive? (bossinfo-remaining-timer bossinfo))
		 (positive? (enm-health doremi)))))
  (common-spell-postlude bossinfo doremi)
  (common-boss-postlude bossinfo doremi #f)
  (common-boss-postlude bossinfo hazuki #f)
  (common-boss-postlude bossinfo aiko #f)
  (wait 120)
  (stage-ctx-dialogue-set!
   current-stage-ctx
   (with-input-from-file "assets/dialogue/postbattle.dat" read))
  (stage-ctx-dialogue-idx-set! current-stage-ctx 0)
  (wait-until (thunk (not (stage-ctx-dialogue current-stage-ctx))))
  ;; todo formula
  (let ([clear-bonus 10000000])
	(set! current-score (+ current-score clear-bonus))
	(spawn-particle
	 (make-particle
	  (particletype clear-bonus)
	  ;; Position dynamically calculated at render to avoid
	  ;; needing to access the fonts here
	  0.0 0.0 240 0
	  (format "Clear Bonus: ~:d" clear-bonus))))
  (when (is-liveplay)
	(let ([pair (assq 'games-cleared play-data)])
	  (set-cdr! pair (add1 (cdr pair))))
	(save-play-data play-data))
  (wait 300)
  (replace-gui (mk-pause-gui
				(if (is-liveplay)
					(pausetype gameclear)
					(pausetype replaydone)))))
