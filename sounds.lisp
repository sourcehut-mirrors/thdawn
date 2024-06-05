(in-package :thdawn)

(defstruct sebundle
  "Bundle of loaded sound effects"
  spellcapture spelldeclare
  longcharge shortcharge
  enmdie bossdie playerdie playershoot
  shoot0 shoot1 shoot2
  extend graze bell
  oldvwoopfast oldvwoopslow
  pause menuselect
  timeout timeoutwarn)
(defvar sounds nil)
(defun load-sfx ()
  (flet ((lsfx (file) (raylib:load-sound (concatenate 'string "assets/sfx/" file))))
	(setf sounds
		  (make-sebundle
		   :spellcapture (lsfx "se_cardget.wav")
		   :spelldeclare (lsfx "se_cat00.wav")
		   :longcharge (lsfx "se_ch00.wav")
		   :shortcharge (lsfx "se_ch02.wav")
		   :enmdie (lsfx "se_enep00.wav")
		   :bossdie (lsfx "se_enep01.wav")
		   :playerdie (lsfx "se_pldead00.wav")
		   :playershoot (lsfx "se_plst00.wav")
		   :shoot0 (lsfx "se_tan00.wav")
		   :shoot1 (lsfx "se_tan01.wav")
		   :shoot2 (lsfx "se_tan02.wav")
		   :extend (lsfx "se_extend.wav")
		   :graze (lsfx "se_graze.wav")
		   :bell (lsfx "se_kira00.wav")
		   :oldvwoopfast (lsfx "se_power1.wav")
		   :oldvwoopslow (lsfx "se_power2.wav")
		   :pause (lsfx "se_pause.wav")
		   :menuselect (lsfx "se_select00.wav")
		   :timeout (lsfx "se_timeout.wav")
		   :timeoutwarn (lsfx "se_timeout2.wav")))))
(defun unload-sfx ()
  ;; meh, todo.
  )
