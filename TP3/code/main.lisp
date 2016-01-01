(defun l ()
	(load "tp3_test.lisp"))

(load "base_faits.lisp")
(load "base_regle.lisp")
(load "chainage_avant.lisp")

(defun qui_gagne? (pk1 lv1 pk2 lv2)
	(format t "~%~%================ ~A VS ~A ================~%" pk1 pk2)
	(let ((BF *baseFaits*)
		  (BR (générer_BR)))
		(setq BF (ajouter_fait BF 'pk1 pk1))
		(setq BF (ajouter_fait BF 'pk2 pk2))
		(setq BF (ajouter_fait BF 'pk1level lv1))
		(setq BF (ajouter_fait BF 'pk2level lv2))

		;(print BF)
		;(print BR)

		(chainage_avant 'gagnant BF BR)
		)
	)

(qui_gagne? "salamèche" 5 "carapuce" 10)
(qui_gagne? "aquali" 20 "carapuce" 21)
(qui_gagne? "pikachu" 20 "carapuce" 30)
(qui_gagne? "bulbizare" 20 "carapuce" 21)