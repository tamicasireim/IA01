(defun l ()
  (load "TD4.lisp"))

;représentation du labyrinthe par un arbre
(setq *laby* '((E 1) (1 E 2) (2 1 7) (7 2 6 8) (6 7 3) 
		  (7 2 8) (8 7 9) (9 8 10) (10 11 15 9) 
		  (11 10 14 12) (14 11) (12 11 5) (5 12 4) 
		  (4 5) (15 10 16) (16 15 17) (17 16 18)
		  (18 17 19) (19 18 20) (20 13 19 S)))

(defun successeurs (etat lab deja_visites)
  (let ((retour nil))
    (dolist (successeur (cdr (assoc etat lab)) retour)
      (when (not (member successeur deja_visites))
		(setq retour (append retour (list successeur)))))))

(defun explore (lab)
  (recherche lab 'E nil))

;Algo de recherche en profondeur : renvoie le chemin ou NIL si le chemin n'existe pas.
(defun recherche (lab etatActuel etatsVisites)
	(cond ((eq etatActuel 'S) etatsVisites)
		(T (let ((succs (successeurs etatActuel lab etatsVisites))
			(NetatsVisites (append etatsVisites (list etatActuel))))
		     (when succs 
			(dolist (succ succs)
				(let ((chemin (recherche lab succ NetatsVisites)))
					(when chemin
						(return-from recherche chemin)
						)
					)
				)
			)
		)
		)
		)
	)

