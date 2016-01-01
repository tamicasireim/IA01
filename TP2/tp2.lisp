;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FONCTIONS UTILITAIRES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;renvoie le l'élément à la position pos de la liste (similaire à nth)
(defun nelem (liste pos)
  (if (eq pos 1)
    (car liste)
    (nelem (cdr liste) (- pos 1))))

;échange les éléments aux positions pos1 et pos2 d'une liste. (echange '(A B C D) 1 2) renvoie (B A C D)
(defun echange (liste pos1 pos2)
  (let ((pos_tmp 0) (retour nil))
    (dolist (elem liste retour)
      (incf pos_tmp 1)
      (cond ((eq pos_tmp pos1) (setq retour (append retour (list (nelem liste pos2)))))
	    ((eq pos_tmp pos2) (setq retour (append retour (list (nelem liste pos1)))))
	    (T (setq retour (append retour (list elem))))))))


;équivalent à la fonction member native de common lisp, mais gère le cas des listes de listes.
(defun is_member (item liste)
  (cond ((eq liste nil) NIL)
	((equal item (car liste)) T)
	(T (is_member item (cdr liste)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FONCTIONS POUR FACILITER LA MANIPULATION DES ETATS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;renvoie T si la liste est un état adéquat c'est à dire s'il n'y a pas de A après D et si ce n'est pas un état qui ait déjà été visité.
(defun allowed_state (etat etats_visites)
  (if (and (eq nil (member 'A (member 'D etat))) (not (is_member etat etats_visites)))
    T
    nil))

;Renvoie la liste des successeurs valides d'un état.
(defun successeurs (etat etats_visites)
  (let ((retour nil))
    (if (allowed_state (echange etat 1 2) etats_visites)
      (setq retour (list (echange etat 1 2))))
    (if (allowed_state (echange etat 2 3) etats_visites)
      (setq retour (append retour (list (echange etat 2 3)))))
    (if (allowed_state (echange etat 2 4) etats_visites)
      (setq retour (append retour (list (echange etat 2 4)))))
      retour))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Algorithme de recherche en profondeur d'abord
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;renvoie le chemin pour aller de l'état actuel à l'état recherché
(defun recherche_prof (etatActuel etatRecherche etatsVisites)
  (let ((NetatsVisites (append etatsVisites (list etatActuel))))
    (cond ((equal etatActuel etatRecherche) NetatsVisites)
	  (T (let ((succs (successeurs etatActuel NetatsVisites)))
	       (loop 
		 (when (not succs)
		   (return nil)
		   )
		 (let ((chemin (recherche_prof (pop succs) etatRecherche NetatsVisites)))
		   (when chemin
		     (return chemin)
		     )
		   )
		 )
	       )
	     )
	  )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ALGORITHME DE RECHERCHE SELON UNE HEURISTIQUE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FONCTIONS DE CALCUL POUR L'HEURISTIQUE

;renvoie T si les éléments 3 et 4 des états 1 et 2 sont identiques
(defun fin_liste (etat1 etat2)
  (if (and (eq (caddr etat1) (caddr etat2)) (equal (last etat1) (last etat2)))
    T
    Nil
    )
  )

;renvoie le nombre d'éléments en commun dans une liste
(defun lettre_commun (etat1 etat2)
  (let ((retour 0))
    (mapcar (lambda (x y)
	      (if (eq x y)
		(incf retour 1)
		)
	      )
	    etat1 etat2
	    )
    retour
    )
  )

; détermine le nombre d'états successeurs à un état donné
(defun nombre_successeurs (etat etatsVisites)
  (length (successeurs etat etatsVisites)))

; détermine si les successeurs d'un état sont solutions ou non
(defun is_solution (etat etatRecherche etatsVisites)
  (is_member etatRecherche (successeurs etat etatsVisites)))

;;; HEURISTIQUE ET EXPLOITATION

; calcul de l'heuristique = nombre de lettre en commun + nombre de successeur
(defun heuristique (etat etatRecherche etatsVisites)
  (let ((score 0))
    (if (is_solution etat etatRecherche etatsVisites)
      (incf score 1000)
      (progn (incf score (lettre_commun etat etatRecherche))
	     (incf score (nombre_successeurs etat etatsVisites))
	     )
      )
    )
  )

; choisi l'état le plus prometteur selon l'heuristique
(defun choixEtat (listeEtats etatRecherche etatsVisites)
  (let ((meilleur_choix nil)
	(maximum 0)
	(pos 1)
	(pos_max 0)
	)
    (dolist (etat listeEtats)
      (if (> (heuristique etat etatRecherche etatsVisites) maximum)
	(progn (setq maximum (heuristique etat etatRecherche etatsVisites))
	       (setq pos_max pos))
	)
      (incf pos 1)
      )
    (nelem listeEtats pos_max)
    )
 
  )
; classe les états selon leur pertinence d'après l'heuristique.
(defun classementEtat (listeEtats etatRecherche etatsVisites)
  (if (eq listeEtats nil)
    nil
    (append (classementEtat (remove (choixEtat listeEtats etatRecherche etatsVisites) listeEtats)
			    etatRecherche etatsVisites)
	    (list (choixEtat listeEtats etatRecherche etatsVisites))
	    )
    )
  )

;;; ALGORITHME DE RECHERCHE USANT DE L'HEURISTIQUE

(defun recherche_h (etatActuel etatRecherche etatsVisites)
  (let ((NetatsVisites (append etatsVisites (list etatActuel))))
    (cond ((equal etatActuel etatRecherche) NetatsVisites)
	  (T (let ((succs (classementEtat (successeurs etatActuel NetatsVisites) etatRecherche etatsVisites)))
	       (loop 
		 (when (not succs)
		   (return nil)
		   )
		 (let ((chemin (recherche_h (pop succs) etatRecherche NetatsVisites)))
		   (when chemin
		     (return chemin)
		     )
		   )
		 )
	       )
	     )
	  )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ALGORITHME DE RECHERCHE SELON UNE AUTRE HEURISTIQUE 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun heuristique_optimale (etatActuel etatRecherche etatsVisites)
  (let ((succs (successeurs etatActuel etatsVisites))
	(NetatsVisites (append etatsVisites (list etatActuel))))
    (if (equal etatActuel etatRecherche)
      1
      (if (eq succs nil)
	50
	(progn
	  (let ((heur_succs nil))
	    (dolist (succ succs)
	      (push (heuristique_optimale succ etatRecherche NetatsVisites) heur_succs)
	      )
	    (+ 1 (apply 'min heur_succs))
	    )
	  )
	)
      )
    )
  )

(defun choixEtat2 (listeEtats etatRecherche etatsVisites)
  (let ((meilleur_choix nil)
	(maximum 0)
	(pos 1)
	(pos_max 0)
	)
    (dolist (etat listeEtats)
      (if (> (heuristique_optimale etat etatRecherche etatsVisites) maximum)
	(progn (setq maximum (heuristique_optimale etat etatRecherche etatsVisites))
	       (setq pos_max pos))
	)
      (incf pos 1)
      )
    (nelem listeEtats pos_max)
    )
  )
; classe les états selon leur pertinence d'après l'heuristique.
(defun classementEtat2 (listeEtats etatRecherche etatsVisites)
  (if (eq listeEtats nil)
    nil
    (append (classementEtat2 (remove (choixEtat2 listeEtats etatRecherche etatsVisites) listeEtats)
			    etatRecherche etatsVisites)
	    (list (choixEtat2 listeEtats etatRecherche etatsVisites))
	    )
    )
  )

(defun recherche_h2 (etatActuel etatRecherche etatsVisites)
  (let ((NetatsVisites (append etatsVisites (list etatActuel))))
    (cond ((equal etatActuel etatRecherche) NetatsVisites)
	  (T (let ((succs (classementEtat2 (successeurs etatActuel etatsVisites) etatRecherche etatsVisites)))
	       (loop 
		 (when (not succs)
		   (return nil)
		   )
		 (let ((chemin (recherche_h2 (pop succs) etatRecherche NetatsVisites)))
		   (when chemin
		     (return chemin)
		     )
		   )
		 )
	       )
	     )
	  )
    )
  )
