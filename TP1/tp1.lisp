;;; Déclaration d'une liste d'association et d'une base de donnée d'ouvrages pour les exercices 3 et 5

(setq notre_liste '((Babouin Singe) (Lion RoiDeLaSavane) (Poireau Légume) (Marie Simatic) (Agathe Pingouin)))

(setq notre_base '((" Le Dernier Jour d'un condamné " Hugo 1829 50000)
		   (" Notre-Dame de Paris " Hugo 1831 3000000)
		   (" Les Misérables " Hugo 1862 2000000)
		   ("Le Horla " Maupassant 1887 2000000)
		   (" Contes de la bécasse " Maupassant 1883 500000)
		   ("Germinal " Zola 1885 3000000)
		   ))

;;;;;;;;;;;;;;;;; EXERCICE 1 

;retourne une liste composée des N premiers éléments
(defun firstn (N L)
  ; La récurrence s'arrête si on arrive au bout de la liste ou si N = 0
  (if (or (= N 0) (EQ NIL L))
    NIL
    (append (list (first L)) 
	    (firstn (- N 1) (cdr L))
	    )
    )
  )

;supprime la première occurence de N de la liste L
(defun suppr (N L)
  ;la récurrence s'arrête à la première rencontre de N, où elle est supprimée de la liste retournée.
  (if (or (EQ NIL L) (= (car L) N))
    (cdr L)
    (append (list (car L))
	    (suppr N (cdr L))
	    )
    )
  )
;version itérative de la fonction suppr
(defun supprIter (N L)
  (progn
    (setq Result NIL)
    (setq break_cond NIL)
    ;On parcoure toute la liste et on décide ou non pour chaque élément si on le rajoute ou non à la liste résultat. La boucle se termine quand on atteint la fin de la liste L.
    (loop (if (EQ NIL L)
	    (return-from NIL Result)
	    (progn (if (and (EQ break_cond NIL) (= (car L) N) )
		     (setq break_cond T) ;Cet élément ne sera pas rajouté. Tous les autres éléments seront acceptés dans la liste retour.
		     (setq Result (append Result (list (car L))) ; Ajout de l'élément à la liste retour
			   )
		     )
		   (setq L (cdr L)) ; On passe à l'analyse de l'élément suivant.
		   )
	    )
	  )
    )
  )


;supprime toutes les occurences de N dans la liste L
(defun supprAll (N L)
  ; A chaque rencontre de N, on le supprime de la liste retournée, jusqu'à ce qu'on arrive au bout de la liste
  (if (EQ NIL L)
    NIL
    (if (= (car L) N)
      (supprAll N (cdr L))
      (append (list (car L))
	      (supprAll N (cdr L))
	      )
      )
    )
  )
;Version itérative
(defun supprAllIter (N L)
  (progn
    (setq Result NIL)
    (dolist (x L Result)
      (if (= x N)
	nil
	(setq Result (append Result (list x)))
	)
      )
    )
  )

;retourne l'intersection des deux listes L et M
(defun inter (L M)
  (cond ((or (EQ NIL L) (EQ NIL M))
		 NIL
	 ) ;la récurrence s'arrête quand la liste L est vide
	((member (car L) M)
	 ; Si l'élément de L existe dans M alors on le rajoute dans la liste de retour.
	 ; On appelle ensuite de nouveau inter sur le reste de la liste L et sur la liste M, privée du premier élément de L
	 (append (list (car L))
		 (inter (cdr L) (suppr (car L) M))
		 )
	 )
	(T
	  (inter (cdr L) (suppr (car L) M) )
	  )
	)
  )

;supprime toutes les répétitions d'une liste
(defun elim (L)
  (if (EQ NIL L)
    NIL
    (append (list (car L)) ; la liste retournée est le premier élément de la liste suivi du reste de la liste, dont où a supprimé toutes les occurences du premier élément.
	    (elim (supprAll (car L) (cdr L)))
	    )
    )
  )

; compte le nombre de feuilles d'un arbre
(defun nbFeuilles (L)
  (if (EQ NIL L)
    0 ; Une liste vide a 0 feuilles
    (if (listp L)
      (+ (nbFeuilles (car L)) (nbFeuilles (cdr L))) ; le nombre de feuilles d'un arbre, c'est la somme des feuilles de sa première branche + le nombre de feuilles du reste des branches. Ainsi, la fonction "descent" d'un niveau dans l'arbre à chaque appel récursif de la fonction.
      1 ; Si ce n'est pas une liste, c'est donc une feuille, la fonction renvoie, par définition, 1.
      )
    )
  )

; retourne t ou nil selon l'égalité de ses deux arguments
(defun monEqual (A B)
  ;Si les arguments sont des atomes, on utilise la fonction standard eq, sinon, on fait appel à eqliste, prévue pour comparer deux listes
  (if (and (listp A) (listp B))
    (eqListe A B)
    (eq A B)
    )
  )

;retourne t ou nil selon l'égalité des deux listes
(defun eqListe (A B)
  ; deux listes sont égales si tous ses éléments sont égaux deux à deux. Cette fonction va donc tester l'égalité de chacuns des éléments des listes passées en arguments, que ces éléments soient eux même des listes ou non.
  (cond
    ((and (EQ NIL A) (EQ NIL B)) ;deux listes vides sont égales. Cette condition arrête également la récurrence.
     T
     )
    ((and (listp (car A)) (listp (car B))) ; Si les deux premiers éléments sont des listes, on doit réutiliser eqliste sur ces derniers pour déterminer leur égalité, et appeler récursivement eqliste sur le reste des deux listes.
     (and (eqliste (car A) (car B)) (eqliste (cdr A) (cdr B)))
     )
    ((eq (car A) (car B)) ;Si au moins l'un des éléments est un atome, on peu utiliser la fonction standard eq pour déterminer l'égalité de ces derniers
     (eqliste (cdr A) (cdr B))
     )
    (T ;si aucune de ces conditions n'est vérifiée, les listes ne sont pas égales.
      NIL)
    )
  )

;;;;;;;;;;;;;;;;; EXERCICE 2

; retourne la liste des éléments par paire de deux listes fournies en paramètre
(defun list-paire (A B)
  (if (and (listp A) (listp B))
    (mapcar (lambda (x y) ;mapcar va appeler la fonction anonyme sur le premier élément de A et B, puis le deuxième etc.
	      (list x y)
	      )
	    A B)
    NIL
    )
  )

;;;;;;;;;;;;;;;; EXERCICE 3

(defun my-assoc (cle a-liste)
  (let ((retour nil)) 
    (dolist (association a-liste retour);Si la clé n'est pas trouvée ou si a-liste est vide (on n'entrera alors pas dans la condition dolist), retour vaudra nil. Sinon, retour aura pour valeur l'association associée à la clé.
      (if (eq cle (car association))
		(setq retour association)
		nil
		)
      )
    )
  )

;;;;;;;;;;;;;;; EXERCICE 4

;ces fonctions sont triviales : elles n'ont pour objectif que faciliter la lecture des fonctions d'après. Elles renvoient le nième élément d'une liste de format "ouvrage" (c'est à dire ("titre" auteur annee nombre_d_ouvrages_vendus))
(defun auteur (ouvrage)
  (cadr ouvrage)
  )
(defun titre (ouvrage)
  (car ouvrage)
  )
(defun annee (ouvrage)
  (caddr ouvrage)
  )
(defun nombre (ouvrage)
  (cadddr ouvrage)
  )

; affiche tous les ouvrages
(defun FB1 (BDD)
  (dolist (ouvrage BDD T);Pour chaque ouvrage de BDD, on affiche ses caractéristiques. On notera que la valeur de retour de FB1 ne nous importe pas, elle renvoie donc true, de façon arbitraire.
    (print (list (auteur ouvrage)
		 (titre ouvrage)
		 (annee ouvrage)
		 (nombre ouvrage)
		 )
	   )
    )
  )
;affiche tous les ouvrages dont l'auteur est Hugo
(defun FB2 (BDD)
  (dolist (ouvrage BDD T);pour chaque ouvrage de BDD
    (if (eq (auteur ouvrage) 'Hugo);Si l'auteur est Hugo, on affiche ses caractérisques.
      (print (list (auteur ouvrage)
		   (titre ouvrage)
		   (annee ouvrage)
		   (nombre ouvrage)
		   )
	     )
      )
    )
  )

;retourne la liste des titres des ouvrages dont l'auteur est spécifié en argument
(defun FB3 (BDD nom_auteur)
  (if (not (eq BDD NIL)) ;la récurrence s'arrête quand la liste est vide.
    (if (eq (auteur (car BDD)) nom_auteur) ;Si l'ouvrage a été écrit par l'auteur, on l'ajoute son titre à la liste de retour.
      (append (list (titre (car BDD))) (FB3 (cdr BDD) nom_auteur))
      (FB3 (cdr BDD) nom_auteur)
      )
    NIL
    )
  )
;cette fonction a un fonctionnement à peu près analogue à celui de FB3, à la seule différence qu'il renvoie la liste des ouvrages écrits par un auteur, et non seulement les titre. Cette fonction est utile pour la fonction FB6.
(defun FB3bis (BDD nom_auteur)
  (if (not (eq BDD NIL))
    (if (eq (auteur (car BDD)) nom_auteur)
      (append (list (car BDD)) (FB3bis (cdr BDD) nom_auteur))
      (FB3bis (cdr BDD) nom_auteur)
      )
    NIL
    )
  )


; retourne le premier ouvrage paru en année X ou nil
(defun FB4 (BDD annee)
  (if (not (eq BDD NIL));la récurrence s'arrête si on a fini de chercher dans la BDD...
    (if (eq (annee (car BDD)) annee)
      (car BDD);ou si la recherche a porté ses fruits.
      (FB4 (cdr BDD) annee)
      )
    NIL
    )
  )

;retourne la liste des ouvrages dont le nombre d'exemplaires vendus dépasse 1 000 000 ou nil
;l'algorithme est le même que FB3 (et plus particulièrement celui de la fonction FB3bis), au seul changement de la condition.
(defun FB5 (BDD)
  (if (not (eq BDD NIL))
    (if (> (nombre (car BDD)) 1000000)
      (append (list (car BDD)) (FB5 (cdr BDD)))
      (FB5 (cdr BDD))
      )
    NIL
    )
  )

; calcule et retourne la moyene du nombre d'exemplaires vendus de l'auteur passé en argument
(defun FB6 (BDD auteur)
  (let ((liste_auteur (FB3bis BDD auteur));on initialise des variables locales : la liste des ouvrages écrits par l'auteur via la fonction FB3bis, le nombre d'ouvrage et le nombre d'exemplaires vendus, dont nous aurons besoin pour calculer la moyenne.
					      (nombre_ouvrage '0)
					      (exemplaires_vendus '0)
					      )
    (dolist (ouvrage liste_auteur T);pour chaque ouvrage écrit par l'auteur, le nombre d'ouvrage est incrémenté de 1 et le nombre d'exemplaires vendus est mis à jour
      (incf nombre_ouvrage 1);identique à (setq nombre_ouvrage (+ nombre_ouvrage 1)) mais plus propre.
      (incf exemplaires_vendus (nombre ouvrage))
      )
    (if (not (= nombre_ouvrage '0));s'il y a au moins un ouvrage écrit par l'auteur, on calcule la moyenne. (on aurait sinon une erreur de division par 0)
      (/ exemplaires_vendus nombre_ouvrage)
      nil
      )
    )
  )
