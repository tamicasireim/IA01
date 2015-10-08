(defun l ()
  (load "tp2.lisp"))

(setq notreliste '(A B C D))

(defun nelem (liste pos)
  (if (eq pos 1)
    (car liste)
    (nelem (cdr liste) (- pos 1))))

(defun echange (liste pos1 pos2)
  (let ((pos_tmp 0) (retour nil))
    (dolist (elem liste retour)
      (incf pos_tmp 1)
      (cond ((eq pos_tmp pos1) (setq retour (append retour (list (nelem liste pos2)))))
	    ((eq pos_tmp pos2) (setq retour (append retour (list (nelem liste pos1)))))
	    (T (setq retour (append retour (list elem))))))))

(defun allowed_state (liste)
  (if (eq nil (member 'A (member 'D liste)))
    T
    nil))

(defun successeurs (etat)
  (let ((retour nil))
    (if (allowed_state (echange etat 1 2))
      (setq retour (list (echange etat 1 2))))
    (if (allowed_state (echange etat 2 3))
      (setq retour (append retour (list (echange etat 2 3)))))
    (if (allowed_state (echange etat 2 4))
      (setq retour (append retour (list (echange etat 2 4))))
      retour)))

;fonction recherche (etatA etatR profondeur chemin)
;si etatA = etatR
;	retourner chemin
;sinon
;	si profondeur = 0
;		retourner NIL
;	sinon
;		pour chaque etat de successeurs(etatA)
;			nouveauchemin := append(chemin, n° état)
;			r := recherche(etat etatR (profondeur -1) nouveauchemin)
;			si r != NIL retourner r 
;		fin pour
;		pour chaque etat de successeurs(etatA)
;			nouveauchemin := append(chemin, n° état)
;			r := recherche(etat etatR profondeur nouveauchemin)
;			si r != NIL retourner r 
;		fin pour
;		
(defun size (liste)
  (if (eq nil liste)
    0
    (+ 1 (size (cdr liste)))))

(defun avant_dernier (liste)
  (if (> (size liste) 1)
    (nelem liste (- (size liste) 1))
    (car liste)))

(defun recherche (etatA etatR profondeur chemin)
  (cond ((equal etatA etatR) chemin)
	((eq profondeur 0) NIL)
	(T (dolist (etat (successeurs etatA) NIL)
	     (if (not (eq nil (member etat chemin)))
	       (let ((nouveauchemin (append chemin (list etat))))
	       ;(print nouveauchemin)
	       (let ((research (recherche etat etatR (- profondeur 1) nouveauchemin)))
		 (if (not (equal research NIL))
		   (return-from recherche research))))))
	   (dolist (etat (successeurs etatA) NIL)
	     (if (not (eq nil (member etat chemin)))
	       (let ((nouveauchemin (append chemin (list etat))))
	       (print nouveauchemin)
	       (let ((research (recherche etat etatR profondeur nouveauchemin)))
		 (if (not (equal research NIL))
		   (return-from recherche research))))))
	)))
