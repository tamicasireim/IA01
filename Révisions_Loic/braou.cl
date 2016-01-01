;;;;;;;;;BASE DE REGLES;;;;;;;;;
(setq baseregle
      '((1 ((actions sauver)) ((alignement bon)))
        (2 ((actions tuer)) ((alignement mauvais)))
        (3 ((actions autre)) ((alignement neutre)))
        (4 ((alignement bon) (base gotham)) ((affiliation batmanfamily)))
        (5 ((alignement bon) (base espace)) ((affiliation justiceleague)))
        (6 ((alignement mauvais)) ((affiliation injusticeleague)))
        (7 ((alignement neutre)) ((affiliation aucune)))
        (8 ((faiblesse kryptonite)) ((race kryptonien)))
        (9 ((origine krypton)) ((race kryptonien)))
        (10 ((origine surfaceterre)) ((race humain)))
        (11 ((origine atlantis)) ((race atlantean)))
        (12 ((origine thermiscira)) ((race amazone)))
        (13 ((affiliation justiceleague) (pouvoir aucun) (occupation pdg)) ((perso batman)))
        (14 ((affiliation justiceleague) (costume noir)) ((perso batman))) 
        (15 ((affiliation justiceleague) (pouvoir aucun) (occupation reporter)) ((perso greenarrow)))
        (16 ((affiliation justiceleague) (pouvoir aucun) (costume vert)) ((perso greenarrow)))
        (17 ((affiliation justiceleague) (arme arc)) ((perso greenarrow)))
        (18 ((affiliation justiceleague) (race humain) (pouvoir vitesse)) ((perso theflash)))
        (19 ((affiliation justiceleague) (pouvoir vitesse) (costume rouge)) ((perso theflash)))
        (20 ((affiliation justiceleague) (race humain) (pouvoir anneau)) ((perso greenlantern)))
        (21 ((affiliation justiceleague) (pouvoir anneau) (costume vert)) ((perso greenlantern)))
        (22 ((affiliation justiceleague) (race kryptonien) (genre homme)) ((perso superman)))
        (23 ((affiliation justiceleague) (genre homme) (costume bleu)) ((perso superman)))
        (24 ((affiliation justiceleague) (race kryptonien) (genre femme)) ((perso supergirl)))
        (25 ((affiliation justiceleague) (genre femme) (costume bleu)) ((perso supergirl)))
        (26 ((affiliation justiceleague) (race amazone)) ((perso wonderwoman)))
        (27 ((affiliation justiceleague) (genre femme)) ((perso wonderwoman)))
        (28 ((affiliation justiceleague) (arme lasso)) ((perso wonderwoman)))
        (29 ((affiliation justiceleague) (race humain) (faiblesse malediction) (genre homme)) ((perso hawkman)))
        (30 ((affiliation justiceleague) (arme massue) (genre homme)) ((perso hawkman)))
        (31 ((affiliation justiceleague) (race humain) (faiblesse malediction) (genre femme)) ((perso hawkgirl)))
        (32 ((affiliation justiceleague) (arme massue) (genre femme)) ((perso hawkgirl)))
        (33 ((affiliation justiceleague) (pouvoir controlemarin)) ((perso aquaman)))
        (34 ((affiliation justiceleague) (race atlantean)) ((perso aquaman)))
        (35 ((affiliation justiceleague) (arme trident)) ((perso aquaman)))
        (36 ((alignement neutre) (genre femme) (pouvoir aucun)) ((perso catwoman)))
        (37 ((alignement neutre) (occupation voleur)) ((perso catwoman)))
        (38 ((alignement neutre) (pouvoir plantes)) ((perso poisonivy)))
        (39 ((alignement neutre) (occupation scientifique)) ((perso poisonivy)))
        (40 ((alignement neutre) (occupation detective)) ((perso enigma)))
        (41 ((alignement neutre) (occupation mercenaire)) ((perso deathstroke)))
        (42 ((alignement mauvais) (genre homme) (faiblesse maladiementale)) ((perso joker)))
        (43 ((alignement mauvais) (genre femme) (faiblesse maladiementale)) ((perso harleyquinn)))
        (44 ((alignement mauvais) (occupation psychiatre)) ((perso harleyquinn)))
        (45 ((alignement mauvais) (pouvoir force)) ((perso bane)))
        (46 ((alignement mauvais) (pouvoir immortalite)) ((perso rasalghul)))
        (47 ((alignement mauvais) (occupation pdg)) ((perso lexluthor)))
        (48 ((alignement mauvais) (base metropolis)) ((perso lexluthor)))
        (49 ((alignement mauvais) (occupation trader)) ((perso penguin)))
        (50 ((alignement mauvais) (occupation scientifique) (identite revelee)) ((perso epouvantail)))
        (51 ((alignement mauvais) (pouvoir peur)) ((perso epouvantail)))
        (52 ((alignement bon) (occupation detective)) ((perso jamesgordon)))
        (53 ((affiliation batmanfamily) (genre femme)) ((perso batgirl)))
        (54 ((affiliation batmanfamily) (identite revelee)) ((perso alfredpennyworth)))
        (55 ((affiliation batmanfamily) (occupation acrobate)) ((perso nightwing)))
        (56 ((affiliation batmanfamily) (genre homme) (apparition non)) ((perso robin)))
        (57 ((affiliation batmanfamily) (occupation pdg)) ((perso batman)))))

;Listes servant à proposer des choix à l'utilisateur :
(setq typepossibles '(actions
                      affiliation
                      alignement
                      apparition
                      arme
                      costume
                      base
                      faiblesse
                      genre
                      identite
                      occupation
                      origine
                      pouvoir
                      race))
(setq actions '(tuer sauver autre))
(setq affiliation '(batmanfamily justiceleague injusticeleague aucune autre))
(setq alignement '(bon neutre mauvais))
(setq apparition '(oui non))
(setq arme '(arc lasso massue trident autre))
(setq base '(gotham espace metropolis autre))
(setq costume '(noir vert bleu rouge autre))
(setq faiblesse '(maladiementale malediction kryptonite autre))
(setq genre '(homme femme))
(setq identite '(revelee secrete))
(setq occupation '(scientifique trader reporter pdg detective voleur mercenaire acrobate autre))
(setq origine '(thermiscira surfaceterre krypton atlantis autre))
(setq pouvoir '(aucun anneau vitesse plantes peur controlemarin autre))
(setq race '(humain atlantean amazone kryptonien autre))
(setq persospossibles '(alfredpennyworth
                        aquaman
                        bane
                        batgirl
                        batman
                        catwoman
                        enigma
                        epouvantail
                        jamesgordon
                        deathstroke
                        greenarrow
                        greenlantern
                        harleyquinn
                        hawkgirl
                        hawkman
                        joker
                        lexluthor
                        nightwing
                        penguin
                        poisonivy
                        rasalghul
                        robin
                        supergirl
                        superman
                        theflash
                        wonderwoman))

						

						
;;;;;;;;;CHAINAGE AVANT;;;;;;;;;


(setq reglesappliquees nil) ;liste des règles déjà appliquées

(defun premisses (regle) ;retourne les prémisses d'une règle
  (cadr regle))
  
(defun conclusion (regle) ;retourne les conclusions d'une règle
  (caddr regle))

(defun reglevalide (regle basef) ;retourne T si la regle est valide selon la basede faits
  (let ((valide T))
    (dolist (x (premisses regle) valide) ;on parcourt les prémisses de la règle
      (if (not (member x basef :test #'equal)) ;si une prémisse n'appartient pas à la base de faits envoyée en paramètre on retourne NIL
          (setq valide nil)))))

(defun reglesajoutables (basef) ;renvoie la liste des regles applicables selon si elles sont valides et qu'elles n'ont pas encore été appliquées
  (let ((listeR nil))
    (dolist (regle baseregle listeR) ;on parcourt la base de règles
      (if (AND (reglevalide regle basef) (not (member regle reglesappliquees))) ;si la règle est valide selon la base de faits courante et qu'elle n'appartient pas aux règles appliquées, on l'ajoute à la liste retournée
          (push regle listeR)))))

(defun appliquerregles (base modif) ;ajoute les regles ajoutables aux regles appliquées et met à jour la base de faits
  (let ((listeR (reglesajoutables base)) (nouvellebase '()))
    (setq nouvellebase base) ;on initialise la nouvelle base de faits avec la base de faits courante
    (if (not (null listeR)) ;on vérifie qu'il reste des règles à appliquer
        (progn
          (dolist (regle listeR) ;on parcourt la liste des règles à appliquer
            (push regle reglesappliquees) ;on ajoute chaque règle à la liste des règles déjà appliquées
            (dolist (fait (conclusion regle)) ;on parcourt les conclusions de chaque règle
              (if (equal (car fait) 'perso) ;si la conclusion est un personnage on l'ajoute directement à la nouvelle base de faits
                  (push fait nouvellebase)
                (if (assoc (car fait) nouvellebase) ;vérifie si un fait du même type que la conclusion est déjà dans la base
                    (if (equal modif "y") ;si l'utilisateur a accepté que les règles modifient les faits déjà enregistrés :
                        (progn
                          (setf (cadr (assoc (car fait) nouvellebase)) (cadr fait)) ;on modifie le fait déjà présent
                          (setq nouvellebase (appliquerregles nouvellebase modif)) ;appel récursif pour corriger les erreurs faites avec les règles précédentes
                          )
                      nil ;si l'utilisateur n'a pas accepté l'ancien fait reste tel quel
                      )
                  (push fait nouvellebase) ;si le type du nouveau fait n'était pas présent dans la base on l'ajoute directement
                  ))))
          (setq nouvellebase (appliquerregles nouvellebase modif))
          )
      base ;s'il n'y a pas de règles à appliquer on renvoie la base courante
      )
    nouvellebase ;on retourne la nouvelle base de faits
    ))

(defun renvoyerpersos (base) ;renvoie la liste des personnages présents dans la base de faits
  (let ((listepersos nil))
    (dolist (x base listepersos) ;on parcourt la base donnée en paramètre
      (if (AND (equal (car x) 'perso) (not (member x listepersos :test #'equal))) ;si un fait est du type personnage est qu'il n'est pas déjà présent dans la liste on l'ajoute
          (push x listepersos)))))

(defun chainageavant () ;fonction principale du chainage avant
  (setq reglesappliquees nil) ;on initialise la liste des règles appliquées à nil
  (let ((modif "y"))
    (format t "~%S'il y a conflit(s) entre une règle et un fait de la base,~%modifier le fait (informations potentiellement erronées) ? y/n : ")
    (setq modif (read-line)) ;on enregistre le choix de l'utilisateur par rapport à la modification des faits par les règles
    (let ((nouvellebase (appliquerregles basefaits modif))) ;on obtient la nouvelle base de faits
      (let ((listeP (renvoyerpersos nouvellebase)) (ajout "y")) ;on obtient la liste des personnages trouvés
        (if (not (null listeP))
            (if (= (length listeP) 1) ;cas où 1 personnage a été trouvé
                (progn
                  (format t "~%~A est le personnage qui correspond aux caractéristiques." (cadr (car listeP)))
                  T
                  )
              (progn
                (if (AND (> (length listeP) 1) (equal ajout "y")) ;cas où plusieurs personnages ont été trouvés
                    (progn
                      (format t "~%~A personnages correspondent aux caractéristiques données.~%Ajouter une nouvelle caractéristique ? y/n : " (length listeP))
                      (setq ajout (read-line)) ;on enregistre le choix de l'utilisateur par rapport à l'ajout d'une nouvelle caractéristique pouvant départager les personnages trouvés
                      (if (equal ajout "y") ;si l'utilisateur accepte on appelle la fonction d'ajout de faits et la fonction de chaînage avant
                          (progn
                            (ajoutfait)
                            (chainageavant)
                            )
                        (dolist (perso listeP T) ;si l'utilisateur n'a pas accepté on retourne les noms des personnages trouvés
                          (format t "~%~A est un personnage qui correspond aux caractéristiques." (cadr perso))
                          ))))))
          (progn ;cas où aucun personnage n'a été trouvé, on propose à l'utilisateur d'ajouter des faits
            (format t "~%Il n'y a aucun personnage correspondant aux caractéristiques données.~%Ajouter une nouvelle caractéristique ? y/n : ")
            (setq ajout (read-line))
            (if (equal ajout "y")
                (progn
                  (ajoutfait)
                  (chainageavant)
                  ))))))))
				

				

;;;;;;;;;CHAINAGE ARRIERE;;;;;;;;;


(defun prem_regle (regle) ;retourne les prémisses d'une règle
  (cadr regle))

(defun conc_regle (regle) ;retourne les conclusions d'une règle
  (last regle))

(defun regles_cand (but) ;retourne les règles dont les conclusions contiennent le fait "but"
  (let (basereg '())
    (dolist (x baseregle basereg) ;on parcourt la base de règles
      (dolist (conc (car (conc_regle x))) ;on parcourt les conclusions de chaque règle
        (if (equal but conc) ;si une conclusion correspond au but on ajoute la règle à la liste retournée
            (push x basereg))))))

(defun verif_ou (but) ;vérifie si un fait est appliqué
  (let ((RC '())(bverifet nil))
    (progn
      (if (member but basefaits :test #'equal) ;si le fait "but" appartient à la base de faits on retourne T
          T
        (progn ;sinon
          (setq RC (regles_cand but))
          (while (AND (not (null RC)) (equal bverifet nil)) ;tant qu'on n'a pas trouvé de règle confirmant le but on parcourt la liste des règles candidates
            (if (verif_et (car RC)) ;si verif_et confirme une règle
                (progn
                  (setq bverifet T) ;on retournera T et on affiche comment la règle a confirmé le but
                  (format t "La regle ~A valide : ~%" (caar RC))
                  (dolist (conc (car(conc_regle (car RC))))
                    (format t "      ~A=~A~%" (car conc) (cadr conc))
                    )
                  (format t "   avec les prémisses : ~%")
                  (dolist (prem (prem_regle (car RC)))
                    (format t "      ~A=~A~%" (car prem) (cadr prem))
                    )
                  (format t "~%")
                  )
              (pop RC) ;si verif_et ne confirme pas la règle on passe à la règle suivante
              ))
          bverifet
          )))))
		  
(defun verif_et (r) ;vérifie si une règle est confirmée
  (let ((prem '()) (regle_verifiee t))
    (progn
      (setq prem (prem_regle r))
      (while (AND (not (null (car prem))) (equal regle_verifiee (not nil))) ;on parcourt les prémisses de la règle "r" tant qu'elle n'a pas été infirmée
        (if (not (verif_ou (car prem))) ;si vérif_ou ne confirme pas l'une des prémisses on arrête les boucles et on retournera NIL
            (setq regle_verifiee nil)
          (pop prem)
          ))
      regle_verifiee
      )))

(defun chainagearriere () ;fait le lien entre l'utilisateur et les autres fonctions du chaînage arrière
  (let ((nom nil))
    (format t "~%Donner le nom du personnage à vérifier : ")
    ;ON AFFICHE LES PERSOS POSSIBLES
    (dotimes (x (length persospossibles))
      (format t "~%~A : ~A" x (nth x persospossibles)))
    (format t "~%")
    (setq nom (nth (parse-integer (read-line)) persospossibles)) ;on prend le personnage choisi par l'utilisateur
    (if (verif_ou (list 'perso nom)) ;en fontion de ce que retourne verif_ou avec le nom du personnage choisi, on affiche s'il a été confirmé ou pas par la base de faits
        (progn
          (format t "~%Les caractéristiques désignent bien ~A.~%" nom)
          T
          )
      (format t "~%Les caractéristiques ne désignent pas ~A.~%" nom)
	  )))




;;;;;;;;;ACTIONS;;;;;;;;;


(defun afficherbase () ;affiche la base de faits
  (if (null basefaits)
      (format t "~%La base de faits est vide.")
    (dolist (fait basefaits T) ;si la base n'est pas vide on affiche chaque fait avec le type et la valeur
      (format t "~%~A : ~A" (car fait) (cadr fait)))))

(defun ajoutfait () ;ajoute ou modifie un fait dans la base de faits
  (let ((typef nil) (valeurf nil) (modif "n"))
    (format t "~%Entrez le type du nouveau fait : ")
    ;ON AFFICHE LES TYPES POSSIBLES
    (dotimes (x (length typepossibles))
      (format t "~%~A : ~A" x (nth x typepossibles))
      )
    (format t "~%")
    (setq typef (nth (parse-integer (read-line)) typepossibles))
    (format t "~%Entrez la valeur du nouveau fait : ")
    ;ON AFFICHE LES VALEURS POSSIBLES POUR LE TYPE DEMANDE
    (dotimes (x (length (eval typef)))
      (format t "~%~A : ~A" x (nth x (eval typef)))
      )
    (format t "~%")
    (setq valeurf (nth (parse-integer (read-line)) (eval typef)))
    (if (assoc typef basefaits) ;on vérifie si le type du fait à ajouter est déjà présent dans la base
        (progn
          (format t "~%Le fait appartient déjà à la base.~%Voulez-vous le modifier ? y/n : ")
          (setq modif (read-line))
          (if (equal modif "y") ;si un fait du même type est déjà présent et que l'utilisateur accepte, on modifie le fait avec la nouvelle valeur choisie par l'utilisateur
              (progn
                (setf (cadr (assoc typef basefaits)) valeurf)
                (format t "~%Le fait a été modifié dans la base.~%")
                )))
      (progn
        (push (list typef valeurf) basefaits) ;si le type du fait n'est pas déjà dans la base on y ajoute le nouveau fait
        (format t "~%Le nouveau fait a été ajouté à la base.~%")
        ))))

  
  
  
;;;;;;;;;MENU;;;;;;;;;


(defun action () ;propose un menu d'actions à l'utilisateur
  (let ((choix "0"))
    (while (not (equal choix "7"))
        (format t "~%~%Choisissez l'action à effectuer :")
        (format t "~%    1-Pour réinitialiser la base de faits.")
        (format t "~%    2-Pour ajouter/modifier un fait dans la base.")
        (format t "~%    3-Pour afficher la base de faits.")
        (format t "~%    4-Pour lancer le chaînage en avant.")
        (format t "~%    5-Pour lancer le chaînage en arrière.")
        (format t "~%    7-Pour quitter.~%")
        (setq choix (read-line))
        (cond
         ((equal choix "1") (setq basefaits '()))
         ((equal choix "2") (ajoutfait))
         ((equal choix "3") (afficherbase))
         ((equal choix "4") (chainageavant))
         ((equal choix "5") (chainagearriere))
         ))))




;;;;;;;;;SCENARIOS D'UTILISATION;;;;;;;;;

;;Premier cas
;(setq basefaits '((base espace) (genre homme) (origine krypton) (actions sauver)))
;;(action) ;puis sélectionner 4
;;ou
;(chainageavant)

;;Second cas
;(setq basefaits '((base gotham) (genre homme) (occupation acrobate) (actions sauver) (apparition non)))
;;(action) ;puis sélectionner 4
;;ou
;(chainageavant)

;;Troisieme cas
;(setq basefaits '((base gotham) (genre femme) (actions sauver)))
;;(action) ;puis sélectionner 5 puis 3
;;ou
;(chainagearriere) ;puis sélectionner 3
