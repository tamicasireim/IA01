;TD 3

(setq f "td3.lisp")

;variables pour tester les fonctions
(setq page '(html (header
		    (title "ma page")
		    )
		  (body
		    (h1 "un titre")
		    (p "soro et aemula Romae")
		    )
		  )
	)
(setq spage '(html (header "braou")))

;fait la conversion entre notre structure de donnée et le html.
;amélioration possible : étendre le fonctionne aux balises singleton telles que <br />
(defun make-html-print (page)
  (if (and (listp page) (not (eq page nil))) ;si c'est une liste, il faut créer une balise
    (progn
      (princ (concatenate 'string "<" (string (car page)) ">")) ;ouvre la balise
      (dolist (elem (cdr page))
	(make-html-print elem) ;pour chaque élément de la balise, on l'interprête de façon récursive.
	)
      (princ (concatenate 'string "</" (string (car page)) ">"));fermeture de la balise
      )
    (princ page)
    T;la fonction renvoie true.
    )
  )

;même algorithme que la fonction différente, mais make-html-return retourne une unique chaine de caractère contenant le html.
(defun make-html-return (page)
  (let ((retour ""))
    (if (and (listp page) (not (eq page nil)))
      (progn
	(setq retour (concatenate 'string "<" (string (car page)) ">"))
	(dolist (elem (cdr page))
	  (setq retour (concatenate 'string retour (make-html-return elem)))
	  )
	(concatenate 'string retour "</" (string (car page)) ">"))
      (string page))))
;crée un fichier html depuis une liste passée en argument.
(defun make-html-file (page filename)
  (let ((tmp (make-html-return page)))
    (with-open-file (file filename :direction :output :if-does-not-exist :create :if-exists :overwrite)
      (print tmp file))))
