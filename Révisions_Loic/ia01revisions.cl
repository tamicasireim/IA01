;;TD1
(defun infpref (liste)
  (if (listp liste)
      (list (cadr liste)
            (infpref (car liste))
            (infpref (caddr liste))
            )
    liste
    )
  )

(infpref '((x + 2) / (x - 3)))


;;TD2
(defun derterm (term var)
  (if (equal var term)
      1
    0)
  )

(derterm '5 'x)

(defun deradd (term var)
  (if (listp term)
      (list '+
             (deradd (cadr term) var)
             (deradd (caddr term) var)
             )
    (derterm term var)
    )
  )

(deradd '(+ (+ x 2) (+ (+ X 2) 5)) 'x)

(defun dermul (term var)
  (if (listp term)
      (list '+
            (list '*
                  (dermul (cadr term) var)
                  (caddr term)
                  )
            (list '*
                  (cadr term)
                  (dermul (caddr term) var)
                  )
            )
    (derterm term var)
    )
  )

(dermul '(* x 5) 'x)


;;TD3
(setq *html 
      )

(defun html (page)
  (format t "<~A>" (car page))
  (if (listp (cadr page))
      (dolist (x (cdr page))
        (html x)
        )
    (format t "~A" (cadr page))
    )
  (format t "</~A>" (car page))
  )

(html '(html
               (header
                (title
                 "ma page"))
               (body
                (h1
                 "un titre")
                (p
                 "truc"))
        ))

;;TD4
(setq *laby '((Entree 1)
              (1 2)
              (2 1 7)
              (3 6)
              (4 5)
              (5 4 12)
              (6 3 7)
              (7 2 6 8)
              (8 7 9)
              (9 8 10)
              (10 9 11 15)
              (11 10 12 14)
              (12 5 11)
              (13 20)
              (14 11)
              (15 10 16)
              (16 15 17)
              (17 16 18)
              (18 17 19)
              (19 18 20)
              (20 13 Sortie)))

(defun successeurs (etat laby)
  (cdr (assoc etat laby))
  )

(defun succ_valide (etat laby chemin)
  (let ((s (successeurs etat laby))(svalide '()))
    (dolist (x s svalide)
      (if (not(member x chemin))
          (push x svalide)
        )
      )
    )
  )

(defun explore (etat laby chemin)
  (let ((trouve nil)(s (succ_valide etat laby chemin)))
    (push etat chemin)
    (if (equal etat 'sortie)
        (progn
          (format t "Trouvé~%")
          t
          )
      (while (AND (equal trouve nil) (not (null s)))
        (format t "Je vais en ~A.~%" (car s))
        (if (explore (car s) laby chemin)
            (progn
              (setq trouve T)
              (pop s)
              )
          (pop s)
          )
        )
      )
    trouve
    )
  )

(explore 'Entree '((Entree 1)
              (1 2)
              (2 1 7)
              (3 6)
              (4 5)
              (5 4 12)
              (6 3 7)
              (7 2 6 8)
              (8 7 9)
              (9 8 10)
              (10 9 11 15)
              (11 10 12 14)
              (12 5 11)
              (13 20)
              (14 11)
              (15 10 16)
              (16 15 17)
              (17 16 18)
              (18 17 19)
              (19 18 20)
              (20 13 Sortie)) '())