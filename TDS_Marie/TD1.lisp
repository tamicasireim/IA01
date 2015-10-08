;exercice 1
(defun exercice 1 ()
  23 ;23 (un nombre s'évalue à lui même)
  (quote 23) ;23 (la fonction renvoie le symbole sans l'évaluer, c'est à dire 23)
  '23 ;23 (cette notation est strictement la même que (quote 23)
  (set x 32) ;renvoie une erreur : en effet, x n'a pas de valeur, son évaluation renvoie donc une erreur.
  (setq x 32) ; assigne la valeur 32 à X et renvoie la valeur assignée. (setq a b) est identique  à (set (quote a) b)
  (list x x 32) ;renvoie la liste composée arguments suivants. Comme x est évalué, l'expression renvoie (32 32 32)
  (cadr (list x x 32)); (cadr A) est équivalent à (car (cdr A)) soit le deuxième élément de A. Dans notre cas, l'expression renvoie 32.
  (setq x 'Y) ;assigne le symbole Y à X et renvoie Y.
  (setq xx 5) ;assigne 5 à XX et renvoie 5.
  (setq Y '(+ XX XX 32)) ;(+ xx xx 32) n'est pas évalué à cause de la quote. (+ xx xx 32) est donc assigné à Y et l'expression renvoie (+ xx xx 32)
  x ;renvoie Y
  (eval x) ;équivaut dans notre cas à l'expression Y, soit (+ xx xx 32)
  (eval y) ;équivaut dans notre cas à l'évalutation de la liste (+ xx xx 32) soit 42
  (cadr y) ;renvoie le deuxième élément de la liste Y soit xx.
  (eval (list '+ (eval y) (cadr y)))
  	;décomposons cette question :
	(list '+ (eval y) (cadr y)) ;renvoie (+ 42 xx) 
	(eval (+ 42 xx)) ;renvoie donc 47
  (setq z (+ (if x 2 0) (caddr y) (* y y))) ;renvoie une erreur. En effet, y est évalué à (+ xx xx 32), qui n'est pas un nombre.
  (setq y (list (setq z "Albert") x y)) ;renvoie ("Albert" 32 (+ xx xx 32))
  z ;z est évalué à Albert.
  y ; y est désormais évalué à ("Albert" 32 (+ xx xx 32)).
  (setq x (* x x)) ;renvoie 1024 soit x²
  x ; x est désormais évalué à 1024.
  )

;exercice 2
;l'arbre peut être défini des façons suivantes :

(setq arbre1 '(a b (c d e))) ;le premier élément d'une liste est la racine et les deux éléments suivants sont respectivement l'arbre fils gauche puis l'arbre fils droit.
(setq arbre2 '(a (b (c (d e))))) ;l'arbre est cette fois représenté sous la forme : (racine (filsgauche filsdroit))
;ou encore (en infixé cette fois ci)
(setq arbre3 '(b a (d c e))) ;où le deuxième élément est la racine et à droite et à gauche sont respectivement les fils drotis et gauches.
;en fait, les possibilités sont infinies. En effet, il ne s'agit que de représentation de connaissance. tant qu'on peut exploiter ces représentations, elles sont valables.

;((x + 2) / (x - 3)) en infixé correspond à (/ (+ x 2) (- x 3)) en préfixé.
;
;       /
;     /  \
;    +    -
;  /  \  / \
; x   2 x   3

;cette fonction prend en argument une expression infixée et la transforme en expression préfixée.
(defun infix2prefix (expression)
  (if (listp expression)
    (list (infix2prefix (cadr x)) (infix2prefix (car x)) (infix2prefix (last x))) ;si l'expression est une liste, on rappelle la fonction de façon récurssive en plaçant les éléments de la liste dans cet ordre : 2ème élément, 1er élément, dernier élément. AInsi (A + B) devient bient (+ A B). l'appel récursif sert au cas où A ou B est lui même une liste.
    x
    )
  )
