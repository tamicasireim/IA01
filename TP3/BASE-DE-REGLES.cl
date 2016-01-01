;BASE DE REGLES

;IDEAS
	;utiliser seulement la base des STRENGTHS mais créer une fonction inverse appelée find_weaknesses

;TYPES STRENGTHS

(type normal)	;no strength
(type fight)	((strength normal)(strength rock)(strength steel)(strength ice)(strength dark))
(type flying)	((strength fight)(strength bug)(strength grass))
(type poison)	((strength grass))
(type ground)	((strength poison)(strength rock)(strength steel)(strength fire)(strength electric))
(type rock)		((strength flying)(strength bug)(strength fire)(strength ice))
(type bug)		((strength grass)(strength psychic)(strength dark))
(type ghost)	((strength ghost)(strength psychic))
(type steel)	((strength rock)(strength ice))
(type fire)		((strength bug)(strength steel)(strength grass)(strength ice))
(type water)	((strength ground)(strength rock)(strength fire))
(type grass)	((strength ground)(strength rock)(strength water))
(type electric)	((strength flying)(strength water))
(type psychic)	((strength fight)(strength poison))
(type ice)		((strength flying)(strength ground)(strength grass)(strength dragon))
(type dragon)	((strength dragon))
(type dark)		((strength ghost)(strength psychic))


;TYPE WEAKNESSES

(type normal)	((weakness fight))
(type fight)	((weakness flying)(weakness psychic))
(type flying)	((weakness rock)(weakness electric)(weakness ice))
(type poison)	((weakness ground)(weakness psychotic))
(type ground)	((weakness water)(weakness grass)(weakness ice))
(type rock)		((weakness fight)(weakness ground)(weakness steel)(weakness water)(weakness grass))
(type bug)		((weakness flying)(weakness rock)(weakness fire))
(type ghost)	((weakness ghost)(weakness dark))
(type steel)	((weakness fight)(weakness ground)(weakness fire))
(type fire)		((weakness  ground)(weakness rock)(weakness water))
(type water)	((weakness grass)(weakness electric))
(type grass)	((weakness  flying)(weakness poison)(weakness bug)(weakness fire)(weakness ice))
(type electric)	((weakness ground))
(type psychic)	((weakness bug)(weakness ghost)(weakness dark))
(type ice)		((weakness fight)(weakness rock)(weakness steel)(weakness fire))
(type dragon)	((weakness ice)(weakness dragon))
(type dark)		((weakness fight)(weakness bug))


;TYPES IMMUNITY

(type normal)	(immunity ghost)
(type fight)	(immunity ghost)
(type poison)	(immunity steel)
(type ground)	(immunity flying)
(type ghost)	(immunity normal)
(type electric)	(immunity ground)
(type psychic)	(immunity dark)


;BDD POKEMONS ET STATISTIQUES DE BASE)
(à récupérer à Compy #BOLOSSE)


;DECISIONS LOGIQUES
	;A NOTER :	HP  de 1 à 255
	;			ATK de 5 à 190
	;			DEF de 5 à 230
	;			VIT de 5 à 180
(HP > 50)(ATK > 65)
(HP > 100)(ATK > 110)
(HP > 150)(ATK > 150)
;idem DEF, VIT, ATK etc


