; Introduction
; Différence entre paire pointée et liste classique :
; P (A . B)    <=>    [A|B]
; L (A B)      <=>    [A|-]-->[B|NIL]
; Une même liste peut ainsi être représentée de plusieurs manières : (a b c), (a .(b c), (a . b . c)

; Symbole :
;  Un symbole correspond à un Nom, une éventuelle valeur et une éventuelle fonction (nom, valeur?, fonction?)

; Faire attention avec les (setq a v) qui font pointer a sur v.
; Pour éviter le pointage sur le même élément, on peut utilise (copy-list ...)

; Différence entre apply et funcall. 
; apply "éclate" la liste des paramètres en paramètres lors de l'appel de la fonction
; funcall appel la fonction avec les paramètres la suivant




;;; Reseaux semantiques rappel ;;;;
; Individu : cercle vide o
; Concept  : cercle vide o---* cercle plein (ensemble
; (clyde)o--is-s--->*-o (elephant)
; négation, split, mapnode (voir cours)


; Représentation sous forme de a-list d'un noeud et d'une relation
; (setq noeud '(nom type))
; NB : possibilité pour un noeud, d'indiquer également les arcs-in et arcs-out pour avoir accès
; aux arcs entrants et sortants d'un noeud
; (setq n3 '((nom cyrano)(type individu)))
; (setq n2 '((nom cadetDG)(type concept)))
; (setq a1 '((type is-a)(from n3)(to n2)))

(setq *nodes* '())
(setq *arcs* '())
; fonctions defnode et defarc
(defun defnode (nom type)
  (let ((id (gentemp "N"))) ; créer un identifiant
    (set id (list (list 'type type)(list 'nom nom))) ; lui associer la a-list (nom type)
    (push id *nodes*) ; mettre à jour la liste *nodes*
    id ; retourner l'identifiant
    )
  )

(defun pushProperty (node property content)
  (if (not (assoc property (symbol-value node))) ;; crée la propriété  property dans node si elle n'existe pas
    (nconc (symbol-value node) (list (list property)))
    )
  (nconc (assoc property (symbol-value node)) (list content)) ;; push content dans la propriété property
  )

(defun defarc (type from to)
  (if (and (member from *nodes*) (member to *nodes*)) ; vérifier l'existence des noeuds (from et to)
      (let ((id (gentemp "A"))) ; créér l'ID
        (set id (list (list 'type type)(list 'from from)(list 'to to)))  ; associer à id la a-list (type from to)
        (pushProperty from 'arcs-out id) ; modification du node from pour ajouter l'arc-out
        (pushProperty to 'arcs-in id) ; modification du node to pour ajouter l'arc-in
        (push id *arcs*) ; mettre à jour la liste *arcs*
        id ; retourner l'ID
    )
    NIL
    )
  )
; (gentemp 'N) permet de générer des clés uniques avec auto-incrémentation