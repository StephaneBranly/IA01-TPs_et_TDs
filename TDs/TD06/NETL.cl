; Introduction
; Diff�rence entre paire point�e et liste classique :
; P (A . B)    <=>    [A|B]
; L (A B)      <=>    [A|-]-->[B|NIL]
; Une m�me liste peut ainsi �tre repr�sent�e de plusieurs mani�res : (a b c), (a .(b c), (a . b . c)

; Symbole :
;  Un symbole correspond � un Nom, une �ventuelle valeur et une �ventuelle fonction (nom, valeur?, fonction?)

; Faire attention avec les (setq a v) qui font pointer a sur v.
; Pour �viter le pointage sur le m�me �l�ment, on peut utilise (copy-list ...)

; Diff�rence entre apply et funcall. 
; apply "�clate" la liste des param�tres en param�tres lors de l'appel de la fonction
; funcall appel la fonction avec les param�tres la suivant




;;; Reseaux semantiques rappel ;;;;
; Individu : cercle vide o
; Concept  : cercle vide o---* cercle plein (ensemble
; (clyde)o--is-s--->*-o (elephant)
; n�gation, split, mapnode (voir cours)


; Repr�sentation sous forme de a-list d'un noeud et d'une relation
; (setq noeud '(nom type))
; NB : possibilit� pour un noeud, d'indiquer �galement les arcs-in et arcs-out pour avoir acc�s
; aux arcs entrants et sortants d'un noeud
; (setq n3 '((nom cyrano)(type individu)))
; (setq n2 '((nom cadetDG)(type concept)))
; (setq a1 '((type is-a)(from n3)(to n2)))

(setq *nodes* '())
(setq *arcs* '())
; fonctions defnode et defarc
(defun defnode (nom type)
  (let ((id (gentemp "N"))) ; cr�er un identifiant
    (set id (list (list 'type type)(list 'nom nom))) ; lui associer la a-list (nom type)
    (push id *nodes*) ; mettre � jour la liste *nodes*
    id ; retourner l'identifiant
    )
  )

(defun pushProperty (node property content)
  (if (not (assoc property (symbol-value node))) ;; cr�e la propri�t�  property dans node si elle n'existe pas
    (nconc (symbol-value node) (list (list property)))
    )
  (nconc (assoc property (symbol-value node)) (list content)) ;; push content dans la propri�t� property
  )

(defun defarc (type from to)
  (if (and (member from *nodes*) (member to *nodes*)) ; v�rifier l'existence des noeuds (from et to)
      (let ((id (gentemp "A"))) ; cr��r l'ID
        (set id (list (list 'type type)(list 'from from)(list 'to to)))  ; associer � id la a-list (type from to)
        (pushProperty from 'arcs-out id) ; modification du node from pour ajouter l'arc-out
        (pushProperty to 'arcs-in id) ; modification du node to pour ajouter l'arc-in
        (push id *arcs*) ; mettre � jour la liste *arcs*
        id ; retourner l'ID
    )
    NIL
    )
  )
; (gentemp 'N) permet de g�n�rer des cl�s uniques avec auto-incr�mentation