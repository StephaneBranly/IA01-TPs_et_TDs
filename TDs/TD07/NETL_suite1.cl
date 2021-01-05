;; TD07 
;; Objectif :
;; implémenter en LISP un mécanisme de marquage exploitant la représen-tation 
;; élaborée lors de la précédente séance.


;; Création de fonction d'acces
(defun getv (id prop)
  (cdr (assoc prop (symbol-value id)))
  )

;; Création de fonction d'ajout de valeur
(defun addv (id prop value)
  (let ((pair (assoc prop (symbol-value id))))
    (setv id prop (cons value (cdr pair))))
  )
(defun setv (id prop val)
  (let ((pair (assoc prop (symbol-value id))))
  (set id (cons (cons prop val)
                (remove pair (symbol-value id) :test #'equal)
                )
       )
    )
  )

;; OU
(defun addv (id prop val)
  (let ((pair (assoc prop (symbol-value id))))
    (if pair 
        (setf (cdr pair)(cons val (cdr pair)))
      (push (list prop val)(symbol-value id))
      )
    )
  )

(setq *nodes* '())
(setq *arcs* '())
(defun initGraph ()
  (defnode 'Roxane 'Personne)
  (defnode 'Mondaine 'Ensemble)
  (defnode 'Christian 'Personne)
  (defnode 'CadetDeGascogne 'Ensemble)
  (defnode 'Noble 'Ensemble)
  (defnode 'deGuiche 'Personne)
  (defnode 'Comte 'Ensemble)
  (defnode 'Cyrano 'Personne)
  
  (defarc 'is-a 'N0 'N1)
  (defarc 'aime 'N7 'N0)
  (defarc 'is-a 'N7 'N3)
  (defarc 'is-a 'N3 'N4)
  (defarc 'aime 'N0 'N2)
  (defarc 'aime 'N2 'N0)
  (defarc 'is-a 'N2 'N3)
  (defarc 'aime 'N5 'N0)
  (defarc 'is-a 'N5 'N6)
  (defarc 'is-a 'N6 'N4)
  
  (mark-node 'N0 'M1)
  (mark-node 'N4 'M2)
  
  (wave 'M2 'is-a :inverse)
  
  (getresults 'aime 'M1 'M2)
  )
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
        (pushProperty from 'out-arcs id) ; modification du node from pour ajouter l'arc-out
        (pushProperty to 'in-arcs id) ; modification du node to pour ajouter l'arc-in
        (push id *arcs*) ; mettre à jour la liste *arcs*
        id ; retourner l'ID
    )
    NIL
    )
  )
;; autre manierer d'implementer defarc :
(defun defarc (type from to)
  (let ((id (gentemp "A")))
    (unless (and (boundp from)(boundp to)
                 (error "Au moins un noeud non existant" )))
    (set id `((type ,type)(from ,from)(to ,to))) ;; a noter : le raccourci pour concevoir des listes
    (set from (addv id 'out-arcs (symbol-value from)))
    (set to (addv id 'in-arcs (symbol-value to)))
    (pushnew id *arcs*)
    id
    )
  )

;; Explicitez les étapes du processus de marquage permettant de répondre à la question :
;; « Quels sont les nobles aimés de Roxane ? ».
;; algorithme pour répondre à la question
;; étapes :
;; 1- marquer Roxanne par M1
;;  - marquer Nobles par M2
;; 2- propager M2 le long des arcs is-a dans le sens inverse
;; 3- on regarde s'il y a des arcs "aime" ayant pour origine M1 et pour autre extremite M2

;; Fonctions de marquage et connaissance marquage d'un noeud
(defun mark-node (node mark)
  (addv node 'mark mark)
  )

(defun is-marked (node mark)
  (member mark (getv node 'mark))
  )

;; Fonction pour connaitre les noeuds marqués
(defun get-marked-nodes (mark)
  (let ((result NIL))
    (dolist (node *nodes*)
      (if (is-marked node mark) (push node result))
      )
    result
    )
  )

;; Fonctions successeurs et predecesseurs pour la propagation
(defun successeurs (node type-arc)
  (let ((succ NIL))
    (dolist (arc (getv node 'out-arcs))
      (if (equal type-arc (car (getv arc 'type)))
          (push (car (getv arc 'to)) succ)
        )
      )
    succ
  )
)
(defun predecesseurs (node type-arc)
 (let ((pred NIL))
    (dolist (arc (getv node 'in-arcs))
      (if (equal type-arc (car (getv arc 'type)))
          (push (car (getv arc 'from)) pred)
        )
      )
    pred
  )
)
;; Fonction de propagation
(defun wave (mark type-arc direction) ;; avec direction :diret ou :inverse
  (let ((marked-nodes (get-marked-nodes mark))(next-nodes NIL)(new-marked-nodes NIL))
    (loop 
      (dolist (node marked-nodes)
        (if (equal direction :direct)
            (setq next-nodes (successeurs node type-arc))
          (setq next-nodes (predecesseurs node type-arc))
          )
        (if next-nodes
            (dolist (n next-nodes)
              (if (not (is-marked n mark)) (progn (mark-node n mark)(pushnew n new-marked-nodes)))
              )
          )
        )
      (when (not marked-nodes) (return "Propagation finie")) 
      (setq marked-nodes new-marked-nodes)
      (setq new-marked-nodes NIL)
    )
    )
  )

(defun getresults (arc-type from-mark to-mark)
   (let ((result NIL)(current NIL))
     (dolist (arc *arcs*)
       (setq current (symbol-value arc))
       (if (and (equal (cadr (assoc 'type current)) arc-type)
                (and (is-marked (cadr (assoc 'from current)) from-mark)(is-marked (cadr (assoc 'to current)) to-mark)))
         (print arc))
      )
    result
     )
)