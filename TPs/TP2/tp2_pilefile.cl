(setq *graphe* '((((H -1) (L -1) (C -1) (X -1)) 0 10) 
                 (((H -1) (L -1) (C -1) (X 1)) 1 11 13) 
                 (((H -1) (L -1) (C 1) (X -1)) 2 10 11 14)
                 (((H -1) (L -1) (C 1) (X 1)) 3) 
                 (((H -1) (L 1) (C -1) (X -1)) 4 13 14) 
                 (((H -1) (L 1) (C -1) (X 1)) 5 13 15) 
                 (((H -1) (L 1) (C 1) (X -1)) 6) 
                 (((H -1) (L 1) (C 1) (X 1)) 7) 
                 (((H 1) (L -1) (C -1) (X -1)) 8) 
                 (((H 1) (L -1) (C -1) (X 1)) 9) 
                 (((H 1) (L -1) (C 1) (X -1)) 10 0 2) 
                 (((H 1) (L -1) (C 1) (X 1)) 11 1 2) 
                 (((H 1) (L 1) (C -1) (X -1)) 12) 
                 (((H 1) (L 1) (C -1) (X 1)) 13 1 4 5) 
                 (((H 1) (L 1) (C 1) (X -1)) 14 2 4) 
                 (((H 1) (L 1) (C 1) (X 1)) 15  5)))



(defun successeurs (etat)
  (cdr (assoc etat (mapcar #'cdr *graphe*)))
  )

(defun vars-state (nbState)
  (car (nth nbState *graphe*))
  )
(defun varH-state (nbState)
  (cadr (nth 0 (vars-state nbState)))
  )
(defun varL-state (nbState)
  (cadr (nth 1 (vars-state nbState)))
  )
(defun varC-state (nbState)
  (cadr (nth 2 (vars-state nbState)))
  )
(defun varX-state (nbState)
  (cadr (nth 3 (vars-state nbState)))
  )


(defun valid_successeurs (etat visites)
  (let ((valid_successeurs 'NIL)(succs (successeurs etat)))
  (dolist (x succs)
    (if (member x visites) 
        NIL
      (push x valid_successeurs )
      )
    )
    valid_successeurs
    )
  )


(defun rech-prof (start end)
  (let ((current start)
        (pile (list start))
        (chemin 'NIL)
        (solved NIL)
        )
    (loop
      (setq current (pop pile))
      (push current chemin)
      ;; (print (reverse chemin)) ;; a decommenter pour voir les chemins empruntes
      
      (if (eq current end) (progn (print-solution (reverse chemin))(setq solved T))) ;; si le noeud actuel = noeud final alors imprimer chemin      

      (let ((succs (valid_successeurs current chemin))) ;; on determine les successeurs valides (non visites dans le chemin actuel)
        (if succs
            (mapcar #'(lambda (s)(push s pile)) succs)  ;; s'il y en a, on les push dans la pile                 
          (progn                                        ;; sinon, on remonte le chemin
            (loop ;; boucle pour remonter l'arbre
              (if (and pile (not (member (car pile) (valid_successeurs (car chemin) chemin)))) ;; on remonte tant que la pile est non vide et que le noeud actuel n'a pas un fils a visiter (sur la pile)
                  (pop chemin)
                (return NIL)
                )
              )
            )
          )
        )
      (when (not pile) (return solved)) ;; retour quand la pile est vide
        )
    )
  )

(defun rech-larg (start end)
  (let ((current (list start))
        (file* (list (list start)))
        (chemin 'NIL)
        (solved NIL)
        )
    (loop ;; boucle
      (setq file* (reverse file*)) ;; on défile le chemin à explorer
      (setq chemin (pop file*))
      (setq file* (reverse file*))
      
      ;; (print (reverse chemin)) ;; a decommenter pour voir les chemins empruntes
      
      (if (eq (car chemin) end) (progn (print-solution (reverse chemin))(setq solved T))) ;; si le noeud actuel = noeud final alors imprimer chemin      

      (let ((succs (valid_successeurs (car chemin) chemin))) ;; on determine les successeurs valides (non visites dans le chemin actuel)
        (if succs
            (mapcar #'(lambda (s)(push (append (list s) chemin) file*)) succs)  ;; s'il y en a, on les push dans la file                 
          )
        )
      
      (when (not file*) (return solved)) ;; retour quand la file est vide
      )
  )
  )

(defun print-solution (solution)
  (format t "~%~%>>> Solution trouvée")
  (format t "~%>> Parcours des noeuds : ~S" solution)
  (format t "~%>> Actions : " solution)
  (let ((old-state (car solution)))
    (mapcar 
        #'(lambda (e)
            (print-action e old-state)
            (setq old-state e)) 
      solution)
    )
  )

(defun print-action (current-state old-state)
  (if (/= (varX-state current-state) (varX-state old-state))
      (format t "~%>Traversez avec le chou")
    (if (/= (varL-state current-state) (varL-state old-state))
        (format t "~%>Traversez avec le loup")
      (if (/= (varC-state current-state) (varC-state old-state))
        (format t "~%>Traversez avec la chèvre")
       (if (/= (varH-state current-state) (varH-state old-state))
        (format t "~%>Traversez tout seul")
      )
      )
      )
    )
  )



(defun unitTest (fx data expectedResult)       ; fonction de test unitaire
    (let ((result (apply fx data)))              ; on calcule le resultat de fx(data) avec les donnees envoyees
      (if (equal result expectedResult)            ; on compare avec le resultat attendu indique
        (setq test 'pass)
        (setq test 'error)
        )
      (format t "~% ~S unitTest : ~S ; result = ~A | expected = ~A" fx test result expectedResult)   ; on affiche le resultat du test
      )
  )


(defun tests ()
  (unitTest 'successeurs '(5)  '(13 15))
  (unitTest 'valid_successeurs '(2 (15 3 4 11))  '(14 10))
  (unitTest 'vars-state '(10)  '((H 1) (L -1) (C 1) (X -1)))
  (unitTest 'varH-state '(10)  '1)
  (unitTest 'varL-state '(10)  '-1)
  (unitTest 'varC-state '(10)  '1)
  (unitTest 'varX-state '(10)  '-1)
)
(tests)
