(defun main ()
  
  ; Définition des différentes fonctions
  
  (defun my-assoc (cle a-list)
    (if (> (length a-list) 0)
        (if (equal (car (car a-list)) cle)
            (car a-list)
            (my-assoc cle (cdr a-list))
          )
        NIL
      )
    )

  (defun cles (a-list)
    (mapcar #'car a-list)
    )

  (defun creation (listeCles listeValeurs)
    (mapcar 'list listeCles listeValeurs) 
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
  
  
  (defun newLine ()  ; pour faire un retour a la ligne
    (format t "~%") 
    )


  ; Execution des differentes commandes

  (newLine)
  (unitTest 'my-assoc '(Pierre ((Yolande 25) (Pierre 22) (Julie 45)))  '(Pierre 22))
  (newLine)
  (unitTest 'cles '(((Yolande 25) (Pierre 22) (Julie 45))) '(Yolande Pierre Julie))
  (newLine)
  (unitTest 'creation '((Yolande Pierre Julie)(25 22 45)) '((Yolande 25) (Pierre 22) (Julie 45)))
  "EndMain"
  
  )

(main)