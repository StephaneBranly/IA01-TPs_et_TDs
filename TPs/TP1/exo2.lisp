

(defun main ()
  ; Définition des différentes fonctions 

  (defun list-triple-couple (L)
    (mapcar #'(lambda (a) (list a (* a 3))) L)
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
  
  
  
  (defun newLine ()  ; pour faire un retour à la ligne
    (format t "~%") 
    )
  
  
  
  
  ; Execution des différentes commandes

  (newLine)
  (unitTest 'list-triple-couple '((0 2 3 11))  '((0 0) (2 6) (3 9) (11 33)))
  "EndMain"

 )

(main)