

(defun main ()
  ; Définition des différentes fonctions 
  
  (defun reverseA (arg1 arg2 arg3)
	(list arg3 arg2 arg1)        ; on inverse les arguments et on renvoie en liste
    )
  
  (defun reverseB (L)
    (if (= (length L) 3)
        (list (nth 2 L) (nth 1 L) (nth 0 L))    ; si la liste contient 3args, on renvoie un inversant les argument
        (if (= (length L) 2)
            (list (nth 1 L) (nth 0 L))          ; sinon si la liste contient 2args, on renvoie un inversant les argument
            L                                   ; sinon on renvoit la liste elle meme
        )
    )
  )


  (defun reverseC (L)
    (if (> (length L) 1)
        (append (reverseC (cdr L)) (list (car L)))
        (list (car L))
      )
   )
  
  
  
  (defun doubleIfAtom (A)
    (if (listp A) 
        (list A)              ; si A est une liste, on la renvoie elle meme
        (append (list  A  A)) ; sinon, on renvoie en doublant les atomes
     )
    )
  
  
  (defun double (L)
    (if (> (length L) 0)
        (append (doubleIfAtom (car L)) (double (cdr L))) ; Si la liste est plus grande que 1
      NIL; sinon on renvoie NIL
      )
   )
  
  
  (defun nombre3 (L)
    (if (> (length L) 2)    ; On regarde si la liste fait au moins 3 S-expressions
        (if (and (numberp (nth 2 L)) (and (numberp (nth 1 L)) (numberp (nth 0 L))))    ; On regarde si les trois premiers caractères sont des nombres
            "BRAVO"
          "PERDU"
         )
      "PERDU"
      )
    )
  
  (defun grouper (L1 L2)
    (mapcar #'list L1 L2)   ; On fait un mappage de chaque item et on les assemble en liste
    )

  
  (defun palindrome (L)
    (equal L (reverseC L))  ; On vérifie si la liste est égale à son reverse
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
  (unitTest 'reverseA '(1 2 3)  '(3 2 1))
  (newLine)
  (unitTest 'reverseB '((1))  '(1))
  (unitTest 'reverseB '((1 2))  '(2 1))
  (unitTest 'reverseB '((1 2 3))  '(3 2 1))
  (newLine)
  (unitTest 'reverseC '((1 2 3))  '(3 2 1))
  (unitTest 'reverseC '((a b (c d) e f))  '(f e (c d) b a))
  (newLine)
  (unitTest 'double '(((1 2) 2 (1 2 3) 3 4)) '((1 2) 2 2 (1 2 3) 3 3 4 4))
  (unitTest 'double '((1 (1) 2 3 (4))) '(1 1 (1) 2 2 3 3 (4)))
  (newLine)
  (unitTest 'nombre3 '((1 2 3))  '"BRAVO")
  (unitTest 'nombre3 '((1 2))  '"PERDU")
  (unitTest 'nombre3 '((1 D 3 C D 2))  '"PERDU")
  (unitTest 'nombre3 '((1 2 3 B C 5))  '"BRAVO")
  (newLine)
  (unitTest 'palindrome '((a b b a))  'T)
  (unitTest 'palindrome '((t o t o))  'NIL)
  (newLine)
  (unitTest 'grouper '((1 2 3)(4 5 6)) '((1 4) (2 5) (3 6)))
 "EndMain"
 )

(main)