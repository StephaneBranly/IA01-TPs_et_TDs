(defun main ()
  
  ; Definition des differentes fonctions
  
  ; Question 1

  (defun nom (tombe)
    (car tombe)  
    )
 
  (defun an-inhum (tombe)
    (cadr tombe)
    )

  (defun num (tombe)
    (second (third tombe))
    )

  (defun rangee (tombe)
    (caaddr tombe)
    )

  (defun debut-loc (tombe)
    (cadddr tombe)
    )

  (defun duree-loc (tombe)
    (nth 0 (cddddr tombe))
    )


  ; Question 2
  
  (defun pos (tombe)
    (caddr tombe)
   )
  
  (defun qui-est-la (position cimetiere)
    (if (> (length cimetiere) 0)                     ; tant que la liste est non vide
        (if (equal (pos (car cimetiere)) position) ; on recupere la postion de la tombe
            (caar cimetiere)                         ; on recupere le prenom sur la tombe si elle correspond
            (qui-est-la position (cdr cimetiere))    ; sinon on verifie le reste
          )
        '"Emplacement non attribue"
      )
    )

  ; Question 3
  
  ; retourne t ou nil si la condition est valide
  (defun prevoyant? (tombe)
    (> (an-inhum tombe) (debut-loc tombe))
    )
 
  ; Question 4

  ; map sur chaque tombe et incrementation d'une variable n quand la condition est validee grace a la fonction anonyme
  (defun nb-prevoyants (cimetiere)
    (let ((n 0))                                                                          ; on declare n
      (mapcar #'(lambda (tombe) (if (prevoyant? tombe) (setq n (+ n 1)) nil)) cimetiere)
      n
      )
    )

  ; Question 5

  ; on parcourt chaque tombe et on verifie si la rangee correspond a r
  (defun annuaire (cimetiere n)
    (if (> (length cimetiere) 0)
        (if (= (rangee (car cimetiere)) n)                                ; si la rangee est la bonne
            (cons (nom (car cimetiere)) (annuaire (cdr cimetiere) n))     ; on concatène avec le prochain (en recursif)
          (annuaire (cdr cimetiere) n)                                    ; sinon on avance sans concatener     
          )
      )
    )

  ; Question 6

  ; map sur chaque tombe et affectation sur la variable de doyen ou benjamin si possible
  (defun doyen-benjamin (cimetiere)
    (let (                                                                       ; on declare deux variables pour le doyen et le benjamin avec let
        (doyen (car cimetiere)) 
        (benjamin (car cimetiere))
        )
      (mapcar                                                                    ; on map sur le cimetière une fonction anonyme
        #'(lambda (tombe)                                                        ; la fonction anonyme affecte ou pas aux deux valeurs une nouvelle selon leurs conditions respectives
        (if (< (an-inhum tombe) (an-inhum doyen)) (setq doyen tombe) nil)        ; condition du doyen: si la nouvelle tombe est plus ancienne
        (if (> (an-inhum tombe) (an-inhum benjamin)) (setq benjamin tombe) nil)  ; condition du benjamin: si la nouvelle tombe est plus récente
          ) 
        cimetiere)
      (list doyen benjamin)                                                      ; on retourne une liste comportant le doyen et le benjamin
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
  
  
  (defun newLine ()  ; pour faire un retour à la ligne
    (format t "~%") 
    )
  
  
  
  ; Execution des differentes commandes
  
  (setq pere-lachaise '(
    ("Becaud" 2001 (45 17) 2000 30) ("Desproges" 1988 (11 6) 1988 30)
    ("Grappelli" 1997 (85 23) 1997 5) ("Morrison" 1971 (6 12) 1971 30)
    ("Mouloudji" 1994 (42 9) 1990 15) ("Nohain" 1981 (89 14) 1979 15)
    ("Oussekine" 1986 (85 37) 1986 5) ("Petrucciani" 1999 (11 26) 1999 15)
    ("Popesco" 1993 (85 16) 1985 30) ("Signoret" 1985 (44 7) 1980 30)
    ("Zavatta" 1993 (11 16) 1993 15))
    )
  
  (setq tombe-test '("Becaud" 2001 (45 17) 2000 30))

  (newLine)
  (unitTest 'nom '(("Becaud" 2001 (45 17) 2000 30))  '"Becaud")
  "EndMain"

  (newLine)
  (unitTest 'an-inhum '(("Becaud" 2001 (45 17) 2000 30))  2001)
  "EndMain"

  (newLine)
  (unitTest 'num '(("Becaud" 2001 (45 17) 2000 30))  17)
  "EndMain"

  (newLine)
  (unitTest 'rangee '(("Becaud" 2001 (45 17) 2000 30))  45)
  "EndMain" 

  (newLine)
  (unitTest 'debut-loc '(("Becaud" 2001 (45 17) 2000 30))  2000)
  "EndMain"

  (newLine)
  (unitTest 'duree-loc '(("Becaud" 2001 (45 17) 2000 30))  30)
  "EndMain" 

  (newLine)
  (unitTest 'pos '(("Becaud" 2001 (45 17) 2000 30)) '(45 17))
  "EndMain"

  (newLine)
  (unitTest 'qui-est-la '((11 16) (
    ("Becaud" 2001 (45 17) 2000 30) ("Desproges" 1988 (11 6) 1988 30)
    ("Grappelli" 1997 (85 23) 1997 5) ("Morrison" 1971 (6 12) 1971 30)
    ("Mouloudji" 1994 (42 9) 1990 15) ("Nohain" 1981 (89 14) 1979 15)
    ("Oussekine" 1986 (85 37) 1986 5) ("Petrucciani" 1999 (11 26) 1999 15)
    ("Popesco" 1993 (85 16) 1985 30) ("Signoret" 1985 (44 7) 1980 30)
                                   ("Zavatta" 1993 (11 16) 1993 15))
                          ) '"Zavatta")
  "EndMain"

  (newLine)
  (unitTest 'prevoyant? '(("Becaud" 2001 (45 17) 2000 30)) T)
  "EndMain"

  (newLine)
  (unitTest 'nb-prevoyants '((
    ("Becaud" 2001 (45 17) 2000 30) ("Desproges" 1988 (11 6) 1988 30)
    ("Grappelli" 1997 (85 23) 1997 5) ("Morrison" 1971 (6 12) 1971 30)
    ("Mouloudji" 1994 (42 9) 1990 15) ("Nohain" 1981 (89 14) 1979 15)
    ("Oussekine" 1986 (85 37) 1986 5) ("Petrucciani" 1999 (11 26) 1999 15)
    ("Popesco" 1993 (85 16) 1985 30) ("Signoret" 1985 (44 7) 1980 30)
                                      ("Zavatta" 1993 (11 16) 1993 15))
                             ) 5)
  "EndMain"

  (newLine)
  (unitTest 'annuaire '((
    ("Becaud" 2001 (45 17) 2000 30) ("Desproges" 1988 (11 6) 1988 30)
    ("Grappelli" 1997 (85 23) 1997 5) ("Morrison" 1971 (6 12) 1971 30)
    ("Mouloudji" 1994 (42 9) 1990 15) ("Nohain" 1981 (89 14) 1979 15)
    ("Oussekine" 1986 (85 37) 1986 5) ("Petrucciani" 1999 (11 26) 1999 15)
    ("Popesco" 1993 (85 16) 1985 30) ("Signoret" 1985 (44 7) 1980 30)
                                 ("Zavatta" 1993 (11 16) 1993 15)) 85
                        ) '("Grappelli" "Oussekine" "Popesco"))
  "EndMain"

  (newLine)
  (unitTest 'doyen-benjamin '((
    ("Becaud" 2001 (45 17) 2000 30) ("Desproges" 1988 (11 6) 1988 30)
    ("Grappelli" 1997 (85 23) 1997 5) ("Morrison" 1971 (6 12) 1971 30)
    ("Mouloudji" 1994 (42 9) 1990 15) ("Nohain" 1981 (89 14) 1979 15)
    ("Oussekine" 1986 (85 37) 1986 5) ("Petrucciani" 1999 (11 26) 1999 15)
    ("Popesco" 1993 (85 16) 1985 30) ("Signoret" 1985 (44 7) 1980 30)
                                       ("Zavatta" 1993 (11 16) 1993 15))
                              ) '(("Morrison" 1971 (6 12) 1971 30) ("Becaud" 2001 (45 17) 2000 30)))
  "EndMain"

  )
(main)