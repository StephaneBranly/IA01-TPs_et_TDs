(defun main ()
  (setq ll '(A 1 BB 2 CCC 3 DDD 4))
  
  
  (defun F1 (L) ; Par mappage
    (mapcar #'print L) 
    )
  
  (defun F2 (L) ; Par récurrence
    (if L       ; condition d arret
        (progn (print (car L))(F2 (cdr L))) ; progn afin de créer un bloc contenant plusieurs s-expressions
      "done"
      )
    )
  
  (defun F3 (L) ; Avec un dolist
    (dolist (x L 'done)(print x))
    )
  
  
  (defun F4 (L)    ; avec chaque élément x de la liste L
    (loop for x in L
        do (print x)
          )
    'done
    )
  
  (defun F5 (L)   ; avec x dans [0 ; length L [ (x entier)
    (dotimes (x (length L) 'done)(print (nth x L)))
    )
  
  
  (setq *html* '(html (head (title "Ma page")) (body (h1 "Mon titre")(p "Soror et amula Romae"))))
  
  (defun make-html (parent file)
    (if (listp parent)   ; Si parent est une liste
        (progn (format file "~%<~S>" (car parent))   ; balise HTML ouvrante <balise>
          (loop for node in (cdr parent)             ; boucle pour chaque node de cette balise
              do (make-html node file)
                )
          (format file "~%</~S>" (car parent))       ; balise HTML fermante </balise>
          )
      (format file "~A" parent)                      ; affichage du contenu
      )
    )
  
  (defun unitTest (fx data expectedResult)       ; fonction de test unitaire
    (setq result (apply fx data))             ; on calcule le résultat de fx(data) avec les données envoyées
    (if (equal result expectedResult)            ; on compare avec le résultat attendu indiqué
      (setq test 'pass)
      (setq test 'error)
    )
    (format t "~% ~S unitTest : ~S ; result = ~S | expected = ~S" fx test result expectedResult)   ; on affiche le résultat du test
    )
  
  (defun newLine ()  ; pour faire un retour à la ligne
    (format t "~%") 
    )
       
  (newLine)
  (F5 ll)
  
  (with-open-file (file "page.html" :if-­does-­not-­exist :create :if-­exists :overwrite :direction :output)
    (make-html *html* file)
    )
  )


(main)