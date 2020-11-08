(defun deriv-term (A dA) 
  (if (eq a DA)
      1
    0
    )
  )

  
(defun deriv-sum (exp var)
  (if (listp exp)
      (list (car exp) (deriv (cadr exp) var) (deriv (caddr exp) var))
    (deriv-term exp var)
    )
  )

(defun deriv-dif (exp var)
  (if (listp exp)
      (list (car exp) (deriv (cadr exp) var) (deriv (caddr exp) var))
    (deriv-term exp var)
    )
  )

(defun deriv-mult (exp var)
  (if (listp exp)
      (list 
       '+ 
       (list '* (deriv (cadr exp) var) (caddr exp))
       (list '*  (cadr exp) (deriv (caddr exp) var))
       )
    (deriv-term exp var)
    )
  )

(defun deriv-div (exp var)
  (if (listp exp)
      (list 
       '/
       (list 
        '- 
        (list '* (deriv (cadr exp) var) (caddr exp))
        (list '*  (cadr exp) (deriv (caddr exp) var))
        )
       (list 
        '* 
        (caddr exp)
        (caddr exp)
        )
       )              
    (deriv-term exp var)
    )
  )

(defun simplifyExp (exp)
  (if (list exp)
      (cond 
       ((and (numberp (caddr exp))(numberp (cadr exp)))  ; Opération a signe b = évalution (a signe b)
        (eval exp))
       
       ((and (eq (car exp) '+)(eq (cadr exp) 0))    ; Somme 0 + a = a
        (caddr exp))
       
       ((and (eq (car exp) '+)(eq (caddr exp) 0))   ; Somme a + 0 = a
        (cadr exp))
       
       
       ((and (eq (car exp) '-)(eq (caddr exp) 0))   ; Somme a - 0 = a
        (cadr exp))

       ((and (eq (car exp) '*)(or (eq (cadr exp) 0)(eq (caddr exp) 0)))  ; Produit (a * 0) ou (0 * a) = 0
        0)
       
       ((and (eq (car exp) '*)(eq (cadr exp) 1)) ; Produit 1 * x = x
        (caddr exp))
       
       ((and (eq (car exp) '*)(eq (caddr exp) 1)) ; Produit x * 1 = x
        (cadr exp))
       
       ((and (eq (car exp) '/)(eq (cadr exp) 0))   ; Somme 0 / a = 0
        0)
    
       (T 
        exp)
       )
    
    
    NIL)
  )

(defun simplify (exp)
  (if (listp exp)
      (cond 
       ((and (not (listp (cadr exp))) (not (listp (caddr exp)))) 
        (simplifyExp exp))
       
       ((and (listp (cadr exp)) (not (listp (caddr exp)))) 
        (simplifyExp (list (car exp) (simplify (cadr exp))(caddr exp))))
       
       ((and (listp (caddr exp)) (not (listp (cadr exp)))) 
        (simplifyExp (list (car exp) (simplify (caddr exp))(cadr exp))))
       
       (T 
        (simplifyExp (list (car exp) (simplify (cadr exp)) (simplify (caddr exp)))))
       
       )   
    exp    
    )
  )


(defun deriv (exp var)
  (if (listp exp)
      (cond 
       ((eq (first exp) '+) (deriv-sum exp var))
       ((eq (first exp) '-) (deriv-dif exp var))
       ((eq (first exp) '*) (deriv-mult exp var))
       ((eq (first exp) '/) (deriv-div exp var))
       (T (print '"error"))
       )
    (deriv-term exp var)
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