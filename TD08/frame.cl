;; Fonctions faisant parties du 'vocabulaire' frame : type, value, is-a, if-needed, if-added

(setq ELEPHANT
      '(ELEPHANT
        (TYPE (VALUE CONCEPT))
        (IS-A (VALUE ETRE))
        (COLOR (DEFAULT "grey"))
        (AGE (IF-NEEDED ask-user)
             (IF-ADDED check-age))
        (POIDS (IF-NEEDED computer-weight-from-age))
        (AFFICHAGE (IF-ADDED draw-elephant)
                   (IF-REMOVED erase-elephant))))

(setq *frames* NIL)
(pushnew 'ELEPHANT *frames*)


;; (make-individu 'ELEPHANT '(NAME "Clyde" COLOR "grey" AGE 5))

;; syntaxe a prendre pour ce td:
(make-individu 'Clyde 'Elephant 'color "rose" 'age 405)

(defun make-individu (frame concept &rest prop-val)
  (unless (concept-defined concept)(error "Concept non defini"))
  (let ((id (gentemp "F"))(allowed-slots (mapcar 'car (cdr (symbol-value concept)))) slot value)
    (set id (list frame 
                        (list 'type (list 'value 'individu))
                        (list 'is-a (list 'value concept))))
    (loop (unless prop-val (return nil))
          (setq slot (pop prop-val))
          (setq value (pop prop-val))
               (if (slot-defined concept slot)
                   (progn
                     (setq fn (cadr (assoc 'if-added (cdr (assoc slot (cdr (symbol-value concept)))))))
                     (setq value (if fn (funcall fn slot value) value))
                     (when value 
                     (set id (append (symbol-value id)
                                     (list (list slot (list 'value value))))))
                     )
              )
          )
    (pushnew id *frames*)
    id
    )
  )



;; création des fonctions 'demons'
;; - déclenchée par if-added
(defun check-age (slot value)
  (if (AND (< value 100)(> value 0)) value NIL))
 
(get-slot-value 'F29 'age)
(get-slot-value 'F29 'color)

;; creation des fonctions d'acces
(defun get-slot-value (frame slot)
  (let ((value NIL))
    (if (assoc slot (cdr (symbol-value frame))) 
        (setq value (cadr (assoc 'VALUE (cdr (assoc slot (cdr (symbol-value frame)))))))
        
      (let ((type (cadr (assoc 'VALUE (cdr (assoc 'is-a (cdr (symbol-value frame))))))))
        (if (assoc 'default (cdr (assoc slot (cdr (symbol-value type)))))
            (setq value (cadr (assoc 'default (cdr (assoc slot (cdr (symbol-value type)))))))
          (progn
            (setq fn (cadr (assoc 'if-needed (cdr (assoc slot (cdr (symbol-value type)))))))
            (setq value (if fn (funcall fn frame slot) NIL)))
             )
          )
      )
    value
      )
    
    )
  ;; 3 cas possibles :
  ;; - valeur lue directement dans frames individus
  ;; - valeur par défaut dans le parent
  ;; - déclenchement du démon if-needed
  )

(defun ask-user (frame slot)
  (print "Ask user appele")
  1
)  
(defun slot-defined (concept slot)
  (if (assoc slot (cdr (symbol-value concept)))
      T
    NIL
    )
  )
(defun concept-defined (concept)
  (if (member concept *frames*)
      T
    NIL
    )
  )