;; EXPLICATIONS CLOS
;; Definition d'une classe :
;; entre () se trouveront les classes dont on hérite
(defclass $person ()
  (($nom :accessor nom :initarg :nom :type string)
   ($prenom :accessor prenom :initarg :prenom :type string)
   ($age :accessor age :initarg :age :type int))
  )

;; Création d'une instance
(setq $0 (make-instance '$person :nom "Albert" :prenom "Jean"))

;; Affichage de l'instance créée
(describe $0)





;; TD10
;; Définition des différentes classes
;; idéalement, chaque classe devrait hériter de $figure qui permettrait d'ajouter des attributs communs 
;; comme $couleur, $epaisseur, $nom, $createur, etc

;; QUESTION 1, création des classes

(defclass $point ()
  (($x :accessor x :initarg :x :type real) ;; pas besoin de mettre le type pour les points
   ($y :accessor y :initarg :y :type real))
  )
(setq $1 (make-instance '$point :x 1 :y 2))
(describe $1)


(defclass $cercle ()
  (($c :accessor c :initarg :c :type $point)
   ($r :accessor r :initarg :r :type real))
  )
(setq $2 (make-instance '$cercle :c $1 :r 5))
(describe $2)


(defclass $carre ()
  (($M :accessor M :initarg :M :type $point)
   ($w :accessor w :initarg :w :type real))
  )
(setq $3 (make-instance '$point :x 3 :y 7))
(setq $4 (make-instance '$carre :M $3 :w 9))
(describe $4)


(defclass $rectangle ()
  (($M :accessor M :initarg :M :type $point)
   ($w :accessor w :initarg :w :type real)
   ($h :accessor h :initarg :h :type real))
  )
(setq $5 (make-instance '$point :x -4 :y -2))
(setq $6 (make-instance '$rectangle :M $5 :w 3 :h 2))
(describe $6)

(defclass $triangle ()
  (($M1 :accessor M1 :initarg :M1 :type $point)
   ($M2 :accessor M2 :initarg :M2 :type $point)
   ($M3 :accessor M3 :initarg :M3 :type $point))
  )
(setq $7 (make-instance '$triangle :M1 $5 :M2 $3 :M3 $1))
(describe $7)

(defclass $polygone ()
  (($sommets :accessor sommets :initarg :sommets :type list))
  )
(setq $8 (make-instance '$polygone :sommets (list $5 $3 $1)))
(describe $8)


;; QUESTIONS SUPP :
;; créer l'origine 0 de coordonnées (0,0)
(setq $origin (make-instance '$point :x 0 :y 0))
(describe $origin)
;; créer un carré un carré de sommet (3,4) et de coté 3
(setq $carreSommet (make-instance '$point :x 3 :y 4))
(setq $carreInstance (make-instance '$carre :M $carreSommet :w 3))
(describe $carreInstance)



;; Explication des méthodes
(defmethod printself ((p $person))
  (print (nom p))
  (print (prenom p))
  )
(printself $0) ;; appele de la methode




(defmethod translate ((p $point) dx dy)
  (setf (x p) (+ (x p) dx))
  (setf (y p) (+ (y p) dy))
  p
  )
(setq $ptest (make-instance '$point :x 1 :y 1))
(describe $ptest)
(translate $ptest 4 -1)

(defmethod translate ((instance $cercle) dx dy)
  (translate (c instance) dx dy)
  instance
  )
(setq $pcercle (make-instance '$point :x 1 :y 1))
(setq $ctest (make-instance '$cercle :c $pcercle :r 5))
(describe $pcercle)
(translate $pcercle 4 -1)


(defmethod translate ((instance $triangle) dx dy)
  (translate (M1 instance) dx dy)
  (translate (M2 instance) dx dy)
  (translate (M3 instance) dx dy)
  instance
  )
(defmethod translate ((instance $carre) dx dy)
  (translate (M instance) dx dy)
  instance
  )
(defmethod translate ((instance $triangle) dx dy)
  (translate (M instance) dx dy)
  instance
  )
(defmethod translate ((instance $polygone) dx dy)
  (dolist (point (sommets instance))
    (translate point dx dy)
    )
  instance
  )
(describe $5)
(translate $8 1 2)

;; SYM X
(defmethod symx ((p $point))
  (setf (x p) (* (x p) -1))
  p
  )
(defmethod symx ((instance $cercle))
  (symx (c instance))
  instance
  )
(defmethod symx ((instance $triangle))
  (symx (M1 instance))
  (symx (M2 instance))
  (symx (M3 instance))
  instance
  )
(defmethod symx ((instance $carre))
  (symx (M instance))
  (translate (M instance) (- 0 (w instance)) 0)
  instance
  )
(defmethod symx ((instance $rectangle))
  (symx (M instance))
  (translate (M instance) (- 0 (w instance)) 0)
  instance
  )
(defmethod symx ((instance $polygone))
  (dolist (point (sommets instance))
    (symx point)
    )
  instance
  )

;; SYM Y
(defmethod symy ((p $point))
  (setf (y p) (* (y p) -1))
  p
  )
(defmethod symy ((instance $cercle))
  (symy (c instance))
  instance
  )
(defmethod symy ((instance $triangle))
  (symy (M1 instance))
  (symy (M2 instance))
  (symy (M3 instance))
  instance
  )
(defmethod symy ((instance $carre))
  (symy (M instance))
  (translate (M instance) 0 (- 0 (w instance))) 
  instance
  )
(defmethod symy ((instance $rectangle))
  (symx (M instance))
  (translate (M instance) 0 (- 0 (h instance)))
  instance
  )
(defmethod symy ((instance $polygone))
  (dolist (point (sommets instance))
    (symy point)
    )
  instance
  )

;; SYM Origine
(defmethod symo ((p $point))
  (symx p)
  (symy p)
  p
)
(defmethod symo ((p $cercle))
  (symx p)
  (symy p)
  p
)
(defmethod symo ((p $carre))
  (symx p)
  (symy p)
  p
)
(defmethod symo ((p $rectangle))
  (symx p)
  (symy p)
  p
)
(defmethod symo ((p $polygone))
  (symx p)
  (symy p)
  p
)
(defmethod symo ((p $polygone))
  (symx p)
  (symy p)
  p
  )
(symo $ctest)
(describe $pcercle)

(defmethod zoom ((p $point) facteur)
  (setf (x p) (* (x p) facteur))
  (setf (y p) (* (y p) facteur))
  p
  )
(defmethod zoom ((p $cercle) facteur)
  (zoom (c p) facteur)
  (setf (r p) (* (r p) facteur))
  p
  )
(defmethod zoom ((p $carre) facteur)
  (zoom (M p) facteur)
  (setf (w p) (* (w p) facteur))
  p
  )
(defmethod zoom ((p $rectangle) facteur)
  (zoom (M p) facteur)
  (setf (w p) (* (w p) facteur))
  (setf (h p) (* (h p) facteur))
  p
  )
(defmethod zoom ((p $triangle) facteur)
  (zoom (M1 p) facteur)
  (zoom (M2 p) facteur)
  (zoom (M3 p) facteur)
  p
  )
(defmethod zoom ((instance $polygone) facteur)
  (dolist (point (sommets instance))
    (zoom point facteur)
    )
  instance
  )

(defmethod duplicate ((instance $point) dx dy)
  (translate (make-instance '$point :x (x instance) :y (y instance)) dx dy)
  )
(defmethod duplicate ((instance $cercle) dx dy)
   (make-instance '$cercle :c (duplicate (c instance) dx dy) :r (r instance))
  )
(defmethod duplicate ((instance $carre) dx dy)
   (make-instance '$carre :M (duplicate (M instance) dx dy) :w (w instance))
  )
(defmethod duplicate ((instance $rectangle) dx dy)
   (make-instance '$rectangle :M (duplicate (M instance) dx dy) :w (w instance) :h (h instance))
  )
(defmethod duplicate ((instance $triangle) dx dy)
  (make-instance '$triangle 
    :M1 (duplicate (M1 instance) dx dy) 
    :M2 (duplicate (M2 instance) dx dy)
    :M3 (duplicate (M3 instance) dx dy))
  )
(defmethod duplicate ((instance $polygone) dx dy)
  (let ((listeID (gentemp "listeDeSommets")))
    (dolist (sommet (sommets instance))
      (set listeID (append (translate sommet dx dy) listeID))
      )
    (make-instance '$polygone :sommets listeID)
    )
  )
(describe $ptest)
(zoom $ptest 2)
(describe (duplicate $ptest 1 2))