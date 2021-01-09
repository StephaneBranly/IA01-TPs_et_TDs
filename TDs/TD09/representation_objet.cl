;; td9 ; representation objet

;;;;;; QUESTION 1
;;;; Comment repr�senter un objet ? Nous prendrons l�exemple d�Albert, qui est ag� de 26 ans et qui a deux fr�res : Max et Jean.
;; Exemple de representation possible
;; $2(($type.$person)($name."Filbert")($age.26)($brother $4 $5))
;; $4(($type.$person)($name."Max)($brother $2 $5))
;; $5(($type.$person)($name."Jean)($brother $2 $4))


;;;;;; QUESTION 2
;;;; Comment peut-on abstraire cet objet ?
;; Notion de type apportee :
;; $person(($type.$class)($name."personne")($attr $name $age)($ref $brother $sister))

;; Notion d'heritage apportee :
;; $etudiant(($type.$class)($is-a $person))

;; Representation du type class lui-meme :
;; $class(($type.$class)($attr.$nom)($rel $attr $rel))
;; attr: valeur simple ; rel: valeur objet




;;;;;; QUESTION 3
;;;;  � partir de cette repr�sentation, comment pouvons-nous d�duire qu�un �tudiant est une personne ? qu�il peut avoir des fr�res et des soeurs ?
;; Il faudrait mettre un marqueur sur $person et le propager en sens-inverse pour les relations 'is-a et verifier a la fin si $etudiant est marqu�
;; Pour v�rifier s'il peut avoir des freres et soeurs, il faudrait remonter par les relations 'is-a et verifier s'il y a existence d'une relation $brother ou $sister



;;;;;; QUESTION 4
;;;; Avec une telle repr�sentation, comment peut-on introduire des valeurs par defaut au niveau des propri�t�s ?
;; Il existe au moins 2 repr�sentations possibles 
;; - mod�liser les propri�t�s
;;   - $brother(($type.$rel)($owner.$person)($successors.$person)($default.NIL))
;;   - $sister(($type.$rel)($owner.$person)($successors.$person)($default.NIL))
;;   - $age(($type.$attr)($owner.$person)($age 15))
;; - instancier un individu 'ideal'
;;   - $ideal(($type.$person)($owner."x")($age.15)($sister.NIL)($brother.NIL))


;;;;;; QUESTION 5
;;;; Comment peut-on exprimer des contraintes sur les objets d�une telle repr�sentation (par exemple, le fait qu�une personne de sexe masculin ne peut �tre m�re de quelqu�un) ?
;; Les contraintes peuvent �tre exprim�es par :
;; - des r�gles 
;; - des m�thodes
