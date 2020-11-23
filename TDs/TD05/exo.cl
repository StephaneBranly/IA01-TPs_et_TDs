;;; Systèmes experts - Stéphane BRANLY

;; RECAP COURS

;; Un système expert est composé d'un moteur d'inférences, d'une base de connaissance et d'une interface utilisateur

;; Ordre d'un système expert 
;; 0  -> Faits booléens (vrai/faux)
;; 0+ -> attribut - valeur
;;       objet.attribut - valeur
;;      exemples : (Pluie = Forte) ET (dist>3) ALORS (transport = voiture)
;; 1  -> Utilisation de variables
;;       Si père($x) = $y ET père($y) = $z ALORS grandpère($x) = $z

;; Mécanismes du moteur d'inférences
;; Chainage avant   : (modus ponens)  SI (A => B ET A) ALORS B
;; Chainage arrière : (modus tollens) SI (A => B ET ¬B) ALORS ¬A


;; Question 2)
;; Base de règles 
;; R1 B et D et E? F
;; R2 D et G?A
;; R3 C et F?A
;; R4 C?D
;; R5 D?E
;; R6 A?H
;; R7 B?X
;; R8 X et C?A

;; Base de faits
;; B, C

;; But
;; H

;; Par chainage avant
;; Ec = Ensemble des conflits (= toutes les règles pouvant être appliquées à l'état actuel)
;; Cycle | EC      | BF             | Règle déclechée (en profondeur d'abord)
;; 0     |         | {B,C}          |
;; 1     | {R4,R7} | {B,C,D}        | R4
;; 2     | {R5,R7} | {B,C,D,F}      | R5
;; 3     | {R2,R7} | {B,C,D,E,F}    | R1
;; 4     | {R3,R7} | {A,B,C,D,E,F}  | R3
;; 5     | {R6,R7} | {A,B,C,D,E,F,H}| R6

;; Cycle | EC      | BF               | Règle déclechée (en largeur d'abord)
;; 0     |         | {B,C}            |
;; 1     | {R4,R7} | {B,C,D}          | R4
;; 2     | {R7,R5} | {B,C,D,X}        | R7
;; 3     | {R5,R8} | {B,C,D,E,X}      | R5
;; 4     | {R8,R1} | {A,B,C,D,E,X}    | R8
;; 5     | {R1,R6} | {A,B,C,D,E,F,X}  | R1
;; 6     | {R6}    | {A,B,C,D,E,F,H,X}| R6

;; Par chainage arrière
;; Image graph en utilisant un arbre ET_OU (ET Atomes) (OU relations)
;; Permet de poser des questions à l'utisateur pour savoir la valeur de certains atomes (afin d'éviter l'exploration de tout le sous-arbre si une condition est fausse en ET)


;; Representation des données
;; Base de faits
(setq faits '(B C))
;; Représentation des règles 
;; Il peut être intéressant d'ajouter un nom aux règles afin d'expliquer le raisonnement logique
(setq regles '(
              ((B C E) F)
              ((D G) A)
              ((C F) A)
              ((C) D)
              ((D) E)
              ((A) H)
              ((B) X)
              ((X C) A)
              )
    )
;; Il faudra créer des fonctions de services permettant de rendre indépendant la représentation des données avec leur traitement
;; Il suffira alors de modifier la fonction de service pour l'adapter à une autre représentation de données



;; Algorithmes

;; fonctions élémentaires :
;; primisse (r) retourne premisses de r;
;; conclusion (r) retourne la conclusion;
;; question (f) pose la question sur f à l'utilisateur et retourne T si fait est vrai, NIL sinon
;; vrai (f) retourne T si f est vrai, NIL sinon
;; regles_candidates (f) retourne la liste des regles candidates pour que f soit T


(defun premisse (r)
  (car r))

(defun conclusion (r)
  (car (cdr r)))

(defun question (f)
  (format t "~%Quelle est la valeur de ~A ?~%" f)
  (let* ((var (read)))
    (if (and var (not (equal var 0))) T NIL) ;; Retourne vrai si var et var!=0 ;; NIL si var=0 ou var=NIL
    )
  )

(defun vrai (f)
  (if (member f faits) ;; faits est variables globale
      T
    NIL
    )
  )

(defun regles_candidates (f)
  (let ((candidats NIL))
    (dolist (r regles)
      (if (eq (conclusion r) f) ;; Si a conclusion de la règle est bien le fait que l'on veut atteindre
          (setq candidats (append candidats (list r))) ;; Alors on l'ajoute à la liste des candidats
        )
      )
    candidats
    )
  )   
      
(defun verifier (but)
   (format t "~%     verifier : ~A" but) ;; affichage pour suivre le parcours de l'arbre
  (let ((ok NIL)(ec NIL))
    (if (vrai but)  ;; on vérifie si le but n'est pas déjà dans la base de faits
        (setq ok T)
      (progn
        (setq ec (append (regles_candidates but) ec))  ;; !!! append ne modifie pas la valeur de la liste, elle retourne uniquement la liste modifiée
        (dolist (r ec)   ;; Pour chaque r de EC 
          (setq ok (verifier_ET r))  ;; OK <- verifier_ET (r)
        ;; Fin pour
          (when (equal ok T) ;; et tq Ok = faux ;; quand une règle est vérifiée, pas besoin de checker les autres
            (return-from NIL)
            )
          )

        (if (not ok)     ;; si le but n'est pas atteignable par des règles vérifiées ou déjà existant dans la BF, on demande à l'utilisateur explicitement sa valeur
            (setq ok (question but)))
        )
      )
    ok
    )
  )


(defun verifier_ET (r)
  (format t "~%     verifier_ET : ~A" r) ;; affichage pour suivre le parcours de l'arbre
  (let ((ok T)(prem (premisse r)))
    (dolist (p prem)  ;; Pour chaque p de prem
      (setq ok (verifier p))
      (when (equal ok NIL) ;; et tant que OK = T
        (return-from NIL)
        )
      )
    ok
    )
  )


(defun boucle (given-list) ;; Test pour "break" un dolist
  (dolist (e given-list)  
    (print e)
    (when (equal e 'B)      ;; permet de réduire le nombre n d'itérations
        (return-from NIL)
    )
    )
  (print "Doit etre affiche")
  )


; Pour passer en 0+ 
; plutot que de stocker des faits, il faudrait stocker les faits connus avec leur valeur.
; concernant les règles, il ne s'agira plus de l'existence ou non d'un fait, mais plutot d'un test logique de comparaison effectué sur la valeur du fait
; ainsi, vrai(f) renverra T si le test logique est vrai, faut sinon
; la question pour renseigner un fait sera posée à l'utilisateur uniquement si la valeur d'un fait n'est pas renseignée.