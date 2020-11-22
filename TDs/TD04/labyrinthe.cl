; Labyrinthe - BRANLY Stephane


; Avant propos sur la fonction format

; syntaxes sur la fonction format
; (format dest control_string args)
; dest pouvant prendre les formes :
; nul ; chaine de car
; T   ; affiche à l'écran
; "__"; ecrit dans un fichier
; control_string entre "" et pouvant contenir des expressions lisps, caracteres et caracteres spéciaux :
; ~A ; expression sous forme asthetic
; ~S ; expression sous forme standard
; ~C ; expression sous forme char
; ~% ou ~& ; new_line


; Rappel sur les variables let (locales au bloc)

; La syntaxe a preferer est la suivante
; (let (
;       (un 1)
;       (deux (car L)
;      )
;      S-expressions
; )
; let* permet d'initialiser des variables par d'autres variables definies juste avant dans le meme bloc let


; Avant-propos sur la fonction assoc

; La fonction assoc permet de retourner le tupple d'une clef contenue dans une liste
; Exemple
; (setq L ((a 1)(b 2 3)(c 4)))
; > (assoc 'b L)
; (b 2 3)


; Avant-propos sur la fonction member

; La fonction member permet de voir si une liste est contenue dans une autre liste
; (member '(a b) '((c d)(a b)(e f)) :test #'equal)
; Attention, cela renvoit la liste a partir de l'element trouve
; ici on aurait donc :
; ((a b)(e f))



; Avant propos sur la fonction push et pop
; exemple
; > (setq L '(a b c))
; > (push 'z L)
; (z a b c)

; > L
; (z a b c)

; > (pop L)
; z

; > L
; (a b c)





; Exercice

; 1) Definition formelle du probleme
;   ensemble d'etats : chaques cellules qui composent le labyrinthe {E, 1, ..., 20, S}
;   etats initiaux   : position de depart de l'automate et sortie du labyrinthe E et S
;   ensembles d'etats-solutions : ensemble des points a parcourir pour aller de depart à fin sans traverser des murs
;   ensembles d'actions : actions possibles par l'automate {haut, droite, bas, gauche} sans traverser un mur


; 2) Deduction d'une representation du labyrinthe
(setq *lab* '((E 1)(1 E 2)(2 1 7)(3 6)(4 5)(5 4 12)(6 3 7)(7 2 6 8)(8 7 9)(9 8 10)
              (10 9 15 11)(11 10 14 12)(12 5 11)(13 20)(14 11)(15 10 16)(16 17 15)
              (17 16 18)(18 17 19)(19 18 20)(20 13 19 S)(S 20)))
; On decide de creer une liste lab contenant chaque cellule 
; chaque cellule est representee par une liste, son premier element (car) est son numero de cellule
; le reste des elements (cdr) constituent les autres cellules atteignables
; 


(defun successors (n_cell lab) ; Renvoie les successeurs d'une cellule n_cell du labyrinthe lab
  (cdr (assoc n_cell lab))
    )

(defun valid_successors (n_cell lab visited) ; Renvoie les successeurs valides / non visites d'une cellule n_cell du labyrinthe lab
  (let ((valid_successors 'NIL)(succs (successors n_cell lab)))
  (dolist (x succs)
    (if (member x visited) 
        NIL
      (push x valid_successors )
      )
    )
    valid_successors
    )
  )

(defun notAtEnd (stack end)
  (not (equal end (car stack)))
  )


(defun explore_depth (lab start end visited)
  (push start visited)

  (if (notAtEnd (list start) end)
  (dolist (x (valid_successors start lab visited))
    (explore_depth lab x end visited))
    (format T "~%Answer ~A" (reverse visited))
    )
  )

(defun explore_width (candidate-list old-states)
  (print "** candidates **")
  (print candidate-list)
  (cond
   ((null candidate-list) nil)
   ((member 'S candidate-list)(return-from rech-larg1 "Succes"))
   (t (rech-larg1 (diff (flatten 
                         (mapcar (lambda (xx)(successors xx laby)) candidate-list))
                        old-states)
                  (append candidate-list old-states)))))

(defun rech-larg (etat) ;; Premier appel de rech-larg1 avec entrée en liste et NIL pour les anciens états
  (rech-larg1 (list etat) nil))

(defun diff (L M) ;; Peut etre remplacé par la fonction primitive en lisp
  (cond ((null L) nil)
        ((member (car L) M :test 'equal)(diff (cdr L) M))
        (t (cons (car L)(diff (cdr L) M))) ))

(defun flatten (L)
  (if L (append (car L)(flatten (cdr L)))))

(rech-larg 'e)
  )


(explore_depth *lab* 'E 'S NIL)
