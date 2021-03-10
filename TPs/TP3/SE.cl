;; ########### CAS D'USAGE DU SYSTEME EXPERT ###########
;; CHAINAGE-AVANT exemple 1:  
 ;; (chainage-avant)
 ;; > 4
 ;; > intellectuelle 
 ;; > reflexive
 ;; > cartes
 ;; > Nous avons trouvons une activité qui vous convient : le Bridge !


;; CHAINAGE-AVANT exemple 2:  
 ;; (chainage-avant)
 ;; > 5
 ;; > physique
 ;; > terrain
 ;; > 3
 ;; > Nous avons trouvons une activité qui vous convient : le Baseball !

;; CHAINAGE-ARRIERE:
 ;; (chainage-arriere)
 ;; > "Cyclisme"
 ;; > Physique
 ;; > EXTERIEUR
 ;; > 1
 ;; L'activite Cyclsime convient à l'utilisateur.


;; Code du SE à compiler:
(progn
  (format t "~%Opening expert system...")
  
  ;; Initialisation a NIL des différentes variables globales au SE
  (setq *RULES-SE* NIL)
  (setq *QUESTIONS-SE* NIL)
  (setq *ACTIVITIES-SE* NIL)
  (setq *RULES* NIL)
  (setq *QUESTIONS* NIL)
  (setq *ACTIVITIES* NIL)
  (setq *FACTS* NIL)
  (setq *HISTORIC* NIL)
  
  
  ;; ########### FONCTIONS EN RAPPORT AVEC LES REGLES ########### 
  
  ;; Fonction d'ajout d'une règle 
  (defun add-rule (conditions conclusion)
    (let ((id (gentemp "R")))
      (set id (list (list 'conditions conditions) (list 'conclusion conclusion)))
      (pushnew id *RULES-SE*)
      )
    )
  
  ;; Définition de fonctions d'accès
  (defun get-conditions-rule (rule)
    (cadr (assoc 'CONDITIONS rule))
    )

  (defun get-conclusion-rule (rule)
    (cadr (assoc 'CONCLUSION rule))
    )

  ;; Fonction de désactivation d'une règle
  (defun desactive-rule (rule)
    (setf *RULES* (delete-if #'(lambda (item) (eq (symbol-value item) rule)) *RULES*))
    )
    
  ;; Fonction d'exécution d'une règle
  (defun execute-rule (rule)
    (let ((conclusion (get-conclusion-rule rule)))
      ;; (pushnew (symbol rule) *HISTORIC*)
      (pushnew conclusion *FACTS*)
      )
    )
  
  
  ;; Fonction de vérification si une règle peut-être enclenchée, ne peut plus être enclenchée, ou doit attendre un prochain état
  (defun check-rule (rule)
      (let ((to-delete 0)(execute 1))
        (dolist (condition (get-conditions-rule rule)) ;; Pour chaque condition ET de la regle
          (if (is-fact-defined  (car condition)) ;; On regarde si la variable est definie
              (progn 
                (setq execute (* (check-condition condition) execute)) ;; On check la du test
                (if (= (check-condition condition) 0) (setq to-delete 1))
                )
              (setq execute 0)    
              )
          )
        (if (= execute 1) (progn (execute-rule rule)(desactive-rule rule))) ;; On execute eventuellement la regle si toutes les conditions sont valides
        (if (= to-delete 1)(desactive-rule rule)) ;; On supprime la regle si au moins un test est 0, ou si tous les tests sont valides
        execute
        )
    )
  
  ;; Fonction pour vérifier toutes les règles avec la base de fait
  (defun check-rules ()
    (let ((executed NIL))
    (dolist (rule *rules* T) ;; Pour chaque regle
      (if (= (check-rule (symbol-value rule)) 1) (setq executed T)) ;; On verifie la regle
      )
      executed
      )
    )
  
  ;; Fonction pour vérifier si un fait est défini (pour par la suite prendre sa valeur)
  (defun is-fact-defined (fact)
    (if (member fact *FACTS*) T NIL)
    )
  
  ;; Fonction qui effectue le test condition (variable operateur valeur) -> retourne le test
  (defun check-condition (condition)
    (if (funcall (cadr condition) (symbol-value (car condition)) (caddr condition)) 1 0)
    )
   
  
  
  ;; ########### FONCTIONS EN RAPPORT AVEC LES QUESTIONS ########### 
  
  ;; Fonction d'ajout de fonction 
   (defun add-question (question response)
    (let ((id (gentemp "Q")))
      (set id (list (list 'question question) (list 'response response)))
      (pushnew id *QUESTIONS-SE*)
      )
    )
  
  ;; Définition de fonctions d'accès
  (defun get-question (question)
    (cadr (assoc 'QUESTION question))
    )
   (defun get-response-var (question)
    (cadr (assoc 'RESPONSE question))
    )
  
  ;; Fonction qui pose une question
  (defun ask-question (question)
    (format t "~%~S~%> " (get-question question)) ;; Pose la question
    (clear-input)
    (set (get-response-var question) (read)) ;; Lis la reponse
    (pushnew (get-response-var question) *facts*) ;; Enregistre la reponse
    (setf *QUESTIONS* (delete-if #'(lambda (item) (eq (symbol-value item) question)) *QUESTIONS*)) ;; Delete la question posee
    )
  
  
  
   ;; ########### FONCTIONS EN RAPPORT AVEC LES ACTIVITES ########### 
  
  ;; Fonction d'ajout d'une activite
  (defun add-activity (conditions activity description)
    (let ((id (gentemp "A")))
      (set id (list (list 'conditions conditions) (list 'activity activity)(list 'description description)))
      (pushnew id *ACTIVITIES-SE*)
      )
    )
  
  ;; Fonction d'acces
  (defun get-conditions-activity (activity)
    (cadr (assoc 'conditions activity))
    )
  (defun get-activity-activity (activity)
    (cadr (assoc 'activity activity))
    )
  (defun get-description-activity (activity)
    (cadr (assoc 'description activity))
    )
  
  ;; Fonction pour trouver retrouver une activité par identifiant demandé
  (defun ask-activity ()
    (let ((act NIL)(returnID NIL))
      (format t "~%Indiquez le nom de l'activité dont vous voulez vérifier si elle vous convient~%> ") ;; Pose la question pour savoir l'activité
      (clear-input)
     (setq act (read))
     (dolist (activity *ACTIVITIES*) ;; Cherche l'activité dans la base d'activités
       (if (equal (get-activity-activity (symbol-value activity)) act) (setq returnID activity))
       )
     returnID
     )
   )
  
  ;; Fonction de désactivation d'une activite
  (defun desactive-activity (activity)
    (setf *ACTIVITIES* (delete-if #'(lambda (item) (eq (symbol-value item) activity)) *ACTIVITIES*))
    )
  
  ;; Fonction pour vérifier si un fait est toujours atteignable A -> A existant en conclusion d'une règle encore active
  (defun is-fact-possible (fact)
    (let ((possible NIL))
      (dolist (rule *rules*) ;; Pour chaque regle
        (if (equal (get-conclusion-rule (symbol-value rule)) fact) (setq possible T)) ;; On regarde si la conclusion correspond au fait recherche
        )
      possible
      )
    )
  
  ;; Fonction pour vérifier si une condition est toujours atteignable '(A B C) -> A OU B OU C
  (defun is-condition-possible (condition)
    (let ((possible NIL))
      (dolist (fact condition) ;; Pour chaque fait, on regarde s'il est encore atteignable (il en faudra au moins 1)
        (if (is-fact-possible fact) (setq possible T))
        )
      possible
      )
    )
  
  ;; Fonction pour vérifier si un fait est approuvée
  (defun is-fact-approved (fact)
    (if (member fact *facts*) T NIL) ;; Fait boolean valide correspond a sa presence dans la base de faits
    )
  
  ;; Fonction pour vérifier si une condition est approuvée
  (defun is-condition-approved (condition)
    (let ((approved NIL))
      (dolist (fact condition) ;; Pour chaque condition OU, on verifie si c'est valide
        (if (is-fact-approved fact) (setq approved T))
        )
      approved
      )
    )
  
  
  ;; Fonction pour vérifier si activité est possible
  (defun check-activity (activity)
    (let ((possible T)(to-delete NIL))
      (dolist (condition (get-conditions-activity activity)) ;; Pour chaque conditions ET des activites
        (if (not (is-condition-approved condition)) ;; On verifie si la condition est valide
            (progn 
              (setq possible NIL)
              (if (not (is-condition-possible condition)) ;; On verifie si elle est encore possible (pour etre validee), si elle ne peut pas etre validee, on la supprime
                  (setq to-delete T)
                )
              )
          )
        )
      (if to-delete (desactive-activity activity)) 
      possible
      )
    )
  
  (defun check-activities ()
    (let ((act NIL))
      (dolist (activity *activities*) ;; Pour chaque activite
        (if (check-activity (symbol-value activity)) (setq act activity)) ;; On la "met a jour" (supprime si elle ne pourra jamais etre atteinte)
        )
      (if (= (length *activities*) 1) (setq act (car *activities*))) ;; S'il ne reste qu'une activite, on la renvoit
      act
    )
    )
  
  ;; Fonction permettant de trouver la prochaine question à poser
  (defun ask-better-question ()
    (let ((questions NIL)(param NIL)(best-score 0)(question-to-ask NIL))
      (dolist (activity *activities*) ;; On parcours toutes les activites
        (dolist (condition (get-conditions-activity (symbol-value activity))) ;; Pour chaque condition ET des activites 
          (dolist (fact condition) ;; Pour chaque condition OU des activites
            (setq questions (increment-var-to-fact fact questions)) ;; On incremente le compteur question permettant de clarifier le fait de la condition
            )
          )
        )
      (dolist (q questions) ;; On cherche la question avec le meilleur score
        (if (>= (cadr q) best-score)
            (progn
              (setq best-score (cadr q))
              (setq question-to-ask (car q))
              )
        )
        )
      (if question-to-ask (progn (ask-question (symbol-value question-to-ask)) T) NIL) ;; On pose la question si elle existe et on retourne T, NIL sinon
      )
    )
  
  ;; Fonction qui incrémente les variables questions nécessaires pour activer une règle menant au but
  (defun increment-var-to-fact (fact variables)
    (let ((variables-r variables)) 
    (dolist (rule *rules*) ;; Pour toutes les regles
      (if (eq (get-conclusion-rule (symbol-value rule)) fact) ;; Si la conclusion de la regle mene au fait que l'on cherche
          (progn 
            (dolist (condition (get-conditions-rule (symbol-value rule))) ;; Alors, pour toutes les conditions de cette regle, on incremente la question relative a la condition
              (setq variables-r (increment-question-priority variables-r (car condition)))
              )
            )
        )
      )
      variables-r
      )
    )
  
  ;; Fonction qui cherche la question a incrémenter
  (defun increment-question-priority (variables param)
    (let ((variables-r variables)(tmp NIL))
      (dolist (question *questions*) ;; Pour toutes les questions
        (if (eq (get-response-var (symbol-value question)) param) ;; Si la variable de stockage de la reponse et identique a celle du parametre de la condition
            (if (assoc question variables-r)
                (progn    ;; On incremente le compteur associe a la question s'il existe
                  (setf tmp (cadr (assoc question variables-r)))
                  (setf (cadr (assoc question variables-r)) (+ tmp 1))
                 )
              (pushnew (list question 1) variables-r) ;; Ou on l'initialise a 1
              )
          )
        )
      variables-r
      )
    )
  
  
  (format t "~%Lancez le (chainage-avant) ou (chainage-arriere) pour essayer le SE ~%")
  
  ;; ########### BOUCLE PRINCIPALE CHAINAGE AVANT ########### 
  (defun chainage-avant ()
    (setf *RULES* (copy-list *RULES-SE*))
    (setf *FACTS* NIL)
    (setf *QUESTIONS* (copy-list *QUESTIONS-SE*))
    (setf *ACTIVITIES*  (copy-list *ACTIVITIES-SE*))
    (let (
          (end NIL)
          (activity NIL)
          )
      (loop
        (if (not (ask-better-question)) (setq end T) ;; S'il n'y a plus de questions a poser, on met end a vrai
          (progn 
            (loop ;; Tant que des règles sont activées, on regarde si on peut activer de nouvelles règles
             (when (not (check-rules)) (return T))
              )
            (setq activity (check-activities)) ;; On regarde si une activité match avec les faits
            (if activity (setq end T)) ;; Si oui, on met fin a vrai
            )
          )   
         (when end (return activity)) ;; On quitte quitte la boucle quand fin est vrai, sinon on repete les etapes
        )
       (if activity  ;; Affichage de l'activite si elle est existante
           (progn (format t "~%~%###################################~%~%Nous avons trouvé une activité qui pourrait vous convenir !")
             (format t "~%Il s'agit de l'activité ~S" (get-activity-activity (symbol-value activity)))
             (format t "~%~S" (get-description-activity (symbol-value activity)))
             )
         (format t "~%~%###################################~%~%Nous n'avons malheureusement pas trouvé d'activité pour vous...~%(les activites les plus proches de vos envies se situent dans la liste *ACTIVITIES*)")
         )
      )
    (format t "~%~%~%Lancez à nouveau (chainage-avant) ou (chainage-arriere) pour re-essayer le SE ~%")
    )
  
  ;; ########### BOUCLE PRINCIPALE CHAINAGE ARRIERE ########### 
  ;; Indiquez le nom exact de l'activité : exemple> "Badminton"
  (defun chainage-arriere ()
    (setf *RULES* (copy-list *RULES-SE*))
    (setf *FACTS* NIL)
    (setf *QUESTIONS* (copy-list *QUESTIONS-SE*))
    (setf *ACTIVITIES*  (copy-list *ACTIVITIES-SE*))
    (let (
          (end NIL)
          (activityA NIL)
          )
      (setq *ACTIVITIES* (list (ask-activity)))
      (loop
        (if (not (ask-better-question)) (setq end T) ;; S'il n'y a plus de questions a poser, on met end a vrai
          (progn 
            (loop ;; Tant que des règles sont activées, on regarde si on peut activer de nouvelles règles
             (when (not (check-rules)) (return T))
              )
            (setq activity (check-activities)) ;; On regarde si une activité match avec les faits
            )
          )   
         (when end (return activity)) ;; On quitte quitte la boucle quand fin est vrai, sinon on repete les etapes
        )
       (if activity  ;; Affichage de l'activite si elle est existante
           (format t "~%~%###################################~%~%L'activité que vous avez indiqué semble vous convenir !")
         (format t "~%~%###################################~%~%L'activité ne vous convient malheureusement pas...")
         )
      )
    (format t "~%~%~%Lancez à nouveau (chainage-avant) ou (chainage-arriere) pour re-essayer le SE ~%")
    )

  
  ;; Ajout des règles 
  (format t "~%Generating rules...")
  (add-rule '((type_activite eq intellectuelle)) 'intellectuelle)
  (add-rule '((type_activite eq physique)) 'physique)
  (add-rule '((type_intellectuelle eq creative)) 'creative)
  (add-rule '((type_intellectuelle eq culturelle)) 'culturelle)
  (add-rule '((type_intellectuelle eq reflexive)) 'reflexive)
  (add-rule '((type_intellectuelle eq detente)) 'detente)
  (add-rule '((art eq visuel)) 'visuel)
  (add-rule '((art eq musical)) 'musical)
  (add-rule '((art eq litteraire)) 'litteraire)
  (add-rule '((reflexion eq plateau)) 'plateau)
  (add-rule '((reflexion eq cartes)) 'cartes)
  (add-rule '((reflexion eq informatique)) 'informatique)
  (add-rule '((personnes = 1)) 'seul)
  (add-rule '((personnes = 2)) 'duo)
  (add-rule '((personnes = 3)) 'trio)
  (add-rule '((personnes = 4)) 'quatuor)
  (add-rule '((personnes > 4)(personnes < 9)) 'petit_groupe)
  (add-rule '((personnes > 8)) 'grand_groupe)
  (add-rule '((type_info eq software)) 'software)
  (add-rule '((type_info eq hardware)) 'hardware)
  (add-rule '((type_art_visuel eq numerique)) 'numerique)
  (add-rule '((type_art_visuel eq manuel)) 'manuel)
  (add-rule '((heure <= 2)) 'duree_courte)
  (add-rule '((heure > 2)(heure <= 4)) 'duree_moyenne)
  (add-rule '((heure > 4)) 'duree_longue)
  (add-rule '((lieu_culture eq domicile)) 'chez_sois)
  (add-rule '((lieu_culture eq exterieur)) 'exterieur)
  (add-rule '((budget = 0)) 'gratuit)
  (add-rule '((budget > 0)(budget <= 10)) 'petit_budget)
  (add-rule '((budget > 10)(budget <= 100)) 'moyen_budget)
  (add-rule '((budget > 100)) 'grand_budget)
  (add-rule '((proches_activite eq famille)) 'famille)
  (add-rule '((proches_activite eq amis)) 'amis)
  (add-rule '((type_detente eq societe)) 'jeux_societe)
  (add-rule '((type_detente eq video)) 'jeux_video)
  (add-rule '((type_physique eq terrain)) 'pTerrain)
  (add-rule '((type_physique eq interieur)) 'pIntCours)
  (add-rule '((type_physique eq speciale)) 'pSpeciale)
  (add-rule '((type_physique eq exterieur)) 'pExterieur)
  (add-rule '((membre_physique eq pied)) 'se_joue_au_pied)
  (add-rule '((membre_physique eq main)) 'se_joue_a_la_main)
  (add-rule '((type_pIntCours eq cours)) 'pCours)
  (add-rule '((type_pIntCours eq interieur)) 'pInterieur)
  (add-rule '((type_pInterieure eq filet)) 'filet)
  (add-rule '((type_pInterieure eq but)) 'but)
  (add-rule '((type_pInterieure eq panier)) 'panier)
  (add-rule '((type_pSpeciale eq hivernale)) 'pHivernale)
  (add-rule '((type_pSpeciale eq aquatique)) 'pAquatique)
  (add-rule '((proche_station eq oui)) 'proche_station_ski)
  (add-rule '((proche_station eq non)) 'loin_station_ski)
  
  ;; Ajout de questions
  (format t "~%Generating questions...")
  (add-question "Quel type d'activité chechez-vous ? (intellectuelle ou physique)" 'type_activite)
  (add-question "Quel type d'activité intellectuelle chechez-vous ? (creative, culturelle, reflexive ou detente)" 'type_intellectuelle)
  (add-question "Quel art aimez-vous ? (visuel, musical ou litteraire)" 'art)
  (add-question "Quel type de reflexion aimez-vous ? (jeu de PLATEAU, jeu de CARTES, INFORMATIQUE)" 'reflexion)
  (add-question "Vous cherchez une activité pour combien de personnes ?" 'personnes)
  (add-question "Quel aspect vous attire le plus en informatique ? (software, hardware)" 'type_info)
  (add-question "Préférez-vous le NUMERIQUE ou le MANUEL ?" 'type_art_visuel)
  (add-question "Indiques le nombre d'heures que vous pouvez consacrer à l'activité (par session)" 'heure)
  (add-question "Préférez vous une activité culturelle chez vous (DOMICILE) ou à l'extérieur (EXTERIEUR) ?" 'lieu_culture)
  (add-question "Quel est votre budget par session en euros ?" 'budget)
  (add-question "Avec qui voulez-vous faire cette activité ? (FAMILLE, AMIS)" 'proches_activite)
  (add-question "Etes vous plutot jeux de SOCIETE ou jeux VIDEO ?" 'type_detente)
  (add-question "Quel type d'activité physique chechez-vous ? (sur TERRAIN, en INTERIEUR (et cours), SPECIALE (aquatique et hivernale) ou en EXTERIEUR)" 'type_physique)
  (add-question "Préférez-vous jouer au PIED ou a la MAIN ?" 'membre_physique)
  (add-question "Cherchez-vous une activité dans une COURS ou en INTERIEUR ?" 'type_pIntCours)
  (add-question "Quelle particularité préférez-vous parmis FILET, BUT, PANIER ?" 'type_pInterieure)
  (add-question "Cherchez-vous une activité dans AQUATIQUE ou HIVERNALE ?" 'type_pSpeciale)
  (add-question "Etes-vous proche d'une station de ski ? (OUI ou NON)" 'proche_station)
  
  ;; Ajout d'activités
  (format t "~%Generating activities...")
  (add-activity '((intellectuelle)(creative)(visuel)(numerique)) "Photographie" "Evadez vous, capturez et observez mieux l'environnement qui vous entoure !")
  (add-activity '((intellectuelle)(creative)(visuel)(manuel)) "Peinture" "Evadez vous, capturez et observez mieux l'environnement qui vous entoure !")
  (add-activity '((intellectuelle)(creative)(musical)) "Composition musicale" "Faites parler votre créativité dans le domaine musicale !")
  (add-activity '((intellectuelle)(creative)(litteraire)) "Ecriture" "Prennez un papier et un crayon et exprimez-vous librement !")
  (add-activity '((intellectuelle)(reflexive)(plateau)(duree_moyenne duree_longue)) "Echecs" "Réflechissez aux meilleures stratégies pour battre votre adversaire ! Les mathématiques vous aiderons.")
  (add-activity '((intellectuelle)(reflexive)(plateau)(duree_courte)) "Dames" "Jeu classique, détendez-vous tout en réflechissant aux stratégies pour prendre les pions de l'adversaire !")
  (add-activity '((intellectuelle)(reflexive)(cartes)(seul)) "Le solitaire" "Le jeu classique, idéal pour passer le temps.")
  (add-activity '((intellectuelle)(reflexive)(cartes)(duo)) "Manille découverte" "Peu connu mais interessant, une variante du jeu a 4 ou 6 joueurs.")
  (add-activity '((intellectuelle)(reflexive)(cartes)(trio)) "Rummy" "Idéal pour passer le temps a trois")
  (add-activity '((intellectuelle)(reflexive)(cartes)(quatuor)) "Bridge" "Réflechissez bien avant de prononcer vos contrats, ayez une bonne cohésion avec votre pole inverse.")
  (add-activity '((intellectuelle)(reflexive)(cartes)(petit_groupe grand_groupe)) "Président" "Un jeu qui s'apprend rapidement et dont on ne se lasse jamais.")
  (add-activity '((intellectuelle)(reflexive)(informatique)(software)) "Programmation" "Réflechissez aux meilleurs algos, optimisez vos programmes, découvrez le monde de l'informatique grace aux nombreuses ressources.")
  (add-activity '((intellectuelle)(reflexive)(informatique)(hardware)) "Robotique" "Quoi de mieux que de voir ses créations se mouvoir ! Fabriquez votre Frankeinstein de métal !")
  (add-activity '((intellectuelle)(culturelle)(chez_sois)(visuel)) "Documentaire" "Youtube et d'autres plateformes sont riches en documentaires ! De nombreux sujets disponibles et des documentaires de 5minutes à plus de 2heurex ! Découvrez le monde qui vous entoure !")
  (add-activity '((intellectuelle)(culturelle)(chez_sois)(musical)) "Musique" "Ecoutez les messages transmis dans les paroles, voyagez par ces styles musicaux si variés !")
  (add-activity '((intellectuelle)(culturelle)(chez_sois)(litteraire)) "Lecture" "Plongez dans un livre et voyagez avec l'auteur et vous même !")
  (add-activity '((intellectuelle)(culturelle)(exterieur)(gratuit)) "Musee" "Découvrez les meilleurs pièces artistiques, des cultures, des styles, des représentations uniques!")
  (add-activity '((intellectuelle)(culturelle)(exterieur)(petit_budget)) "Cinema" "Regardez de véritables chefs-d'oeuvre sur grand écran avec un son de qualité, plongez dans les histoires !")
  (add-activity '((intellectuelle)(culturelle)(exterieur)(moyen_budget grand_budget)) "Theatre" "Emerveillez-vous devant de magnifiques représentations !")
  (add-activity '((intellectuelle)(detente)(amis)(jeux_societe)) "Jeux de société humouristique" "Passez de bons moments avec vos amis avec des jeux du style Limite-Limite")
  (add-activity '((intellectuelle)(detente)(famille)(jeux_societe)) "Jeux de société famillaux" "Passez de bons moments avec votre famille avec des jeux du style Monopoly")
  (add-activity '((intellectuelle)(detente)(amis)(jeux_video)) "Jeux video sur console de salon (PS, Xbox)" "Passez des bons moments avec vos amis sur des jeux de console comme The Last of Us, Sackboy, Forza !")
  (add-activity '((intellectuelle)(detente)(famille)(jeux_video)) "Jeux video sur console famillale" "Passez des bons moments avec votre famille sur des jeux comme Mariokart !")
  (add-activity '((physique)(pTerrain)(duree_courte)(se_joue_au_pied)(petit_groupe grand_groupe)) "Football" "Défoulez-vous avec une bonne partie de football !")
  (add-activity '((physique)(pTerrain)(duree_courte)(se_joue_a_la_main)(petit_groupe grand_groupe)) "Rugby" "Défoulez-vous avec une bonne partie de rugby !")
  (add-activity '((physique)(pTerrain)(duree_moyenne)(petit_groupe grand_groupe)) "Baseball" "Défoulez-vous avec une bonne partie de baseball !")
  (add-activity '((physique)(pTerrain)(duree_longue)(petit_groupe grand_groupe)) "Cricket" "Défoulez-vous avec une bonne partie de cricket !")
  (add-activity '((physique)(pIntCours)(pInterieur)(seul trio)) "Squash" "Défoulez-vous avec une bonne partie de squash !")
  (add-activity '((physique)(pIntCours)(pInterieur)(duo quatuor)) "Badminton" "Défoulez-vous avec une bonne partie de badminton !")
  (add-activity '((physique)(pIntCours)(pInterieur)(petit_groupe grand_groupe)(filet)) "Volleyball" "Défoulez-vous avec une bonne partie de volleyball !")
  (add-activity '((physique)(pIntCours)(pInterieur)(petit_groupe grand_groupe)(but)) "Handball" "Défoulez-vous avec une bonne partie de handball !")
  (add-activity '((physique)(pIntCours)(pInterieur)(petit_groupe grand_groupe)(panier)) "Basketball" "Défoulez-vous avec une bonne partie de basketball !")
  (add-activity '((physique)(pIntCours)(pCours)) "Tennis" "Défoulez-vous avec une bonne partie de tennis !")
  (add-activity '((physique)(pSpeciale)(pAquatique)(seul)) "Natation" "Faites-vous plaisir en nageant en piscine, mer, rivière, lac !")
  (add-activity '((physique)(pSpeciale)(pAquatique)(duo trio quatuor)) "Plongée / apnée" "Découvrez-la plongée et l'apnée à plusieurs !")
  (add-activity '((physique)(pSpeciale)(pAquatique)(petit_groupe grand_groupe)) "Waterpolo" "Défoulez-vous avec une bonne partie de waterpolo !")
  (add-activity '((physique)(pSpeciale)(pHivernale)(proche_station_ski)) "Ski" "Descendez les pistes à toute vitesse et profitez de la vue !")
  (add-activity '((physique)(pSpeciale)(pHivernale)(loin_station_ski)) "Patinnage" "Dévouvrez la sensation unique de glisse !")
  (add-activity '((physique)(pExterieur)(seul)) "Cyclisme" "Dévorez le bitume lors de sorties plus ou moins longues ! Choisissez la route ou le tout terrain ! Ce n'est pas le vélo qui fait mais l'homme, alors en selle !")
  (add-activity '((physique)(pExterieur)(duo)) "Equitation" "Faites des randonnées a cheval, prennez soin de vos animaux, entrainez vous a la carriere !")
  (add-activity '((physique)(pExterieur)(trio)) "Course a pied" "Défoulez vous et dévorez les kilomètres avec vos proches !")
  (add-activity '((physique)(pExterieur)(quatuor)) "Petanque" "En équipe ou chacun pour sois, rapprochez-vous le plus du cochonnet et dégommez les adversaires !")
  (add-activity '((physique)(pExterieur)(petit_groupe grand_groupe)) "Airsoft" "Trouvez votre style de jeu, courrez, campez, 'réanimez' vos alliés ! Mais n'oubliez pas qu'il s'agit d'une simulation et d'un jeu.")
  T
  )

