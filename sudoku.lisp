(defun print-stars ()
  "Affiche une ligne d'étoile"
  (format t "**************************** ~%"))

(defun print-en-tete ()
  "Affiche l'en-tete de la grille"
  (format t "   | A B C | D E F | G H I | ~%"))

(defun creer-grille ()
  "Cree une grille 9x9 vide"
  (make-array '(9 9)))

(defun creer-grille-facile ()
  "Cree une grille prédéfini de difficulté facile"
  (make-array '(9 9)
	      :initial-contents
	      '((0 0 0 4 0 0 8 7 0)
		(0 4 7 0 9 2 0 5 0)
		(2 0 0 6 0 0 0 3 0)
		(9 7 0 5 0 0 2 0 3)
		(5 0 8 0 2 4 7 0 6)
		(6 0 4 0 0 7 0 8 5)
		(0 9 0 3 0 8 0 0 7)
		(0 0 3 2 4 0 1 6 0)
		(0 1 2 0 0 0 0 9 0))))

(defun creer-grille-difficile ()
  (make-array '(9 9)
	      :initial-contents
	      '((1 0 0 0 0 4 0 0 5)
		(0 0 0 9 5 0 0 8 0)
		(0 0 0 0 0 3 0 9 0)
		(0 0 5 0 0 2 0 0 4)
		(0 0 1 0 6 0 7 0 0)
		(7 0 0 3 0 0 2 0 0)
		(0 6 0 5 0 0 0 0 0)
		(0 8 0 0 1 6 0 0 0)
		(5 0 0 2 0 0 0 0 7))))

(defun acces-case (grille ligne colonne)
  "Affiche la valeur de la case dans GRILLE correspondant aux coordonnees LIGNE et COLONNE"
  (aref grille ligne colonne))

(defun print-case (grille ligne colonne)
  "Affiche le contenu de la case dans GRILLE aux coordonnees LIGNE et COLONNE. Laisse un blanc si la valeur est 0"
  (if (zerop (acces-case grille ligne colonne))
      (format t "  ")
      (format t " ~D" (acces-case grille ligne colonne))))

(defun print-ligne (grille ligne)
  "Affiche la ligne de la GRILLE au numero LIGNE"
  (format t " ~D" ligne)
  (dotimes (colonne 9)
    (if (zerop (mod colonne 3))
	(format t " |")
	())
    (print-case grille ligne colonne))
  (format t " |~%"))

(defun print-grille (grille)
  "Affiche la GRILLE du sudoku"
  (print-en-tete)
  (dotimes (ligne 9)
    (if (zerop (mod ligne 3))
	(print-stars)
	())
    (print-ligne grille ligne))
  (print-stars))

(defun nombre-element-restant (grille)
  "Retourne le nombre d'element a trouver dans la GRILLE"
  (let ((nb 0))
  (dotimes (colonne 9 nb)
    (dotimes (ligne 9)
      (if (zerop (acces-case grille ligne colonne))
	  (incf nb))))))

(defun placer-element (grille ligne colonne nombre)
  "Place dans la GRILLE aux coordonnes LIGNE et COLONNE le NOMBRE"
  (setf (aref grille ligne colonne) nombre))

(defun liste-ligne (grille ligne)
  "Retourne une liste composee de la ligne de la GRILLE correpondant au numero LIGNE"
  (let ((l '()))
    (dotimes (colonne 9 l)
      (setf l (append l (list (acces-case grille ligne colonne)))))))

(defun liste-colonne (grille colonne)
  "Retourne une liste composee de la colonne de la GRILLE correspondant au numero COLONNE"
  (let ((l '()))
    (dotimes (ligne 9 l)
      (setf l (append l (list (acces-case grille ligne colonne)))))))

(defun coordonnee-grande-case (ligne colonne)
  "Retourne les coordonnes de la grande case dans laquelle se trouve la petite case aux coordonnes LIGNE et COLONNE"
  (list (truncate ligne 3) (truncate colonne 3)))

(defun liste-grande-case (grille ligne colonne)
  "Retoune une liste composee des elements se trouvant dans la GRILLE dans la grande case aux coordonnes LIGNE et COLONNE"
  (let ((l '()))
    (loop for i from (* 3 ligne) to (+ 2 (* 3 ligne))
     do (loop for j from (* 3 colonne) to (+ 2 (* 3 colonne))
	   do(setf l (append l (list (acces-case grille i j))))))
    (append '() l)))

(defun verif-possible (grille ligne colonne nombre)
  "Retourne vrai si, dans GRILLE, aux coordonnes LIGNE et COLONNE, le NOMBRE peut etre place. Retourne NIL sinon"
  (let ((coord '()))
    (setf coord (coordonnee-grande-case ligne colonne))
    ;(format t "Coord-grande-case ~D% ~%" (coordonnee-grande-case ligne colonne))
    (not (or (member nombre (liste-ligne grille ligne)) (member nombre (liste-colonne grille colonne)) (member nombre (liste-grande-case grille (first coord) (second coord)))))))
    ;(format t "Verif ~D% ~D% ~D% ~%" (liste-ligne grille ligne) (liste-colonne grille colonne) (liste-grande-case grille (first coord) (second coord)))))
  

(defun converti-en-colonne (lettre)
  "Retourne l'entier correspondant a la LETTRE de la colonne"
  (cond ((equal lettre "A") 0)
	((equal lettre "B") 1)
	((equal lettre "C") 2)
	((equal lettre "D") 3)
	((equal lettre "E") 4)
	((equal lettre "F") 5)
	((equal lettre "G") 6)
	((equal lettre "H") 7)
	((equal lettre "I") 8)))


(defun sudoku (grille)
  "Fonction principale qui lance une partie de Sudoku à partir de la GRILLE"
  (let ((temporaire NIL) (ligne NIL) (colonne NIL) (nombre NIL) (grid-base (copy-grid grille)) (exit NIL))
    (loop
       (if (zerop (nombre-element-restant grille))
	   (progn
	    (print-grille grille)
	    (return 'Fini)))
       (print-grille grille)
       (format t "~D ~%" (nombre-element-restant grille))
       (setf temporaire (demande-coordonnees))

       (setf exit (loop
		     while (equal temporaire "")
		     do (if (exit-req temporaire)
			    (return T)
			    (setf temporaire (demande-coordonnees)))))

       (if exit
	   (return))
	    
       (ignore-errors (setf colonne (string-upcase (subseq temporaire 0 1))))
       (ignore-errors (setf ligne (subseq temporaire 2 3)))
       (setf temporaire (demande-valeur))

       (setf exit (loop
		     while (equal temporaire "")
		     do (if (exit-req temporaire)
			    (return T)
			    (setf temporaire (demande-valeur)))))
       (if exit
	   (return))
     
       (ignore-errors (setf nombre temporaire))	     
       (if (test-entree-utilisateur ligne colonne nombre)
	   (progn
	     (setf colonne (converti-en-colonne colonne))
	     (setf ligne (parse-integer ligne))
	     (setf nombre (parse-integer nombre))
	     (if (or (and (verif-possible grille ligne colonne nombre) (zerop (acces-case grid-base ligne colonne)))
		     (and (zerop nombre) (zerop (acces-case grid-base ligne colonne))))
		 (placer-element grille ligne colonne nombre)
		 (format t "Cette valeur ne peut pas être placé à cet endroit~%")))
	   (format t "Veuillez recommencer ~%")))))

(defun exit-req (req )
  (if (equal req "")
      (let ((c ""))
	(progn
	  (format t "Etes vous sur de vouloir quitter la partie? <O/n> ")
	  (loop
	     (setf c (read-line))
	     (if (equalp c "o")
		 (return T))
	     (if (equalp c "n")
		 (return))
	     (format t "appuiyer sur <O/n> "))))))

       

(defun test-entree-utilisateur (ligne colonne nombre)
  "Retourne T si la LIGNE, la COLONNE et le NOMBRE entrée par l'utilisateur sont interpretables, retourne NIL et la raison de l'erreur sinon"

  (if (and (stringp ligne) (stringp colonne) (stringp nombre))
      (if (numberp (digit-char-p (char ligne 0) 10))
	  (if (and (>= (parse-integer ligne) 0) (<= (parse-integer ligne) 8))
	      (if (converti-en-colonne colonne)
		  (if (numberp (digit-char-p (char nombre 0) 10))
		      (if (and (>= (parse-integer nombre) 0) (<= (parse-integer nombre) 9))
			  T
			  (progn
			    (format t "ERREUR : la valeur entree n'est pas comprise entre 1 et 9 ~%")
			    NIL))
		      (progn
			(format t "ERREUR : la valeur entree n'est pas un nombre")
			NIL))
		  (progn
		    (format t "ERREUR : la colonne entree ne correspond pas à une colonne de la grille~%")
		    NIL))
	      (progn
		(format t "ERREUR : La ligne entrée n'est pas comprise entre 0 et 8 ~%")
		NIL))
	  (progn
	    (format t "ERREUR : La ligne entrée n'est pas un nombre ~D ~%" ligne)
	    NIL))
      NIL))


(defun demande-coordonnees ()
  (format t "C L ?")
  (read-line))	
	  
	   
       

(defun demande-valeur ()
  (format t "Valeur ?")
  (read-line))


(defun copy-grid (grid)
  (let ( (new-grid (creer-grille)))
    
    (dotimes (i (array-total-size grid))
      
      (setf (row-major-aref new-grid i)
            (row-major-aref grid i)))
    new-grid))

(defun coup-possible (grille ligne colonne)
  "Renvoie une liste des valeurs possible dans la GRILLE, aux coordonnees LIGNE et COLONNE"
  (let ((liste '()))
    (setf liste (union liste (liste-ligne grille ligne)))
    (setf liste (union liste (liste-colonne grille colonne)))
    (setf liste (union liste (liste-grande-case grille (first (coordonnee-grande-case ligne colonne )) (second (coordonnee-grande-case ligne colonne)))))
    (set-difference '(0 1 2 3 4 5 6 7 8 9) liste)))

(defun essaie-coup (grille ligne colonne)
  "Place une valeur dans la GRILLE aux coordonnes LIGNE et COLONNE si c'est le seul possible, retourne T si un coup est joue, retourne NIL sinon"
  (let ((coup-pos NIL))
    (if (not (second (coup-possible grille ligne colonne)))
	(progn
	  (setf coup-pos (first (coup-possible grille ligne colonne)))
	  (placer-element grille ligne colonne coup-pos)
	  ))
    coup-pos))
  

(defvar *grille-a-resoudre* NIL)

(defun init-standalone (grid)
   (setq *grille-a-resoudre* grid))


(defun main-standalone ( )
  (let ((retL NIL) (lig NIL) (col NIL) (chifre NIL))
    
    (dotimes (ligne 9)
      (if (not (equal retL NIL))
	    (return))
      (dotimes (colonne 9)
	(if (zerop (aref *grille-a-resoudre* ligne colonne))
	    (progn
	      (setf chifre (essaie-coup *grille-a-resoudre* ligne colonne))
	      (if (not (equal chifre NIL))
		  (progn
		    (setf retL (list (list ligne colonne) chifre))
		    (setf lig ligne)
		    (setf col colonne)
		    (return)))))))
    (if (not (equal retL NIL))
	(values lig col chifre)
	NIL)))
      
    

