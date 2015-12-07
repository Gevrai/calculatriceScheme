#! /usr/bin/env gsi -:dR

;;; Fichier : tp2.scm

;;; Ce programme est une version incomplete du TP2.  Vous devez uniquement
;;; changer et ajouter du code dans la première section.

;;;----------------------------------------------------------------------------

;;; Vous devez modifier cette section.  La fonction "traiter" doit
;;; être définie, et vous pouvez ajouter des définitions de fonction
;;; afin de bien décomposer le traitement à faire en petites
;;; fonctions.  Il faut vous limiter au sous-ensemble *fonctionnel* de
;;; Scheme dans votre codage (donc n'utilisez pas set!, set-car!,
;;; begin, etc).

;;; La fonction traiter reçoit en paramètre une liste de caractères
;;; contenant la requête lue et le dictionnaire des variables sous
;;; forme d'une liste d'association.  La fonction retourne
;;; une paire contenant la liste de caractères qui sera imprimée comme
;;; résultat de l'expression entrée et le nouveau dictionnaire.  Vos
;;; fonctions ne doivent pas faire d'affichage car c'est la fonction
;;; "repl" qui se charge de cela.

; Liste association un charactère d'opérateur à sa fonction, facilite l'implémentation de nouveaux opérateurs.
(define permittedOperations (list (list #\+ +) (list #\- -) (list #\* *)))

;Gestion des erreurs
(define throwError
  (lambda (errTuple)
    (let ((errType (car errTuple)) (errChar (cdr errTuple)))
      (cond
        ((equal? errType 'ERROR_NOT_ENOUGH_ARGS)
          "Erreur de syntaxe: Pas assez d'arguments pour le nombre d'operateurs")
        ((equal? errType 'ERROR_TOO_MANY_ARGS)
          "Erreur de syntaxe: Pas assez d'operateurs pour le nombre d'arguments")
        ((equal? errType 'ERROR_VAR_ACCESS)
          (string-append "Erreur de variable: '" (string errChar) "' n'est pas une variable préalablement définie"))
        ((equal? errType 'ERROR_VAR_NAME)
          (string-append "Erreur de variable: Nom de variable '" (string errChar) "' invalide (doit etre un charactere de a à z en minuscule)"))
        ((equal? errType 'ERROR_CHAR)
          (string-append "Erreur de syntaxe: '" (string errChar) "' n'est pas un charactère valide"))
        ((equal? errType 'ERROR_NUMBER)
          (string-append "Erreur de syntaxe: Un nombre ne peut contenir que des chiffres et doit être suivi d'un espace"))
        (else
          "Erreur inconnue")))))

;Retourne la valeur d'une variable
(define getVar
  (lambda (var dict)
    (if (null? dict) 
      #f
      (if (assoc var dict)
        (cdr (assoc var dict))
        #f
        ))))
      
;Assigne un nombre "num" au nom de variable "var" et les ajoutent au dict
(define assign
  (lambda (var num dict)
    (reduceVars dict (cons(cons var num) '()))))

;Retourne un dict "propre" ou chaque il n'y a qu'une seule instance de chaque variable
(define reduceVars
  (lambda (dict newDict)
    (if (null? dict) ;if the end of dict has been reached, return.
      newDict
    (if(assoc (car(car dict)) newDict) ;if the current variable isn't in the newDict, add it.
      (reduceVars (cdr dict) newDict)
      (reduceVars (cdr dict) (cons (car dict) newDict))))))

; Recoit une liste de charactere de type "char-numeric?" et retourne le nombre associé
(define constructNumber
  (lambda(lst)
    (string->number
      (list->string 
        (reverse lst)))))

; Cherche la fonction associé au charactère "op"
(define getFunction
  (lambda(op)
    (cadr (assoc op permittedOperations))))

; Boucle principale, voir le rapport pour explications
(define tokenizer
  (lambda(expr dict stack numberConstructor)
    ; Liste vide => Fin de la ligne et du traitement!
    (if (null? expr)
      (cond
        ((not (null? numberConstructor))
          (tokenizer expr dict (cons (constructNumber numberConstructor) stack) '() ; Creer le nombre
          ))
        ((null? stack)
          (cons 'ERROR_NOT_ENOUGH_ARGS #\e))
        ((not (null? (cdr stack)))
          (cons 'ERROR_TOO_MANY_ARGS #\e))
        (else
          (cons (car stack) dict)
        ))
      ; Si on est en train de construire un nombre, les seuls charactere valides sont un chiffre ou un espace
      (if (not (or (null? numberConstructor) (char-numeric? (car expr)) (char-whitespace? (car expr))))
        (cons 'ERROR_NUMBER (car expr))
      	(cond
          ; Charactere courrant est un numeral?
      	  ((char-numeric? (car expr)) 
        		(tokenizer
              (cdr expr)
              dict
              stack
              (cons (car expr) numberConstructor)
            ))

          ; Whitespace
          ((char-whitespace? (car expr))
            (if (null? numberConstructor)
              (tokenizer (cdr expr) dict stack '())
              (tokenizer
                (cdr expr)
                dict
                (cons (constructNumber numberConstructor) stack); Creer le nombre et ajoute au stack
                '() ; Reinitialise numberConstructor
              )))

          ; Charactere est une variable?
          ((char<=? #\a (car expr) #\z)
            (if (and (or (null? (cdr expr)) (char-whitespace? (cadr expr))) (getVar (car expr) dict))
              (tokenizer
                (cdr expr)
                dict
                (cons (getVar (car expr) dict) stack)
                '()
              )
              (cons 'ERROR_VAR_ACCESS (car expr))
            ))

          ; Charactere est un operateur?
      	  ((assoc (car expr) permittedOperations)
            (if (or (null? stack) (null? (cdr stack)))
              (cons 'ERROR_NOT_ENOUGH_ARGS (car expr))
              (tokenizer
                (cdr expr)
                dict
                (cons ((getFunction (car expr)) (cadr stack) (car stack)); resultat de l'addition
                  (cddr stack)); consomme les deux nombres du stack
                '()
              )))

          ; Charactere d'affectation
          ((char=? (car expr) #\=)
            (if (and (not (null? (cdr expr))) (char<=? #\a (cadr expr) #\z)) ; Nom de variable valide?
              (if (or (null? (cddr expr)) (char-whitespace? (caddr expr)))
                (if (not (null? stack)) ; Element sur le stack?
                  (tokenizer
                    (cddr expr)
                    (assign (cadr expr) (car stack) dict)
                    stack
                    '()
                  )
                  (cons 'ERROR_NOT_ENOUGH_ARGS (car expr)))
                (cons 'ERROR_VAR_NAME (cadr expr)))
              (cons 'ERROR_VAR_NAME (cadr expr)))
            )

          ; Charactere inconnu
          (else
            (cons 'ERROR_CHAR (car expr)))
          ))
        )))

(define traiter
  (lambda (expr dict)
    (let ((resultat (tokenizer expr dict '() '() )))
      ; Tokenizer retourne soit une paire (number . dict) ou bien un symbole d'erreur, donc on fait un simple test pour
      ; definir se qu'on affiche et si on garde le nouveau dictionnaire ou l'ancien (en cas d'erreur) pour la suite.
      (if (number? (car resultat))
        (cons (append (string->list (number->string (car resultat))) (list #\newline)) (cdr resultat))
        (cons (append (string->list (throwError resultat)) (list #\newline)) dict)
    ))))

;;;----------------------------------------------------------------------------

;;; Ne pas modifier cette section.

(define repl
  (lambda (dict)
	(print "? ")
	(let ((ligne (read-line)))
	  (if (string? ligne)
		  (let ((r (traiter-ligne ligne dict)))
			(for-each write-char (car r))
			(repl (cdr r)))))))

(define traiter-ligne
  (lambda (ligne dict)
	(traiter (string->list ligne) dict)))

(define main
  (lambda ()
	(repl '()))) ;; dictionnaire initial est vide
	
;;;----------------------------------------------------------------------------
