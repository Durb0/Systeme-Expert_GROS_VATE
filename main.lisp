(load "rule.lisp")
(load "question.lisp")
(load "fact.lisp")

(defvar *questions* '())
(defvar *facts* '())

(defvar *rules*
    (list
        (make-rule 
            :name "VSAV"
            :conditions (list
                '(string= (getf *facts* :spe) "SUAP")
            )
            :actions (list
                '(set-value *facts* :vehicule "VSAV")
            )
        )
        (make-rule 
            :name "CCF4"
            :conditions (list
                '(string= (getf *facts* :spe) "INC")
                '(string= (getf *facts* :env) "FORET")
            )
            :actions (list
                '(set-value *facts* :vehicule "CCF4")
            )
        )
        (make-rule
            :name "INC ET VILLE"
            :conditions (list
                '(string= (getf *facts* :spe) "INC")
                '(string= (getf *facts* :env) "VILLE")
            )
            :actions (list
                ;crée une question
                '(setq *questions* (create-question "Le feu est-il haut ?" :haut *questions*))
            )
        )
        (make-rule 
            :name "FPT"
            :conditions (list
                '(string= (getf *facts* :spe) "INC")
                '(string= (getf *facts* :env) "VILLE")
                '(string= (getf *facts* :haut) "NON")
            )
            :actions (list
                '(set-value *facts* :vehicule "FPT")
            )
        )
        (make-rule
            :name "FPT ET MEA"
            :conditions (list
                '(string= (getf *facts* :spe) "INC")
                '(string= (getf *facts* :env) "VILLE")
                '(string= (getf *facts* :haut) "OUI")
            )
            :actions (list
                '(set-value *facts* :vehicule "FPT")
                '(set-value *facts* :mea "OUI")
            )
        )
        (make-rule
            :name "FSR"
            :conditions (list
                '(string= (getf *facts* :spe) "AVP")
            )
            :actions (list
                '(set-value *facts* :vehicule "FSR")
            )
        )
    )
)

(defun apply-step ()
        ;if there is vehicle
        (if (not (string= (get-value *facts* :vehicule) ""))
            (progn
                (format t "Fin de l'application ~%")
                (show-facts *facts*)
                (return-from apply-step)
            )
        )
        ;if there is no more questions
        (if (null *questions*)
            (progn
                (format t "Fin de l'application ~%")
                (show-facts *facts*)
                (return-from apply-step)
            )
        )
        ;get a random question
        (let ((choosen-question (get-random-question *questions*)))
            (ask-question choosen-question)
            (let ((answer (question-answer choosen-question)))
                ;answer to uppercase
                (setf answer (string-upcase answer))
                (set-value *facts* (question-type choosen-question) answer)
                (show-facts *facts*)
            )
            ;remove the question from the list
            (setq *questions* (remove choosen-question *questions*))
        )
        ;first show the rules
        (let ((matching-rules (get-matching-rules *rules* *facts*)))
            ;if there is no matching rules
            (if (null matching-rules)
                (progn
                    (format t "Aucune règle ne correspond à vos réponses ~%")
                )
            )
            ;if there is only one matching rule
            (if (= 1 (length matching-rules))
                (progn
                    (format t "Une règle correspond à vos réponses ~%")
                    ;on applique les actions de la règle
                    (let ((matching-rule (car matching-rules)))
                        (do-actions matching-rule)
                    )
                    ;on supprime la règle de la li;on applique les actions de la règle
                    (let ((matching-rule (car matching-rules)))
                        (do-actions matching-rule)
                    )
                    ;on supprime la règle de la liste des règles
                    (setq *rules* (remove (car matching-rules) *rules*))
                )
            )
            ;if there is more than one matching rule
            (if (> (length matching-rules) 1)
                (progn
                    (format t "Plusieurs règles correspondent à vos réponses ~%")
                    ;on récupère une règle au hasard
                    (let ((matching-rule (get-random-rule matching-rules)))
                        (do-actions matching-rule)
                    )
                    ;on supprime la règle de la liste des règles
                    (setq *rules* (remove (car matching-rules) *rules*))
                )
            )
            (apply-step)
        )
)




(defun main ()
        ;load the facts
        (setq *facts* (list :env "" :spe "" :haut "" :vehicule "" :mea ""))
        ;questions est une liste vide
        ;(setq questions '())
        ;create the questions
        (setq *questions* (create-question "Quelle est le type de l'opération ?" :spe *questions*))
        (setq *questions* (create-question "Dans quel environnement se passe l'opération ?" :env *questions*))

        ;show the rules
        ;(print rules)
        ;(print questions)

        (apply-step)
)


(main)