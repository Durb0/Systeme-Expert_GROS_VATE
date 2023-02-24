

(defun set-value (fact attribute value)
    (setf (getf fact attribute) value)
    fact
)

(defun get-value (fact attribute)
    (getf fact attribute)
)

(defun show-facts (facts)
    (format t "Etat des faits : ~%")
    (format t "Environnement : ~a ~%" (get-value facts :env))
    (format t "Specialite : ~a ~%" (get-value facts :spe))
    (format t "Hauteur : ~a ~%~%" (get-value facts :haut))
    
    (format t "Vehicule : ~a ~%" (get-value facts :vehicule))
    (if (string= "OUI" (get-value facts :mea))
        (format t "Avec MEA~%~%" (get-value facts :mea))
    )
    
)

;(defvar test (list :env "toto" :spe "tata" :vehicule "titi"))
;(print test)
;(set-value test :env "tutu")
;(print test)
