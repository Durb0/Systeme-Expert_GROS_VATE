(defstruct rule
    name
    conditions
    actions
)

(defun check-conditions (rule facts)
    ;check all conditions, return true if all conditions are true
    ;(print facts)
    (loop for condition in (rule-conditions rule)
        do (if (not (eval condition))
                (return-from check-conditions nil)
            )
    )
    t
)

(defun do-actions (rule)
    ;do all actions
    (loop for action in (rule-actions rule)
        do (eval action)
    )
)


(defun get-matching-rules (rules facts)
    ;return a list of rules that match the current state
    ;(print facts)
    (loop for rule in rules
        do (if (check-conditions rule facts)
                (return-from get-matching-rules (list rule))
            )
    )
    nil
)

(defun get-random-rule (rules)
    (nth (random (length rules)) rules)
)

(defun print-rules (rules)
    (loop for rule in rules do
        (print (rule-name rule))
    )
)


(defun remove-rule (rule rules)
    (remove-if (lambda (r) (eq r rule)) rules)
)

