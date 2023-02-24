(defstruct question
    text
    answer
    type
)


(defun create-question (text type questions)
    (let ((question (make-question :text text :type type)))
        (setf questions (push question questions))
    )
)

(defun ask-question (question)
    (format t "~a~%" (question-text question))
    (let ((answer (read-line)))
        (setf (question-answer question) answer)
    )
)

(defun ask-random-question (questions)
    (let ((question (nth (random (length questions)) questions)))
        (ask-question question)
    )
)

(defun remove-question (question questions)
    ;supprime la question qui a la même question que la question passée en paramètre
    (remove-if (lambda (q) (equal (question-question q) (question-text question))) questions)
)

(defun get-random-question (questions)
    (nth (random (length questions)) questions)
)

(defun first-question (questions)
    (car questions)
)

;(loop for question in *questions* do (ask-question question))