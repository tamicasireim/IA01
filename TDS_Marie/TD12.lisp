(setq *agents* NIL)
(setq *message* NIL)

(defun set-prop-val (id prop val)
  (let ((pair (assoc prop (symbol-value id))))
    (set id (cons (cons prop val)
		  (remove pair (symbol-value id):test #'equal)))
    )
  )
(defun add-prop-val (id prop val)
  ; on ajoute la valeur val à prop dans id
  (let ((pair (assoc prop (symbol-value id))))
    (set-prop-val id prop (cons val (cdr pair)))
    )
  )

(defun make-agent (&key name comments)
  (let ((agent (gentemps "A-")) (process-id nil))
    (set agent (list (cons 'name name)
		     (cons 'id agent)
		     (cons 'skills nil)
		     (cons 'memory nil)
		     (cons 'inbox nil)
		     (cons 'comments comments)))
    (push agent *agents*)

    ;process
    ;launch then scanner process on input message queue
    (setq process-id (mp:process-run-function
		       (concatenate 'string (symbol-name agent) "-scan")
		       #'scan-messages ; fonction executee par le process
		       agent))
    (push (cons 'process process-id) (symbol-value agent))
    agent)

  )


