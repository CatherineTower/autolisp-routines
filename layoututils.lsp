(defun totalpages ()
  (length (vl-remove-if-not
           '(lambda (page) (numberp (read page)))
           (layoutlist))))
