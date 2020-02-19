(defun totalpages ()
  (length (vl-remove-if-not
           '(lambda (page) (numberp (read page)))
           (layoutlist))))

(defun zoom-all-layouts ()
  (mapcar '(lambda (x)
            (setvar "ctab" x)
            (if (not (in-paperspace-p))
                (command ".pspace"))
            (command ".zoom" "E"))
          (layoutlist))
  (setvar "ctab" (first (layoutlist))))
