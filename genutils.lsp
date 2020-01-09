;; ======================================================================
;; GENERAL UTILITIES
;; ======================================================================

(defun reduce (function args)
  (cond ((eq nil (cdr args)) (car args))
        (t (apply function (list (car args)
                                 (reduce function (cdr args)))))))

(defun stringp (arg)
  (eq (type arg) 'STR))

(defun realp (arg)
  (eq (type arg) 'REAL))

(defun endp (arg)
  (and (listp arg) (null (cdr arg))))
