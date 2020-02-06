;; ======================================================================
;; GENERAL UTILITIES
;; ======================================================================

;; AutoLISP is what I'm going to call a "degenerate Lisp". A lot of
;; the standard functions you expect from an implementation of Lisp
;; aren't included. This file aims to bridge the gap between a "proper
;; Lisp" (Common Lisp, Scheme) and AutoLISP.

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
