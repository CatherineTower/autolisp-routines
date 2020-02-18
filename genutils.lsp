;; ======================================================================
;; GENERAL UTILITIES
;; ======================================================================

;; AutoLISP is what I'm going to call a "degenerate Lisp". A lot of
;; the standard functions you expect from an implementation of Lisp
;; aren't included. This file aims to bridge the gap between a "proper
;; Lisp" (Common Lisp, Scheme) and AutoLISP.

;; I don't hardly use most of these, but first and rest are a necessity.
(defun first (list)
  (car list))

(defun rest (list)
  (cdr list))

(defun second (list)
  (nth 1 list))

(defun third (list)
  (nth 2 list))

(defun fourth (list)
  (nth 3 list))

(defun fifth (list)
  (nth 4 list))

(defun sixth (list)
  (nth 5 list))

(defun seventh (list)
  (nth 6 list))

(defun eighth (list)
  (nth 7 list))

(defun ninth (list)
  (nth 8 list))

(defun tenth (list)
  (nth 9 list))

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

(defun acons (obj1 obj2 alist)
  (setq alist (cons (cons obj1 obj2) alist)))

;; this might be built-in already
;; (defun 1+ (number)
;;   (+ 1 number))

;; (defun 1- (number)
;;   (- number 1))

(defun push (obj list)
  (setq list (cons obj list)))

;; These functions were written by Lee Mac and can be found at
;; http://www.lee-mac.com/random.html

(defun randgen (/ a c m)
  (setq m 4294967296.0
        a 1664525.0
        c 1013904223.0
        $xn (rem (+ c (* a (cond ($xn) ((getvar 'date))))) m))
  (/ $xn m))

(defun random (a b)
  (+ (min a b) (* (randgen) (1+ (abs (- a b))))))

;; AutoLISP has assoc but not rassoc
(defun rassoc (value list)
  (cond ((null list) nil)
        ((equal value (cdr (first list))) (first list))
        (t (rassoc value (rest list)))))
