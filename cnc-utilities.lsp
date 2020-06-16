;; ================================================================================
;; ENTITY UTILITIES
;; ================================================================================

;; Written by Catherine Tower while she was working at Baye Enterprises
;; these utilities are meant specifically for the Baye environment and
;; may malfunction under any other circumstances

;; This file contains routines that I find useful for operating our
;; CNC rounter programming software. It really only works for the
;; specific software we're using.

;; Hide the layers the CNC software defines
(defun c:hidenc ()
  (command "hideobjects" (ssget "x" '((8 . "NC_*"))) ""))

;; Calculates the "Max Offset" of a pocket cut. This is just to prove
;; that the calculation can be done in software instead of manually
(defun max-offset (polyline / points-list min-dimension)
  (if (not *tr*)
      (progn (princ "Error: *TR* not defined") (princ))
      (progn
        (setq points-list
              (vl-remove-if-not '(lambda (x) (= 10 (car x)))
                                (entget polyline)))
        (setq min-dimension
              (reduce 'min
                      (vl-remove-if 'zerop
                                    (map-all-pairs
                                     '(lambda (x y) (distance
                                                     (list (cadr x) (caddr x))
                                                     (list (cadr y) (caddr y))))
                                     points-list))))
        (abs (/ (- min-dimension *tr*) 2.0)))))

;; Removes redundant vertices from an object. Currently only
;; polylines. The resolution of the final product is determined by *EPSILON*
(defun c:prune (/ polyline-set i)
  (setq polyline-set (ssget '((0 . "LWPOLYLINE"))))
  (setq i 0)
  (if polyline-set
      (repeat (sslength polyline-set)
              (prune-polyline (entget (ssname polyline-set i)))
              (setq i (1+ i))))
  (princ))

;; Prunes unnecessary vertices from dense polylines
;; NOTE: this only works with polylines made entirely of line
;; segments. For now, at least.
(defun prune-polyline (polyline / points-list i result)
  (setq points-list (vl-remove-if-not
                     '(lambda (x) (= 10 (car x))) polyline))
  (setq i 0)
  (setq result (list
                '(0 . "LWPOLYLINE")
                '(100 . "AcDbEntity")
                '(100 . "AcDbPolyline")
                (cons 8 (cdr (assoc 8 polyline)))))
  (while (< i (length points-list))
    (mapc-1 '(lambda (x)
              (setq points-list (vl-remove x points-list)))
            (generate-prune-candidates
             (subseq points-list
                     i
                     (if (> (- (length points-list) (+ 5 i)) 2)
                         (+ 5 i)
                         i))))
    (setq i (1+ i)))
  (setq result (append result (list (cons 90 (length points-list)))))
  (setq result (append result (list '(70 . 1))))
  (setq result (append result points-list))
  (entdel (cdr (assoc -1 polyline)))
  (entmake result))

(setq *epsilon* 0.0001)
(defun generate-prune-candidates (points-list / angles candidates)
  (setq angles (subseq (map-pairs 'angle points-list)
                       0 (1- (length points-list)))
        candidates (map-pairs
                    '(lambda (x y) (< (abs (- x y)) *epsilon*))
                    angles))
  (print (mapcar '(lambda (x y z) (list x y z))
                 angles candidates points-list))
  (vl-remove-if 'null
                (mapcar '(lambda (x y) (if x y))
                        candidates
                        (subseq points-list 1 (length points-list)))))
