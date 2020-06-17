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
    (mapc-1 '(lambda (x) (setq points-list (vl-remove x points-list)))
            ;; Only generate prune candidates if we have more than two points to go
            (if (> (length points-list) (+ 5 i))
                (generate-prune-candidates (subseq points-list i (+ 5 i)))
                (generate-prune-candidates (subseq points-list i (length points-list)))))
    (setq i (1+ i)))
  (setq result (append result (list (cons 90 (length points-list)))))
  (setq result (append result (list (cons 70 (cdr (assoc 70 polyline))))))
  (setq result (append result points-list))
  (entdel (cdr (assoc -1 polyline)))
  (entmake result))

;; Determining the difference between two angles in AutoCAD is constrained by the precision of
;; floating-point arithmetic in AutoLISP. Which is to say that it's altogether NOT precise enough.
(defun generate-prune-candidates (points-list / angles candidates)
  ;; With every map-pairs we have to disregard the last value, since it wraps around the input list
  (setq angles (subseq (map-pairs 'angle points-list) 0 (1- (length points-list)))
        candidates (subseq (map-pairs '(lambda (x y) (= (- x y) 0)) angles) 0 (1- (length angles))))
  (vl-remove-if 'null (mapcar '(lambda (x y) (if x y))
                              candidates
                              (subseq points-list 1 (length points-list)))))
