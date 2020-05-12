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
