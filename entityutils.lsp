;; ================================================================================
;; ENTITY UTILITIES
;; ================================================================================

;; Written by Catherine Tower while she was working at Baye Enterprises
;; these utilities are meant specifically for the Baye environment and
;; may malfunction under any other circumstances

;; I wanted to define this in terms of a function that builds point
;; filter functions, but AutoLisp doesn't support lexical
;; closures. Instead, it'll just be a series of functions and one
;; master dispatch function.

(defun entity-type (entity-list)
  (cdr (assoc 0 entity-list)))

(defun get-polyline-points (entity-list)
  (mapcar 'cdr (vl-remove-if-not
                '(lambda (x) (equal 10 (car x)))
                entity-list)))

(defun get-circle-points (entity-list / intermediate-list)
  (setq intermediate-list
        (mapcar 'cdr (vl-remove-if-not
                      '(lambda (x) (or (equal 10 (car x))
                                    (equal 40 (car x))))
                      entity-list)))

  ;; This is garbage to read, but basically it makes a list of two
  ;; lists, and sets the second and third elements of the second list
  ;; to the second and third elements of the first list.
  (list (car intermediate-list)
        (list (cadr intermediate-list)
              (cadar intermediate-list)
              (caddar intermediate-list))))

;; This feels super dumb, but it's for completeness
(defun get-point-points (entity-list)
  (mapcar 'cdr (vl-remove-if-not
                '(lambda (x) (equal 10 (car x)))
                entity-list)))

(defun get-spline-points (entity-list)
  (mapcar 'cdr (vl-remove-if-not
                '(lambda (x) (equal 10 (car x)))
                entity-list)))

(defun C:getpoints (/ entity-list)
  (setq entity-list (entget (car (entsel))))
  (cond ((= (entity-type entity-list) "CIRCLE")
         (get-circle-points entity-list))

        ((= (entity-type entity-list) "LWPOLYLINE")
         (get-polyline-points entity-list))

        ((= (entity-type entity-list) "POINT")
         (get-point-points (entity-list)))

        (t (princ "Error: no points for object"))))

;; Calculates the "Max Offset" of a pocket cut. This is just to prove
;; that the calculation can be done in software instead of manually
(defun max-offset (polyline / points-list min-dimension)
  (if (not *tr*) (*error*))
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
  (abs (/ (- min-dimension *tr*) 2.0)))
