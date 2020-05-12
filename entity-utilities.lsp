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
