;; ================================================================================
;; HATCH UTILITIES
;; ================================================================================

;; Written by Catherine Tower while she was working at Baye Enterprises
;; these utilities are meant specifically for the Baye environment and
;; may malfunction under any other circumstances

;; This function requires genutils.lsp to be loaded
(defun randomize-origin-points (/ hatch-set i current-hatch
                                origin-x origin-y half-scale)
  (setq hatch-set (ssget '((0 . "HATCH"))))
  (setq i 0)
  (repeat (sslength hatch-set)
          (setq current-hatch  (ssname hatch-set i))
          (setq origin-x (cdr (assoc 43 (entget current-hatch)))
                origin-y (cdr (assoc 44 (entget current-hatch)))
                half-scale (/ (cdr (assoc 41 (entget current-hatch))) 2))
          (command ".hatchedit" current-hatch
                   "O" "S"
                   (list (+ origin-x (random (- half-scale) half-scale))
                         (+ origin-y (random (- half-scale) half-scale)))
                   "N")
          (setq i (1+ i)))
  (princ))
