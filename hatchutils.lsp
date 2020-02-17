;; ================================================================================
;; HATCH UTILITIES
;; ================================================================================

;; Written by Catherine Tower while she was working at Baye Enterprises
;; these utilities are meant specifically for the Baye environment and
;; may malfunction under any other circumstances

;; This function requires genutils.lsp to be loaded
(defun randomize-origin-points (/ hatch-set i current-hatch
                                origin-x origin-y)
  (setq hatch-set (ssget '((0 . "HATCH"))))
  (setq i 0)
  (repeat (sslength hatch-set)
          (setq current-hatch  (ssname hatch-set i))
          (setq origin-x (cdr (assoc 43 (entget current-hatch)))
                origin-y (cdr (assoc 44 (entget current-hatch))))
          (command ".hatchedit" current-hatch
                   "O" "S"
                   (list (+ origin-x (random -20 20))
                         (+ origin-y (random -20 20)))
                   "N")
          (setq i (1+ i)))
  (princ))
