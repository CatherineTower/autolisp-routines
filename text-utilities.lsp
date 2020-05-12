;; ================================================================================
;; TEXT UTILITIES
;; ================================================================================

;; Written by Catherine Tower while she was working at Baye Enterprises
;; these utilities are meant specifically for the Baye environment and
;; may malfunction under any other circumstances

;; These replace the TEXT and MLEADER commands with commands that set
;; the layer before creating the object

(command ".undefine" "mleader")
(defun C:mleader (/ oldlayer *error*)

  (defun *error* (message)
    (setvar "clayer" oldlayer)
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" *text-layer*)
  (command ".mleader" pause pause "")
  (command ".textedit" (entlast))
  (setvar "clayer" oldlayer)
  (princ))

(command ".undefine" "mtext")
(defun C:mtext (/ oldlayer *error*)

  (defun *error* (message)
    (setvar "clayer" oldlayer)
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" *text-layer*)
  (command ".mtext" pause pause "")
  (command ".textedit" (entlast))
  (setvar "clayer" oldlayer)
  (princ))

;; So. For some reason this function works just fine, no errors, and
;; modifies the entity it's passed. However, the modification doesn't
;; stick. It works on polylines (which also have DXF group codes 41
;; and 42) but not MTEXT objects. I cannot believe this disrespect.
(defun trim-text (entlist / text-width)
  (setq text-width (cdr (assoc 42 entlist)))
  (setq entlist (subst (cons 41 text-width) (assoc 41 entlist) entlist))
  (entmod entlist))

(defun revcloud-around-text (text / text-list revcloud-offset
                             text-width text-height
                             text-origin-x text-origin-y
                             rectangle-entity)
  (setq text-list (entget text)
        revcloud-offset (cdr (assoc 40 text-list))
        text-width (+ (* 2 revcloud-offset)
                      (cdr (assoc 42 text-list)))
        text-height (+ (* 2 revcloud-offset)
                       (cdr (assoc 43 text-list)))
        text-origin-x (- (second (assoc 10 text-list))
                         revcloud-offset)
        text-origin-y (+ (third (assoc 10 text-list))
                         revcloud-offset))
  (setq rectangle-entity
        (list (cons 0 "LWPOLYLINE")
              (cons 100 "AcDbEntity")
              (cons 100 "AcDbPolyline")
              (cons 8 *text-layer*)
              (cons 90 4)
              (cons 70 1)
              (list 10 text-origin-x text-origin-y 0.0)
              (list 10 (+ text-origin-x text-width) text-origin-y 0.0)
              (list 10 (+ text-origin-x text-width)
                    (- text-origin-y text-height) 0.0)
              (list 10 text-origin-x
                    (- text-origin-y text-height) 0.0)))
  (entmake rectangle-entity)
  (command ".revcloud" "O" (entlast) "")
  (entmod (subst (cons 8 *text-layer*) (assoc 8 (entget (entlast)))
                 (entget (entlast)))))

(defun c:revcloudtext (/ text-set i)
  (setq text-set (ssget '((0 . "MTEXT"))))
  (setq i 0)
  (if text-set
      (repeat (sslength text-set)
              (revcloud-around-text (ssname text-set i))
              (setq i (1+ i))))
      (princ))

(defun evaltext (text)
  (eval (read (cdr (assoc 1 (entget text))))))

(defun replace-text-with-output (text / output entity)
  (setq output (evaltext text)
        entity (entget text))
  (setq entity (subst (cons 1 (vl-princ-to-string output)) (assoc 1 entity) entity))
  (entmod entity)
  (princ))
