;; ================================================================================
;; TEXT UTILITIES
;; ================================================================================

;; Written by Catherine Tower while she was working at Baye Enterprises
;; these utilities are meant specifically for the Baye environment and
;; may malfunction under any other circumstances

;; These replace the TEXT and MLEADER commands with commands that set
;; the layer before creating the object

;; (defun changelayer (entitydata newlayername)
;;   (subst (cons 8 newlayername) (assoc 8 entitydata) entitydata)

(command ".undefine" "mleader")
(defun C:mleader (/ oldlayer *error*)

  (defun *error* (message)
    (*layer-error* oldlayer)
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "text")
  (command ".mleader" pause pause "")
  (command ".textedit" (entlast))
  (setvar "clayer" oldlayer)
  (princ))

(command ".undefine" "mtext")
(defun C:mtext (/ oldlayer *error*)

  (defun *error* (message)
    (*layer-error* oldlayer)
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "text")
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

;; Utility function that draws a rectangle around text
(defun textbox_ (textobj / tb ll ur ul lr)
  (command "ucs" "Object" textobj)
  (setq tb (textbox (list (cons -1 textobj)))
    ll (car tb)
    ur (cadr tb)
    ul (list (car ll) (cadr ur))
    lr (list (car ur) (cadr ll)))
  (command "pline" ll lr ur ul "Close")
  (command "ucs" "p")
  (princ))

(defun c:textcloud (/ oldlayer boxpoints)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "text")
  (command ".mtext")
  (textbox_ (entget (entlast)))
  (command "revcloud" "o" (entlast))
  (setvar "clayer" oldlayer)
  (princ))


(defun evaltext (text)
  (eval (read (cdr (assoc 1 (entget text))))))

(defun replace-text-with-output (text / output entity)
  (setq output (evaltext text)
        entity (entget text))
  (setq entity (subst (cons 1 (vl-princ-to-string output)) (assoc 1 entity) entity))
  (entmod entity)

  (princ))
