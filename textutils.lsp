;; ================================================================================
;; TEXT UTILITIES
;; ================================================================================

;; Written by Catherine Tower while she was working at Baye Enterprises
;; these utilities are meant specifically for the Baye environment and
;; may malfunction under any other circumstances

;; These replace the TEXT and MLEADER commands with commands that set
;; the layer before creating the object

(command ".undefine" "mleader")
(defun C:mleader (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "text")
  (command ".mleader" pause pause "")
  (command ".textedit" (entlast))
  (setvar "clayer" oldlayer)
  (princ))
