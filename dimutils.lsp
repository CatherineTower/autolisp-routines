;; ================================================================================
;; DIMENSION UTILITIES
;; ================================================================================

;; Written by Catherine Tower while she was working at Baye Enterprises
;; these utilities are meant specifically for the Baye environment and
;; may malfunction under any other circumstances

;; These replace the standard dimension commands with routines that
;; swap the layer for "DIM" before applying the dimension

(undefine dimlinear)
(defun C:dimlinear (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimlinear")
  (setvar "clayer" oldlayer)
  (princ))

(undefine dimaligned)
(defun C:dimaligned (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimaligned")
  (setvar "clayer" oldlayer)
  (princ))

(undefine dimangular)
(defun C:dimangular (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimangular")
  (setvar "clayer" oldlayer)
  (princ))

(undefine dimarc)
(defun C:dimarc (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimarc")
  (setvar "clayer" oldlayer)
  (princ))

(undefine dimdiameter)
(defun C:dimdiameter (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimdiameter")
  (setvar "clayer" oldlayer)
  (princ))

(undefine dimordinate)
(defun C:dimordinate (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimordinate")
  (setvar "clayer" oldlayer)
  (princ))

(undefine dimradius)
(defun C:dimradius (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimradius")
  (setvar "clayer" oldlayer)
  (princ))

(undefine dim)
(defun C:dim (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dim")
  (setvar "clayer" oldlayer)
  (princ))

;; FIELD VERIFY DIMENSIONS
;;
;;These are shortcuts for commands to create dimensions in the style
;;"FIELD VERIFY" and edit the text immediately after

(defun C:fdli (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (setvar "dimstyle" "field verify")
  (command "dimlinear")
  (command "textedit" (entlast))
  (setvar "dimstyle" olddimstyle)
  (princ))

(defun C:fdal (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (setvar "dimstyle" "field verify")
  (command "dimaligned")
  (command "textedit" (entlast))
  (setvar "dimstyle" olddimstyle)
  (princ))

(defun C:fdan (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (setvar "dimstyle" "field verify")
  (command "dimangular")
  (command "textedit" (entlast))
  (setvar "dimstyle" olddimstyle)
  (princ))

(defun C:fdimarc (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (setvar "dimstyle" "field verify")
  (command "dimarc")
  (command "textedit" (entlast))
  (setvar "dimstyle" olddimstyle)
  (princ))

(defun C:fddi (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (setvar "dimstyle" "field verify")
  (command "dimdiameter")
  (command "textedit" (entlast))
  (setvar "dimstyle" olddimstyle)
  (princ))

(defun C:fdimord (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (setvar "dimstyle" "field verify")
  (command "dimordinate")
  (command "textedit" (entlast))
  (setvar "dimstyle" olddimstyle)
  (princ))

(defun C:fdra (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (setvar "dimstyle" "field verify")
  (command "dimradius")
  (command "textedit" (entlast))
  (setvar "dimstyle" olddimstyle)
  (princ))

(defun C:fdim (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (setvar "dimstyle" "field verify")
  (command "dim")
  (command "textedit" (entlast))
  (setvar "dimstyle" olddimstyle)
  (princ))
