;; ================================================================================
;; DIMENSION UTILITIES
;; ================================================================================

;; Written by Catherine Tower while she was working at Baye Enterprises
;; these utilities are meant specifically for the Baye environment and
;; may malfunction under any other circumstances

;; These replace the standard dimension commands with routines that
;; swap the layer for "DIM" before applying the dimension

(command ".undefine" "dimlinear")
(defun C:dimlinear (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimlinear")
  (setvar "clayer" oldlayer)
  (princ))

(command ".undefine" "dimaligned")
(defun C:dimaligned (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimaligned")
  (setvar "clayer" oldlayer)
  (princ))

(command ".undefine" "dimangular")
(defun C:dimangular (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimangular")
  (setvar "clayer" oldlayer)
  (princ))

(command ".undefine" "dimarc")
(defun C:dimarc (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimarc")
  (setvar "clayer" oldlayer)
  (princ))

(command ".undefine" "dimdiameter")
(defun C:dimdiameter (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimdiameter")
  (setvar "clayer" oldlayer)
  (princ))

(command ".undefine" "dimordinate")
(defun C:dimordinate (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimordinate")
  (setvar "clayer" oldlayer)
  (princ))

(command ".undefine" "dimradius")
(defun C:dimradius (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimradius")
  (setvar "clayer" oldlayer)
  (princ))

(command ".undefine" "dimbaseline")
(defun C:dimbaseline (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimbaseline")
  (while (= 1 (getvar "cmdactive"))
    (command pause))
  (setvar "clayer" oldlayer)
  (princ))

(command ".undefine" "dimcontinue")
(defun C:dimcontinue (/ oldlayer)  
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimcontinue")
  (while (= 1 (getvar "cmdactive"))
    (command pause))
  (setvar "clayer" oldlayer)
  (princ))

(command ".undefine" "dim")
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

;; NOTE: as of now these functions don't work.

(defun C:fdli (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (command "dimstyle" "r" "field verify")
  (c:dimlinear)
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (princ))

(defun C:fdal (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (command "dimstyle" "r" "field verify")
  (command "dimaligned")
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (princ))

(defun C:fdan (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (command "dimstyle" "r" "field verify")
  (command "dimangular")
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (princ))

(defun C:fdimarc (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (command "dimstyle" "r" "field verify")
  (command "dimarc")
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (princ))

(defun C:fddi (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (command "dimstyle" "r" "field verify")
  (command "dimdiameter")
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (princ))

(defun C:fdimord (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (command "dimstyle" "r" "field verify")
  (command "dimordinate")
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (princ))

(defun C:fdra (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (command "dimstyle" "r" "field verify")
  (command "dimradius")
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (princ))

(defun C:fdim (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (command "dimstyle" "r" "field verify")
  (command "dim")
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (princ))
