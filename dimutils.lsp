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
  (while (= 1 (getvar "cmdactive"))
    (command pause))
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
(defun C:dimbaseline (/ oldlayer old-dim-spacing)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (setq old-dim-spacing (getvar "dimdli"))
  (setvar "dimdli" 0.375)
  (command ".dimbaseline")
  (while (= 1 (getvar "cmdactive"))
    (command pause))
  (setvar "clayer" oldlayer)
  (setvar "dimdli" old-dim-spacing)
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

;; This one doesn't work because of the specific nature of DIM. I'll
;; have to investigate it more later.
;; Of course, I might not, since I never use the DIM command.

(command ".undefine" "dim")
(defun C:dim (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dim")
  (while (= 1 (getvar "cmdactive"))
    (command pause))
  (setvar "clayer" oldlayer)
  (princ))

;; FIELD VERIFY DIMENSIONS
;;
;;These are shortcuts for commands to create dimensions in the style
;;"FIELD VERIFY" and edit the text immediately after

;; NOTE: as of now these functions don't work.

(defun C:fdli (/ olddimstyle oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setq olddimstyle (getvar "dimstyle"))
  (setvar "clayer" "dim")
  (command "dimstyle" "r" "field verify")
  (command ".dimlinear" pause pause pause)
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (setvar "clayer" oldlayer)
  (princ))

(defun C:fdal (/ olddimstyle oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setq olddimstyle (getvar "dimstyle"))
  (command "dimstyle" "r" "field verify")
  (setvar "clayer" "dim")
  (command ".dimaligned" pause pause pause)
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (setvar "clayer" oldlayer)
  (princ))

(defun C:fdan (/ olddimstyle oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setq olddimstyle (getvar "dimstyle"))
  (command "dimstyle" "r" "field verify")
  (setvar "clayer" "dim")
  (command ".dimangular")
  (while (= 1 (getvar "cmdactive"))
    (command pause))
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (setvar "clayer" oldlayer)
  (princ))

(defun C:fdimarc (/ olddimstyle oldlayer)
  (setq olddimstyle (getvar "dimstyle"))
  (setq oldlayer (getvar "clayer"))
  (command "dimstyle" "r" "field verify")
  (setvar "clayer" "dim")
  (command ".dimarc" pause pause)
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (setvar "clayer" oldlayer)
  (princ))

(defun C:fddi (/ olddimstyle oldlayer)
  (setq olddimstyle (getvar "dimstyle"))
  (setq oldlayer (getvar "clayer"))
  (command "dimstyle" "r" "field verify")
  (setvar "clayer" "dim")
  (command ".dimdiameter" pause pause)
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (setvar "clayer" oldlayer)
  (princ))

(defun C:fdimord (/ olddimstyle oldlayer)
  (setq olddimstyle (getvar "dimstyle"))
  (setq oldlayer (getvar "clayer"))
  (command "dimstyle" "r" "field verify")
  (setvar "clayer" "dim")
  (command ".dimordinate")
  (while (= 1 (getvar "cmdactive"))
    (command pause))
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (setver "clayer" oldlayer)
  (princ))

(defun C:fdra (/ olddimstyle oldlayer)
  (setq olddimstyle (getvar "dimstyle"))
  (setq oldlayer (getvar "clayer"))
  (command "dimstyle" "r" "field verify")
  (setvar "clayer" "dim")
  (command ".dimradius" pause pause)
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (setvar "clayer" oldlayer)
  (princ))

;; This is almost a lost cause for now
(defun C:fdim (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (command "dimstyle" "r" "field verify")
  (command "dim")
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (princ))
