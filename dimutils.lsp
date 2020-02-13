;; ================================================================================
;; DIMENSION UTILITIES
;; ================================================================================

;; Written by Catherine Tower while she was working at Baye Enterprises
;; these utilities are meant specifically for the Baye environment and
;; may malfunction under any other circumstances

;; These replace the standard dimension commands with routines that
;; swap the layer for "DIM" before applying the dimension

(setq +preferred-dim-spacing+ 0.375)

(defun *layer-error* (layer)
  (setvar "clayer" layer))

(defun *spacing-error* (spacing)
  (setvar "dimdli" spacing))

(command ".undefine" "dimlinear")
(defun C:dimlinear (/ oldlayer olderror)

  (defun *error* (message)
    (*layer-error* oldlayer)
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimlinear")
  (while (= 1 (getvar "cmdactive"))
    (command pause))
  (setvar "clayer" oldlayer)
  (princ))

(command ".undefine" "dimaligned")
(defun C:dimaligned (/ oldlayer)

  (defun *error* (message)
    (*layer-error* oldlayer)
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimaligned")
  (while (= 1 (getvar "cmdactive"))
    (command pause))
  (setvar "clayer" oldlayer)
  (princ))

(command ".undefine" "dimangular")
(defun C:dimangular (/ oldlayer)

  (defun *error* (message)
    (*layer-error* oldlayer)
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimangular")
  (while (= 1 (getvar "cmdactive"))
    (command pause))
  (setvar "clayer" oldlayer)
  (princ))

(command ".undefine" "dimarc")
(defun C:dimarc (/ oldlayer)

  (defun *error* (message)
    (*layer-error* oldlayer)
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimarc")
  (while (= 1 (getvar "cmdactive"))
    (command pause))
  (setvar "clayer" oldlayer)
  (princ))

(command ".undefine" "dimdiameter")
(defun C:dimdiameter (/ oldlayer)

  (defun *error* (message)
    (*layer-error* oldlayer)
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimdiameter")
  (while (= 1 (getvar "cmdactive"))
    (command pause))
  (setvar "clayer" oldlayer)
  (princ))

(command ".undefine" "dimordinate")
(defun C:dimordinate (/ oldlayer)

  (defun *error* (message)
    (*layer-error* oldlayer)
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimordinate")
  (while (= 1 (getvar "cmdactive"))
    (command pause))
  (setvar "clayer" oldlayer)
  (princ))

(command ".undefine" "dimradius")
(defun C:dimradius (/ oldlayer)

  (defun *error* (message)
    (*layer-error* oldlayer)
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (command ".dimradius")
  (while (= 1 (getvar "cmdactive"))
    (command pause))
  (setvar "clayer" oldlayer)
  (princ))

(command ".undefine" "dimbaseline")
(defun C:dimbaseline (/ oldlayer old-dim-spacing)

  (defun *error* (message)
    (*layer-error* oldlayer)
    (*spacing-error* old-dim-spacing)
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (setq old-dim-spacing (getvar "dimdli"))
  (setvar "dimdli" +preferred-dim-spacing+)
  (command ".dimbaseline")
  (while (= 1 (getvar "cmdactive"))
    (command pause))
  (setvar "clayer" oldlayer)
  (setvar "dimdli" old-dim-spacing)
  (princ))

(command ".undefine" "dimcontinue")
(defun C:dimcontinue (/ oldlayer)

  (defun *error* (message)
    (*layer-error* oldlayer)
    (princ))

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

  (defun *error* (message)
    (*layer-error* oldlayer)
    (princ))

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
  (c:dimaligned)
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (princ))

(defun C:fdan (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (command "dimstyle" "r" "field verify")
  (c:dimangular)
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (princ))

(defun C:fdimarc (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (command "dimstyle" "r" "field verify")
  (c:dimarc)
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (princ))

(defun C:fddi (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (command "dimstyle" "r" "field verify")
  (c:dimdiameter)
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (princ))

(defun C:fdimord (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (command "dimstyle" "r" "field verify")
  (c:dimord)
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (princ))

(defun C:fdra (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (command "dimstyle" "r" "field verify")
  (c:dimradius)
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (princ))

;; These next two are different in that they don't automatically edit
;; the text. This wouldn't work very well, since these commands create
;; many dimension objects
(defun C:fdba (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (command "dimstyle" "r" "field verify")
  (c:dimbaseline)
  (command "dimstyle" "r" olddimstyle "")
  (princ))

(defun C:fdco (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (command "dimstyle" "r" "field verify")
  (c:dimcontinue)
  (command "dimstyle" "r" olddimstyle "")
  (princ))

;; This is almost a lost cause for now
(defun C:fdim (/ olddimstyle)
  (setq olddimstyle (getvar "dimstyle"))
  (command "dimstyle" "r" "field verify")
  (c:dim)
  (command "textedit" (entlast))
  (command "dimstyle" "r" olddimstyle "")
  (princ))


;; This is purely a convenience function for something I draw often. I
;; frequently need a chain of dimensions followed by an overall. I'm
;; tired of going through all the motions, so here's a quick script to
;; do it.
(defun c:dimchain (/ oldlayer first-dim old-dim-spacing)

  (defun *error* (message)
    (*layer-error* oldlayer)
    (*spacing-error* old-dim-spacing)
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")

  (setq old-dim-spacing (getvar "dimdli"))
  (setvar "dimdli" +preferred-dim-spacing+)

  (command ".dimlinear")
  (while (= 1 (getvar "cmdactive"))
    (command pause))
  (setq first-dim (entlast))
  (command ".dimcontinue")
  (while (= 1 (getvar "cmdactive"))
    (command pause))
  (setq endpoint (getvar "lastpoint"))
  (command ".dimbaseline" "s" first-dim)
  (while (= 1 (getvar "cmdactive"))
    (command pause))

  (setvar "dimdli" old-dim-spacing)
  (setvar "clayer" oldlayer)
  (princ))
