;; ================================================================================
;; DIMENSION UTILITIES
;; ================================================================================

;; Written by Catherine Tower while she was working at Baye Enterprises
;; these utilities are meant specifically for the Baye environment and
;; may malfunction under any other circumstances

;; This is a somewhat onerous attempt at using a Common Lisp-style
;; macro. Instead of the nice defmacro-and-backquote syntax found in
;; proper Lisps, this opts for a runtime-constructed list which is fed
;; into eval. Still, this is a huge breakthrough and reduces code
;; duplication by a LOT.

;; A warning, however: the commands will not execute without a layer
;; named "DIM" in the drawing. What's more, they'll give you a really
;; cryptic and unhelpful error message.
(defun define-dimension-command (name command-name)
  (eval (list 'command "undefine" command-name))
  (eval
   (list 'defun name '(/ oldlayer *error*)
         '(defun *error* (msg)
           (setvar "clayer" oldlayer))
         '(setq oldlayer (getvar "clayer"))
         '(setvar "clayer" "dim")
         (list 'command (strcat "." command-name))
         '(while (= 1 (getvar "cmdactive"))
           (command pause))
         '(setvar "clayer" oldlayer)
         '(princ)))
  (princ))

;; These replace the standard dimension commands with routines that
;; swap the layer for "DIM" before applying the dimension
(define-dimension-command 'c:dimlinear "dimlinear")
(define-dimension-command 'c:dimaligned "dimaligned")
(define-dimension-command 'c:dimangular "dimangular")
(define-dimension-command 'c:dimarc "dimarc")
(define-dimension-command 'c:dimdiameter "dimdiameter")
(define-dimension-command 'c:dimordinate "dimordinate")
(define-dimension-command 'c:dimradius "dimradius")
(define-dimension-command 'c:dimcontinue "dimcontinue")

;; This one doesn't work because of the specific nature of DIM. I'll
;; have to investigate it more later.
;; Of course, I might not, since I never use the DIM command.
(define-dimension-command 'c:dim "dim")

;; This one doesn't quite fit into the template mold of
;; define-dimension-command. I might try and figure out a way to
;; factor it out later, but this is enough for now.
(setq +preferred-dim-spacing+ 0.375)
(command ".undefine" "dimbaseline")
(defun C:dimbaseline (/ oldlayer old-dim-spacing *error*)

  (defun *error* (message)
    (setvar "clayer" oldlayer)
    (setvar "dimdli" old-dim-spacing)
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

;; FIELD VERIFY DIMENSIONS

;; Template macro for field verify dimensions
(defun define-fv-dimension-command (name command-name)
  (eval
   (list 'defun name '(/ old-dim-style old-layer *error*)
         '(defun *error* (message)
           (setvar "clayer" old-layer)
           (command "dimstyle" "r" old-dim-style))

         '(setq old-layer (getvar "clayer"))
         '(setvar "clayer" "dim")
         '(setq old-dim-style (getvar "dimstyle"))
         '(command "dimstyle" "r" "field verify")
         (list 'command (strcat "." command-name))
         '(while (= 1 (getvar "cmdactive"))
           (command pause))
         '(command "textedit" (entlast))
         '(command "dimstyle" "r" old-dim-style "")
         '(setvar "clayer" old-layer)
         '(princ))))

(define-fv-dimension-command 'c:fdli ".dimlinear")
(define-fv-dimension-command 'c:fdal "dimaligned")
(define-fv-dimension-command 'c:fdan "dimangular")
(define-fv-dimension-command 'c:fdimarc "dimarc")
(define-fv-dimension-command 'c:fddi "dimdiameter")
(define-fv-dimension-command 'c:fdimord "dimordinate")
(define-fv-dimension-command 'c:fdra "dimradius")
(define-fv-dimension-command 'c:fdco "dimcontinue")

;; This one's almost a lost cause for now
(define-fv-dimension-command 'c:fdim "dim")

;; These next two are different in that they don't automatically edit
;; the text. This wouldn't work very well, since these commands create
;; many dimension objects
(defun C:fdba (/ oldlayer olddimstyle old-dim-spacing *error*)

  (defun *error* (message)
    (setvar "clayer" oldlayer)
    (setvar "dimdli" old-dim-spacing)
    (command "dimstyle" "r" olddimstyle "")
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "dim")
  (setq old-dim-spacing (getvar "dimdli"))
  (setvar "dimdli" +preferred-dim-spacing+)
  (setq olddimstyle (getvar "dimstyle"))
  (command "dimstyle" "r" "field verify")
  (command ".dimbaseline")
  (while (= 1 (getvar "cmdactive"))
    (command pause))
  (setvar "clayer" oldlayer)
  (setvar "dimdli" old-dim-spacing)
  (command "dimstyle" "r" olddimstyle "")
  (princ))

;; This is purely a convenience function for something I draw often. I
;; frequently need a chain of dimensions followed by an overall. I'm
;; tired of going through all the motions, so here's a quick script to
;; do it.
(defun c:dimchain (/ oldlayer first-dim old-dim-spacing *error*)

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
