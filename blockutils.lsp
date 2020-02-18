;; ================================================================================
;; BLOCK UTILITIES
;; ================================================================================

;; Written by Catherine Tower while she was working at Baye Enterprises
;; these utilities are meant specifically for the Baye environment and
;; may malfunction under any other circumstances

(defun radtodeg (radians)
  (* radians (/ 180 pi)))

;; Inserts a break block on the "SYMB" layer between two points
(defun c:insbr (/ oldlayer first-point second-point x1 x2 y1 y2 *error*)

  (defun *error* (message)
    (*layer-error* oldlayer)
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setq first-point (getpoint "Select first point: ")
    second-point (getpoint "Select second point: ")
    x1 (car first-point)
    y1 (cadr first-point)
    x2 (car second-point)
    y2 (cadr second-point))
  (setvar "clayer" "symb")
  (command "insert" "break"
       (list (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))
       "" ""
       (+ 90 (radtodeg (angle first-point second-point))))
  (setvar "clayer" oldlayer)
  (princ))

;; Utility function to determine whether we're in model or paperspace
(defun in-paperspace-p ()
  (and (/= "Model" (getvar "ctab"))
       (= 1 (getvar "cvport"))))

;; insert title bar
(defun c:intitle (/ oldlayer *error*)

  (defun *error* (message)
    (*layer-error* oldlayer)
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "symb")
  (command ".insert" "title bar_anno" pause "" "" ""
       "" "" ""
       (if (in-paperspace-p)
           "1'-0\" = 1'-0\""
           (getvar "cannoscale"))
       "")
  (setvar "clayer" oldlayer)
  (princ))

;; insert elevation
(defun c:insel (/ oldlayer *error*)

  (defun *error* (message)
    (*layer-error* oldlayer)
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "symb")
  (command ".insert" "elev bug_anno" pause "" "" "")
  (setvar "clayer" oldlayer)
  (princ))

;; insert section
(defun c:insec (/ oldlayer *error*)

  (defun *error* (message)
    (*layer-error* oldlayer)
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "symb")
  (command ".insert" "section marker_anno" pause "" "" "")
  (setvar "clayer" oldlayer)
  (princ))

;; insert finished end symbol
(defun c:fe (/ oldlayer *error*)

  (defun *error* (message)
    (*layer-error* oldlayer)
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "symb")
  (command ".insert" "finished end" pause "" "" "")
  (setvar "clayer" oldlayer)
  (princ))

;; insert detail bubble
(defun c:indet (/ oldlayer *error*)

  (defun *error* (message)
    (*layer-error* oldlayer)
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "symb")
  (command ".insert" "detail bubble_anno" pause "" "" "")
  (setvar "clayer" oldlayer)
  (princ))

;; insert leveler block
(defun c:inlev (/ oldlayer *error*)

  (defun *error* (message)
    (*layer-error* oldlayer)
    (princ))

  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "hardware")
  (command ".insert" "leveler" pause "" "" "")
  (setvar "clayer" oldlayer)
  (princ))
