;; ================================================================================
;; BLOCK UTILITIES
;; ================================================================================

;; Written by Catherine Tower while she was working at Baye Enterprises
;; these utilities are meant specifically for the Baye environment and
;; may malfunction under any other circumstances

;; Inserts a break block on the "SYMB" layer

(defun radtodeg (radians)
  (* radians (/ 180 pi)))

;; insert break
(defun c:insbr (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "symb")
  (command ".insert" "break" pause "" ""
	   (+ 90 (radtodeg (getangle "Rotation Angle: "))))
  (setvar "clayer" oldlayer)
  (princ))

;; (defun calculate-angle (point1 point2)
;;   (let ((rise (- (cadr point2) (cadr point1)))
;; 	(run (- (car point2) (car point1))))
;;     (radtodeg (/ rise run))))

;; (defun c:insbr (/ first-point second-point x1 x2 y1 y2)
;;   (setq first-point (getpoint "Select first point: "))
;;   (setq second-point (getpoint "Select second point: "))
;;   (setq x1 (car first-point))
;;   (setq y1 (cadr first-point))
;;   (setq x2 (car second-point))
;;   (setq y2 (cadr second-point))
;;   (command "insert" "break"
;; 	   (list (/ (+ x1 x2) 2) (/ (+ y1 y2)))
;; 	   "" ""
;; 	   (+ 90 (calculate-angle point1 point2))))

;; Utility function to determine whether we're in model or paperspace
(defun in-paperspace-p ()
  (and (/= "Model" (getvar "ctab"))
       (= 1 (getvar "cvport"))))

;; insert title bar
(defun c:intitle (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "symb")
  (command ".insert" "title bar_anno" pause "" "" ""
	   "" "" ""
	   (if (in-paperspace-p)
	       "1'-0\" = 1'-0\""
	       (getvar "cannoscale")))
  (setvar "clayer" oldlayer)
  (princ))

;; insert elevation
(defun c:insel (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "symb")
  (command ".insert" "elev bug_anno" pause "" "" "")
  (setvar "clayer" oldlayer)
  (princ))

;; insert section
(defun c:insec (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "symb")
  (command ".insert" "section marker_anno" pause "" "" "")
  (setvar "clayer" oldlayer)
  (princ))

;; insert finished end symbol
(defun c:fe (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "symb")
  (command ".insert" "finished end" pause "" "" "")
  (setvar "clayer" oldlayer)
  (princ))

;; insert detail bubble
(defun c:indet (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "symb")
  (command ".insert" "detail bubble_anno" pause "" "" "")
  (setvar "clayer" oldlayer)
  (princ))
