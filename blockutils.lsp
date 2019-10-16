;; ================================================================================
;; BLOCK UTILITIES
;; ================================================================================

;; Written by Catherine Tower while she was working at Baye Enterprises
;; these utilities are meant specifically for the Baye environment and
;; may malfunction under any other circumstances

;; Inserts a break block on the "SYMB" layer

(defun radtodeg (radians)
  (* radians (/ 180 pi)))

(defun c:insbr (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "symb")
  (command ".insert" "break" pause "" "" (+ 90 (radtodeg (getangle))))
  (setvar "clayer" oldlayer)
  (princ))

(defun c:fe (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "symb")
  (command ".insert" "finished end" pause "" "" "")
  (setvar "clayer" oldlayer)
  (princ))
