;; ================================================================================
;; BLOCK UTILITIES
;; ================================================================================

;; Written by Catherine Tower while she was working at Baye Enterprises
;; these utilities are meant specifically for the Baye environment and
;; may malfunction under any other circumstances

;; Inserts a break block on the "SYMB" layer

(defun c:insbr (/ oldlayer)
  (setq oldlayer (getvar "clayer"))
  (setvar "clayer" "symb")
  (command "insert" "break" pause "" "" "")
  (setvar "clayer" oldlayer)
  (princ))
