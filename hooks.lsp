;; ================================================================================
;; HOOKS
;; ================================================================================

;; Written by Catherine Tower while she was working at Baye Enterprises
;; these utilities are meant specifically for the Baye environment and
;; may malfunction under any other circumstances

;; These are hooks to run arbitrary code before and after certain
;; commands. They work by overriding the default commands and
;; replacing them with a version that wraps the hooks around the
;; default commands.

;; This is the interface into the hooks.
(defun add-hook (where func)
  (cond ((eq 'before-save-hook where)
         (if (not (member func *before-save-hooks*))
             (setq *before-save-hooks*
                   (cons func *before-save-hooks*))))
        ((eq 'after-save-hook where)
         (if (not (member func *after-save-hooks*))
             (setq *after-save-hooks*
                   (cons func *after-save-hooks*))))))

(defun remove-hook (where func)
  (cond ((eq 'before-save-hook where)
         (vl-remove func *before-save-hooks*))
        ((eq 'after-save-hook where)
         (vl-remove func *after-save-hooks*))))

(setq *before-save-hooks* nil)
(setq *after-save-hooks* nil)

(command ".undefine" "qsave")
(defun c:qsave ()
  (mapcar '(lambda (x) (apply x nil))
          *before-save-hooks*)
  (command ".qsave")
  (mapcar '(lambda (x) (apply x nil))
          *after-save-hooks*)
  (princ))
