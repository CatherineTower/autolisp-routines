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

;; These are the lists of hooks.
(setq *before-save-hooks* nil)
(setq *after-save-hooks* nil)
(setq *after-open-hooks* nil)
(setq *before-close-hooks* nil)

;; add-hook and remove-hook are the interface into the hooks.
(defun add-hook (where func)
  (cond ((eq 'before-save-hook where)
         (if (not (member func *before-save-hooks*))
             (setq *before-save-hooks*
                   (cons func *before-save-hooks*))))

        ((eq 'after-save-hook where)
         (if (not (member func *after-save-hooks*))
             (setq *after-save-hooks*
                   (cons func *after-save-hooks*))))

        ((eq 'after-open-hook where)
         (if (not (member func *after-open-hooks*))
             (setq *after-open-hooks*
                   (cons func *after-open-hooks*))))

        ((eq 'before-close-hook where)
             (if (not (member func *before-close-hooks*))
                 (setq *before-close-hooks*
                       (cons func *before-close-hooks*))))))

;; This works wonderfully when using a named function, since the
;; symbol tests eq to the symbol in the list. However, if you're
;; trying to remove a lambda from a hooks list, it will fail.
(defun remove-hook (where func)
  (cond ((eq 'before-save-hook where)
         (setq *before-save-hooks*
               (vl-remove func *before-save-hooks*)))
        ((eq 'after-save-hook where)
         (setq *after-save-hooks*
               (vl-remove func *after-save-hooks*)))
        ((eq 'after-open-hook where)
         (setq *after-open-hooks*
               (vl-remove func *after-open-hooks*)))
        ((eq 'before-close-hook where)
         (setq *before-close-hooks*
               (vl-remove func *before-close-hooks)))))

(command ".undefine" "qsave")
(defun c:qsave ()
  (mapcar '(lambda (x) (apply x nil))
          *before-save-hooks*)
  (command ".qsave")
  (mapcar '(lambda (x) (apply x nil))
          *after-save-hooks*)
  (princ))

;; This one was irritating because for SOME REASON you have to
;; duplicate the functionality of (command ".open") with Visual Lisp
;; code.
(command ".undefine" "open")
(defun c:open (/ file)
  (setq file (getfiled "Select File" "" "dwg" 0))
  (if file
      (vla-activate
       (vla-open
        (vla-get-documents (vlax-get-acad-object))
        file)))
      (mapcar '(lambda () (apply x nil))
              *after-open-hooks*))

(command ".undefine" "close")
(defun c:close ()
  (mapcar '(lambda (x) (apply x nil))
          *before-close-hooks*)
  (vla-SendCommand (vla-get-ActiveDocument (vlax-get-acad-object)) "_.CLOSE "))
