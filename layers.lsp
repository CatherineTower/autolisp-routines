;; ================================================================================
;; LAYERS
;; ================================================================================

;; Written by Catherine Tower while she was working at Baye Enterprises
;; these utilities are meant specifically for the Baye environment and
;; may malfunction under any other circumstances

;; These are variables to hold the names of layers. Objects of
;; different types should go on semantically different layers, and
;; these are the categories of layer.

;; Do what you can to make this file load first, otherwise some
;; commands may not work properly

(setq *dimension-layer* "DIM")
(setq *symbol-layer* "SYMB")
(setq *hardware-layer* "HARDWARE")
(setq *hatch-layer* "HATCH")
(setq *text-layer* "TEXT")
