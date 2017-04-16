(define comm-version "~2.1.4")
(define comm-module "jupyter-js-widgets")
(define comm-target "jupyter.widget")
(define (make-comm-model id)
  (string-append "IPY_MODEL_" id))

(define (open-comm model-name view-name #!optional state)
  (let ((id (*make-msg-id*)))
    (let ((comm (*make-comm* id comm-target)))
      (*session*/add-comm! *session* comm)
      (*pub* "comm_open"
	     (fold-right
	      cons
	      (if (default-object? state) '() state)
	      `((target_name . ,comm-target)
		(target_module . #!unspecific)
		(data (_view_module . ,comm-module)
		      (_view_name . ,view-name)
		      (msg_throttle . 1)
		      (_model_module . ,comm-module)
		      (_model_module_version . ,comm-version)
		      (_view_module_version . ,comm-version)
		      (_model_name . ,model-name))
		(comm_id . ,id))))
      id)))

(define (msg-comm id data)
  (*pub* "comm_msg"
       `((comm_id . ,id)
	 (data . ,data))))

(define (slider-state style layout)
  `((max . 100)
     (style . ,(make-comm-model style))
     (layout . ,(make-comm-model layout))
     (_dom_classes . #())
     (orientation . "horizontal")
     (continuous_update . #t)
     (step . 1)
     (readout_format . "d")
     (readout . #t)
     (disabled . #f)
     (value . #f)))

(define (slider)
  (let ((style (open-comm "SliderStyleModel" "StyleView"))
	(layout (open-comm "LayoutModel" "LayoutView")))
    (let ((widget (open-comm "IntSliderModel" "IntSliderView"
			     (slider-state style layout))))
      (msg-comm widget '((method . "display")))
      (*pub* "display_data"
	   `((transient)
	     (data
	      (application/vnd.jupyter.widget-view+json
	       (model_id . ,widget)))
	     (metadata))))))
