(import (merge-pathnames "shared" *source-directory*)
  session-add-comm!
  session-add-widget!
  session-pub
  initialize-widget
  widget-comm
  widget-state
	set-widget-state!)
(define *the-environment* (the-environment))
(load (merge-pathnames "runtime/widget" *source-directory*))
