(in-package :edu.case.acm-people.jkp46.graph)

;;; *** For a association-list (ALIST) based approach ****************
(defclass list-graph (graph) ())

(defmethod get-neighbors ((mygraph list-graph) node)
  (with-accessors ((adjlist graph-al)) mygraph
    (cadr (assoc node adjlist :test #'equal))))
 
(defmethod show-graph ((mygraph list-graph))
  (dolist (v (graph-al mygraph))
    (format t "~a -> ~a~%"
        (car v)
        (cadr v))))

; update to reflect other add-vertex method
(defmethod add-vertex ((mygraph list-graph) name &optional neighbors)
  (with-accessors ((adjlist graph-al)) mygraph
    (let ((already-exists (assoc name adjlist)))
      (if (not already-exists)
        (push (cons name (cons neighbors nil)) adjlist))
      (return-from add-vertex already-exists))))

(defmethod add-edge ((mygraph list-graph) v1 v2)
  (with-accessors ((adjlist graph-al)) mygraph
    (push v2 (cadr (assoc v1 adjlist :test #'equal))))
  mygraph)
 
(defmethod remove-vertex ((mygraph list-graph) to-destroy)
  (with-accessors ((adjlist graph-al)) mygraph
    (setf adjlist (delete (assoc to-destroy adjlist) adjlist))))

(defmethod search-adjlist ((g list-graph) key)
  (assoc key (graph-al g)))
 
