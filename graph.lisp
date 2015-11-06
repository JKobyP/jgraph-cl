(in-package :edu.case.acm-people.jkp46.graph)

;;; *** Base class for GRAPH
    ; Defaults as object oriented Adjacency list implementation
(defclass graph ()
  ((adjlist
    :accessor graph-al
    :initarg :adjlist
    :initform NIL)))

(defgeneric get-neighbors (graph node)
  (:documentation "get all the neighbors of node 'node'"))
 
(defgeneric show-graph (graph)
  (:documentation "print the graph in an ugly format")) 

(defgeneric add-vertex (graph name &optional neighbors)
  (:documentation "add the named vertex to the graph. Neighbors MUST be passed as a list")) 

(defgeneric add-edge (graph v1 v2)
  (:documentation "add an edge between the two vertices")) 

;;; *** For a association-list (ALIST) based approach
(defclass list-graph (graph) ())

(defmethod get-neighbors ((mygraph list-graph) node)
  (with-accessors ((adjlist graph-al)) mygraph
    (cadr (assoc node adjlist :test #'equal))))
 
(defmethod show-graph ((mygraph list-graph))
  (with-accessors ((adjlist graph-al)) mygraph
    (dolist (v adjlist)
      (format t "~a -> ~a~%"
          (car v)
          (cadr v)))))

(defmethod add-vertex ((mygraph list-graph) name &optional neighbors)
  (if (not (typep neighbors 'list)) (error "Please pass the neighbors as a list"))
  (with-accessors ((adjlist graph-al)) mygraph
    (push (cons name (cons neighbors nil)) adjlist)))

(defmethod add-edge ((mygraph list-graph) v1 v2)
  (with-accessors ((adjlist graph-al)) mygraph
    (push v2 (cadr (assoc v1 adjlist :test #'equal)))
    (values mygraph)))
 
;;; ***  For an OBJECT oriented approach
;; Vertex class
(defclass vertex ()
  ((name
    :accessor vname
    :initarg :name
    :initform NIL)
   (neighbors
    :accessor vneighbors
    :initarg :neighbors
    :initform NIL)))

;; Utility function determines if the given vertex has name <node>
(defun is-namep (node vertex)
  (equal (vname vertex) node))

(defmethod get-neighbors ((mygraph graph) node) 
  (with-accessors ((adjlist graph-al)) mygraph
    (vneighbors
     (find node adjlist :test
       #'is-namep))))

(defmethod show-graph ((mygraph graph))
  (with-accessors ((adjlist graph-al)) mygraph
    (loop
       for v in adjlist
       do (format t "~a -> ~a~%"
          (vname v)
          (vneighbors v)))))

(defmethod add-vertex ((mygraph graph) name &optional neighbors)
  (if (not (typep neighbors 'list))
      (error "Please pass the neighbors as a list")) 
    (with-accessors ((adjlist graph-al)) mygraph
      (push (make-instance 'vertex :name name :neighbors neighbors) adjlist))
    (values mygraph))

(defmethod add-edge ((mygraph graph) v1 v2)
  (with-accessors ((adjlist graph-al)) mygraph
    (nconc (vneighbors (car (member v1 adjlist :test #'is-namep)))
           v2)))
