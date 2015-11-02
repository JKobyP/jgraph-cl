(in-package :edu.case.acm-people.jkp46.graph)

;; Base class for adjacency list
(defclass graph ()
  ((adjlist
    :accessor graph-al
    :initarg :adjlist
    :initform NIL)))

;; For an object oriented approach
(defclass vertex ()
  ((name
    :accessor vname
    :initarg :name
    :initform NIL)
   (neighbors
    :accessor vneighbors
    :initarg :neighbors
    :initform NIL)))

;; For a association-list based approach
(defclass list-graph (graph) ())

(defgeneric get-neighbors (graph node)
  (:documentation "get all the neighbors of node 'node'"))

;; determines if the vertex is named 'node'
(defun is-namep (node vertex)
  (equal (vname vertex) node))

(defmethod get-neighbors ((mygraph graph) node) 
  (with-accessors ((adjlist graph-al)) mygraph
    (vneighbors
     (find node adjlist :test
	   #'is-namep))))

(defmethod get-neighbors ((mygraph list-graph) node)
  (with-accessors ((adjlist graph-al)) mygraph
    (cadr (assoc node adjlist :test #'equal))))

(defgeneric show-graph (graph)
  (:documentation "print the graph in an ugly format"))

(defmethod show-graph ((mygraph graph))
  (with-accessors ((adjlist graph-al)) mygraph
    (loop
       for v in adjlist
       do (format t "~a -> ~a~%"
		  (vname v)
		  (vneighbors v)))))

(defmethod show-graph ((mygraph list-graph))
  (with-accessors ((adjlist graph-al)) mygraph
    (dolist (v adjlist)
      (format t "~a -> ~a~%"
	      (car v)
	      (cadr v)))))

(defgeneric add-vertex (graph name &optional neighbors)
  (:documentation "add the named vertex to the graph. Neighbors MUST be passed as a list"))

(defmethod add-vertex ((mygraph graph) name &optional neighbors)
  (if (not (typep neighbors 'list))
      (error "Please pass the neighbors as a list")) 
    (with-accessors ((adjlist graph-al)) mygraph
      (push (make-instance 'vertex :name name :neighbors neighbors) adjlist))
    (values mygraph))

(defmethod add-vertex ((mygraph list-graph) name &optional neighbors)
  (if (not (typep neighbors 'list)) (error "Please pass the neighbors as a list"))
  (with-accessors ((adjlist graph-al)) mygraph
    (push (cons name (cons neighbors nil)) adjlist)))

(defgeneric add-edge (graph v1 v2)
  (:documentation "add an edge between the two vertices"))

(defmethod add-edge ((mygraph list-graph) v1 v2)
  (with-accessors ((adjlist graph-al)) mygraph
    (push v2 (cadr (assoc v1 adjlist :test #'equal)))
    (values mygraph)))

;; TODO : make this work - currently won't modify the object in-place
(defmethod add-edge ((mygraph graph) v1 v2)
  (with-accessors ((adjlist graph-al)) mygraph
    (push (vneighbors (car (member v1 adjlist :test #'is-namep)))
	  v2))) 
