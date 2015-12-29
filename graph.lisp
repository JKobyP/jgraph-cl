(in-package :edu.case.acm-people.jkp46.graph)
;;; *** Utilities ****************************************************

;;; *** Base class for GRAPH *****************************************
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

; TODO: Implement removing edges to the vertex
(defgeneric remove-vertex (graph name)
  (:documentation "remove a vertex from graph by name"))
 
(defgeneric add-vertex (graph name &optional neighbors)
  (:documentation "add the named vertex to the graph if \
                   one of the same name does not already exist. \
                   Neighbors MUST be passed as a list, \
                   name must be a symbol."))

(defgeneric search-adjlist (graph name)
  (:documentation "searches for a vertex by name"))

(defmethod add-vertex :before ((mygraph graph) name &optional neighbors)
  (check-type neighbors list)
  (check-type name symbol))

(defgeneric add-edge (graph v1 v2)
  (:documentation "add a directed edge from v1 to v2")) 

(defmethod add-edge :before (graph v1 v2)
  (check-type v1 symbol)
  (check-type v2 symbol))

(defgeneric add-vertexc (graph name &optional neighbors)
  (:documentation "wrapper on add-vertex which returns the graph
                   for chaining calls."))

(defmethod add-vertexc ((g graph) name &optional neighbors)
  (add-vertex g name neighbors)
  g)

(defgeneric remove-vertexc (graph name)
  (:documentation "wrapper on remove-vertex which returns the graph
                   for chaining calls."))

(defmethod remove-vertexc ((g graph) name)
  (remove-vertex g name)
  g)

(defgeneric emptyp (graph)
  (:documentation "returns true if the graph is empty"))

(defmethod emptyp ((g graph))
  (not (first (graph-al g))))

;;; ***  For an OBJECT oriented approach ******************************

;; Graph that contains Vertex objects as its primary structure
;;      The edge-list will remain unimplemented until it is necessary
(defclass vgraph (graph) 
  ((edge-list
     :accessor vg-edge-list
     :initarg :edge-list
     :initform NIL)))

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

(defun make-vertex (name neighbors)
  (make-instance 'vertex :name name :neighbors neighbors))

;; Utility function determines if the given vertex has name <node-name>
(defun is-namep (node-name vertex)
  (equal (vname vertex) node-name))

(defmethod get-neighbors ((mygraph vgraph) node) 
  (with-accessors ((adjlist graph-al)) mygraph
    (vneighbors
     (find node adjlist :test
       #'is-namep))))

(defmethod show-graph ((mygraph vgraph))
    (dolist (v (graph-al mygraph))
      (format t "~a -> ~a~%"
        (vname v)
        (vneighbors v))))

(defmethod add-vertex ((mygraph vgraph) name &optional neighbors)
  (let ((already-exists (search-adjlist mygraph name))
        (new-vertex (make-vertex name neighbors)))
    (when (not already-exists)
        (return-from add-vertex (push new-vertex (graph-al mygraph))))
    already-exists))

(defmethod add-edge ((mygraph vgraph) v1 v2)
  (nconc (vneighbors (car (search-adjlist mygraph v1)))
           v2))

(defmethod remove-vertex ((mygraph vgraph) to-destroy)
  (with-accessors ((adjlist graph-al)) mygraph
    (setq adjlist (delete-if
                    (lambda (current-vertex) ; A fun example of currying!
                      (is-namep to-destroy current-vertex))
                    adjlist))))

(defmethod search-adjlist ((g vgraph) key)
  (member key (graph-al g) :test #'is-namep))
