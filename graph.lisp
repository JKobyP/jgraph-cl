(in-package :edu.case.acm-people.jkp46.graph)

; TODO : reorganize object hierarchy and think about inheritance/polymorphism
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

(defgeneric add-vertex (graph name &optional neighbors)
  (:documentation "add the named vertex to the graph if \
                   one of the same name does not already exist. \
                   Neighbors MUST be passed as a list, \
                   name must be a symbol."))

(defgeneric add-edge (graph v1 v2)
  (:documentation "add a directed edge from v1 to v2")) 

; TODO: Implement removing edges to the vertex
(defgeneric remove-vertex (graph name)
  (:documentation "remove a vertex from graph by name"))

;;; *** For a association-list (ALIST) based approach ****************
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
  (check-type neighbors list)
  (check-type name symbol)
  (with-accessors ((adjlist graph-al)) mygraph
    (if (not (find name adjlist :test #'is-namep))
         (push (cons name (cons neighbors nil)) adjlist))
    (values mygraph)))

(defmethod add-edge ((mygraph list-graph) v1 v2)
  (with-accessors ((adjlist graph-al)) mygraph
    (push v2 (cadr (assoc v1 adjlist :test #'equal)))
    (values mygraph)))
 
(defmethod remove-vertex ((mygraph list-graph) to-destroy)
  (with-accessors ((adjlist graph-al)) mygraph
    (setf adjlist (delete (assoc to-destroy adjlist) adjlist))))

;;; ***  For an OBJECT oriented approach ******************************
; TODO: Implement pointer-based system with 
;       the proper AST specs (edge-list and node-list)
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

;; Utility function determines if the given vertex has name <node-name>
(defun is-namep (node-name vertex)
  (equal (vname vertex) node-name))

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
  (check-type neighbors list)
  (check-type name symbol)
  (with-accessors ((adjlist graph-al)) mygraph
    (if (not (find name adjlist :test #'is-namep))
      (push (make-instance 'vertex :name name :neighbors neighbors) adjlist)))
  (values mygraph))

(defmethod add-edge ((mygraph graph) v1 v2)
  (with-accessors ((adjlist graph-al)) mygraph
    (nconc (vneighbors (car (member v1 adjlist :test #'is-namep)))
           v2)))

(defmethod remove-vertex ((mygraph graph) to-destroy)
  (with-accessors ((adjlist graph-al)) mygraph
    (setq adjlist 
          (delete-if 
            (lambda (current-vertex) ; This is a fun example of currying!
              (is-namep to-destroy current-vertex)) 
            adjlist))))
