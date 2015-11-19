(in-package :edu.case.acm-people.jkp46.graph)
(compile-file "graph-test.lisp")
(load "graph-test.lisp")

(setq lisp-unit:*print-failures* t)
(setq lisp-unit:*print-errors* t)
(setq lisp-unit:*print-summary* t)
(run-tests)
