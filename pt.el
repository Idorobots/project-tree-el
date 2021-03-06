;; pt.el - A bunch of functions for general project tracking & productivity enhancement.

(require 'cl)

(defvar pt-state-init 'init)
(defvar pt-state-started 'started)
(defvar pt-state-done 'done)
(defvar pt-state-cancelled 'cancelled)

(defvar pt-color-unavailable "lightgray")
(defvar pt-fontcolor-unavailable "lightgray")
(defvar pt-color-available "black")
(defvar pt-fontcolor-available "black")
(defvar pt-color-started "blue")
(defvar pt-fontcolor-started "black")
(defvar pt-color-done "green")
(defvar pt-fontcolor-done "black")
(defvar pt-color-cancelled "red")
(defvar pt-fontcolor-cancelled "black")

(defvar pt-fillcolor-default "white")
(defvar pt-fillcolor-top "lightyellow")

(defvar pt-bgcolor "white")

(defvar pt-edge-color "gray")

(defvar pt-edge-style "")
(defvar pt-node-style "shape=box, style=\"rounded,filled\", penwidth=3.0")
(defvar pt-graph-style "rankdir=LR")


(defun pt (&rest nodes)
  "Function builds PTs. `nodes' are the same as args passed to `pt-node'."
  (pt-compute (mapcar (lambda (n)
                        (apply 'pt-node n))
                      nodes)))

(defun pt-node (id descr &optional pred state succ rank available-p)
  "`pred' is a list of node `id's. `succ', `rank' and `available-p' are recomputed automatically, best left unasigned."
  (list id
        descr
        (or state pt-state-init)
        pred
        succ
        (or rank 0)
        available-p))

(defun pt-get (graph id)
  (assoc id graph)) ;; NOTE Can be used as a predicate as well.

(defun pt-get-some (graph ids)
  (remove-if 'not
             (mapcar (lambda (id)
                       (pt-get graph id))
                     ids)))

(defun pt-set (graph node)
  (let ((id (pt-node-id node)))
    (cons node
          (remove-if (lambda (n)
                       (equal (pt-node-id n) id))
                     graph))))

(defun pt-node-id (node)
  (nth 0 node))

(defun pt-node-descr (node)
  (nth 1 node))

(defun pt-node-state (node)
  (nth 2 node))

(defun pt-node-started-p (node)
  (equal (pt-node-state node) pt-state-started))

(defun pt-node-done-p (node)
  (equal (pt-node-state node) pt-state-done))

(defun pt-node-init-p (node)
  (equal (pt-node-state node) pt-state-init))

(defun pt-node-cancelled-p (node)
  (equal (pt-node-state node) pt-state-cancelled))

(defun pt-node-pred (node)
  (nth 3 node))

(defun pt-node-succ (node)
  (nth 4 node))

(defun pt-node-top-p (node)
  (not (pt-node-succ node)))

(defun pt-node-rank (node)
  (nth 5 node))

(defun pt-node-available-p (node)
  (nth 6 node))

(defun pt-node-unavailable-p (node)
  (not (pt-node-available-p node)))

(defun pt-node-parents (node graph)
  (pt-get-some graph (pt-node-succ node)))

(defun pt-node-children (node graph)
  (pt-get-some graph (pt-node-pred node)))

(defun pt-node-color (node)
  (cond ((pt-node-done-p node) pt-color-done)
        ((pt-node-cancelled-p node) pt-color-cancelled)
        ((pt-node-started-p node) pt-color-started)
        ((pt-node-init-p node)
         (if (pt-node-available-p node)
             pt-color-available
           pt-color-unavailable))))

(defun pt-node-fontcolor (node)
  (cond ((pt-node-done-p node) pt-fontcolor-done)
        ((pt-node-cancelled-p node) pt-fontcolor-cancelled)
        ((pt-node-started-p node) pt-fontcolor-started)
        ((pt-node-init-p node)
         (if (pt-node-available-p node)
             pt-fontcolor-available
           pt-fontcolor-unavailable))))

(defun pt-node-fillcolor (node)
  (if (pt-node-top-p node)
      pt-fillcolor-top
    pt-fillcolor-default))

(defun pt-compute (graph)
  "Computes `succ', `rank' and `available-p' for each node. Does not alter graph structure."
  (pt-compute-ranks
   ;; NOTE Availability checks perform local rank computation. This is left here for completeness.
   (pt-compute-availability
    (pt-compute-succ graph))))

(defun pt-compute-succ (graph)
  (pt-compute-succ-iter graph graph))

(defun pt-compute-succ-iter (acc left)
  (if (not left)
      acc
    (let ((n (car left)))
      (pt-compute-succ-iter (pt-update-succ acc
                                            (pt-node-pred n)
                                            (pt-node-id n))
                            (cdr left)))))

(defun pt-update-succ (graph ids succ-id)
  (if (not ids)
      graph
    (let ((n (pt-get graph (car ids))))
      (pt-update-succ (pt-set graph
                              (pt-node (pt-node-id n)
                                       (pt-node-descr n)
                                       (pt-node-pred n)
                                       (pt-node-state n)
                                       (cons succ-id (pt-node-succ n))
                                       (pt-node-rank n)
                                       (pt-node-available-p n)))
                      (cdr ids)
                      succ-id))))

(defun pt-compute-ranks (graph)
  (pt-compute-ranks-iter '() graph graph))

(defun pt-compute-ranks-iter (acc left graph)
  (let ((n (car left)))
    (cond ((not left)
           acc)
          ((pt-get acc (pt-node-id n))
           (pt-compute-ranks-iter acc (cdr left) graph))
          (t
           (pt-compute-ranks-iter (cond ((pt-node-done-p n)
                                         (pt-update-rank acc n 0))
                                        ((pt-node-cancelled-p n)
                                         (pt-update-rank acc n 0))
                                        ((pt-node-top-p n)
                                         (pt-update-rank acc n 0))
                                        (t
                                         ;; NOTE We only need these computed for the let body.
                                         (let ((a (pt-compute-ranks-iter acc
                                                                         (pt-node-parents n graph)
                                                                         graph)))
                                           (pt-update-rank a
                                                           n
                                                           (+ 1 (apply 'max
                                                                       (mapcar 'pt-node-rank
                                                                               (pt-node-parents n a))))))))
                                  (cdr left)
                                  graph)))))

(defun pt-update-rank (graph node rank)
  (pt-set graph
          (pt-node (pt-node-id node)
                   (pt-node-descr node)
                   (pt-node-pred node)
                   (pt-node-state node)
                   (pt-node-succ node)
                   rank
                   (pt-node-available-p node))))

(defun pt-compute-availability (graph)
  (let ((sgs (mapcar (lambda (n)
                       ;; NOTE Subgraph-local rank computation.
                       (pt-compute-ranks
                        (pt-direct-subgraph n graph)))
                     (remove-if (lambda (n)
                                  (not (pt-node-top-p n)))
                                graph))))
    (pt-compute-availability-iter graph
                                  sgs
                                  (mapcar (lambda (s)
                                            (apply 'max
                                                   (mapcar 'pt-node-rank s)))
                                          sgs))))

(defun pt-compute-availability-iter (acc sub-graphs ranks)
  (if (not sub-graphs)
      acc
    (pt-compute-availability-iter (pt-update-availability-iter acc
                                                               (car sub-graphs)
                                                               (car ranks))
                                  (cdr sub-graphs)
                                  (cdr ranks))))

(defun pt-update-availability-iter (acc left rank)
  (if (not left)
      acc
    (let ((n (car left)))
      (pt-update-availability-iter (pt-update-available-p acc
                                                          (pt-node-id n)
                                                          (equal (pt-node-rank n) rank))
                                   (cdr left)
                                   rank))))

(defun pt-update-available-p (graph id available-p)
  (let ((n (pt-get graph id)))
    (pt-set graph
            (pt-node (pt-node-id n)
                     (pt-node-descr n)
                     (pt-node-pred n)
                     (pt-node-state n)
                     (pt-node-succ n)
                     (pt-node-rank n)
                     ;; NOTE A node can be available or not depending on different subgraphs.
                     (or (pt-node-available-p n) available-p)))))

(defun pt-all (f node graph)
  (pt-all-iter f
               '()
               (apply f (list node graph))
               graph))

(defun pt-all-iter (f acc nodes graph)
  (let ((n (car nodes)))
    (cond ((not nodes)
           acc)
          ((pt-get acc (pt-node-id (car nodes)))
           (pt-all-iter f acc (cdr nodes) graph))
          (t
           (pt-all-iter f
                        (cons n acc)
                        (append (apply f (list n graph))
                                (cdr nodes))
                        graph)))))

(defun pt-all-children (node graph)
  (pt-all 'pt-node-children node graph))

(defun pt-all-parents (node graph)
  (pt-all 'pt-node-parents node graph))

(defun pt-direct-subgraph (node graph)
  (cons node (pt-all-children node graph)))

(defun pt-top-parents (node graph)
  (remove-if (lambda (p)
               (not (pt-node-top-p p)))
             (pt-all-parents node graph)))

(defun pt-lexical-ordering (a b)
  ;; FIXME This actually works for some reason.
  (string< (pt-node-id a)
           (pt-node-id b)))

(defun pt-node->dot (node)
  (format "\"%s\"[label=\"%s\", fillcolor=%s, color=%s, fontcolor=%s];\n%s"
          (pt-node-id node)
          (pt-node-descr node)
          (pt-node-fillcolor node)
          (pt-node-color node)
          (pt-node-fontcolor node)
          (let ((id (pt-node-id node)))
            (apply 'concat
                   (mapcar (lambda (req)
                             (format "\"%s\" -> \"%s\";\n" req id))
                           (pt-node-pred node))))))

(defun pt->dot (graph)
  (format "digraph PT {\nbgcolor=%s;\n%s;\nedge[color=%s, %s];\nnode[%s, fillcolor=%s, color=%s, fontcolor=%s];\n%s}"
          pt-bgcolor
          pt-graph-style
          pt-edge-color
          pt-edge-style
          pt-node-style
          pt-fillcolor-default
          pt-color-unavailable
          pt-fontcolor-unavailable
          (apply 'concat
                 (mapcar 'pt-node->dot
                         (sort graph
                               'pt-lexical-ordering)))))

(defun pt->png (graph filename)
  (let ((tmp-file (concat "/tmp/" (md5 filename) ".dot"))
        (contents (pt->dot graph)))
    (with-temp-buffer
      (insert contents)
      (write-file tmp-file))
    (shell-command-to-string
     (concat "ccomps -y " tmp-file " | "
             "dot | "
             "unflatten | "
             "gvpack -array3 | "
             "neato -Tpng -n2 -o " filename))))

(provide 'pt)
