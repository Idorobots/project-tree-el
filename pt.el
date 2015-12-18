;; pt.el - A bunch of functions for general project tracking & productivity enhencement.

(require 'cl)

(defvar pt-state-init 'init)
(defvar pt-state-started 'started)
(defvar pt-state-done 'done)

(defvar pt-color-unavailable "lightgray")
(defvar pt-fontcolor-unavailable "lightgray")
(defvar pt-color-available "black")
(defvar pt-fontcolor-available "black")
(defvar pt-color-started "blue")
(defvar pt-fontcolor-started "black")
(defvar pt-color-done "green")
(defvar pt-fontcolor-done "black")

(defvar pt-fillcolor-default "white")
(defvar pt-fillcolor-top "lightyellow")

(defvar pt-edge-style "color=gray")
(defvar pt-node-style "shape=box, style=\"rounded,filled\", penwidth=3.0")
(defvar pt-graph-style "rankdir=LR")

(defun pt-goal (id descr state pred &optional succ rank available-p)
  (list id
        descr
        state
        pred
        succ
        (or rank 0)
        available-p))

(defun pt-get (goals id)
  (assoc id goals))

(defun pt-get-some (goals ids)
  (mapcar (lambda (id)
            (pt-get goals id))
          ids))

(defun pt-set (goals goal)
  (let ((id (pt-goal-id goal)))
    (cons goal
          (remove-if (lambda (g)
                       (equal (pt-goal-id g) id))
                     goals))))

(defun pt-goal-id (goal)
  (nth 0 goal))

(defun pt-goal-descr (goal)
  (nth 1 goal))

(defun pt-goal-state (goal)
  (nth 2 goal))

(defun pt-goal-started-p (goal)
  (equal (pt-goal-state goal) pt-state-started))

(defun pt-goal-done-p (goal)
  (equal (pt-goal-state goal) pt-state-done))

(defun pt-goal-init-p (goal)
  (equal (pt-goal-state goal) pt-state-init))

(defun pt-goal-pred (goal)
  (nth 3 goal))

(defun pt-goal-succ (goal)
  (nth 4 goal))

(defun pt-goal-top-p (goal)
  (not (pt-goal-succ goal)))

(defun pt-goal-rank (goal)
  (nth 5 goal))

(defun pt-goal-available-p (goal)
  (nth 6 goal))

(defun pt-goal-unavailable-p (goal)
  (not (pt-goal-available-p goal)))

(defun pt-goal-parents (goal goals)
  (pt-get-some goals (pt-goal-succ goal)))

(defun pt-goal-children (goal goals)
  (pt-get-some goals (pt-goal-pred goal)))

(defun pt-goal-color (goal)
  (cond ((pt-goal-done-p goal) pt-color-done)
        ((pt-goal-started-p goal) pt-color-started)
        ((pt-goal-init-p goal)
         (if (pt-goal-available-p goal)
             pt-color-available
           pt-color-unavailable))))

(defun pt-goal-fontcolor (goal)
  (cond ((pt-goal-done-p goal) pt-fontcolor-done)
        ((pt-goal-started-p goal) pt-fontcolor-started)
        ((pt-goal-init-p goal)
         (if (pt-goal-available-p goal)
             pt-fontcolor-available
           pt-fontcolor-unavailable))))

(defun pt-goal-fillcolor (goal)
  (if (pt-goal-top-p goal)
      pt-fillcolor-top
    pt-fillcolor-default))

(defun pt-compute (goals)
  (pt-compute-availability
   (pt-compute-ranks
    (pt-compute-succ goals))))

(defun pt-compute-succ (goals)
  (pt-compute-succ-acc goals goals))

(defun pt-compute-succ-acc (acc goals)
  (if (not goals)
      acc
    (let ((g (car goals)))
      (pt-compute-succ-acc (pt-update-succ acc
                                           (pt-goal-pred g)
                                           (pt-goal-id g))
                           (cdr goals)))))

(defun pt-update-succ (goals ids succ-id)
  (if (not ids)
      goals
    (let ((g (pt-get goals (car ids))))
      (pt-update-succ (pt-set goals
                              (pt-goal (pt-goal-id g)
                                       (pt-goal-descr g)
                                       (pt-goal-state g)
                                       (pt-goal-pred g)
                                       (cons succ-id (pt-goal-succ g))))
                      (cdr ids)
                      succ-id))))

(defun pt-compute-ranks (goals)
  (pt-compute-ranks-acc '() goals goals))

(defun pt-compute-ranks-acc (acc left goals)
  (let ((g (car left)))
    (cond ((not left)
           acc) ;; NOTE Done.
          ((pt-get acc (pt-goal-id g))
           (pt-compute-ranks-acc acc (cdr left) goals)) ;; NOTE Skip if already processed.
          (t
           (cond ((pt-goal-done-p g)
                  (pt-compute-ranks-acc (pt-update-rank acc g 0)
                                        (cdr left)
                                        goals))
                 ((not (pt-goal-pred g))
                  (pt-compute-ranks-acc (pt-update-rank acc g 1)
                                        (cdr left)
                                        goals))
                 (t
                  (let ((a (pt-compute-ranks-acc acc ;; NOTE We only need these computed for the let body.
                                                 (pt-goal-children g goals)
                                                 goals)))
                    (pt-compute-ranks-acc (pt-update-rank a
                                                          g
                                                          (+ 1 (apply 'max
                                                                      (mapcar 'pt-goal-rank
                                                                              (pt-goal-children g a)))))
                                          (cdr left)
                                          goals))))))))

(defun pt-update-rank (goals goal rank)
  (pt-set goals
          (pt-goal (pt-goal-id goal)
                   (pt-goal-descr goal)
                   (pt-goal-state goal)
                   (pt-goal-pred goal)
                   (pt-goal-succ goal)
                   rank)))

(defun pt-compute-availability (goals)
  (pt-compute-availability-acc '() goals goals))

(defun pt-compute-availability-acc (acc left goals)
  (if (not left)
      acc
    (let* ((g (car left))
           (a (cond ((not (equal (pt-goal-rank g) 1)) nil)
                    ((pt-goal-top-p g) t)
                    (t (<= (apply 'max
                                  (mapcar 'pt-goal-rank
                                          (pt-goal-children (pt-min-rank (pt-goal-parents g goals))
                                                            goals)))
                           1)))))
      (pt-compute-availability-acc (pt-update-available-p acc g a)
                                   (cdr left)
                                   goals))))

(defun pt-update-available-p (goals goal available-p)
  (pt-set goals
          (pt-goal (pt-goal-id goal)
                   (pt-goal-descr goal)
                   (pt-goal-state goal)
                   (pt-goal-pred goal)
                   (pt-goal-succ goal)
                   (pt-goal-rank goal)
                   available-p)))

(defun pt-min-rank (nodes)
  (pt-min-rank-acc (car nodes) (cdr nodes)))

(defun pt-min-rank-acc (acc nodes)
  (cond ((not nodes) acc)
        ((< (pt-goal-rank acc)
            (pt-goal-rank (car nodes)))
         (pt-min-rank-acc acc (cdr nodes)))
        (t (pt-min-rank-acc (car nodes) (cdr nodes)))))

(defun pt-goal->dot (goal)
  (format "\"%s\"[label=\"%s\", fillcolor=%s, color=%s, fontcolor=%s];\n%s"
          (pt-goal-id goal)
          (pt-goal-descr goal)
          (pt-goal-fillcolor goal)
          (pt-goal-color goal)
          (pt-goal-fontcolor goal)
          (let ((id (pt-goal-id goal)))
            (apply 'concat
                   (mapcar (lambda (req)
                             (format "\"%s\" -> \"%s\";\n" req id))
                           (pt-goal-pred goal))))))

(defun pt-goals->dot (goals)
  (format "digraph PT {\n%s;\nedge[%s];\nnode[%s, fillcolor=%s, color=%s, fontcolor=%s];\n%s}"
          pt-graph-style
          pt-edge-style
          pt-node-style
          pt-fillcolor-default
          pt-color-unavailable
          pt-fontcolor-unavailable
          (apply 'concat (mapcar 'pt-goal->dot goals))))

(defun pt-goals->png (goals filename)
  (let ((tmp-file (concat "/tmp/" (md5 filename) ".dot"))
        (contents (pt-goals->dot goals)))
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
