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
(defvar pt-fillcolor-last "lightyellow")

(defvar pt-edge-style "color=gray")
(defvar pt-node-style "shape=box, style=\"rounded,filled\", penwidth=3.0")
(defvar pt-graph-style "rankdir=LR")

(defun pt-goal (id description state requirements &optional required-by rank availability)
  (list id
        description
        state
        requirements
        required-by
        (or rank 0)
        availability))

(defun pt-get (goals id)
  (assoc id goals))

(defun pt-set (goals goal)
  (let ((id (pt-goal-id goal)))
    (cons goal
          (remove-if (lambda (g)
                       (equal (pt-goal-id g) id))
                     goals))))

(defun pt-goal-id (goal)
  (nth 0 goal))

(defun pt-goal-description (goal)
  (nth 1 goal))

(defun pt-goal-state (goal)
  (nth 2 goal))

(defun pt-goal-requirements (goal)
  (nth 3 goal))

(defun pt-goal-required-by (goal)
  (nth 4 goal))

(defun pt-goal-init-p (goal)
  (equal (pt-goal-state goal) pt-state-init))

(defun pt-goal-started-p (goal)
  (equal (pt-goal-state goal) pt-state-started))

(defun pt-goal-done-p (goal)
  (equal (pt-goal-state goal) pt-state-done))

(defun pt-goal-last-p (goal goals)
  (not (pt-goal-parents goal goals)))

(defun pt-goal-unavailable-p (goal goals)
  (not (pt-goal-available-p goal goals)))

(defun pt-goal-available-p (goal goals)
  (cond ((not (equal (pt-goal-rank goal) 1)) nil)
        ((pt-goal-last-p goal goals) t)
        (t (<= (apply 'max
                      (mapcar (lambda (c)
                                (pt-goal-rank c))
                              (pt-goal-children (pt-min-rank (pt-goal-parents goal goals))
                                                goals)))
               1))))

(defun pt-goal-rank (goal)
  (nth 5 goal))

(defun pt-min-rank (nodes)
  (pt-min-rank-acc (car nodes) (cdr nodes)))

(defun pt-min-rank-acc (acc nodes)
  (cond ((not nodes) acc)
        ((< (pt-goal-rank acc)
            (pt-goal-rank (car nodes)))
         (pt-min-rank-acc acc (cdr nodes)))
        (t (pt-min-rank-acc (car nodes) (cdr nodes)))))

(defun pt-goal-parents (goal goals)
  (mapcar (lambda (id)
            (pt-get goals id))
          (pt-goal-required-by goal)))

(defun pt-goal-children (goal goals)
  (mapcar (lambda (id)
            (pt-get goals id))
          (pt-goal-requirements goal)))

(defun pt-goal-color (goal goals)
  (cond ((pt-goal-done-p goal) pt-color-done)
        ((pt-goal-started-p goal) pt-color-started)
        ((pt-goal-init-p goal)
         (if (pt-goal-available-p goal goals)
             pt-color-available
           pt-color-unavailable))))

(defun pt-goal-fontcolor (goal goals)
  (cond ((pt-goal-done-p goal) pt-fontcolor-done)
        ((pt-goal-started-p goal) pt-fontcolor-started)
        ((pt-goal-init-p goal)
         (if (pt-goal-available-p goal goals)
             pt-fontcolor-available
           pt-fontcolor-unavailable))))

(defun pt-goal-fillcolor (goal goals)
  (if (pt-goal-last-p goal goals)
      pt-fillcolor-last
    pt-fillcolor-default))

(defun pt-compute (goals)
  (pt-compute-availability
   (pt-compute-ranks
    (pt-compute-parents goals))))

(defun pt-compute-parents (goals)
  (pt-compute-parents-acc goals goals))

(defun pt-compute-parents-acc (acc goals)
  (if (not goals)
      acc
    (let ((g (car goals)))
      (pt-compute-parents-acc (pt-update-parents acc
                                                 (pt-goal-requirements g)
                                                 (pt-goal-id g))
                              (cdr goals)))))

(defun pt-update-parents (goals ids parent-id)
  (if (not ids)
      goals
    (let ((g (pt-get goals (car ids))))
      (pt-update-parents (pt-set goals
                                 (pt-goal (pt-goal-id g)
                                          (pt-goal-description g)
                                          (pt-goal-state g)
                                          (pt-goal-requirements g)
                                          (cons parent-id (pt-goal-required-by g))))
                         (cdr ids)
                         parent-id))))

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
                 ((not (pt-goal-requirements g))
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
                   (pt-goal-description goal)
                   (pt-goal-state goal)
                   (pt-goal-requirements goal)
                   (pt-goal-required-by goal)
                   rank)))

(defun pt-compute-availability (goals)
  ;; TODO compute availability
  goals)

(defun pt-goal->dot (goal goals)
  (format "\"%s\"[label=\"%s\", fillcolor=%s, color=%s, fontcolor=%s];\n%s"
          (pt-goal-id goal)
          (pt-goal-description goal)
          (pt-goal-fillcolor goal goals)
          (pt-goal-color goal goals)
          (pt-goal-fontcolor goal goals)
          (let ((id (pt-goal-id goal)))
            (apply 'concat
                   (mapcar (lambda (req)
                             (format "\"%s\" -> \"%s\";\n"
                                     req id))
                           (pt-goal-requirements goal))))))

(defun pt-goals->dot (goals)
  (format "digraph PT {\n%s;\nedge[%s];\nnode[%s, fillcolor=%s, color=%s, fontcolor=%s];\n%s}"
          pt-graph-style
          pt-edge-style
          pt-node-style pt-fillcolor-default pt-color-unavailable pt-fontcolor-unavailable
          (apply 'concat
                 (mapcar (lambda (g)
                           (pt-goal->dot g goals))
                         goals))))

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
