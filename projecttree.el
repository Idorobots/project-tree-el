;; A bunch of functions for general project tracking & productivity enhencement.

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

(defun pt-goal (id description state requirements)
  (list id description state requirements))

(defun pt-get (goals id)
  (assoc id goals))

(defun pt-goal-id (goal)
  (nth 0 goal))

(defun pt-goal-description (goal)
  (nth 1 goal))

(defun pt-goal-state (goal)
  (nth 2 goal))

(defun pt-goal-requirements (goal)
  (nth 3 goal))

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
  (let ((req (mapcar 'pt-goal-done-p
                     (mapcar (lambda (x)
                               (pt-get goals x))
                             (pt-goal-requirements goal)))))
    (or (not req)
        (reduce (lambda (a b) (and a b)) req))))

(defun pt-goal-parents (goal goals)
  (let ((id (pt-goal-id goal)))
    (remove-if-not (lambda (g)
                     (member id (pt-goal-requirements g)))
                   goals)))

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
