;; A bunch of functions for general project tracking & productivity enhencement.

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

(defun pt-goal-id (goal)
  (nth 0 goal))

(defun pt-goal-description (goal)
  (nth 1 goal))

(defun pt-goal-state (goal)
  (nth 2 goal))

(defun pt-goal-requirements (goal)
  (nth 3 goal))
