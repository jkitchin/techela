;;; techela.el --- techela utilities

;; These need to be defined in .dir-locals.el

;; techela-types - a list of assignment categories
;; techela-graders - a list of names
;; techela-rubrics - a list of rubrics
;; techela-admin-box-path - a string to your box admin path

;;; Commentary:
;;

;;; Code:

(use-package ox-ipynb
  :load-path (lambda () (expand-file-name "ox-ipynb" scimax-dir)))



(defun techela-assign ()
  "Export the current heading to an ipynb.
It is assumed your org file is in the assignments directory. The
ipynb will be named with the label corresponding to the heading
you are in."
  (interactive)
  (save-restriction
    (org-narrow-to-subtree)
    (unless (org-entry-get nil "LABEL")
      (org-entry-put nil "LABEL" (completing-read "Label: " (org-map-entries
							     (lambda ()
							       (org-entry-get nil "LABEL"))))))
    (unless (org-entry-get nil "POINTS")
      (org-entry-put nil "POINTS" (read-string "Points: ")))

    (unless (org-entry-get nil "TYPE")
      (org-entry-put nil "TYPE" (completing-read "Type: " techela-types)))

    (unless (org-entry-get nil "RUBRIC")
      (org-entry-put nil "RUBRIC" (completing-read "Rubric: " techela-rubrics)))

    (unless (org-entry-get nil "DUEDATE")
      (org-entry-put nil "DUEDATE" (concat (org-read-date t nil nil nil nil) " 23:59:59")))

    (unless (org-entry-get nil "GRADER")
      (org-entry-put nil "GRADER" (completing-read "Grader: " techela-graders)))

    (let* ((label (or (org-entry-get nil "LABEL")
		      (read-string "Label: ")))
	   (points (org-entry-get nil "POINTS"))
	   (duedate (org-entry-get nil "DUEDATE"))
	   (type (org-entry-get nil "TYPE"))
	   (rubric (org-entry-get nil "RUBRIC"))
	   (body (progn
		   (org-end-of-meta-data t) (buffer-substring (point) (point-max))))
	   (grader (org-entry-get nil "GRADER"))
	   (org-file (concat label ".org"))
	   (ipynb (concat label ".ipynb"))
	   (content (s-format "#+OX-IPYNB-KEYWORD-METADATA: assignment points category rubric duedate grader
#+ASSIGNMENT: ${label}
#+POINTS: ${points}
#+DUEDATE: ${duedate}
#+CATEGORY: ${type}
#+RUBRIC: ${rubric}
#+GRADER: ${grader}

${body}" 'aget (list (cons "label" label)
		     (cons "points" points)
		     (cons "duedate" duedate)
		     (cons "type" type)
		     (cons "rubric" rubric)
		     (cons "body" body)
		     (cons "grader" grader)))))

      (org-entry-put nil "DUEDATE" duedate)
      (org-todo nil)

      (with-temp-file org-file
	(insert content))
      (with-current-buffer (find-file org-file)
	(let ((org-export-exclude-tags '("noexport" "solution"))
	      (org-export-with-author nil)
	      (org-export-with-title nil)
	      (export-file-name ipynb))
	  (ox-ipynb-export-to-ipynb-file-and-open)))
      (org-todo "ASSIGNED")
      (kill-buffer org-file)
      (delete-file org-file))))


(defun techela-solution ()
  "Make a solution ipynb in `techela-admin-box-path' from the current heading.
You have to copy this file to the course github repo when you are
ready to release it."
  (interactive)
  (save-restriction
    (org-narrow-to-subtree)

    (let* ((label (or (org-entry-get nil "LABEL")
		      (read-string "Label: ")))
	   (points (org-entry-get nil "POINTS"))
	   (duedate (org-entry-get nil "DUEDATE"))
	   (type (org-entry-get nil "TYPE"))
	   (rubric (org-entry-get nil "RUBRIC"))
	   (body (progn
		   (org-end-of-meta-data t)
		   (buffer-substring (point) (point-max))))
	   (org-file (concat  techela-admin-box-path "/solutions/" label ".org"))
	   (ipynb (concat  techela-admin-box-path "/solutions/" label ".ipynb"))
	   (content (s-format "#+ASSIGNMENT: ${label}
#+POINTS: ${points}
#+DUEDATE: ${duedate}
#+CATEGORY: ${type}
#+RUBRIC: ${rubric}

${body}" 'aget (list (cons "label" label)
		     (cons "points" points)
		     (cons "duedate" duedate)
		     (cons "type" type)
		     (cons "rubric" rubric)
		     (cons "body" body)))))

      (org-entry-put nil "DUEDATE" duedate)

      (with-temp-file org-file
	(insert content))
      (let ((export-file-name ipynb)
	    (org-export-exclude-tags '("noexport")))
	(ox-ipynb-export-to-ipynb-file-and-open))
      (delete-file org-file))))

(provide 'techela)

;;; techela.el ends here
