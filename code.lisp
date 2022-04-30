;; Copyright 2022 Alan Tseng
;; 
;; Permission is hereby granted, free of charge, to any person obtaining a 
;; copy of this software and associated documentation files (the "Software"), 
;; to deal in the Software without restriction, including without limitation 
;; the rights to use, copy, modify, merge, publish, distribute, sublicense, 
;; and/or sell copies of the Software, and to permit persons to whom the 
;; Software is furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in 
;; all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
;; DEALINGS IN THE SOFTWARE.

(uiop:define-package :markdown-tables
    (:mix :closer-mop :cl)
  (:export :make-table
	   :output-markdown
	   :output-markdown-file))
(in-package :markdown-tables)

(defclass table ()
  ((title :initarg :title)
   (column-spec :initarg :column-spec)
   (rows :initarg :rows))
  (:documentation "A table that can be represented in Markdown"))

(defun sum-objects (objects slot)
  "Computes the total of the objects' values in a particular slot."
  (reduce (lambda (acc x)
	    (+ acc (slot-value x slot)))
	  objects :initial-value 0))

(defun slot-names (obj)
  "Returns list of names of slots belonging to an object."
  (mapcar #'slot-definition-name
	  (class-slots
	   (class-of obj))))

(defun auto-column-spec (obj)
  "By default, list every column as the slots of object."
  (loop for slot in (slot-names obj)
     collect `(:label ,(string slot)
		      :slot ,slot)))

(defun make-table (rows &key (title nil) (column-spec nil))
  "Returns a table object."
  (make-instance 'table
   :title title
	      :rows rows
	      :column-spec
	      (if (and rows (not column-spec))
		  (auto-column-spec (elt rows 0))
		  column-spec)))

(defun fnil (&rest args)
  "Shorthand for (format nil ...)"
  (apply #'format (cons nil args)))
;; (fnil "~A ~A" 1 2)

(defun default-formatter ()
  (lambda (x)
    (if (not x)
	""
	(fnil "~A" x))))

(defun float-formatter (decimal-places)
  "Returns a function that converts its argument to string."
  (let ((format-str (fnil "~~,~Af" decimal-places)))
    (lambda (x)
      (fnil format-str x))))
;; (funcall (float-formatter 3) pi)

;; Column spec is a plist containing these keys
;; (one for each column):
;;   slot label formatter align

;; label: text to be displayed in header column
;; slot: symbol of the slot containing the object's data
;; formatter: function that takes object's data and returns a string
;; align: either 'left, 'center, or 'right

(defun row-text (lst)
  (fnil "|~{~A~^|~}|" lst))
;;p (row-text '(1 2 3)) ;=> |1|2|3|

(defun get-props (list-of-plists key default)
  "Returns the value of key for every plist in a
list of plists."
  (mapcar (lambda (pl)
	    (getf pl key default))
	  list-of-plists))

(defun markdown-alignment-string (align)
  (case align
    (left ":---")
    (center ":---:")
    (right "---:")
    (t "---")))

(defun concat-lines (&rest lst)
  (fnil "~{~A~^~%~}" lst))
;; (concat-lines 1 2 3)

(defun col-header (column-spec)
  (concat-lines
   (row-text (get-props column-spec :label ""))
   (row-text (mapcar #'markdown-alignment-string
		     (get-props column-spec :align nil)))))
;; (col-header '((:label "X") (:label "Y" :align right)))

(defun body-text (column-spec rows)
  (let ((slot-names (get-props column-spec :slot
			       'slot-doesnt-exist))
	(formatters (get-props column-spec :formatter
			       (default-formatter))))
    (apply #'concat-lines
	   (loop for row in rows collect
		(row-text
		 (loop for name in slot-names
		    for fmt in formatters
		    collect (funcall fmt (slot-value row name))))))))

(defun find-formatter (table-object slot)
  "Finds the formatter for a specific slot in
an already-initialized table object."
  (with-slots (column-spec) table-object
    (loop for s in column-spec
       if (equal (getf s :slot) slot)
	 return (getf s :formatter))))

(defun write-to-stream (stream x &optional (newline t))
  (format stream "~A" x)
  (when newline (terpri stream)))

(defun output-markdown (stream table-object &key (sum-slot nil))
  "Writes the table in Markdown form to the stream."
  (with-slots (title column-spec rows total) table-object
    (flet ((ws (x) (write-to-stream stream x)))
      (ws title)
      (terpri stream)
      (ws (col-header column-spec))
      (ws (body-text column-spec rows))
      (when sum-slot
	(terpri stream)
	(ws (fnil "Total: ~A"
		  ;; Make sure to format the total
		  ;; in the same way as the column being summed.
		  (funcall
		   (find-formatter table-object sum-slot)
		   (sum-objects rows sum-slot)))))
      (terpri stream))))

(defmacro with-output-file (stream filename &body body)
  `(with-open-file (,stream ,filename
			    :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
     ,@body))

(defun output-markdown-file (file table-object &key (sum-slot nil))
  "Writes table in Markdown format to the specified file.
  Existing file will be overwritten."
  (with-output-file stream file
    (output-markdown stream table-object :sum-slot sum-slot)))

