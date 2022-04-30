(ql:quickload 'markdown-tables)

(defpackage :markdown-table-example
  (:use :cl :markdown-tables))
(in-package :markdown-table-example)

;; Definition of a struct to be outputted on each row of the table
(defstruct country
  name
  capital
  gdp)

(defun make-entry (name capital gdp)
  (make-country :name name
		:capital capital
		:gdp gdp))

;; Have a list of structs or objects containing data to be put on the table.
;; (This data is from the World Bank for the year 2020.
;; https://data.worldbank.org/indicator/NY.GDP.MKTP.CD )
(defparameter *data*
  (list (make-entry "China" "Beijing" 14722730.70)
	(make-entry "Japan" "Tokyo" 5057758.96)
	(make-entry "United States" "Washington, D.C." 20953030.00)
	(make-entry "Germany" "Berlin" 3846413.93)))

(defun format-gdp (x)
  ;; Divide millions by 1000 to get billions.
  ;; Return a string to 3 decimal places.
  (format nil "~,3f" (/ x 1000)))

;; Create table object
(defparameter *table*
  (make-table *data*
	      :title "Countries by gross domestic product (2020)"
	      :column-spec `((:label "Country" :slot name)
			     (:label "Capital" :slot capital)
			     (:label "GDP (billions of current US dollars)"
				     :slot gdp
				     :align right
				     :formatter ,#'format-gdp))))

;; Output table object as Markdown.
;; (Optionally compute the sum of a column)
(output-markdown-file "GDP.md" *table* :sum-slot 'gdp)

