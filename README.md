# markdown-tables
Generates Markdown tables from Lisp objects

# Install

Simply clone this repository to `~/common-lisp` or wherever ASDF can find it.

# Use

```lisp
;; First, load the markdown-tables package using any way you prefer.

;; Define a struct to be outputted on each row of the table
(defstruct country
  name
  capital
  gdp)
  
;; Generate a sequence of such structs or objects
(defparameter *data*
  (list <many-entries-here...>))

;; Create a table object.
;; Use the column-spec parameter to control column titles, alignment, and formatting.
;; For each column, you must specify the slot of the objects
;;    that contain the values for that column.
(defparameter *table*
  (make-table *data*
	      :title "Countries by gross domestic product (2020)"
	      :column-spec `((:label "Country" :slot name)
			     (:label "Capital" :slot capital)
			     (:label "GDP (billions of current US dollars)"
				     :slot gdp
				     :align markdown-tables::right
				     :formatter ,#'format-gdp))))

;; Output table object as Markdown.
;; (Optionally compute the sum of a column)
(output-markdown-file "GDP.md" *table* :sum-slot 'gdp)
```
Please see [example.lisp](example.lisp) for full details.

After running the code, `GDP.md` contains:

```markdown
Countries by gross domestic product (2020)

|Country|Capital|GDP (billions of current US dollars)|
|---|---|---:|
|China|Beijing|14722.731|
|Japan|Tokyo|5057.759|
|United States|Washington, D.C.|20953.030|
|Germany|Berlin|3846.414|

Total: 44579.938

```

Other tools can process the Markdown code to give this table:

Countries by gross domestic product (2020)

|Country|Capital|GDP (billions of current US dollars)|
|---|---|---:|
|China|Beijing|14722.731|
|Japan|Tokyo|5057.759|
|United States|Washington, D.C.|20953.030|
|Germany|Berlin|3846.414|

Total: 44579.938

# Author, License
Copyright ©️ 2022 Alan Tseng

MIT License
