
(defun file-contents (pathname &key (element-type '(unsigned-byte 8)) (external-format :utf-8))
  (with-open-file (s pathname :element-type element-type :external-format external-format)
    (let ((result
           (make-array (file-length s) :element-type element-type)))
      (read-sequence result s)
      result)))

(defun parse-flag (string)
  (and (equal "x" string)
       t))

(defmacro do-with-tetra-parameters (() &body body)
  ; unhygienic
  `(dolist (def (mapcar (alexandria:curry #'cl-ppcre:split "\\t")
                        (cl-ppcre:split "\\r"
                                        (cl-ppcre:regex-replace-all "\\n"
                                                                    (file-contents "tetra-parameters.txt"
                                                                                   :external-format :cp437
                                                                                   :element-type 'character)
                                                                    ""))))
     (destructuring-bind (param name short-name gui-title
                                range type comment
                                nrpn &optional suppress-b suppress-v1 suppress-v2 suppress-v3 suppress-v4 &rest junk) def
       (declare (ignorable param name short-name gui-title
                           range type comment
                           nrpn suppress-b suppress-v1 suppress-v2 suppress-v3 suppress-v4 junk))
       (unless (or (equal param "Parm")
                   (equal name ""))
         (setf param (parse-integer param)
               nrpn (parse-integer nrpn)
               suppress-b (parse-flag suppress-b)
               suppress-v1 (parse-flag suppress-v1)
               suppress-v2 (parse-flag suppress-v2)
               suppress-v3 (parse-flag suppress-v3)
               suppress-v4 (parse-flag suppress-v4)
               type (unless (equal "" type) type))
         ,@body))))

(defun make-tetra-controls ()
  (do-with-tetra-parameters ()
    (let* ((constructor (if type
                            (format nil "tetraControl.~A" type)
                            (format nil "tetraSpinnerWithRange(~{~A, ~A~})" (cl-ppcre:split "-" range))))
           (element-name (format nil "~(~A~)" (cl-ppcre:regex-replace-all " +" name "-"))))
      (format t "~
~A
  .call(this,
        ~S,
        ~S, ~D, ~D, ~A, ~A, ~A, ~A, ~A);~%"
                  constructor
                  element-name
                  (if (equal gui-title "") name gui-title)
                  param nrpn
                  suppress-b suppress-v1 suppress-v2 suppress-v3 suppress-v4))))

(defun make-tetra-param-list ()
  (let ((params (make-array 200 :initial-element nil)))
    (do-with-tetra-parameters ()
      (format t "~A ~A~%" name range)
      (destructuring-bind (min max) (cl-ppcre:split "-" range)
        (setf min (parse-integer min)
              max (parse-integer max))
        (setf (aref params nrpn)
              `(:name ,name
                :def (format nil "{ name: ~S~@[, min: ~A~], max: ~A~@[, type: ~S~]~@[, supressA: ~*true~]~@[, supressV1: ~*true~]~
                                  ~@[, supressV2: ~*true~]~@[, supressV3: ~*true~]~@[, supressV4: ~*true~] }"
                             name (and (plusp min) min) max type suppress-b suppress-v1 suppress-v2 suppress-v3 suppress-v4)))))
    (format t "var parameterDefinitions = [~%")
    (dotimes (i 200)
      (let ((def (aref params i)))
        (format t "~:[undefined~;~:*~A~],~%" (and def (getf def :name)))))
    (format t "];~%~%")
    (format t "var parameterNameMap = {~%")
    (dotimes (i 200)
      (let ((def (aref params i)))
        (when def
          (format t " ~S: ~D, ~%" (getf def :name) i))))
    (format t "};~%~%")))
    