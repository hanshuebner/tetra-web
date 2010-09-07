
(defun file-contents (pathname &key (element-type '(unsigned-byte 8)) (external-format :utf-8))
  (with-open-file (s pathname :element-type element-type :external-format external-format)
    (let ((result
           (make-array (file-length s) :element-type element-type)))
      (read-sequence result s)
      result)))

(defun parse-flag (string)
  (if (equal "x" string)
      "true"
      "false"))

(defun make-tetra-controls ()
  (dolist (def (mapcar (alexandria:curry #'cl-ppcre:split "\\t")
                       (cl-ppcre:split "\\r"
                                       (cl-ppcre:regex-replace-all "\\n"
                                                                   (file-contents "tetra parameters.txt"
                                                                                  :external-format :cp437
                                                                                  :element-type 'character)
                                                                   ""))))
    (destructuring-bind (param name short-name gui-title
                               range type comment
                               nrpn &optional suppress-b suppress-v1 suppress-v2 suppress-v3 suppress-v4 &rest junk) def
      (declare (ignore short-name comment junk))
      (unless (or (equal param "Parm")
                  (equal name ""))
        (setf param (parse-integer param)
              nrpn (parse-integer nrpn)
              suppress-b (parse-flag suppress-b)
              suppress-v1 (parse-flag suppress-v1)
              suppress-v2 (parse-flag suppress-v2)
              suppress-v3 (parse-flag suppress-v3)
              suppress-v4 (parse-flag suppress-v4))
        (setf type (if (equal "" type)
                       (format nil "tetraSpinnerWithRange(~{~A, ~A~})" (cl-ppcre:split "-" range))
                       (format nil "tetraControl.~A" type)))
        (let ((element-name (format nil "~(~A~)" (cl-ppcre:regex-replace-all " +" name "-"))))
          (format t "~
~A
  .call(this,
        ~S,
        ~S, ~D, ~D, ~A, ~A, ~A, ~A, ~A);~%"
                  type
                  element-name
                  (if (equal gui-title "") name gui-title)
                  param nrpn
                  suppress-b suppress-v1 suppress-v2 suppress-v3 suppress-v4))))))