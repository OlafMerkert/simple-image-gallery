(in-package :simple-image-gallery-web)

(defun authorised-p (object)
  "Check if the user has already supplied the password for `object'."
  (let ((session (start-session)))
    (aand (session-value 'simple-image-gallery-authorisation session)
          (gethash (sig:protection-identifier object) it))))

(defun authorise (object)
  "Mark `object' as free to access for current user."
  (let* ((session (start-session))
         (table #1=(session-value 'simple-image-gallery-authorisation session)))
    (unless table
      (setf table (make-hash-table)
            #1# table))
    (setf (gethash (sig:protection-identifier object) table) t)))



(defun with-protection% (object body-function)
  "Backing function for `with-protection'."
  (if (and (sig:protected-p object)
           (not (authorised-p object)))
      ;; produce the login form
      (let ((lf (gallery-login (sig:protected-p object) "This gallery is protected by a password.")))
        (if (eq lf :success)
            (progn
              (authorise object)
              (funcall body-function))
            lf))
      ;; provide the page content
      (funcall body-function)))

(defmacro! with-protection (object &body body)
  "Helper macro, which causes a password prompt to be shown if the
`object' is protected."
  `(with-protection% ,object (lambda () ,@body)))

(defmacro! with-protection/silent (o!object &body body)
  "Helper macro, which denies access to `object' if it is protected
and no yet authorised."
  `(if (or (not (sig:protected-p ,g!object))
           (authorised-p ,g!object))
       (progn ,@body)
       (error-code hunchentoot:+http-forbidden+)))
