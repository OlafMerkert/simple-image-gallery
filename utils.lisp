(in-package :simple-image-gallery)

(cl-secure-read:define-secure-read safe-read)

(defun safe-read-file-1 (pathname)
  "Read the first sexp in the given file in a secure manner."
  (with-open-file (stream pathname :if-does-not-exist :error)
    (safe-read stream)))

(defun safe-read-file-* (pathname)
  "Read the first sexp in the given file in a secure manner."
  (with-open-file (stream pathname :if-does-not-exist :error)
    (do (sexps
         (r #1=(safe-read stream nil 'eof) #1#))
        ((eq r 'eof) (nreverse sexps))
      (push r sexps))))

;;; interface for supporting password protected objects
(defgeneric protected-p (object)
  (:documentation "If this returns non-nil, this means we require an
  authentification before granting access to the object. The value
  should indicate how to authentify, for now we it should be a string
  which we interpret as a password."))

(defgeneric protection-identifier (object)
  (:documentation "If several (grouped) objects need the same
  password, we only want to ask once for it. So every object needs to
  report the identifier of its group. This can be an arbitrary object,
  by default the object itself."))

(defmethod protection-identifier (object)
  "By default, `protection-identifier' is just `identity'."
  object)
