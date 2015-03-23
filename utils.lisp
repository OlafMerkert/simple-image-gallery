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
