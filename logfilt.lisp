;; To rebuild executable (ccl)
;; (load "logfilt.lisp")
;; (ccl:save-application "logfilt" :toplevel-function #'logfilt:main :prepend-kernel t)

(load "~/quicklisp/setup.lisp")

(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload :cl-interpol)
  (ql:quickload :cl-ppcre)
  (ql:quickload :col))

(in-package :cl-user)

(defpackage :logfilt
  (:use :cl :cl-interpol :col)
  (:shadowing-import-from :cl-ppcre :split :all-matches-as-strings
                          :regex-replace-all)
  (:export main))

(in-package :logfilt)

(enable-interpol-syntax)

(defvar *filters*
  `(("INFO" . :green)
    ("WARNING" . :yellow)
    ("ERROR" . :red)
    ("CRITICAL" . :magenta)
    ("^.*!!!.*$" . :blue)
    (#?/Profile start .*\.\.\./ . :green)
    (#?/Profile stop .* sec./ . :red)
    (#?/\s\d{4}-\d{2}-\d{2}(?:\s|\d|:|,)+/ . :delete)))

(defvar *color-escapes* (ht
  :red #?"\x1b[31m"
  :green #?"\x1b[32m"
  :yellow #?"\x1b[33m"
  :magenta #?"\x1b[35m"
  :blue #?"\x1b[34m"
  :clear #?"\x1b[0m"
  :white #?"\x1b[37m"))

(defun format-fn (color)
  (format nil ":~a:" color))

(defvar *placeholders*
  (mapcar #'format-fn (keys *color-escapes*)))

(defun apply-filter (filt line)
  (let ((re (car filt))
        (color (cdr filt)))
    (let ((replacement (if (eql color :delete)
                           ""
                           (concatenate 'string
                                        (format-fn color)
                                        "\\&"
                                        (format-fn :restore)))))
      (regex-replace-all re line replacement))))

(defun apply-filters (flist line)
  (if flist
      (apply-filters (cdr flist) (apply-filter (car flist) line))
      line))

(defun next-to-last-color (lst)
  (if (< (length lst) 2)
      (format-fn :clear)
        (elt lst (- (length lst) 2))))

(defun restore-replacement-helper (building curr-color remaining)
  (if remaining
      (let* ((new-building
              (concatenate 'string
                           building
                           (if (equal (car remaining) (format-fn :restore))
                               curr-color
                               (car remaining))))

             (color-re (format nil "~{~A~^|~}" *placeholders*))
             (last-color (if (equal (car remaining) (format-fn :restore))
                             curr-color
                             (next-to-last-color (all-matches-as-strings
                                                           color-re new-building)))))
        (restore-replacement-helper new-building
                                    (or last-color curr-color)
                                    (cdr remaining)))
      building))

(defun concrete-color-replacement-helper (colors line)
  (if colors
      (concrete-color-replacement-helper
       (cdr colors)
       (regex-replace-all (format-fn (car colors))
                          line
                          (mget *color-escapes* (car colors))))
      line))

(defun do-replacements (line)
  ; First replace all instances of :restore with the appropriate color.
  (let* ((parts (split (concatenate 'string
                                    "(" (format-fn :restore) ")")
                       line
                       :with-registers-p t
                       :omit-unmatched-p t))
         (with-concrete-colors (restore-replacement-helper ""
                                (format-fn :clear) parts)))
    (concrete-color-replacement-helper (keys *color-escapes*)
                                       with-concrete-colors)))

(defun process-line (line)
  (do-replacements (apply-filters *filters* line)))

(defun internal-main ()
  (declare (optimize (debug 0)))
  (println (process-line (read-line)))
  (finish-output nil)
  (internal-main))

(defun main (&optional argv)
  (declare (ignore argv))
  #+sbcl
  (handler-case (internal-main)
    (sb-sys:interactive-interrupt ()
      (sb-ext:quit)))
  #+ccl
  (progn
    (setf ccl:*break-hook*
          (lambda (condition hook)
            (declare (ignore hook)
                     (ignore condition))
            (ccl:quit)))
    (internal-main))
  )

(main)
