(load "~/quicklisp/setup.lisp")

(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload :cl-interpol)
  (ql:quickload :cl-ppcre)
  (ql:quickload :cjf-stdlib))

(defpackage :logfilt
  (:use :cl :cl-interpol :cjf-stdlib)
  (:shadowing-import-from :cl-ppcre :split :all-matches-as-strings
                          :regex-replace-all)
  (:export main))

(in-package :logfilt)

(enable-interpol-syntax)

(defvar *filters* `(
     ("INFO" . :green)
     ("WARNING" . :yellow)
     ("ERROR" . :red)
     ("CRITICAL" . :magenta)
     ("^.*!!!.*$" . :cyan)
     (#?/\d{4}-\d{2}-\d{2}[^]]*]/ . :grey)
     ))

(defvar *color-escapes* (ht
  :red #?"\x1b[31m"
  :green #?"\x1b[32m"
  :yellow #?"\x1b[33m"
  :magenta #?"\x1b[35m"
  :cyan #?"\x1b[36m"
  :clear #?"\x1b[0m"
  :grey #?"\x1b[30;1m"

  ))

(defun format-fn (color)
  (format nil ":~a:" color))

(defvar *placeholders*
  (mapcar #'format-fn (keys *color-escapes*)))

(defun apply-filter (filt line)
  (let ((re (car filt))
        (color (cdr filt)))
    (regex-replace-all re line (concatenate 'string
                                            (format-fn color)
                                            "\\&"
                                            (format-fn :restore)))))

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
      building
      ))

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

(defun main ()
  (println (process-line (read-line)))
  (finish-output nil)
  (main))

#+sbcl
(handler-case (main)
  (sb-sys:interactive-interrupt ()
    (sb-ext:quit)))
#-sbcl
(main)
