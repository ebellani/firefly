;;;; firefly.lisp

(in-package #:firefly)

(declaim (optimize (debug 2)))

(defvar *sldb-stack-top*)

(defvar *source-file-cache* (make-hash-table :test 'equal)
  "Cache of source file contents.
Maps from truename to source-cache-entry structure.")

(defvar *cache-sourcecode* t
  "When true complete source files are cached.
The cache is used to keep known good copies of the source text which
correspond to the loaded code. Finding definitions is much more
reliable when the exact source is available, so we cache it in case it
gets edited on disk later.")

(defstruct (source-cache-entry
             (:conc-name source-cache-entry.)
             (:constructor make-source-cache-entry (text date)))
  text date)

(defstruct (:location (:type list) :named
                      (:constructor make-location
                                    (buffer position &optional hints)))
  buffer position
  ;; Hints is a property list optionally containing:
  ;;   :snippet SOURCE-TEXT
  ;;     This is a snippet of the actual source text at the start of
  ;;     the definition, which could be used in a text search.
  hints)

(defun find-external-format (coding-system)
  "Return a \"external file format designator\" for CODING-SYSTEM.
CODING-SYSTEM is Emacs-style coding system name (a string),
e.g. \"latin-1-unix\"."
  (if (equal coding-system "iso-latin-1-unix")
      :default
      nil))

(defun %search-coding (str start end)
  (let ((p (search "coding:" str :start2 start :end2 end)))
    (when p
      (incf p (length "coding:"))
      (loop while (and (< p end)
                       (member (aref str p) '(#\space #\tab)))
            do (incf p))
      (let ((end (position-if (lambda (c) (find c '(#\space #\tab #\newline)))
                              str :start p)))
        (find-external-format (subseq str p end))))))

(defun guess-external-format (pathname)
  "Detect the external format for the file with name pathname.
Return nil if the file contains no special markers."
  ;; Look for a Emacs-style -*- coding: ... -*- or Local Variable: section.
  (with-open-file (s pathname :if-does-not-exist nil
                     :external-format (or (find-external-format "latin-1-unix")
                                          :default))
    (if s
        (or (let* ((line (read-line s nil))
                   (p (search "-*-" line)))
              (when p
                (let* ((start (+ p (length "-*-")))
                       (end (search "-*-" line :start2 start)))
                  (when end
                    (%search-coding line start end)))))
            (let* ((len (file-length s))
                   (buf (make-string (min len 3000))))
              (file-position s (- len (length buf)))
              (read-sequence buf s)
              (let ((start (search "Local Variables:" buf :from-end t))
                    (end (search "End:" buf :from-end t)))
                (and start end (< start end)
                     (%search-coding buf start end))))))))

(defun read-file (filename)
  "Return the entire contents of FILENAME as a string."
  (with-open-file (s filename :direction :input
		     :external-format (or (guess-external-format filename)
					  (find-external-format "latin-1")
					  :default))
    (let* ((string (make-string (file-length s)))
           (length (read-sequence string s)))
      (subseq string 0 length))))

(defun feature-in-list-p (feature list)
  (etypecase feature
    (symbol (member feature list :test #'eq))
    (cons (flet ((subfeature-in-list-p (subfeature)
		   (feature-in-list-p subfeature list)))
	    (ecase (first feature)
	      (:or  (some  #'subfeature-in-list-p (rest feature)))
	      (:and (every #'subfeature-in-list-p (rest feature)))
	      (:not (destructuring-bind (e) (cdr feature)
                      (not (subfeature-in-list-p e)))))))))

(defun shebang-reader (stream sub-character infix-parameter)
  (declare (ignore sub-character))
  (when infix-parameter
    (error "illegal read syntax: #~D!" infix-parameter))
  (let ((next-char (read-char stream)))
    (unless (find next-char "+-")
      (error "illegal read syntax: #!~C" next-char))
    ;; When test is not satisfied
    ;; FIXME: clearer if order of NOT-P and (NOT NOT-P) were reversed? then
    ;; would become "unless test is satisfied"..
    (when (let* ((*package* (find-package "KEYWORD"))
		 (*read-suppress* nil)
		 (not-p (char= next-char #\-))
		 (feature (read stream)))
	    (if (feature-in-list-p feature *features*)
		not-p
		(not not-p)))
      ;; Read (and discard) a form from input.
      (let ((*read-suppress* t))
	(read stream t nil t))))
 (values))

(defun code-location-debug-source-name (code-location)
  (namestring (truename (sb-c::debug-source-namestring
                         (sb-di::code-location-debug-source code-location)))))

(defun code-location-debug-source-created (code-location)
  (sb-c::debug-source-created
   (sb-di::code-location-debug-source code-location)))

(defun sbcl-source-file-p (filename)
  (when filename
    (loop for (nil pattern) in (logical-pathname-translations "SYS")
          thereis (pathname-match-p filename pattern))))

(defvar *shebang-readtable*
  (let ((*readtable* (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\!
                                  (lambda (s c n) (shebang-reader s c n))
                                  *readtable*)
    *readtable*))

(defun shebang-readtable ()
  *shebang-readtable*)

(defun guess-readtable-for-filename (filename)
  (if (sbcl-source-file-p filename)
      (shebang-readtable)
      *readtable*))

(defun source-cache-get (filename date)
  "Return the source code for FILENAME as written on DATE in a string.
Return NIL if the right version cannot be found."
  (when *cache-sourcecode*
    (let ((entry (gethash filename *source-file-cache*)))
      (cond ((and entry (equal date (source-cache-entry.date entry)))
             ;; Cache hit.
             (source-cache-entry.text entry))
            ((or (null entry)
                 (not (equal date (source-cache-entry.date entry))))
             ;; Cache miss.
             (if (equal (file-write-date filename) date)
                 ;; File on disk has the correct version.
                 (let ((source (read-file filename)))
                   (setf (gethash filename *source-file-cache*)
                         (make-source-cache-entry source date))
                   source)
                 nil))))))

(defun make-source-recorder (fn source-map)
  "Return a macro character function that does the same as FN, but
additionally stores the result together with the stream positions
before and after of calling FN in the hashtable SOURCE-MAP."
  (declare (type function fn))
  (lambda (stream char)
    (let ((start (1- (file-position stream)))
	  (values (multiple-value-list (funcall fn stream char)))
	  (end (file-position stream)))
      #+(or)
      (format t "[~D \"~{~A~^, ~}\" ~D ~D ~S]~%"
	      start values end (char-code char) char)
      (unless (null values)
	(push (cons start end) (gethash (car values) source-map)))
      (values-list values))))

(defun make-sharpdot-reader (orig-sharpdot-reader)
  #'(lambda (s c n)
      ;; We want things like M-. to work regardless of any #.-fu in
      ;; the source file that is to be visited. (For instance, when a
      ;; file contains #. forms referencing constants that do not
      ;; currently exist in the image.)
      (ignore-errors (funcall orig-sharpdot-reader s c n))))

(defun get-source-code (filename code-date)
  "Return the source code for FILENAME as written on DATE in a string.
If the exact version cannot be found then return the current one from disk."
  (or (source-cache-get filename code-date)
      (read-file filename)))

(defun make-source-recording-readtable (readtable source-map)
  "Return a source position recording copy of READTABLE.
The source locations are stored in SOURCE-MAP."
  (flet ((install-special-sharpdot-reader (*readtable*)
	   (let ((old-reader (ignore-errors
			       (get-dispatch-macro-character #\# #\.))))
	     (when old-reader
	       (set-dispatch-macro-character #\# #\.
		 (make-sharpdot-reader old-reader))))))
    (let* ((tab (copy-readtable readtable))
	   (*readtable* tab))
      (dotimes (code 128)
	(let ((char (code-char code)))
	  (multiple-value-bind (fn term) (get-macro-character char tab)
	    (when fn
	      (set-macro-character char (make-source-recorder fn source-map)
				   term tab)))))
      (install-special-sharpdot-reader tab)
      tab)))

(defun read-and-record-source-map (stream)
  "Read the next object from STREAM.
Return the object together with a hashtable that maps
subexpressions of the object to stream positions."
  (let* ((source-map (make-hash-table :test #'eq))
         (*readtable* (make-source-recording-readtable *readtable* source-map))
	 (start (file-position stream))
	 (form (ignore-errors (read stream)))
	 (end (file-position stream)))
    ;; ensure that at least FORM is in the source-map
    (unless (gethash form source-map)
      (push (cons start end) (gethash form source-map)))
    (values form source-map)))

(defun skip-toplevel-forms (n stream)
  (let ((*read-suppress* t))
    (dotimes (i n)
      (read stream))))

(defun read-source-form (n stream)
  "Read the Nth toplevel form number with source location recording.
Return the form and the source-map."
  (skip-toplevel-forms n stream)
  (let ((*read-suppress* nil))
    (read-and-record-source-map stream)))

(defun source-path-source-position (path form source-map)
  "Return the start position of PATH from FORM and SOURCE-MAP.  All
subforms along the path are considered and the start and end position
of the deepest (i.e. smallest) possible form is returned."
  ;; compute all subforms along path
  (let ((forms (loop for n in path
		     for f = form then (nth n f)
		     collect f)))
    ;; select the first subform present in source-map
    (loop for form in (reverse forms)
	  for positions = (gethash form source-map)
	  until (and positions (null (cdr positions)))
	  finally (destructuring-bind ((start . end)) positions
		    (return (values start end))))))

(defun stream-source-position (code-location stream)
  (let* ((cloc (sb-debug::maybe-block-start-location code-location))
	 (tlf-number (sb-di::code-location-toplevel-form-offset cloc))
	 (form-number (sb-di::code-location-form-number cloc)))
    (multiple-value-bind (tlf pos-map) (read-source-form tlf-number stream)
      (let* ((path-table (sb-di::form-number-translations tlf 0))
             (path (cond ((<= (length path-table) form-number)
                          (warn "inconsistent form-number-translations")
                          (list 0))
                         (t
                          (reverse (cdr (aref path-table form-number)))))))
        (source-path-source-position path tlf pos-map)))))

(defun call-with-debootstrapping (fun)
  (handler-bind ((sb-int:bootstrap-package-not-found
                  #'sb-int:debootstrap-package))
    (funcall fun)))

(defmacro with-debootstrapping (&body body)
  `(call-with-debootstrapping (lambda () ,@body)))

(defun skip-comments-and-whitespace (stream)
  (case (peek-char nil stream)
    ((#\Space #\Tab #\Newline #\Linefeed #\Page)
     (read-char stream)
     (skip-comments-and-whitespace stream))
    (#\;
     (read-line stream)
     (skip-comments-and-whitespace stream))))

(defun read-upto-n-chars (stream n)
  "Return a string of upto N chars from STREAM."
  (let* ((string (make-string n))
         (chars  (read-sequence string stream)))
    (subseq string 0 chars)))

(defvar *source-snippet-size* 256
  "Maximum number of characters in a snippet of source code.
Snippets at the beginning of definitions are used to tell Emacs what
the definitions looks like, so that it can accurately find them by
text search.")

(defun read-snippet (stream &optional position)
  "Read a string of upto *SOURCE-SNIPPET-SIZE* characters from STREAM.
If POSITION is given, set the STREAM's file position first."
  (when position
    (file-position stream position))
  (skip-comments-and-whitespace stream)
  (read-upto-n-chars stream *source-snippet-size*))

(defun source-file-source-location (code-location)
  (let* ((code-date (code-location-debug-source-created code-location))
         (filename  (code-location-debug-source-name code-location))
         (*readtable* (guess-readtable-for-filename filename))
         (source-code (get-source-code filename code-date)))
    (with-debootstrapping
      (with-input-from-string (s source-code)
        (let* ((pos (stream-source-position code-location s))
               (snippet (read-snippet s pos)))
          (make-location `(:file ,filename)
                         `(:position ,pos)
                         `(:snippet ,snippet)))))))

(defun code-location-has-debug-block-info-p (code-location)
  (handler-case
      (progn (sb-di:code-location-debug-block code-location)
             t)
    (sb-di:no-debug-blocks  () nil)))

(defun file-source-location (code-location)
  (source-file-source-location code-location))

(defun make-error-location (datum &rest args)
  (cond ((typep datum 'condition)
         `(:error ,(format nil "Error: ~A" datum)))
        ((symbolp datum)
         `(:error ,(format nil "Error: ~A"
                           (apply #'make-condition datum args))))
        (t
         (assert (stringp datum))
         `(:error ,(apply #'format nil datum args)))))

(defmacro converting-errors-to-error-location (&body body)
  "Catches errors during BODY and converts them to an error location."
  (let ((gblock (gensym "CONVERTING-ERRORS+")))
    `(block ,gblock
       (handler-bind ((error
                       #'(lambda (e)
                           (return-from ,gblock
                             (make-error-location e)))))
         ,@body))))

(defun lisp-source-location (code-location)
  (let ((source (prin1-to-string
                 (sb-debug::code-location-source-form code-location 100))))
    (if (and (search "SB-IMPL::WITH-STEPPING-ENABLED" source
                     :test #'char-equal)
             (search "SB-IMPL::STEP-FINISHED" source :test #'char-equal))
        ;; The initial form is utterly uninteresting -- and almost
        ;; certainly right there in the REPL.
        (make-error-location "Stepping...")
        (make-location `(:source-form ,source) '(:position 1)))))

(defun code-location-source-location (code-location)
  (let* ((dsource (sb-di:code-location-debug-source code-location))
         ;; (plist (sb-c::debug-source-plist dsource))
         )
    ;; (ecase (sb-di:debug-source-from dsource)
    ;;   (:file (file-source-location code-location))
    ;;   (:lisp (lisp-source-location code-location)))

    (if (sb-di:debug-source-namestring dsource)
        (file-source-location code-location)
        (lisp-source-location code-location))))

(defun frame-source-location (index)
  (converting-errors-to-error-location
    (code-location-source-location
     (sb-di:frame-code-location (nth-frame index)))))

(defun compute-backtrace (start end)
  "Return a list of frames starting with frame number START and
continuing to frame number END or, if END is nil, the last frame on the
stack."
  (let ((end (or end most-positive-fixnum)))
    (loop for f = (nth-frame start) then (sb-di:frame-down f)
	  for i from start below end
	  while f collect f)))

(defun call-with-debugging-environment (debugger-loop-fn)
  ""
  (declare (type function debugger-loop-fn))
  (let ((*sldb-stack-top*
         (if sb-debug:*stack-top-hint*
             (sb-debug::resolve-stack-top-hint)
             (sb-di:top-frame)))
        (sb-debug:*stack-top-hint* nil))
    (handler-bind ((sb-di:debug-condition
                    (lambda (condition)
                      (signal ;; 'sldb-condition
                              ;; :original-condition
                              condition))))
      (funcall debugger-loop-fn))))

(defun nth-frame (index)
  (do ((frame *sldb-stack-top* (sb-di:frame-down frame))
       (i index (1- i)))
      ((zerop i) frame)))

(defun backtrace-with-extra-info (&key (start 1) (end 20))
  (call-with-debugging-environment
   (lambda ()
     (loop for i from start to (length (compute-backtrace
                                        start end))
           do (ignore-errors (print-frame i))))))

(defun debug-var-info (var)
  ;; Introduced by SBCL 1.0.49.76.
  (let ((s (find-symbol "DEBUG-VAR-INFO" :sb-di)))
    (when (and s (fboundp s))
      (funcall s var))))

(defun debug-var-value (var frame location)
  (ecase (sb-di:debug-var-validity var location)
    (:valid (sb-di:debug-var-value var frame))
    ((:invalid :unknown) ':<not-available>)))

(defun frame-debug-vars (frame)
  "Return a vector of debug-variables in frame."
  (sb-di::debug-fun-debug-vars (sb-di:frame-debug-fun frame)))

(defun frame-locals (index)
  (let* ((frame (nth-frame index))
	 (loc (sb-di:frame-code-location frame))
	 (vars (frame-debug-vars frame))
         ;; Since SBCL 1.0.49.76 PREPROCESS-FOR-EVAL understands SB-DEBUG::MORE
         ;; specially.
         (more-name (or (find-symbol "MORE" :sb-debug) 'more))
         (more-context nil)
         (more-count nil)
         (more-id 0))
    (when vars
      (let ((locals
              (loop for v across vars
                    do (when (eq (sb-di:debug-var-symbol v) more-name)
                         (incf more-id))
                       (case (debug-var-info v)
                         (:more-context
                          (setf more-context (debug-var-value v frame loc)))
                         (:more-count
                          (setf more-count (debug-var-value v frame loc))))
                    collect
                       (list :name (sb-di:debug-var-symbol v)
                             :id (sb-di:debug-var-id v)
                             :value (debug-var-value v frame loc)))))
        (when (and more-context more-count)
          (setf locals (append locals
                               (list
                                (list :name more-name
                                      :id more-id
                                      :value (multiple-value-list
                                              (sb-c:%more-arg-values
                                               more-context
                                               0 more-count)))))))
        locals))))

(defun print-frame (i)
 (destructuring-bind (&key file position &allow-other-keys)
      (apply #'append
             (remove-if #'atom
                        (frame-source-location i)))
    (let* ((frame (nth-frame i))
           (line-number (find-line-position file position frame)))
      (format t "~2@a: ~s~%~
                   ~:[~*~;~:[~2:*    At ~a (unknown line)~*~%~;~
                             ~2:*    At ~a:~a~%~]~]~
                   ~:[~*~;    Local variables:~%~{      ~a = ~s~%~}~]"
              i
              (sb-debug::frame-call (nth-frame i))
              file line-number
              (frame-locals i)
              (mapcan (lambda (x)
                        ;; Filter out local variables whose variables we
                        ;; don't know
                        (unless (eql (getf x :value) :<not-available>)
                          (list (getf x :name) (getf x :value))))
                      (frame-locals i))))))

(defun find-line-position (file char-offset frame)
  ;; It would be nice if SBCL stored line number information in
  ;; addition to form path information by default Since it doesn't
  ;; we need to use Swank to map the source path to a character
  ;; offset, and then map the character offset to a line number
  (ignore-errors
   (let* ((location (sb-di::frame-code-location frame))
          (debug-source (sb-di::code-location-debug-source location))
          (line (with-open-file (stream file)
                  (1+ (loop repeat char-offset
                            count (eql (read-char stream) #\Newline))))))
     (format nil "~:[~a (file modified)~;~a~]"
             (= (file-write-date file)
                (sb-di::debug-source-created debug-source))
             line))))


;; (declaim (optimize debug))
(defun foo (x)
  (let ((y (+ x 3)))
    (backtrace-with-extra-info)
    (+ x y)))

;; (defmethod bar ((n fixnum) (y (eql 1)))
;;   (foo (+ y n)))
