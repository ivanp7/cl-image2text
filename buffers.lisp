;;;; buffers.lisp

(in-package #:cl-image2text)

;;; COLOR BUFFER

(deftype pixel-buffer () 'opticl:8-bit-rgb-image)
(defconstant +pixel-buffer-type+ 'opticl:8-bit-rgb-image)
(deftype color () '(unsigned-byte 8))

(defun create-pixel-buffer (x y)
  (opticl:make-8-bit-rgb-image y x :initial-element 0))

(defmacro with-pixel-buffer-size ((width height) buffer &body body)
  `(opticl:with-image-bounds (,height ,width) ,buffer
     ,@body))

(defmacro pixel-buffer-color (color buffer x y)
  (ecase color
    (:red `(the color (aref ,buffer ,y ,x 0)))
    (:green `(the color (aref ,buffer ,y ,x 1)))
    (:blue `(the color (aref ,buffer ,y ,x 2)))))

(defmacro with-pixel-buffer-colors (var-prefix buffer x y &body body)
  (let* ((prefixstr (symbol-name var-prefix)) 
         (red (intern (concatenate 'string prefixstr "-RED")))
         (green (intern (concatenate 'string prefixstr "-GREEN")))
         (blue (intern (concatenate 'string prefixstr "-BLUE")))) 
    (alexandria:with-gensyms (bufferg xg yg)
      `(let ((,bufferg ,buffer) (,xg ,x) (,yg ,y))
         (declare (ignorable ,bufferg ,xg ,yg))
         (symbol-macrolet ((,red (pixel-buffer-color :red ,bufferg ,xg ,yg))
                           (,green (pixel-buffer-color :green ,bufferg ,xg ,yg))
                           (,blue (pixel-buffer-color :blue ,bufferg ,xg ,yg)))
           (declare (ignorable ,red ,green ,blue))
           ,@body)))))

(defmacro modify-places (result-type operation (&rest places) (&rest arglists) &environment env)
  (if (/= (length places) (length arglists))
    (error "Number of arglists must correspond to the number of places")
    (flet ((generate-setf (setf-exp arglist)
             (unless (listp arglist)
               (setf arglist (list arglist)))
             (let ((vars (first setf-exp)) (forms (second setf-exp)) (var (car (third setf-exp)))
                   (set (fourth setf-exp)) (access (fifth setf-exp)))
               `(let (,@(mapcar #'list vars forms))
                  (let ((,var (the ,result-type (,operation ,access ,@arglist))))
                    ,set)))))
      `(progn ,@(mapcar #'generate-setf 
                        (mapcar #'(lambda (place)
                                    (multiple-value-list 
                                      (get-setf-expansion place env)))
                                places)
                        arglists)))))

;;; TERMINAL BUFFER

(defstruct terminal-buffer
  (char-array nil :type (simple-array character (* *)))
  (fg-color-array nil :type pixel-buffer)
  (bg-color-array nil :type pixel-buffer))

(defun create-terminal-buffer (x y)
  (make-terminal-buffer 
    :char-array (make-array `(,y ,x) :element-type 'character :initial-element #\Space)
    :fg-color-array (create-pixel-buffer x y)
    :bg-color-array (create-pixel-buffer x y)))

(defmacro with-terminal-buffer-size ((width height) terminal-buffer &body body)
  `(with-pixel-buffer-size (,width ,height) (terminal-buffer-fg-color-array ,terminal-buffer)
     ,@body))

(defmacro with-terminal-buffer-element (terminal-buffer x y &body body)
  (alexandria:once-only (terminal-buffer x y) 
    (alexandria:with-gensyms (fg-col-buf bg-col-buf char-buf)
      `(let ((,fg-col-buf (the pixel-buffer (terminal-buffer-fg-color-array ,terminal-buffer)))
             (,bg-col-buf (the pixel-buffer (terminal-buffer-bg-color-array ,terminal-buffer)))
             (,char-buf (the (simple-array character (* *))
                             (terminal-buffer-char-array ,terminal-buffer)))) 
         (declare (ignorable ,char-buf))
         (with-pixel-buffer-colors fg ,fg-col-buf ,x ,y
           (with-pixel-buffer-colors bg ,bg-col-buf ,x ,y
             (symbol-macrolet ((char (aref ,char-buf ,y ,x)))
               (declare (ignorable char))
               ,@body)))))))

