(in-package :transducl)

(defun list-conj (accum item)
  (list* item accum))

(defun map-reducer (fun)
  (lambda (accum input)
    (list-conj accum (funcall fun input))))

(defun filter-reducer (pred)
  (lambda (accum input)
    (if (funcall pred input)
        (list-conj accum input)
        accum)))

(defun mapping (fun)
  (lambda (reducing)
    (lambda (accum input)
      (funcall reducing accum (funcall fun input)))))

(defun filtering (pred)
  (lambda (reducing)
    (lambda (accum input)
      (if (funcall pred input)
          (funcall reducing accum input)
          accum))))

(defun filter-mapping (pred fun)
  (lambda (reducing)
    (lambda (accum input)
      (if (funcall pred input)
          (funcall reducing accum (funcall fun input))
          accum))))

(defun mapcatting (fun)
  (lambda (reducing)
    (lambda (accum input)
      (reduce reducing (funcall fun input) :initial-value accum))))

(defun transduce (transducer reducer initial-value input)
  (reduce (funcall transducer reducer) input :initial-value initial-value))

(defmacro def-pipeline (name (reducer) &rest transducers)
  (with-gensyms (accum input full-reducer)
    `(defun ,name (,reducer)
       (let ((,full-reducer
              (funcall
               (compose
                ,@transducers)
               ,reducer)))
         (lambda (,accum ,input)
           (funcall ,full-reducer ,accum ,input))))))

(defmacro let-pipeline (bindings &body body)
  (with-gensyms (reducer full-reducer accum input)
    `(flet (,@(loop for (name . transducers) in bindings
                 collect
                   `(,name (,reducer)
                           (let ((,full-reducer
                                  (funcall
                                   (compose
                                    ,@transducers)
                                   ,reducer)))
                             (lambda (,accum ,input)
                               (funcall ,full-reducer ,accum ,input))))))
       ,@body)))

(defmacro super-transducer (vars fan-out-form &body pipelines)
  (let* ((size (length pipelines))
         (out-syms
          (loop for i from 1 to size
             collect (gensym (format nil "OUT-~A" i))))
         (accum-syms
          (loop for i from 1 to size
             collect (gensym (format nil "ACCUM-~A" i)))))
    `(lambda (,@accum-syms ,@vars)
       (multiple-value-bind (,@out-syms)
           ,fan-out-form
         (values
          ,@(loop for pipeline in pipelines
               for out-sym in out-syms
               for accum-sym in accum-syms
               collect
                 `(funcall ,pipeline ,accum-sym ,out-sym)))))))

(defmacro super-fold (function (&rest seeds) &body sequences)
  (let ((out-syms
         (loop for i from 1 to (length seeds)
            collect (gensym (format nil "OUT-~A" i))))
        (accum-syms
         (loop for i from 1 to (length seeds)
            collect (gensym (format nil "ACCUM-~A" i))))
        (seq-syms
         (loop for i from 1 to (length sequences)
            collect (gensym (format nil "SEQ-~A" i)))))
    `(labels ((recurse (,@accum-syms ,@seq-syms)
                (cond
                  ((or
                    ,@(loop for seq-sym in seq-syms
                         collect
                           `(endp ,seq-sym)))
                   (values ,@accum-syms))
                  (t
                   (multiple-value-bind (,@out-syms)
                       (funcall ,function ,@accum-syms
                                ,@(loop for seq-sym in seq-syms
                                     collect `(first ,seq-sym)))
                     (recurse ,@out-syms
                              ,@(loop for seq-sym in seq-syms
                                   collect `(rest ,seq-sym))))))))
       (recurse ,@seeds ,@sequences))))
