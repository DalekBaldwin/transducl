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

(defmacro super-transducer ((expander ins) &body pipelines)
  (let ((in-syms
         (loop for i from 1 to ins
            collect (gensym (format nil "IN-~A" i))))
        (out-syms
         (loop for i from 1 to (length pipelines)
            collect (gensym (format nil "OUT-~A" i))))
        (accum-syms
         (loop for i from 1 to (length pipelines)
            collect (gensym (format nil "ACCUM-~A" i)))))
    `(lambda (,@(loop for accum-sym in accum-syms
                   collect accum-sym)
              ,@(loop for in-sym in in-syms
                   collect in-sym))
       (multiple-value-bind (,@out-syms)
           (,expander ,@in-syms)
         (values
          ,@(loop for (reducer . transducers) in pipelines
               for out-sym in out-syms
               for accum-sym in accum-syms
               collect
                 `(funcall
                   (funcall
                    (compose
                     ,@transducers)
                    ,reducer)
                   ,accum-sym
                   ,out-sym)))))))

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
