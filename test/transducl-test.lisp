(in-package :transducl-test)

(in-root-suite)

(defsuite* test-all)

(deftest test-transducer ()
  (is
   (equal
    (list 65 37 17 5 1)
    (reduce
     (funcall
      (compose
       (filtering #'evenp)
       (filtering (lambda (x) (< x 10)))
       (mapping (lambda (x) (* x x)))
       (mapping #'1+))
      #'list-conj)
     (iota 20)
     :initial-value nil))))

(deftest test-super-transducer ()
  (is
   (equal
    (list
     (list 25 9 1)
     (list 6 6 8 8 4 4)
     (list 40 20 50 10 30))
    (multiple-value-list   
     (super-fold
         (super-transducer
             (;; pipe two inputs into three pipelines
              (lambda (x y) (values x (+ x y) y)) 2) 
           
           ;; pipeline 1
           ((lambda (x y) (list* y x)) ;; base-level reducer, essentially conj
            
            (filtering #'oddp)
            (mapping (lambda (x) (* x x))))
           
           ;; pipeline 2
           ((lambda (x y) (list* y x))
            
            (filtering #'evenp)
            (mapcatting (lambda (x) (list x x))))
           
           ;; pipeline 3
           ((lambda (x y) (list* y x))
            
            (mapping (lambda (x) (* 10 x)))))
         
         (nil nil nil) ;; pipeline seeds
       (list 1 2 3 4 5) ;; input 1
       (list 3 1 5 2 4) ;; input 2
       )))))
