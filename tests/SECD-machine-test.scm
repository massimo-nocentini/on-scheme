

(import chicken scheme)

    (use numbers test)

    (use commons SECD-machine)

    (let* ((v 'x)
           (e (Comb
               (Lambda v
                (Comb
                 (Comb 
                  (Comb 
                   (Id 'p) 
                   (Comb 
                    (Comb 
                     (Id '*) 
                     (Comb 
                      (Id '²) 
                      (Id v))) 
                    (Id 'a)))
                  (Comb 
                   (Comb 
                    (Id '*) 
                    (Id v)) 
                   (Id 'b))) 
                 (Id 'c)))
               (Id 'd))))
    (test "((λ (x) (((p ((* (² x)) a)) ((* x) b)) c)) d)" (to-string e))
    (test (Lambda 'x (Id 'y)) (curryfy '(λ (x) y)))
    (test (Lambda 'x (Comb (Id 'y) (Id 'z))) (curryfy '(λ (x) (y z))))
    (test e (curryfy `((λ (,v) (p (* (² ,v) a) (* ,v b) c)) d)))
    (test 27 ((λ (x a b c) (+ (* (² x) a) (* x b) c)) 4 1 2 3))
    (let₁ (p (λ (x) (λ (y) (λ (z) (+ x y z)))))
     (test 27 ((λ (x a b c) (((p (((curry₁ *) (² x)) a)) (((curry₁ *) x) b)) c)) 4 1 2 3)))
    (let₁ (E ((extend E₀) 
              `(a . ,1) `(b . ,2) `(c . ,3) `(d . ,4) `(o . ,-24)
              `(p . ,(lambda (x) (lambda (y) (lambda (z) (+ x y z)))))
              `(* . ,(lambda (x) (lambda (y) (* x y))))
              `(² . ,²)))
     (test 27 ((value E) e))
     
     (test "((λ (x) (λ (y) (((p ((* (² x)) a)) ((* x) b)) y))) d)" 
      ((○ to-string curryfy) `((λ (,v y) (p (* (² ,v) a) (* ,v b) y)) d)))

     (test-assert ((○ procedure? (value E) curryfy) 
                   `((λ (,v y) (p (* (² ,v) a) (* ,v b) y)) d)))
     
     (let₁ (e₁ `((λ (,v y) (p (* (² ,v) a) (* ,v b) y)) d o))
      (test "(((λ (x) (λ (y) (((p ((* (² x)) a)) ((* x) b)) y))) d) o)" 
       ((○ to-string curryfy) e₁))
      (test 0 ((○ (value E) curryfy) e₁)))

     ))


    (let* ((control (curryfy '((λ (f x) (f (f x))) ² three)))
           (control₁ (curryfy '((λ (f) (λ (x) (f (f x)))) ² three)))
           (E ((extend E₀) `(² . ,²) `(three . 3)))
           (s₀ (status-init E (list control)))
           #;(→/compiled (rtc (→/compiled E)))
           (F (lambda (s) 
               (format #t "~a\n" s)
               (→/interpreted s)))
           (→/interpreted* (rtc F)))

     (print control)
     (print control₁)
     (test-assert (equal? control control₁))

     (test "(() #<procedure (bind11561182 z1185)> ((((λ (f) (λ (x) (f (f x)))) ²) three)) #<unspecified>)"
      (to-string (status-init E (list control))))

     #;(test "" (to-string (→/interpreted* s₀)))

     (test `(81 ,'() ,(void)) 
      (let₁ (s (last (→/interpreted* s₀)))
       (list 
        ((○ car status-S) s)
        (status-C s)
        (status-D s))))

     #;(test 81
      ""
      (with-output-to-string (τ ((fmap F) (→/interpreted* (status-init E (list control)))))))

     #;(test
      "(() ((Load 3) (Load 1) (Load #<procedure (f a216)>) Apply Apply (Load 3) (Load 2) (Load 1) (Load #<procedure (p a210)>) Apply Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((3) ((Load 1) (Load #<procedure (f a216)>) Apply Apply (Load 3) (Load 2) (Load 1) (Load #<procedure (p a210)>) Apply Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((1 3) ((Load #<procedure (f a216)>) Apply Apply (Load 3) (Load 2) (Load 1) (Load #<procedure (p a210)>) Apply Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((#<procedure (f a216)> 1 3) (Apply Apply (Load 3) (Load 2) (Load 1) (Load #<procedure (p a210)>) Apply Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((#<procedure (f_671 b217)> 3) (Apply (Load 3) (Load 2) (Load 1) (Load #<procedure (p a210)>) Apply Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((10) ((Load 3) (Load 2) (Load 1) (Load #<procedure (p a210)>) Apply Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((3 10) ((Load 2) (Load 1) (Load #<procedure (p a210)>) Apply Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((2 3 10) ((Load 1) (Load #<procedure (p a210)>) Apply Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((1 2 3 10) ((Load #<procedure (p a210)>) Apply Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((#<procedure (p a210)> 1 2 3 10) (Apply Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((#<procedure (f_657 b211)> 2 3 10) (Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((3 3 10) ((Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((#<procedure (m a213)> 3 3 10) (Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((#<procedure (f_664 b214)> 3 10) (Apply (Load #<procedure (p a210)>) Apply Apply))\n((0 10) ((Load #<procedure (p a210)>) Apply Apply))\n((#<procedure (p a210)> 0 10) (Apply Apply))\n((#<procedure (f_657 b211)> 10) (Apply))\n((10) ())\n"
      (with-output-to-string (τ ((fmap F)
                                 (→/compiled
                                  (make-status '() ((compile E) control)))))))

     #;(test 
      "(() (((p ((f a) c)) ((m c) ((p b) a)))))\n(() (((m c) ((p b) a)) (p ((f a) c)) apply0))\n(() (((p b) a) (m c) apply0 (p ((f a) c)) apply0))\n(() (a (p b) apply0 (m c) apply0 (p ((f a) c)) apply0))\n((1) ((p b) apply0 (m c) apply0 (p ((f a) c)) apply0))\n((1) (b p apply0 apply0 (m c) apply0 (p ((f a) c)) apply0))\n((2 1) (p apply0 apply0 (m c) apply0 (p ((f a) c)) apply0))\n((#<procedure (p a210)> 2 1) (apply0 apply0 (m c) apply0 (p ((f a) c)) apply0))\n((#<procedure (f_657 b211)> 1) (apply0 (m c) apply0 (p ((f a) c)) apply0))\n((3) ((m c) apply0 (p ((f a) c)) apply0))\n((3) (c m apply0 apply0 (p ((f a) c)) apply0))\n((3 3) (m apply0 apply0 (p ((f a) c)) apply0))\n((#<procedure (m a213)> 3 3) (apply0 apply0 (p ((f a) c)) apply0))\n((#<procedure (f_664 b214)> 3) (apply0 (p ((f a) c)) apply0))\n((0) ((p ((f a) c)) apply0))\n((0) (((f a) c) p apply0 apply0))\n((0) (c (f a) apply0 p apply0 apply0))\n((3 0) ((f a) apply0 p apply0 apply0))\n((3 0) (a f apply0 apply0 p apply0 apply0))\n((1 3 0) (f apply0 apply0 p apply0 apply0))\n((#<procedure (f a216)> 1 3 0) (apply0 apply0 p apply0 apply0))\n((#<procedure (f_671 b217)> 3 0) (apply0 p apply0 apply0))\n((10 0) (p apply0 apply0))\n((#<procedure (p a210)> 10 0) (apply0 apply0))\n((#<procedure (f_657 b211)> 0) (apply0))\n((10) ())\n"
      (with-output-to-string (τ ((fmap F) 
                                 (→/interpreted 
                                  (make-status '() (list control₁))))))))





