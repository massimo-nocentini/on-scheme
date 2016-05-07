
(let* ((str-port (open-output-string)) 
       (result (with-output-to-port str-port 
                (lambda () 
                 (display "hello world") 
                 "some value"))))
 (display (string-append (get-output-string str-port) "!\n"))
 (display (string-append result " from display!\n"))
 result)

(display (call-with-output-string (lambda (port) (display "second hello world" port))))
