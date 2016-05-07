
(let ((str-port (open-output-string))) 
 (with-output-to-port str-port (lambda () (display "hello world")))
 (display (string-append (get-output-string str-port) "!")))

(display (call-with-output-string (lambda (port) (display "second hello world" port))))
