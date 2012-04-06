(define-module taskf.cmd
			   (use srfi-1)
			   (use srfi-19)
			   (use gauche.process)
			   (use file.util)
			   (export clear ToDo w r del))
(select-module taskf.cmd)

(define clear
  (let1 c (process-output->string '("clear"))
		(lambda ()
		  (display c))))
(define (ToDo)
  (display "?> ")
  (flush)
  (let1 cmd (read)
		(cond ((eq? cmd ':q)(exit))
			  ((eq? cmd ':w)(w))
			  ((eq? cmd ':r)(r))
			  ((eq? cmd ':del)(del))
			  (else (begin
					  (print "***ERROR***\ncommand not found: " cmd)
					  (ToDo))))))
(define (w)
  (let1 words (with-input-from-file 
				(string-append (home-directory) "/SchemeToDo/.todo") 
				(pa$ read))
		(if (eof-object? words)
		  (writ (create) '())
		  (writ (create) (cons words '())))))
(define (create)
  (display "新しいタスク> ")
  (flush)
  (let1 solve (read)
		(if (eq? solve ':q)
		  (r)
		  (begin
			(display "期限日> ")
			(flush)
			(let1 ttime (read)
				  (if (eq? ttime ':q)
					(r)
					(cons solve (cons ttime '()))))))))
(define (writ result words)
  (with-output-to-file 
	(string-append (home-directory) "/SchemeToDo/.todo")
	(pa$ print
		 (if (null? words)
		   (x->string (cons result words))
		   (x->string (cons result (car words))))))
  (w))
(define (timer result)
  (let*	((date1 
		   (make-date 0 0 0 0 (date-day(current-date))(date-month(current-date))(date-year(current-date))(date-zone-offset (current-date))))
		 (date2 (make-date 0 0 0 0 
						   (third result) 
						   (second result) 
						   (first result) 
						   (date-zone-offset (current-date)))))
		(cond ((> (date->modified-julian-day date1) (date->modified-julian-day date2))'期限過ぎてます。)
			  ((eqv? (date->modified-julian-day date1) (date->modified-julian-day date2))'期限日です。)
			  (else (string->symbol
					  (string-append "残り" 
									 (x->string 
									   (- (date->modified-julian-day date2) 
										  (date->modified-julian-day date1)))"日です。"))))))
(define (r)
  (let1 words (with-input-from-file (string-append (home-directory) "/SchemeTodo/.todo") (pa$ read))
		(cond ((eof-object? words)
			   (begin
				 (print "Nothing!!!!!!")
				 (ToDo)))
			  ((null? words)
			   (begin 
				 (print "Nothing!!!!!!")
				 (ToDo)))
			  (else (begin
					  (for-each
						(lambda(word n)
						  (format #t "[~s]~s: ~s\n-> ~s\n" n (second word) (first word) 
								  (timer (map (lambda(n)(x->number n)) (string-split (x->string (second word)) #\/)))))
						(reverse words)(iota (length words) 1))(ToDo))))))
(define (del)
  (let1 n (read)
		(let1 words (with-input-from-file 
					  (string-append (home-directory) "/SchemeToDo/.todo") (pa$ read))
			  (with-output-to-file (string-append (home-directory) "/SchemeToDo/.todo")
								   (pa$ print
										(cond
										  ((number? n)(x->string (delete (ref (reverse words) (- n 1)) words)))
										  ((eq? n '-a)
										   (begin 
											 (display "本当にいいですか?(y/n)")
											 (flush)
											 (let1 ans (read)
												   (if (eq? ans 'y)
													 (x->string (filter symbol? words)))
												   '())))
										  (else (string-append "***ERROR command not found: "n)))))
			  (ToDo))))
(provide "taskf/cmd")
