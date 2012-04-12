#!/usr/local/bin/gosh
(use file.util)
(use gauche.parseopt)
(use taskf.cmd)
(define dirname (string-append (home-directory) "/.SchemeTask"))
(define filename ".todo")
(define (main args)
  (let-args (cdr args)
			((w "w|write" => w)
			 (r "r|read" => r)
			 (d "d|del" => del))
			(clear)
			(if (file-is-directory? dirname)
			  (begin
				(current-directory dirname))
			  (begin
				(make-directory* dirname)
				(current-directory dirname)
				(create-directory-tree "./" filename)))
			(print "YES,TaskList!!!!!!!!")
			(print "-------------------")
			(ToDo)))
