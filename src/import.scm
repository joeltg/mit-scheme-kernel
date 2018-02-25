(define (normalize-pathname pathname)
	(directory-pathname-as-file (pathname-simplify pathname)))

(define (initialize-environments ht ie nie nee)
	(environment-define nie 'hash-table ht)
	(environment-define nie 'import-env nie)
	(environment-define nie 'export-env nee)
	(environment-define nie 'import-list (make-import-list nie ht))
	(environment-define nie 'export-list (make-export-list nie nee))
	(link-variables nie 'import ie 'import)
	(link-variables nie 'export ie 'export))

(define (copy-bindings symbols source-env target-env)
	(for-each
		(lambda (symbol)
			(link-variables target-env symbol source-env symbol))
		symbols))

(define-syntax import
	(er-macro-transformer
		(lambda (exp rename compare)
			(let ((path (cadr exp))
						(names (cddr exp)))
				`(import-list ,path (quote ,names))))))

(define-syntax export
	(er-macro-transformer
		(lambda (exp rename compare)
			(let ((names (cdr exp)))
				`(export-list (quote ,names))))))

(define ((make-export-list import-env export-env) symbols)
	(for-each
		(lambda (name) (link-variables export-env name import-env name))
		symbols))

(define ((make-import-list import-env hash-table) path symbols)
	(let* ((working-directory (working-directory-pathname))
				 (merged-pathname (merge-pathnames path working-directory))
				 (target-pathname (normalize-pathname merged-pathname))
				 (key (->namestring target-pathname))
				 (export-env (hash-table/get hash-table key #f)))
		(if (and export-env (environment? export-env))
			(copy-bindings symbols export-env import-env)
			(let ((target-directory (directory-pathname target-pathname))
						(target-file (file-pathname target-pathname))
						(next-import-env (make-top-level-environment))
						(next-export-env (make-root-top-level-environment)))
				(initialize-environments hash-table import-env next-import-env next-export-env)
				(hash-table/put! hash-table key next-export-env)
				(set-working-directory-pathname! target-directory)
				(load target-pathname next-import-env)
				(set-working-directory-pathname! working-directory)
				(copy-bindings symbols next-export-env import-env)))))

(define import-list
	(make-import-list
		(the-environment)
		(make-string-hash-table)))
