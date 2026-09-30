(library (shared-library)
  (export load-shared-library)
  (import
    (scheme)
    (lets))

  (define load-shared-library
    (lambda (name)
      (lets
        ($machine-type (machine-type))
        ($filename
          (case $machine-type
            ((i3nt ti3nt a6nt ta6nt) (string-append name ".dll"))
            ((i3le ti3le a6le ta6le) (string-append "lib" name ".so"))
            ((i3osx ti3osx a6osx ta6osx arm64osx tarm64osx) (string-append "lib" name ".dylib"))
            (else (error 'load-shared-object-named "unsupported machine type" $machine-type))))
        (load-shared-object $filename)))))
