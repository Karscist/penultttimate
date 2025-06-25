(require :asdf)
(asdf:initialize-source-registry 
 `(:source-registry (:tree ,(uiop:getcwd)) :inherit-configuration))

(let ((arch "x86_64-pc-linux-gnu")
      (path (merge-pathnames #P"lib/" (asdf:component-pathname (asdf:find-system '#:claw-raylib)))))
  (dolist (lib '("raylib" "rlgl" "raygui"))
    (uiop:run-program
     (list "gcc" "-O3" "-fPIC" "-shared" "-Iraylib/src" "-Iraygui/src"
           "-o" (namestring (merge-pathnames (format nil "lib~A-adapter.so" lib) path))
           (namestring (merge-pathnames (format nil "lib~A-adapter.~A.c" lib arch) path))))))
    
