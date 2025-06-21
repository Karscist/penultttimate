
(asdf:initialize-source-registry 
 `(:source-registry (:tree :here) :inherit-configuration))

(asdf:defsystem "penultttimate"
  :depends-on (:claw-raylib))
