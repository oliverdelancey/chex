(asdf:defsystem "chex"
                :description "EXecute commands in CHild process"
                :version "0.5.0"
                :author "Oliver Delancey"
                :license "MIT"
                ;:depends-on ()
                :serial t
                :components ((:module "chex"
                                      :serial t
                                      :components ((:file "package")
                                                   (:file "chex")))))
