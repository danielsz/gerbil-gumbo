(def ldflags (env-ldflags))
(def cppflags (env-cppflags))
(import :std/build-script)
(defbuild-script '(("gumbo" "-ld-options" "-lgumbo")))