#!/usr/bin/env gxi

(import :std/make :std/build-script)

(def ldflags (env-ldflags))
(def cppflags (env-cppflags))

(defbuild-script `((gxc:"gumbo" "-ld-options" ,(ldflags "-lgumbo") "-cc-options" ,(cppflags ""))))