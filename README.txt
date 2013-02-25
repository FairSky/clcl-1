clcl is a preprocessor for OpenCL/C99/JS.

It doesn't do anything interesting yet. WIP, so use it only if you
plan on contributing.

It'll feature a ML-style type system, Haskell-inspired type classes,
as well as an interactive environment. FFI for backends, as well
as fully optional runtime.

If you plan on hacking on it, due to the presence of type declarations
for slots, it's recommended to do:

(declaim (optimize (debug 3)
         (safety 3)
         (space 0)
         (compilation-speed 0)
         (speed 0)))
(asdf:oos 'asdf:load-op :clcl-tests :force t)

to save yourself some trouble when inevitable type errors occur.
