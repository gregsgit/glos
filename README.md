# glos - Generic Little Object System

GLOS is a library of Scheme functions and macros that adds two primary
bits of functionality to Scheme:  

* *Types*:  top, bottom, (and? t1 t2 ...), (or? t1 t2 ...), 
(== val), predicates (boolean-returning functions), and record
subtypes. 

* *Dynamic Dispatch*:  When a *generic function* is called, it
selects the _most applicable method_ based on matching the types
of the arguments against the specializers of each method.  This is
multiple dispatch, as opposed to the single dispatch of C++, Java,
Smalltalk, etc.  GLOS also supports method combination (before, after,
and around) methods.


Currently the code is specific to Scheme48 (see <a
href="http://www.s48.org/">www.s48.org</a>), but only very slightly so.
Ports welcome.

To load GLOS, fire up scheme48, open the file test.scm, and paste the
first five lines from test.scm at the scheme48 prompt.

Some description of GLOS is in the paper [ref-dyn-patterns.pdf](ref-dyn-patters.pdf)
