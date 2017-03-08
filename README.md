waa
===

TDMA memory access and instruction count analyzer


Usage: wcma class-path class-name

Note
- Class-name should be given without the .class extension
- Should be a fully qualified name, .e.g,: java.lang.Object
- Should have the main function

## How to build
1. Download __javalib 2.3.2__ and update `c_const` field of a type `'a jclass` to __mutable__ in `src/jClass.ml`, `src/jClass.mli` and `src/javalib.mli`.
2. Rename `src/jClassLow.mli` to `src/jClassLow.ml`.
3. Open `src/Makefile` and add `jClassLow` in between `jCode` and `jClass` elements in the list variable `Module`.
4. Bulild the package and install.
5. Install __sawja 1.5.2__.
