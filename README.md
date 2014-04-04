qt-generator
============

Bindings generator for Qt with support to multiple languages

This project is built with `cmake`.  We recommend using a build
directory:

    mkdir build
    cd build
    cmake ..
    make

There are a couple of configuration options available. They can
be configured via `ccmake` or by command line parameters to
`cmake`:

    cmake -DBINDINGS_TARGET=build_all.xml ..
    cmake -DGENERATOR_LANG=JAVA ..

The bindings target should be one of the `build_*.xml` files in
`src/targets`.

The generator language should currently be one of:

* `JAVA`: This will generate Java bindings.
* `DYLAN`: This will generate Dylan bindings for use with [Open Dylan](http://opendylan.org/).
