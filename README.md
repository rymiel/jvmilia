# JVMilia

An experiment in creating a Java Virtual Machine in OCaml.

### Type-check a class file

```sh
dune exec jvmilia -- verify java/lang/Object
```

### Execute a class file

```sh
dune build
export LD_LIBRARY_PATH="_build/install/default/lib/jvmilia/"
dune exec jvmilia -- exec test/Main
```

Class files are loaded from `class/`