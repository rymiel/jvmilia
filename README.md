# JVMilia

An experiment in creating a Java Virtual Machine in OCaml.

### Type-check a class file

```sh
dune exec jvmilia -- verify java/lang/Object
```

Class files are loaded from `class/`