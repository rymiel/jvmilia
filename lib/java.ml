open Shared
open Attr

type jmethod = {
  name : string;
  access_flags : method_access_flags;
  desc : string;
  attributes : attribute list;
  nargs : int;
  arg_types : Type.dtype list;
  ret_type : Type.dtype;
  cls : string;
}

type jfield = {
  name : string;
  access_flags : field_access_flags;
  desc : string;
  attributes : attribute list;
  field_type : Type.dtype;
  cls : string;
}

type jclass = {
  name : string;
  access_flags : class_access_flags;
  superclass : string option;
  superinterfaces : string list;
  methods : jmethod list;
  fields : jfield list;
  attributes : attribute list;
  loader : jloader;
}
