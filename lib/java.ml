open Shared
open Attr

type jmethod = {
  name : string;
  access_flags : method_access_flags;
  desc : string;
  attributes : attribute list;
}

type jclass = {
  name : string;
  access_flags : class_access_flags;
  superclass : string option;
  superinterfaces : string list;
  methods : jmethod list;
  mutable loader : jloader option;
}
