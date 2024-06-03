
type field_info = {
  access_flags : Shared.field_access_flags;
  name : string;
  descriptor : string;
  attributes : Attr.attribute list;
}
type class_file = {
  major_version : int;
  minor_version : int;
  const_pool : Constpool.const_pool;
  access_flags : Shared.class_access_flags;
  this_class : string;
  super_class : string option;
  interfaces : string list;
  fields : field_info list;
  methods : Java.jmethod list;
  attributes : Attr.attribute list;
}
val convert_class_file : class_file -> Shared.jloader -> Java.jclass
val read_class_file : in_channel -> class_file
