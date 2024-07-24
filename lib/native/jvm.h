#pragma once

#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "jni.h"
#include <cassert>
#include <cstring>
#include <deque>
#include <filesystem>
#include <string>
#include <unordered_map>
#include <vector>

extern "C" struct JNINativeInterface;
namespace jvmilia {
using bridge_t = jvalue(void*, JNIEnv*, std::vector<jvalue>);

struct JNIFrame {
  std::vector<std::shared_ptr<value>> localReferences;
};

auto make_reference(value v) -> std::shared_ptr<value>;
struct JVMData {
  std::unordered_map<std::string, void*> registeredNatives;
  std::unordered_map<std::string, bridge_t*> cachedBridges;
  std::unordered_map<std::string, value*> cachedJMethods;
  std::deque<JNIFrame> frames;
  std::vector<std::shared_ptr<value>> globalReferences;
  std::filesystem::path temp;

  value callbacks;

  value find_class_callback() { return Field(callbacks, 0); };
  value get_static_method_callback() { return Field(callbacks, 1); };
  value class_name_callback() { return Field(callbacks, 2); };
  value make_string_callback() { return Field(callbacks, 3); };
  value invoke_method_callback() { return Field(callbacks, 4); };
  value get_virtual_method_callback() { return Field(callbacks, 5); };
  value make_object_array_callback() { return Field(callbacks, 6); };
  value set_object_array_callback() { return Field(callbacks, 7); };
  value reference_type_name_callback() { return Field(callbacks, 8); };
  value object_instance_field_callback() { return Field(callbacks, 9); };
  value make_class_direct_callback() { return Field(callbacks, 10); };
  value string_hash_callback() { return Field(callbacks, 11); }
  value get_field_by_hash_callback() { return Field(callbacks, 12); }

  auto make_local_reference(value v) -> std::shared_ptr<value>& {
    return frames.back().localReferences.emplace_back(make_reference(v));
  }

  // little helper to avoid allocating the field name
  auto get_object_field(value evalue, const char* field_name) -> value {
    CAMLparam1(evalue);
    CAMLlocal1(n);

    n = caml_copy_string(field_name);

    CAMLreturn(caml_callback2(this->object_instance_field_callback(), evalue, n));
  }

  auto class_name(jclass clazz) -> const char* {
    CAMLparam0();
    CAMLlocal1(result);

    result = caml_callback(this->class_name_callback(), *std::bit_cast<value*>(clazz));

    CAMLreturnT(const char*, String_val(result));
  }

  auto object_type_name(jobject obj) -> const char* {
    if (obj == nullptr)
      return "null";
    return String_val(caml_callback(this->reference_type_name_callback(), *std::bit_cast<value*>(obj)));
  }

  auto string_value(jstring str) -> value {
    CAMLparam0();
    CAMLlocal3(str_obj, name_obj, val_obj);

    str_obj = *std::bit_cast<value*>(str);
    name_obj = caml_callback(this->reference_type_name_callback(), str_obj);
    assert(strcmp(String_val(name_obj), "java/lang/String") == 0);
    val_obj = this->get_object_field(str_obj, "value");
    assert(Is_block(val_obj) && Tag_val(val_obj) == 4 && Wosize_val(val_obj) == 1); // ByteArray

    CAMLreturn(Field(val_obj, 0));
  }

  auto string_content(jstring str) -> const char* { return String_val(string_value(str)); }
};

inline auto make_reference(value v) -> std::shared_ptr<value> {
  static auto deleter = [](value* p) {
    printf("reference deleting: %p\n", p);
    caml_remove_global_root(p);
    delete p;
  };
  auto raw = new value;
  *raw = v;
  auto p = std::shared_ptr<value>(raw, deleter);

  caml_register_global_root(p.get());

  return p;
}

struct Context {
  const JNINativeInterface* interface;
  JVMData* data;
};

inline JVMData* getData(JNIEnv* env) {
  Context* context = std::bit_cast<Context*>(env);
  //   __builtin_dump_struct(context->data, printf);
  return context->data;
}

inline std::string registerKey(const char* className, const char* methodName, const char* signature) {
  return std::string(className) + ";" + methodName + ";" + signature;
}
} // namespace jvmilia