#pragma once

#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "jni.h"
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
  value find_class_callback;
  value get_static_method_callback;
  value class_name_callback;
  value make_string_callback;
  value invoke_method_callback;
  value get_virtual_method_callback;
  value make_object_array_callback;
  value set_object_array_callback;
  value object_type_name_callback;
  value object_instance_field_callback;
  value make_class_direct_callback;

  auto make_local_reference(value v) -> std::shared_ptr<value>& {
    return frames.back().localReferences.emplace_back(make_reference(v));
  }

  // little helper to avoid allocating the field name
  auto get_object_field(value evalue, const char* field_name) -> value {
    CAMLparam1(evalue);
    CAMLlocal1(n);

    n = caml_copy_string(field_name);

    CAMLreturn(caml_callback2(this->object_instance_field_callback, evalue, n));
  }

  auto class_name(jclass clazz) -> const char* {
    CAMLparam0();
    CAMLlocal1(result);

    result = caml_callback(this->class_name_callback, *std::bit_cast<value*>(clazz));

    CAMLreturnT(const char*, String_val(result));
  }
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