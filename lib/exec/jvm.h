#pragma once

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

inline auto class_name(JVMData* data, jclass clazz) -> const char* {
  CAMLparam0();
  CAMLlocal1(result);

  result = caml_callback(data->class_name_callback, *std::bit_cast<value*>(clazz));

  CAMLreturnT(const char*, String_val(result));
}

inline std::string registerKey(const char* className, const char* methodName, const char* signature) {
  return std::string(className) + ";" + methodName + ";" + signature;
}
} // namespace jvmilia