#include "native.h"
#include "caml_interface.h"
#include "jni.h"
#include "jvm.h"
#include <bit>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <dlfcn.h>
#include <elf.h>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <link.h>
#include <sstream>
#include <string_view>
#include <sys/wait.h>
#include <tuple>
#include <unistd.h>
#include <vector>

namespace jvmilia {

std::filesystem::path create_temporary_directory() {
  auto tmp_dir = std::filesystem::temp_directory_path();
  auto path = tmp_dir / "jvmilia";
  std::filesystem::create_directory(path);
  return path;
}

std::filesystem::path create_temporary_file(std::filesystem::path& temp_dir, std::string_view id,
                                            std::string_view suffix) {
  auto i = getpid();
  return temp_dir / (std::to_string(i) + "_" + std::string(id) + "_" + std::string(suffix));
}

value handle_to_value(void* handle) { return Val_long(std::bit_cast<uint64_t>(handle)); }

template <typename T = void> T* value_to_handle(value val) { return std::bit_cast<T*>(Long_val(val)); }

CAMLprim value load_library_native(value path) {
  const char* path_str = String_val(path);
  void* library = dlopen(path_str, RTLD_LAZY | RTLD_GLOBAL);

  const char* err = dlerror();
  if (err != nullptr) {
    log_printf("error: load_library_native: dlopen: %s: %s\n", path_str, err);
    std::exit(1);
  }

  return handle_to_value(library);
}

CAMLprim value load_method_native(value lib_val, value name_val) {
  void* lib = value_to_handle(lib_val);
  const char* name = String_val(name_val);

  void* method = dlsym(lib, name);

  return handle_to_value(method);
}

// this could go in a separate file
namespace codegen {
std::string build_bridge_name(std::vector<ntype>& args, ntype ret) {
  std::stringstream bridge_os{};
  bridge_os << "jvmilia_bridge_";
  for (auto v : args) {
    ntype_c_active_union(v, bridge_os);
  }
  bridge_os << "_";
  ntype_c_active_union(ret, bridge_os);

  return bridge_os.str();
}

void build_source_header(std::ostream& os) {
  os << "#include <vector>\n#include <bit>\n\n";
  os << "union jvalue { unsigned char z; signed char b; unsigned short c; short s; int i; long j; float f; double d; "
        "void* l; };\n\n";
  os.flush();
}

void build_source_function(std::vector<ntype>& args, ntype ret, std::ostream& os, int key) {
  bool not_void = ret != ntype::Void;

  ntype_c_type(ret, os);
  os << " target" << key << "(";
  os << "void** env, void* receiver";
  int i = 0;
  for (auto v : args) {
    os << ", ";
    ntype_c_type(v, os);
    os << " arg" << i;
    i++;
  }
  os << ");\n";
  os << "using target" << key << "_type = decltype(target" << key << ");\n\n";
  os << "extern \"C\" jvalue jvmilia_bridge_";
  for (auto v : args) {
    ntype_c_active_union(v, os);
  }
  os << "_";
  ntype_c_active_union(ret, os);
  os << "(void* fn_ptr, void** env, std::vector<jvalue> args) {\n";
  os << "  auto function = std::bit_cast<target" << key << "_type*>(fn_ptr);\n";
  os << "  jvalue ret;\n";
  os << "  ret.j = 0;\n";
  os << "  ";
  if (not_void) {
    os << "auto result = ";
  }
  os << "function(env, args[0].l";
  i = 1;
  for (auto v : args) {
    os << ", args[";
    os << i << "].";
    ntype_c_active_union(v, os);
    i++;
  }
  os << ");\n";
  if (not_void) {
    os << "  ret.";
    ntype_c_active_union(ret, os);
    os << " = result;\n";
  }
  os << "  return ret;\n";
  os << "}\n\n";
  os.flush();
}

void build_source(std::vector<ntype>& args, ntype ret, std::ostream& os, int key = 0) {
  build_source_header(os);
  build_source_function(args, ret, os, key);
}

void compile_shared_library(std::filesystem::path& src_path, std::filesystem::path& dst_path) {
  auto child = fork();
  log_printf("fork: %d\n", child);
  if (child == 0) {
    execlp("clang++", "clang++", src_path.c_str(), "-std=c++20", "-shared", "-o", dst_path.c_str(), nullptr);
    std::exit(1);
  } else {
    int status;
    do {
      if (::waitpid(child, &status, 0) == -1) {
        std::perror("waitpid");
        break;
      }
    } while (!WIFEXITED(status) && !WIFSIGNALED(status));
    std::puts("Exited");
  }
}

auto load_and_cache_bridge_function(std::filesystem::path& lib_path, std::string_view bridge_name,
                                    std::unordered_map<std::string, bridge_t*>& cache) -> bridge_t* {
  void* library = dlopen(lib_path.c_str(), RTLD_LAZY | RTLD_GLOBAL);

  const char* err = dlerror();
  if (err != nullptr) {
    log_printf("error: load_bridge_function: dlopen: %s: %s\n", lib_path.c_str(), err);
    std::exit(1);
  }

  void* bridge_ptr = dlsym(library, bridge_name.data());
  err = dlerror();
  if (err != nullptr) {
    log_printf("error: load_bridge_function: dlsym: %s: %s\n", lib_path.c_str(), err);
    std::exit(1);
  }

  bridge_t* bridge = std::bit_cast<bridge_t*>(bridge_ptr);
  cache.insert_or_assign(std::string(bridge_name), bridge);

  return bridge;
}
} // namespace codegen

CAMLprim value make_native_interface_native(value interface_data) {
  const char* env = getenv("JVMILIA_NATIVE_LOG");
  if (env != nullptr && strcmp(env, "silent") == 0) {
    silent = true;
  }

  JNINativeInterface* interface = new JNINativeInterface;
  *interface = {
      nullptr,
      nullptr,
      nullptr,
      nullptr,

      &jvmilia::GetVersion,
      &jvmilia::DefineClass,
      &jvmilia::FindClass,
      &jvmilia::FromReflectedMethod,
      &jvmilia::FromReflectedField,
      &jvmilia::ToReflectedMethod,
      &jvmilia::GetSuperclass,
      &jvmilia::IsAssignableFrom,
      &jvmilia::ToReflectedField,
      &jvmilia::Throw,
      &jvmilia::ThrowNew,
      &jvmilia::ExceptionOccurred,
      &jvmilia::ExceptionDescribe,
      &jvmilia::ExceptionClear,
      &jvmilia::FatalError,
      &jvmilia::PushLocalFrame,
      &jvmilia::PopLocalFrame,
      &jvmilia::NewGlobalRef,
      &jvmilia::DeleteGlobalRef,
      &jvmilia::DeleteLocalRef,
      &jvmilia::IsSameObject,
      &jvmilia::NewLocalRef,
      &jvmilia::EnsureLocalCapacity,
      &jvmilia::AllocObject,
      &jvmilia::NewObject,
      &jvmilia::NewObjectV,
      &jvmilia::NewObjectA,
      &jvmilia::GetObjectClass,
      &jvmilia::IsInstanceOf,
      &jvmilia::GetMethodID,
      &jvmilia::CallObjectMethod,
      &jvmilia::CallObjectMethodV,
      &jvmilia::CallObjectMethodA,
      &jvmilia::CallBooleanMethod,
      &jvmilia::CallBooleanMethodV,
      &jvmilia::CallBooleanMethodA,
      &jvmilia::CallByteMethod,
      &jvmilia::CallByteMethodV,
      &jvmilia::CallByteMethodA,
      &jvmilia::CallCharMethod,
      &jvmilia::CallCharMethodV,
      &jvmilia::CallCharMethodA,
      &jvmilia::CallShortMethod,
      &jvmilia::CallShortMethodV,
      &jvmilia::CallShortMethodA,
      &jvmilia::CallIntMethod,
      &jvmilia::CallIntMethodV,
      &jvmilia::CallIntMethodA,
      &jvmilia::CallLongMethod,
      &jvmilia::CallLongMethodV,
      &jvmilia::CallLongMethodA,
      &jvmilia::CallFloatMethod,
      &jvmilia::CallFloatMethodV,
      &jvmilia::CallFloatMethodA,
      &jvmilia::CallDoubleMethod,
      &jvmilia::CallDoubleMethodV,
      &jvmilia::CallDoubleMethodA,
      &jvmilia::CallVoidMethod,
      &jvmilia::CallVoidMethodV,
      &jvmilia::CallVoidMethodA,
      &jvmilia::CallNonvirtualObjectMethod,
      &jvmilia::CallNonvirtualObjectMethodV,
      &jvmilia::CallNonvirtualObjectMethodA,
      &jvmilia::CallNonvirtualBooleanMethod,
      &jvmilia::CallNonvirtualBooleanMethodV,
      &jvmilia::CallNonvirtualBooleanMethodA,
      &jvmilia::CallNonvirtualByteMethod,
      &jvmilia::CallNonvirtualByteMethodV,
      &jvmilia::CallNonvirtualByteMethodA,
      &jvmilia::CallNonvirtualCharMethod,
      &jvmilia::CallNonvirtualCharMethodV,
      &jvmilia::CallNonvirtualCharMethodA,
      &jvmilia::CallNonvirtualShortMethod,
      &jvmilia::CallNonvirtualShortMethodV,
      &jvmilia::CallNonvirtualShortMethodA,
      &jvmilia::CallNonvirtualIntMethod,
      &jvmilia::CallNonvirtualIntMethodV,
      &jvmilia::CallNonvirtualIntMethodA,
      &jvmilia::CallNonvirtualLongMethod,
      &jvmilia::CallNonvirtualLongMethodV,
      &jvmilia::CallNonvirtualLongMethodA,
      &jvmilia::CallNonvirtualFloatMethod,
      &jvmilia::CallNonvirtualFloatMethodV,
      &jvmilia::CallNonvirtualFloatMethodA,
      &jvmilia::CallNonvirtualDoubleMethod,
      &jvmilia::CallNonvirtualDoubleMethodV,
      &jvmilia::CallNonvirtualDoubleMethodA,
      &jvmilia::CallNonvirtualVoidMethod,
      &jvmilia::CallNonvirtualVoidMethodV,
      &jvmilia::CallNonvirtualVoidMethodA,
      &jvmilia::GetFieldID,
      &jvmilia::GetObjectField,
      &jvmilia::GetBooleanField,
      &jvmilia::GetByteField,
      &jvmilia::GetCharField,
      &jvmilia::GetShortField,
      &jvmilia::GetIntField,
      &jvmilia::GetLongField,
      &jvmilia::GetFloatField,
      &jvmilia::GetDoubleField,
      &jvmilia::SetObjectField,
      &jvmilia::SetBooleanField,
      &jvmilia::SetByteField,
      &jvmilia::SetCharField,
      &jvmilia::SetShortField,
      &jvmilia::SetIntField,
      &jvmilia::SetLongField,
      &jvmilia::SetFloatField,
      &jvmilia::SetDoubleField,
      &jvmilia::GetStaticMethodID,
      &jvmilia::CallStaticObjectMethod,
      &jvmilia::CallStaticObjectMethodV,
      &jvmilia::CallStaticObjectMethodA,
      &jvmilia::CallStaticBooleanMethod,
      &jvmilia::CallStaticBooleanMethodV,
      &jvmilia::CallStaticBooleanMethodA,
      &jvmilia::CallStaticByteMethod,
      &jvmilia::CallStaticByteMethodV,
      &jvmilia::CallStaticByteMethodA,
      &jvmilia::CallStaticCharMethod,
      &jvmilia::CallStaticCharMethodV,
      &jvmilia::CallStaticCharMethodA,
      &jvmilia::CallStaticShortMethod,
      &jvmilia::CallStaticShortMethodV,
      &jvmilia::CallStaticShortMethodA,
      &jvmilia::CallStaticIntMethod,
      &jvmilia::CallStaticIntMethodV,
      &jvmilia::CallStaticIntMethodA,
      &jvmilia::CallStaticLongMethod,
      &jvmilia::CallStaticLongMethodV,
      &jvmilia::CallStaticLongMethodA,
      &jvmilia::CallStaticFloatMethod,
      &jvmilia::CallStaticFloatMethodV,
      &jvmilia::CallStaticFloatMethodA,
      &jvmilia::CallStaticDoubleMethod,
      &jvmilia::CallStaticDoubleMethodV,
      &jvmilia::CallStaticDoubleMethodA,
      &jvmilia::CallStaticVoidMethod,
      &jvmilia::CallStaticVoidMethodV,
      &jvmilia::CallStaticVoidMethodA,
      &jvmilia::GetStaticFieldID,
      &jvmilia::GetStaticObjectField,
      &jvmilia::GetStaticBooleanField,
      &jvmilia::GetStaticByteField,
      &jvmilia::GetStaticCharField,
      &jvmilia::GetStaticShortField,
      &jvmilia::GetStaticIntField,
      &jvmilia::GetStaticLongField,
      &jvmilia::GetStaticFloatField,
      &jvmilia::GetStaticDoubleField,
      &jvmilia::SetStaticObjectField,
      &jvmilia::SetStaticBooleanField,
      &jvmilia::SetStaticByteField,
      &jvmilia::SetStaticCharField,
      &jvmilia::SetStaticShortField,
      &jvmilia::SetStaticIntField,
      &jvmilia::SetStaticLongField,
      &jvmilia::SetStaticFloatField,
      &jvmilia::SetStaticDoubleField,
      &jvmilia::NewString,
      &jvmilia::GetStringLength,
      &jvmilia::GetStringChars,
      &jvmilia::ReleaseStringChars,
      &jvmilia::NewStringUTF,
      &jvmilia::GetStringUTFLength,
      &jvmilia::GetStringUTFChars,
      &jvmilia::ReleaseStringUTFChars,
      &jvmilia::GetArrayLength,
      &jvmilia::NewObjectArray,
      &jvmilia::GetObjectArrayElement,
      &jvmilia::SetObjectArrayElement,
      &jvmilia::NewBooleanArray,
      &jvmilia::NewByteArray,
      &jvmilia::NewCharArray,
      &jvmilia::NewShortArray,
      &jvmilia::NewIntArray,
      &jvmilia::NewLongArray,
      &jvmilia::NewFloatArray,
      &jvmilia::NewDoubleArray,
      &jvmilia::GetBooleanArrayElements,
      &jvmilia::GetByteArrayElements,
      &jvmilia::GetCharArrayElements,
      &jvmilia::GetShortArrayElements,
      &jvmilia::GetIntArrayElements,
      &jvmilia::GetLongArrayElements,
      &jvmilia::GetFloatArrayElements,
      &jvmilia::GetDoubleArrayElements,
      &jvmilia::ReleaseBooleanArrayElements,
      &jvmilia::ReleaseByteArrayElements,
      &jvmilia::ReleaseCharArrayElements,
      &jvmilia::ReleaseShortArrayElements,
      &jvmilia::ReleaseIntArrayElements,
      &jvmilia::ReleaseLongArrayElements,
      &jvmilia::ReleaseFloatArrayElements,
      &jvmilia::ReleaseDoubleArrayElements,
      &jvmilia::GetBooleanArrayRegion,
      &jvmilia::GetByteArrayRegion,
      &jvmilia::GetCharArrayRegion,
      &jvmilia::GetShortArrayRegion,
      &jvmilia::GetIntArrayRegion,
      &jvmilia::GetLongArrayRegion,
      &jvmilia::GetFloatArrayRegion,
      &jvmilia::GetDoubleArrayRegion,
      &jvmilia::SetBooleanArrayRegion,
      &jvmilia::SetByteArrayRegion,
      &jvmilia::SetCharArrayRegion,
      &jvmilia::SetShortArrayRegion,
      &jvmilia::SetIntArrayRegion,
      &jvmilia::SetLongArrayRegion,
      &jvmilia::SetFloatArrayRegion,
      &jvmilia::SetDoubleArrayRegion,
      &jvmilia::RegisterNatives,
      &jvmilia::UnregisterNatives,
      &jvmilia::MonitorEnter,
      &jvmilia::MonitorExit,
      &jvmilia::GetJavaVM,
      &jvmilia::GetStringRegion,
      &jvmilia::GetStringUTFRegion,
      &jvmilia::GetPrimitiveArrayCritical,
      &jvmilia::ReleasePrimitiveArrayCritical,
      &jvmilia::GetStringCritical,
      &jvmilia::ReleaseStringCritical,
      &jvmilia::NewWeakGlobalRef,
      &jvmilia::DeleteWeakGlobalRef,
      &jvmilia::ExceptionCheck,
      &jvmilia::NewDirectByteBuffer,
      &jvmilia::GetDirectBufferAddress,
      &jvmilia::GetDirectBufferCapacity,
      &jvmilia::GetObjectRefType,
      &jvmilia::GetModule,
      &jvmilia::IsVirtualThread,
  };
  JVMData* data = new JVMData;

  data->temp = create_temporary_directory();
  data->callbacks = interface_data;
  caml_register_global_root(&data->callbacks);

  Context* context = new Context;
  context->interface = interface;
  context->data = data;

  // __builtin_dump_struct(context, printf);
  log_printf("interface: %p; data: %p; context: %p\n", interface, data, context);

  using enum ntype;
  const std::tuple<std::vector<ntype>, ntype> to_preload[] = {
      {{}, Void},       {{Reference}, Int},
      {{}, Reference},  {{Reference}, Reference},
      {{}, Int},        {{Reference, Int, Reference, Int, Int}, Void},
      {{}, Long},       {{Float}, Int},
      {{Double}, Long},
  };

  auto src_path = create_temporary_file(context->data->temp, "preload", "source.cpp");
  auto dst_path = create_temporary_file(context->data->temp, "preload", "lib.so");
  auto os = std::ofstream(src_path.c_str());
  codegen::build_source_header(std::cout);
  codegen::build_source_header(os);
  int i = 0;
  for (auto [args, ret] : to_preload) {
    codegen::build_source_function(args, ret, std::cout, i);
    codegen::build_source_function(args, ret, os, i++);
  }

  codegen::compile_shared_library(src_path, dst_path);
  for (auto [args, ret] : to_preload) {
    auto bridge_name = codegen::build_bridge_name(args, ret);
    auto* bridge = codegen::load_and_cache_bridge_function(dst_path, bridge_name, context->data->cachedBridges);
    log_printf("%s: %p\n", bridge_name.c_str(), bridge);
  }

  unlink(src_path.c_str());
  unlink(dst_path.c_str());

  return handle_to_value(context);
}

CAMLprim value free_native_interface_native(value handle) {
  auto* context = value_to_handle<Context>(handle);

  caml_remove_global_root(&context->data->callbacks);

  for (auto [k, v] : context->data->cachedJMethods) {
    caml_remove_global_root(v);
    delete v;
  }

  std::filesystem::remove_all(context->data->temp);

  delete context;

  return Val_unit;
}

CAMLprim value execute_native_auto_native(value interface, value params, value param_types, value ret_type,
                                          value fn_ptr) {
  CAMLparam5(interface, params, param_types, ret_type, fn_ptr);
  CAMLlocal1(result_value);
  auto* context = value_to_handle<Context>(interface);

  context->data->frames.emplace_back();

  auto param_vtypes = list_vector_map(param_types, &ntype_of_dtype);
  for (auto v : param_vtypes) {
    log_printf("<- %s\n", ntype_string(v).data());
  }

  auto ret_vtype = ntype_of_dtype(ret_type);
  log_printf("-> %s\n", ntype_string(ret_vtype).data());

  auto bridge_name = codegen::build_bridge_name(param_vtypes, ret_vtype);
  bridge_t* bridge;

  auto& map = context->data->cachedBridges;
  auto iter = map.find(bridge_name);
  if (iter != map.end()) {
    log_printf("found cached bridge for %s: %p\n", bridge_name.c_str(), iter->second);
    bridge = iter->second;
  } else {
    codegen::build_source(param_vtypes, ret_vtype, std::cout);
    auto src_path = create_temporary_file(context->data->temp, bridge_name, "source.cpp");
    auto dst_path = create_temporary_file(context->data->temp, bridge_name, "lib.so");
    auto os = std::ofstream(src_path.c_str());
    codegen::build_source(param_vtypes, ret_vtype, os);

    codegen::compile_shared_library(src_path, dst_path);
    bridge = codegen::load_and_cache_bridge_function(dst_path, bridge_name, context->data->cachedBridges);

    unlink(src_path.c_str());
    unlink(dst_path.c_str());
  }

  log_printf("bridge: %p\n", bridge);

  auto arg_evalues = list_vector_map(params, [&context](value v) {
    auto ref = context->data->frames.back().localReferences.emplace_back(make_reference(v));
    return evalue_conversion(ref.get());
  });
  auto result = bridge(value_to_handle(fn_ptr), &context->interface, arg_evalues);

  log_printf("result: %lx\n", result.j);

  if (ret_vtype == ntype::Void) {
    context->data->frames.pop_back();
    CAMLreturn(Val_unit);
  }

  result_value = reconstruct_evalue(result, ret_vtype);
  context->data->frames.pop_back();
  CAMLreturn(result_value);
}

CAMLprim value get_registered_fnptr_native(value interface_int, value class_name, value method, value signature) {
  CAMLparam4(interface_int, class_name, method, signature);

  auto* context = value_to_handle<Context>(interface_int);
  const char* cls_string = String_val(class_name);
  const char* mth_string = String_val(method);
  const char* sig_string = String_val(signature);

  auto key = jvmilia::registerKey(cls_string, mth_string, sig_string);
  log_printf("get_registered_fnptr_native: looking up: %s\n", key.data());
  auto& map = context->data->registeredNatives;
  auto iter = map.find(key);
  if (iter == map.end()) {
    log_printf("get_registered_fnptr_native: didn't find %s\n", key.data());
    CAMLreturn(Val_none);
  }

  void* fnptr = iter->second;
  log_printf("get_registered_fnptr_native: found %s: %p\n", key.data(), fnptr);
  auto fnptr_value = handle_to_value(fnptr);
  auto return_value = caml_alloc_some(fnptr_value);
  CAMLreturn(return_value);
}

bool silent = false;
int log_printf(const char* format, ...) {
  va_list args;
  va_start(args, format);

  if (!silent)
    return vprintf(format, args);

  va_end(args);
  return 0;
}

} // namespace jvmilia