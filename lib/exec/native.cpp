#include "native.h"
#include "jni.h"
#include <bit>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <cassert>
#include <concepts>
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
#include <unistd.h>
#include <vector>

using jvmilia::JVMData, jvmilia::Context;

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
    printf("error: load_library_native: dlopen: %s: %s\n", path_str, err);
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

CAMLprim value make_native_interface_native(value unit) {
  (void)unit;

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

  Context* context = new Context;
  context->interface = interface;
  context->data = data;

  // __builtin_dump_struct(context, printf);
  printf("interface: %p; data: %p; context: %p\n", interface, data, context);

  return handle_to_value(context);
}

CAMLprim value free_native_interface_native(value handle) {
  auto* context = value_to_handle<Context>(handle);

  std::filesystem::remove_all(context->data->temp);

  delete context;

  return Val_unit;
}

bool value_is_cons(value v) {
  CAMLparam1(v);

  CAMLreturnT(bool, Is_block(v) && Tag_val(v) == 0 && Wosize_val(v) == 2);
}

void iter_list(value input, std::invocable<value> auto fn) {
  CAMLparam1(input);
  CAMLlocal3(list, a, b);

  list = input;

  while (true) {
    if (list == Val_emptylist) {
      // printf("(empty list)\n");
      CAMLreturn0;
    } else {
      assert(value_is_cons(list));
      a = Field(list, 0);
      b = Field(list, 1);
      // printf("car = %p; cdr = %p\n", std::bit_cast<void*>(a), std::bit_cast<void*>(b));
      fn(a);
      list = b;
    }
  }

  CAMLreturn0;
}

auto list_vector(value list) -> std::vector<value> {
  CAMLparam1(list);

  auto vec = std::vector<value>{};

  iter_list(list, [&vec](value v) { vec.push_back(v); });

  CAMLreturnT(auto&, vec);
}

template <std::invocable<value> Fn>
auto list_vector_map(value list, Fn fn) -> std::vector<std::invoke_result_t<Fn, value>> {
  CAMLparam1(list);

  auto vec = std::vector<std::invoke_result_t<Fn, value>>{};

  iter_list(list, [&vec, &fn](value v) { vec.push_back(fn(v)); });

  CAMLreturnT(auto&, vec);
}

void dump_value(value input, int max_depth = 100, std::vector<int> depth = {}) {
  CAMLparam1(input);

  int indent = depth.size();

  for (int i = 0; i < indent; i++) {
    std::putchar('\t');
  }

  if (indent > 0) {
    std::putchar('[');
    bool flag = false;
    for (int i : depth) {
      if (flag) {
        std::putchar('.');
      }
      printf("%d", i);
      flag = true;
    }
    std::printf("] ");
  }

  if (Is_block(input)) {
    auto tag = Tag_val(input);
    if (tag < No_scan_tag) {
      auto wosize = Wosize_val(input);
      std::printf("block (tag=%d, wosize=%lu, p=%p)", tag, wosize, std::bit_cast<void*>(input));
      if (max_depth == 0) {
        std::puts(" <...>");
        CAMLreturn0;
      } else {
        std::puts("");
        for (mlsize_t i = 0; i < wosize; i++) {
          auto d = depth;
          d.push_back(i);
          dump_value(Field(input, i), max_depth - 1, d);
        }
      }
    } else if (tag == String_tag) {
      std::printf("string (\"%s\")\n", String_val(input));
    } else {
      std::printf("raw block (tag=%d, p=%p)\n", tag, std::bit_cast<void*>(input));
    }
  } else {
    std::printf("int %ld\n", Long_val(input));
  }

  CAMLreturn0;
}

bool evalue_is_class(value evalue) {
  CAMLparam1(evalue);

  CAMLreturnT(bool, Is_block(evalue) && Tag_val(evalue) == 0 && Wosize_val(evalue) == 1);
}

const char* evalue_class_name(value evalue) {
  CAMLparam1(evalue);

  assert(evalue_is_class(evalue));

  // Class.eclassvalue(0).cls(0).raw(0).name(0)
  const char* name = String_val(Field(Field(Field(Field(evalue, 0), 0), 0), 0));

  CAMLreturnT(const char*, name);
}

jvalue evalue_conversion(value v) {
  jvalue j = {};
  if (evalue_is_class(v)) {
    j.l = std::bit_cast<jclass>(evalue_class_name(v));
    return j;
  } else {
    std::puts("unimplement evalue");
    std::exit(4);
  }
}

enum struct vtype { Nil, Class, Void, Int };

value reconstruct_evalue(jvalue j, vtype ty) {
  CAMLparam0();
  CAMLlocal1(result);

  switch (ty) {
  case vtype::Int: {
    result = caml_alloc(1, 1);
    Store_field(result, 0, Val_int(j.i));
    CAMLreturn(result);
  }
  case vtype::Class:
  case vtype::Void:
  case vtype::Nil: std::puts("Unimplemented"); std::exit(7);
  }

  __builtin_unreachable();
}

auto vtype_string(vtype v) -> std::string_view {
  switch (v) {
  case vtype::Class: return "class";
  case vtype::Void: return "void";
  case vtype::Nil: return "nil";
  case vtype::Int: return "int";
  }
  __builtin_unreachable();
}

void vtype_c_type(vtype ty, std::ostream& os) {
  switch (ty) {
  case vtype::Class: os << "void*"; return;
  case vtype::Void: os << "void"; return;
  case vtype::Nil: os << "nil"; return;
  case vtype::Int: os << "int"; return;
  }
  __builtin_unreachable();
}

void vtype_c_active_union(vtype ty, std::ostream& os) {
  switch (ty) {
  case vtype::Class: os << "l"; return;
  case vtype::Void: os << "void"; return;
  case vtype::Nil: os << "nil"; return;
  case vtype::Int: os << "i"; return;
  }
  __builtin_unreachable();
}

auto vtype_conversion(value v) -> vtype {
  CAMLparam1(v);

  if (Is_block(v)) {
    switch (Tag_val(v)) {
    case 0: dump_value(v); caml_failwith("Invalid vtype in this context");
    case 1: {
      const char* name = String_val(Field(v, 0));
      if (strcmp(name, "java/lang/Class") == 0) {
        CAMLreturnT(vtype, vtype::Class);
      }
      caml_failwith("Unimplemented class vtype");
    }
    default: caml_failwith("Unimplemented block vtype");
    }
  } else {
    switch (Long_val(v)) {
    case 2: return vtype::Int;
    case 11: return vtype::Void;
    default: caml_failwith("Unimplemented integer vtype");
    }
  }
}

std::string build_bridge_name(std::vector<vtype>& args, vtype ret) {
  std::stringstream bridge_os{};
  bridge_os << "jvmilia_bridge_";
  for (auto v : args) {
    vtype_c_active_union(v, bridge_os);
  }
  bridge_os << "_";
  vtype_c_active_union(ret, bridge_os);

  return bridge_os.str();
}

void build_source(std::vector<vtype>& args, vtype ret, std::ostream& os) {
  bool not_void = ret != vtype::Void;
  os << "#include <vector>\n#include <bit>\n\n";
  os << "union jvalue { unsigned char z; signed char b; unsigned short c; short s; int i; long j; float f; double d; "
        "void* l; };\n\n";
  vtype_c_type(ret, os);
  os << " target(";
  os << "void** env, void* receiver";
  int i = 0;
  for (auto v : args) {
    os << ", ";
    vtype_c_type(v, os);
    os << " arg" << i;
    i++;
  }
  os << ");\n";
  os << "using target_type = decltype(target);\n\n";
  os << "extern \"C\" jvalue jvmilia_bridge_";
  for (auto v : args) {
    vtype_c_active_union(v, os);
  }
  os << "_";
  vtype_c_active_union(ret, os);
  os << "(void* fn_ptr, void** env, std::vector<jvalue> args) {\n";
  os << "  auto function = std::bit_cast<target_type*>(fn_ptr);\n";
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
    vtype_c_active_union(v, os);
    i++;
  }
  os << ");\n";
  if (not_void) {
    os << "  ret.";
    vtype_c_active_union(ret, os);
    os << " = result;\n";
  }
  os << "  return ret;\n";
  os << "}\n";
}

using fn_t = void (*)(void);
using bridge_t = jvalue (*)(fn_t*, JNIEnv*, std::vector<jvalue>);
CAMLprim value execute_native_auto_native(value interface, value params, value param_types, value ret_type,
                                          value fn_ptr) {
  CAMLparam5(interface, params, param_types, ret_type, fn_ptr);
  auto* context = value_to_handle<Context>(interface);

  // std::printf("params: %p\nparam_types: %p\nret_type: %p\n", std::bit_cast<void*>(params),
  //             std::bit_cast<void*>(param_types), std::bit_cast<void*>(ret_type));

  // std::puts("params:");
  // int i = 0;
  // iter_list(params, [&i](value v) {
  //   dump_value(v, 3, {i});
  //   i++;
  // });
  // std::puts("param_types:");
  // i = 0;
  // iter_list(param_types, [&i](value v) {
  //   dump_value(v, 4, {i});
  //   i++;
  // });
  // std::printf("ret_type: ");
  // dump_value(ret_type, 4);

  auto arg_evalues = list_vector_map(params, &evalue_conversion);
  auto param_vtypes = list_vector_map(param_types, &vtype_conversion);
  for (auto v : param_vtypes) {
    std::printf("<- %s\n", vtype_string(v).data());
  }

  auto ret_vtype = vtype_conversion(ret_type);
  std::printf("-> %s\n", vtype_string(ret_vtype).data());

  auto bridge_name = build_bridge_name(param_vtypes, ret_vtype);
  build_source(param_vtypes, ret_vtype, std::cout);
  auto src_path = create_temporary_file(context->data->temp, bridge_name, "source.cpp");
  auto dst_path = create_temporary_file(context->data->temp, bridge_name, "lib.so");
  auto os = std::ofstream(src_path.c_str());
  build_source(param_vtypes, ret_vtype, os);
  os.flush();

  auto child = fork();
  std::printf("fork: %d\n", child);
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

  void* library = dlopen(dst_path.c_str(), RTLD_LAZY | RTLD_GLOBAL);

  const char* err = dlerror();
  if (err != nullptr) {
    printf("error: execute_native_auto_native: dlopen: %s: %s\n", dst_path.c_str(), err);
    std::exit(1);
  }

  void* bridge_ptr = dlsym(library, bridge_name.c_str());
  // TODO: cache
  err = dlerror();
  if (err != nullptr) {
    printf("error: execute_native_auto_native: dlsym: %s: %s\n", dst_path.c_str(), err);
    std::exit(1);
  }

  unlink(src_path.c_str());
  unlink(dst_path.c_str());

  auto* bridge = std::bit_cast<bridge_t>(bridge_ptr);
  std::printf("bridge: %p\n", bridge);

  auto result = bridge(value_to_handle<fn_t>(fn_ptr), &context->interface, arg_evalues);

  std::printf("result: %lx\n", result.j);

  if (ret_vtype == vtype::Void) {
    CAMLreturn(Val_unit);
  }

  CAMLreturn(reconstruct_evalue(result, ret_vtype));
}

CAMLprim value get_registered_fnptr_native(value interface_int, value class_name, value method, value signature) {
  CAMLparam4(interface_int, class_name, method, signature);

  auto* context = value_to_handle<Context>(interface_int);
  const char* cls_string = String_val(class_name);
  const char* mth_string = String_val(method);
  const char* sig_string = String_val(signature);

  auto key = jvmilia::registerKey(cls_string, mth_string, sig_string);
  printf("get_registered_fnptr_native: looking up: %s\n", key.data());
  auto& map = context->data->registeredNatives;
  auto iter = map.find(key);
  if (iter == map.end()) {
    printf("get_registered_fnptr_native: didn't find %s\n", key.data());
    CAMLreturn(Val_none);
  }

  void* fnptr = iter->second;
  printf("get_registered_fnptr_native: found %s: %p\n", key.data(), fnptr);
  auto fnptr_value = handle_to_value(fnptr);
  auto return_value = caml_alloc_some(fnptr_value);
  CAMLreturn(return_value);
}
