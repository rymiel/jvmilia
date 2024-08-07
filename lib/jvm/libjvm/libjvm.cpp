#include "../../native/caml_interface.h"
#include "../../native/jni.h"
#include "../../native/jvm.h"
#include "../../native/native.h"
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/memory.h"
#include <cassert>
#include <csignal>
#include <cstdio>
#include <cstdlib>
#include <cstring>

using jvmilia::log_printf;

[[noreturn]] void unimplemented(const char* methodName) {
  log_printf("libjvm: Unimplemented libjvm method %s\n", methodName);
  std::exit(1);
}

[[noreturn]] void unimplemented_unsafe(const char* methodName) {
  log_printf("libjvm shim: Unsafe: unimplemented %s\n", methodName);
  std::exit(1);
}

jint unsafe_arrayBaseOffset0(JNIEnv* env, jobject unsafe, jclass clazz) {
  (void)unsafe;
  jvmilia::JVMData* data = jvmilia::getData(env);
  log_printf("libjvm shim: Unsafe: arrayBaseOffset0: %s\n", data->class_name(clazz));

  return 0;
}

jint unsafe_arrayIndexScale0(JNIEnv* env, jobject unsafe, jclass clazz) {
  (void)unsafe;
  jvmilia::JVMData* data = jvmilia::getData(env);
  log_printf("libjvm shim: Unsafe: arrayIndexScale0: %s\n", data->class_name(clazz));

  return 1;
}

// TODO: actually use the fields
jlong unsafe_objectFieldOffset1(JNIEnv* env, jobject unsafe, jclass clazz, jstring name) {
  CAMLparam0();
  CAMLlocal1(offset);
  jvmilia::JVMData* data = jvmilia::getData(env);
  (void)unsafe;

  log_printf("libjvm shim: Unsafe: objectFieldOffset1: %s %s\n", data->class_name(clazz), data->string_content(name));

  offset = caml_callback(data->string_hash_callback(), data->string_value(name));

  log_printf("libjvm shim: Unsafe: objectFieldOffset1: %s %s -> %lx\n", data->class_name(clazz),
             data->string_content(name), Long_val(offset));

  CAMLreturnT(jlong, Long_val(offset));
}

void unsafe_fullFence(JNIEnv* env, jobject unsafe) {
  // we don't have threading!
  (void)env;
  (void)unsafe;
}

struct field_ref {
  value ref;
  long offset;

  auto get() -> value { return Field(ref, offset); }
  void set(value v) { Store_field(ref, offset, v); }

  // some of this could go into caml_interface or that file should be removed
  auto is_int() -> bool { return Is_block(get()) && Tag_val(get()) == jvmilia::EVALUE_INT_TAG; }
  auto as_int() -> int { return Int32_val(Field(get(), 0)); }

  auto is_long() -> bool { return Is_block(get()) && Tag_val(get()) == jvmilia::EVALUE_LONG_TAG; }
  auto as_long() -> long { return Int64_val(Field(get(), 0)); }

  auto is_reference() -> bool {
    return (Is_block(get()) && Tag_val(get()) == jvmilia::EVALUE_OBJECT_TAG) || get() == jvmilia::EVALUE_NULL;
  }
  auto as_jobject(jvmilia::JVMData* data) -> jobject { return jvmilia::coerce_null(get(), data); }
};

field_ref offset_field(jvmilia::JVMData* data, jobject obj, jlong offset) {
  CAMLparam0();
  value obj_val = jvmilia::coerce_null(obj);
  value ref;
  long field_offset;

  if (Is_block(obj_val) && Tag_val(obj_val) == jvmilia::EVALUE_ARRAY_TAG) {
    ref = Field(Field(obj_val, 0), 1);
    field_offset = offset;
  } else {
    ref = caml_callback2(data->get_field_by_hash_callback(), obj_val, Val_int(offset));
    field_offset = 0;
  }

  CAMLreturnT(field_ref, (field_ref{ref, field_offset}));
}

// some of this could go into caml_interface or that file should be removed
value evalue_int32(int i) {
  CAMLparam0();
  CAMLlocal2(int32, val);
  int32 = caml_copy_int32(i);
  val = caml_alloc(1, jvmilia::EVALUE_INT_TAG);
  Store_field(val, 0, int32);
  CAMLreturn(val);
}
value evalue_int64(long i) {
  CAMLparam0();
  CAMLlocal2(int64, val);
  int64 = caml_copy_int64(i);
  val = caml_alloc(1, jvmilia::EVALUE_LONG_TAG);
  Store_field(val, 0, int64);
  CAMLreturn(val);
}

jboolean unsafe_compareAndSetInt(JNIEnv* env, jobject unsafe, jobject obj, jlong offset, jint e, jint x) {
  CAMLparam0();
  jvmilia::JVMData* data = jvmilia::getData(env);
  (void)unsafe;

  log_printf("libjvm shim: Unsafe: compareAndSetInt: %s %lx (%d -> %d)\n", data->object_type_name(obj), offset, e, x);

  auto field = offset_field(data, obj, offset);

  jvmilia::dump_value(field.get(), 4);
  assert(field.is_int());
  int actual = field.as_int();

  log_printf("libjvm shim: Unsafe: compareAndSetInt: %s %lx %d -> %d, actual %d\n", data->object_type_name(obj), offset,
             e, x, actual);

  if (e == actual) {
    field.set(evalue_int32(x));
    CAMLreturnT(jboolean, true);
  } else {
    CAMLreturnT(jboolean, false);
  }
}

jboolean unsafe_compareAndSetLong(JNIEnv* env, jobject unsafe, jobject obj, jlong offset, jlong e, jlong x) {
  CAMLparam0();
  jvmilia::JVMData* data = jvmilia::getData(env);
  (void)unsafe;

  auto field = offset_field(data, obj, offset);

  jvmilia::dump_value(field.get(), 4);
  assert(field.is_long());
  long actual = field.as_long();

  log_printf("libjvm shim: Unsafe: compareAndSetLong: %s %lx %ld -> %ld, actual %ld\n", data->object_type_name(obj),
             offset, e, x, actual);

  if (e == actual) {
    field.set(evalue_int64(x));
    CAMLreturnT(jboolean, true);
  } else {
    CAMLreturnT(jboolean, false);
  }
}

jboolean unsafe_compareAndSetReference(JNIEnv* env, jobject unsafe, jobject obj, jlong offset, jobject e, jobject x) {
  CAMLparam0();
  CAMLlocal3(val, e_v, x_v);
  jvmilia::JVMData* data = jvmilia::getData(env);
  (void)unsafe;

  e_v = jvmilia::coerce_null(e);
  x_v = jvmilia::coerce_null(x);
  log_printf("libjvm shim: Unsafe: compareAndSetReference: %s %lx (%lx %s -> %lx %s)\n", data->object_type_name(obj),
             offset, e_v, data->object_type_name(e), x_v, data->object_type_name(x));

  auto field = offset_field(data, obj, offset);

  val = field.get();
  jvmilia::dump_value(val, 4);
  assert(field.is_reference()); // assert it's a reference

  if (val == jvmilia::EVALUE_NULL && e_v == jvmilia::EVALUE_NULL) { // i dont actually handle objects yet
    field.set(x_v);
    CAMLreturnT(jboolean, true);
  } else {
    assert(false);
    CAMLreturnT(jboolean, false);
  }
}

jlong unsafe_getLongVolatile(JNIEnv* env, jobject unsafe, jobject obj, jlong offset) {
  CAMLparam0();
  jvmilia::JVMData* data = jvmilia::getData(env);
  (void)unsafe;

  log_printf("libjvm shim: Unsafe: getLongVolatile: %s %lx\n", data->object_type_name(obj), offset);

  auto field = offset_field(data, obj, offset);

  jvmilia::dump_value(field.get(), 4);
  assert(field.is_long());

  CAMLreturnT(jlong, field.as_long());
}

jobject unsafe_getReferenceVolatile(JNIEnv* env, jobject unsafe, jobject obj, jlong offset) {
  CAMLparam0();
  jvmilia::JVMData* data = jvmilia::getData(env);
  (void)unsafe;

  log_printf("libjvm shim: Unsafe: getReferenceVolatile: %s %lx\n", data->object_type_name(obj), offset);

  auto field = offset_field(data, obj, offset);

  jvmilia::dump_value(field.get(), 4);
  assert(field.is_reference());

  CAMLreturnT(jobject, field.as_jobject(data));
}

static JNINativeMethod unsafe_native_methods[] = {
    {"arrayBaseOffset0", "(Ljava/lang/Class;)I", std::bit_cast<void*>(&unsafe_arrayBaseOffset0)},
    {"arrayIndexScale0", "(Ljava/lang/Class;)I", std::bit_cast<void*>(&unsafe_arrayIndexScale0)},
    {"objectFieldOffset1", "(Ljava/lang/Class;Ljava/lang/String;)J", std::bit_cast<void*>(&unsafe_objectFieldOffset1)},
    {"fullFence", "()V", std::bit_cast<void*>(&unsafe_fullFence)},
    {"compareAndSetInt", "(Ljava/lang/Object;JII)Z", std::bit_cast<void*>(&unsafe_compareAndSetInt)},
    {"compareAndSetLong", "(Ljava/lang/Object;JJJ)Z", std::bit_cast<void*>(&unsafe_compareAndSetLong)},
    {"compareAndSetReference", "(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z",
     std::bit_cast<void*>(&unsafe_compareAndSetReference)},
    {"getLongVolatile", "(Ljava/lang/Object;J)J", std::bit_cast<void*>(&unsafe_getLongVolatile)},
    {"getReferenceVolatile", "(Ljava/lang/Object;J)Ljava/lang/Object;",
     std::bit_cast<void*>(&unsafe_getReferenceVolatile)},
};

static JNINativeMethod sma_native_methods[] = {};

extern "C" {
void Java_jdk_internal_misc_Unsafe_registerNatives(JNIEnv* env, jclass unsafecls) {
  log_printf("libjvm shim: jdk.internal.misc.Unsafe registerNatives\n");
  (*env)->RegisterNatives(env, unsafecls, unsafe_native_methods,
                          sizeof(unsafe_native_methods) / sizeof(JNINativeMethod));
}

void Java_jdk_internal_misc_ScopedMemoryAccess_registerNatives(JNIEnv* env, jclass smaclass) {
  log_printf("libjvm shim: jdk.internal.misc.ScopedMemoryAccess registerNatives\n");
  (*env)->RegisterNatives(env, smaclass, sma_native_methods, sizeof(sma_native_methods) / sizeof(JNINativeMethod));
}

void Java_java_lang_Object_notifyAll(JNIEnv* env, jobject object) {
  // we don't have threading!
  (void)env;
  (void)object;
}

jboolean JVM_DesiredAssertionStatus(JNIEnv* env, jclass unused, jclass cls) {
  jvmilia::JVMData* data = jvmilia::getData(env);
  auto* unused_name = data->class_name(unused);
  auto* cls_name = data->class_name(cls);
  log_printf("libjvm: JVM_DesiredAssertionStatus: %s, %s\n", unused_name, cls_name);

  return 0;
}

jobjectArray JVM_GetProperties(JNIEnv* env) {
  log_printf("libjvm: JVM_GetProperties\n");
  jclass string = jvmilia::FindClass(env, "java/lang/String");
  auto array = jvmilia::NewObjectArray(env, 3, string, nullptr);
  auto element0 = jvmilia::NewStringUTF(env, "java.home");
  jvmilia::SetObjectArrayElement(env, array, 0, element0);
  auto element1 = jvmilia::NewStringUTF(env, "/jvmilia/non/existent/path/");
  jvmilia::SetObjectArrayElement(env, array, 1, element1);
  return array;
}

jclass JVM_FindPrimitiveClass(JNIEnv* env, const char* utf) {
  CAMLparam0();
  CAMLlocal2(n, result);
  jvmilia::JVMData* data = jvmilia::getData(env);
  log_printf("libjvm: JVM_FindPrimitiveClass: %s\n", utf);
  if (strcmp(utf, "float") == 0) {
    n = caml_copy_string("/float");
  } else if (strcmp(utf, "int") == 0) {
    n = caml_copy_string("/int");
  } else if (strcmp(utf, "byte") == 0) {
    n = caml_copy_string("/byte");
  } else if (strcmp(utf, "double") == 0) {
    n = caml_copy_string("/double");
  } else if (strcmp(utf, "char") == 0) {
    n = caml_copy_string("/char");
  } else {
    log_printf("libjvm: JVM_FindPrimitiveClass: unimplemented primitive class %s\n", utf);
    assert(false);
  }
  result = caml_callback(data->make_class_direct_callback(), n);
  auto ref = data->make_local_reference(result);
  log_printf("libjvm: JVM_FindPrimitiveClass %s -> %lx (%p)\n", utf, result, ref.get());
  CAMLreturnT(jclass, std::bit_cast<jclass>(ref.get()));
}

jboolean JVM_IsPrimitiveClass(JNIEnv* env, jclass cls) {
  jvmilia::JVMData* data = jvmilia::getData(env);
  const char* name = data->class_name(cls);
  return name[0] == '/';
}

void print_array(unsigned char* arr, unsigned long len) {
  for (unsigned long i = 0; i < len; i++) {
    unsigned char v = arr[i];
    if (v == 0) {
      log_printf("\\0");
    } else {
      log_printf("%c", v);
    }
  }
}

void JVM_ArrayCopy(JNIEnv* env, jclass ignored, jobject src, jint src_pos, jobject dst, jint dst_pos, jint length) {
  CAMLparam0();
  CAMLlocal2(src_val, dst_val);
  (void)env;
  (void)ignored;

  log_printf("libjvm: JVM_ArrayCopy: %d -> %d, length %d\n", src_pos, dst_pos, length);
  src_val = *std::bit_cast<value*>(src);
  dst_val = *std::bit_cast<value*>(dst);

  assert(Tag_val(src_val) == 4 && Tag_val(dst_val) == 4); // only byte arrays for now
  assert(src_pos >= 0 && dst_pos >= 0 && length >= 0);

  unsigned char* src_bytes = Bytes_val(Field(src_val, 0));
  unsigned char* dst_bytes = Bytes_val(Field(dst_val, 0));
  auto src_len = caml_string_length(Field(src_val, 0));
  auto dst_len = caml_string_length(Field(dst_val, 0));

  print_array(src_bytes, src_len);
  log_printf(" -> ");
  print_array(dst_bytes, dst_len);
  log_printf("\n");

  memmove(dst_bytes + dst_pos, src_bytes + src_pos, length);

  print_array(src_bytes, src_len);
  log_printf(" -> ");
  print_array(dst_bytes, dst_len);
  log_printf("\n");

  CAMLreturn0;
}

jlong JVM_MaxMemory() { return 9223372036854775807l; }
jint JVM_ActiveProcessorCount() { return 1; }

struct signal_def {
  int signal;
  const char* name;
};
static constexpr signal_def SIGNALS[] = {{SIGHUP, "HUP"}, {SIGINT, "INT"}, {SIGTERM, "TERM"}};

jint JVM_FindSignal(const char* name) {
  log_printf("libjvm: JVM_FindSignal: %s\n", name);
  for (signal_def def : SIGNALS) {
    if (strcmp(def.name, name) == 0) {
      log_printf("libjvm: JVM_FindSignal: %s -> %d\n", name, def.signal);
      return def.signal;
    }
  }

  log_printf("libjvm: JVM_FindSignal: %s: not found\n", name);
  assert(false);
}

void* JVM_RegisterSignal(jint sig, void* handler) {
  const char* name = "unknown";
  for (signal_def def : SIGNALS) {
    if (def.signal == sig) {
      name = def.name;
    }
  }
  log_printf("libjvm: JVM_RegisterSignal: %d (%s) %p\n", sig, name, handler);

  if (std::bit_cast<long>(handler) == 2) {
    // User handler. Do nothing for now.
    return nullptr;
  }

  unimplemented("JVM_RegisterSignal");
  return nullptr;
}

// NOTE: removed after jdk 22
jboolean JVM_IsDumpingClassList(JNIEnv* env) {
  (void)env;
  log_printf("libjvm: JVM_IsDumpingClassList\n");
  return false;
}

// NOTE: removed after jdk 22
jboolean JVM_IsCDSDumpingEnabled(JNIEnv* env) {
  (void)env;
  log_printf("libjvm: JVM_IsCDSDumpingEnabled\n");
  return false;
}

// NOTE: removed after jdk 22
jboolean JVM_IsSharingEnabled(JNIEnv* env) {
  (void)env;
  log_printf("libjvm: JVM_IsSharingEnabled\n");
  return false;
}

void JVM_InitializeFromArchive(JNIEnv* env, jclass cls) {
  log_printf("libjvm: JVM_InitializeFromArchive: %s\n", jvmilia::getData(env)->class_name(cls));
  return;
}

jboolean JVM_IsFinalizationEnabled(JNIEnv* env) {
  (void)env;
  log_printf("libjvm: JVM_IsFinalizationEnabled\n");
  return false;
}

void store_object_field(jvmilia::JVMData* data, value object, const char* name, value v) {
  CAMLparam2(object, v);
  CAMLlocal2(name_val, field);

  name_val = caml_copy_string(name);
  field = caml_callback2(data->object_instance_field_callback(), object, name_val);
  Store_field(field, 0, v);

  CAMLreturn0;
}

jobject JVM_CurrentThread(JNIEnv* env, jclass threadClass) {
  CAMLparam0();
  CAMLlocal5(thread_val, thread_holder_name, thread_holder_val, main_name, main_name_val);
  CAMLlocal4(thread_group_name, thread_group_val, system_name, system_name_val);
  (void)threadClass;
  jvmilia::JVMData* data = jvmilia::getData(env);

  log_printf("libjvm: JVM_CurrentThread\n");

  if (data->primoridal_thread) {
    log_printf("libjvm: JVM_CurrentThread: existing %p\n", data->primoridal_thread.get());
    CAMLreturnT(jobject, std::bit_cast<jobject>(data->primoridal_thread.get()));
  }

  log_printf("libjvm: JVM_CurrentThread: initializing primordial thread\n");

  thread_val = caml_callback(data->make_new_callback(), data->class_name_value(threadClass));

  main_name = caml_copy_string("main");
  main_name_val = caml_callback(data->make_string_callback(), main_name);
  store_object_field(data, thread_val, "name", main_name_val);

  thread_holder_name = caml_copy_string("java/lang/Thread$FieldHolder");
  thread_holder_val = caml_callback(data->make_new_callback(), thread_holder_name);

  store_object_field(data, thread_holder_val, "threadStatus", evalue_int32(0x4 + 0x1)); // runnable + alive

  // TODO: I should be calling the constructor instead of doing all this
  thread_group_name = caml_copy_string("java/lang/ThreadGroup");
  thread_group_val = caml_callback(data->make_new_callback(), thread_group_name);

  system_name = caml_copy_string("system");
  system_name_val = caml_callback(data->make_string_callback(), system_name);
  store_object_field(data, thread_group_val, "name", system_name_val);

  store_object_field(data, thread_group_val, "maxPriority", evalue_int32(10)); // magic value

  store_object_field(data, thread_holder_val, "group", thread_group_val);

  store_object_field(data, thread_val, "holder", thread_holder_val);

  // caml_callback(data->print_evalue_detailed_callback(), thread_val);
  auto ref = data->make_local_reference(thread_val);
  data->primoridal_thread = ref;
  log_printf("libjvm: JVM_CurrentThread: initialized primordial thread: %p\n", ref.get());

  CAMLreturnT(jobject, std::bit_cast<jobject>(ref.get()));
}

jlong JVM_GetNextThreadIdOffset(JNIEnv* env, jclass threadClass) {
  CAMLparam0();
  CAMLlocal2(name, offset);
  jvmilia::JVMData* data = jvmilia::getData(env);
  (void)threadClass;

  log_printf("libjvm: JVM_GetNextThreadIdOffset\n");

  name = caml_copy_string("__jvmilia_next_tid");
  offset = caml_callback(data->string_hash_callback(), name);

  log_printf("libjvm: JVM_GetNextThreadIdOffset -> %lx\n", Long_val(offset));
  CAMLreturnT(jlong, Long_val(offset));
}

jobject JVM_GetStackAccessControlContext(JNIEnv* env, jclass cls) {
  (void)env;
  (void)cls;
  log_printf("libjvm: JVM_GetStackAccessControlContext\n");
  return nullptr; // I don't know what else to do
}

void JVM_SetThreadPriority(JNIEnv* env, jobject jthread, jint prio) {
  CAMLparam0();
  CAMLlocal1(name);
  jvmilia::JVMData* data = jvmilia::getData(env);
  name = data->get_object_field(jvmilia::coerce_null(jthread), "name");
  log_printf("libjvm: JVM_SetThreadPriority: %s <- %d\n", data->string_content(std::bit_cast<jstring>(&Field(name, 0))),
             prio);
  CAMLreturn0;
}

void JVM_StartThread(JNIEnv* env, jobject jthread) {
  CAMLparam0();
  CAMLlocal1(name);
  jvmilia::JVMData* data = jvmilia::getData(env);
  name = data->get_object_field(jvmilia::coerce_null(jthread), "name");
  log_printf("libjvm: JVM_StartThread: %s\n", data->string_content(std::bit_cast<jstring>(&Field(name, 0))));
  CAMLreturn0;
}

void JVM_AddModuleExports() { unimplemented("JVM_AddModuleExports"); }
void JVM_AddModuleExportsToAll() { unimplemented("JVM_AddModuleExportsToAll"); }
void JVM_AddModuleExportsToAllUnnamed() { unimplemented("JVM_AddModuleExportsToAllUnnamed"); }
void JVM_AddReadsModule() { unimplemented("JVM_AddReadsModule"); }
void JVM_AreNestMates() { unimplemented("JVM_AreNestMates"); }
void JVM_AssertionStatusDirectives() { unimplemented("JVM_AssertionStatusDirectives"); }
void JVM_BeforeHalt() { unimplemented("JVM_BeforeHalt"); }
void JVM_CallStackWalk() { unimplemented("JVM_CallStackWalk"); }
void JVM_ConstantPoolGetClassAt() { unimplemented("JVM_ConstantPoolGetClassAt"); }
void JVM_ConstantPoolGetClassAtIfLoaded() { unimplemented("JVM_ConstantPoolGetClassAtIfLoaded"); }
void JVM_ConstantPoolGetClassRefIndexAt() { unimplemented("JVM_ConstantPoolGetClassRefIndexAt"); }
void JVM_ConstantPoolGetDoubleAt() { unimplemented("JVM_ConstantPoolGetDoubleAt"); }
void JVM_ConstantPoolGetFieldAt() { unimplemented("JVM_ConstantPoolGetFieldAt"); }
void JVM_ConstantPoolGetFieldAtIfLoaded() { unimplemented("JVM_ConstantPoolGetFieldAtIfLoaded"); }
void JVM_ConstantPoolGetFloatAt() { unimplemented("JVM_ConstantPoolGetFloatAt"); }
void JVM_ConstantPoolGetIntAt() { unimplemented("JVM_ConstantPoolGetIntAt"); }
void JVM_ConstantPoolGetLongAt() { unimplemented("JVM_ConstantPoolGetLongAt"); }
void JVM_ConstantPoolGetMemberRefInfoAt() { unimplemented("JVM_ConstantPoolGetMemberRefInfoAt"); }
void JVM_ConstantPoolGetMethodAt() { unimplemented("JVM_ConstantPoolGetMethodAt"); }
void JVM_ConstantPoolGetMethodAtIfLoaded() { unimplemented("JVM_ConstantPoolGetMethodAtIfLoaded"); }
void JVM_ConstantPoolGetNameAndTypeRefIndexAt() { unimplemented("JVM_ConstantPoolGetNameAndTypeRefIndexAt"); }
void JVM_ConstantPoolGetNameAndTypeRefInfoAt() { unimplemented("JVM_ConstantPoolGetNameAndTypeRefInfoAt"); }
void JVM_ConstantPoolGetSize() { unimplemented("JVM_ConstantPoolGetSize"); }
void JVM_ConstantPoolGetStringAt() { unimplemented("JVM_ConstantPoolGetStringAt"); }
void JVM_ConstantPoolGetTagAt() { unimplemented("JVM_ConstantPoolGetTagAt"); }
void JVM_ConstantPoolGetUTF8At() { unimplemented("JVM_ConstantPoolGetUTF8At"); }
void JVM_CurrentCarrierThread() { unimplemented("JVM_CurrentCarrierThread"); }
void JVM_CurrentTimeMillis() { unimplemented("JVM_CurrentTimeMillis"); }
void JVM_DefineArchivedModules() { unimplemented("JVM_DefineArchivedModules"); }
void JVM_DefineClassWithSource() { unimplemented("JVM_DefineClassWithSource"); }
void JVM_DefineModule() { unimplemented("JVM_DefineModule"); }
void JVM_DumpClassListToFile() { unimplemented("JVM_DumpClassListToFile"); }
void JVM_DumpDynamicArchive() { unimplemented("JVM_DumpDynamicArchive"); }
void JVM_DumpThreads() { unimplemented("JVM_DumpThreads"); }
void JVM_EnsureMaterializedForStackWalk_func() { unimplemented("JVM_EnsureMaterializedForStackWalk_func"); }
void JVM_ExpandStackFrameInfo() { unimplemented("JVM_ExpandStackFrameInfo"); }
void JVM_FillInStackTrace() { unimplemented("JVM_FillInStackTrace"); }
void JVM_FindClassFromBootLoader() { unimplemented("JVM_FindClassFromBootLoader"); }
void JVM_FindClassFromCaller() { unimplemented("JVM_FindClassFromCaller"); }
void JVM_FindLibraryEntry() { unimplemented("JVM_FindLibraryEntry"); }
void JVM_FindLoadedClass() { unimplemented("JVM_FindLoadedClass"); }
void JVM_FindScopedValueBindings() { unimplemented("JVM_FindScopedValueBindings"); }
void JVM_FreeMemory() { unimplemented("JVM_FreeMemory"); }
void JVM_GC() { unimplemented("JVM_GC"); }
void JVM_GetAllThreads() { unimplemented("JVM_GetAllThreads"); }
void JVM_GetAndClearReferencePendingList() { unimplemented("JVM_GetAndClearReferencePendingList"); }
void JVM_GetArrayElement() { unimplemented("JVM_GetArrayElement"); }
void JVM_GetArrayLength() { unimplemented("JVM_GetArrayLength"); }
void JVM_GetCallerClass() { unimplemented("JVM_GetCallerClass"); }
void JVM_GetClassAccessFlags() { unimplemented("JVM_GetClassAccessFlags"); }
void JVM_GetClassAnnotations() { unimplemented("JVM_GetClassAnnotations"); }
void JVM_GetClassConstantPool() { unimplemented("JVM_GetClassConstantPool"); }
void JVM_GetClassContext() { unimplemented("JVM_GetClassContext"); }
void JVM_GetClassDeclaredConstructors() { unimplemented("JVM_GetClassDeclaredConstructors"); }
void JVM_GetClassDeclaredFields() { unimplemented("JVM_GetClassDeclaredFields"); }
void JVM_GetClassDeclaredMethods() { unimplemented("JVM_GetClassDeclaredMethods"); }
void JVM_GetClassFileVersion() { unimplemented("JVM_GetClassFileVersion"); }
void JVM_GetClassInterfaces() { unimplemented("JVM_GetClassInterfaces"); }
void JVM_GetClassModifiers() { unimplemented("JVM_GetClassModifiers"); }
void JVM_GetClassSignature() { unimplemented("JVM_GetClassSignature"); }
void JVM_GetClassSigners() { unimplemented("JVM_GetClassSigners"); }
void JVM_GetClassTypeAnnotations() { unimplemented("JVM_GetClassTypeAnnotations"); }
void JVM_GetDeclaredClasses() { unimplemented("JVM_GetDeclaredClasses"); }
void JVM_GetDeclaringClass() { unimplemented("JVM_GetDeclaringClass"); }
void JVM_GetEnclosingMethodInfo() { unimplemented("JVM_GetEnclosingMethodInfo"); }
void JVM_GetExtendedNPEMessage() { unimplemented("JVM_GetExtendedNPEMessage"); }
void JVM_GetFieldTypeAnnotations() { unimplemented("JVM_GetFieldTypeAnnotations"); }
void JVM_GetInheritedAccessControlContext() { unimplemented("JVM_GetInheritedAccessControlContext"); }
void JVM_GetMethodParameters() { unimplemented("JVM_GetMethodParameters"); }
void JVM_GetMethodTypeAnnotations() { unimplemented("JVM_GetMethodTypeAnnotations"); }
void JVM_GetNanoTimeAdjustment() { unimplemented("JVM_GetNanoTimeAdjustment"); }
void JVM_GetNestHost() { unimplemented("JVM_GetNestHost"); }
void JVM_GetNestMembers() { unimplemented("JVM_GetNestMembers"); }
void JVM_GetPermittedSubclasses() { unimplemented("JVM_GetPermittedSubclasses"); }
void JVM_GetPrimitiveArrayElement() { unimplemented("JVM_GetPrimitiveArrayElement"); }
void JVM_GetProtectionDomain() { unimplemented("JVM_GetProtectionDomain"); }
void JVM_GetRandomSeedForDumping() { unimplemented("JVM_GetRandomSeedForDumping"); }
void JVM_GetRecordComponents() { unimplemented("JVM_GetRecordComponents"); }
void JVM_GetSimpleBinaryName() { unimplemented("JVM_GetSimpleBinaryName"); }
void JVM_GetStackTrace() { unimplemented("JVM_GetStackTrace"); }
void JVM_GetSystemPackage() { unimplemented("JVM_GetSystemPackage"); }
void JVM_GetSystemPackages() { unimplemented("JVM_GetSystemPackages"); }
void JVM_GetTemporaryDirectory() { unimplemented("JVM_GetTemporaryDirectory"); }
void JVM_GetVmArguments() { unimplemented("JVM_GetVmArguments"); }
void JVM_Halt() { unimplemented("JVM_Halt"); }
void JVM_HasReferencePendingList() { unimplemented("JVM_HasReferencePendingList"); }
void JVM_HoldsLock() { unimplemented("JVM_HoldsLock"); }
void JVM_IHashCode() { unimplemented("JVM_IHashCode"); }
void JVM_InitAgentProperties() { unimplemented("JVM_InitAgentProperties"); }
void JVM_InitClassName() { unimplemented("JVM_InitClassName"); }
void JVM_InitStackTraceElement() { unimplemented("JVM_InitStackTraceElement"); }
void JVM_InitStackTraceElementArray() { unimplemented("JVM_InitStackTraceElementArray"); }
void JVM_InternString() { unimplemented("JVM_InternString"); }
void JVM_Interrupt() { unimplemented("JVM_Interrupt"); }
void JVM_InvokeMethod() { unimplemented("JVM_InvokeMethod"); }
void JVM_IsArrayClass() { unimplemented("JVM_IsArrayClass"); }
void JVM_IsContinuationsSupported() { unimplemented("JVM_IsContinuationsSupported"); }
void JVM_IsForeignLinkerSupported() { unimplemented("JVM_IsForeignLinkerSupported"); }
void JVM_IsHiddenClass() { unimplemented("JVM_IsHiddenClass"); }
void JVM_IsInterface() { unimplemented("JVM_IsInterface"); }
void JVM_IsPreviewEnabled() { unimplemented("JVM_IsPreviewEnabled"); }
void JVM_IsRecord() { unimplemented("JVM_IsRecord"); }
void JVM_IsSupportedJNIVersion() { unimplemented("JVM_IsSupportedJNIVersion"); }
void JVM_IsUseContainerSupport() { unimplemented("JVM_IsUseContainerSupport"); }
void JVM_LatestUserDefinedLoader() { unimplemented("JVM_LatestUserDefinedLoader"); }
void JVM_LoadLibrary() { unimplemented("JVM_LoadLibrary"); }
void JVM_LogLambdaFormInvoker() { unimplemented("JVM_LogLambdaFormInvoker"); }
void JVM_LookupDefineClass() { unimplemented("JVM_LookupDefineClass"); }
void JVM_LookupLambdaProxyClassFromArchive() { unimplemented("JVM_LookupLambdaProxyClassFromArchive"); }
void JVM_MoreStackWalk() { unimplemented("JVM_MoreStackWalk"); }
void JVM_NanoTime() { unimplemented("JVM_NanoTime"); }
void JVM_NewArray() { unimplemented("JVM_NewArray"); }
void JVM_NewInstanceFromConstructor() { unimplemented("JVM_NewInstanceFromConstructor"); }
void JVM_NewMultiArray() { unimplemented("JVM_NewMultiArray"); }
void JVM_PhantomReferenceRefersTo() { unimplemented("JVM_PhantomReferenceRefersTo"); }
void JVM_RaiseSignal() { unimplemented("JVM_RaiseSignal"); }
void JVM_ReferenceClear() { unimplemented("JVM_ReferenceClear"); }
void JVM_ReferenceRefersTo() { unimplemented("JVM_ReferenceRefersTo"); }
void JVM_RegisterContinuationMethods() { unimplemented("JVM_RegisterContinuationMethods"); }
void JVM_RegisterLambdaProxyClassForArchiving() { unimplemented("JVM_RegisterLambdaProxyClassForArchiving"); }
void JVM_ReportFinalizationComplete() { unimplemented("JVM_ReportFinalizationComplete"); }
void JVM_ScopedValueCache() { unimplemented("JVM_ScopedValueCache"); }
void JVM_SetArrayElement() { unimplemented("JVM_SetArrayElement"); }
void JVM_SetBootLoaderUnnamedModule() { unimplemented("JVM_SetBootLoaderUnnamedModule"); }
void JVM_SetClassSigners() { unimplemented("JVM_SetClassSigners"); }
void JVM_SetCurrentThread() { unimplemented("JVM_SetCurrentThread"); }
void JVM_SetNativeThreadName() { unimplemented("JVM_SetNativeThreadName"); }
void JVM_SetPrimitiveArrayElement() { unimplemented("JVM_SetPrimitiveArrayElement"); }
void JVM_SetScopedValueCache() { unimplemented("JVM_SetScopedValueCache"); }
void JVM_SetStackWalkContinuation() { unimplemented("JVM_SetStackWalkContinuation"); }
void JVM_SleepNanos() { unimplemented("JVM_SleepNanos"); }
void JVM_TotalMemory() { unimplemented("JVM_TotalMemory"); }
void JVM_UnloadLibrary() { unimplemented("JVM_UnloadLibrary"); }
void JVM_VirtualThreadEnd() { unimplemented("JVM_VirtualThreadEnd"); }
void JVM_VirtualThreadHideFrames() { unimplemented("JVM_VirtualThreadHideFrames"); }
void JVM_VirtualThreadMount() { unimplemented("JVM_VirtualThreadMount"); }
void JVM_VirtualThreadStart() { unimplemented("JVM_VirtualThreadStart"); }
void JVM_VirtualThreadUnmount() { unimplemented("JVM_VirtualThreadUnmount"); }
void JVM_WaitForReferencePendingList() { unimplemented("JVM_WaitForReferencePendingList"); }
void JVM_Yield() { unimplemented("JVM_Yield"); }

void jio_vfprintf() { unimplemented("jio_vfprintf"); }
void jio_vsnprintf() { unimplemented("jio_vsnprintf"); }
}