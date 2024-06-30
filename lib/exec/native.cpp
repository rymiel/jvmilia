#include "native.h"
#include "jni.h"
#include <bit>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <dlfcn.h>
#include <elf.h>
#include <link.h>
#include <unistd.h>

using jvmilia::JVMData, jvmilia::Context;

value handle_to_value(void* handle) { return caml_copy_int64(std::bit_cast<uint64_t>(handle)); }

template <typename T = void> T* value_to_handle(value val) { return std::bit_cast<T*>(Int64_val(val)); }

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

  Context* context = new Context;
  context->interface = interface;
  context->data = data;

  // __builtin_dump_struct(context, printf);
  printf("interface: %p; data: %p; context: %p\n", interface, data, context);

  return handle_to_value(context);
}

CAMLprim value free_native_interface_native(value handle) {
  auto* context = value_to_handle<Context>(handle);

  delete context;

  return Val_unit;
}

using noargs_void = void(JNIEnv*, jclass);
CAMLprim value execute_native_noargs_void_native(value interface_int, value cls_value, value fn_int) {
  CAMLparam3(interface_int, cls_value, fn_int);
  auto* context = value_to_handle<Context>(interface_int);
  const char* cls_string = String_val(cls_value);
  auto* cls = std::bit_cast<jclass>(cls_string);
  auto* function = value_to_handle<noargs_void>(fn_int);

  if (function == nullptr) {
    std::puts("Function handle is null!");
    std::exit(1);
  }

  function(&context->interface, cls);

  CAMLreturn(Val_unit);
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
