#include "native.h"
#include "jni.h"
#include <bit>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <dlfcn.h>
#include <elf.h>
#include <link.h>
#include <unistd.h>

using jvmilia::JVMData, jvmilia::Context;

value handle_to_value(void* handle) { return caml_copy_int64(std::bit_cast<uint64_t>(handle)); }

template <typename T = void> T* value_to_handle(value val) { return std::bit_cast<T*>(Int64_val(val)); }

int fib(int n) {
  if (n < 2)
    return 1;
  else
    return fib(n - 1) + fib(n - 2);
}

CAMLprim value fib_native(value n) { return Val_int(fib(Int_val(n))); }

CAMLprim value load_library_native(value path) {
  const char* path_str = String_val(path);
  char* current_path = get_current_dir_name();
  printf("We are in: %s\n", current_path);
  free(current_path);
  printf("Loading library: %s\n", path_str);
  void* library = dlopen(path_str, RTLD_LAZY | RTLD_GLOBAL);
  printf("Library: %p\n", library);
  // link_map *map = nullptr;
  // dlinfo(library, RTLD_DI_LINKMAP, &map);

  // Elf64_Sym *symtab = nullptr;
  // char *strtab = nullptr;
  // int symentries = 0;
  // for (auto section = map->l_ld; section->d_tag != DT_NULL; ++section) {
  //   if (section->d_tag == DT_SYMTAB) {
  //     symtab = (Elf64_Sym *)section->d_un.d_ptr;
  //   }
  //   if (section->d_tag == DT_STRTAB) {
  //     strtab = (char *)section->d_un.d_ptr;
  //   }
  //   if (section->d_tag == DT_SYMENT) {
  //     symentries = section->d_un.d_val;
  //   }
  // }
  // int size = strtab - (char *)symtab;
  // for (int k = 0; k < size / symentries; ++k) {
  //   auto sym = &symtab[k];
  //   // If sym is function
  //   if (ELF64_ST_TYPE(symtab[k].st_info) == STT_FUNC) {
  //     // str is name of each symbol
  //     auto str = &strtab[sym->st_name];
  //     printf("%s\n", str);
  //   }
  // }

  // dlclose(library);

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

      /* GetVersion= */ &jvmilia::GetVersion,
      /* DefineClass= */ &jvmilia::DefineClass,
      /* FindClass= */ &jvmilia::FindClass,
      /* FromReflectedMethod= */ &jvmilia::FromReflectedMethod,
      /* FromReflectedField= */ &jvmilia::FromReflectedField,
      /* ToReflectedMethod= */ &jvmilia::ToReflectedMethod,
      /* GetSuperclass= */ &jvmilia::GetSuperclass,
      /* IsAssignableFrom= */ &jvmilia::IsAssignableFrom,
      /* ToReflectedField= */ &jvmilia::ToReflectedField,
      /* Throw= */ &jvmilia::Throw,
      /* ThrowNew= */ &jvmilia::ThrowNew,
      /* ExceptionOccurred= */ &jvmilia::ExceptionOccurred,
      /* ExceptionDescribe= */ &jvmilia::ExceptionDescribe,
      /* ExceptionClear= */ &jvmilia::ExceptionClear,
      /* FatalError= */ &jvmilia::FatalError,
      /* PushLocalFrame= */ &jvmilia::PushLocalFrame,
      /* PopLocalFrame= */ &jvmilia::PopLocalFrame,
      /* NewGlobalRef= */ &jvmilia::NewGlobalRef,
      /* DeleteGlobalRef= */ &jvmilia::DeleteGlobalRef,
      /* DeleteLocalRef= */ &jvmilia::DeleteLocalRef,
      /* IsSameObject= */ &jvmilia::IsSameObject,
      /* NewLocalRef= */ &jvmilia::NewLocalRef,
      /* EnsureLocalCapacity= */ &jvmilia::EnsureLocalCapacity,
      /* AllocObject= */ &jvmilia::AllocObject,
      /* NewObject= */ &jvmilia::NewObject,
      /* NewObjectV= */ &jvmilia::NewObjectV,
      /* NewObjectA= */ &jvmilia::NewObjectA,
      /* GetObjectClass= */ &jvmilia::GetObjectClass,
      /* IsInstanceOf= */ &jvmilia::IsInstanceOf,
      /* GetMethodID= */ &jvmilia::GetMethodID,
      /* CallObjectMethod= */ &jvmilia::CallObjectMethod,
      /* CallObjectMethodV= */ &jvmilia::CallObjectMethodV,
      /* CallObjectMethodA= */ &jvmilia::CallObjectMethodA,
      /* CallBooleanMethod= */ &jvmilia::CallBooleanMethod,
      /* CallBooleanMethodV= */ &jvmilia::CallBooleanMethodV,
      /* CallBooleanMethodA= */ &jvmilia::CallBooleanMethodA,
      /* CallByteMethod= */ &jvmilia::CallByteMethod,
      /* CallByteMethodV= */ &jvmilia::CallByteMethodV,
      /* CallByteMethodA= */ &jvmilia::CallByteMethodA,
      /* CallCharMethod= */ &jvmilia::CallCharMethod,
      /* CallCharMethodV= */ &jvmilia::CallCharMethodV,
      /* CallCharMethodA= */ &jvmilia::CallCharMethodA,
      /* CallShortMethod= */ &jvmilia::CallShortMethod,
      /* CallShortMethodV= */ &jvmilia::CallShortMethodV,
      /* CallShortMethodA= */ &jvmilia::CallShortMethodA,
      /* CallIntMethod= */ &jvmilia::CallIntMethod,
      /* CallIntMethodV= */ &jvmilia::CallIntMethodV,
      /* CallIntMethodA= */ &jvmilia::CallIntMethodA,
      /* CallLongMethod= */ &jvmilia::CallLongMethod,
      /* CallLongMethodV= */ &jvmilia::CallLongMethodV,
      /* CallLongMethodA= */ &jvmilia::CallLongMethodA,
      /* CallFloatMethod= */ &jvmilia::CallFloatMethod,
      /* CallFloatMethodV= */ &jvmilia::CallFloatMethodV,
      /* CallFloatMethodA= */ &jvmilia::CallFloatMethodA,
      /* CallDoubleMethod= */ &jvmilia::CallDoubleMethod,
      /* CallDoubleMethodV= */ &jvmilia::CallDoubleMethodV,
      /* CallDoubleMethodA= */ &jvmilia::CallDoubleMethodA,
      /* CallVoidMethod= */ &jvmilia::CallVoidMethod,
      /* CallVoidMethodV= */ &jvmilia::CallVoidMethodV,
      /* CallVoidMethodA= */ &jvmilia::CallVoidMethodA,
      /* CallNonvirtualObjectMethod= */ &jvmilia::CallNonvirtualObjectMethod,
      /* CallNonvirtualObjectMethodV= */ &jvmilia::CallNonvirtualObjectMethodV,
      /* CallNonvirtualObjectMethodA= */ &jvmilia::CallNonvirtualObjectMethodA,
      /* CallNonvirtualBooleanMethod= */ &jvmilia::CallNonvirtualBooleanMethod,
      /* CallNonvirtualBooleanMethodV= */ &jvmilia::CallNonvirtualBooleanMethodV,
      /* CallNonvirtualBooleanMethodA= */ &jvmilia::CallNonvirtualBooleanMethodA,
      /* CallNonvirtualByteMethod= */ &jvmilia::CallNonvirtualByteMethod,
      /* CallNonvirtualByteMethodV= */ &jvmilia::CallNonvirtualByteMethodV,
      /* CallNonvirtualByteMethodA= */ &jvmilia::CallNonvirtualByteMethodA,
      /* CallNonvirtualCharMethod= */ &jvmilia::CallNonvirtualCharMethod,
      /* CallNonvirtualCharMethodV= */ &jvmilia::CallNonvirtualCharMethodV,
      /* CallNonvirtualCharMethodA= */ &jvmilia::CallNonvirtualCharMethodA,
      /* CallNonvirtualShortMethod= */ &jvmilia::CallNonvirtualShortMethod,
      /* CallNonvirtualShortMethodV= */ &jvmilia::CallNonvirtualShortMethodV,
      /* CallNonvirtualShortMethodA= */ &jvmilia::CallNonvirtualShortMethodA,
      /* CallNonvirtualIntMethod= */ &jvmilia::CallNonvirtualIntMethod,
      /* CallNonvirtualIntMethodV= */ &jvmilia::CallNonvirtualIntMethodV,
      /* CallNonvirtualIntMethodA= */ &jvmilia::CallNonvirtualIntMethodA,
      /* CallNonvirtualLongMethod= */ &jvmilia::CallNonvirtualLongMethod,
      /* CallNonvirtualLongMethodV= */ &jvmilia::CallNonvirtualLongMethodV,
      /* CallNonvirtualLongMethodA= */ &jvmilia::CallNonvirtualLongMethodA,
      /* CallNonvirtualFloatMethod= */ &jvmilia::CallNonvirtualFloatMethod,
      /* CallNonvirtualFloatMethodV= */ &jvmilia::CallNonvirtualFloatMethodV,
      /* CallNonvirtualFloatMethodA= */ &jvmilia::CallNonvirtualFloatMethodA,
      /* CallNonvirtualDoubleMethod= */ &jvmilia::CallNonvirtualDoubleMethod,
      /* CallNonvirtualDoubleMethodV= */ &jvmilia::CallNonvirtualDoubleMethodV,
      /* CallNonvirtualDoubleMethodA= */ &jvmilia::CallNonvirtualDoubleMethodA,
      /* CallNonvirtualVoidMethod= */ &jvmilia::CallNonvirtualVoidMethod,
      /* CallNonvirtualVoidMethodV= */ &jvmilia::CallNonvirtualVoidMethodV,
      /* CallNonvirtualVoidMethodA= */ &jvmilia::CallNonvirtualVoidMethodA,
      /* GetFieldID= */ &jvmilia::GetFieldID,
      /* GetObjectField= */ &jvmilia::GetObjectField,
      /* GetBooleanField= */ &jvmilia::GetBooleanField,
      /* GetByteField= */ &jvmilia::GetByteField,
      /* GetCharField= */ &jvmilia::GetCharField,
      /* GetShortField= */ &jvmilia::GetShortField,
      /* GetIntField= */ &jvmilia::GetIntField,
      /* GetLongField= */ &jvmilia::GetLongField,
      /* GetFloatField= */ &jvmilia::GetFloatField,
      /* GetDoubleField= */ &jvmilia::GetDoubleField,
      /* SetObjectField= */ &jvmilia::SetObjectField,
      /* SetBooleanField= */ &jvmilia::SetBooleanField,
      /* SetByteField= */ &jvmilia::SetByteField,
      /* SetCharField= */ &jvmilia::SetCharField,
      /* SetShortField= */ &jvmilia::SetShortField,
      /* SetIntField= */ &jvmilia::SetIntField,
      /* SetLongField= */ &jvmilia::SetLongField,
      /* SetFloatField= */ &jvmilia::SetFloatField,
      /* SetDoubleField= */ &jvmilia::SetDoubleField,
      /* GetStaticMethodID= */ &jvmilia::GetStaticMethodID,
      /* CallStaticObjectMethod= */ &jvmilia::CallStaticObjectMethod,
      /* CallStaticObjectMethodV= */ &jvmilia::CallStaticObjectMethodV,
      /* CallStaticObjectMethodA= */ &jvmilia::CallStaticObjectMethodA,
      /* CallStaticBooleanMethod= */ &jvmilia::CallStaticBooleanMethod,
      /* CallStaticBooleanMethodV= */ &jvmilia::CallStaticBooleanMethodV,
      /* CallStaticBooleanMethodA= */ &jvmilia::CallStaticBooleanMethodA,
      /* CallStaticByteMethod= */ &jvmilia::CallStaticByteMethod,
      /* CallStaticByteMethodV= */ &jvmilia::CallStaticByteMethodV,
      /* CallStaticByteMethodA= */ &jvmilia::CallStaticByteMethodA,
      /* CallStaticCharMethod= */ &jvmilia::CallStaticCharMethod,
      /* CallStaticCharMethodV= */ &jvmilia::CallStaticCharMethodV,
      /* CallStaticCharMethodA= */ &jvmilia::CallStaticCharMethodA,
      /* CallStaticShortMethod= */ &jvmilia::CallStaticShortMethod,
      /* CallStaticShortMethodV= */ &jvmilia::CallStaticShortMethodV,
      /* CallStaticShortMethodA= */ &jvmilia::CallStaticShortMethodA,
      /* CallStaticIntMethod= */ &jvmilia::CallStaticIntMethod,
      /* CallStaticIntMethodV= */ &jvmilia::CallStaticIntMethodV,
      /* CallStaticIntMethodA= */ &jvmilia::CallStaticIntMethodA,
      /* CallStaticLongMethod= */ &jvmilia::CallStaticLongMethod,
      /* CallStaticLongMethodV= */ &jvmilia::CallStaticLongMethodV,
      /* CallStaticLongMethodA= */ &jvmilia::CallStaticLongMethodA,
      /* CallStaticFloatMethod= */ &jvmilia::CallStaticFloatMethod,
      /* CallStaticFloatMethodV= */ &jvmilia::CallStaticFloatMethodV,
      /* CallStaticFloatMethodA= */ &jvmilia::CallStaticFloatMethodA,
      /* CallStaticDoubleMethod= */ &jvmilia::CallStaticDoubleMethod,
      /* CallStaticDoubleMethodV= */ &jvmilia::CallStaticDoubleMethodV,
      /* CallStaticDoubleMethodA= */ &jvmilia::CallStaticDoubleMethodA,
      /* CallStaticVoidMethod= */ &jvmilia::CallStaticVoidMethod,
      /* CallStaticVoidMethodV= */ &jvmilia::CallStaticVoidMethodV,
      /* CallStaticVoidMethodA= */ &jvmilia::CallStaticVoidMethodA,
      /* GetStaticFieldID= */ &jvmilia::GetStaticFieldID,
      /* GetStaticObjectField= */ &jvmilia::GetStaticObjectField,
      /* GetStaticBooleanField= */ &jvmilia::GetStaticBooleanField,
      /* GetStaticByteField= */ &jvmilia::GetStaticByteField,
      /* GetStaticCharField= */ &jvmilia::GetStaticCharField,
      /* GetStaticShortField= */ &jvmilia::GetStaticShortField,
      /* GetStaticIntField= */ &jvmilia::GetStaticIntField,
      /* GetStaticLongField= */ &jvmilia::GetStaticLongField,
      /* GetStaticFloatField= */ &jvmilia::GetStaticFloatField,
      /* GetStaticDoubleField= */ &jvmilia::GetStaticDoubleField,
      /* SetStaticObjectField= */ &jvmilia::SetStaticObjectField,
      /* SetStaticBooleanField= */ &jvmilia::SetStaticBooleanField,
      /* SetStaticByteField= */ &jvmilia::SetStaticByteField,
      /* SetStaticCharField= */ &jvmilia::SetStaticCharField,
      /* SetStaticShortField= */ &jvmilia::SetStaticShortField,
      /* SetStaticIntField= */ &jvmilia::SetStaticIntField,
      /* SetStaticLongField= */ &jvmilia::SetStaticLongField,
      /* SetStaticFloatField= */ &jvmilia::SetStaticFloatField,
      /* SetStaticDoubleField= */ &jvmilia::SetStaticDoubleField,
      /* NewString= */ &jvmilia::NewString,
      /* GetStringLength= */ &jvmilia::GetStringLength,
      /* GetStringChars= */ &jvmilia::GetStringChars,
      /* ReleaseStringChars= */ &jvmilia::ReleaseStringChars,
      /* NewStringUTF= */ &jvmilia::NewStringUTF,
      /* GetStringUTFLength= */ &jvmilia::GetStringUTFLength,
      /* GetStringUTFChars= */ &jvmilia::GetStringUTFChars,
      /* ReleaseStringUTFChars= */ &jvmilia::ReleaseStringUTFChars,
      /* GetArrayLength= */ &jvmilia::GetArrayLength,
      /* NewObjectArray= */ &jvmilia::NewObjectArray,
      /* GetObjectArrayElement= */ &jvmilia::GetObjectArrayElement,
      /* SetObjectArrayElement= */ &jvmilia::SetObjectArrayElement,
      /* NewBooleanArray= */ &jvmilia::NewBooleanArray,
      /* NewByteArray= */ &jvmilia::NewByteArray,
      /* NewCharArray= */ &jvmilia::NewCharArray,
      /* NewShortArray= */ &jvmilia::NewShortArray,
      /* NewIntArray= */ &jvmilia::NewIntArray,
      /* NewLongArray= */ &jvmilia::NewLongArray,
      /* NewFloatArray= */ &jvmilia::NewFloatArray,
      /* NewDoubleArray= */ &jvmilia::NewDoubleArray,
      /* GetBooleanArrayElements= */ &jvmilia::GetBooleanArrayElements,
      /* GetByteArrayElements= */ &jvmilia::GetByteArrayElements,
      /* GetCharArrayElements= */ &jvmilia::GetCharArrayElements,
      /* GetShortArrayElements= */ &jvmilia::GetShortArrayElements,
      /* GetIntArrayElements= */ &jvmilia::GetIntArrayElements,
      /* GetLongArrayElements= */ &jvmilia::GetLongArrayElements,
      /* GetFloatArrayElements= */ &jvmilia::GetFloatArrayElements,
      /* GetDoubleArrayElements= */ &jvmilia::GetDoubleArrayElements,
      /* ReleaseBooleanArrayElements= */ &jvmilia::ReleaseBooleanArrayElements,
      /* ReleaseByteArrayElements= */ &jvmilia::ReleaseByteArrayElements,
      /* ReleaseCharArrayElements= */ &jvmilia::ReleaseCharArrayElements,
      /* ReleaseShortArrayElements= */ &jvmilia::ReleaseShortArrayElements,
      /* ReleaseIntArrayElements= */ &jvmilia::ReleaseIntArrayElements,
      /* ReleaseLongArrayElements= */ &jvmilia::ReleaseLongArrayElements,
      /* ReleaseFloatArrayElements= */ &jvmilia::ReleaseFloatArrayElements,
      /* ReleaseDoubleArrayElements= */ &jvmilia::ReleaseDoubleArrayElements,
      /* GetBooleanArrayRegion= */ &jvmilia::GetBooleanArrayRegion,
      /* GetByteArrayRegion= */ &jvmilia::GetByteArrayRegion,
      /* GetCharArrayRegion= */ &jvmilia::GetCharArrayRegion,
      /* GetShortArrayRegion= */ &jvmilia::GetShortArrayRegion,
      /* GetIntArrayRegion= */ &jvmilia::GetIntArrayRegion,
      /* GetLongArrayRegion= */ &jvmilia::GetLongArrayRegion,
      /* GetFloatArrayRegion= */ &jvmilia::GetFloatArrayRegion,
      /* GetDoubleArrayRegion= */ &jvmilia::GetDoubleArrayRegion,
      /* SetBooleanArrayRegion= */ &jvmilia::SetBooleanArrayRegion,
      /* SetByteArrayRegion= */ &jvmilia::SetByteArrayRegion,
      /* SetCharArrayRegion= */ &jvmilia::SetCharArrayRegion,
      /* SetShortArrayRegion= */ &jvmilia::SetShortArrayRegion,
      /* SetIntArrayRegion= */ &jvmilia::SetIntArrayRegion,
      /* SetLongArrayRegion= */ &jvmilia::SetLongArrayRegion,
      /* SetFloatArrayRegion= */ &jvmilia::SetFloatArrayRegion,
      /* SetDoubleArrayRegion= */ &jvmilia::SetDoubleArrayRegion,
      /* RegisterNatives= */ &jvmilia::RegisterNatives,
      /* UnregisterNatives= */ &jvmilia::UnregisterNatives,
      /* MonitorEnter= */ &jvmilia::MonitorEnter,
      /* MonitorExit= */ &jvmilia::MonitorExit,
      /* GetJavaVM= */ &jvmilia::GetJavaVM,
      /* GetStringRegion= */ &jvmilia::GetStringRegion,
      /* GetStringUTFRegion= */ &jvmilia::GetStringUTFRegion,
      /* GetPrimitiveArrayCritical= */ &jvmilia::GetPrimitiveArrayCritical,
      /* ReleasePrimitiveArrayCritical= */ &jvmilia::ReleasePrimitiveArrayCritical,
      /* GetStringCritical= */ &jvmilia::GetStringCritical,
      /* ReleaseStringCritical= */ &jvmilia::ReleaseStringCritical,
      /* NewWeakGlobalRef= */ &jvmilia::NewWeakGlobalRef,
      /* DeleteWeakGlobalRef= */ &jvmilia::DeleteWeakGlobalRef,
      /* ExceptionCheck= */ &jvmilia::ExceptionCheck,
      /* NewDirectByteBuffer= */ &jvmilia::NewDirectByteBuffer,
      /* GetDirectBufferAddress= */ &jvmilia::GetDirectBufferAddress,
      /* GetDirectBufferCapacity= */ &jvmilia::GetDirectBufferCapacity,
      /* GetObjectRefType= */ &jvmilia::GetObjectRefType,
      /* GetModule= */ &jvmilia::GetModule,
      /* IsVirtualThread= */ &jvmilia::IsVirtualThread,
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
  auto context = value_to_handle<Context>(interface_int);
  auto cls_string = String_val(cls_value);
  auto cls = std::bit_cast<jclass>(cls_string);
  auto function = value_to_handle<noargs_void>(fn_int);

  function(&context->interface, cls);

  return Val_unit;
}
