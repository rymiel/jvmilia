#include "jni.h"
#include "jvm.h"
#include <bit>
#include <cstdio>
#include <cstdlib>

namespace jvmilia {
JVMData* getData(JNIEnv* env) {
  Context* context = std::bit_cast<Context*>(env);
  //   __builtin_dump_struct(context->data, printf);
  return context->data;
}
[[noreturn]] void unimplemented(const char* methodName) {
  printf("Unimplemented JNI method %s\n", methodName);
  std::exit(1);
}

jint RegisterNatives(JNIEnv* env, jclass clazz, const JNINativeMethod* methods, jint nMethods) {
  auto* data = getData(env);
  const char* className = std::bit_cast<const char*>(clazz);
  for (int i = 0; i < nMethods; i++) {
    auto method = methods[i];
    auto key = registerKey(className, method.name, method.signature);
    data->registeredNatives.insert_or_assign(key, method.fnPtr);
  }

  // for (auto [k, v] : data->registeredNatives) {
  //   printf("%s -> %p\n", k.data(), v);
  // }

  return 0;
}

jboolean ExceptionCheck(JNIEnv* env) {
  // TODO: exceptions
  (void)env;
  puts("jni: ExceptionCheck");

  return 0;
}

jint EnsureLocalCapacity(JNIEnv* env, jint capacity) {
  // TODO: not sure what i'm even meant to check here
  (void)env;
  (void)capacity;
  printf("jni: EnsureLocalCapacity %d\n", capacity);

  return 0;
}

jclass FindClass(JNIEnv* env, const char* name) {
  printf("jni: FindClass %s\n", name);
  unimplemented("FindClass");
}

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-parameter"

jint GetVersion(JNIEnv* env) { unimplemented("GetVersion"); }
jclass DefineClass(JNIEnv* env, const char* name, jobject loader, const jbyte* buf, jsize len) {
  unimplemented("DefineClass");
}
jmethodID FromReflectedMethod(JNIEnv* env, jobject method) { unimplemented("FromReflectedMethod"); }
jfieldID FromReflectedField(JNIEnv* env, jobject field) { unimplemented("FromReflectedField"); }
jobject ToReflectedMethod(JNIEnv* env, jclass cls, jmethodID methodID, jboolean isStatic) {
  unimplemented("ToReflectedMethod");
}
jclass GetSuperclass(JNIEnv* env, jclass sub) { unimplemented("GetSuperclass"); }
jboolean IsAssignableFrom(JNIEnv* env, jclass sub, jclass sup) { unimplemented("IsAssignableFrom"); }
jobject ToReflectedField(JNIEnv* env, jclass cls, jfieldID fieldID, jboolean isStatic) {
  unimplemented("ToReflectedField");
}
jint Throw(JNIEnv* env, jthrowable obj) { unimplemented("Throw"); }
jint ThrowNew(JNIEnv* env, jclass clazz, const char* msg) { unimplemented("ThrowNew"); }
jthrowable ExceptionOccurred(JNIEnv* env) { unimplemented("ExceptionOccurred"); }
void ExceptionDescribe(JNIEnv* env) { unimplemented("ExceptionDescribe"); }
void ExceptionClear(JNIEnv* env) { unimplemented("ExceptionClear"); }
void FatalError(JNIEnv* env, const char* msg) { unimplemented("FatalError"); }
jint PushLocalFrame(JNIEnv* env, jint capacity) { unimplemented("PushLocalFrame"); }
jobject PopLocalFrame(JNIEnv* env, jobject result) { unimplemented("PopLocalFrame"); }
jobject NewGlobalRef(JNIEnv* env, jobject lobj) { unimplemented("NewGlobalRef"); }
void DeleteGlobalRef(JNIEnv* env, jobject gref) { unimplemented("DeleteGlobalRef"); }
void DeleteLocalRef(JNIEnv* env, jobject obj) { unimplemented("DeleteLocalRef"); }
jboolean IsSameObject(JNIEnv* env, jobject obj1, jobject obj2) { unimplemented("IsSameObject"); }
jobject NewLocalRef(JNIEnv* env, jobject ref) { unimplemented("NewLocalRef"); }
jobject AllocObject(JNIEnv* env, jclass clazz) { unimplemented("AllocObject"); }
jobject NewObject(JNIEnv* env, jclass clazz, jmethodID methodID, ...) { unimplemented("NewObject"); }
jobject NewObjectV(JNIEnv* env, jclass clazz, jmethodID methodID, va_list args) { unimplemented("NewObjectV"); }
jobject NewObjectA(JNIEnv* env, jclass clazz, jmethodID methodID, const jvalue* args) { unimplemented("NewObjectA"); }
jclass GetObjectClass(JNIEnv* env, jobject obj) { unimplemented("GetObjectClass"); }
jboolean IsInstanceOf(JNIEnv* env, jobject obj, jclass clazz) { unimplemented("IsInstanceOf"); }
jmethodID GetMethodID(JNIEnv* env, jclass clazz, const char* name, const char* sig) { unimplemented("GetMethodID"); }
jobject CallObjectMethod(JNIEnv* env, jobject obj, jmethodID methodID, ...) { unimplemented("CallObjectMethod"); }
jobject CallObjectMethodV(JNIEnv* env, jobject obj, jmethodID methodID, va_list args) {
  unimplemented("CallObjectMethodV");
}
jobject CallObjectMethodA(JNIEnv* env, jobject obj, jmethodID methodID, const jvalue* args) {
  unimplemented("CallObjectMethodA");
}
jboolean CallBooleanMethod(JNIEnv* env, jobject obj, jmethodID methodID, ...) { unimplemented("CallBooleanMethod"); }
jboolean CallBooleanMethodV(JNIEnv* env, jobject obj, jmethodID methodID, va_list args) {
  unimplemented("CallBooleanMethodV");
}
jboolean CallBooleanMethodA(JNIEnv* env, jobject obj, jmethodID methodID, const jvalue* args) {
  unimplemented("CallBooleanMethodA");
}
jbyte CallByteMethod(JNIEnv* env, jobject obj, jmethodID methodID, ...) { unimplemented("CallByteMethod"); }
jbyte CallByteMethodV(JNIEnv* env, jobject obj, jmethodID methodID, va_list args) { unimplemented("CallByteMethodV"); }
jbyte CallByteMethodA(JNIEnv* env, jobject obj, jmethodID methodID, const jvalue* args) {
  unimplemented("CallByteMethodA");
}
jchar CallCharMethod(JNIEnv* env, jobject obj, jmethodID methodID, ...) { unimplemented("CallCharMethod"); }
jchar CallCharMethodV(JNIEnv* env, jobject obj, jmethodID methodID, va_list args) { unimplemented("CallCharMethodV"); }
jchar CallCharMethodA(JNIEnv* env, jobject obj, jmethodID methodID, const jvalue* args) {
  unimplemented("CallCharMethodA");
}
jshort CallShortMethod(JNIEnv* env, jobject obj, jmethodID methodID, ...) { unimplemented("CallShortMethod"); }
jshort CallShortMethodV(JNIEnv* env, jobject obj, jmethodID methodID, va_list args) {
  unimplemented("CallShortMethodV");
}
jshort CallShortMethodA(JNIEnv* env, jobject obj, jmethodID methodID, const jvalue* args) {
  unimplemented("CallShortMethodA");
}
jint CallIntMethod(JNIEnv* env, jobject obj, jmethodID methodID, ...) { unimplemented("CallIntMethod"); }
jint CallIntMethodV(JNIEnv* env, jobject obj, jmethodID methodID, va_list args) { unimplemented("CallIntMethodV"); }
jint CallIntMethodA(JNIEnv* env, jobject obj, jmethodID methodID, const jvalue* args) {
  unimplemented("CallIntMethodA");
}
jlong CallLongMethod(JNIEnv* env, jobject obj, jmethodID methodID, ...) { unimplemented("CallLongMethod"); }
jlong CallLongMethodV(JNIEnv* env, jobject obj, jmethodID methodID, va_list args) { unimplemented("CallLongMethodV"); }
jlong CallLongMethodA(JNIEnv* env, jobject obj, jmethodID methodID, const jvalue* args) {
  unimplemented("CallLongMethodA");
}
jfloat CallFloatMethod(JNIEnv* env, jobject obj, jmethodID methodID, ...) { unimplemented("CallFloatMethod"); }
jfloat CallFloatMethodV(JNIEnv* env, jobject obj, jmethodID methodID, va_list args) {
  unimplemented("CallFloatMethodV");
}
jfloat CallFloatMethodA(JNIEnv* env, jobject obj, jmethodID methodID, const jvalue* args) {
  unimplemented("CallFloatMethodA");
}
jdouble CallDoubleMethod(JNIEnv* env, jobject obj, jmethodID methodID, ...) { unimplemented("CallDoubleMethod"); }
jdouble CallDoubleMethodV(JNIEnv* env, jobject obj, jmethodID methodID, va_list args) {
  unimplemented("CallDoubleMethodV");
}
jdouble CallDoubleMethodA(JNIEnv* env, jobject obj, jmethodID methodID, const jvalue* args) {
  unimplemented("CallDoubleMethodA");
}
void CallVoidMethod(JNIEnv* env, jobject obj, jmethodID methodID, ...) { unimplemented("CallVoidMethod"); }
void CallVoidMethodV(JNIEnv* env, jobject obj, jmethodID methodID, va_list args) { unimplemented("CallVoidMethodV"); }
void CallVoidMethodA(JNIEnv* env, jobject obj, jmethodID methodID, const jvalue* args) {
  unimplemented("CallVoidMethodA");
}
jobject CallNonvirtualObjectMethod(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, ...) {
  unimplemented("CallNonvirtualObjectMethod");
}
jobject CallNonvirtualObjectMethodV(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, va_list args) {
  unimplemented("CallNonvirtualObjectMethodV");
}
jobject CallNonvirtualObjectMethodA(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, const jvalue* args) {
  unimplemented("CallNonvirtualObjectMethodA");
}
jboolean CallNonvirtualBooleanMethod(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, ...) {
  unimplemented("CallNonvirtualBooleanMethod");
}
jboolean CallNonvirtualBooleanMethodV(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, va_list args) {
  unimplemented("CallNonvirtualBooleanMethodV");
}
jboolean CallNonvirtualBooleanMethodA(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, const jvalue* args) {
  unimplemented("CallNonvirtualBooleanMethodA");
}
jbyte CallNonvirtualByteMethod(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, ...) {
  unimplemented("CallNonvirtualByteMethod");
}
jbyte CallNonvirtualByteMethodV(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, va_list args) {
  unimplemented("CallNonvirtualByteMethodV");
}
jbyte CallNonvirtualByteMethodA(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, const jvalue* args) {
  unimplemented("CallNonvirtualByteMethodA");
}
jchar CallNonvirtualCharMethod(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, ...) {
  unimplemented("CallNonvirtualCharMethod");
}
jchar CallNonvirtualCharMethodV(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, va_list args) {
  unimplemented("CallNonvirtualCharMethodV");
}
jchar CallNonvirtualCharMethodA(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, const jvalue* args) {
  unimplemented("CallNonvirtualCharMethodA");
}
jshort CallNonvirtualShortMethod(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, ...) {
  unimplemented("CallNonvirtualShortMethod");
}
jshort CallNonvirtualShortMethodV(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, va_list args) {
  unimplemented("CallNonvirtualShortMethodV");
}
jshort CallNonvirtualShortMethodA(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, const jvalue* args) {
  unimplemented("CallNonvirtualShortMethodA");
}
jint CallNonvirtualIntMethod(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, ...) {
  unimplemented("CallNonvirtualIntMethod");
}
jint CallNonvirtualIntMethodV(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, va_list args) {
  unimplemented("CallNonvirtualIntMethodV");
}
jint CallNonvirtualIntMethodA(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, const jvalue* args) {
  unimplemented("CallNonvirtualIntMethodA");
}
jlong CallNonvirtualLongMethod(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, ...) {
  unimplemented("CallNonvirtualLongMethod");
}
jlong CallNonvirtualLongMethodV(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, va_list args) {
  unimplemented("CallNonvirtualLongMethodV");
}
jlong CallNonvirtualLongMethodA(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, const jvalue* args) {
  unimplemented("CallNonvirtualLongMethodA");
}
jfloat CallNonvirtualFloatMethod(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, ...) {
  unimplemented("CallNonvirtualFloatMethod");
}
jfloat CallNonvirtualFloatMethodV(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, va_list args) {
  unimplemented("CallNonvirtualFloatMethodV");
}
jfloat CallNonvirtualFloatMethodA(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, const jvalue* args) {
  unimplemented("CallNonvirtualFloatMethodA");
}
jdouble CallNonvirtualDoubleMethod(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, ...) {
  unimplemented("CallNonvirtualDoubleMethod");
}
jdouble CallNonvirtualDoubleMethodV(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, va_list args) {
  unimplemented("CallNonvirtualDoubleMethodV");
}
jdouble CallNonvirtualDoubleMethodA(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, const jvalue* args) {
  unimplemented("CallNonvirtualDoubleMethodA");
}
void CallNonvirtualVoidMethod(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, ...) {
  unimplemented("CallNonvirtualVoidMethod");
}
void CallNonvirtualVoidMethodV(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, va_list args) {
  unimplemented("CallNonvirtualVoidMethodV");
}
void CallNonvirtualVoidMethodA(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, const jvalue* args) {
  unimplemented("CallNonvirtualVoidMethodA");
}
jfieldID GetFieldID(JNIEnv* env, jclass clazz, const char* name, const char* sig) { unimplemented("GetFieldID"); }
jobject GetObjectField(JNIEnv* env, jobject obj, jfieldID fieldID) { unimplemented("GetObjectField"); }
jboolean GetBooleanField(JNIEnv* env, jobject obj, jfieldID fieldID) { unimplemented("GetBooleanField"); }
jbyte GetByteField(JNIEnv* env, jobject obj, jfieldID fieldID) { unimplemented("GetByteField"); }
jchar GetCharField(JNIEnv* env, jobject obj, jfieldID fieldID) { unimplemented("GetCharField"); }
jshort GetShortField(JNIEnv* env, jobject obj, jfieldID fieldID) { unimplemented("GetShortField"); }
jint GetIntField(JNIEnv* env, jobject obj, jfieldID fieldID) { unimplemented("GetIntField"); }
jlong GetLongField(JNIEnv* env, jobject obj, jfieldID fieldID) { unimplemented("GetLongField"); }
jfloat GetFloatField(JNIEnv* env, jobject obj, jfieldID fieldID) { unimplemented("GetFloatField"); }
jdouble GetDoubleField(JNIEnv* env, jobject obj, jfieldID fieldID) { unimplemented("GetDoubleField"); }
void SetObjectField(JNIEnv* env, jobject obj, jfieldID fieldID, jobject val) { unimplemented("SetObjectField"); }
void SetBooleanField(JNIEnv* env, jobject obj, jfieldID fieldID, jboolean val) { unimplemented("SetBooleanField"); }
void SetByteField(JNIEnv* env, jobject obj, jfieldID fieldID, jbyte val) { unimplemented("SetByteField"); }
void SetCharField(JNIEnv* env, jobject obj, jfieldID fieldID, jchar val) { unimplemented("SetCharField"); }
void SetShortField(JNIEnv* env, jobject obj, jfieldID fieldID, jshort val) { unimplemented("SetShortField"); }
void SetIntField(JNIEnv* env, jobject obj, jfieldID fieldID, jint val) { unimplemented("SetIntField"); }
void SetLongField(JNIEnv* env, jobject obj, jfieldID fieldID, jlong val) { unimplemented("SetLongField"); }
void SetFloatField(JNIEnv* env, jobject obj, jfieldID fieldID, jfloat val) { unimplemented("SetFloatField"); }
void SetDoubleField(JNIEnv* env, jobject obj, jfieldID fieldID, jdouble val) { unimplemented("SetDoubleField"); }
jmethodID GetStaticMethodID(JNIEnv* env, jclass clazz, const char* name, const char* sig) {
  unimplemented("GetStaticMethodID");
}
jobject CallStaticObjectMethod(JNIEnv* env, jclass clazz, jmethodID methodID, ...) {
  unimplemented("CallStaticObjectMethod");
}
jobject CallStaticObjectMethodV(JNIEnv* env, jclass clazz, jmethodID methodID, va_list args) {
  unimplemented("CallStaticObjectMethodV");
}
jobject CallStaticObjectMethodA(JNIEnv* env, jclass clazz, jmethodID methodID, const jvalue* args) {
  unimplemented("CallStaticObjectMethodA");
}
jboolean CallStaticBooleanMethod(JNIEnv* env, jclass clazz, jmethodID methodID, ...) {
  unimplemented("CallStaticBooleanMethod");
}
jboolean CallStaticBooleanMethodV(JNIEnv* env, jclass clazz, jmethodID methodID, va_list args) {
  unimplemented("CallStaticBooleanMethodV");
}
jboolean CallStaticBooleanMethodA(JNIEnv* env, jclass clazz, jmethodID methodID, const jvalue* args) {
  unimplemented("CallStaticBooleanMethodA");
}
jbyte CallStaticByteMethod(JNIEnv* env, jclass clazz, jmethodID methodID, ...) {
  unimplemented("CallStaticByteMethod");
}
jbyte CallStaticByteMethodV(JNIEnv* env, jclass clazz, jmethodID methodID, va_list args) {
  unimplemented("CallStaticByteMethodV");
}
jbyte CallStaticByteMethodA(JNIEnv* env, jclass clazz, jmethodID methodID, const jvalue* args) {
  unimplemented("CallStaticByteMethodA");
}
jchar CallStaticCharMethod(JNIEnv* env, jclass clazz, jmethodID methodID, ...) {
  unimplemented("CallStaticCharMethod");
}
jchar CallStaticCharMethodV(JNIEnv* env, jclass clazz, jmethodID methodID, va_list args) {
  unimplemented("CallStaticCharMethodV");
}
jchar CallStaticCharMethodA(JNIEnv* env, jclass clazz, jmethodID methodID, const jvalue* args) {
  unimplemented("CallStaticCharMethodA");
}
jshort CallStaticShortMethod(JNIEnv* env, jclass clazz, jmethodID methodID, ...) {
  unimplemented("CallStaticShortMethod");
}
jshort CallStaticShortMethodV(JNIEnv* env, jclass clazz, jmethodID methodID, va_list args) {
  unimplemented("CallStaticShortMethodV");
}
jshort CallStaticShortMethodA(JNIEnv* env, jclass clazz, jmethodID methodID, const jvalue* args) {
  unimplemented("CallStaticShortMethodA");
}
jint CallStaticIntMethod(JNIEnv* env, jclass clazz, jmethodID methodID, ...) { unimplemented("CallStaticIntMethod"); }
jint CallStaticIntMethodV(JNIEnv* env, jclass clazz, jmethodID methodID, va_list args) {
  unimplemented("CallStaticIntMethodV");
}
jint CallStaticIntMethodA(JNIEnv* env, jclass clazz, jmethodID methodID, const jvalue* args) {
  unimplemented("CallStaticIntMethodA");
}
jlong CallStaticLongMethod(JNIEnv* env, jclass clazz, jmethodID methodID, ...) {
  unimplemented("CallStaticLongMethod");
}
jlong CallStaticLongMethodV(JNIEnv* env, jclass clazz, jmethodID methodID, va_list args) {
  unimplemented("CallStaticLongMethodV");
}
jlong CallStaticLongMethodA(JNIEnv* env, jclass clazz, jmethodID methodID, const jvalue* args) {
  unimplemented("CallStaticLongMethodA");
}
jfloat CallStaticFloatMethod(JNIEnv* env, jclass clazz, jmethodID methodID, ...) {
  unimplemented("CallStaticFloatMethod");
}
jfloat CallStaticFloatMethodV(JNIEnv* env, jclass clazz, jmethodID methodID, va_list args) {
  unimplemented("CallStaticFloatMethodV");
}
jfloat CallStaticFloatMethodA(JNIEnv* env, jclass clazz, jmethodID methodID, const jvalue* args) {
  unimplemented("CallStaticFloatMethodA");
}
jdouble CallStaticDoubleMethod(JNIEnv* env, jclass clazz, jmethodID methodID, ...) {
  unimplemented("CallStaticDoubleMethod");
}
jdouble CallStaticDoubleMethodV(JNIEnv* env, jclass clazz, jmethodID methodID, va_list args) {
  unimplemented("CallStaticDoubleMethodV");
}
jdouble CallStaticDoubleMethodA(JNIEnv* env, jclass clazz, jmethodID methodID, const jvalue* args) {
  unimplemented("CallStaticDoubleMethodA");
}
void CallStaticVoidMethod(JNIEnv* env, jclass cls, jmethodID methodID, ...) { unimplemented("CallStaticVoidMethod"); }
void CallStaticVoidMethodV(JNIEnv* env, jclass cls, jmethodID methodID, va_list args) {
  unimplemented("CallStaticVoidMethodV");
}
void CallStaticVoidMethodA(JNIEnv* env, jclass cls, jmethodID methodID, const jvalue* args) {
  unimplemented("CallStaticVoidMethodA");
}
jfieldID GetStaticFieldID(JNIEnv* env, jclass clazz, const char* name, const char* sig) {
  unimplemented("GetStaticFieldID");
}
jobject GetStaticObjectField(JNIEnv* env, jclass clazz, jfieldID fieldID) { unimplemented("GetStaticObjectField"); }
jboolean GetStaticBooleanField(JNIEnv* env, jclass clazz, jfieldID fieldID) { unimplemented("GetStaticBooleanField"); }
jbyte GetStaticByteField(JNIEnv* env, jclass clazz, jfieldID fieldID) { unimplemented("GetStaticByteField"); }
jchar GetStaticCharField(JNIEnv* env, jclass clazz, jfieldID fieldID) { unimplemented("GetStaticCharField"); }
jshort GetStaticShortField(JNIEnv* env, jclass clazz, jfieldID fieldID) { unimplemented("GetStaticShortField"); }
jint GetStaticIntField(JNIEnv* env, jclass clazz, jfieldID fieldID) { unimplemented("GetStaticIntField"); }
jlong GetStaticLongField(JNIEnv* env, jclass clazz, jfieldID fieldID) { unimplemented("GetStaticLongField"); }
jfloat GetStaticFloatField(JNIEnv* env, jclass clazz, jfieldID fieldID) { unimplemented("GetStaticFloatField"); }
jdouble GetStaticDoubleField(JNIEnv* env, jclass clazz, jfieldID fieldID) { unimplemented("GetStaticDoubleField"); }
void SetStaticObjectField(JNIEnv* env, jclass clazz, jfieldID fieldID, jobject value) {
  unimplemented("SetStaticObjectField");
}
void SetStaticBooleanField(JNIEnv* env, jclass clazz, jfieldID fieldID, jboolean value) {
  unimplemented("SetStaticBooleanField");
}
void SetStaticByteField(JNIEnv* env, jclass clazz, jfieldID fieldID, jbyte value) {
  unimplemented("SetStaticByteField");
}
void SetStaticCharField(JNIEnv* env, jclass clazz, jfieldID fieldID, jchar value) {
  unimplemented("SetStaticCharField");
}
void SetStaticShortField(JNIEnv* env, jclass clazz, jfieldID fieldID, jshort value) {
  unimplemented("SetStaticShortField");
}
void SetStaticIntField(JNIEnv* env, jclass clazz, jfieldID fieldID, jint value) { unimplemented("SetStaticIntField"); }
void SetStaticLongField(JNIEnv* env, jclass clazz, jfieldID fieldID, jlong value) {
  unimplemented("SetStaticLongField");
}
void SetStaticFloatField(JNIEnv* env, jclass clazz, jfieldID fieldID, jfloat value) {
  unimplemented("SetStaticFloatField");
}
void SetStaticDoubleField(JNIEnv* env, jclass clazz, jfieldID fieldID, jdouble value) {
  unimplemented("SetStaticDoubleField");
}
jstring NewString(JNIEnv* env, const jchar* unicode, jsize len) { unimplemented("NewString"); }
jsize GetStringLength(JNIEnv* env, jstring str) { unimplemented("GetStringLength"); }
const jchar* GetStringChars(JNIEnv* env, jstring str, jboolean* isCopy) { unimplemented("GetStringChars"); }
void ReleaseStringChars(JNIEnv* env, jstring str, const jchar* chars) { unimplemented("ReleaseStringChars"); }
jstring NewStringUTF(JNIEnv* env, const char* utf) { unimplemented("NewStringUTF"); }
jsize GetStringUTFLength(JNIEnv* env, jstring str) { unimplemented("GetStringUTFLength"); }
const char* GetStringUTFChars(JNIEnv* env, jstring str, jboolean* isCopy) { unimplemented("GetStringUTFChars"); }
void ReleaseStringUTFChars(JNIEnv* env, jstring str, const char* chars) { unimplemented("ReleaseStringUTFChars"); }
jsize GetArrayLength(JNIEnv* env, jarray array) { unimplemented("GetArrayLength"); }
jobjectArray NewObjectArray(JNIEnv* env, jsize len, jclass clazz, jobject init) { unimplemented("NewObjectArray"); }
jobject GetObjectArrayElement(JNIEnv* env, jobjectArray array, jsize index) { unimplemented("GetObjectArrayElement"); }
void SetObjectArrayElement(JNIEnv* env, jobjectArray array, jsize index, jobject val) {
  unimplemented("SetObjectArrayElement");
}
jbooleanArray NewBooleanArray(JNIEnv* env, jsize len) { unimplemented("NewBooleanArray"); }
jbyteArray NewByteArray(JNIEnv* env, jsize len) { unimplemented("NewByteArray"); }
jcharArray NewCharArray(JNIEnv* env, jsize len) { unimplemented("NewCharArray"); }
jshortArray NewShortArray(JNIEnv* env, jsize len) { unimplemented("NewShortArray"); }
jintArray NewIntArray(JNIEnv* env, jsize len) { unimplemented("NewIntArray"); }
jlongArray NewLongArray(JNIEnv* env, jsize len) { unimplemented("NewLongArray"); }
jfloatArray NewFloatArray(JNIEnv* env, jsize len) { unimplemented("NewFloatArray"); }
jdoubleArray NewDoubleArray(JNIEnv* env, jsize len) { unimplemented("NewDoubleArray"); }
jboolean* GetBooleanArrayElements(JNIEnv* env, jbooleanArray array, jboolean* isCopy) {
  unimplemented("GetBooleanArrayElements");
}
jbyte* GetByteArrayElements(JNIEnv* env, jbyteArray array, jboolean* isCopy) { unimplemented("GetByteArrayElements"); }
jchar* GetCharArrayElements(JNIEnv* env, jcharArray array, jboolean* isCopy) { unimplemented("GetCharArrayElements"); }
jshort* GetShortArrayElements(JNIEnv* env, jshortArray array, jboolean* isCopy) {
  unimplemented("GetShortArrayElements");
}
jint* GetIntArrayElements(JNIEnv* env, jintArray array, jboolean* isCopy) { unimplemented("GetIntArrayElements"); }
jlong* GetLongArrayElements(JNIEnv* env, jlongArray array, jboolean* isCopy) { unimplemented("GetLongArrayElements"); }
jfloat* GetFloatArrayElements(JNIEnv* env, jfloatArray array, jboolean* isCopy) {
  unimplemented("GetFloatArrayElements");
}
jdouble* GetDoubleArrayElements(JNIEnv* env, jdoubleArray array, jboolean* isCopy) {
  unimplemented("GetDoubleArrayElements");
}
void ReleaseBooleanArrayElements(JNIEnv* env, jbooleanArray array, jboolean* elems, jint mode) {
  unimplemented("ReleaseBooleanArrayElements");
}
void ReleaseByteArrayElements(JNIEnv* env, jbyteArray array, jbyte* elems, jint mode) {
  unimplemented("ReleaseByteArrayElements");
}
void ReleaseCharArrayElements(JNIEnv* env, jcharArray array, jchar* elems, jint mode) {
  unimplemented("ReleaseCharArrayElements");
}
void ReleaseShortArrayElements(JNIEnv* env, jshortArray array, jshort* elems, jint mode) {
  unimplemented("ReleaseShortArrayElements");
}
void ReleaseIntArrayElements(JNIEnv* env, jintArray array, jint* elems, jint mode) {
  unimplemented("ReleaseIntArrayElements");
}
void ReleaseLongArrayElements(JNIEnv* env, jlongArray array, jlong* elems, jint mode) {
  unimplemented("ReleaseLongArrayElements");
}
void ReleaseFloatArrayElements(JNIEnv* env, jfloatArray array, jfloat* elems, jint mode) {
  unimplemented("ReleaseFloatArrayElements");
}
void ReleaseDoubleArrayElements(JNIEnv* env, jdoubleArray array, jdouble* elems, jint mode) {
  unimplemented("ReleaseDoubleArrayElements");
}
void GetBooleanArrayRegion(JNIEnv* env, jbooleanArray array, jsize start, jsize l, jboolean* buf) {
  unimplemented("GetBooleanArrayRegion");
}
void GetByteArrayRegion(JNIEnv* env, jbyteArray array, jsize start, jsize len, jbyte* buf) {
  unimplemented("GetByteArrayRegion");
}
void GetCharArrayRegion(JNIEnv* env, jcharArray array, jsize start, jsize len, jchar* buf) {
  unimplemented("GetCharArrayRegion");
}
void GetShortArrayRegion(JNIEnv* env, jshortArray array, jsize start, jsize len, jshort* buf) {
  unimplemented("GetShortArrayRegion");
}
void GetIntArrayRegion(JNIEnv* env, jintArray array, jsize start, jsize len, jint* buf) {
  unimplemented("GetIntArrayRegion");
}
void GetLongArrayRegion(JNIEnv* env, jlongArray array, jsize start, jsize len, jlong* buf) {
  unimplemented("GetLongArrayRegion");
}
void GetFloatArrayRegion(JNIEnv* env, jfloatArray array, jsize start, jsize len, jfloat* buf) {
  unimplemented("GetFloatArrayRegion");
}
void GetDoubleArrayRegion(JNIEnv* env, jdoubleArray array, jsize start, jsize len, jdouble* buf) {
  unimplemented("GetDoubleArrayRegion");
}
void SetBooleanArrayRegion(JNIEnv* env, jbooleanArray array, jsize start, jsize l, const jboolean* buf) {
  unimplemented("SetBooleanArrayRegion");
}
void SetByteArrayRegion(JNIEnv* env, jbyteArray array, jsize start, jsize len, const jbyte* buf) {
  unimplemented("SetByteArrayRegion");
}
void SetCharArrayRegion(JNIEnv* env, jcharArray array, jsize start, jsize len, const jchar* buf) {
  unimplemented("SetCharArrayRegion");
}
void SetShortArrayRegion(JNIEnv* env, jshortArray array, jsize start, jsize len, const jshort* buf) {
  unimplemented("SetShortArrayRegion");
}
void SetIntArrayRegion(JNIEnv* env, jintArray array, jsize start, jsize len, const jint* buf) {
  unimplemented("SetIntArrayRegion");
}
void SetLongArrayRegion(JNIEnv* env, jlongArray array, jsize start, jsize len, const jlong* buf) {
  unimplemented("SetLongArrayRegion");
}
void SetFloatArrayRegion(JNIEnv* env, jfloatArray array, jsize start, jsize len, const jfloat* buf) {
  unimplemented("SetFloatArrayRegion");
}
void SetDoubleArrayRegion(JNIEnv* env, jdoubleArray array, jsize start, jsize len, const jdouble* buf) {
  unimplemented("SetDoubleArrayRegion");
}

jint UnregisterNatives(JNIEnv* env, jclass clazz) { unimplemented("UnregisterNatives"); }
jint MonitorEnter(JNIEnv* env, jobject obj) { unimplemented("MonitorEnter"); }
jint MonitorExit(JNIEnv* env, jobject obj) { unimplemented("MonitorExit"); }
jint GetJavaVM(JNIEnv* env, JavaVM** vm) { unimplemented("GetJavaVM"); }
void GetStringRegion(JNIEnv* env, jstring str, jsize start, jsize len, jchar* buf) { unimplemented("GetStringRegion"); }
void GetStringUTFRegion(JNIEnv* env, jstring str, jsize start, jsize len, char* buf) {
  unimplemented("GetStringUTFRegion");
}
void* GetPrimitiveArrayCritical(JNIEnv* env, jarray array, jboolean* isCopy) {
  unimplemented("GetPrimitiveArrayCritical");
}
void ReleasePrimitiveArrayCritical(JNIEnv* env, jarray array, void* carray, jint mode) {
  unimplemented("ReleasePrimitiveArrayCritical");
}
const jchar* GetStringCritical(JNIEnv* env, jstring string, jboolean* isCopy) { unimplemented("GetStringCritical"); }
void ReleaseStringCritical(JNIEnv* env, jstring string, const jchar* cstring) {
  unimplemented("ReleaseStringCritical");
}
jweak NewWeakGlobalRef(JNIEnv* env, jobject obj) { unimplemented("NewWeakGlobalRef"); }
void DeleteWeakGlobalRef(JNIEnv* env, jweak ref) { unimplemented("DeleteWeakGlobalRef"); }
jobject NewDirectByteBuffer(JNIEnv* env, void* address, jlong capacity) { unimplemented("NewDirectByteBuffer"); }
void* GetDirectBufferAddress(JNIEnv* env, jobject buf) { unimplemented("GetDirectBufferAddress"); }
jlong GetDirectBufferCapacity(JNIEnv* env, jobject buf) { unimplemented("GetDirectBufferCapacity"); }
jobjectRefType GetObjectRefType(JNIEnv* env, jobject obj) { unimplemented("GetObjectRefType"); }
jobject GetModule(JNIEnv* env, jclass clazz) { unimplemented("GetModule"); }
jboolean IsVirtualThread(JNIEnv* env, jobject obj) { unimplemented("IsVirtualThread"); }

#pragma clang diagnostic pop

} // namespace jvmilia