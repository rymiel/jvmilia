#include "jni.h"
#include <bit>
#include <cstdio>
#include <cstdlib>

namespace jvmilia {
JVMData *getData(JNIEnv *env) {
  Context *context = std::bit_cast<Context *>(env);
//   __builtin_dump_struct(context->data, printf);
  return context->data;
}
jint GetVersion(JNIEnv *env) {
  puts("Unimplemented JNI method GetVersion");
  std::exit(1);
}
jclass DefineClass(JNIEnv *env, const char *name, jobject loader,
                   const jbyte *buf, jsize len) {
  puts("Unimplemented JNI method DefineClass");
  std::exit(1);
}
jclass FindClass(JNIEnv *env, const char *name) {
  puts("Unimplemented JNI method FindClass");
  std::exit(1);
}
jmethodID FromReflectedMethod(JNIEnv *env, jobject method) {
  puts("Unimplemented JNI method FromReflectedMethod");
  std::exit(1);
}
jfieldID FromReflectedField(JNIEnv *env, jobject field) {
  puts("Unimplemented JNI method FromReflectedField");
  std::exit(1);
}
jobject ToReflectedMethod(JNIEnv *env, jclass cls, jmethodID methodID,
                          jboolean isStatic) {
  puts("Unimplemented JNI method ToReflectedMethod");
  std::exit(1);
}
jclass GetSuperclass(JNIEnv *env, jclass sub) {
  puts("Unimplemented JNI method GetSuperclass");
  std::exit(1);
}
jboolean IsAssignableFrom(JNIEnv *env, jclass sub, jclass sup) {
  puts("Unimplemented JNI method IsAssignableFrom");
  std::exit(1);
}
jobject ToReflectedField(JNIEnv *env, jclass cls, jfieldID fieldID,
                         jboolean isStatic) {
  puts("Unimplemented JNI method ToReflectedField");
  std::exit(1);
}
jint Throw(JNIEnv *env, jthrowable obj) {
  puts("Unimplemented JNI method Throw");
  std::exit(1);
}
jint ThrowNew(JNIEnv *env, jclass clazz, const char *msg) {
  puts("Unimplemented JNI method ThrowNew");
  std::exit(1);
}
jthrowable ExceptionOccurred(JNIEnv *env) {
  puts("Unimplemented JNI method ExceptionOccurred");
  std::exit(1);
}
void ExceptionDescribe(JNIEnv *env) {
  puts("Unimplemented JNI method ExceptionDescribe");
  std::exit(1);
}
void ExceptionClear(JNIEnv *env) {
  puts("Unimplemented JNI method ExceptionClear");
  std::exit(1);
}
void FatalError(JNIEnv *env, const char *msg) {
  puts("Unimplemented JNI method FatalError");
  std::exit(1);
}
jint PushLocalFrame(JNIEnv *env, jint capacity) {
  puts("Unimplemented JNI method PushLocalFrame");
  std::exit(1);
}
jobject PopLocalFrame(JNIEnv *env, jobject result) {
  puts("Unimplemented JNI method PopLocalFrame");
  std::exit(1);
}
jobject NewGlobalRef(JNIEnv *env, jobject lobj) {
  puts("Unimplemented JNI method NewGlobalRef");
  std::exit(1);
}
void DeleteGlobalRef(JNIEnv *env, jobject gref) {
  puts("Unimplemented JNI method DeleteGlobalRef");
  std::exit(1);
}
void DeleteLocalRef(JNIEnv *env, jobject obj) {
  puts("Unimplemented JNI method DeleteLocalRef");
  std::exit(1);
}
jboolean IsSameObject(JNIEnv *env, jobject obj1, jobject obj2) {
  puts("Unimplemented JNI method IsSameObject");
  std::exit(1);
}
jobject NewLocalRef(JNIEnv *env, jobject ref) {
  puts("Unimplemented JNI method NewLocalRef");
  std::exit(1);
}
jint EnsureLocalCapacity(JNIEnv *env, jint capacity) {
  puts("Unimplemented JNI method EnsureLocalCapacity");
  std::exit(1);
}
jobject AllocObject(JNIEnv *env, jclass clazz) {
  puts("Unimplemented JNI method AllocObject");
  std::exit(1);
}
jobject NewObject(JNIEnv *env, jclass clazz, jmethodID methodID, ...) {
  puts("Unimplemented JNI method NewObject");
  std::exit(1);
}
jobject NewObjectV(JNIEnv *env, jclass clazz, jmethodID methodID,
                   va_list args) {
  puts("Unimplemented JNI method NewObjectV");
  std::exit(1);
}
jobject NewObjectA(JNIEnv *env, jclass clazz, jmethodID methodID,
                   const jvalue *args) {
  puts("Unimplemented JNI method NewObjectA");
  std::exit(1);
}
jclass GetObjectClass(JNIEnv *env, jobject obj) {
  puts("Unimplemented JNI method GetObjectClass");
  std::exit(1);
}
jboolean IsInstanceOf(JNIEnv *env, jobject obj, jclass clazz) {
  puts("Unimplemented JNI method IsInstanceOf");
  std::exit(1);
}
jmethodID GetMethodID(JNIEnv *env, jclass clazz, const char *name,
                      const char *sig) {
  puts("Unimplemented JNI method GetMethodID");
  std::exit(1);
}
jobject CallObjectMethod(JNIEnv *env, jobject obj, jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallObjectMethod");
  std::exit(1);
}
jobject CallObjectMethodV(JNIEnv *env, jobject obj, jmethodID methodID,
                          va_list args) {
  puts("Unimplemented JNI method CallObjectMethodV");
  std::exit(1);
}
jobject CallObjectMethodA(JNIEnv *env, jobject obj, jmethodID methodID,
                          const jvalue *args) {
  puts("Unimplemented JNI method CallObjectMethodA");
  std::exit(1);
}
jboolean CallBooleanMethod(JNIEnv *env, jobject obj, jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallBooleanMethod");
  std::exit(1);
}
jboolean CallBooleanMethodV(JNIEnv *env, jobject obj, jmethodID methodID,
                            va_list args) {
  puts("Unimplemented JNI method CallBooleanMethodV");
  std::exit(1);
}
jboolean CallBooleanMethodA(JNIEnv *env, jobject obj, jmethodID methodID,
                            const jvalue *args) {
  puts("Unimplemented JNI method CallBooleanMethodA");
  std::exit(1);
}
jbyte CallByteMethod(JNIEnv *env, jobject obj, jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallByteMethod");
  std::exit(1);
}
jbyte CallByteMethodV(JNIEnv *env, jobject obj, jmethodID methodID,
                      va_list args) {
  puts("Unimplemented JNI method CallByteMethodV");
  std::exit(1);
}
jbyte CallByteMethodA(JNIEnv *env, jobject obj, jmethodID methodID,
                      const jvalue *args) {
  puts("Unimplemented JNI method CallByteMethodA");
  std::exit(1);
}
jchar CallCharMethod(JNIEnv *env, jobject obj, jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallCharMethod");
  std::exit(1);
}
jchar CallCharMethodV(JNIEnv *env, jobject obj, jmethodID methodID,
                      va_list args) {
  puts("Unimplemented JNI method CallCharMethodV");
  std::exit(1);
}
jchar CallCharMethodA(JNIEnv *env, jobject obj, jmethodID methodID,
                      const jvalue *args) {
  puts("Unimplemented JNI method CallCharMethodA");
  std::exit(1);
}
jshort CallShortMethod(JNIEnv *env, jobject obj, jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallShortMethod");
  std::exit(1);
}
jshort CallShortMethodV(JNIEnv *env, jobject obj, jmethodID methodID,
                        va_list args) {
  puts("Unimplemented JNI method CallShortMethodV");
  std::exit(1);
}
jshort CallShortMethodA(JNIEnv *env, jobject obj, jmethodID methodID,
                        const jvalue *args) {
  puts("Unimplemented JNI method CallShortMethodA");
  std::exit(1);
}
jint CallIntMethod(JNIEnv *env, jobject obj, jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallIntMethod");
  std::exit(1);
}
jint CallIntMethodV(JNIEnv *env, jobject obj, jmethodID methodID,
                    va_list args) {
  puts("Unimplemented JNI method CallIntMethodV");
  std::exit(1);
}
jint CallIntMethodA(JNIEnv *env, jobject obj, jmethodID methodID,
                    const jvalue *args) {
  puts("Unimplemented JNI method CallIntMethodA");
  std::exit(1);
}
jlong CallLongMethod(JNIEnv *env, jobject obj, jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallLongMethod");
  std::exit(1);
}
jlong CallLongMethodV(JNIEnv *env, jobject obj, jmethodID methodID,
                      va_list args) {
  puts("Unimplemented JNI method CallLongMethodV");
  std::exit(1);
}
jlong CallLongMethodA(JNIEnv *env, jobject obj, jmethodID methodID,
                      const jvalue *args) {
  puts("Unimplemented JNI method CallLongMethodA");
  std::exit(1);
}
jfloat CallFloatMethod(JNIEnv *env, jobject obj, jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallFloatMethod");
  std::exit(1);
}
jfloat CallFloatMethodV(JNIEnv *env, jobject obj, jmethodID methodID,
                        va_list args) {
  puts("Unimplemented JNI method CallFloatMethodV");
  std::exit(1);
}
jfloat CallFloatMethodA(JNIEnv *env, jobject obj, jmethodID methodID,
                        const jvalue *args) {
  puts("Unimplemented JNI method CallFloatMethodA");
  std::exit(1);
}
jdouble CallDoubleMethod(JNIEnv *env, jobject obj, jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallDoubleMethod");
  std::exit(1);
}
jdouble CallDoubleMethodV(JNIEnv *env, jobject obj, jmethodID methodID,
                          va_list args) {
  puts("Unimplemented JNI method CallDoubleMethodV");
  std::exit(1);
}
jdouble CallDoubleMethodA(JNIEnv *env, jobject obj, jmethodID methodID,
                          const jvalue *args) {
  puts("Unimplemented JNI method CallDoubleMethodA");
  std::exit(1);
}
void CallVoidMethod(JNIEnv *env, jobject obj, jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallVoidMethod");
  std::exit(1);
}
void CallVoidMethodV(JNIEnv *env, jobject obj, jmethodID methodID,
                     va_list args) {
  puts("Unimplemented JNI method CallVoidMethodV");
  std::exit(1);
}
void CallVoidMethodA(JNIEnv *env, jobject obj, jmethodID methodID,
                     const jvalue *args) {
  puts("Unimplemented JNI method CallVoidMethodA");
  std::exit(1);
}
jobject CallNonvirtualObjectMethod(JNIEnv *env, jobject obj, jclass clazz,
                                   jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallNonvirtualObjectMethod");
  std::exit(1);
}
jobject CallNonvirtualObjectMethodV(JNIEnv *env, jobject obj, jclass clazz,
                                    jmethodID methodID, va_list args) {
  puts("Unimplemented JNI method CallNonvirtualObjectMethodV");
  std::exit(1);
}
jobject CallNonvirtualObjectMethodA(JNIEnv *env, jobject obj, jclass clazz,
                                    jmethodID methodID, const jvalue *args) {
  puts("Unimplemented JNI method CallNonvirtualObjectMethodA");
  std::exit(1);
}
jboolean CallNonvirtualBooleanMethod(JNIEnv *env, jobject obj, jclass clazz,
                                     jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallNonvirtualBooleanMethod");
  std::exit(1);
}
jboolean CallNonvirtualBooleanMethodV(JNIEnv *env, jobject obj, jclass clazz,
                                      jmethodID methodID, va_list args) {
  puts("Unimplemented JNI method CallNonvirtualBooleanMethodV");
  std::exit(1);
}
jboolean CallNonvirtualBooleanMethodA(JNIEnv *env, jobject obj, jclass clazz,
                                      jmethodID methodID, const jvalue *args) {
  puts("Unimplemented JNI method CallNonvirtualBooleanMethodA");
  std::exit(1);
}
jbyte CallNonvirtualByteMethod(JNIEnv *env, jobject obj, jclass clazz,
                               jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallNonvirtualByteMethod");
  std::exit(1);
}
jbyte CallNonvirtualByteMethodV(JNIEnv *env, jobject obj, jclass clazz,
                                jmethodID methodID, va_list args) {
  puts("Unimplemented JNI method CallNonvirtualByteMethodV");
  std::exit(1);
}
jbyte CallNonvirtualByteMethodA(JNIEnv *env, jobject obj, jclass clazz,
                                jmethodID methodID, const jvalue *args) {
  puts("Unimplemented JNI method CallNonvirtualByteMethodA");
  std::exit(1);
}
jchar CallNonvirtualCharMethod(JNIEnv *env, jobject obj, jclass clazz,
                               jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallNonvirtualCharMethod");
  std::exit(1);
}
jchar CallNonvirtualCharMethodV(JNIEnv *env, jobject obj, jclass clazz,
                                jmethodID methodID, va_list args) {
  puts("Unimplemented JNI method CallNonvirtualCharMethodV");
  std::exit(1);
}
jchar CallNonvirtualCharMethodA(JNIEnv *env, jobject obj, jclass clazz,
                                jmethodID methodID, const jvalue *args) {
  puts("Unimplemented JNI method CallNonvirtualCharMethodA");
  std::exit(1);
}
jshort CallNonvirtualShortMethod(JNIEnv *env, jobject obj, jclass clazz,
                                 jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallNonvirtualShortMethod");
  std::exit(1);
}
jshort CallNonvirtualShortMethodV(JNIEnv *env, jobject obj, jclass clazz,
                                  jmethodID methodID, va_list args) {
  puts("Unimplemented JNI method CallNonvirtualShortMethodV");
  std::exit(1);
}
jshort CallNonvirtualShortMethodA(JNIEnv *env, jobject obj, jclass clazz,
                                  jmethodID methodID, const jvalue *args) {
  puts("Unimplemented JNI method CallNonvirtualShortMethodA");
  std::exit(1);
}
jint CallNonvirtualIntMethod(JNIEnv *env, jobject obj, jclass clazz,
                             jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallNonvirtualIntMethod");
  std::exit(1);
}
jint CallNonvirtualIntMethodV(JNIEnv *env, jobject obj, jclass clazz,
                              jmethodID methodID, va_list args) {
  puts("Unimplemented JNI method CallNonvirtualIntMethodV");
  std::exit(1);
}
jint CallNonvirtualIntMethodA(JNIEnv *env, jobject obj, jclass clazz,
                              jmethodID methodID, const jvalue *args) {
  puts("Unimplemented JNI method CallNonvirtualIntMethodA");
  std::exit(1);
}
jlong CallNonvirtualLongMethod(JNIEnv *env, jobject obj, jclass clazz,
                               jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallNonvirtualLongMethod");
  std::exit(1);
}
jlong CallNonvirtualLongMethodV(JNIEnv *env, jobject obj, jclass clazz,
                                jmethodID methodID, va_list args) {
  puts("Unimplemented JNI method CallNonvirtualLongMethodV");
  std::exit(1);
}
jlong CallNonvirtualLongMethodA(JNIEnv *env, jobject obj, jclass clazz,
                                jmethodID methodID, const jvalue *args) {
  puts("Unimplemented JNI method CallNonvirtualLongMethodA");
  std::exit(1);
}
jfloat CallNonvirtualFloatMethod(JNIEnv *env, jobject obj, jclass clazz,
                                 jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallNonvirtualFloatMethod");
  std::exit(1);
}
jfloat CallNonvirtualFloatMethodV(JNIEnv *env, jobject obj, jclass clazz,
                                  jmethodID methodID, va_list args) {
  puts("Unimplemented JNI method CallNonvirtualFloatMethodV");
  std::exit(1);
}
jfloat CallNonvirtualFloatMethodA(JNIEnv *env, jobject obj, jclass clazz,
                                  jmethodID methodID, const jvalue *args) {
  puts("Unimplemented JNI method CallNonvirtualFloatMethodA");
  std::exit(1);
}
jdouble CallNonvirtualDoubleMethod(JNIEnv *env, jobject obj, jclass clazz,
                                   jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallNonvirtualDoubleMethod");
  std::exit(1);
}
jdouble CallNonvirtualDoubleMethodV(JNIEnv *env, jobject obj, jclass clazz,
                                    jmethodID methodID, va_list args) {
  puts("Unimplemented JNI method CallNonvirtualDoubleMethodV");
  std::exit(1);
}
jdouble CallNonvirtualDoubleMethodA(JNIEnv *env, jobject obj, jclass clazz,
                                    jmethodID methodID, const jvalue *args) {
  puts("Unimplemented JNI method CallNonvirtualDoubleMethodA");
  std::exit(1);
}
void CallNonvirtualVoidMethod(JNIEnv *env, jobject obj, jclass clazz,
                              jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallNonvirtualVoidMethod");
  std::exit(1);
}
void CallNonvirtualVoidMethodV(JNIEnv *env, jobject obj, jclass clazz,
                               jmethodID methodID, va_list args) {
  puts("Unimplemented JNI method CallNonvirtualVoidMethodV");
  std::exit(1);
}
void CallNonvirtualVoidMethodA(JNIEnv *env, jobject obj, jclass clazz,
                               jmethodID methodID, const jvalue *args) {
  puts("Unimplemented JNI method CallNonvirtualVoidMethodA");
  std::exit(1);
}
jfieldID GetFieldID(JNIEnv *env, jclass clazz, const char *name,
                    const char *sig) {
  puts("Unimplemented JNI method GetFieldID");
  std::exit(1);
}
jobject GetObjectField(JNIEnv *env, jobject obj, jfieldID fieldID) {
  puts("Unimplemented JNI method GetObjectField");
  std::exit(1);
}
jboolean GetBooleanField(JNIEnv *env, jobject obj, jfieldID fieldID) {
  puts("Unimplemented JNI method GetBooleanField");
  std::exit(1);
}
jbyte GetByteField(JNIEnv *env, jobject obj, jfieldID fieldID) {
  puts("Unimplemented JNI method GetByteField");
  std::exit(1);
}
jchar GetCharField(JNIEnv *env, jobject obj, jfieldID fieldID) {
  puts("Unimplemented JNI method GetCharField");
  std::exit(1);
}
jshort GetShortField(JNIEnv *env, jobject obj, jfieldID fieldID) {
  puts("Unimplemented JNI method GetShortField");
  std::exit(1);
}
jint GetIntField(JNIEnv *env, jobject obj, jfieldID fieldID) {
  puts("Unimplemented JNI method GetIntField");
  std::exit(1);
}
jlong GetLongField(JNIEnv *env, jobject obj, jfieldID fieldID) {
  puts("Unimplemented JNI method GetLongField");
  std::exit(1);
}
jfloat GetFloatField(JNIEnv *env, jobject obj, jfieldID fieldID) {
  puts("Unimplemented JNI method GetFloatField");
  std::exit(1);
}
jdouble GetDoubleField(JNIEnv *env, jobject obj, jfieldID fieldID) {
  puts("Unimplemented JNI method GetDoubleField");
  std::exit(1);
}
void SetObjectField(JNIEnv *env, jobject obj, jfieldID fieldID, jobject val) {
  puts("Unimplemented JNI method SetObjectField");
  std::exit(1);
}
void SetBooleanField(JNIEnv *env, jobject obj, jfieldID fieldID, jboolean val) {
  puts("Unimplemented JNI method SetBooleanField");
  std::exit(1);
}
void SetByteField(JNIEnv *env, jobject obj, jfieldID fieldID, jbyte val) {
  puts("Unimplemented JNI method SetByteField");
  std::exit(1);
}
void SetCharField(JNIEnv *env, jobject obj, jfieldID fieldID, jchar val) {
  puts("Unimplemented JNI method SetCharField");
  std::exit(1);
}
void SetShortField(JNIEnv *env, jobject obj, jfieldID fieldID, jshort val) {
  puts("Unimplemented JNI method SetShortField");
  std::exit(1);
}
void SetIntField(JNIEnv *env, jobject obj, jfieldID fieldID, jint val) {
  puts("Unimplemented JNI method SetIntField");
  std::exit(1);
}
void SetLongField(JNIEnv *env, jobject obj, jfieldID fieldID, jlong val) {
  puts("Unimplemented JNI method SetLongField");
  std::exit(1);
}
void SetFloatField(JNIEnv *env, jobject obj, jfieldID fieldID, jfloat val) {
  puts("Unimplemented JNI method SetFloatField");
  std::exit(1);
}
void SetDoubleField(JNIEnv *env, jobject obj, jfieldID fieldID, jdouble val) {
  puts("Unimplemented JNI method SetDoubleField");
  std::exit(1);
}
jmethodID GetStaticMethodID(JNIEnv *env, jclass clazz, const char *name,
                            const char *sig) {
  puts("Unimplemented JNI method GetStaticMethodID");
  std::exit(1);
}
jobject CallStaticObjectMethod(JNIEnv *env, jclass clazz, jmethodID methodID,
                               ...) {
  puts("Unimplemented JNI method CallStaticObjectMethod");
  std::exit(1);
}
jobject CallStaticObjectMethodV(JNIEnv *env, jclass clazz, jmethodID methodID,
                                va_list args) {
  puts("Unimplemented JNI method CallStaticObjectMethodV");
  std::exit(1);
}
jobject CallStaticObjectMethodA(JNIEnv *env, jclass clazz, jmethodID methodID,
                                const jvalue *args) {
  puts("Unimplemented JNI method CallStaticObjectMethodA");
  std::exit(1);
}
jboolean CallStaticBooleanMethod(JNIEnv *env, jclass clazz, jmethodID methodID,
                                 ...) {
  puts("Unimplemented JNI method CallStaticBooleanMethod");
  std::exit(1);
}
jboolean CallStaticBooleanMethodV(JNIEnv *env, jclass clazz, jmethodID methodID,
                                  va_list args) {
  puts("Unimplemented JNI method CallStaticBooleanMethodV");
  std::exit(1);
}
jboolean CallStaticBooleanMethodA(JNIEnv *env, jclass clazz, jmethodID methodID,
                                  const jvalue *args) {
  puts("Unimplemented JNI method CallStaticBooleanMethodA");
  std::exit(1);
}
jbyte CallStaticByteMethod(JNIEnv *env, jclass clazz, jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallStaticByteMethod");
  std::exit(1);
}
jbyte CallStaticByteMethodV(JNIEnv *env, jclass clazz, jmethodID methodID,
                            va_list args) {
  puts("Unimplemented JNI method CallStaticByteMethodV");
  std::exit(1);
}
jbyte CallStaticByteMethodA(JNIEnv *env, jclass clazz, jmethodID methodID,
                            const jvalue *args) {
  puts("Unimplemented JNI method CallStaticByteMethodA");
  std::exit(1);
}
jchar CallStaticCharMethod(JNIEnv *env, jclass clazz, jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallStaticCharMethod");
  std::exit(1);
}
jchar CallStaticCharMethodV(JNIEnv *env, jclass clazz, jmethodID methodID,
                            va_list args) {
  puts("Unimplemented JNI method CallStaticCharMethodV");
  std::exit(1);
}
jchar CallStaticCharMethodA(JNIEnv *env, jclass clazz, jmethodID methodID,
                            const jvalue *args) {
  puts("Unimplemented JNI method CallStaticCharMethodA");
  std::exit(1);
}
jshort CallStaticShortMethod(JNIEnv *env, jclass clazz, jmethodID methodID,
                             ...) {
  puts("Unimplemented JNI method CallStaticShortMethod");
  std::exit(1);
}
jshort CallStaticShortMethodV(JNIEnv *env, jclass clazz, jmethodID methodID,
                              va_list args) {
  puts("Unimplemented JNI method CallStaticShortMethodV");
  std::exit(1);
}
jshort CallStaticShortMethodA(JNIEnv *env, jclass clazz, jmethodID methodID,
                              const jvalue *args) {
  puts("Unimplemented JNI method CallStaticShortMethodA");
  std::exit(1);
}
jint CallStaticIntMethod(JNIEnv *env, jclass clazz, jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallStaticIntMethod");
  std::exit(1);
}
jint CallStaticIntMethodV(JNIEnv *env, jclass clazz, jmethodID methodID,
                          va_list args) {
  puts("Unimplemented JNI method CallStaticIntMethodV");
  std::exit(1);
}
jint CallStaticIntMethodA(JNIEnv *env, jclass clazz, jmethodID methodID,
                          const jvalue *args) {
  puts("Unimplemented JNI method CallStaticIntMethodA");
  std::exit(1);
}
jlong CallStaticLongMethod(JNIEnv *env, jclass clazz, jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallStaticLongMethod");
  std::exit(1);
}
jlong CallStaticLongMethodV(JNIEnv *env, jclass clazz, jmethodID methodID,
                            va_list args) {
  puts("Unimplemented JNI method CallStaticLongMethodV");
  std::exit(1);
}
jlong CallStaticLongMethodA(JNIEnv *env, jclass clazz, jmethodID methodID,
                            const jvalue *args) {
  puts("Unimplemented JNI method CallStaticLongMethodA");
  std::exit(1);
}
jfloat CallStaticFloatMethod(JNIEnv *env, jclass clazz, jmethodID methodID,
                             ...) {
  puts("Unimplemented JNI method CallStaticFloatMethod");
  std::exit(1);
}
jfloat CallStaticFloatMethodV(JNIEnv *env, jclass clazz, jmethodID methodID,
                              va_list args) {
  puts("Unimplemented JNI method CallStaticFloatMethodV");
  std::exit(1);
}
jfloat CallStaticFloatMethodA(JNIEnv *env, jclass clazz, jmethodID methodID,
                              const jvalue *args) {
  puts("Unimplemented JNI method CallStaticFloatMethodA");
  std::exit(1);
}
jdouble CallStaticDoubleMethod(JNIEnv *env, jclass clazz, jmethodID methodID,
                               ...) {
  puts("Unimplemented JNI method CallStaticDoubleMethod");
  std::exit(1);
}
jdouble CallStaticDoubleMethodV(JNIEnv *env, jclass clazz, jmethodID methodID,
                                va_list args) {
  puts("Unimplemented JNI method CallStaticDoubleMethodV");
  std::exit(1);
}
jdouble CallStaticDoubleMethodA(JNIEnv *env, jclass clazz, jmethodID methodID,
                                const jvalue *args) {
  puts("Unimplemented JNI method CallStaticDoubleMethodA");
  std::exit(1);
}
void CallStaticVoidMethod(JNIEnv *env, jclass cls, jmethodID methodID, ...) {
  puts("Unimplemented JNI method CallStaticVoidMethod");
  std::exit(1);
}
void CallStaticVoidMethodV(JNIEnv *env, jclass cls, jmethodID methodID,
                           va_list args) {
  puts("Unimplemented JNI method CallStaticVoidMethodV");
  std::exit(1);
}
void CallStaticVoidMethodA(JNIEnv *env, jclass cls, jmethodID methodID,
                           const jvalue *args) {
  puts("Unimplemented JNI method CallStaticVoidMethodA");
  std::exit(1);
}
jfieldID GetStaticFieldID(JNIEnv *env, jclass clazz, const char *name,
                          const char *sig) {
  puts("Unimplemented JNI method GetStaticFieldID");
  std::exit(1);
}
jobject GetStaticObjectField(JNIEnv *env, jclass clazz, jfieldID fieldID) {
  puts("Unimplemented JNI method GetStaticObjectField");
  std::exit(1);
}
jboolean GetStaticBooleanField(JNIEnv *env, jclass clazz, jfieldID fieldID) {
  puts("Unimplemented JNI method GetStaticBooleanField");
  std::exit(1);
}
jbyte GetStaticByteField(JNIEnv *env, jclass clazz, jfieldID fieldID) {
  puts("Unimplemented JNI method GetStaticByteField");
  std::exit(1);
}
jchar GetStaticCharField(JNIEnv *env, jclass clazz, jfieldID fieldID) {
  puts("Unimplemented JNI method GetStaticCharField");
  std::exit(1);
}
jshort GetStaticShortField(JNIEnv *env, jclass clazz, jfieldID fieldID) {
  puts("Unimplemented JNI method GetStaticShortField");
  std::exit(1);
}
jint GetStaticIntField(JNIEnv *env, jclass clazz, jfieldID fieldID) {
  puts("Unimplemented JNI method GetStaticIntField");
  std::exit(1);
}
jlong GetStaticLongField(JNIEnv *env, jclass clazz, jfieldID fieldID) {
  puts("Unimplemented JNI method GetStaticLongField");
  std::exit(1);
}
jfloat GetStaticFloatField(JNIEnv *env, jclass clazz, jfieldID fieldID) {
  puts("Unimplemented JNI method GetStaticFloatField");
  std::exit(1);
}
jdouble GetStaticDoubleField(JNIEnv *env, jclass clazz, jfieldID fieldID) {
  puts("Unimplemented JNI method GetStaticDoubleField");
  std::exit(1);
}
void SetStaticObjectField(JNIEnv *env, jclass clazz, jfieldID fieldID,
                          jobject value) {
  puts("Unimplemented JNI method SetStaticObjectField");
  std::exit(1);
}
void SetStaticBooleanField(JNIEnv *env, jclass clazz, jfieldID fieldID,
                           jboolean value) {
  puts("Unimplemented JNI method SetStaticBooleanField");
  std::exit(1);
}
void SetStaticByteField(JNIEnv *env, jclass clazz, jfieldID fieldID,
                        jbyte value) {
  puts("Unimplemented JNI method SetStaticByteField");
  std::exit(1);
}
void SetStaticCharField(JNIEnv *env, jclass clazz, jfieldID fieldID,
                        jchar value) {
  puts("Unimplemented JNI method SetStaticCharField");
  std::exit(1);
}
void SetStaticShortField(JNIEnv *env, jclass clazz, jfieldID fieldID,
                         jshort value) {
  puts("Unimplemented JNI method SetStaticShortField");
  std::exit(1);
}
void SetStaticIntField(JNIEnv *env, jclass clazz, jfieldID fieldID,
                       jint value) {
  puts("Unimplemented JNI method SetStaticIntField");
  std::exit(1);
}
void SetStaticLongField(JNIEnv *env, jclass clazz, jfieldID fieldID,
                        jlong value) {
  puts("Unimplemented JNI method SetStaticLongField");
  std::exit(1);
}
void SetStaticFloatField(JNIEnv *env, jclass clazz, jfieldID fieldID,
                         jfloat value) {
  puts("Unimplemented JNI method SetStaticFloatField");
  std::exit(1);
}
void SetStaticDoubleField(JNIEnv *env, jclass clazz, jfieldID fieldID,
                          jdouble value) {
  puts("Unimplemented JNI method SetStaticDoubleField");
  std::exit(1);
}
jstring NewString(JNIEnv *env, const jchar *unicode, jsize len) {
  puts("Unimplemented JNI method NewString");
  std::exit(1);
}
jsize GetStringLength(JNIEnv *env, jstring str) {
  puts("Unimplemented JNI method GetStringLength");
  std::exit(1);
}
const jchar *GetStringChars(JNIEnv *env, jstring str, jboolean *isCopy) {
  puts("Unimplemented JNI method GetStringChars");
  std::exit(1);
}
void ReleaseStringChars(JNIEnv *env, jstring str, const jchar *chars) {
  puts("Unimplemented JNI method ReleaseStringChars");
  std::exit(1);
}
jstring NewStringUTF(JNIEnv *env, const char *utf) {
  puts("Unimplemented JNI method NewStringUTF");
  std::exit(1);
}
jsize GetStringUTFLength(JNIEnv *env, jstring str) {
  puts("Unimplemented JNI method GetStringUTFLength");
  std::exit(1);
}
const char *GetStringUTFChars(JNIEnv *env, jstring str, jboolean *isCopy) {
  puts("Unimplemented JNI method GetStringUTFChars");
  std::exit(1);
}
void ReleaseStringUTFChars(JNIEnv *env, jstring str, const char *chars) {
  puts("Unimplemented JNI method ReleaseStringUTFChars");
  std::exit(1);
}
jsize GetArrayLength(JNIEnv *env, jarray array) {
  puts("Unimplemented JNI method GetArrayLength");
  std::exit(1);
}
jobjectArray NewObjectArray(JNIEnv *env, jsize len, jclass clazz,
                            jobject init) {
  puts("Unimplemented JNI method NewObjectArray");
  std::exit(1);
}
jobject GetObjectArrayElement(JNIEnv *env, jobjectArray array, jsize index) {
  puts("Unimplemented JNI method GetObjectArrayElement");
  std::exit(1);
}
void SetObjectArrayElement(JNIEnv *env, jobjectArray array, jsize index,
                           jobject val) {
  puts("Unimplemented JNI method SetObjectArrayElement");
  std::exit(1);
}
jbooleanArray NewBooleanArray(JNIEnv *env, jsize len) {
  puts("Unimplemented JNI method NewBooleanArray");
  std::exit(1);
}
jbyteArray NewByteArray(JNIEnv *env, jsize len) {
  puts("Unimplemented JNI method NewByteArray");
  std::exit(1);
}
jcharArray NewCharArray(JNIEnv *env, jsize len) {
  puts("Unimplemented JNI method NewCharArray");
  std::exit(1);
}
jshortArray NewShortArray(JNIEnv *env, jsize len) {
  puts("Unimplemented JNI method NewShortArray");
  std::exit(1);
}
jintArray NewIntArray(JNIEnv *env, jsize len) {
  puts("Unimplemented JNI method NewIntArray");
  std::exit(1);
}
jlongArray NewLongArray(JNIEnv *env, jsize len) {
  puts("Unimplemented JNI method NewLongArray");
  std::exit(1);
}
jfloatArray NewFloatArray(JNIEnv *env, jsize len) {
  puts("Unimplemented JNI method NewFloatArray");
  std::exit(1);
}
jdoubleArray NewDoubleArray(JNIEnv *env, jsize len) {
  puts("Unimplemented JNI method NewDoubleArray");
  std::exit(1);
}
jboolean *GetBooleanArrayElements(JNIEnv *env, jbooleanArray array,
                                  jboolean *isCopy) {
  puts("Unimplemented JNI method GetBooleanArrayElements");
  std::exit(1);
}
jbyte *GetByteArrayElements(JNIEnv *env, jbyteArray array, jboolean *isCopy) {
  puts("Unimplemented JNI method GetByteArrayElements");
  std::exit(1);
}
jchar *GetCharArrayElements(JNIEnv *env, jcharArray array, jboolean *isCopy) {
  puts("Unimplemented JNI method GetCharArrayElements");
  std::exit(1);
}
jshort *GetShortArrayElements(JNIEnv *env, jshortArray array,
                              jboolean *isCopy) {
  puts("Unimplemented JNI method GetShortArrayElements");
  std::exit(1);
}
jint *GetIntArrayElements(JNIEnv *env, jintArray array, jboolean *isCopy) {
  puts("Unimplemented JNI method GetIntArrayElements");
  std::exit(1);
}
jlong *GetLongArrayElements(JNIEnv *env, jlongArray array, jboolean *isCopy) {
  puts("Unimplemented JNI method GetLongArrayElements");
  std::exit(1);
}
jfloat *GetFloatArrayElements(JNIEnv *env, jfloatArray array,
                              jboolean *isCopy) {
  puts("Unimplemented JNI method GetFloatArrayElements");
  std::exit(1);
}
jdouble *GetDoubleArrayElements(JNIEnv *env, jdoubleArray array,
                                jboolean *isCopy) {
  puts("Unimplemented JNI method GetDoubleArrayElements");
  std::exit(1);
}
void ReleaseBooleanArrayElements(JNIEnv *env, jbooleanArray array,
                                 jboolean *elems, jint mode) {
  puts("Unimplemented JNI method ReleaseBooleanArrayElements");
  std::exit(1);
}
void ReleaseByteArrayElements(JNIEnv *env, jbyteArray array, jbyte *elems,
                              jint mode) {
  puts("Unimplemented JNI method ReleaseByteArrayElements");
  std::exit(1);
}
void ReleaseCharArrayElements(JNIEnv *env, jcharArray array, jchar *elems,
                              jint mode) {
  puts("Unimplemented JNI method ReleaseCharArrayElements");
  std::exit(1);
}
void ReleaseShortArrayElements(JNIEnv *env, jshortArray array, jshort *elems,
                               jint mode) {
  puts("Unimplemented JNI method ReleaseShortArrayElements");
  std::exit(1);
}
void ReleaseIntArrayElements(JNIEnv *env, jintArray array, jint *elems,
                             jint mode) {
  puts("Unimplemented JNI method ReleaseIntArrayElements");
  std::exit(1);
}
void ReleaseLongArrayElements(JNIEnv *env, jlongArray array, jlong *elems,
                              jint mode) {
  puts("Unimplemented JNI method ReleaseLongArrayElements");
  std::exit(1);
}
void ReleaseFloatArrayElements(JNIEnv *env, jfloatArray array, jfloat *elems,
                               jint mode) {
  puts("Unimplemented JNI method ReleaseFloatArrayElements");
  std::exit(1);
}
void ReleaseDoubleArrayElements(JNIEnv *env, jdoubleArray array, jdouble *elems,
                                jint mode) {
  puts("Unimplemented JNI method ReleaseDoubleArrayElements");
  std::exit(1);
}
void GetBooleanArrayRegion(JNIEnv *env, jbooleanArray array, jsize start,
                           jsize l, jboolean *buf) {
  puts("Unimplemented JNI method GetBooleanArrayRegion");
  std::exit(1);
}
void GetByteArrayRegion(JNIEnv *env, jbyteArray array, jsize start, jsize len,
                        jbyte *buf) {
  puts("Unimplemented JNI method GetByteArrayRegion");
  std::exit(1);
}
void GetCharArrayRegion(JNIEnv *env, jcharArray array, jsize start, jsize len,
                        jchar *buf) {
  puts("Unimplemented JNI method GetCharArrayRegion");
  std::exit(1);
}
void GetShortArrayRegion(JNIEnv *env, jshortArray array, jsize start, jsize len,
                         jshort *buf) {
  puts("Unimplemented JNI method GetShortArrayRegion");
  std::exit(1);
}
void GetIntArrayRegion(JNIEnv *env, jintArray array, jsize start, jsize len,
                       jint *buf) {
  puts("Unimplemented JNI method GetIntArrayRegion");
  std::exit(1);
}
void GetLongArrayRegion(JNIEnv *env, jlongArray array, jsize start, jsize len,
                        jlong *buf) {
  puts("Unimplemented JNI method GetLongArrayRegion");
  std::exit(1);
}
void GetFloatArrayRegion(JNIEnv *env, jfloatArray array, jsize start, jsize len,
                         jfloat *buf) {
  puts("Unimplemented JNI method GetFloatArrayRegion");
  std::exit(1);
}
void GetDoubleArrayRegion(JNIEnv *env, jdoubleArray array, jsize start,
                          jsize len, jdouble *buf) {
  puts("Unimplemented JNI method GetDoubleArrayRegion");
  std::exit(1);
}
void SetBooleanArrayRegion(JNIEnv *env, jbooleanArray array, jsize start,
                           jsize l, const jboolean *buf) {
  puts("Unimplemented JNI method SetBooleanArrayRegion");
  std::exit(1);
}
void SetByteArrayRegion(JNIEnv *env, jbyteArray array, jsize start, jsize len,
                        const jbyte *buf) {
  puts("Unimplemented JNI method SetByteArrayRegion");
  std::exit(1);
}
void SetCharArrayRegion(JNIEnv *env, jcharArray array, jsize start, jsize len,
                        const jchar *buf) {
  puts("Unimplemented JNI method SetCharArrayRegion");
  std::exit(1);
}
void SetShortArrayRegion(JNIEnv *env, jshortArray array, jsize start, jsize len,
                         const jshort *buf) {
  puts("Unimplemented JNI method SetShortArrayRegion");
  std::exit(1);
}
void SetIntArrayRegion(JNIEnv *env, jintArray array, jsize start, jsize len,
                       const jint *buf) {
  puts("Unimplemented JNI method SetIntArrayRegion");
  std::exit(1);
}
void SetLongArrayRegion(JNIEnv *env, jlongArray array, jsize start, jsize len,
                        const jlong *buf) {
  puts("Unimplemented JNI method SetLongArrayRegion");
  std::exit(1);
}
void SetFloatArrayRegion(JNIEnv *env, jfloatArray array, jsize start, jsize len,
                         const jfloat *buf) {
  puts("Unimplemented JNI method SetFloatArrayRegion");
  std::exit(1);
}
void SetDoubleArrayRegion(JNIEnv *env, jdoubleArray array, jsize start,
                          jsize len, const jdouble *buf) {
  puts("Unimplemented JNI method SetDoubleArrayRegion");
  std::exit(1);
}
jint RegisterNatives(JNIEnv *env, jclass clazz, const JNINativeMethod *methods,
                     jint nMethods) {
  auto *data = getData(env);
  for (int i = 0; i < nMethods; i++) {
    auto method = methods[i];
    auto key = std::string(method.name) + ";" + method.signature;
    data->registered_natives.insert_or_assign(key, method.fnPtr);
  }

  for (auto [k, v] : data->registered_natives) {
    printf("%s -> %p\n", k.data(), v);
  }

  return 0;
}
jint UnregisterNatives(JNIEnv *env, jclass clazz) {
  puts("Unimplemented JNI method UnregisterNatives");
  std::exit(1);
}
jint MonitorEnter(JNIEnv *env, jobject obj) {
  puts("Unimplemented JNI method MonitorEnter");
  std::exit(1);
}
jint MonitorExit(JNIEnv *env, jobject obj) {
  puts("Unimplemented JNI method MonitorExit");
  std::exit(1);
}
jint GetJavaVM(JNIEnv *env, JavaVM **vm) {
  puts("Unimplemented JNI method GetJavaVM");
  std::exit(1);
}
void GetStringRegion(JNIEnv *env, jstring str, jsize start, jsize len,
                     jchar *buf) {
  puts("Unimplemented JNI method GetStringRegion");
  std::exit(1);
}
void GetStringUTFRegion(JNIEnv *env, jstring str, jsize start, jsize len,
                        char *buf) {
  puts("Unimplemented JNI method GetStringUTFRegion");
  std::exit(1);
}
void *GetPrimitiveArrayCritical(JNIEnv *env, jarray array, jboolean *isCopy) {
  puts("Unimplemented JNI method GetPrimitiveArrayCritical");
  std::exit(1);
}
void ReleasePrimitiveArrayCritical(JNIEnv *env, jarray array, void *carray,
                                   jint mode) {
  puts("Unimplemented JNI method ReleasePrimitiveArrayCritical");
  std::exit(1);
}
const jchar *GetStringCritical(JNIEnv *env, jstring string, jboolean *isCopy) {
  puts("Unimplemented JNI method GetStringCritical");
  std::exit(1);
}
void ReleaseStringCritical(JNIEnv *env, jstring string, const jchar *cstring) {
  puts("Unimplemented JNI method ReleaseStringCritical");
  std::exit(1);
}
jweak NewWeakGlobalRef(JNIEnv *env, jobject obj) {
  puts("Unimplemented JNI method NewWeakGlobalRef");
  std::exit(1);
}
void DeleteWeakGlobalRef(JNIEnv *env, jweak ref) {
  puts("Unimplemented JNI method DeleteWeakGlobalRef");
  std::exit(1);
}
jboolean ExceptionCheck(JNIEnv *env) {
  puts("Unimplemented JNI method ExceptionCheck");
  std::exit(1);
}
jobject NewDirectByteBuffer(JNIEnv *env, void *address, jlong capacity) {
  puts("Unimplemented JNI method NewDirectByteBuffer");
  std::exit(1);
}
void *GetDirectBufferAddress(JNIEnv *env, jobject buf) {
  puts("Unimplemented JNI method GetDirectBufferAddress");
  std::exit(1);
}
jlong GetDirectBufferCapacity(JNIEnv *env, jobject buf) {
  puts("Unimplemented JNI method GetDirectBufferCapacity");
  std::exit(1);
}
jobjectRefType GetObjectRefType(JNIEnv *env, jobject obj) {
  puts("Unimplemented JNI method GetObjectRefType");
  std::exit(1);
}
jobject GetModule(JNIEnv *env, jclass clazz) {
  puts("Unimplemented JNI method GetModule");
  std::exit(1);
}
jboolean IsVirtualThread(JNIEnv *env, jobject obj) {
  puts("Unimplemented JNI method IsVirtualThread");
  std::exit(1);
}
} // namespace jvmilia