#pragma once

#include <cstdarg>

extern "C" {

using JNIEnv = const struct JNINativeInterface*;
using jint = int;
using jlong = long;
using jboolean = unsigned char;
using jbyte = signed char;
using jsize = jint;
using jchar = unsigned short;
using jshort = short;
using jfloat = float;
using jdouble = double;

class _jobject {};
class _jclass : public _jobject {};
class _jthrowable : public _jobject {};
class _jstring : public _jobject {};
class _jarray : public _jobject {};
class _jbooleanArray : public _jarray {};
class _jbyteArray : public _jarray {};
class _jcharArray : public _jarray {};
class _jshortArray : public _jarray {};
class _jintArray : public _jarray {};
class _jlongArray : public _jarray {};
class _jfloatArray : public _jarray {};
class _jdoubleArray : public _jarray {};
class _jobjectArray : public _jarray {};

using jobject = _jobject*;
using jclass = _jclass*;
using jthrowable = _jthrowable*;
using jstring = _jstring*;
using jarray = _jarray*;
using jbooleanArray = _jbooleanArray*;
using jbyteArray = _jbyteArray*;
using jcharArray = _jcharArray*;
using jshortArray = _jshortArray*;
using jintArray = _jintArray*;
using jlongArray = _jlongArray*;
using jfloatArray = _jfloatArray*;
using jdoubleArray = _jdoubleArray*;
using jobjectArray = _jobjectArray*;

using jweak = jobject;

struct _jmethodID;
using jmethodID = _jmethodID*;
struct _jfieldID;
using jfieldID = _jfieldID*;

union jvalue {
  jboolean z;
  jbyte b;
  jchar c;
  jshort s;
  jint i;
  jlong j;
  jfloat f;
  jdouble d;
  jobject l;
};

struct JNINativeMethod {
  char* name;
  char* signature;
  void* fnPtr;
};

struct JavaVM_;
using JavaVM = JavaVM_*;

enum jobjectRefType { JNIInvalidRefType = 0, JNILocalRefType = 1, JNIGlobalRefType = 2, JNIWeakGlobalRefType = 3 };

namespace jvmilia {
jint GetVersion(JNIEnv* env);
jclass DefineClass(JNIEnv* env, const char* name, jobject loader, const jbyte* buf, jsize len);
jclass FindClass(JNIEnv* env, const char* name);
jmethodID FromReflectedMethod(JNIEnv* env, jobject method);
jfieldID FromReflectedField(JNIEnv* env, jobject field);
jobject ToReflectedMethod(JNIEnv* env, jclass cls, jmethodID methodID, jboolean isStatic);
jclass GetSuperclass(JNIEnv* env, jclass sub);
jboolean IsAssignableFrom(JNIEnv* env, jclass sub, jclass sup);
jobject ToReflectedField(JNIEnv* env, jclass cls, jfieldID fieldID, jboolean isStatic);
jint Throw(JNIEnv* env, jthrowable obj);
jint ThrowNew(JNIEnv* env, jclass clazz, const char* msg);
jthrowable ExceptionOccurred(JNIEnv* env);
void ExceptionDescribe(JNIEnv* env);
void ExceptionClear(JNIEnv* env);
void FatalError(JNIEnv* env, const char* msg);
jint PushLocalFrame(JNIEnv* env, jint capacity);
jobject PopLocalFrame(JNIEnv* env, jobject result);
jobject NewGlobalRef(JNIEnv* env, jobject lobj);
void DeleteGlobalRef(JNIEnv* env, jobject gref);
void DeleteLocalRef(JNIEnv* env, jobject obj);
jboolean IsSameObject(JNIEnv* env, jobject obj1, jobject obj2);
jobject NewLocalRef(JNIEnv* env, jobject ref);
jint EnsureLocalCapacity(JNIEnv* env, jint capacity);
jobject AllocObject(JNIEnv* env, jclass clazz);
jobject NewObject(JNIEnv* env, jclass clazz, jmethodID methodID, ...);
jobject NewObjectV(JNIEnv* env, jclass clazz, jmethodID methodID, va_list args);
jobject NewObjectA(JNIEnv* env, jclass clazz, jmethodID methodID, const jvalue* args);
jclass GetObjectClass(JNIEnv* env, jobject obj);
jboolean IsInstanceOf(JNIEnv* env, jobject obj, jclass clazz);
jmethodID GetMethodID(JNIEnv* env, jclass clazz, const char* name, const char* sig);
jobject CallObjectMethod(JNIEnv* env, jobject obj, jmethodID methodID, ...);
jobject CallObjectMethodV(JNIEnv* env, jobject obj, jmethodID methodID, va_list args);
jobject CallObjectMethodA(JNIEnv* env, jobject obj, jmethodID methodID, const jvalue* args);
jboolean CallBooleanMethod(JNIEnv* env, jobject obj, jmethodID methodID, ...);
jboolean CallBooleanMethodV(JNIEnv* env, jobject obj, jmethodID methodID, va_list args);
jboolean CallBooleanMethodA(JNIEnv* env, jobject obj, jmethodID methodID, const jvalue* args);
jbyte CallByteMethod(JNIEnv* env, jobject obj, jmethodID methodID, ...);
jbyte CallByteMethodV(JNIEnv* env, jobject obj, jmethodID methodID, va_list args);
jbyte CallByteMethodA(JNIEnv* env, jobject obj, jmethodID methodID, const jvalue* args);
jchar CallCharMethod(JNIEnv* env, jobject obj, jmethodID methodID, ...);
jchar CallCharMethodV(JNIEnv* env, jobject obj, jmethodID methodID, va_list args);
jchar CallCharMethodA(JNIEnv* env, jobject obj, jmethodID methodID, const jvalue* args);
jshort CallShortMethod(JNIEnv* env, jobject obj, jmethodID methodID, ...);
jshort CallShortMethodV(JNIEnv* env, jobject obj, jmethodID methodID, va_list args);
jshort CallShortMethodA(JNIEnv* env, jobject obj, jmethodID methodID, const jvalue* args);
jint CallIntMethod(JNIEnv* env, jobject obj, jmethodID methodID, ...);
jint CallIntMethodV(JNIEnv* env, jobject obj, jmethodID methodID, va_list args);
jint CallIntMethodA(JNIEnv* env, jobject obj, jmethodID methodID, const jvalue* args);
jlong CallLongMethod(JNIEnv* env, jobject obj, jmethodID methodID, ...);
jlong CallLongMethodV(JNIEnv* env, jobject obj, jmethodID methodID, va_list args);
jlong CallLongMethodA(JNIEnv* env, jobject obj, jmethodID methodID, const jvalue* args);
jfloat CallFloatMethod(JNIEnv* env, jobject obj, jmethodID methodID, ...);
jfloat CallFloatMethodV(JNIEnv* env, jobject obj, jmethodID methodID, va_list args);
jfloat CallFloatMethodA(JNIEnv* env, jobject obj, jmethodID methodID, const jvalue* args);
jdouble CallDoubleMethod(JNIEnv* env, jobject obj, jmethodID methodID, ...);
jdouble CallDoubleMethodV(JNIEnv* env, jobject obj, jmethodID methodID, va_list args);
jdouble CallDoubleMethodA(JNIEnv* env, jobject obj, jmethodID methodID, const jvalue* args);
void CallVoidMethod(JNIEnv* env, jobject obj, jmethodID methodID, ...);
void CallVoidMethodV(JNIEnv* env, jobject obj, jmethodID methodID, va_list args);
void CallVoidMethodA(JNIEnv* env, jobject obj, jmethodID methodID, const jvalue* args);
jobject CallNonvirtualObjectMethod(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, ...);
jobject CallNonvirtualObjectMethodV(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, va_list args);
jobject CallNonvirtualObjectMethodA(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, const jvalue* args);
jboolean CallNonvirtualBooleanMethod(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, ...);
jboolean CallNonvirtualBooleanMethodV(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, va_list args);
jboolean CallNonvirtualBooleanMethodA(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, const jvalue* args);
jbyte CallNonvirtualByteMethod(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, ...);
jbyte CallNonvirtualByteMethodV(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, va_list args);
jbyte CallNonvirtualByteMethodA(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, const jvalue* args);
jchar CallNonvirtualCharMethod(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, ...);
jchar CallNonvirtualCharMethodV(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, va_list args);
jchar CallNonvirtualCharMethodA(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, const jvalue* args);
jshort CallNonvirtualShortMethod(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, ...);
jshort CallNonvirtualShortMethodV(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, va_list args);
jshort CallNonvirtualShortMethodA(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, const jvalue* args);
jint CallNonvirtualIntMethod(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, ...);
jint CallNonvirtualIntMethodV(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, va_list args);
jint CallNonvirtualIntMethodA(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, const jvalue* args);
jlong CallNonvirtualLongMethod(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, ...);
jlong CallNonvirtualLongMethodV(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, va_list args);
jlong CallNonvirtualLongMethodA(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, const jvalue* args);
jfloat CallNonvirtualFloatMethod(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, ...);
jfloat CallNonvirtualFloatMethodV(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, va_list args);
jfloat CallNonvirtualFloatMethodA(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, const jvalue* args);
jdouble CallNonvirtualDoubleMethod(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, ...);
jdouble CallNonvirtualDoubleMethodV(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, va_list args);
jdouble CallNonvirtualDoubleMethodA(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, const jvalue* args);
void CallNonvirtualVoidMethod(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, ...);
void CallNonvirtualVoidMethodV(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, va_list args);
void CallNonvirtualVoidMethodA(JNIEnv* env, jobject obj, jclass clazz, jmethodID methodID, const jvalue* args);
jfieldID GetFieldID(JNIEnv* env, jclass clazz, const char* name, const char* sig);
jobject GetObjectField(JNIEnv* env, jobject obj, jfieldID fieldID);
jboolean GetBooleanField(JNIEnv* env, jobject obj, jfieldID fieldID);
jbyte GetByteField(JNIEnv* env, jobject obj, jfieldID fieldID);
jchar GetCharField(JNIEnv* env, jobject obj, jfieldID fieldID);
jshort GetShortField(JNIEnv* env, jobject obj, jfieldID fieldID);
jint GetIntField(JNIEnv* env, jobject obj, jfieldID fieldID);
jlong GetLongField(JNIEnv* env, jobject obj, jfieldID fieldID);
jfloat GetFloatField(JNIEnv* env, jobject obj, jfieldID fieldID);
jdouble GetDoubleField(JNIEnv* env, jobject obj, jfieldID fieldID);
void SetObjectField(JNIEnv* env, jobject obj, jfieldID fieldID, jobject val);
void SetBooleanField(JNIEnv* env, jobject obj, jfieldID fieldID, jboolean val);
void SetByteField(JNIEnv* env, jobject obj, jfieldID fieldID, jbyte val);
void SetCharField(JNIEnv* env, jobject obj, jfieldID fieldID, jchar val);
void SetShortField(JNIEnv* env, jobject obj, jfieldID fieldID, jshort val);
void SetIntField(JNIEnv* env, jobject obj, jfieldID fieldID, jint val);
void SetLongField(JNIEnv* env, jobject obj, jfieldID fieldID, jlong val);
void SetFloatField(JNIEnv* env, jobject obj, jfieldID fieldID, jfloat val);
void SetDoubleField(JNIEnv* env, jobject obj, jfieldID fieldID, jdouble val);
jmethodID GetStaticMethodID(JNIEnv* env, jclass clazz, const char* name, const char* sig);
jobject CallStaticObjectMethod(JNIEnv* env, jclass clazz, jmethodID methodID, ...);
jobject CallStaticObjectMethodV(JNIEnv* env, jclass clazz, jmethodID methodID, va_list args);
jobject CallStaticObjectMethodA(JNIEnv* env, jclass clazz, jmethodID methodID, const jvalue* args);
jboolean CallStaticBooleanMethod(JNIEnv* env, jclass clazz, jmethodID methodID, ...);
jboolean CallStaticBooleanMethodV(JNIEnv* env, jclass clazz, jmethodID methodID, va_list args);
jboolean CallStaticBooleanMethodA(JNIEnv* env, jclass clazz, jmethodID methodID, const jvalue* args);
jbyte CallStaticByteMethod(JNIEnv* env, jclass clazz, jmethodID methodID, ...);
jbyte CallStaticByteMethodV(JNIEnv* env, jclass clazz, jmethodID methodID, va_list args);
jbyte CallStaticByteMethodA(JNIEnv* env, jclass clazz, jmethodID methodID, const jvalue* args);
jchar CallStaticCharMethod(JNIEnv* env, jclass clazz, jmethodID methodID, ...);
jchar CallStaticCharMethodV(JNIEnv* env, jclass clazz, jmethodID methodID, va_list args);
jchar CallStaticCharMethodA(JNIEnv* env, jclass clazz, jmethodID methodID, const jvalue* args);
jshort CallStaticShortMethod(JNIEnv* env, jclass clazz, jmethodID methodID, ...);
jshort CallStaticShortMethodV(JNIEnv* env, jclass clazz, jmethodID methodID, va_list args);
jshort CallStaticShortMethodA(JNIEnv* env, jclass clazz, jmethodID methodID, const jvalue* args);
jint CallStaticIntMethod(JNIEnv* env, jclass clazz, jmethodID methodID, ...);
jint CallStaticIntMethodV(JNIEnv* env, jclass clazz, jmethodID methodID, va_list args);
jint CallStaticIntMethodA(JNIEnv* env, jclass clazz, jmethodID methodID, const jvalue* args);
jlong CallStaticLongMethod(JNIEnv* env, jclass clazz, jmethodID methodID, ...);
jlong CallStaticLongMethodV(JNIEnv* env, jclass clazz, jmethodID methodID, va_list args);
jlong CallStaticLongMethodA(JNIEnv* env, jclass clazz, jmethodID methodID, const jvalue* args);
jfloat CallStaticFloatMethod(JNIEnv* env, jclass clazz, jmethodID methodID, ...);
jfloat CallStaticFloatMethodV(JNIEnv* env, jclass clazz, jmethodID methodID, va_list args);
jfloat CallStaticFloatMethodA(JNIEnv* env, jclass clazz, jmethodID methodID, const jvalue* args);
jdouble CallStaticDoubleMethod(JNIEnv* env, jclass clazz, jmethodID methodID, ...);
jdouble CallStaticDoubleMethodV(JNIEnv* env, jclass clazz, jmethodID methodID, va_list args);
jdouble CallStaticDoubleMethodA(JNIEnv* env, jclass clazz, jmethodID methodID, const jvalue* args);
void CallStaticVoidMethod(JNIEnv* env, jclass cls, jmethodID methodID, ...);
void CallStaticVoidMethodV(JNIEnv* env, jclass cls, jmethodID methodID, va_list args);
void CallStaticVoidMethodA(JNIEnv* env, jclass cls, jmethodID methodID, const jvalue* args);
jfieldID GetStaticFieldID(JNIEnv* env, jclass clazz, const char* name, const char* sig);
jobject GetStaticObjectField(JNIEnv* env, jclass clazz, jfieldID fieldID);
jboolean GetStaticBooleanField(JNIEnv* env, jclass clazz, jfieldID fieldID);
jbyte GetStaticByteField(JNIEnv* env, jclass clazz, jfieldID fieldID);
jchar GetStaticCharField(JNIEnv* env, jclass clazz, jfieldID fieldID);
jshort GetStaticShortField(JNIEnv* env, jclass clazz, jfieldID fieldID);
jint GetStaticIntField(JNIEnv* env, jclass clazz, jfieldID fieldID);
jlong GetStaticLongField(JNIEnv* env, jclass clazz, jfieldID fieldID);
jfloat GetStaticFloatField(JNIEnv* env, jclass clazz, jfieldID fieldID);
jdouble GetStaticDoubleField(JNIEnv* env, jclass clazz, jfieldID fieldID);
void SetStaticObjectField(JNIEnv* env, jclass clazz, jfieldID fieldID, jobject value);
void SetStaticBooleanField(JNIEnv* env, jclass clazz, jfieldID fieldID, jboolean value);
void SetStaticByteField(JNIEnv* env, jclass clazz, jfieldID fieldID, jbyte value);
void SetStaticCharField(JNIEnv* env, jclass clazz, jfieldID fieldID, jchar value);
void SetStaticShortField(JNIEnv* env, jclass clazz, jfieldID fieldID, jshort value);
void SetStaticIntField(JNIEnv* env, jclass clazz, jfieldID fieldID, jint value);
void SetStaticLongField(JNIEnv* env, jclass clazz, jfieldID fieldID, jlong value);
void SetStaticFloatField(JNIEnv* env, jclass clazz, jfieldID fieldID, jfloat value);
void SetStaticDoubleField(JNIEnv* env, jclass clazz, jfieldID fieldID, jdouble value);
jstring NewString(JNIEnv* env, const jchar* unicode, jsize len);
jsize GetStringLength(JNIEnv* env, jstring str);
const jchar* GetStringChars(JNIEnv* env, jstring str, jboolean* isCopy);
void ReleaseStringChars(JNIEnv* env, jstring str, const jchar* chars);
jstring NewStringUTF(JNIEnv* env, const char* utf);
jsize GetStringUTFLength(JNIEnv* env, jstring str);
const char* GetStringUTFChars(JNIEnv* env, jstring str, jboolean* isCopy);
void ReleaseStringUTFChars(JNIEnv* env, jstring str, const char* chars);
jsize GetArrayLength(JNIEnv* env, jarray array);
jobjectArray NewObjectArray(JNIEnv* env, jsize len, jclass clazz, jobject init);
jobject GetObjectArrayElement(JNIEnv* env, jobjectArray array, jsize index);
void SetObjectArrayElement(JNIEnv* env, jobjectArray array, jsize index, jobject val);
jbooleanArray NewBooleanArray(JNIEnv* env, jsize len);
jbyteArray NewByteArray(JNIEnv* env, jsize len);
jcharArray NewCharArray(JNIEnv* env, jsize len);
jshortArray NewShortArray(JNIEnv* env, jsize len);
jintArray NewIntArray(JNIEnv* env, jsize len);
jlongArray NewLongArray(JNIEnv* env, jsize len);
jfloatArray NewFloatArray(JNIEnv* env, jsize len);
jdoubleArray NewDoubleArray(JNIEnv* env, jsize len);
jboolean* GetBooleanArrayElements(JNIEnv* env, jbooleanArray array, jboolean* isCopy);
jbyte* GetByteArrayElements(JNIEnv* env, jbyteArray array, jboolean* isCopy);
jchar* GetCharArrayElements(JNIEnv* env, jcharArray array, jboolean* isCopy);
jshort* GetShortArrayElements(JNIEnv* env, jshortArray array, jboolean* isCopy);
jint* GetIntArrayElements(JNIEnv* env, jintArray array, jboolean* isCopy);
jlong* GetLongArrayElements(JNIEnv* env, jlongArray array, jboolean* isCopy);
jfloat* GetFloatArrayElements(JNIEnv* env, jfloatArray array, jboolean* isCopy);
jdouble* GetDoubleArrayElements(JNIEnv* env, jdoubleArray array, jboolean* isCopy);
void ReleaseBooleanArrayElements(JNIEnv* env, jbooleanArray array, jboolean* elems, jint mode);
void ReleaseByteArrayElements(JNIEnv* env, jbyteArray array, jbyte* elems, jint mode);
void ReleaseCharArrayElements(JNIEnv* env, jcharArray array, jchar* elems, jint mode);
void ReleaseShortArrayElements(JNIEnv* env, jshortArray array, jshort* elems, jint mode);
void ReleaseIntArrayElements(JNIEnv* env, jintArray array, jint* elems, jint mode);
void ReleaseLongArrayElements(JNIEnv* env, jlongArray array, jlong* elems, jint mode);
void ReleaseFloatArrayElements(JNIEnv* env, jfloatArray array, jfloat* elems, jint mode);
void ReleaseDoubleArrayElements(JNIEnv* env, jdoubleArray array, jdouble* elems, jint mode);
void GetBooleanArrayRegion(JNIEnv* env, jbooleanArray array, jsize start, jsize l, jboolean* buf);
void GetByteArrayRegion(JNIEnv* env, jbyteArray array, jsize start, jsize len, jbyte* buf);
void GetCharArrayRegion(JNIEnv* env, jcharArray array, jsize start, jsize len, jchar* buf);
void GetShortArrayRegion(JNIEnv* env, jshortArray array, jsize start, jsize len, jshort* buf);
void GetIntArrayRegion(JNIEnv* env, jintArray array, jsize start, jsize len, jint* buf);
void GetLongArrayRegion(JNIEnv* env, jlongArray array, jsize start, jsize len, jlong* buf);
void GetFloatArrayRegion(JNIEnv* env, jfloatArray array, jsize start, jsize len, jfloat* buf);
void GetDoubleArrayRegion(JNIEnv* env, jdoubleArray array, jsize start, jsize len, jdouble* buf);
void SetBooleanArrayRegion(JNIEnv* env, jbooleanArray array, jsize start, jsize l, const jboolean* buf);
void SetByteArrayRegion(JNIEnv* env, jbyteArray array, jsize start, jsize len, const jbyte* buf);
void SetCharArrayRegion(JNIEnv* env, jcharArray array, jsize start, jsize len, const jchar* buf);
void SetShortArrayRegion(JNIEnv* env, jshortArray array, jsize start, jsize len, const jshort* buf);
void SetIntArrayRegion(JNIEnv* env, jintArray array, jsize start, jsize len, const jint* buf);
void SetLongArrayRegion(JNIEnv* env, jlongArray array, jsize start, jsize len, const jlong* buf);
void SetFloatArrayRegion(JNIEnv* env, jfloatArray array, jsize start, jsize len, const jfloat* buf);
void SetDoubleArrayRegion(JNIEnv* env, jdoubleArray array, jsize start, jsize len, const jdouble* buf);
jint RegisterNatives(JNIEnv* env, jclass clazz, const JNINativeMethod* methods, jint nMethods);
jint UnregisterNatives(JNIEnv* env, jclass clazz);
jint MonitorEnter(JNIEnv* env, jobject obj);
jint MonitorExit(JNIEnv* env, jobject obj);
jint GetJavaVM(JNIEnv* env, JavaVM** vm);
void GetStringRegion(JNIEnv* env, jstring str, jsize start, jsize len, jchar* buf);
void GetStringUTFRegion(JNIEnv* env, jstring str, jsize start, jsize len, char* buf);
void* GetPrimitiveArrayCritical(JNIEnv* env, jarray array, jboolean* isCopy);
void ReleasePrimitiveArrayCritical(JNIEnv* env, jarray array, void* carray, jint mode);
const jchar* GetStringCritical(JNIEnv* env, jstring string, jboolean* isCopy);
void ReleaseStringCritical(JNIEnv* env, jstring string, const jchar* cstring);
jweak NewWeakGlobalRef(JNIEnv* env, jobject obj);
void DeleteWeakGlobalRef(JNIEnv* env, jweak ref);
jboolean ExceptionCheck(JNIEnv* env);
jobject NewDirectByteBuffer(JNIEnv* env, void* address, jlong capacity);
void* GetDirectBufferAddress(JNIEnv* env, jobject buf);
jlong GetDirectBufferCapacity(JNIEnv* env, jobject buf);
jobjectRefType GetObjectRefType(JNIEnv* env, jobject obj);
jobject GetModule(JNIEnv* env, jclass clazz);
jboolean IsVirtualThread(JNIEnv* env, jobject obj);
} // namespace jvmilia

struct JNINativeInterface {
  void* reserved0;
  void* reserved1;
  void* reserved2;

  void* reserved3;
  decltype(jvmilia::GetVersion)* GetVersion;

  decltype(jvmilia::DefineClass)* DefineClass;
  decltype(jvmilia::FindClass)* FindClass;

  decltype(jvmilia::FromReflectedMethod)* FromReflectedMethod;
  decltype(jvmilia::FromReflectedField)* FromReflectedField;

  decltype(jvmilia::ToReflectedMethod)* ToReflectedMethod;

  decltype(jvmilia::GetSuperclass)* GetSuperclass;
  decltype(jvmilia::IsAssignableFrom)* IsAssignableFrom;

  decltype(jvmilia::ToReflectedField)* ToReflectedField;

  decltype(jvmilia::Throw)* Throw;
  decltype(jvmilia::ThrowNew)* ThrowNew;
  decltype(jvmilia::ExceptionOccurred)* ExceptionOccurred;
  decltype(jvmilia::ExceptionDescribe)* ExceptionDescribe;
  decltype(jvmilia::ExceptionClear)* ExceptionClear;
  decltype(jvmilia::FatalError)* FatalError;

  decltype(jvmilia::PushLocalFrame)* PushLocalFrame;
  decltype(jvmilia::PopLocalFrame)* PopLocalFrame;

  decltype(jvmilia::NewGlobalRef)* NewGlobalRef;
  decltype(jvmilia::DeleteGlobalRef)* DeleteGlobalRef;
  decltype(jvmilia::DeleteLocalRef)* DeleteLocalRef;
  decltype(jvmilia::IsSameObject)* IsSameObject;
  decltype(jvmilia::NewLocalRef)* NewLocalRef;
  decltype(jvmilia::EnsureLocalCapacity)* EnsureLocalCapacity;

  decltype(jvmilia::AllocObject)* AllocObject;
  decltype(jvmilia::NewObject)* NewObject;
  decltype(jvmilia::NewObjectV)* NewObjectV;
  decltype(jvmilia::NewObjectA)* NewObjectA;

  decltype(jvmilia::GetObjectClass)* GetObjectClass;
  decltype(jvmilia::IsInstanceOf)* IsInstanceOf;

  decltype(jvmilia::GetMethodID)* GetMethodID;

  decltype(jvmilia::CallObjectMethod)* CallObjectMethod;
  decltype(jvmilia::CallObjectMethodV)* CallObjectMethodV;
  decltype(jvmilia::CallObjectMethodA)* CallObjectMethodA;

  decltype(jvmilia::CallBooleanMethod)* CallBooleanMethod;
  decltype(jvmilia::CallBooleanMethodV)* CallBooleanMethodV;
  decltype(jvmilia::CallBooleanMethodA)* CallBooleanMethodA;

  decltype(jvmilia::CallByteMethod)* CallByteMethod;
  decltype(jvmilia::CallByteMethodV)* CallByteMethodV;
  decltype(jvmilia::CallByteMethodA)* CallByteMethodA;

  decltype(jvmilia::CallCharMethod)* CallCharMethod;
  decltype(jvmilia::CallCharMethodV)* CallCharMethodV;
  decltype(jvmilia::CallCharMethodA)* CallCharMethodA;

  decltype(jvmilia::CallShortMethod)* CallShortMethod;
  decltype(jvmilia::CallShortMethodV)* CallShortMethodV;
  decltype(jvmilia::CallShortMethodA)* CallShortMethodA;

  decltype(jvmilia::CallIntMethod)* CallIntMethod;
  decltype(jvmilia::CallIntMethodV)* CallIntMethodV;
  decltype(jvmilia::CallIntMethodA)* CallIntMethodA;

  decltype(jvmilia::CallLongMethod)* CallLongMethod;
  decltype(jvmilia::CallLongMethodV)* CallLongMethodV;
  decltype(jvmilia::CallLongMethodA)* CallLongMethodA;

  decltype(jvmilia::CallFloatMethod)* CallFloatMethod;
  decltype(jvmilia::CallFloatMethodV)* CallFloatMethodV;
  decltype(jvmilia::CallFloatMethodA)* CallFloatMethodA;

  decltype(jvmilia::CallDoubleMethod)* CallDoubleMethod;
  decltype(jvmilia::CallDoubleMethodV)* CallDoubleMethodV;
  decltype(jvmilia::CallDoubleMethodA)* CallDoubleMethodA;

  decltype(jvmilia::CallVoidMethod)* CallVoidMethod;
  decltype(jvmilia::CallVoidMethodV)* CallVoidMethodV;
  decltype(jvmilia::CallVoidMethodA)* CallVoidMethodA;

  decltype(jvmilia::CallNonvirtualObjectMethod)* CallNonvirtualObjectMethod;
  decltype(jvmilia::CallNonvirtualObjectMethodV)* CallNonvirtualObjectMethodV;
  decltype(jvmilia::CallNonvirtualObjectMethodA)* CallNonvirtualObjectMethodA;

  decltype(jvmilia::CallNonvirtualBooleanMethod)* CallNonvirtualBooleanMethod;
  decltype(jvmilia::CallNonvirtualBooleanMethodV)* CallNonvirtualBooleanMethodV;
  decltype(jvmilia::CallNonvirtualBooleanMethodA)* CallNonvirtualBooleanMethodA;

  decltype(jvmilia::CallNonvirtualByteMethod)* CallNonvirtualByteMethod;
  decltype(jvmilia::CallNonvirtualByteMethodV)* CallNonvirtualByteMethodV;
  decltype(jvmilia::CallNonvirtualByteMethodA)* CallNonvirtualByteMethodA;

  decltype(jvmilia::CallNonvirtualCharMethod)* CallNonvirtualCharMethod;
  decltype(jvmilia::CallNonvirtualCharMethodV)* CallNonvirtualCharMethodV;
  decltype(jvmilia::CallNonvirtualCharMethodA)* CallNonvirtualCharMethodA;

  decltype(jvmilia::CallNonvirtualShortMethod)* CallNonvirtualShortMethod;
  decltype(jvmilia::CallNonvirtualShortMethodV)* CallNonvirtualShortMethodV;
  decltype(jvmilia::CallNonvirtualShortMethodA)* CallNonvirtualShortMethodA;

  decltype(jvmilia::CallNonvirtualIntMethod)* CallNonvirtualIntMethod;
  decltype(jvmilia::CallNonvirtualIntMethodV)* CallNonvirtualIntMethodV;
  decltype(jvmilia::CallNonvirtualIntMethodA)* CallNonvirtualIntMethodA;

  decltype(jvmilia::CallNonvirtualLongMethod)* CallNonvirtualLongMethod;
  decltype(jvmilia::CallNonvirtualLongMethodV)* CallNonvirtualLongMethodV;
  decltype(jvmilia::CallNonvirtualLongMethodA)* CallNonvirtualLongMethodA;

  decltype(jvmilia::CallNonvirtualFloatMethod)* CallNonvirtualFloatMethod;
  decltype(jvmilia::CallNonvirtualFloatMethodV)* CallNonvirtualFloatMethodV;
  decltype(jvmilia::CallNonvirtualFloatMethodA)* CallNonvirtualFloatMethodA;

  decltype(jvmilia::CallNonvirtualDoubleMethod)* CallNonvirtualDoubleMethod;
  decltype(jvmilia::CallNonvirtualDoubleMethodV)* CallNonvirtualDoubleMethodV;
  decltype(jvmilia::CallNonvirtualDoubleMethodA)* CallNonvirtualDoubleMethodA;

  decltype(jvmilia::CallNonvirtualVoidMethod)* CallNonvirtualVoidMethod;
  decltype(jvmilia::CallNonvirtualVoidMethodV)* CallNonvirtualVoidMethodV;
  decltype(jvmilia::CallNonvirtualVoidMethodA)* CallNonvirtualVoidMethodA;

  decltype(jvmilia::GetFieldID)* GetFieldID;

  decltype(jvmilia::GetObjectField)* GetObjectField;
  decltype(jvmilia::GetBooleanField)* GetBooleanField;
  decltype(jvmilia::GetByteField)* GetByteField;
  decltype(jvmilia::GetCharField)* GetCharField;
  decltype(jvmilia::GetShortField)* GetShortField;
  decltype(jvmilia::GetIntField)* GetIntField;
  decltype(jvmilia::GetLongField)* GetLongField;
  decltype(jvmilia::GetFloatField)* GetFloatField;
  decltype(jvmilia::GetDoubleField)* GetDoubleField;

  decltype(jvmilia::SetObjectField)* SetObjectField;
  decltype(jvmilia::SetBooleanField)* SetBooleanField;
  decltype(jvmilia::SetByteField)* SetByteField;
  decltype(jvmilia::SetCharField)* SetCharField;
  decltype(jvmilia::SetShortField)* SetShortField;
  decltype(jvmilia::SetIntField)* SetIntField;
  decltype(jvmilia::SetLongField)* SetLongField;
  decltype(jvmilia::SetFloatField)* SetFloatField;
  decltype(jvmilia::SetDoubleField)* SetDoubleField;

  decltype(jvmilia::GetStaticMethodID)* GetStaticMethodID;

  decltype(jvmilia::CallStaticObjectMethod)* CallStaticObjectMethod;
  decltype(jvmilia::CallStaticObjectMethodV)* CallStaticObjectMethodV;
  decltype(jvmilia::CallStaticObjectMethodA)* CallStaticObjectMethodA;

  decltype(jvmilia::CallStaticBooleanMethod)* CallStaticBooleanMethod;
  decltype(jvmilia::CallStaticBooleanMethodV)* CallStaticBooleanMethodV;
  decltype(jvmilia::CallStaticBooleanMethodA)* CallStaticBooleanMethodA;

  decltype(jvmilia::CallStaticByteMethod)* CallStaticByteMethod;
  decltype(jvmilia::CallStaticByteMethodV)* CallStaticByteMethodV;
  decltype(jvmilia::CallStaticByteMethodA)* CallStaticByteMethodA;

  decltype(jvmilia::CallStaticCharMethod)* CallStaticCharMethod;
  decltype(jvmilia::CallStaticCharMethodV)* CallStaticCharMethodV;
  decltype(jvmilia::CallStaticCharMethodA)* CallStaticCharMethodA;

  decltype(jvmilia::CallStaticShortMethod)* CallStaticShortMethod;
  decltype(jvmilia::CallStaticShortMethodV)* CallStaticShortMethodV;
  decltype(jvmilia::CallStaticShortMethodA)* CallStaticShortMethodA;

  decltype(jvmilia::CallStaticIntMethod)* CallStaticIntMethod;
  decltype(jvmilia::CallStaticIntMethodV)* CallStaticIntMethodV;
  decltype(jvmilia::CallStaticIntMethodA)* CallStaticIntMethodA;

  decltype(jvmilia::CallStaticLongMethod)* CallStaticLongMethod;
  decltype(jvmilia::CallStaticLongMethodV)* CallStaticLongMethodV;
  decltype(jvmilia::CallStaticLongMethodA)* CallStaticLongMethodA;

  decltype(jvmilia::CallStaticFloatMethod)* CallStaticFloatMethod;
  decltype(jvmilia::CallStaticFloatMethodV)* CallStaticFloatMethodV;
  decltype(jvmilia::CallStaticFloatMethodA)* CallStaticFloatMethodA;

  decltype(jvmilia::CallStaticDoubleMethod)* CallStaticDoubleMethod;
  decltype(jvmilia::CallStaticDoubleMethodV)* CallStaticDoubleMethodV;
  decltype(jvmilia::CallStaticDoubleMethodA)* CallStaticDoubleMethodA;

  decltype(jvmilia::CallStaticVoidMethod)* CallStaticVoidMethod;
  decltype(jvmilia::CallStaticVoidMethodV)* CallStaticVoidMethodV;
  decltype(jvmilia::CallStaticVoidMethodA)* CallStaticVoidMethodA;

  decltype(jvmilia::GetStaticFieldID)* GetStaticFieldID;
  decltype(jvmilia::GetStaticObjectField)* GetStaticObjectField;
  decltype(jvmilia::GetStaticBooleanField)* GetStaticBooleanField;
  decltype(jvmilia::GetStaticByteField)* GetStaticByteField;
  decltype(jvmilia::GetStaticCharField)* GetStaticCharField;
  decltype(jvmilia::GetStaticShortField)* GetStaticShortField;
  decltype(jvmilia::GetStaticIntField)* GetStaticIntField;
  decltype(jvmilia::GetStaticLongField)* GetStaticLongField;
  decltype(jvmilia::GetStaticFloatField)* GetStaticFloatField;
  decltype(jvmilia::GetStaticDoubleField)* GetStaticDoubleField;

  decltype(jvmilia::SetStaticObjectField)* SetStaticObjectField;
  decltype(jvmilia::SetStaticBooleanField)* SetStaticBooleanField;
  decltype(jvmilia::SetStaticByteField)* SetStaticByteField;
  decltype(jvmilia::SetStaticCharField)* SetStaticCharField;
  decltype(jvmilia::SetStaticShortField)* SetStaticShortField;
  decltype(jvmilia::SetStaticIntField)* SetStaticIntField;
  decltype(jvmilia::SetStaticLongField)* SetStaticLongField;
  decltype(jvmilia::SetStaticFloatField)* SetStaticFloatField;
  decltype(jvmilia::SetStaticDoubleField)* SetStaticDoubleField;

  decltype(jvmilia::NewString)* NewString;
  decltype(jvmilia::GetStringLength)* GetStringLength;
  decltype(jvmilia::GetStringChars)* GetStringChars;
  decltype(jvmilia::ReleaseStringChars)* ReleaseStringChars;

  decltype(jvmilia::NewStringUTF)* NewStringUTF;
  decltype(jvmilia::GetStringUTFLength)* GetStringUTFLength;
  decltype(jvmilia::GetStringUTFChars)* GetStringUTFChars;
  decltype(jvmilia::ReleaseStringUTFChars)* ReleaseStringUTFChars;

  decltype(jvmilia::GetArrayLength)* GetArrayLength;

  decltype(jvmilia::NewObjectArray)* NewObjectArray;
  decltype(jvmilia::GetObjectArrayElement)* GetObjectArrayElement;
  decltype(jvmilia::SetObjectArrayElement)* SetObjectArrayElement;

  decltype(jvmilia::NewBooleanArray)* NewBooleanArray;
  decltype(jvmilia::NewByteArray)* NewByteArray;
  decltype(jvmilia::NewCharArray)* NewCharArray;
  decltype(jvmilia::NewShortArray)* NewShortArray;
  decltype(jvmilia::NewIntArray)* NewIntArray;
  decltype(jvmilia::NewLongArray)* NewLongArray;
  decltype(jvmilia::NewFloatArray)* NewFloatArray;
  decltype(jvmilia::NewDoubleArray)* NewDoubleArray;

  decltype(jvmilia::GetBooleanArrayElements)* GetBooleanArrayElements;
  decltype(jvmilia::GetByteArrayElements)* GetByteArrayElements;
  decltype(jvmilia::GetCharArrayElements)* GetCharArrayElements;
  decltype(jvmilia::GetShortArrayElements)* GetShortArrayElements;
  decltype(jvmilia::GetIntArrayElements)* GetIntArrayElements;
  decltype(jvmilia::GetLongArrayElements)* GetLongArrayElements;
  decltype(jvmilia::GetFloatArrayElements)* GetFloatArrayElements;
  decltype(jvmilia::GetDoubleArrayElements)* GetDoubleArrayElements;

  decltype(jvmilia::ReleaseBooleanArrayElements)* ReleaseBooleanArrayElements;
  decltype(jvmilia::ReleaseByteArrayElements)* ReleaseByteArrayElements;
  decltype(jvmilia::ReleaseCharArrayElements)* ReleaseCharArrayElements;
  decltype(jvmilia::ReleaseShortArrayElements)* ReleaseShortArrayElements;
  decltype(jvmilia::ReleaseIntArrayElements)* ReleaseIntArrayElements;
  decltype(jvmilia::ReleaseLongArrayElements)* ReleaseLongArrayElements;
  decltype(jvmilia::ReleaseFloatArrayElements)* ReleaseFloatArrayElements;
  decltype(jvmilia::ReleaseDoubleArrayElements)* ReleaseDoubleArrayElements;

  decltype(jvmilia::GetBooleanArrayRegion)* GetBooleanArrayRegion;
  decltype(jvmilia::GetByteArrayRegion)* GetByteArrayRegion;
  decltype(jvmilia::GetCharArrayRegion)* GetCharArrayRegion;
  decltype(jvmilia::GetShortArrayRegion)* GetShortArrayRegion;
  decltype(jvmilia::GetIntArrayRegion)* GetIntArrayRegion;
  decltype(jvmilia::GetLongArrayRegion)* GetLongArrayRegion;
  decltype(jvmilia::GetFloatArrayRegion)* GetFloatArrayRegion;
  decltype(jvmilia::GetDoubleArrayRegion)* GetDoubleArrayRegion;

  decltype(jvmilia::SetBooleanArrayRegion)* SetBooleanArrayRegion;
  decltype(jvmilia::SetByteArrayRegion)* SetByteArrayRegion;
  decltype(jvmilia::SetCharArrayRegion)* SetCharArrayRegion;
  decltype(jvmilia::SetShortArrayRegion)* SetShortArrayRegion;
  decltype(jvmilia::SetIntArrayRegion)* SetIntArrayRegion;
  decltype(jvmilia::SetLongArrayRegion)* SetLongArrayRegion;
  decltype(jvmilia::SetFloatArrayRegion)* SetFloatArrayRegion;
  decltype(jvmilia::SetDoubleArrayRegion)* SetDoubleArrayRegion;

  decltype(jvmilia::RegisterNatives)* RegisterNatives;
  decltype(jvmilia::UnregisterNatives)* UnregisterNatives;

  decltype(jvmilia::MonitorEnter)* MonitorEnter;
  decltype(jvmilia::MonitorExit)* MonitorExit;

  decltype(jvmilia::GetJavaVM)* GetJavaVM;

  decltype(jvmilia::GetStringRegion)* GetStringRegion;
  decltype(jvmilia::GetStringUTFRegion)* GetStringUTFRegion;

  decltype(jvmilia::GetPrimitiveArrayCritical)* GetPrimitiveArrayCritical;
  decltype(jvmilia::ReleasePrimitiveArrayCritical)* ReleasePrimitiveArrayCritical;

  decltype(jvmilia::GetStringCritical)* GetStringCritical;
  decltype(jvmilia::ReleaseStringCritical)* ReleaseStringCritical;

  decltype(jvmilia::NewWeakGlobalRef)* NewWeakGlobalRef;
  decltype(jvmilia::DeleteWeakGlobalRef)* DeleteWeakGlobalRef;

  decltype(jvmilia::ExceptionCheck)* ExceptionCheck;

  decltype(jvmilia::NewDirectByteBuffer)* NewDirectByteBuffer;
  decltype(jvmilia::GetDirectBufferAddress)* GetDirectBufferAddress;
  decltype(jvmilia::GetDirectBufferCapacity)* GetDirectBufferCapacity;

  /* New JNI 1.6 Features */

  decltype(jvmilia::GetObjectRefType)* GetObjectRefType;

  /* Module Features */

  decltype(jvmilia::GetModule)* GetModule;

  /* Virtual threads */

  decltype(jvmilia::IsVirtualThread)* IsVirtualThread;
};
}
