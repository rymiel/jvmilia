#include "../../exec/jni.h"
#include "../../exec/jvm.h"
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/memory.h"
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>

[[noreturn]] void unimplemented(const char* methodName) {
  printf("libjvm: Unimplemented libjvm method %s\n", methodName);
  std::exit(1);
}

extern "C" {
jboolean JVM_DesiredAssertionStatus(JNIEnv* env, jclass unused, jclass cls) {
  jvmilia::JVMData* data = jvmilia::getData(env);
  auto* unused_name = jvmilia::class_name(data, unused);
  auto* cls_name = jvmilia::class_name(data, cls);
  std::printf("libjvm: JVM_DesiredAssertionStatus: %s, %s\n", unused_name, cls_name);

  return 0;
}

jobjectArray JVM_GetProperties(JNIEnv* env) {
  std::printf("libjvm: JVM_GetProperties\n");
  jclass string = jvmilia::FindClass(env, "java/lang/String");
  return jvmilia::NewObjectArray(env, 1, string, nullptr);
}

jclass JVM_FindPrimitiveClass(JNIEnv* env, const char* utf) {
  CAMLparam0();
  CAMLlocal2(n, result);
  jvmilia::JVMData* data = jvmilia::getData(env);
  printf("libjvm: JVM_FindPrimitiveClass: %s\n", utf);
  if (strcmp(utf, "float") == 0) {
    n = caml_copy_string("/float");
    result = caml_callback(data->make_class_direct_callback, n);
    auto ref = data->make_local_reference(result);
    printf("jni: JVM_FindPrimitiveClass %s -> %lx (%p)\n", utf, result, ref.get());
    CAMLreturnT(jclass, std::bit_cast<jclass>(ref.get()));
  } else {
    printf("libjvm: JVM_FindPrimitiveClass: unimplemented primitive class %s\n", utf);
    assert(false);
  }
  unimplemented("JVM_FindPrimitiveClass");
  CAMLreturnT(jclass, nullptr); // unreachable
}

void JVM_ActiveProcessorCount() { unimplemented("JVM_ActiveProcessorCount"); }
void JVM_AddModuleExports() { unimplemented("JVM_AddModuleExports"); }
void JVM_AddModuleExportsToAll() { unimplemented("JVM_AddModuleExportsToAll"); }
void JVM_AddModuleExportsToAllUnnamed() { unimplemented("JVM_AddModuleExportsToAllUnnamed"); }
void JVM_AddReadsModule() { unimplemented("JVM_AddReadsModule"); }
void JVM_AreNestMates() { unimplemented("JVM_AreNestMates"); }
void JVM_ArrayCopy() { unimplemented("JVM_ArrayCopy"); }
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
void JVM_CurrentThread() { unimplemented("JVM_CurrentThread"); }
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
void JVM_FindSignal() { unimplemented("JVM_FindSignal"); }
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
void JVM_GetNextThreadIdOffset() { unimplemented("JVM_GetNextThreadIdOffset"); }
void JVM_GetPermittedSubclasses() { unimplemented("JVM_GetPermittedSubclasses"); }
void JVM_GetPrimitiveArrayElement() { unimplemented("JVM_GetPrimitiveArrayElement"); }
void JVM_GetProtectionDomain() { unimplemented("JVM_GetProtectionDomain"); }
void JVM_GetRandomSeedForDumping() { unimplemented("JVM_GetRandomSeedForDumping"); }
void JVM_GetRecordComponents() { unimplemented("JVM_GetRecordComponents"); }
void JVM_GetSimpleBinaryName() { unimplemented("JVM_GetSimpleBinaryName"); }
void JVM_GetStackAccessControlContext() { unimplemented("JVM_GetStackAccessControlContext"); }
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
void JVM_InitializeFromArchive() { unimplemented("JVM_InitializeFromArchive"); }
void JVM_InitStackTraceElement() { unimplemented("JVM_InitStackTraceElement"); }
void JVM_InitStackTraceElementArray() { unimplemented("JVM_InitStackTraceElementArray"); }
void JVM_InternString() { unimplemented("JVM_InternString"); }
void JVM_Interrupt() { unimplemented("JVM_Interrupt"); }
void JVM_InvokeMethod() { unimplemented("JVM_InvokeMethod"); }
void JVM_IsArrayClass() { unimplemented("JVM_IsArrayClass"); }
void JVM_IsCDSDumpingEnabled() { unimplemented("JVM_IsCDSDumpingEnabled"); }
void JVM_IsContinuationsSupported() { unimplemented("JVM_IsContinuationsSupported"); }
void JVM_IsDumpingClassList() { unimplemented("JVM_IsDumpingClassList"); }
void JVM_IsFinalizationEnabled() { unimplemented("JVM_IsFinalizationEnabled"); }
void JVM_IsForeignLinkerSupported() { unimplemented("JVM_IsForeignLinkerSupported"); }
void JVM_IsHiddenClass() { unimplemented("JVM_IsHiddenClass"); }
void JVM_IsInterface() { unimplemented("JVM_IsInterface"); }
void JVM_IsPreviewEnabled() { unimplemented("JVM_IsPreviewEnabled"); }
void JVM_IsPrimitiveClass() { unimplemented("JVM_IsPrimitiveClass"); }
void JVM_IsRecord() { unimplemented("JVM_IsRecord"); }
void JVM_IsSharingEnabled() { unimplemented("JVM_IsSharingEnabled"); }
void JVM_IsSupportedJNIVersion() { unimplemented("JVM_IsSupportedJNIVersion"); }
void JVM_IsUseContainerSupport() { unimplemented("JVM_IsUseContainerSupport"); }
void JVM_LatestUserDefinedLoader() { unimplemented("JVM_LatestUserDefinedLoader"); }
void JVM_LoadLibrary() { unimplemented("JVM_LoadLibrary"); }
void JVM_LogLambdaFormInvoker() { unimplemented("JVM_LogLambdaFormInvoker"); }
void JVM_LookupDefineClass() { unimplemented("JVM_LookupDefineClass"); }
void JVM_LookupLambdaProxyClassFromArchive() { unimplemented("JVM_LookupLambdaProxyClassFromArchive"); }
void JVM_MaxMemory() { unimplemented("JVM_MaxMemory"); }
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
void JVM_RegisterSignal() { unimplemented("JVM_RegisterSignal"); }
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
void JVM_SetThreadPriority() { unimplemented("JVM_SetThreadPriority"); }
void JVM_SleepNanos() { unimplemented("JVM_SleepNanos"); }
void JVM_StartThread() { unimplemented("JVM_StartThread"); }
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