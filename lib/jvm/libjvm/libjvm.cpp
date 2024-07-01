#include "../../exec/jni.hpp"
#include <bit>
#include <cstdio>
#include <cstdlib>

extern "C" {

jboolean JVM_DesiredAssertionStatus(JNIEnv*, jclass unused, jclass cls) {
  auto* unused_name = std::bit_cast<const char*>(unused);
  auto* cls_name = std::bit_cast<const char*>(cls);
  std::printf("libjvm: JVM_DesiredAssertionStatus: %s, %s\n", unused_name, cls_name);
  //   std::exit(1);

  return 0;
}

void JVM_ActiveProcessorCount() {
  std::puts("libjvm: JVM_ActiveProcessorCount");
  std::exit(1);
}
void JVM_AddModuleExports() {
  std::puts("libjvm: JVM_AddModuleExports");
  std::exit(1);
}
void JVM_AddModuleExportsToAll() {
  std::puts("libjvm: JVM_AddModuleExportsToAll");
  std::exit(1);
}
void JVM_AddModuleExportsToAllUnnamed() {
  std::puts("libjvm: JVM_AddModuleExportsToAllUnnamed");
  std::exit(1);
}
void JVM_AddReadsModule() {
  std::puts("libjvm: JVM_AddReadsModule");
  std::exit(1);
}
void JVM_AreNestMates() {
  std::puts("libjvm: JVM_AreNestMates");
  std::exit(1);
}
void JVM_ArrayCopy() {
  std::puts("libjvm: JVM_ArrayCopy");
  std::exit(1);
}
void JVM_AssertionStatusDirectives() {
  std::puts("libjvm: JVM_AssertionStatusDirectives");
  std::exit(1);
}
void JVM_BeforeHalt() {
  std::puts("libjvm: JVM_BeforeHalt");
  std::exit(1);
}
void JVM_CallStackWalk() {
  std::puts("libjvm: JVM_CallStackWalk");
  std::exit(1);
}
void JVM_ConstantPoolGetClassAt() {
  std::puts("libjvm: JVM_ConstantPoolGetClassAt");
  std::exit(1);
}
void JVM_ConstantPoolGetClassAtIfLoaded() {
  std::puts("libjvm: JVM_ConstantPoolGetClassAtIfLoaded");
  std::exit(1);
}
void JVM_ConstantPoolGetClassRefIndexAt() {
  std::puts("libjvm: JVM_ConstantPoolGetClassRefIndexAt");
  std::exit(1);
}
void JVM_ConstantPoolGetDoubleAt() {
  std::puts("libjvm: JVM_ConstantPoolGetDoubleAt");
  std::exit(1);
}
void JVM_ConstantPoolGetFieldAt() {
  std::puts("libjvm: JVM_ConstantPoolGetFieldAt");
  std::exit(1);
}
void JVM_ConstantPoolGetFieldAtIfLoaded() {
  std::puts("libjvm: JVM_ConstantPoolGetFieldAtIfLoaded");
  std::exit(1);
}
void JVM_ConstantPoolGetFloatAt() {
  std::puts("libjvm: JVM_ConstantPoolGetFloatAt");
  std::exit(1);
}
void JVM_ConstantPoolGetIntAt() {
  std::puts("libjvm: JVM_ConstantPoolGetIntAt");
  std::exit(1);
}
void JVM_ConstantPoolGetLongAt() {
  std::puts("libjvm: JVM_ConstantPoolGetLongAt");
  std::exit(1);
}
void JVM_ConstantPoolGetMemberRefInfoAt() {
  std::puts("libjvm: JVM_ConstantPoolGetMemberRefInfoAt");
  std::exit(1);
}
void JVM_ConstantPoolGetMethodAt() {
  std::puts("libjvm: JVM_ConstantPoolGetMethodAt");
  std::exit(1);
}
void JVM_ConstantPoolGetMethodAtIfLoaded() {
  std::puts("libjvm: JVM_ConstantPoolGetMethodAtIfLoaded");
  std::exit(1);
}
void JVM_ConstantPoolGetNameAndTypeRefIndexAt() {
  std::puts("libjvm: JVM_ConstantPoolGetNameAndTypeRefIndexAt");
  std::exit(1);
}
void JVM_ConstantPoolGetNameAndTypeRefInfoAt() {
  std::puts("libjvm: JVM_ConstantPoolGetNameAndTypeRefInfoAt");
  std::exit(1);
}
void JVM_ConstantPoolGetSize() {
  std::puts("libjvm: JVM_ConstantPoolGetSize");
  std::exit(1);
}
void JVM_ConstantPoolGetStringAt() {
  std::puts("libjvm: JVM_ConstantPoolGetStringAt");
  std::exit(1);
}
void JVM_ConstantPoolGetTagAt() {
  std::puts("libjvm: JVM_ConstantPoolGetTagAt");
  std::exit(1);
}
void JVM_ConstantPoolGetUTF8At() {
  std::puts("libjvm: JVM_ConstantPoolGetUTF8At");
  std::exit(1);
}
void JVM_CurrentCarrierThread() {
  std::puts("libjvm: JVM_CurrentCarrierThread");
  std::exit(1);
}
void JVM_CurrentThread() {
  std::puts("libjvm: JVM_CurrentThread");
  std::exit(1);
}
void JVM_CurrentTimeMillis() {
  std::puts("libjvm: JVM_CurrentTimeMillis");
  std::exit(1);
}
void JVM_DefineArchivedModules() {
  std::puts("libjvm: JVM_DefineArchivedModules");
  std::exit(1);
}
void JVM_DefineClassWithSource() {
  std::puts("libjvm: JVM_DefineClassWithSource");
  std::exit(1);
}
void JVM_DefineModule() {
  std::puts("libjvm: JVM_DefineModule");
  std::exit(1);
}
void JVM_DumpClassListToFile() {
  std::puts("libjvm: JVM_DumpClassListToFile");
  std::exit(1);
}
void JVM_DumpDynamicArchive() {
  std::puts("libjvm: JVM_DumpDynamicArchive");
  std::exit(1);
}
void JVM_DumpThreads() {
  std::puts("libjvm: JVM_DumpThreads");
  std::exit(1);
}
void JVM_EnsureMaterializedForStackWalk_func() {
  std::puts("libjvm: JVM_EnsureMaterializedForStackWalk_func");
  std::exit(1);
}
void JVM_ExpandStackFrameInfo() {
  std::puts("libjvm: JVM_ExpandStackFrameInfo");
  std::exit(1);
}
void JVM_FillInStackTrace() {
  std::puts("libjvm: JVM_FillInStackTrace");
  std::exit(1);
}
void JVM_FindClassFromBootLoader() {
  std::puts("libjvm: JVM_FindClassFromBootLoader");
  std::exit(1);
}
void JVM_FindClassFromCaller() {
  std::puts("libjvm: JVM_FindClassFromCaller");
  std::exit(1);
}
void JVM_FindLibraryEntry() {
  std::puts("libjvm: JVM_FindLibraryEntry");
  std::exit(1);
}
void JVM_FindLoadedClass() {
  std::puts("libjvm: JVM_FindLoadedClass");
  std::exit(1);
}
void JVM_FindPrimitiveClass() {
  std::puts("libjvm: JVM_FindPrimitiveClass");
  std::exit(1);
}
void JVM_FindScopedValueBindings() {
  std::puts("libjvm: JVM_FindScopedValueBindings");
  std::exit(1);
}
void JVM_FindSignal() {
  std::puts("libjvm: JVM_FindSignal");
  std::exit(1);
}
void JVM_FreeMemory() {
  std::puts("libjvm: JVM_FreeMemory");
  std::exit(1);
}
void JVM_GC() {
  std::puts("libjvm: JVM_GC");
  std::exit(1);
}
void JVM_GetAllThreads() {
  std::puts("libjvm: JVM_GetAllThreads");
  std::exit(1);
}
void JVM_GetAndClearReferencePendingList() {
  std::puts("libjvm: JVM_GetAndClearReferencePendingList");
  std::exit(1);
}
void JVM_GetArrayElement() {
  std::puts("libjvm: JVM_GetArrayElement");
  std::exit(1);
}
void JVM_GetArrayLength() {
  std::puts("libjvm: JVM_GetArrayLength");
  std::exit(1);
}
void JVM_GetCallerClass() {
  std::puts("libjvm: JVM_GetCallerClass");
  std::exit(1);
}
void JVM_GetClassAccessFlags() {
  std::puts("libjvm: JVM_GetClassAccessFlags");
  std::exit(1);
}
void JVM_GetClassAnnotations() {
  std::puts("libjvm: JVM_GetClassAnnotations");
  std::exit(1);
}
void JVM_GetClassConstantPool() {
  std::puts("libjvm: JVM_GetClassConstantPool");
  std::exit(1);
}
void JVM_GetClassContext() {
  std::puts("libjvm: JVM_GetClassContext");
  std::exit(1);
}
void JVM_GetClassDeclaredConstructors() {
  std::puts("libjvm: JVM_GetClassDeclaredConstructors");
  std::exit(1);
}
void JVM_GetClassDeclaredFields() {
  std::puts("libjvm: JVM_GetClassDeclaredFields");
  std::exit(1);
}
void JVM_GetClassDeclaredMethods() {
  std::puts("libjvm: JVM_GetClassDeclaredMethods");
  std::exit(1);
}
void JVM_GetClassFileVersion() {
  std::puts("libjvm: JVM_GetClassFileVersion");
  std::exit(1);
}
void JVM_GetClassInterfaces() {
  std::puts("libjvm: JVM_GetClassInterfaces");
  std::exit(1);
}
void JVM_GetClassModifiers() {
  std::puts("libjvm: JVM_GetClassModifiers");
  std::exit(1);
}
void JVM_GetClassSignature() {
  std::puts("libjvm: JVM_GetClassSignature");
  std::exit(1);
}
void JVM_GetClassSigners() {
  std::puts("libjvm: JVM_GetClassSigners");
  std::exit(1);
}
void JVM_GetClassTypeAnnotations() {
  std::puts("libjvm: JVM_GetClassTypeAnnotations");
  std::exit(1);
}
void JVM_GetDeclaredClasses() {
  std::puts("libjvm: JVM_GetDeclaredClasses");
  std::exit(1);
}
void JVM_GetDeclaringClass() {
  std::puts("libjvm: JVM_GetDeclaringClass");
  std::exit(1);
}
void JVM_GetEnclosingMethodInfo() {
  std::puts("libjvm: JVM_GetEnclosingMethodInfo");
  std::exit(1);
}
void JVM_GetExtendedNPEMessage() {
  std::puts("libjvm: JVM_GetExtendedNPEMessage");
  std::exit(1);
}
void JVM_GetFieldTypeAnnotations() {
  std::puts("libjvm: JVM_GetFieldTypeAnnotations");
  std::exit(1);
}
void JVM_GetInheritedAccessControlContext() {
  std::puts("libjvm: JVM_GetInheritedAccessControlContext");
  std::exit(1);
}
void JVM_GetMethodParameters() {
  std::puts("libjvm: JVM_GetMethodParameters");
  std::exit(1);
}
void JVM_GetMethodTypeAnnotations() {
  std::puts("libjvm: JVM_GetMethodTypeAnnotations");
  std::exit(1);
}
void JVM_GetNanoTimeAdjustment() {
  std::puts("libjvm: JVM_GetNanoTimeAdjustment");
  std::exit(1);
}
void JVM_GetNestHost() {
  std::puts("libjvm: JVM_GetNestHost");
  std::exit(1);
}
void JVM_GetNestMembers() {
  std::puts("libjvm: JVM_GetNestMembers");
  std::exit(1);
}
void JVM_GetNextThreadIdOffset() {
  std::puts("libjvm: JVM_GetNextThreadIdOffset");
  std::exit(1);
}
void JVM_GetPermittedSubclasses() {
  std::puts("libjvm: JVM_GetPermittedSubclasses");
  std::exit(1);
}
void JVM_GetPrimitiveArrayElement() {
  std::puts("libjvm: JVM_GetPrimitiveArrayElement");
  std::exit(1);
}
void JVM_GetProperties() {
  std::puts("libjvm: JVM_GetProperties");
  std::exit(1);
}
void JVM_GetProtectionDomain() {
  std::puts("libjvm: JVM_GetProtectionDomain");
  std::exit(1);
}
void JVM_GetRandomSeedForDumping() {
  std::puts("libjvm: JVM_GetRandomSeedForDumping");
  std::exit(1);
}
void JVM_GetRecordComponents() {
  std::puts("libjvm: JVM_GetRecordComponents");
  std::exit(1);
}
void JVM_GetSimpleBinaryName() {
  std::puts("libjvm: JVM_GetSimpleBinaryName");
  std::exit(1);
}
void JVM_GetStackAccessControlContext() {
  std::puts("libjvm: JVM_GetStackAccessControlContext");
  std::exit(1);
}
void JVM_GetStackTrace() {
  std::puts("libjvm: JVM_GetStackTrace");
  std::exit(1);
}
void JVM_GetSystemPackage() {
  std::puts("libjvm: JVM_GetSystemPackage");
  std::exit(1);
}
void JVM_GetSystemPackages() {
  std::puts("libjvm: JVM_GetSystemPackages");
  std::exit(1);
}
void JVM_GetTemporaryDirectory() {
  std::puts("libjvm: JVM_GetTemporaryDirectory");
  std::exit(1);
}
void JVM_GetVmArguments() {
  std::puts("libjvm: JVM_GetVmArguments");
  std::exit(1);
}
void JVM_Halt() {
  std::puts("libjvm: JVM_Halt");
  std::exit(1);
}
void JVM_HasReferencePendingList() {
  std::puts("libjvm: JVM_HasReferencePendingList");
  std::exit(1);
}
void JVM_HoldsLock() {
  std::puts("libjvm: JVM_HoldsLock");
  std::exit(1);
}
void JVM_IHashCode() {
  std::puts("libjvm: JVM_IHashCode");
  std::exit(1);
}
void JVM_InitAgentProperties() {
  std::puts("libjvm: JVM_InitAgentProperties");
  std::exit(1);
}
void JVM_InitClassName() {
  std::puts("libjvm: JVM_InitClassName");
  std::exit(1);
}
void JVM_InitializeFromArchive() {
  std::puts("libjvm: JVM_InitializeFromArchive");
  std::exit(1);
}
void JVM_InitStackTraceElement() {
  std::puts("libjvm: JVM_InitStackTraceElement");
  std::exit(1);
}
void JVM_InitStackTraceElementArray() {
  std::puts("libjvm: JVM_InitStackTraceElementArray");
  std::exit(1);
}
void JVM_InternString() {
  std::puts("libjvm: JVM_InternString");
  std::exit(1);
}
void JVM_Interrupt() {
  std::puts("libjvm: JVM_Interrupt");
  std::exit(1);
}
void JVM_InvokeMethod() {
  std::puts("libjvm: JVM_InvokeMethod");
  std::exit(1);
}
void JVM_IsArrayClass() {
  std::puts("libjvm: JVM_IsArrayClass");
  std::exit(1);
}
void JVM_IsCDSDumpingEnabled() {
  std::puts("libjvm: JVM_IsCDSDumpingEnabled");
  std::exit(1);
}
void JVM_IsContinuationsSupported() {
  std::puts("libjvm: JVM_IsContinuationsSupported");
  std::exit(1);
}
void JVM_IsDumpingClassList() {
  std::puts("libjvm: JVM_IsDumpingClassList");
  std::exit(1);
}
void JVM_IsFinalizationEnabled() {
  std::puts("libjvm: JVM_IsFinalizationEnabled");
  std::exit(1);
}
void JVM_IsForeignLinkerSupported() {
  std::puts("libjvm: JVM_IsForeignLinkerSupported");
  std::exit(1);
}
void JVM_IsHiddenClass() {
  std::puts("libjvm: JVM_IsHiddenClass");
  std::exit(1);
}
void JVM_IsInterface() {
  std::puts("libjvm: JVM_IsInterface");
  std::exit(1);
}
void JVM_IsPreviewEnabled() {
  std::puts("libjvm: JVM_IsPreviewEnabled");
  std::exit(1);
}
void JVM_IsPrimitiveClass() {
  std::puts("libjvm: JVM_IsPrimitiveClass");
  std::exit(1);
}
void JVM_IsRecord() {
  std::puts("libjvm: JVM_IsRecord");
  std::exit(1);
}
void JVM_IsSharingEnabled() {
  std::puts("libjvm: JVM_IsSharingEnabled");
  std::exit(1);
}
void JVM_IsSupportedJNIVersion() {
  std::puts("libjvm: JVM_IsSupportedJNIVersion");
  std::exit(1);
}
void JVM_IsUseContainerSupport() {
  std::puts("libjvm: JVM_IsUseContainerSupport");
  std::exit(1);
}
void JVM_LatestUserDefinedLoader() {
  std::puts("libjvm: JVM_LatestUserDefinedLoader");
  std::exit(1);
}
void JVM_LoadLibrary() {
  std::puts("libjvm: JVM_LoadLibrary");
  std::exit(1);
}
void JVM_LogLambdaFormInvoker() {
  std::puts("libjvm: JVM_LogLambdaFormInvoker");
  std::exit(1);
}
void JVM_LookupDefineClass() {
  std::puts("libjvm: JVM_LookupDefineClass");
  std::exit(1);
}
void JVM_LookupLambdaProxyClassFromArchive() {
  std::puts("libjvm: JVM_LookupLambdaProxyClassFromArchive");
  std::exit(1);
}
void JVM_MaxMemory() {
  std::puts("libjvm: JVM_MaxMemory");
  std::exit(1);
}
void JVM_MoreStackWalk() {
  std::puts("libjvm: JVM_MoreStackWalk");
  std::exit(1);
}
void JVM_NanoTime() {
  std::puts("libjvm: JVM_NanoTime");
  std::exit(1);
}
void JVM_NewArray() {
  std::puts("libjvm: JVM_NewArray");
  std::exit(1);
}
void JVM_NewInstanceFromConstructor() {
  std::puts("libjvm: JVM_NewInstanceFromConstructor");
  std::exit(1);
}
void JVM_NewMultiArray() {
  std::puts("libjvm: JVM_NewMultiArray");
  std::exit(1);
}
void JVM_PhantomReferenceRefersTo() {
  std::puts("libjvm: JVM_PhantomReferenceRefersTo");
  std::exit(1);
}
void JVM_RaiseSignal() {
  std::puts("libjvm: JVM_RaiseSignal");
  std::exit(1);
}
void JVM_ReferenceClear() {
  std::puts("libjvm: JVM_ReferenceClear");
  std::exit(1);
}
void JVM_ReferenceRefersTo() {
  std::puts("libjvm: JVM_ReferenceRefersTo");
  std::exit(1);
}
void JVM_RegisterContinuationMethods() {
  std::puts("libjvm: JVM_RegisterContinuationMethods");
  std::exit(1);
}
void JVM_RegisterLambdaProxyClassForArchiving() {
  std::puts("libjvm: JVM_RegisterLambdaProxyClassForArchiving");
  std::exit(1);
}
void JVM_RegisterSignal() {
  std::puts("libjvm: JVM_RegisterSignal");
  std::exit(1);
}
void JVM_ReportFinalizationComplete() {
  std::puts("libjvm: JVM_ReportFinalizationComplete");
  std::exit(1);
}
void JVM_ScopedValueCache() {
  std::puts("libjvm: JVM_ScopedValueCache");
  std::exit(1);
}
void JVM_SetArrayElement() {
  std::puts("libjvm: JVM_SetArrayElement");
  std::exit(1);
}
void JVM_SetBootLoaderUnnamedModule() {
  std::puts("libjvm: JVM_SetBootLoaderUnnamedModule");
  std::exit(1);
}
void JVM_SetClassSigners() {
  std::puts("libjvm: JVM_SetClassSigners");
  std::exit(1);
}
void JVM_SetCurrentThread() {
  std::puts("libjvm: JVM_SetCurrentThread");
  std::exit(1);
}
void JVM_SetNativeThreadName() {
  std::puts("libjvm: JVM_SetNativeThreadName");
  std::exit(1);
}
void JVM_SetPrimitiveArrayElement() {
  std::puts("libjvm: JVM_SetPrimitiveArrayElement");
  std::exit(1);
}
void JVM_SetScopedValueCache() {
  std::puts("libjvm: JVM_SetScopedValueCache");
  std::exit(1);
}
void JVM_SetStackWalkContinuation() {
  std::puts("libjvm: JVM_SetStackWalkContinuation");
  std::exit(1);
}
void JVM_SetThreadPriority() {
  std::puts("libjvm: JVM_SetThreadPriority");
  std::exit(1);
}
void JVM_SleepNanos() {
  std::puts("libjvm: JVM_SleepNanos");
  std::exit(1);
}
void JVM_StartThread() {
  std::puts("libjvm: JVM_StartThread");
  std::exit(1);
}
void JVM_TotalMemory() {
  std::puts("libjvm: JVM_TotalMemory");
  std::exit(1);
}
void JVM_UnloadLibrary() {
  std::puts("libjvm: JVM_UnloadLibrary");
  std::exit(1);
}
void JVM_VirtualThreadEnd() {
  std::puts("libjvm: JVM_VirtualThreadEnd");
  std::exit(1);
}
void JVM_VirtualThreadHideFrames() {
  std::puts("libjvm: JVM_VirtualThreadHideFrames");
  std::exit(1);
}
void JVM_VirtualThreadMount() {
  std::puts("libjvm: JVM_VirtualThreadMount");
  std::exit(1);
}
void JVM_VirtualThreadStart() {
  std::puts("libjvm: JVM_VirtualThreadStart");
  std::exit(1);
}
void JVM_VirtualThreadUnmount() {
  std::puts("libjvm: JVM_VirtualThreadUnmount");
  std::exit(1);
}
void JVM_WaitForReferencePendingList() {
  std::puts("libjvm: JVM_WaitForReferencePendingList");
  std::exit(1);
}

void jio_vfprintf() {
  std::puts("libjvm: jio_vfprintf");
  std::exit(1);
}

void jio_vsnprintf() {
  std::puts("libjvm: jio_vsnprintf");
  std::exit(1);
}

void JVM_Yield() {
  std::puts("libjvm: JVM_Yield");
  std::exit(1);
}
}