#pragma once

#include "jni.h"
#include <filesystem>
#include <string>
#include <unordered_map>
#include <vector>

extern "C" struct JNINativeInterface;
namespace jvmilia {
using bridge_t = jvalue(void*, JNIEnv*, std::vector<jvalue>);

struct JVMData {
  std::unordered_map<std::string, void*> registeredNatives;
  std::unordered_map<std::string, bridge_t*> cachedBridges;
  std::filesystem::path temp;
};

struct Context {
  const JNINativeInterface* interface;
  JVMData* data;
};

inline std::string registerKey(const char* className, const char* methodName, const char* signature) {
  return std::string(className) + ";" + methodName + ";" + signature;
}
} // namespace jvmilia