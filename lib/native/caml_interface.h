#pragma once

#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "jni.h"
#include <cassert>
#include <concepts>
#include <cstdio>
#include <string_view>
#include <vector>

namespace jvmilia {

// native type
enum struct ntype { Nil, Reference, Void, Int, Long, Float, Double };

static constexpr tag_t EVALUE_OBJECT_TAG = 0;
static constexpr tag_t EVALUE_INT_TAG = 1;
static constexpr tag_t EVALUE_ARRAY_TAG = 2;
static constexpr tag_t EVALUE_LONG_TAG = 3;
static constexpr tag_t EVALUE_BYTEARRAY_TAG = 4;
static constexpr tag_t EVALUE_FLOAT_TAG = 5;
static constexpr tag_t EVALUE_DOUBLE_TAG = 6;
static constexpr value EVALUE_NULL = Val_int(1);

bool value_is_cons(value v);

auto list_vector(value list) -> std::vector<value>;

void iter_list(value input, std::invocable<value> auto fn) {
  CAMLparam1(input);
  CAMLlocal3(list, a, b);

  list = input;

  while (true) {
    if (list == Val_emptylist) {
      CAMLreturn0;
    } else {
      assert(value_is_cons(list));
      a = Field(list, 0);
      b = Field(list, 1);
      fn(a);
      list = b;
    }
  }

  CAMLreturn0;
}

template <std::invocable<value> Fn>
auto list_vector_map(value list, Fn fn) -> std::vector<std::invoke_result_t<Fn, value>> {
  CAMLparam1(list);

  auto vec = std::vector<std::invoke_result_t<Fn, value>>{};

  iter_list(list, [&vec, &fn](value v) { vec.push_back(fn(v)); });

  CAMLreturnT(auto&, vec);
}

void dump_value(value input, int max_depth = 100, std::vector<int> depth = {});

auto evalue_is_object(value evalue) -> bool;

auto evalue_conversion(value* v) -> jvalue;

auto reconstruct_evalue(jvalue j, ntype ty) -> value;

auto ntype_string(ntype v) -> std::string_view;
void ntype_c_type(ntype ty, std::ostream& os);
void ntype_c_active_union(ntype ty, std::ostream& os);
auto ntype_of_dtype(value v) -> ntype;

auto coerce_null(jobject v) -> value;
auto coerce_null(value v, struct JVMData* data) -> jobject;

} // namespace jvmilia