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

enum struct vtype { Nil, Class, Void, Int, Array };

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

auto evalue_is_class(value evalue) -> bool;

auto evalue_class_name(value evalue) -> const char*;

auto eclass_name(value evalue) -> const char*;

auto evalue_conversion(value v) -> jvalue;

auto reconstruct_evalue(jvalue j, vtype ty) -> value;

auto vtype_string(vtype v) -> std::string_view;
void vtype_c_type(vtype ty, std::ostream& os);
void vtype_c_active_union(vtype ty, std::ostream& os);

auto vtype_conversion(value v) -> vtype;

} // namespace jvmilia