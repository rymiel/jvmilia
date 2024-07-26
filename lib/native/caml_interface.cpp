#include "caml_interface.h"
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/mlvalues.h"
#include "jvm.h"
#include <cstring>
#include <iostream>

namespace jvmilia {
bool value_is_cons(value v) {
  CAMLparam1(v);

  CAMLreturnT(bool, Is_block(v) && Tag_val(v) == 0 && Wosize_val(v) == 2);
}

auto list_vector(value list) -> std::vector<value> {
  CAMLparam1(list);

  auto vec = std::vector<value>{};

  iter_list(list, [&vec](value v) { vec.push_back(v); });

  CAMLreturnT(auto&, vec);
}

void dump_value(value input, int max_depth, std::vector<int> depth) {
  CAMLparam1(input);

  int indent = depth.size();

  for (int i = 0; i < indent; i++) {
    log_printf("\t");
  }

  if (indent > 0) {
    log_printf("[");
    bool flag = false;
    for (int i : depth) {
      if (flag) {
        log_printf(".");
      }
      log_printf("%d", i);
      flag = true;
    }
    log_printf("] ");
  }

  if (Is_block(input)) {
    auto tag = Tag_val(input);
    if (tag == Closure_tag) {
      log_printf("closure (wosize=%lu, p=%p, code=%p)\n", Wosize_val(input), std::bit_cast<void*>(input),
                  Code_val(input));
    } else if (tag < No_scan_tag) {
      auto wosize = Wosize_val(input);
      log_printf("block (tag=%d, wosize=%lu, p=%p)", tag, wosize, std::bit_cast<void*>(input));
      if (max_depth == 0) {
        log_printf(" <...>'n");
        CAMLreturn0;
      } else {
        log_printf("\n");
        for (mlsize_t i = 0; i < wosize; i++) {
          auto d = depth;
          d.push_back(i);
          dump_value(Field(input, i), max_depth - 1, d);
        }
      }
    } else if (tag == String_tag) {
      log_printf("string (%lu) \"%s\"\n", caml_string_length(input), String_val(input));
    } else if (tag == Double_tag) {
      log_printf("double %f\n", Double_val(input));
    } else {
      log_printf("raw block (tag=%d, p=%p)\n", tag, std::bit_cast<void*>(input));
    }
  } else {
    log_printf("int %ld\n", Long_val(input));
  }

  CAMLreturn0;
}

bool evalue_is_object(value evalue) {
  return Is_block(evalue) &&
         (Tag_val(evalue) == EVALUE_OBJECT_TAG || Tag_val(evalue) == EVALUE_ARRAY_TAG ||
          Tag_val(evalue) == EVALUE_BYTEARRAY_TAG) &&
         Wosize_val(evalue) == 1;
}

jvalue evalue_conversion(value* v) {
  jvalue j = {};
  if (Is_block(*v)) {
    switch (Tag_val(*v)) {
    case EVALUE_OBJECT_TAG:
    case EVALUE_ARRAY_TAG:
    case EVALUE_BYTEARRAY_TAG: j.l = std::bit_cast<jobject>(v); break;
    case EVALUE_INT_TAG: j.i = Int32_val(Field(*v, 0)); break;
    case EVALUE_LONG_TAG: j.j = Int64_val(Field(*v, 0)); break;
    case EVALUE_FLOAT_TAG: j.f = static_cast<float>(Double_val(Field(*v, 0))); break;
    case EVALUE_DOUBLE_TAG: j.d = Double_val(Field(*v, 0)); break;
    default:
      dump_value(*v, 4);
      std::puts("unimplemented block evalue");
      std::exit(4);
    }
  } else {
    switch (*v) {
    case EVALUE_NULL: j.l = nullptr; break;
    default:
      dump_value(*v, 4);
      std::puts("unimplemented non-block evalue");
      std::exit(4);
    }
  }
  return j;
}

value reconstruct_evalue(jvalue j, ntype ty) {
  CAMLparam0();
  CAMLlocal2(val, result);

  switch (ty) {
  case ntype::Int: {
    result = caml_alloc(1, EVALUE_INT_TAG);
    val = caml_copy_int32(j.i);
    Store_field(result, 0, val);
    CAMLreturn(result);
  }
  case ntype::Long: {
    result = caml_alloc(1, EVALUE_LONG_TAG);
    val = caml_copy_int64(j.j);
    Store_field(result, 0, val);
    CAMLreturn(result);
  }
  case ntype::Float: {
    result = caml_alloc(1, EVALUE_FLOAT_TAG);
    val = caml_copy_double(j.f);
    Store_field(result, 0, val);
    CAMLreturn(result);
  }
  case ntype::Double: {
    result = caml_alloc(1, EVALUE_DOUBLE_TAG);
    val = caml_copy_double(j.d);
    Store_field(result, 0, val);
    CAMLreturn(result);
  }
  case ntype::Reference:
    if (j.l == nullptr) {
      result = EVALUE_NULL;
    } else {
      result = *std::bit_cast<value*>(j.l);
    }
    CAMLreturn(result);
  case ntype::Void: std::puts("reconstruct_evalue: Unimplemented Void"); std::exit(7);
  case ntype::Nil: std::puts("reconstruct_evalue: Unimplemented Nil"); std::exit(7);
  }

  __builtin_unreachable();
}

auto ntype_string(ntype v) -> std::string_view {
  switch (v) {
  case ntype::Reference: return "object";
  case ntype::Void: return "void";
  case ntype::Nil: return "nil";
  case ntype::Int: return "int";
  case ntype::Long: return "long";
  case ntype::Float: return "float";
  case ntype::Double: return "double";
  }
  __builtin_unreachable();
}

void ntype_c_type(ntype ty, std::ostream& os) {
  switch (ty) {
  case ntype::Reference: os << "void*"; return;
  case ntype::Void: os << "void"; return;
  case ntype::Nil: os << "nil"; return;
  case ntype::Int: os << "int"; return;
  case ntype::Long: os << "long"; return;
  case ntype::Float: os << "float"; return;
  case ntype::Double: os << "double"; return;
  }
  __builtin_unreachable();
}

void ntype_c_active_union(ntype ty, std::ostream& os) {
  switch (ty) {
  case ntype::Reference: os << "l"; return;
  case ntype::Void: os << "void"; return;
  case ntype::Nil: os << "nil"; return;
  case ntype::Int: os << "i"; return;
  case ntype::Long: os << "j"; return;
  case ntype::Float: os << "f"; return;
  case ntype::Double: os << "d"; return;
  }
  __builtin_unreachable();
}

// 2 Double
// 5 Long

auto ntype_of_dtype(value v) -> ntype {
  CAMLparam1(v);

  if (Is_block(v)) {
    CAMLreturnT(ntype, ntype::Reference);
  } else {
    switch (Int_val(v)) {
    case 0:
    case 1:
    case 4:
    case 6:
    case 7: CAMLreturnT(ntype, ntype::Int);
    case 8: CAMLreturnT(ntype, ntype::Void);
    case 5: CAMLreturnT(ntype, ntype::Long);
    case 3: CAMLreturnT(ntype, ntype::Float);
    case 2: CAMLreturnT(ntype, ntype::Double);
    default: caml_failwith("Unimplemented integer dtype");
    }
  }
}

auto coerce_null(jobject v) -> value { return v == nullptr ? EVALUE_NULL : *std::bit_cast<value*>(v); }
auto coerce_null(value v, JVMData* data) -> jobject {
  if (Is_long(v) && Long_val(v) == 1) { // null
    return nullptr;
  } else {
    auto ref = data->make_local_reference(v);
    return std::bit_cast<jobject>(ref.get());
  }
}
} // namespace jvmilia