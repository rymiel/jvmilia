#include "caml_interface.h"
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/mlvalues.h"
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
    std::putchar('\t');
  }

  if (indent > 0) {
    std::putchar('[');
    bool flag = false;
    for (int i : depth) {
      if (flag) {
        std::putchar('.');
      }
      printf("%d", i);
      flag = true;
    }
    std::printf("] ");
  }

  if (Is_block(input)) {
    auto tag = Tag_val(input);
    if (tag == Closure_tag) {
      std::printf("closure (wosize=%lu, p=%p, code=%p)\n", Wosize_val(input), std::bit_cast<void*>(input),
                  Code_val(input));
    } else if (tag < No_scan_tag) {
      auto wosize = Wosize_val(input);
      std::printf("block (tag=%d, wosize=%lu, p=%p)", tag, wosize, std::bit_cast<void*>(input));
      if (max_depth == 0) {
        std::puts(" <...>");
        CAMLreturn0;
      } else {
        std::puts("");
        for (mlsize_t i = 0; i < wosize; i++) {
          auto d = depth;
          d.push_back(i);
          dump_value(Field(input, i), max_depth - 1, d);
        }
      }
    } else if (tag == String_tag) {
      std::printf("string (%lu) \"%s\"\n", caml_string_length(input), String_val(input));
    } else if (tag == Double_tag) {
      std::printf("double %f\n", Double_val(input));
    } else {
      std::printf("raw block (tag=%d, p=%p)\n", tag, std::bit_cast<void*>(input));
    }
  } else {
    std::printf("int %ld\n", Long_val(input));
  }

  CAMLreturn0;
}

bool evalue_is_object(value evalue) {
  return Is_block(evalue)             // has parameter
         && (Tag_val(evalue) == 0     // index 0 (Object)
             || Tag_val(evalue) == 2  // index 2 (Array)
             || Tag_val(evalue) == 4) // index 4 (ByteArray)
         && Wosize_val(evalue) == 1;  // one field (eobjectvalue)
}

bool evalue_is_integer(value evalue) {
  return Is_block(evalue)            // has parameter
         && Tag_val(evalue) == 1     // index 1 (Int)
         && Wosize_val(evalue) == 1; // one field (int32)
}

bool evalue_is_float(value evalue) {
  return Is_block(evalue)            // has parameter
         && Tag_val(evalue) == 5     // index 5 (Float)
         && Wosize_val(evalue) == 1; // one field (double)
}

jvalue evalue_conversion(value* v) {
  jvalue j = {};
  if (evalue_is_object(*v)) {
    j.l = std::bit_cast<jobject>(v);
  } else if (evalue_is_integer(*v)) {
    j.i = std::bit_cast<jint>(Int32_val(Field(*v, 0)));
  } else if (evalue_is_float(*v)) {
    j.f = static_cast<float>(Double_val(Field(*v, 0)));
  } else {
    dump_value(*v, 4);
    std::puts("unimplemented evalue");
    std::exit(4);
  }
  return j;
}

value reconstruct_evalue(jvalue j, ntype ty) {
  CAMLparam0();
  CAMLlocal1(result);

  switch (ty) {
  case ntype::Int: {
    result = caml_alloc(1, 1);
    Store_field(result, 0, caml_copy_int32(j.i));
    CAMLreturn(result);
  }
  case ntype::Long: {
    result = caml_alloc(1, 3);
    Store_field(result, 0, caml_copy_int64(j.j));
    CAMLreturn(result);
  }
  case ntype::Float: {
    result = caml_alloc(1, 5);
    Store_field(result, 0, caml_copy_double(j.f));
    CAMLreturn(result);
  }
  case ntype::Reference: result = *std::bit_cast<value*>(j.l); CAMLreturn(result);
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
    default: caml_failwith("Unimplemented integer dtype");
    }
  }
}
} // namespace jvmilia