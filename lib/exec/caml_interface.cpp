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
      std::printf("string (\"%s\")\n", String_val(input));
    } else {
      std::printf("raw block (tag=%d, p=%p)\n", tag, std::bit_cast<void*>(input));
    }
  } else {
    std::printf("int %ld\n", Long_val(input));
  }

  CAMLreturn0;
}

bool evalue_is_class(value evalue) {
  CAMLparam1(evalue);

  CAMLreturnT(bool, Is_block(evalue) && Tag_val(evalue) == 0 && Wosize_val(evalue) == 1);
}

const char* evalue_class_name(value evalue) {
  CAMLparam1(evalue);

  assert(evalue_is_class(evalue));

  // Class.eclassvalue(0).cls(0).raw(0).name(0)
  const char* name = String_val(Field(Field(Field(Field(evalue, 0), 0), 0), 0));

  CAMLreturnT(const char*, name);
}

const char* eclass_name(value evalue) {
  CAMLparam1(evalue);

  // eclass.raw(0).name(0)
  CAMLreturnT(const char*, String_val(Field(Field(evalue, 0), 0)));
}

jvalue evalue_conversion(value v) {
  jvalue j = {};
  if (evalue_is_class(v)) {
    j.l = std::bit_cast<jclass>(v);
    return j;
  } else {
    std::puts("unimplement evalue");
    std::exit(4);
  }
}

value reconstruct_evalue(jvalue j, vtype ty) {
  CAMLparam0();
  CAMLlocal1(result);

  switch (ty) {
  case vtype::Int: {
    result = caml_alloc(1, 1);
    Store_field(result, 0, Val_int(j.i));
    CAMLreturn(result);
  }
  case vtype::Class:
  case vtype::Void:
  case vtype::Nil:
  case vtype::Array: std::puts("Unimplemented"); std::exit(7);
  }

  __builtin_unreachable();
}

auto vtype_string(vtype v) -> std::string_view {
  switch (v) {
  case vtype::Class: return "class";
  case vtype::Void: return "void";
  case vtype::Nil: return "nil";
  case vtype::Int: return "int";
  case vtype::Array: return "array";
  }
  __builtin_unreachable();
}

void vtype_c_type(vtype ty, std::ostream& os) {
  switch (ty) {
  case vtype::Class:
  case vtype::Array: os << "void*"; return;
  case vtype::Void: os << "void"; return;
  case vtype::Nil: os << "nil"; return;
  case vtype::Int: os << "int"; return;
  }
  __builtin_unreachable();
}

void vtype_c_active_union(vtype ty, std::ostream& os) {
  switch (ty) {
  case vtype::Class:
  case vtype::Array: os << "l"; return;
  case vtype::Void: os << "void"; return;
  case vtype::Nil: os << "nil"; return;
  case vtype::Int: os << "i"; return;
  }
  __builtin_unreachable();
}

auto vtype_conversion(value v) -> vtype {
  CAMLparam1(v);

  if (Is_block(v)) {
    switch (Tag_val(v)) {
    case 0: dump_value(v); caml_failwith("Invalid vtype in this context");
    case 1: {
      const char* name = String_val(Field(v, 0));
      if (strcmp(name, "java/lang/Class") == 0) {
        CAMLreturnT(vtype, vtype::Class);
      }
      caml_failwith("Unimplemented class vtype");
    }
    case 2: CAMLreturnT(vtype, vtype::Array);
    default: caml_failwith("Invalid block vtype");
    }
  } else {
    switch (Long_val(v)) {
    case 2: return vtype::Int;
    case 11: return vtype::Void;
    default: caml_failwith("Unimplemented integer vtype");
    }
  }
}
} // namespace jvmilia