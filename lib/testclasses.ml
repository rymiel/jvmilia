open Basic

let java_lang_Object : jclass =
  {
    name = "java/lang/Object";
    access_flags = class_access_flags_of_int 0x0021l;
    superclass = None;
    superinterfaces = [];
    methods =
      [
        {
          name = "<init>";
          access_flags = method_access_flags_of_int 0x0001l;
          desc = "()V";
          attributes =
            [
              Code
                {
                  frame_size = 1;
                  max_stack = 0;
                  handlers = [];
                  stack_map_desc = [];
                  code = [ (0, Return) ];
                };
            ];
        };
        {
          name = "getClass";
          access_flags = method_access_flags_of_int 0x0111l;
          desc = "()Ljava/lang/Class;";
          attributes = [];
        };
        {
          name = "hashCode";
          access_flags = method_access_flags_of_int 0x0101l;
          desc = "()I";
          attributes = [];
        };
        {
          name = "equals";
          access_flags = method_access_flags_of_int 0x0001l;
          desc = "(Ljava/lang/Object;)Z";
          attributes =
            [
              Code
                {
                  frame_size = 2;
                  max_stack = 2;
                  handlers = [];
                  stack_map_desc = [ (9, Same); (0, SameLocals1StackItem Int) ];
                  code =
                    [
                      (0, Aload_0);
                      (1, Aload_1);
                      (2, If_acmpne 9);
                      (5, Iconst_1);
                      (6, Goto 10);
                      (9, Iconst_0);
                      (10, Ireturn);
                    ];
                };
            ];
        };
        {
          name = "clone";
          access_flags = method_access_flags_of_int 0x0104l;
          desc = "()Ljava/lang/Object;";
          attributes = [];
        };
      ];
    loader = None;
  }

let java_lang_String : jclass =
  {
    name = "java/lang/String";
    access_flags = class_access_flags_of_int 0x0031l;
    superclass = Some "java/lang/Object";
    superinterfaces = [ "java/io/Serializable" ];
    methods =
      [
        {
          name = "<init>";
          access_flags = method_access_flags_of_int 0x0001l;
          desc = "()V";
          attributes =
            [
              Code
                {
                  frame_size = 1;
                  max_stack = 2;
                  handlers = [];
                  stack_map_desc = [];
                  code =
                    [
                      (0, Aload_0);
                      ( 1,
                        Invokespecial
                          {
                            cls = "java/lang/Object";
                            name = "<init>";
                            desc = "()V";
                          } );
                      (4, Aload_0);
                    ];
                };
            ];
        };
        {
          name = "intern";
          access_flags = method_access_flags_of_int 0x0101l;
          desc = "()Ljava/lang/String;";
          attributes = [];
        };
      ];
    loader = None;
  }

let test_One : jclass =
  {
    name = "test/One";
    access_flags = class_access_flags_of_int 0x0020l;
    superclass = Some "java/lang/Object";
    superinterfaces = [];
    methods =
      [
        {
          name = "<init>";
          access_flags = method_access_flags_of_int 0x0001l;
          desc = "()V";
          attributes =
            [
              Code
                {
                  frame_size = 1;
                  max_stack = 1;
                  handlers = [];
                  stack_map_desc = [];
                  code =
                    [
                      (0, Aload_0);
                      ( 1,
                        Invokespecial
                          {
                            cls = "java/lang/Object";
                            name = "<init>";
                            desc = "()V";
                          } );
                      (4, Return);
                    ];
                };
            ];
        };
        {
          name = "foo";
          access_flags = method_access_flags_of_int 0x0001l;
          desc = "()V";
          attributes =
            [
              Code
                {
                  frame_size = 1;
                  max_stack = 0;
                  handlers = [];
                  stack_map_desc = [];
                  code = [ (0, Return) ];
                };
            ];
        };
        {
          name = "add";
          access_flags = method_access_flags_of_int 0x0001l;
          desc = "()V";
          attributes =
            [
              Code
                {
                  frame_size = 3;
                  max_stack = 2;
                  handlers = [];
                  stack_map_desc = [];
                  code =
                    [
                      (0, Iconst_2);
                      (1, Istore_2);
                      (2, Iload_1);
                      (3, Iload_2);
                      (4, Iadd);
                      (5, Ireturn);
                    ];
                };
            ];
        };
      ];
    loader = None;
  }

let test_Two : jclass =
  {
    name = "test/Two";
    access_flags = class_access_flags_of_int 0x0020l;
    superclass = Some "test/One";
    superinterfaces = [];
    methods =
      [
        {
          name = "<init>";
          access_flags = method_access_flags_of_int 0x0000l;
          desc = "()V";
          attributes =
            [
              Code
                {
                  frame_size = 1;
                  max_stack = 1;
                  handlers = [];
                  stack_map_desc = [];
                  code =
                    [
                      (0, Aload_0);
                      ( 1,
                        Invokespecial
                          { cls = "test/One"; name = "<init>"; desc = "()V" } );
                      (4, Return);
                    ];
                };
            ];
        };
        {
          name = "foo";
          access_flags = method_access_flags_of_int 0x0001l;
          desc = "()V";
          attributes =
            [
              Code
                {
                  frame_size = 1;
                  max_stack = 1;
                  handlers = [];
                  stack_map_desc = [];
                  code =
                    [
                      (0, Aload_0);
                      ( 1,
                        Invokespecial
                          { cls = "test/One"; name = "foo"; desc = "()V" } );
                      (4, Return);
                    ];
                };
            ];
        };
      ];
    loader = None;
  }
