package test;

class StackOrder {
  public static void main(String[] args) {
    int i = 17;
    long l = 56L;
    String s = "hello";
    foo(i, l, s);
  }

  public static void foo(int i, long l, String s) {}
}