class One {
	public One() {
	}

	public void foo() {
	}

	@Deprecated
	public int add(int i) {
		int j = 2;
		return i + j;
	}
}

class Two extends One {
	@Override
	public void foo() {
		super.foo();
	}
}
