CXXFLAGS	:= -O0 -g -std=c++20 -I /usr/lib/ocaml

CXX_SOURCES := $(wildcard ./*.cpp)
CXX_OBJECTS := $(patsubst ./%.cpp,./build/%.o,$(CXX_SOURCES))
CXX_DEPENDS := $(patsubst ./%.cpp,./build/%.d,$(CXX_SOURCES))

WARNING := -Wall -Wextra -Wmost

.PHONY: all clean

all: build/libjvm.so build/libjvm.a

clean:
	$(RM) $(CXX_OBJECTS) $(CXX_DEPENDS)
	$(RM) build/libjvm.so

build/libjvm.so: $(CXX_OBJECTS)
	@mkdir -p $(@D)
	clang++ -shared $^ -o $@

build/libjvm.a: $(CXX_OBJECTS)
	@mkdir -p $(@D)
	ar r $@ $^

-include $(CXX_DEPENDS)

build/%.o: ./%.cpp Makefile
	@mkdir -p $(@D)
	clang++ $(WARNING) $(CXXFLAGS) -MMD -MP -c $< -o $@
