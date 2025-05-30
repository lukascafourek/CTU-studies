ARCH=riscv64-unknown-elf

SOURCES = bubble-sort.S
TARGET_EXE = bubble-sort

CC=$(ARCH)-gcc.exe
CXX=$(ARCH)-g++
AS=$(ARCH)-as
LD=$(ARCH)-ld
OBJCOPY=$(ARCH)-objcopy

ARCHFLAGS += -mabi=ilp32
ARCHFLAGS += -march=rv32i
ARCHFLAGS += -fno-lto

CFLAGS  += -ggdb -Os -Wall
CXXFLAGS+= -ggdb -Os -Wall
AFLAGS  += -ggdb
LDFLAGS += -ggdb
LDFLAGS += -nostartfiles
LDFLAGS += -nostdlib
LDFLAGS += -static
#LDFLAGS += -specs=/opt/musl/riscv64-linux-gnu/lib/musl-gcc.specs

CFLAGS  += $(ARCHFLAGS)
CXXFLAGS+= $(ARCHFLAGS)
AFLAGS  += $(ARCHFLAGS)
LDFLAGS += $(ARCHFLAGS)

OBJECTS += $(filter %.o,$(SOURCES:%.S=%.o))
OBJECTS += $(filter %.o,$(SOURCES:%.c=%.o))
OBJECTS += $(filter %.o,$(SOURCES:%.cpp=%.o))

all : default

.PHONY : default clean dep all run_test

%.o:%.S
	$(CC) -D__ASSEMBLY__ $(AFLAGS) -c $< -o $@

%.o:%.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@

%.o:%.cpp
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c $<

%.s:%.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -S $< -o $@

#default : $(TARGET_EXE)
default : run_test

$(TARGET_EXE) : $(OBJECTS)
	$(CC) $(LDFLAGS) $^ -o $@

dep: depend

depend: $(SOURCES) $(glob *.h)
	echo '# autogenerated dependencies' > depend
ifneq ($(filter %.S,$(SOURCES)),)
	$(CC)  -D__ASSEMBLY__ $(AFLAGS) -w -E -M $(filter %.S,$(SOURCES)) \
	  >> depend
endif
ifneq ($(filter %.c,$(SOURCES)),)
	$(CC) $(CFLAGS) $(CPPFLAGS) -w -E -M $(filter %.c,$(SOURCES)) \
	  >> depend
endif
ifneq ($(filter %.cpp,$(SOURCES)),)
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -w -E -M $(filter %.cpp,$(SOURCES)) \
	  >> depend
endif

clean:
	rm -f *.o *.a $(OBJECTS) $(TARGET_EXE) depend array_data.out  array_data.ref  array_size.in

#riscv64-unknown-elf-objdump --source bubble-sort

ARRAY_DATA_IN_FILE=array_data.in
ARRAY_SIZE:=$(words $(shell cat $(ARRAY_DATA_IN_FILE)))
ARRAY_BYTES:=$(words $(shell seq 1 $(ARRAY_SIZE)) $(shell seq 1 $(ARRAY_SIZE)) $(shell seq 1 $(ARRAY_SIZE)) $(shell seq 1 $(ARRAY_SIZE)))

run_test: $(TARGET_EXE) $(ARRAY_DATA_IN_FILE)
	echo $(ARRAY_SIZE) >array_size.in
	sort <array_data.in >array_data.ref
	qtrvsim_cli.exe --dump-cycles $< \
		--load-range array_size,array_size.in \
		--load-range array_start,$(ARRAY_DATA_IN_FILE) \
		--dump-range array_start,$(ARRAY_BYTES),array_data.out
	diff -u -B -b array_data.out array_data.ref

-include depend
