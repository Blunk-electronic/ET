TARGET = et

VPATH = include

OBJS = $(TARGET).o \
	et_kicad_general.o \
	et_kicad_to_native.o \
	et_kicad.o \
	et_kicad_pcb.o \
	et_schematic.o \
	et_project.o \
	et_pcb_math.o \
	et_geometry.o \
	et_pcb_coordinates.o \
	et_pcb.o \
	conventions.o \
	et_libraries.o \
	et_csv.o \
	et_coordinates.o \
	et_general.o \
	et_operations.o \
	et_import.o et_export.o \
	et_string_processing.o

PREFIX = $(HOME)
BINDIR = $(PREFIX)/bin

# compile
.adb.o:
	gcc -c -gnat2012 $< -I $(VPATH)
.SUFFIXES: .adb .o

# link
$(TARGET): $(OBJS)
	gnatbind -x $(TARGET).ali; gnatlink $(TARGET).ali

install:
#	install -D -s $(TARGET) $(BINDIR)/$(TARGET) 
# if debug symbols are required:
	install -D $(TARGET) $(BINDIR)/$(TARGET)

uninstall:
	-rm $(BINDIR)/$(TARGET)

clean:
	rm *.o *.ali $(TARGET)


