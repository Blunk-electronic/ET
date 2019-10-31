TARGET = et

VPATH = include

OBJS = $(TARGET).o \
	et_general.o \
	et_coordinates.o \
	et_frames.o \
	et_text.o \
	et_geometry.o \
	et_devices.o \
	et_symbols.o \
	et_schematic.o \
	et_schematic_sheets.o \
	netlists.o \
	numbering.o \
	assembly_variants.o \
	submodules.o \
	scripting.o \
	schematic_ops.o \
	board_ops.o \
	general_rw.o \
	device_rw.o \
	symbol_rw.o \
	schematic_rw.o \
	pcb_rw.o \
	et_pcb_coordinates.o \
	et_pcb_stack.o \
	et_packages.o \
	et_pcb_sheet.o \
	et_pcb.o \
	pick_and_place.o \
	material.o \
	et_project.o \
	et_import.o et_export.o \
	et_kicad_general.o \
	et_kicad_to_native.o \
	kicad_coordinates.o \
	et_kicad.o \
	et_kicad_pcb.o \
	conventions.o \
	et_csv.o \
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


