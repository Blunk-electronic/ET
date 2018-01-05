------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET LIBRARIES                            --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 Mario Blunk, Blunk electronic                 --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
------------------------------------------------------------------------------

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_coordinates;			use et_coordinates;
with et_string_processing;

package et_libraries is

-- LIBRARY NAMES AND DIRECTORIES

	-- For storing bare library names like "bel_primitives" we use this bounded string:
	library_name_length_max : constant natural := 100; -- CS: increase if necessary
    package type_library_name is new generic_bounded_length (library_name_length_max); use type_library_name;

	function to_string (library_name : in type_library_name.bounded_string) return string;
	-- Returns the given library name as string.
	
	-- Bare library names can be stored further-on in a simple list:
	-- We use a simple list because the order of the library names sometimes matters and must be kept.
    package type_library_names is new doubly_linked_lists (
		element_type => type_library_name.bounded_string);

	-- The base directory where libraries live is stored in a bounded string:
	library_directory_length_max : constant positive := 300; -- CS: increase if necessary
	package type_library_directory is new generic_bounded_length (library_directory_length_max);

	-- This is the library root directory for ALL projects (or for the whole rig).
	-- We assume all projects have their libraries stored in the same directory.
	lib_dir : type_library_directory.bounded_string;
	-- CS: In the future this should be a list of paths as project libraries may be spread
	-- in other directories. For example kicad defines that in the project file in a manner
	-- like "LibDir=../../lbr;/home/tmp/.."

	function to_string (directory : in type_library_directory.bounded_string) return string;
	-- Returns the given library directory as string;
	
	-- If a library is fully specified with path, name and extension we store them in bounded strings:
	library_full_name_max : constant positive := library_directory_length_max + library_name_length_max + 4;
	package type_full_library_name is new generic_bounded_length (library_full_name_max);
	use type_full_library_name;

	function to_string (full_library_name : in type_full_library_name.bounded_string) return string;
	-- Returns the given full library name as string;
	
	-- Full library names can be stored further-on in a simple list:
	-- We use a simple list because the order of the library names sometimes matters and must be kept.
    package type_full_library_names is new doubly_linked_lists (
		element_type => type_full_library_name.bounded_string);

	-- When accessing library files we need this:
	library_handle : ada.text_io.file_type;
	
	-- Full library names can be stored furhter-on in an ordered set like this:
	-- We use a doubly linked list because the order of the library names sometimes matters.
-- 	package type_list_of_full_library_names is new doubly_linked_lists (
-- 		element_type => type_library_full_name.bounded_string);

	-- The name of the person who has drawn, checked or approved something may have 100 characters which seems sufficient for now.
 	person_name_length : constant natural := 100;
	package type_person_name is new generic_bounded_length (person_name_length);

	function to_string (person : in type_person_name.bounded_string) return string;
	-- Returns the given person name as string.





-- TEXT & FIELDS

	-- Texts of any kind must have a size between 0.1 and 50mm
	subtype type_text_size is type_distance range 0.1 .. 50.0; -- unit is mm

	text_size_field_default	: constant type_text_size := 1.27;

	function to_text_size (size : in type_distance) return type_text_size;
	-- Converts given size to type_text_size.
	-- Reports a warning if text size out of range.
	-- CS: make use of it wherever possible !

	function to_string (size : in type_text_size) return string;
	-- Returns the given text size as string.

	subtype type_text_line_width is type_distance range 0.0 .. 5.0; -- unit is mm

	function width_to_string (width : in type_text_line_width) return string;
	-- Returns the given line width as string.
	
	type type_text_style is ( normal, italic, bold, italic_bold);
	type type_text_visible is (yes, no);
    type type_text_alignment_horizontal is ( left, center , right);
	type type_text_alignment_vertical is ( top, center , bottom);    
	type type_text_aligment is record
		horizontal	: type_text_alignment_horizontal := center;
		vertical	: type_text_alignment_vertical := center;
	end record;
	
	type type_text_meaning is (
		REFERENCE,		-- for things like R301 or X9
		VALUE,			-- for component values like "200R"
		COMMISSIONED,	-- for the date of commission in the library
		UPDATED,		-- for the date of the last edit in the library
		AUTHOR,			-- for the person who did the last edit
		
		PACKGE,			-- for compoenent packages like SOT23
		DATASHEET,		-- for url to datasheet
		PURPOSE,		-- for the purpose of the component in the design.
		PARTCODE,		-- for the primary key into an external database (like "R_PAC_S_0805_VAL_200R")
		NOTE,			-- for notes made by a person
		BOM,			-- for assembly variants and assembly options
		MISC); -- CS: others ?

	function to_string (meaning : in type_text_meaning) return string;
	-- Converts meaning to string.

	-- These are basic properties a text has got:
	type type_text_basic is tagged record
		position	: type_2d_point;
        size    	: type_text_size := text_size_field_default;
        style		: type_text_style := type_text_style'first;
        line_width	: type_text_line_width := type_text_line_width'first; -- CS: use a general type_line_width ?
        orientation	: type_angle := 0.0;
		visible		: type_text_visible := yes; -- unless assigned otherwise all texts are visible by default
		alignment	: type_text_aligment;
	end record;

	-- A text have 200 characters which seems sufficient for now.
 	text_length_max : constant natural := 200;
	package type_text_content is new generic_bounded_length(text_length_max); use type_text_content;
	
	-- This is a real text with its content:
	type type_text (meaning : type_text_meaning) is new type_text_basic with record
        content		: type_text_content.bounded_string;
	end record;

	-- This is a placeholder for a text. It does not have content yet, but a meaning:
	type type_text_placeholder (meaning : type_text_meaning) is new type_text_basic with null record;

	procedure write_placeholder_properties (
	-- Writes the properties of the given placeholder.
		placeholder		: in type_text_placeholder;
		log_threshold	: in et_string_processing.type_log_level);

	procedure write_text_properies (
	-- Outputs the properties of the given text.
		text : in et_libraries.type_text;
		log_threshold : in et_string_processing.type_log_level);

	function content ( text : in type_text) return string;
	-- Returns the content of the given text as string.


	
	
	
-- COMPONENTS, PACKAGES, PORTS AND PINS

	-- A port is something where a net can be attached to.
	-- The name of a port represents the function of the port like (A14 or RST_N)
	subtype type_port_length is type_distance range 0.0 .. 20.0; -- unit is millimeters. CS: reasonable limits ?
	
	-- The port has an electrical direction:
	type type_port_direction is (
		PASSIVE,	-- almost all passive components like resistors, capacitors, .. have such ports
		INPUT,		-- signal inputs
		OUTPUT,		-- signal outputs
		BIDIR,		-- bidirectional ports
		TRISTATE,	-- tristate ports
		UNKNOWN,
		POWER_OUT,	-- a power source like power symbol (VCC, GND, ..)
		POWER_IN,	-- a power sink like power ports of ICs
		PULL_LOW,	-- a port with internal pull-up resistor CS: rename to weak1
		PULL_HIGH,	-- a port with internal pull-down resistor CS: rename to weak0
		NOT_CONNECTED	-- advised by manufacturer to be left unconnected
		);

	function to_string (
		direction	: in type_port_direction;
		preamble	: in boolean := true) return string;
	-- Returns the given port direction as string.

	type type_port_visible is (ON, OFF);
	type type_pin_visible is (ON, OFF);

	type type_port_style is ( -- CS: find a better name
		NONE,
		INVERTED,
		CLOCK,
		INVERTED_CLOCK,
		INPUT_LOW,
		CLOCK_LOW,
		OUTPUT_LOW,
		FALLING_EDGE_CLK, RISING_EDGE_CLK,
		NON_LOGIC,

		INVISIBLE_INVERTED,
		INVISIBLE_CLOCK,
		INVISIBLE_INVERTED_CLOCK,
		INVISIBLE_INPUT_LOW,
		INVISIBLE_CLOCK_LOW,
		INVISIBLE_OUTPUT_LOW,
		INVISIBLE_FALLING_EDGE_CLK, INVISIBLE_RISING_EDGE_CLK,
		INVISIBLE_NON_LOGIC);

	
 	port_name_length	: constant natural := 100;
	package type_port_name is new generic_bounded_length(port_name_length); use type_port_name;

	function to_string (port_name : in type_port_name.bounded_string) return string;
	-- Returns the given port name as string.

	-- The name of a pin may have 10 characters which seems sufficient for now.
 	pin_name_length	: constant natural := 10;
	package type_pin_name is new generic_bounded_length(pin_name_length); use type_pin_name;

	function to_string (pin_name : in type_pin_name.bounded_string) return string;
	-- Returns the given pin name as string.
	
	-- Initially, at the lowest level (usually library level), a port has a name, direction,
	-- coordinates, orientation, flags for making port and pin name visible. 
	-- Later, other values are assigned like pin name. CS: set defaults
	type type_port is record
		name				: type_port_name.bounded_string; -- like "CLOCK" or "CE"		
		direction			: type_port_direction; -- example: "passive"
		style				: type_port_style;
		coordinates			: type_2d_point; -- there is only x and y
		length				: type_port_length;
		orientation			: type_angle;
		port_name_visible	: type_port_visible;
		pin_name_visible	: type_pin_visible;
		pin					: type_pin_name.bounded_string; -- example: "144" or in case of a BGA package "E14"
		port_name_size		: type_text_size;
		pin_name_size		: type_text_size;
		port_name_offset	: type_distance; -- the clearance between symbol outline and port name -- CS: define a reasonable range
		-- CS : obsolete ? pin_position_offset ?
		-- CS: port swap level ?
	end record;

	-- Ports of a component are collected in a simple list. A list, because multiple ports
	-- with the same name (but differing pin/pad names) may exist. For example lots of GND
	-- ports at large ICs.
-- 	package type_ports is new ordered_maps ( 
-- 		key_type => type_port_name.bounded_string, -- like "CLOCK" or "CE"
-- 		element_type => type_port); 
	package type_ports is new doubly_linked_lists ( 
-- 		key_type => type_port_name.bounded_string, -- like "CLOCK" or "CE"
		element_type => type_port); 


	-- The generic name of a component in the library. 
 	component_name_length_max : constant natural := 100;
	package type_component_name is new generic_bounded_length (component_name_length_max); use type_component_name;
	-- Only those characters are allowed for the generic component name.
	-- See et_import.check_component_name for customization depending on CAD format.
	component_name_characters : character_set := to_set 
		(ranges => (('A','Z'),('0','9'))) 
		or to_set('-') 
		or to_set('_'); 

	procedure check_component_name (
	-- Checks if the the given generic component name meets certain conventions.
		name : in type_component_name.bounded_string; -- TRANSISTOR_NPN
		customized : in boolean := false); -- when true use customized character set
		-- for the test (depends on import CAD format).

	function strip_tilde (generic_name : in type_component_name.bounded_string) return
		type_component_name.bounded_string;
	-- Removes a heading tilde character from a generic component name.
	-- example: ~TRANSISTOR_NPN becomes TRANSISTOR_NPN

	function prepend_tilde (generic_name : in type_component_name.bounded_string) return
		type_component_name.bounded_string;
	-- Prepends a heading tilde character to a generic component name.
	-- example: TRANSISTOR_NPN becomes ~TRANSISTOR_NPN
	
	function to_string (name_in_library : in type_component_name.bounded_string) return string;
	-- Returns the given name_in_library as as string.

	-- The component value is something like 330R or 100n or 74LS00
	component_value_length_max : constant positive := 100;

	-- Define the characters that are allowed for a component value:
	component_value_characters : character_set := to_set (ranges => (('A','Z'),('a','z'),('0','9'))) or to_set('_');
	package type_component_value is new generic_bounded_length (component_value_length_max);

	function to_string (value : in type_component_value.bounded_string) return string;
	-- Returns the given value as string.
	
	-- For some component (not all !) it is helpful to have an URL to the datasheet:
	component_datasheet_length_max : constant positive := 1000;
	package type_component_datasheet is new generic_bounded_length (component_datasheet_length_max);

	-- A component reference (in Eagle "device name") consists of a prefix (like R, C, IC, ..)
	-- and a consecutive number. Both form something like "IC702"
	-- Component referencees (in Eagle "device names") have prefixes like R, C, IC, ...	
	component_prefix_length_max : constant natural := 10; -- CS: there is no reason to work with longer prefixes.
	package type_component_prefix is new generic_bounded_length (component_prefix_length_max);
	use type_component_prefix;

	function to_string (prefix : in type_component_prefix.bounded_string) return string;
	-- returns the given prefix as string

	type type_component_reference_element is (PREFIX, ID);
	component_reference_prefix_default : constant type_component_prefix.bounded_string := to_bounded_string("?");
	component_reference_id_default : constant natural := 0;

	type type_component_reference is record -- CS: should be private
		prefix		: type_component_prefix.bounded_string := component_reference_prefix_default; -- like "IC"
		id			: natural := component_reference_id_default; -- like "303"
		id_width	: positive; -- the number of digits in the id. 3 in case of an id of 303
		-- NOTE: This allows something like R091 or IC0 (there are reasons for such strange things ...)
	end record;

	function to_string (reference : in type_component_reference) return string;
	-- Returns the given component reference as string.
	-- Prepends leading zeros according to reference.id_width.
	
	function prefix (reference : in type_component_reference) return type_component_prefix.bounded_string;
	-- Returns the prefix of the given component reference.
	
	function component_value_valid (
	-- Returns true if the given component value meets certain conventions.									   
		value 		: in type_component_value.bounded_string;
		reference	: in type_component_reference) 
		return boolean;

	
	-- PACKAGES AND VARIANTS
	-- A component package is something like "SOT32" or "NDIP14". It is a more or less standardized (JEDEC)
	-- designator for the housing or the case of an electronical component. The package name is independed of
	-- the actual purpose of a component. An LED can have an SOT23 package and a transistor can also come in an SOT23.

	-- The variant is usually a suffix in a component name, given by its manufacturer. The variant is a manufacturer
	-- specific abbrevation for the package a component comes with.
	-- Example: An opamp made by TI can be the type TL084N or TL084D. N means the NDIP14 package
	-- whereas D means the SO14 package.

	-- component package names like "SOT23" or "TO220" are stored in bounded strings:
	-- Kicad refers to them as "footprints".
	component_package_name_length_max : constant positive := 100;
	package type_component_package_name is new generic_bounded_length(component_package_name_length_max);
	--use type_component_package_name;

	-- The component partcode is something like "R_PAC_S_0805_VAL_"
	component_partocde_length_max : constant positive := 200;
	package type_component_partcode is new generic_bounded_length (component_partocde_length_max);

	function to_string (partcode : in type_component_partcode.bounded_string) return string;
	-- Returns the given partcode as string.
	
	-- Components that require operator interaction like connectors, LEDs or switches have purpose.
	-- Example: The purpose of connector X44 is "power in". The purpose of LED5 is "system fail":
	component_purpose_length_max : constant positive := 100;
	package type_component_purpose is new generic_bounded_length (component_purpose_length_max);

	function to_string (purpose : in type_component_purpose.bounded_string) return string;
	-- Returns the given purpose as string.
	
	function to_string (packge : in type_component_package_name.bounded_string) return string;
	-- Returns the given package name as as string.
	
	-- VARIANT NAMES
	-- If a component has package variants, a suffix after the component type indicates the package
	-- The variant name is manufacturer specific. example: TL084D or TL084N
	-- component package variant names like "N" or "D" are stored in short bounded strings:
	component_variant_name_length_max : constant positive := 10;
	package type_component_variant_name is new generic_bounded_length(component_variant_name_length_max);
	use type_component_variant_name;

	-- A component variant is a composite of the package, the library it is stored in and
	-- a connection list. The connection list maps from port names to pin/pad names.
	type type_component_variant is record
		packge	: type_component_package_name.bounded_string;	-- like "SOT23"
		library	: type_full_library_name.bounded_string;		-- like "/home/abc/lib/smd.mod"
		-- CS: connection list
	end record;

	function to_string (variant : in type_component_variant) return string;
	-- Returns the given variant as string.
	-- NOTE: This displays the type_component_variant (see et_libraries.ads).
	-- Do not confuse with type_variant (see et_schematic.ads) which also contains the variant name
	-- like in TL084D or TL084N.
	
	-- Component variants are stored in a map where they are accessed by the variant name.
	package type_component_variants is new ordered_maps (
		key_type => type_component_variant_name.bounded_string,
		element_type => type_component_variant);

	-- If certain packages are to be proposed they are collected in a so called "package filter"
	package_proposal_length_max : constant positive := 100;
	package type_package_proposal is new generic_bounded_length (package_proposal_length_max);
	use type_package_proposal;
	package type_package_filter is new ordered_sets (type_package_proposal.bounded_string);

	
	




	
-- SHAPES 

	-- line width
	subtype type_line_width is type_distance;

	-- fill
	type type_fill_border is ( visible, invisible);
	type type_fill_pattern is (none, solid); -- CS: hatched ? and its properties ?
	type type_fill is record 
		border	: type_fill_border := visible;
		pattern : type_fill_pattern := solid;
	end record;
	
	-- straight lines
	type type_line is record
		start_point 	: type_coordinates;
		end_point   	: type_coordinates;
		line_width		: type_line_width;
	end record;
	package type_lines is new doubly_linked_lists (element_type => type_line);

	-- polylines 
	-- A polyline is a list of points. Their interconnections have a width and a fill.
	-- Filling can be done even if start and end point do not meet. In this case a virtual line
	-- is "invented" that connects start and end point.
	-- Finally the polylines are collected in a simple list.
	package type_points is new doubly_linked_lists (element_type => type_coordinates);
	type type_polyline is record
		line_width		: type_line_width;
		fill			: type_fill;
		points			: type_points.list;
	end record;
	package type_polylines is new doubly_linked_lists (element_type => type_polyline);

	-- Rectangles
	-- It is sufficient to specifiy the diagonal of the rectangle.
	type type_rectangle is record
		start_point		: type_coordinates;
		end_point		: type_coordinates;
		line_width		: type_line_width;
		fill			: type_fill;
	end record;
	package type_rectangles is new doubly_linked_lists (element_type => type_rectangle);	
	
	-- Arcs
	type type_arc is record
		center			: type_coordinates;
		radius  		: type_distance;
		start_point		: type_coordinates;
		end_point		: type_coordinates;
		start_angle		: type_angle;
		end_angle		: type_angle;
		line_width		: type_line_width;
 		fill			: type_fill;
	end record;
	package type_arcs is new doubly_linked_lists (element_type => type_arc);

	-- Circles
	type type_circle is record
		center			: type_coordinates;
		radius  		: type_distance;
		line_width		: type_line_width;
		fill			: type_fill;
	end record;
	package type_circles is new doubly_linked_lists (element_type => type_circle);

	-- Shapes are wrapped in a the type_shapes:
	type type_shapes is record
		lines		: type_lines.list 		:= type_lines.empty_list;
		polylines	: type_polylines.list	:= type_polylines.empty_list;
		rectangles	: type_rectangles.list	:= type_rectangles.empty_list;
		arcs 		: type_arcs.list		:= type_arcs.empty_list;
		circles		: type_circles.list		:= type_circles.empty_list;
	end record;


	
-- SYMBOLS AND UNITS

	-- In schematics electrical components like resistors, capactors and inductors are called "symbols".
	-- Since they are frequently used we store such things in symbol libraries like bel_primitives.sym.
	-- The symbol name is something very general like "NAND", "Resistor", "Switch"

	-- A component has one or more units. A unit is a subsection of a component (EAGLE refer to them as "gates").
	-- There are internal units, which exist for the particular component exclusively. 
	-- An internal unit has a symbol and further properties like a swap level.
	-- There are external units, which are used for frequently used symbols like resistors or capacitors.
	-- An external unit is just a reference to a symbol library, the symbol name therein and other properties
	-- like swap level.	
	-- The unit name is something like "I/O Bank 3", "PWR" or "Switch 1" "Switch 2"

	-- SYMBOLS
	
	symbol_name_length_max : constant natural := 50;
	package type_symbol_name is new generic_bounded_length(symbol_name_length_max); use type_symbol_name;

	type type_symbol_element is (
		line, polyline, rectangle, arc, circle, -- shapes
		port, 
		text); -- text embedded in a symbol
		--reference, value, commissioned, updated, author); -- text field placeholders

	-- Texts may be embedded in a symbol like "counter" or "&". So far they have the meaning "misc".
	-- CS: strings and texts within a unit symbol serve for documentation only. As long as
	-- there is no dedicated enumeration value available we choose "misc".
	-- CS: The meaning could be something like "documentation" some day.
	type type_symbol_text is new type_text (meaning => misc) with null record;
	package type_symbol_texts is new doubly_linked_lists (element_type => type_symbol_text);

	type type_component_appearance is ( 
		sch,		-- a component that exists in the schematic only (like power symbols)
		sch_pcb,	-- a component that exists in both schematic and soldered on a pcb
		pcb			-- a component that exists on the pcb only (like a fiducial)		
		-- CS: cable 
		-- CS: wire
		-- CS: net-ties, netchanger
		-- ...
		);

	function to_string (appearance : in type_component_appearance) return string;
	-- Returns the given component appearance as string.
	
	type type_symbol (appearance : type_component_appearance) is record
		shapes		: type_shapes; -- the collection of shapes
		texts		: type_symbol_texts.list; -- the collection of texts (meaning misc)
		--ports		: type_ports.map := type_ports.empty_map; -- the ports of the symbol
		ports		: type_ports.list := type_ports.empty_list; -- the ports of the symbol
		
		-- Placeholders for component wide texts. To be filled with content when 
		-- a symbol is placed in the schematic:
		reference	: type_text_placeholder (meaning => et_libraries.reference);
		value		: type_text_placeholder (meaning => et_libraries.value);
		commissioned: type_text_placeholder (meaning => et_libraries.commissioned);
		updated		: type_text_placeholder (meaning => et_libraries.updated);
		author		: type_text_placeholder (meaning => et_libraries.author);
		-- Symbols have further text placeholders according to the appearance of the component:
		case appearance is
			when sch_pcb =>
				packge		: type_text_placeholder (meaning => et_libraries.packge);
				datasheet	: type_text_placeholder (meaning => et_libraries.datasheet);
				purpose		: type_text_placeholder (meaning => et_libraries.purpose);
				partcode	: type_text_placeholder (meaning => et_libraries.partcode);
				bom 		: type_text_placeholder (meaning => et_libraries.bom);
			when others => null;
		end case;
	end record;



	unit_name_length_max : constant natural := 50;	
	package type_unit_name is new generic_bounded_length (unit_name_length_max); use type_unit_name;

	function to_string (unit_name : in type_unit_name.bounded_string) return string;
	-- Returns the given unit name as string.

	function to_unit_name (unit_name : in string) return type_unit_name.bounded_string;
	-- Returns the given unit name as type_unit_name.
	
	unit_swap_level_max : constant natural := 10;
	type type_unit_swap_level is new natural range 0..unit_swap_level_max;
	unit_swap_level_default : constant := type_unit_swap_level'first;

	type type_unit_add_level is (
		NEXT, 		-- should be default. for things like logig gates, multi-OP-Amps, ...
		REQUEST, 	-- for power supply 
		CAN,		-- for things like optional relay contacts
		ALWAYS,		-- CS: see EAGLE manual
		MUST);		-- for things like relay coils

	function to_string (add_level : in type_unit_add_level) return string;
	-- Returns the given add level as string.
	
	-- An internal unit is a symbol with a swap level.
	-- An internal unit is owned by the particular component exclusively.
	type type_unit_internal (appearance : type_component_appearance) is record
		symbol		: type_symbol (appearance);
		coordinates	: type_coordinates;
		swap_level	: type_unit_swap_level := unit_swap_level_default;
		add_level	: type_unit_add_level := type_unit_add_level'first;

		-- Currentliy only required by kicad: Units that harbor component wide 
		-- pins have this flag set.
		-- Usually units with power supply pins exclusively.
		-- When building portlists this flag is important. See et_netlist package.
		global		: boolean := false; -- CS: use a boolean derived type 
	end record;

	-- Internal units are collected in a map:
	package type_units_internal is new indefinite_ordered_maps (
		key_type => type_unit_name.bounded_string, -- like "I/O-Bank 3" "A" or "B"
		element_type => type_unit_internal);

	-- External units have a reference to an external symbol.
	-- External units are stored in a library and are shared by many components.
	type type_unit_reference is record
		library		: type_full_library_name.bounded_string; -- like /my_libraries/logig.sym
		name		: type_symbol_name.bounded_string;		 -- like "NAND" or "Resistor" or "Switch"
	end record;

	-- An external unit has a reference and a swap level.
	type type_unit_external is record -- CS: parameter appearance ?
		reference	: type_unit_reference;
		coordinates	: type_coordinates;
		swap_level	: type_unit_swap_level := unit_swap_level_default;
		add_level	: type_unit_add_level := type_unit_add_level'first;
		-- NOTE: there is no "global" flag as with type_unit_internal.
		-- The "global" flag is a kicad requirement. Since kicad does not know 
		-- external units, this flag is not present here.
	end record;

	-- External units are collected in a map;
	package type_units_external is new ordered_maps (
		key_type => type_unit_name.bounded_string,		 -- like "I/O-Bank 3"
		element_type => type_unit_external);

	type type_bom is (YES, NO);

	function to_string (bom : in type_bom) return string;
	-- Returns the given bom variable as string.
	
-- COMPONENTS

	type type_component (appearance : type_component_appearance) is record
		prefix			: type_component_prefix.bounded_string; -- R, C, IC, ...
		value			: type_component_value.bounded_string; -- 74LS00
		units_internal	: type_units_internal.map := type_units_internal.empty_map;
		units_external	: type_units_external.map := type_units_external.empty_map;
		commissioned	: et_string_processing.type_date;
		updated			: et_string_processing.type_date;
		author			: type_person_name.bounded_string;

		case appearance is

			-- If a component appears in the schematic only, it does not
			-- have any package variants.
			-- Such components are power symbols or power flags. Later when building netlists
			-- those components enforce net names (like GND or P3V3). Power flags do not
			-- enforce net names. In order to distinguish them from regular power symbols the
			-- power_flag is provided.
			when sch => 
				power_flag	: boolean := false; -- CS: use a derived type

			-- If a component appears in both schematic and layout it comes 
			-- with at least one package/footprint variant. We store variants in a map.
			when sch_pcb => 
				variants	: type_component_variants.map;
				package_filter : type_package_filter.set := type_package_filter.empty_set;
				datasheet	: type_component_datasheet.bounded_string;
				purpose		: type_component_purpose.bounded_string;
				partcode	: type_component_partcode.bounded_string;
				bom			: type_bom;
			when others => null; -- CS
		end case;

	end record;

	-- Components are stored in a map.
	-- Within the map they are accessed by a key type_component_name (something like "CAPACITOR").
	package type_components is new indefinite_ordered_maps (
		key_type => type_component_name.bounded_string, -- example: "TRANSISTOR_PNP"
		element_type => type_component);
	use type_components;

	function component_appearance (cursor : in type_components.cursor)
	-- Returns the component appearance where cursor points to.
		return type_component_appearance;

	--	CS: currently there is no need for a component summary
	-- procedure write_component_properties (component : in type_components.cursor);
	-- Writes the properties of the component indicated by the given cursor.

	--package type_libraries is new indefinite_ordered_maps (	
	package type_libraries is new ordered_maps (
		key_type => type_full_library_name.bounded_string,
		element_type => type_components.map);

	-- All component models are collected here. This collection applies for the whole rig.
	component_libraries : type_libraries.map; -- CS: should be part of type_rig. see et_schematic type_rig

	function find_component (
	-- Searches the given library for the given component. Returns a cursor to that component.
		library		: in type_full_library_name.bounded_string;
		component	: in type_component_name.bounded_string) 
		return type_components.cursor;

	function first_internal_unit (
	-- Returns the cursor to the first unit of the given component
		component_cursor : in type_components.cursor)
		return type_units_internal.cursor;

	function first_port (
	-- Returns the cursor to the first port of the given unit
		unit_cursor : in type_units_internal.cursor)
		return type_ports.cursor;

	procedure no_generic_model_found (
		reference : in type_component_reference; -- IC303
		library : in type_full_library_name.bounded_string; -- ../lib/xilinx.lib
		generic_name : in type_component_name.bounded_string);

		
		
end et_libraries;

-- Soli Deo Gloria
