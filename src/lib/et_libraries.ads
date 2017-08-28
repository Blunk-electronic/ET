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
--with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;
with et_string_processing;

package et_libraries is

	-- LIBRARY NAMES AND DIRECTORIES

	-- For storing bare library names like "bel_primitives" we use this bounded string:
	library_name_length_max : constant natural := 100; -- CS: increase if necessary
    package type_library_name is new generic_bounded_length(library_name_length_max); use type_library_name;

	-- Bare library names can be stored further-on in an ordered set like this:
	-- We use a doubly linked list because the order of the library names sometimes matters.
    package type_list_of_library_names is new doubly_linked_lists (
		element_type => type_library_name.bounded_string);

	project_libraries : type_list_of_library_names.list;
	
	-- The base directory where libraries live is stored in a bounded string:
	library_directory_length_max : constant positive := 300; -- CS: increase if necessary
	package type_library_directory is new generic_bounded_length(library_directory_length_max); use type_library_directory;

	lib_dir : type_library_directory.bounded_string; -- here the path to the project libraries is stored
	-- CS: this should be a list of paths
	
	-- If a library is fully specified with path, name and extension we store them in bounded strings:
	library_full_name_max : constant positive := library_directory_length_max + library_name_length_max + 4;
	package type_library_full_name is new generic_bounded_length(library_full_name_max);
	use type_library_full_name;

	-- Full library names can be stored furhter-on in an ordered set like this:
	-- We use a doubly linked list because the order of the library names sometimes matters.
-- 	package type_list_of_full_library_names is new doubly_linked_lists (
-- 		element_type => type_library_full_name.bounded_string);

	-- The name of the person who has drawn, checked or approved something may have 100 characters which seems sufficient for now.
 	person_name_length	: constant natural := 100;
	package type_person_name is new generic_bounded_length(person_name_length);

-- GRID AND COORDINATES

	type type_grid_extended is digits 11 range -100000000.00 .. 100000000.00;
	subtype type_grid is type_grid_extended range -100000.00 .. 100000.00; -- CS: unit assumed is MIL !!!
	-- CS: negative schematic coordinates should be forbidden
	-- type type_grid is digits 7 range 0.00 .. 100000.00; -- CS: unit assumed is MIL !!!	

	coordinate_zero : constant type_grid := 0.0;

    -- In general every object has at least x,y coordinates.
	type type_coordinates is tagged record
		x,y				: type_grid := coordinate_zero;
	end record;



	coordinates_dimension_separator : constant string (1..1) := "/";
	coordinates_preamble : constant string (1..15) := "position (x" & coordinates_dimension_separator & "y) ";
	
	function to_string ( position : in type_coordinates) return string;
	-- Returns the given position as string.


-- ORIENTATION AND ROTATION
	-- Objects may be placed at a certain orientation:
	type type_orientation is range 0 .. 359; -- CS: a type that allows angles of multiples of 45 degrees ?

	function to_string (orientation : in type_orientation) return string;
	-- Returns the given orientation as string. 

	-- Objects may be placed at a certain angle (unit is degrees):
	type type_angle is digits 4 range -359.9 .. 359.9;
	
-- SCHEMATIC RELATED

 	port_name_length	: constant natural := 50;
	package type_port_name is new generic_bounded_length(port_name_length); use type_port_name;

 	component_name_length_max : constant natural := 100;
	package type_component_name is new generic_bounded_length(component_name_length_max); use type_component_name;

	-- For some component (not all !) it is helpful to have an URL to the datasheet:
	component_datasheet_length_max : constant positive := 1000;
	package type_component_datasheet is new generic_bounded_length (component_datasheet_length_max);

	
	function to_string ( name_in_library : in type_component_name.bounded_string) return string;
	-- Returns the given name_in_library as as string.

	
	-- The name of a pin may have 10 characters which seems sufficient for now.
 	pin_name_length	: constant natural := 10;
	package type_pin_name is new generic_bounded_length(pin_name_length); use type_pin_name;

-- LAYOUT RELATED

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

	-- Components that require operator interaction like connectors, LEDs or switches have purpose.
	-- Example: The purpose of connector X44 is "power in". The purpose of LED5 is "system fail":
	component_purpose_length_max : constant positive := 100;
	package type_component_purpose is new generic_bounded_length (component_purpose_length_max);
	
	function to_string ( packge : in type_component_package_name.bounded_string) return string;
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
		library	: type_library_full_name.bounded_string;		-- like "/home/abc/lib/smd.mod"
		-- CS: connection list
	end record;

	function to_string ( variant : in type_component_variant) return string;
	-- Returns the given variant as string.
	-- NOTE: This displays the type_component_variant (see et_libraries.ads).
	-- Do not confuse with type_variant (see et_schematic.ads) which also contains the variant name
	-- like in TL084D or TL084N.
	
	-- Component variants are stored in a map where they are accessed by the variant name.
	package type_component_variants is new ordered_maps (
		key_type => type_component_variant_name.bounded_string,
		element_type => type_component_variant);
	
-- TEXT & FIELDS

    -- CS: currently we use unit mil which is old fashionated
    type type_text_size is range 1..1000; -- CS unit assumed is MIL !!!
	type type_text_line_width is range 0..100; -- CS unit assumed is MIL !!!
	type type_text_style is ( normal, italic, bold, italic_bold);
	type type_text_visible is (yes, no);
    type type_text_alignment_horizontal is ( left, center , right);
	type type_text_alignment_vertical is ( top, center , bottom);    
	type type_text_aligment is record
		horizontal	: type_text_alignment_horizontal := center;
		vertical	: type_text_alignment_vertical := center;
	end record;
	
	-- Text fields:
	text_meaning_prefix : constant string (1..2) := "P_"; -- workaround, see below
	type type_text_meaning is (
		REFERENCE,		-- for things like R301 or X9
		VALUE,			-- for component values like "200R"
		COMMISSIONED,	-- for the date of commission in the library
		UPDATED,		-- for the date of the last edit in the library
		AUTHOR,			-- for the person who did the last edit
		
		PACKGE,			-- for compoenent packages like SOT23
		DATASHEET,		-- for url to datasheet
		P_FUNCTION, 	-- for the function of the component in the design. workaround: "P_" avoids usage of an ada keyword  -- CS: rename to PURPOSE
		PARTCODE,		-- for the primary key into an external database (like "R_PAC_S_0805_VAL_200R")
		NOTE,			-- for notes made by a person
		MISC); -- CS: others ?

	function to_string ( meaning : in type_text_meaning) return string;
	-- Converts meaning to string.

	-- These are basic properties a text has got:
	type type_text_basic is tagged record
		position	: type_coordinates;
        size    	: type_text_size := 50; -- CS define a default
        style		: type_text_style := type_text_style'first;
        line_width	: type_text_line_width := 0; -- CS: use a general type_line_width ?
        orientation	: type_orientation := type_orientation'first;
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

	procedure write_placeholder_properties ( placeholder : in type_text_placeholder; indentation : in natural := 0);
	-- Writes the properties of the given placeholder.

	procedure write_text_properies ( text : in et_libraries.type_text; indentation : in natural := 0);
	-- Outputs the properties of the given text.

	function content ( text : in type_text) return string;
	-- Returns the content of the given text as string.
	
-- PORTS
	
	-- A port is something where a net can be attached to.
	-- The name of a port represents the function of the port like (A14 or RST_N)

	-- The port has an electrical direction:
	type type_port_direction is (
		DIGIAL_IN,
		DIGIAL_OUT,
		ANALOG_IN,
		ANALOG_OUT,
		PASSIVE, 		-- no explicit direction
		NOT_CONNECTED,
		POWER_OUT, 		-- a power source
		POWER_IN		-- a power sink
		);

	type type_port_visible is ( ON, OFF);
	type type_pin_visible is ( ON, OFF);
	
	-- Initially, at the lowest level (usually library level), a port has a name, direction,
	-- coordinates, orientation, flags for making port and pin name visible. 
	-- Later, other values are assigned like pin name. CS: set defaults
	type type_port is record
		direction         : type_port_direction; -- example: "passive"
		coordinates       : type_coordinates;
		orientation       : type_orientation;
		port_name_visible : type_port_visible;
		pin_name_visible  : type_pin_visible;
		pin               : type_pin_name.bounded_string; -- example: "144" or in case of a BGA package "E14"

		-- pin_position_offset ?
		-- port swap level ?
		
		-- Kicad requirement: sometimes the supply port name has a special position
		name_position_offset : type_grid; 
	end record;

	-- Ports are collected in a map.
	package type_ports is new ordered_maps ( 
		key_type => type_port_name.bounded_string, -- like "CLOCK" or "CE"
		element_type => type_port); 



	
-- SHAPES 

	-- line width
	type type_line_width is new type_grid;

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
		radius  		: type_grid;
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
		radius  		: type_grid;
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
		pin, 
		text, -- text embedded in a symbol
		reference, value, commissioned, updated, author); -- text field placeholders

	-- Texts may be embedded in a symbol like "counter" or "&". So far they have the meaning "misc".
	-- CS: strings and texts within a unit symbol serve for documentation only. As long as
	-- there is no dedicated enumeration value available we choose "misc".
	-- CS: The meaning could be something like "documentation" some day.
	type type_symbol_text is new type_text (meaning => misc) with null record;
	package type_symbol_texts is new doubly_linked_lists (element_type => type_symbol_text);
	
	type type_symbol is record
		shapes		: type_shapes; -- the collection of shapes
		texts		: type_symbol_texts.list; -- the collection of texts (meaning misc)
		ports		: type_ports.map := type_ports.empty_map; -- the ports of the symbol
		
		-- Placeholders for component wide texts. To be filled with content when 
		-- a symbol is placed in the schematic:
		reference	: type_text_placeholder (meaning => et_libraries.reference);
		value		: type_text_placeholder (meaning => et_libraries.value);
		commissioned: type_text_placeholder (meaning => et_libraries.commissioned);
		updated		: type_text_placeholder (meaning => et_libraries.updated);
		author		: type_text_placeholder (meaning => et_libraries.author);
	end record;


	-- UNITS GENERAL

	unit_name_length_max : constant natural := 50;	
	package type_unit_name is new generic_bounded_length(unit_name_length_max); use type_unit_name;	
	
	unit_swap_level_max : constant natural := 10;
	type type_unit_swap_level is new natural range 0..unit_swap_level_max;
	unit_swap_level_default : constant := type_unit_swap_level'first;

	
	-- INTERNAL UNITS
	
	-- An internal unit is a symbol with a swap level.
	-- An internal unit is owned by a particular component exclusively.
	type type_unit_internal is record
		symbol		: type_symbol;
		coordinates	: type_coordinates;
		swap_level	: type_unit_swap_level := unit_swap_level_default;
	end record;

	-- Sometims for initiallization purposes we need a bare internal unit:
	bare_unit_internal : type_unit_internal;
	
	-- Internal units are collected in a map:
	package type_units_internal is new ordered_maps (
		key_type => type_unit_name.bounded_string, -- like "I/O-Bank 3" "A" or "B"
		element_type => type_unit_internal);


	-- EXTERNAL UNITS

	-- External units have a reference to an external symbol.
	-- External units are stored in a library and are shared by many components.
	type type_unit_reference is record
		library		: type_library_full_name.bounded_string; -- like /my_libraries/logig.sym
		name		: type_symbol_name.bounded_string;		 -- like "NAND" or "Resistor" or "Switch"
	end record;

	-- An external unit has a reference and a swap level.
	type type_unit_external is record
		reference	: type_unit_reference;
		coordinates	: type_coordinates;
		swap_level	: type_unit_swap_level;		
	end record;

	-- External units are collected in a map;
	package type_units_external is new ordered_maps (
		key_type => type_unit_name.bounded_string,		 -- like "I/O-Bank 3"
		element_type => type_unit_external);

	
	-- COMPONENTS

	-- The component value is something like 330R or 100n or 74LS00
	component_value_length_max : constant positive := 100;

	-- Define the characters that are allowed for a component value:
	component_value_characters : character_set := to_set (ranges => (('A','Z'),('a','z'),('0','9'))) or to_set('_');
	package type_component_value is new generic_bounded_length (component_value_length_max);

	function to_string ( value : in type_component_value.bounded_string) return string;
	-- Returns the given value as string.

	function component_value_valid (
	-- Returns true if the given component value meets certain conventions.									   
		value 		: in type_component_value.bounded_string;
		reference	: in et_general.type_component_reference) 
		return boolean;


	
	-- A component may have internal and/or external units.
	-- It also has text fields.
	type type_component (appearance : et_general.type_component_appearance) is record
		prefix			: et_general.type_component_prefix.bounded_string; -- R, C, IC, ...
		value			: type_component_value.bounded_string; -- 74LS00
		units_internal	: type_units_internal.map := type_units_internal.empty_map;
		units_external	: type_units_external.map := type_units_external.empty_map;
		commissioned	: et_string_processing.type_date;
		updated			: et_string_processing.type_date;
		author			: type_person_name.bounded_string;

		case appearance is

			-- If a component appears in the schematic only, it does not
			-- have any package variants.
			when et_general.sch => null;

			-- If a component appears in both schematic and layout it comes 
			-- with at least one package/footprint variant. We store variants in a map.
			when et_general.sch_pcb => 
				variants	: type_component_variants.map;
				datasheet	: type_component_datasheet.bounded_string;
				purpose		: type_component_purpose.bounded_string;
				partcode	: type_component_partcode.bounded_string;

			when others => null; -- CS
		end case;

	end record;

	-- Components are stored in a map.
	-- Within the map they are accessed by a key type_component_name (something like "CAPACITOR").
	package type_components is new indefinite_ordered_maps (
		key_type => type_component_name.bounded_string, -- example: "TRANSISTOR_PNP"
		element_type => type_component);
	use type_components;

	procedure write_component_properties ( component : in type_components.cursor; indentation : in natural := 0);
	-- Writes the properties of the component indicated by the given cursor.

	--package type_libraries is new indefinite_ordered_maps (	
	package type_libraries is new ordered_maps (
		key_type => type_library_full_name.bounded_string,
		element_type => type_components.map);
	
	
end et_libraries;

-- Soli Deo Gloria
