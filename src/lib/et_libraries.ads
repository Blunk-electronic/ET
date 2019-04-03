------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET LIBRARIES                            --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

--   For correct displaying set tab width in your edtior to 4.

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
--with ada.containers.vectors;

with et_coordinates;			use et_coordinates;
with et_string_processing;
with et_general;

package et_libraries is


	path_length_max : constant natural := 500; -- CS: increase if necessary
	
	
	-- The name of the person who has drawn, checked or approved something may have 100 characters which seems sufficient for now.
 	person_name_length : constant natural := 100;
	package type_person_name is new generic_bounded_length (person_name_length);

	function to_string (person : in type_person_name.bounded_string) return string;
	-- Returns the given person name as string.





-- TEXT & FIELDS

	-- Texts of any kind must have a size between 0.1 and 50mm
	subtype type_text_size is type_distance range 0.1 .. 50.0; -- unit is mm

	text_size_default : constant type_text_size := 1.3;

	function to_text_size (size : in type_distance) return type_text_size;
	-- Converts given distance to type_text_size. Raises error on excessive text size.
	
	function to_string (
		size 		: in type_text_size;
		preamble	: in boolean := true) return string;
	-- Returns the given text size as string.

	subtype type_text_line_width is type_distance range 0.0 .. 5.0; -- unit is mm -- CS: minimum of 0.0 reasonable ?

	function to_string (width : in type_text_line_width) return string;
	-- Returns the given line width as string.
	
	type type_text_style is (NORMAL, ITALIC, BOLD, ITALIC_BOLD);
	function to_string (style : in type_text_style) return string;
	function to_text_style (style : in string) return type_text_style;
	
	--type type_text_visible is (YES, NO);
	
	type type_text_alignment_horizontal is (LEFT, CENTER, RIGHT);
	function to_string (alignment : in type_text_alignment_horizontal) return string;
	function to_alignment_horizontal (alignment : in string) return type_text_alignment_horizontal;
	
	type type_text_alignment_vertical is (TOP, CENTER, BOTTOM);
	function to_string (alignment : in type_text_alignment_vertical) return string;
	function to_alignment_vertical (alignment : in string) return type_text_alignment_vertical;
	
	type type_text_alignment is record
		horizontal	: type_text_alignment_horizontal := CENTER;
		vertical	: type_text_alignment_vertical := CENTER;
	end record;

	function to_string (alignment : in type_text_alignment) return string;
	
	type type_text_meaning is (
		REFERENCE,		-- for things like R301 or X9
		VALUE,			-- for component values like "200R"
		PACKGE,			-- for component packages like SOT23
		DATASHEET,		-- for url to datasheet
		PURPOSE,		-- for the purpose of the component in the design.
		MISC); -- CS: others ?
	-- CS: The type_text_meaning covers more than actually required by ET.
	-- It also includes text meanings of kicad. Rework required !
	
	text_meaning_default : constant type_text_meaning := MISC;
	
	function to_string (meaning : in type_text_meaning) return string;
	function to_text_meaning (meaning : in string) return type_text_meaning;

	subtype type_placeholder_text_size is type_distance range 1.0 .. 5.0; -- unit is mm
	placeholder_text_size_default : constant type_placeholder_text_size := 1.3;

	function to_component_attribute_text_size (text : in string) return type_placeholder_text_size;
	-- Converts a string to a type_placeholder_text_size.
	
	-- These are basic properties a text has got:
	type type_text_basic is tagged record
        size    	: type_text_size := text_size_default;
        style		: type_text_style := type_text_style'first;
        line_width	: type_text_line_width := type_text_line_width'first; -- CS: use a general type_line_width ?
        rotation	: type_rotation_text := 0;
		alignment	: type_text_alignment;
	end record;

	-- A text may have up to 200 characters which seems sufficient for now.
 	text_length_max : constant natural := 200;
	package type_text_content is new generic_bounded_length (text_length_max); use type_text_content;

	function to_string (text_content : in type_text_content.bounded_string) return string;
	function to_content (content : in string) return type_text_content.bounded_string;
	
	procedure check_text_content_length (content : in string);
	-- Tests if the content is not longer than allowed.
	
	-- This is a placeholder for a text. It does not have content yet, but a meaning:
	type type_text_placeholder (meaning : type_text_meaning) is new type_text_basic with record
		position : type_point;
	end record;

	procedure write_placeholder_properties (
	-- Writes the properties of the given placeholder.
		placeholder		: in type_text_placeholder;
		log_threshold	: in et_string_processing.type_log_level);
	
	-- This is a real text with its content:
	type type_text is new type_text_placeholder with record
        content		: type_text_content.bounded_string;
	end record;

	

	procedure write_text_properies (
	-- Outputs the properties of the given text.
		text 			: in et_libraries.type_text;
		log_threshold	: in et_string_processing.type_log_level);

	function content (text : in type_text) return string;
	-- Returns the content of the given text as string.


	
	
-- TERMINALS
	
	subtype type_terminal_name_text_size is type_distance range 1.0 .. 5.0; -- unit is mm
	terminal_name_text_size_default : constant type_terminal_name_text_size := 1.3;

	function to_terminal_name_text_size (text : in string) return type_terminal_name_text_size;
	-- Converts a string to type_terminal_name_text_size.


	
	
-- PORTS

	-- A port is something where a net can be attached to.
	-- The name of a port represents the function of the port like (A14 or RST_N)
	subtype type_port_length is type_distance range 0.0 .. 20.0; -- unit is millimeters. CS: reasonable limits ?
	
	-- The port has an electrical direction:
	type type_port_direction is (
		PASSIVE,		-- almost all passive components like resistors, capacitors, .. have such ports

		INPUT_ANALOG,	-- signal input analog
		INPUT_DIGITAL,	-- signal input digital

		OUTPUT_ANALOG,	-- signal output analog		
		OUTPUT_DIGITAL,	-- signal outputs

		BIDIR_DIGITAL,	-- bidirectional ports
		-- CS BIDIR_ANALOG, ??

		POWER_OUT,		-- a power source like power symbol (VCC, GND, ..)
		POWER_IN,		-- a power sink like power ports of ICs

		NOT_CONNECTED	-- advised by manufacturer to be left unconnected
		);

	port_direction_default : constant type_port_direction := OUTPUT_ANALOG; 
	-- CS: should be the one with the most severe implications.
	
	function to_string (direction : in type_port_direction) return string;
	function to_port_direction (direction : in string) return type_port_direction;
	
	type type_port_name_visible is (YES, NO);
	function to_string (visible : in type_port_name_visible) return string;
	function to_port_name_visible (visible : in string) return type_port_name_visible;	

	
	type type_terminal_name_visible is (YES, NO);
	function to_string (visible : in type_terminal_name_visible) return string;	
	function to_terminal_name_visible (visible : in string) return type_terminal_name_visible;
	
 	port_name_length_max : constant natural := 100;
	package type_port_name is new generic_bounded_length (port_name_length_max);
	use type_port_name;

	function to_string (port : in type_port_name.bounded_string) return string;
	function to_port_name (name : in string) return type_port_name.bounded_string;
	
	subtype type_port_name_text_size is type_distance range 1.0 .. 5.0; -- unit is mm
	port_name_text_size_default : constant type_port_name_text_size := 1.3;

	function to_port_name_text_size (text : in string) return type_port_name_text_size;
	-- Converts a string to type_port_name_text_size.

	-- line width
	subtype type_line_width is type_distance; -- CS reasonable range
	
	line_width_port_default : constant type_line_width := 0.2;
	
	type type_port_base is tagged record 	-- CS: set defaults	
		position			: type_point;
		length				: type_port_length; 
		-- CS line_width	: type_line_width := line_width_port_default;
		rotation			: type_rotation := 0;
		
		port_name_visible		: type_port_name_visible;
		port_name_size			: type_port_name_text_size;
		
		terminal_name_visible	: type_terminal_name_visible;
		terminal_name_size		: type_terminal_name_text_size;
		-- CS: port swap level ? -> would require a derived new type
	end record;

	-- Sensitity of inputs:
	type type_sensitivity_edge is (
		NONE, 		-- passive and analog
		RISING,		-- digital
		FALLING,	-- digital
		ANY			-- digtial
		);
	sensitivity_edge_default : constant type_sensitivity_edge := NONE;
	function to_string (sensitivity : in type_sensitivity_edge) return string;
	function to_sensitivity_edge (sensitivity : in string) return type_sensitivity_edge;

	type type_sensitivity_level is (NONE, LOW, HIGH); -- CS NONE required ?
	sensitivity_level_default : constant type_sensitivity_level := HIGH; -- CS good idea ?
	function to_string (sensitivity : in type_sensitivity_level) return string;
	function to_sensitivity_level (sensitivity : in string) return type_sensitivity_level;
	
	type type_output_inverted is (NO, YES);
	output_inverted_default : constant type_output_inverted := NO;
	function to_string (inverted : in type_output_inverted) return string;
	function to_output_inverted (inverted : in string) return type_output_inverted;

	type type_output_weakness is (
		NONE, -- push-pull
		WEAK0, WEAK1, -- requires external pull-down/up resistor
		PULL0, PULL1  -- internal pull-down/up resistor
		);
	output_weakness_default : constant type_output_weakness := NONE;
	function to_string (weakness : in type_output_weakness) return string;
	function to_output_weakness (weakness : in string) return type_output_weakness;

	type type_output_tristate is (NO, YES);
	output_tristate_default : constant type_output_tristate := NO;
	function to_string (tristate : in type_output_tristate) return string;
	function to_output_tristate (tristate : in string) return type_output_tristate;

	
	type type_power_level is (LEVEL_ZERO, LEVEL_POSITIVE, LEVEL_NEGATIVE);
	-- The prefix "LEVEL_" is a workaround because GNAT regards "POSITIVE" as keyword.
	-- CAUTION: Adapt functions to_string and to_power_level when changing anything here !
	
	port_power_level_default : constant type_power_level := LEVEL_ZERO;

	function to_string (level : in type_power_level) return string;
	-- Converts the power level (like LEVEL_POSITIVE) to a string (like positive).
	-- The prefix LEVEL_ is removed.
	
	function to_power_level (level : in string) return type_power_level;	
	-- Converts the power level (like positive) to power level (like LEVEL_POSITIVE).
	-- The prefix LEVEL_ is prepended.


	
	type type_port (direction : type_port_direction) is new type_port_base with record 
		case direction is
			when INPUT_DIGITAL =>
				sensitivity_edge		: type_sensitivity_edge;
				sensitivity_level		: type_sensitivity_level;

			when OUTPUT_ANALOG =>
				output_analog_tristate	: type_output_tristate;
				output_analog_weakness	: type_output_weakness;
				
			when OUTPUT_DIGITAL =>
				output_digital_inverted	: type_output_inverted;
				output_digital_tristate	: type_output_tristate;
				output_digital_weakness	: type_output_weakness;
				
			when BIDIR_DIGITAL =>
				output_inverted			: type_output_inverted;
				output_tristate			: type_output_tristate;				
				output_weakness			: type_output_weakness;
				input_sensitivity_edge	: type_sensitivity_edge;
				input_sensitivity_level	: type_sensitivity_level;
				
			when POWER_OUT | POWER_IN =>
				level	: type_power_level;
				
			when others => null;
		end case;
	end record;
	
	-- Ports of a symbol are collected in a map. A map because a port with a certain name
	-- like GND may exist only once in the symbol. Te symbol is an abstraction of a
	-- function block within a device. It does not matter how many GND terminals exist
	-- at the package (footprint):
	package type_ports is new indefinite_ordered_maps (
		key_type		=> type_port_name.bounded_string, -- CLOCK, CE, VDD, GND
		element_type	=> type_port);


	
-- DEVICE VALUES
	-- The device value is something like 330R or 100n or 74LS00
	value_length_max : constant positive := 50;

	-- Define the characters that are allowed for a value:
	value_characters : character_set := 
		to_set (ranges => (('A','Z'),('a','z'),('0','9'))) 
		or to_set ('_')
		or to_set ('-');
	
	package type_value is new generic_bounded_length (value_length_max);

	function to_string (value : in type_value.bounded_string) return string;
	function to_value (value : in string) return type_value.bounded_string;
	
	function value_length_valid (value : in string) return boolean;
	-- Tests if the given value is longer than allowed. Returns false if too long.
	-- Returns true if length is in allowed range.

	function truncate (value : in string) return type_value.bounded_string;
	
	function value_characters_valid (
		value		: in type_value.bounded_string;
		characters	: in character_set := value_characters)
		return boolean;
	-- Tests if the given value contains only valid characters as specified
	-- by given character set. Returns false if invalid character found.
	-- Issues warning.	





	

-- DEVICE NAMES
	-- A device name consists of a prefix (like R, C, IC, ..)
	-- and a consecutive number. Both form something like "IC702"
	device_name_prefix_characters : character_set := to_set (span => ('A','Z'));
	device_name_prefix_length_max : constant natural := 10; -- CS: there is no reason to work with longer prefixes.
	package type_device_name_prefix is new generic_bounded_length (device_name_prefix_length_max);
	use type_device_name_prefix;

	function to_string (prefix : in type_device_name_prefix.bounded_string) return string;
	function to_prefix (prefix : in string) return type_device_name_prefix.bounded_string;

	procedure check_prefix_length (prefix : in string);
	-- Tests if the given prefix is longer than allowed.
	
	procedure check_prefix_characters (prefix : in type_device_name_prefix.bounded_string);
	-- Tests if the given prefix contains only valid characters.
	-- Raises exception if invalid character found.
	
	type type_device_name_element is (PREFIX, ID);
	device_name_prefix_default : constant type_device_name_prefix.bounded_string := to_bounded_string("?");

	subtype type_device_name_index is natural range natural'first .. 99_999; -- R1..R99999, IC1..IC99999 should be enough
	device_name_index_default : constant type_device_name_index := 0;

	function to_string (index : in type_device_name_index) return string;
	function to_device_name_index (index : in string) return type_device_name_index;

	subtype type_device_name_index_width is positive range positive'first .. 5; -- see number of digits of type_device_name_index
	
	type type_device_name is record -- CS: should be private
		prefix		: type_device_name_prefix.bounded_string := device_name_prefix_default; -- like "IC"
		id			: type_device_name_index := device_name_index_default; -- like "303"
		id_width	: type_device_name_index_width; -- the number of digits of the id. 3 in case of an id of 303
		-- NOTE: This allows something like R091 or IC0 (there are reasons for such strange things ...)
	end record;

	function to_device_name (
	-- Converts a string like "IC303" to a composite type_device_name.
	-- Raises constraint error if prefix contains invalid characters.
	-- Raises constraint error if id contains non-digit characters.
	-- Leading zeroes in the index are removed. R002 becomes R2.
		text_in : in string)
		return type_device_name;
	
	function to_string (name : in type_device_name) return string;
	-- Returns the given device name as string.
	-- Prepends leading zeros according to name.id_width.
	
	function prefix (name : in type_device_name) return type_device_name_prefix.bounded_string;
	-- Returns the prefix of the given device name.

	
-- DEVICE APPEARANCE	
	type type_device_appearance is ( 
		sch,		-- a device that exists in the schematic only (like power symbols)
		sch_pcb,	-- a device that exists in both schematic and soldered on a pcb
		pcb			-- a device that exists on the pcb only (like a fiducial)		
		-- CS: cable 
		-- CS: wire
		-- ...
		);

	function to_string (
		appearance	: in type_device_appearance;
		verbose		: in boolean := false)
		return string;
	-- Returns the given device appearance as string.

	function to_appearance (appearance : in string) return type_device_appearance;
	


	
-- PACKAGES
	-- A package is something like "SOT32" or "NDIP14". It is a more or less standardized (JEDEC)
	-- designator for the housing or the case of an electronical component. The package name is independed of
	-- the actual purpose of a device. An LED can have an SOT23 package and a transistor can also come in an SOT23.

	-- device package names like "SOT23" or "TO220" are stored in bounded strings:
	component_package_name_characters : character_set := to_set 
		(ranges => (('a','z'),('A','Z'),('0','9'))) 
		or to_set('.')
		or to_set('_'); 

	component_package_name_length_max : constant positive := 100;
	package type_component_package_name is new generic_bounded_length (component_package_name_length_max);

	function to_string (packge : in type_component_package_name.bounded_string) return string;
	-- Returns the given package name as as string.
	-- CS: provide a parameter that turns the preamble on/off

	function to_package_name (package_name : in string) return type_component_package_name.bounded_string;
	-- Converts a string to a type_component_package_name.
	
	procedure check_package_name_length (packge : in string);
	-- Tests if the given package name is longer than allowed.
	
	procedure check_package_name_characters (
		packge		: in type_component_package_name.bounded_string;
		characters	: in character_set := component_package_name_characters);
	-- Tests if the given package name contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.
	

-- TERMINALS
	type type_terminal_count is new count_type; -- CS: limit to a reasonable range ?

	function to_string (terminals : in type_terminal_count) return string;
	-- Returns the given number of terminals as string.



-- PARTCODES
	-- The component part code is THE key into the ERP system of the user. It can be a crytic SAP number
	-- or something human readable like "R_PAC_S_0805_VAL_100R_PMAX_125_TOL_5".
	-- The keywords for the latter can be specified via the conventions file. See package "convention".
	partcode_characters : character_set := to_set
		(ranges => (('a','z'),('A','Z'),('0','9'))) or to_set ('_'); 
	partcode_length_max : constant positive := 100;
	package type_partcode is new generic_bounded_length (partcode_length_max);
	partcode_default : constant string := "dummy";
	
	function to_string (partcode : in type_partcode.bounded_string) return string;
	function to_partcode (partcode : in string) return type_partcode.bounded_string;

	function partcode_length_valid (partcode : in string) return boolean;
	-- Returns true if length of given partcode is ok. Issues warning if not.
	
	function partcode_characters_valid (
		partcode	: in type_partcode.bounded_string;
		characters	: in character_set := partcode_characters) return boolean;
	-- Tests if the given partcode contains only valid characters as specified
	-- by given character set. Returns false if not. Issues warning.





	-- lines
	type type_line is record
		start_point : type_point;
		end_point   : type_point;
		width		: type_line_width;
	end record;
	package type_lines is new doubly_linked_lists (type_line);

	-- Arcs
	type type_arc is tagged record
		center		: type_point;
		radius  	: type_distance;
		start_point	: type_point;
		end_point	: type_point;
		width		: type_line_width;
	end record;
	package type_arcs is new doubly_linked_lists (type_arc);

	type type_circle_filled is (NO, YES);
	function to_string (filled : in type_circle_filled) return string;
	function to_circle_filled (filled : in string) return type_circle_filled;
	
	-- Circles
	type type_circle_base is tagged record
		center		: type_point;
		radius  	: type_distance;
		width		: type_line_width;
	end record;

	type type_circle is new type_circle_base with record
		filled		: type_circle_filled := NO;
	end record;
	package type_circles is new doubly_linked_lists (type_circle);

	-- Shapes are wrapped in a the type_shapes:
	type type_shapes is record
		lines		: type_lines.list 		:= type_lines.empty_list;
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

	-- Texts may be embedded in a symbol like "counter" or "&". So far they have the meaning "misc".
	-- CS: strings and texts within a unit symbol serve for documentation only. As long as
	-- there is no dedicated enumeration value available we choose "misc".
	-- CS: The meaning could be something like "documentation" some day.
	type type_symbol_text is new type_text (meaning => MISC) with null record;
	package type_symbol_texts is new doubly_linked_lists (type_symbol_text);

	type type_symbol_base is tagged record		
		texts : type_symbol_texts.list; -- the collection of texts (meaning misc)
	end record;

	type type_symbol (appearance : type_device_appearance) is new type_symbol_base with record
		shapes	: type_shapes; -- the collection of shapes
		ports	: type_ports.map;
		case appearance is
			when SCH_PCB =>
				-- Placeholders for component wide texts. To be filled with content when 
				-- a symbol is placed in the schematic:
				reference	: type_text_placeholder (meaning => et_libraries.REFERENCE);
				value		: type_text_placeholder (meaning => et_libraries.VALUE);
				purpose 	: type_text_placeholder (meaning => et_libraries.PURPOSE);
			when SCH => null;				
			when others => null; -- CS
		end case;
	end record;



	unit_name_length_max : constant natural := 50;	
	-- CS unit_name_characters, length check, character check
	package type_unit_name is new generic_bounded_length (unit_name_length_max); use type_unit_name;

	unit_name_default : constant type_unit_name.bounded_string := type_unit_name.to_bounded_string ("");
	
	function to_string (unit_name : in type_unit_name.bounded_string) return string;
	function to_unit_name (unit_name : in string) return type_unit_name.bounded_string;
	
	unit_swap_level_max : constant natural := 10;
	type type_unit_swap_level is new natural range 0..unit_swap_level_max;
	unit_swap_level_default : constant := type_unit_swap_level'first;

	function to_string (swap_level : in type_unit_swap_level) return string;
	function to_swap_level (swap_level : in string) return type_unit_swap_level;	

	type type_unit_add_level is (
		NEXT, 		-- should be default. for things like logic gates, multi-OP-Amps, ...
		REQUEST, 	-- for power supply 
		CAN,		-- for things like optional relay contacts
		ALWAYS,		-- 
		MUST);		-- for things like relay coils

	unit_add_level_default : constant type_unit_add_level := type_unit_add_level'first;
	
	function to_string (add_level : in type_unit_add_level) return string;
	function to_add_level (add_level : in string) return type_unit_add_level;
	
	-- An internal unit is a symbol with a swap level.
	-- An internal unit is owned by the particular device exclusively.
	type type_unit_internal (appearance : type_device_appearance) is record
		symbol		: type_symbol (appearance);
		position	: type_point; -- the position of the unit inside the device editor
		swap_level	: type_unit_swap_level := unit_swap_level_default;
		add_level	: type_unit_add_level := type_unit_add_level'first;
	end record;

	-- Internal units are collected in a map:
	package type_units_internal is new indefinite_ordered_maps (
		key_type		=> type_unit_name.bounded_string, -- like "I/O-Bank 3" "A" or "B"
		element_type	=> type_unit_internal);

	-- SYMBOLS
	package type_symbol_model_file is new generic_bounded_length (path_length_max);
	function to_string (name : in type_symbol_model_file.bounded_string) return string;
	function to_file_name (name : in string) return type_symbol_model_file.bounded_string;
	
	-- An external unit has a reference and a swap level.
	type type_unit_external is record
		file		: type_symbol_model_file.bounded_string; -- like /libraries/symbols/NAND.sym -- CS rename to model
		position	: type_point := zero; -- the position within the device editor
		swap_level	: type_unit_swap_level := unit_swap_level_default;
		add_level	: type_unit_add_level := type_unit_add_level'first;
	end record;

	-- External units are collected in a map;
	package type_units_external is new ordered_maps (
		key_type		=> type_unit_name.bounded_string, -- like "I/O-Bank 3"
		element_type	=> type_unit_external);


	
-- TERMINALS
	-- A terminal is where electrical energy is fed in or provided by a component.
	-- Other CAE systems refer to "pins" or "pads". In order to use only a single word
	-- we further-on speak about "terminals".
	-- The name of a terminal may have 10 characters which seems sufficient for now.
	-- CS: character set, length check, charcter check
 	terminal_name_length_max : constant natural := 10;
	package type_terminal_name is new generic_bounded_length (terminal_name_length_max);
	use type_terminal_name;

	function to_string (terminal : in type_terminal_name.bounded_string) return string;
	-- Returns the given terminal name as string.

	function to_terminal_name (terminal : in string) return type_terminal_name.bounded_string;
	-- Converts a string to a type_terminal_name.
	
-- PACKAGE VARIANTS
	-- The variant is usually a suffix in a component value, given by its manufacturer. The variant is a manufacturer
	-- specific abbrevation for the package a component comes with.
	-- Example: An opamp made by TI can be the type TL084N or TL084D. N means the NDIP14 package
	-- whereas D means the SO14 package.
	-- If a component has package variants, a suffix after the component type indicates the package
	-- The variant name is manufacturer specific. example: TL084D or TL084N
	-- component package variant names like "N" or "D" are stored in bounded strings.
	component_variant_name_characters : character_set := 
		to_set (ranges => (('A','Z'),('a','z'),('0','9'))) or to_set ("_-"); -- CS rename to package_variant_name_characters
	
	component_variant_name_length_max : constant positive := 50;
	package type_component_variant_name is new generic_bounded_length (component_variant_name_length_max);
	use type_component_variant_name;

	function to_string (package_variant : in type_component_variant_name.bounded_string) return string;
	-- converts a type_component_variant_name to a string.
	
	function to_component_variant_name (variant_name : in string) 
		return type_component_variant_name.bounded_string;
	-- converts a string to a variant name

	procedure check_variant_name_length (variant_name : in string);
	-- tests if the given variant name is not longer than allowed
	
	procedure check_variant_name_characters (
		variant		: in type_component_variant_name.bounded_string;
		characters	: in character_set := component_variant_name_characters);
	-- Tests if the given variant name contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.

	type type_port_in_terminal_port_map is record
		name	: type_port_name.bounded_string; -- CLK, CE, VSS
		unit	: type_unit_name.bounded_string; -- GPIO_BANK_3
	end record;
	
	package type_terminal_port_map is new ordered_maps (
		key_type 		=> type_terminal_name.bounded_string, -- H7, 14
		element_type 	=> type_port_in_terminal_port_map); -- unit A, OE1


	-- To handle names of package models like libraries/packages/smd/SOT23.pac use this:
	package type_package_model_file is new generic_bounded_length (path_length_max);
	function to_string (name : in type_package_model_file.bounded_string) return string;
	function to_file_name (name : in string) return type_package_model_file.bounded_string;
	
	type type_component_variant is record -- CS rename to type_package_variant
		package_model		: type_package_model_file.bounded_string; -- libraries/packages/smd/SOT23.pac
		terminal_port_map	: type_terminal_port_map.map; -- which port is connected with with terminal
	end record;

	package type_component_variants is new ordered_maps ( -- CS rename to type_package_variants
		key_type 		=> type_component_variant_name.bounded_string, -- D, N
		element_type 	=> type_component_variant);

	type type_terminal is record
		name	: type_terminal_name.bounded_string; -- H7
		unit	: type_unit_name.bounded_string; -- IO-BANK1
		port	: type_port_name.bounded_string; -- GPIO3
	end record;

	function to_string (
		terminal	: in type_terminal;
		show_unit	: in boolean := false;
		preamble	: in boolean := true)
		return string;
	-- Returns the given terminal as string. 
	-- If show_unit is true, the unit name is output.
	-- If preamble is true, each property of the terminal is headed by a short preamble.
	


-- DEVICES
	type type_device (appearance : type_device_appearance) is record
		prefix			: type_device_name_prefix.bounded_string; -- R, C, IC, ...
		units_internal	: type_units_internal.map := type_units_internal.empty_map;
		units_external	: type_units_external.map := type_units_external.empty_map;

		case appearance is

			-- If a device appears in the schematic only, it is a virtual component 
			-- and thus does not have any package variants.
			-- Such components are power symbols. Later when building netlists
			-- those components enforce net names (like GND or P3V3).
			when sch => 
				null;

			-- If a device appears in both schematic and layout it comes 
			-- with at least one package/footprint variant. We store variants in a map.
			when sch_pcb => 
				value		: type_value.bounded_string; -- 74LS00
				--partcode	: type_component_partcode.bounded_string;
				variants	: type_component_variants.map;
				
			when others => null; -- CS
		end case;

	end record;

	
	
-- STORAGE
	
	package type_symbols is new indefinite_ordered_maps (
		key_type		=> type_symbol_model_file.bounded_string, -- ../libraries/symbols/NAND.sym
		"<"				=> type_symbol_model_file."<",
		element_type	=> type_symbol);

	symbol_library_file_extension : constant string (1..3) := "sym";

	-- HERE RIG WIDE SYMBOLS ARE KEPT:	
	symbols : type_symbols.map;

-- DEVICES
	package type_device_model_file is new generic_bounded_length (path_length_max); -- ../lbr/logic_ttl/7400.dev
	function to_string (name : in type_device_model_file.bounded_string) return string;
	function to_file_name (name : in string) return type_device_model_file.bounded_string;
	
	package type_devices is new indefinite_ordered_maps (
		key_type 		=> type_device_model_file.bounded_string, -- ../libraries/devices/logic_ttl/7400.dev
		"<"				=> type_device_model_file."<",
		element_type	=> type_device);

	device_library_file_extension : constant string (1..3) := "dev";

	-- HERE RIG WIDE DEVICES ARE KEPT:
	devices : type_devices.map;


	
-- DRAWING FRAME

	-- $ET_FRAMES/drawing_frame_version_1.frm
	frame_template_name_length_max : constant positive := 300;
	package type_frame_template_name is new generic_bounded_length (frame_template_name_length_max);

	frame_template_name_dummy : constant type_frame_template_name.bounded_string := 
		type_frame_template_name.to_bounded_string ("dummy_frame");
	
	function to_string (name : in type_frame_template_name.bounded_string) return string;
	function to_template_name (name : in string) return type_frame_template_name.bounded_string;
	
    type type_title_block_line is record
		coordinates_start : et_coordinates.type_point;
		coordinates_end   : et_coordinates.type_point;
    end record;

	package type_title_block_lines is new doubly_linked_lists (type_title_block_line);
    
 	title_block_text_length_max : constant natural := 200;
	package type_title_block_text_content is new generic_bounded_length (title_block_text_length_max);

	type type_title_block_text_meaning is ( 
		PROJECT, TITLE, 
        DRAWN_BY, CHECKED_BY, APPROVED_BY, 
        DRAWN_DATE, CHECKED_DATE, APPROVED_DATE,
        COMPANY,
		REVISION, MISC);
	
	type type_title_block_text is record
		meaning			: type_title_block_text_meaning;
 		coordinates		: et_coordinates.type_point;
		text			: type_title_block_text_content.bounded_string; -- CS: rename to content
 		size			: et_libraries.type_text_size;
 		rotation		: et_coordinates.type_rotation;
		-- CS: font, ...
 	end record;

	package type_title_block_texts is new doubly_linked_lists (type_title_block_text);

    -- the final title block
    type type_title_block is record
        coordinates     : et_coordinates.type_point; -- CS rename to position
        lines           : type_title_block_lines.list;
        texts           : type_title_block_texts.list;
    end record;

    -- A drawing frame consists of straight lines and texts.
    -- The text is a character at the x/y border that helps to locate objects.
    type type_frame_line is record
		coordinates_start : et_coordinates.type_point;
        coordinates_end   : et_coordinates.type_point;
	end record;
	
	package type_frame_lines is new doubly_linked_lists (type_frame_line);

	type type_frame_text is record
		coordinates		: et_coordinates.type_point; -- CS rename to position
		text			: character_set := et_string_processing.general_characters; -- CS rename to content
		size			: et_libraries.type_text_size;
		rotation		: et_coordinates.type_rotation;
		-- CS: font, ...
	end record;
	
	package type_frame_texts is new doubly_linked_lists (type_frame_text);

	-- the final drawing frame
	-- NOTE: The native drawing frame has its lower left corner at position x/y 0/0. always.
    type type_frame is tagged record
        paper_size      : et_general.type_paper_size; -- the size of the paper
        size_x, size_y  : et_coordinates.type_distance; -- the dimensions of the frame (should fit into paper_size) 
        lines           : type_frame_lines.list;
		texts           : type_frame_texts.list;
		title_block		: type_title_block;
    end record;

--     -- There are lots of drawing frames in a schematic. We store them in a vector:
-- 	package type_frames is new vectors (
-- 		index_type		=> et_coordinates.type_submodule_sheet_number,
-- 		element_type	=> type_frame);

	
		
end et_libraries;

-- Soli Deo Gloria
