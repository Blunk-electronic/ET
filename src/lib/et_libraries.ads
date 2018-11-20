------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET LIBRARIES                            --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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

-- LIBRARY NAMES AND DIRECTORIES

	library_name_length_max : constant natural := 100; -- CS: increase if necessary



-- DEVICES
	-- If a library is fully specified with path, name and extension we store them in bounded strings:
	device_library_name_max : constant positive := 300 + library_name_length_max + 4;
	package type_device_library_name is new generic_bounded_length (device_library_name_max);
	
	function to_string (device_library_name : in type_device_library_name.bounded_string) return string;
	-- Returns the given device library name as string;

	function to_device_library_name (device_library_name : in string) return type_device_library_name.bounded_string;
	-- converts a string to a device library name.
	

-- SYMBOLS
	-- If a library is fully specified with path, name and extension we store them in bounded strings:
	symbol_library_name_max : constant positive := 300 + library_name_length_max + 4;
	package type_symbol_library_name is new generic_bounded_length (symbol_library_name_max);
	
	function to_string (symbol_library_name : in type_symbol_library_name.bounded_string) return string;
	-- Returns the given symbol library name as string;

	function to_symbol_library_name (symbol_library_name : in string) return type_symbol_library_name.bounded_string;
	-- converts a string to a symbol library name.

	
-- PACKAGES
	-- If a library is fully specified with path, name and extension we store them in bounded strings:
	package_library_name_max : constant positive := 300 + library_name_length_max + 4;
	package type_package_library_name is new generic_bounded_length (package_library_name_max);
	
	function to_string (package_library_name : in type_package_library_name.bounded_string) return string;
	-- Returns the given package library name as string;

	function to_package_library_name (package_library_name : in string) return type_package_library_name.bounded_string;
	-- converts a string to a package library name.

	
	
	-- When accessing library files we need this:
	library_handle : ada.text_io.file_type;
	
	
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

	subtype type_text_line_width is type_distance range 0.0 .. 5.0; -- unit is mm

	function to_string (width : in type_text_line_width) return string;
	-- Returns the given line width as string.
	
	type type_text_style is (NORMAL, ITALIC, BOLD, ITALIC_BOLD);
	function to_string (style : in type_text_style) return string;
	
	type type_text_visible is (YES, NO);
	
	type type_text_alignment_horizontal is (LEFT, CENTER, RIGHT);
	function to_string (alignment : in type_text_alignment_horizontal) return string;
	
	type type_text_alignment_vertical is (TOP, CENTER, BOTTOM);
	function to_string (alignment : in type_text_alignment_vertical) return string;
	
	type type_text_alignment is record
		horizontal	: type_text_alignment_horizontal := center;
		vertical	: type_text_alignment_vertical := center;
	end record;

	function to_string (alignment : in type_text_alignment) return string;
	
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

	subtype type_placeholder_text_size is type_distance range 1.0 .. 5.0; -- unit is mm
	placeholder_text_size_default : constant type_placeholder_text_size := 1.3;

	function to_component_attribute_text_size (text : in string) return type_placeholder_text_size;
	-- Converts a string to a type_placeholder_text_size.
	
	-- These are basic properties a text has got:
	type type_text_basic is tagged record
		position	: type_2d_point;
        size    	: type_text_size := text_size_default;
        style		: type_text_style := type_text_style'first;
        line_width	: type_text_line_width := type_text_line_width'first; -- CS: use a general type_line_width ?
        orientation	: type_angle := 0.0;
		visible		: type_text_visible := yes; -- unless assigned otherwise all texts are visible by default
		alignment	: type_text_alignment;
	end record;

	-- A text may have up to 200 characters which seems sufficient for now.
 	text_length_max : constant natural := 200;
	package type_text_content is new generic_bounded_length (text_length_max); use type_text_content;

	function to_string (text_content : in type_text_content.bounded_string) return string;
	
	procedure check_text_content_length (content : in string);
	-- Tests if the content not longer than allowed.
	
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
		PASSIVE,	-- almost all passive components like resistors, capacitors, .. have such ports
		INPUT,		-- signal inputs
		OUTPUT,		-- signal outputs
		BIDIR,		-- bidirectional ports
		TRISTATE,	-- tristate ports
		UNKNOWN,
		POWER_OUT,	-- a power source like power symbol (VCC, GND, ..)
		POWER_IN,	-- a power sink like power ports of ICs
		WEAK1,		-- a port with internal pull-up resistor
		WEAK0,		-- a port with internal pull-down resistor
		NOT_CONNECTED	-- advised by manufacturer to be left unconnected
		);

	function to_string (
		direction	: in type_port_direction;
		preamble	: in boolean := true) return string;
	-- Returns the given port direction as string.

	type type_port_name_visible is (ON, OFF);
	type type_terminal_name_visible is (ON, OFF);

	type type_port_style is ( -- CS: find a better name, move it to et_kicad
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
	
 	port_name_length_max : constant natural := 100;
	package type_port_name is new generic_bounded_length (port_name_length_max);
	use type_port_name;

	function to_string (port : in type_port_name.bounded_string) return string;
	-- Returns the given port name as string.

	subtype type_port_name_text_size is type_distance range 1.0 .. 5.0; -- unit is mm
	port_name_text_size_default : constant type_port_name_text_size := 1.3;

	function to_port_name_text_size (text : in string) return type_port_name_text_size;
	-- Converts a string to type_port_name_text_size.
	
	
	-- Initially, a port has at least a name.
	type type_port_base is tagged record
		name				: type_port_name.bounded_string; -- like CLOCK or CE
	end record;
	
	type type_port is new type_port_base with record 	-- CS: set defaults
		direction			: type_port_direction; -- example: "passive"
		style				: type_port_style;
		coordinates			: type_2d_point; -- CS: rename to position
		length				: type_port_length; 
		orientation			: type_angle; -- CS: rename to rotation
		port_name_visible	: type_port_name_visible;
		terminal_visible	: type_terminal_name_visible; -- CS: rename to terminal_name_visible
		port_name_size		: type_port_name_text_size;
		terminal_name_size	: type_terminal_name_text_size;
		-- CS: port swap level ? -> would require a derived new type
	end record;

	-- Ports of a component are collected in a simple list. A list, because multiple ports
	-- with the same name (but differing terminal names) may exist. For example lots of GND
	-- ports at FPGAs.
	package type_ports is new doubly_linked_lists (type_port); 


	
-- COMPONENT GENERIC NAMES
	-- The generic name of a component in the library is something like TRANSISTOR_NPN or RESISTOR
 	component_generic_name_length_max : constant natural := 100;
	package type_component_generic_name is new generic_bounded_length (component_generic_name_length_max);
	use type_component_generic_name;
	-- Only those characters are allowed for the generic component name.
	-- See et_import.check_component_name for customization depending on CAD format.
	component_generic_name_characters : character_set := to_set 
		(ranges => (('A','Z'),('0','9'))) 
		or to_set('-') 
		or to_set('_'); 

	procedure check_generic_name_characters (
	-- Checks if the the given generic component name meets certain conventions.
		name : in type_component_generic_name.bounded_string; -- TRANSISTOR_NPN
		characters : in character_set := component_generic_name_characters);

	function to_string (generic_name : in type_component_generic_name.bounded_string) return string;
	-- Returns the given generic name as as string.



	
-- COMPONENT VALUES
	-- The component value is something like 330R or 100n or 74LS00
	component_value_length_max : constant positive := 50;

	-- Define the characters that are allowed for a component value:
	component_value_characters : character_set := 
		to_set (ranges => (('A','Z'),('a','z'),('0','9'))) 
		or to_set ('_')
		or to_set ('-');
	
	package type_component_value is new generic_bounded_length (component_value_length_max);

	function to_string (value : in type_component_value.bounded_string) return string;
	-- Returns the given value as string.

	function to_value (value : in string) return type_component_value.bounded_string;
	
	procedure check_value_length (value : in string);
	-- Tests if the given value is longer than allowed.
	
	procedure check_value_characters (
		value : in type_component_value.bounded_string;
		characters : in character_set := component_value_characters);
	-- Tests if the given value contains only valid characters as specified
	-- by given character set. Raises exception if invalid character found.
	





	

-- COMPONENT PREFIXES AND REFERENCES
	-- A component reference (in Eagle "device name") consists of a prefix (like R, C, IC, ..)
	-- and a consecutive number. Both form something like "IC702"
	component_prefix_characters : character_set := to_set (span => ('A','Z'));
	component_prefix_length_max : constant natural := 10; -- CS: there is no reason to work with longer prefixes.
	package type_component_prefix is new generic_bounded_length (component_prefix_length_max);
	use type_component_prefix;

	function to_string (prefix : in type_component_prefix.bounded_string) return string;
	-- returns the given prefix as string

	procedure check_prefix_length (prefix : in string);
	-- Tests if the given prefix is longer than allowed.
	
	procedure check_prefix_characters (
		prefix		: in type_component_prefix.bounded_string;
		characters	: in character_set);
	-- Tests if the given prefix contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.
	
	type type_component_reference_element is (PREFIX, ID);
	component_reference_prefix_default : constant type_component_prefix.bounded_string := to_bounded_string("?");

	subtype type_component_reference_id is natural range natural'first .. 99_999; -- R1..R99999, IC1..IC99999 should be enough
	component_reference_id_default : constant type_component_reference_id := 0;

	subtype type_component_reference_id_width is positive range positive'first .. 5; -- see number of digits of type_component_reference_id
	
	type type_component_reference is record -- CS: should be private
		prefix		: type_component_prefix.bounded_string := component_reference_prefix_default; -- like "IC"
		id			: type_component_reference_id := component_reference_id_default; -- like "303"
		id_width	: type_component_reference_id_width; -- the number of digits of the id. 3 in case of an id of 303
		-- NOTE: This allows something like R091 or IC0 (there are reasons for such strange things ...)
	end record;
	
	function to_string (reference : in type_component_reference) return string;
	-- Returns the given component reference as string.
	-- Prepends leading zeros according to reference.id_width.
	
	function prefix (reference : in type_component_reference) return type_component_prefix.bounded_string;
	-- Returns the prefix of the given component reference.

	-- These characters are allowed for a component reference. 
	-- This character set is used for prechecking references (like IC904) if provided as string together
	-- with procedure check_reference_characters.
	component_reference_characters : character_set := component_prefix_characters or to_set (span => ('0','9'));
	procedure check_reference_characters (
	-- Tests if the given reference (as string) contains valid characters.
	-- Unless a special character set is passed, it defaults to component_reference_characters.
		reference : in string; -- IC904
		characters : in character_set := component_reference_characters);



	
-- COMPONENT APPEARANCE	
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


	
-- COMPONENT VALUES
	procedure validate_component_value (
	-- Tests if the given component value meets certain conventions.
	-- This test depends on the category of the component. If no prefixes specified
	-- in the configuration file, this test does nothing.
		value 		: in type_component_value.bounded_string;
		reference	: in type_component_reference;
		appearance	: in type_component_appearance);


	
-- COMPONENT PACKAGES
	-- A component package is something like "SOT32" or "NDIP14". It is a more or less standardized (JEDEC)
	-- designator for the housing or the case of an electronical component. The package name is independed of
	-- the actual purpose of a component. An LED can have an SOT23 package and a transistor can also come in an SOT23.

	-- component package/footprint names like "SOT23" or "TO220" are stored in bounded strings:
	component_package_name_characters : character_set := to_set 
		(ranges => (('a','z'),('A','Z'),('0','9'))) 
		or to_set('.')
		or to_set('_'); 

	component_package_name_length_max : constant positive := 100;
	package type_component_package_name is new generic_bounded_length (component_package_name_length_max);
	--use type_component_package_name;

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
	
	procedure validate_component_package_name (name : in type_component_package_name.bounded_string);
	-- Tests if the given component package name meets certain conventions.

-- TERMINALS
	type type_terminal_count is new count_type; -- CS: limit to a reasonable range ?

	function to_string (terminals : in type_terminal_count) return string;
	-- Returns the given number of terminals as string.


	
-- MISCELLANEOUS
	-- Newly created fields may contain things like "?PARTCODE?" or "?PURPOSE?". 
	-- For checking their content we need this character set:
	component_initial_field_characters : character_set := to_set 
		(ranges => (('a','z'),('A','Z'),('0','9'))) or to_set('_') or to_set('?'); 




-- COMPONENT PART CODES
	-- The component part code is THE key into the ERP system of the user. It can be a crytic SAP number
	-- or something human readable like "R_PAC_S_0805_VAL_100R_PMAX_125_TOL_5".
	-- The keywords for the latter can be specified via the configuration file. See package et_configuration.
	component_partcode_characters : character_set := to_set
		(ranges => (('a','z'),('A','Z'),('0','9'))) or to_set ('_'); 
	component_partcode_length_max : constant positive := 100;
	package type_component_partcode is new generic_bounded_length (component_partcode_length_max);
	
	function to_string (partcode : in type_component_partcode.bounded_string) return string;
	-- Returns the given partcode as string.

	procedure check_partcode_length (partcode : in string);
	-- Tests if the given partcode is longer than allowed.
	
	procedure check_partcode_characters (
		partcode	: in type_component_partcode.bounded_string;
		characters	: in character_set := component_partcode_characters);
	-- Tests if the given partcode contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.


-- COMPONENT PURPOSE
	-- Components that require operator interaction like connectors, LEDs or switches 
	-- MUST have a purpose assigned.
	-- Example: The purpose of connector X44 is "power in". The purpose of LED5 is "system fail":
	component_initial_purpose_characters : character_set := 
		component_initial_field_characters or to_set (' '); 
	component_purpose_characters : character_set := to_set 
		(ranges => (('a','z'),('A','Z'),('0','9'))) or to_set ("_- "); 
	component_purpose_length_max : constant positive := 100;
	package type_component_purpose is new generic_bounded_length (component_purpose_length_max);
	purpose_default : constant string (1..9) := "?PURPOSE?";

	procedure validate_purpose (purpose : in string);
	-- Raises alarm if purpose is empty, purpose_default or nonsense.
	
	function to_string (purpose : in type_component_purpose.bounded_string) return string;
	-- Returns the given purpose as string.

	function to_purpose (purpose : in string) return type_component_purpose.bounded_string;
	-- Converts a string to type_component_purpose
	
	procedure check_purpose_length (purpose : in string);
	-- Tests if the given purpose is longer than allowed.
	
	procedure check_purpose_characters (
		purpose		: in type_component_purpose.bounded_string;
		characters	: in character_set := component_purpose_characters);
	-- Tests if the given purpose contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.

	

-- COMPONENT COMMISSION AND UPDATE DATE
	component_date_characters : character_set := to_set (span => ('0','9')) or to_set ("-:T");
	component_date_length : constant positive := 19; -- "2017-08-17T14:17:25" -- CS: probably way to accurate
	type type_component_date is new string (1..component_date_length); 
	component_date_format  : string (1..component_date_length) := "YYYY-MM-DDTHH:MM:SS";
	component_date_example : type_component_date := "2017-12-31T23:55:04";
	component_date_default : type_component_date := "1970-01-01T00:00:00";

	function compare_date (left, right : in type_component_date) return boolean;
	-- Returns true if left comes before right.
	-- If left equals right, the return is false.
	
	function to_string (date : in type_component_date) return string;
	-- Returns the given date as string.
	
	procedure date_format_error (date : in string);
	
	procedure check_date_length (date : in string);
	-- Tests if the given date is longer than allowed.
	
	procedure check_date_characters (
		date		: in type_component_date;
		characters	: in character_set := component_date_characters);
	-- Tests if the given date contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.




-- COMPONENT AUTHOR
	component_author_characters : character_set := to_set (span => ('A','Z')) or to_set (" -");
	component_author_length_max : constant positive := 20;
	package type_component_author is new generic_bounded_length (component_author_length_max);
	component_author_format  : constant string (1..16) := ("FORENAME SURNAME");
	component_author_example : constant string (1..12) := ("STEVE MILLER");

	procedure check_author_length (author : in string);
	-- Tests if the given author is longer than allowed.
	
	procedure check_author_characters (
		author		: in type_component_author.bounded_string;
		characters	: in character_set := component_author_characters);
	-- Tests if the given author contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.




	
	
-- SHAPES 

	-- line width
	subtype type_line_width is type_distance;

	-- fill
	type type_fill_border is (VISIBLE, INVISIBLE);
	type type_fill_pattern is (NONE, SOLID); -- CS: hatched ? and its properties ?
	type type_fill is record 
		border	: type_fill_border := VISIBLE;
		pattern : type_fill_pattern := SOLID;
	end record;
	
	-- straight lines
	type type_line is record
		start_point 	: type_2d_point;
		end_point   	: type_2d_point;
		line_width		: type_line_width;
	end record;
	package type_lines is new doubly_linked_lists (type_line);

	-- polylines 
	-- A polyline is a list of points. Their interconnections have a width and a fill.
	-- Filling can be done even if start and end point do not meet. In this case a virtual line
	-- is "invented" that connects start and end point.
	-- Finally the polylines are collected in a simple list.
	package type_points is new doubly_linked_lists (type_2d_point);
	type type_polyline is record
		line_width		: type_line_width;
		fill			: type_fill;
		points			: type_points.list;
	end record;
	package type_polylines is new doubly_linked_lists (type_polyline);

	-- Rectangles
	-- It is sufficient to specifiy the diagonal of the rectangle.
	type type_rectangle is record
		start_point		: type_2d_point; -- CS: rename to corner_A
		end_point		: type_2d_point; -- CS: rename to corner_B
		line_width		: type_line_width;
		fill			: type_fill;
	end record;
	package type_rectangles is new doubly_linked_lists (type_rectangle);	
	
	-- Arcs
	type type_arc is record
		center			: type_2d_point;
		radius  		: type_distance;
		start_point		: type_2d_point;
		end_point		: type_2d_point;
		start_angle		: type_angle; -- CS: ?
		end_angle		: type_angle; -- CS: ?
		line_width		: type_line_width;
 		fill			: type_fill;
	end record;
	package type_arcs is new doubly_linked_lists (type_arc);

	-- Circles
	type type_circle is record
		center			: type_2d_point;
		radius  		: type_distance;
		line_width		: type_line_width;
		fill			: type_fill;
	end record;
	package type_circles is new doubly_linked_lists (type_circle);

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
		LINE, POLYLINE, RECTANGLE, ARC, CIRCLE, -- shapes
		PORT, 
		TEXT); -- text embedded in a symbol
		--reference, value, commissioned, updated, author); -- text field placeholders

	-- Texts may be embedded in a symbol like "counter" or "&". So far they have the meaning "misc".
	-- CS: strings and texts within a unit symbol serve for documentation only. As long as
	-- there is no dedicated enumeration value available we choose "misc".
	-- CS: The meaning could be something like "documentation" some day.
	type type_symbol_text is new type_text (meaning => MISC) with null record;
	package type_symbol_texts is new doubly_linked_lists (type_symbol_text);

	type type_symbol_base is tagged record		
		shapes		: type_shapes; -- the collection of shapes
		texts		: type_symbol_texts.list; -- the collection of texts (meaning misc)
		
		-- Placeholders for component wide texts. To be filled with content when 
		-- a symbol is placed in the schematic:
		reference	: type_text_placeholder (meaning => et_libraries.REFERENCE);
		value		: type_text_placeholder (meaning => et_libraries.VALUE);
	end record;

	type type_symbol is new type_symbol_base with record
		ports : type_ports.list;
	end record;



	unit_name_length_max : constant natural := 50;	
	-- CS unit_name_characters, length check, character check
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
		ALWAYS,		-- 
		MUST);		-- for things like relay coils

	function to_string (add_level : in type_unit_add_level) return string;
	-- Returns the given add level as string.
	
	-- An internal unit is a symbol with a swap level.
	-- An internal unit is owned by the particular component exclusively.
	type type_unit_internal is record
		symbol		: type_symbol;
		coordinates	: type_2d_point; -- CS: rename to position
		swap_level	: type_unit_swap_level := unit_swap_level_default;
		add_level	: type_unit_add_level := type_unit_add_level'first;
	end record;

	-- Internal units are collected in a map:
	package type_units_internal is new indefinite_ordered_maps (
		key_type		=> type_unit_name.bounded_string, -- like "I/O-Bank 3" "A" or "B"
		element_type	=> type_unit_internal);

	-- An external unit has a reference and a swap level.
	type type_unit_external is record -- CS: parameter appearance ?
		reference	: type_symbol_library_name.bounded_string; -- like /my_libraries/NAND.sym
		coordinates	: type_2d_point; -- CS: rename to position
		swap_level	: type_unit_swap_level := unit_swap_level_default;
		add_level	: type_unit_add_level := type_unit_add_level'first;
	end record;

	-- External units are collected in a map;
	package type_units_external is new ordered_maps (
		key_type		=> type_unit_name.bounded_string, -- like "I/O-Bank 3"
		element_type	=> type_unit_external);

	type type_bom is (YES, NO); -- if a component is to be mounted or not

	function to_string (bom : in type_bom) return string;
	-- Returns the given bom variable as string.

	procedure check_bom_characters (bom : in string);
	-- Checks if given string is a bom status. Case sensitive !
	
	procedure validate_bom_status (text : in string); -- CS: requires appearance and category
	-- Validates BOM status. Case sensitive !	
	
	procedure validate_component_partcode_in_library (
	-- Tests if the given partcode of a library component is correct.
	-- The given properties are assumed to be those of a real component.
	-- If the component is not to be mounted, no validation takes place.
	-- Otherwise:
	--  - If partcode keywords are not specified in the 
	--    configuration file, nothing is validated. It is the users responsibility 
	--    to specify a correct partcode.
	--  - If partcode keywords are specified in the configuration file,
	--    the root part (like R_PAC_S_0805_VAL_) is validated.
		partcode		: in type_component_partcode.bounded_string;		-- R_PAC_S_0805_VAL_
		name			: in type_component_generic_name.bounded_string;	-- 74LS00	
		prefix			: in type_component_prefix.bounded_string;			-- R
		packge			: in type_component_package_name.bounded_string;	-- S_0805
		bom				: in type_bom; -- YES, NO
		log_threshold	: in et_string_processing.type_log_level);
	
	procedure validate_component_partcode_in_schematic (
	-- Tests if the given partcode of a schematic component is correct.
	-- The given properties are assumed to be those of a real component.
	-- If the component is not to be mounted, no validation takes place.
	-- Otherwise:
	--  - If partcode keywords are not specified in the 
	--    configuration file, nothing is validated. It is the users responsibility 
	--    to specify a correct partcode.
	--  - If partcode keywords are specified in the configuration file,
	--    the root part (like R_PAC_S_0805_VAL_) is validated.
		partcode		: in type_component_partcode.bounded_string;		-- R_PAC_S_0805_VAL_100R
		reference		: in type_component_reference;						-- R45
		packge			: in type_component_package_name.bounded_string;	-- S_0805
		value 			: in type_component_value.bounded_string; 			-- 100R
		bom				: in type_bom;										-- YES, NO
		log_threshold	: in et_string_processing.type_log_level);



	
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
	
-- COMPONENT VARIANTS
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

		
-- 	function to_string (variant : in type_component_variant) return string;
	-- Returns the given variant as string.
	-- NOTE: This displays the type_component_variant (see et_libraries.ads).
	-- Do not confuse with type_variant (see et_schematic.ads) which also contains the variant name
	-- like in TL084D or TL084N.

	type type_port_in_terminal_port_map is new type_port_base with record
		unit	: type_unit_name.bounded_string;
	end record;
	
	package type_terminal_port_map is new ordered_maps (
		key_type 		=> type_terminal_name.bounded_string, -- H7, 14
		element_type 	=> type_port_in_terminal_port_map); -- unit A, OE1

	type type_component_variant is record
		packge				: type_package_library_name.bounded_string; -- libraries/devices/transistors/pnp.dev
		terminal_port_map	: type_terminal_port_map.map; -- which port is connected with with terminal
	end record;

	package type_component_variants is new ordered_maps (
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
	type type_device (appearance : type_component_appearance) is record
		prefix			: type_component_prefix.bounded_string; -- R, C, IC, ...
		value			: type_component_value.bounded_string; -- 74LS00
		units_internal	: type_units_internal.map := type_units_internal.empty_map;
		units_external	: type_units_external.map := type_units_external.empty_map;
		commissioned	: type_component_date;
		updated			: type_component_date;
		author			: type_person_name.bounded_string;

		case appearance is

			-- If a component appears in the schematic only, it is a virtual component 
			-- and thus does not have any package variants.
			-- Such components are power symbols. Later when building netlists
			-- those components enforce net names (like GND or P3V3).
			when sch => 
				null;

			-- If a component appears in both schematic and layout it comes 
			-- with at least one package/footprint variant. We store variants in a map.
			when sch_pcb => 
				purpose			: type_component_purpose.bounded_string;
				partcode		: type_component_partcode.bounded_string;
				bom				: type_bom;
				variants		: type_component_variants.map;
				
			when others => null; -- CS
		end case;

	end record;

	
-- STORAGE
	
	package type_symbols is new indefinite_ordered_maps (
		key_type		=> type_symbol_library_name.bounded_string, -- ../lbr/logic/NAND.sym
		"<"				=> type_symbol_library_name."<",
		element_type	=> type_symbol);

	symbol_library_file_extension : constant string (1..3) := "sym";

	-- HERE RIG WIDE SYMBOLS ARE KEPT:	
	symbols : type_symbols.map;
	
	package type_devices is new indefinite_ordered_maps (
		key_type 		=> type_device_library_name.bounded_string, -- ../lbr/logic_ttl/7400.dev
		"<"				=> type_device_library_name."<",
		element_type	=> type_device);

	device_library_file_extension : constant string (1..3) := "dev";

	-- HERE RIG WIDE DEVICES ARE KEPT:
	devices : type_devices.map;


	
-- DRAWING FRAME

	-- $ET_FRAMES/drawing_frame_version_1.frm
	frame_template_name_length_max : constant positive := 300;
	package type_frame_template_name is new generic_bounded_length (frame_template_name_length_max);

	
    type type_title_block_line is record
		coordinates_start : et_coordinates.type_2d_point;
		coordinates_end   : et_coordinates.type_2d_point;
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
 		coordinates		: et_coordinates.type_2d_point;
		text			: type_title_block_text_content.bounded_string; -- CS: rename to content
 		size			: et_libraries.type_text_size;
 		orientation		: et_coordinates.type_angle; -- CS: rename to rotation
		-- CS: font, ...
 	end record;

	package type_title_block_texts is new doubly_linked_lists (type_title_block_text);

    -- the final title block
    type type_title_block is record
        coordinates     : et_coordinates.type_2d_point; -- CS rename to position
        lines           : type_title_block_lines.list;
        texts           : type_title_block_texts.list;
    end record;

    -- A drawing frame consists of straight lines and texts.
    -- The text is a character at the x/y border that helps to locate objects.
    type type_frame_line is record
		coordinates_start : et_coordinates.type_2d_point;
        coordinates_end   : et_coordinates.type_2d_point;
	end record;
	
	package type_frame_lines is new doubly_linked_lists (type_frame_line);

	type type_frame_text is record
		coordinates		: et_coordinates.type_2d_point; -- CS rename to position
		text			: character_set := et_string_processing.general_characters; -- CS rename to content
		size			: et_libraries.type_text_size;
		orientation		: et_coordinates.type_angle; -- CS rename to rotation
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
