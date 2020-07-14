------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      KICAD COMPONENT LIBRARIES                           --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;				use ada.text_io;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.ordered_sets;
with ada.containers.indefinite_ordered_maps;
with ada.containers.vectors;

with et_general;				use et_general;
with et_project;
with et_geometry;
with et_schematic;
with et_terminals;
with et_packages;
with et_pcb;
with et_kicad_general;			use et_kicad_general;
with kicad_coordinates;			use kicad_coordinates;
-- with et_kicad_pcb;
with et_import;
with et_coordinates;			use et_coordinates;
use et_coordinates.pac_geometry_sch;

with et_pcb_coordinates;
with et_string_processing;		use et_string_processing;
with et_text;					use et_text;
with et_symbols;				use et_symbols;
with et_devices;				use et_devices;
with et_frames;

package et_kicad_libraries is

	function f (line : in type_fields_of_line; position : in positive) return string;
	
	comment_mark : constant string := "#";

	
	-- SYM-LIB-TABLES AND FP-LIB-TABLES ------------------------------------------------------------------------------
	-- For V5:
	file_sym_lib_table			: constant string (1..13) := "sym-lib-table";
	file_fp_lib_table			: constant string (1..12) := "fp-lib-table";
	dir_global_lib_tables_linux	: constant string (1..15) := "/.config/kicad/";
	-- CS: windows ?
	
	file_sym_lib_table_global_linux : constant string (1 .. dir_global_lib_tables_linux'length + file_sym_lib_table'length)
		:= dir_global_lib_tables_linux & file_sym_lib_table;

	file_fp_lib_table_global_linux : constant string (1 .. dir_global_lib_tables_linux'length + file_fp_lib_table'length)
		:= dir_global_lib_tables_linux & file_fp_lib_table;
	------------------------------------------------------------------------------------------------------------------
	
	-- Placeholders for texts have a meaning:
	type type_placeholder_meaning is (
		NAME,			-- for things like R301 or X9
		VALUE,			-- for component values like "200R"
		PACKGE,			-- for component packages like SOT23
		DATASHEET,		-- for url to datasheet
		MISC);
	
	placeholder_meaning_default : constant type_placeholder_meaning := MISC;
	
	function to_string (meaning : in type_placeholder_meaning) return string;
	function to_meaning (meaning : in string) return type_placeholder_meaning;


	text_size_min : constant type_distance_positive := 0.01;
	text_size_max : constant type_distance_positive := 100.0;
	text_size_default : constant type_distance_positive := 1.3;
	
	subtype type_text_line_width is type_distance_positive range 0.0 .. 5.0; -- unit is mm -- CS: minimum of 0.0 reasonable ?
	text_line_width_min : constant type_distance_positive := 0.1;
	text_line_width_max : constant type_distance_positive := 5.0;
	text_line_width_default : constant type_distance_positive := 0.3; 

	-- Instantiation of the shapes package:
	-- Required for instantiation of text package only. See below.
	package pac_shapes is new et_geometry.generic_pac_shapes (et_coordinates.pac_geometry_sch);
	
	-- Instantiation of the text package:
	package pac_text is new et_text.generic_pac_text (
		pac_shapes			=> pac_shapes,
		size_min			=> text_size_min,
		size_max			=> text_size_max,
		size_default		=> text_size_default,
		line_width_min		=> text_line_width_min,
		line_width_max		=> text_line_width_max,
		line_width_default	=> text_line_width_default
		);

	use pac_text;

	-- These are basic properties a text has got:
	type type_text_basic is new pac_text.type_text with record
		-- CS: currently the style of text is ignored
		-- style : ???
		content		: et_text.type_text_content.bounded_string;		
		rotation	: et_coordinates.type_rotation := 0.0;
	end record;
	
	type type_text_placeholder (meaning : type_placeholder_meaning) is new type_text_basic with record
		position	: type_point;		
	end record;	
	
	-- A text/note in the schematic:
	type type_text is new type_text_basic with record
		position	: kicad_coordinates.type_position;
	end record;

	function content (text : in type_text_placeholder) return string;
	-- Returns the content of the given text placeholder as string.

	
	-- COMPONENT PACKAGE FILTER
	-- If certain packages are to be proposed they are collected in a so called "package filter"
	package_proposal_length_max : constant positive := 100;
	package type_package_proposal is new generic_bounded_length (package_proposal_length_max);
	package type_package_filter is new ordered_sets (
		element_type	=> type_package_proposal.bounded_string,
		"="				=> type_package_proposal."=",
		"<"				=> type_package_proposal."<");

	type type_port_style is (
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

	function to_string (style : in type_port_style) return string;

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

	-- Returns the given port direction as string:
	function to_string (
		direction	: in type_port_direction;
		preamble	: in boolean := true) return string;

	
	type type_port_library is new et_symbols.type_port_base with record 	-- CS: set defaults
		name		: et_symbols.type_port_name.bounded_string; -- like CLOCK or CE
		direction 	: type_port_direction;
		style 		: type_port_style := NONE;

		-- the clearance between symbol outline and port name 
		-- CS: define a reasonable range
		port_name_offset : et_coordinates.type_distance;
		-- CS : obsolete ? pin_position_offset ?
	end record;

	-- Ports of a component are collected in a simple list. A list, because multiple ports
	-- with the same name (but differing terminal names) may exist. For example lots of GND
	-- ports at FPGAs.
	package type_ports_library is new doubly_linked_lists (type_port_library);

	-- fill
	type type_fill_border is (VISIBLE, INVISIBLE);
	type type_fill_pattern is (NONE, SOLID); -- CS: hatched ? and its properties ?
	type type_fill is record
		border	: type_fill_border := VISIBLE;
		pattern : type_fill_pattern := SOLID;
	end record;

	function to_string (fill : in type_fill) return string;
	
	-- lines of a symbol:
	package type_symbol_lines is new doubly_linked_lists (
		element_type	=> et_symbols.type_line,
		"="				=> et_symbols."=");

	-- polylines of a symbol:
	-- A polyline is a list of points. Their interconnections have a width and a fill.
	-- Filling can be done even if start and end point do not meet. In this case a virtual line
	-- is "invented" that connects start and end point.
	-- Finally the polylines are collected in a simple list.
	package type_symbol_points is new doubly_linked_lists (
		element_type	=> type_point,
		"="				=> "=");

	type type_symbol_polyline is record
		width	: et_symbols.type_line_width;
		fill	: type_fill;
		points	: type_symbol_points.list;
	end record;
	package type_symbol_polylines is new doubly_linked_lists (type_symbol_polyline);

	-- rectangles of a symbol:
	-- It is sufficient to specifiy the diagonal of the rectangle.
	type type_symbol_rectangle is record
		corner_A	: type_point;
		corner_B	: type_point;
		width		: et_symbols.type_line_width;
		fill		: type_fill;
	end record;
	package type_symbol_rectangles is new doubly_linked_lists (type_symbol_rectangle);	
	
	-- arcs of a symbol:
	type type_symbol_arc is new et_symbols.type_arc with record
		start_angle		: et_coordinates.type_rotation;
		end_angle		: et_coordinates.type_rotation;
 		fill			: type_fill;
	end record;
	package type_symbol_arcs is new doubly_linked_lists (type_symbol_arc);

	-- circles of a symbol:
	type type_symbol_circle is new et_symbols.type_circle_base with record
		fill			: type_fill;
	end record;
	package type_symbol_circles is new doubly_linked_lists (type_symbol_circle);

	-- Shapes are wrapped in a composite:
	type type_symbol_shapes is record
		lines		: type_symbol_lines.list 		:= type_symbol_lines.empty_list;
		arcs 		: type_symbol_arcs.list			:= type_symbol_arcs.empty_list;
		circles		: type_symbol_circles.list		:= type_symbol_circles.empty_list;
		rectangles	: type_symbol_rectangles.list	:= type_symbol_rectangles.empty_list;
		polylines	: type_symbol_polylines.list	:= type_symbol_polylines.empty_list;
	end record;
	
	type type_symbol_element is (
		LINE, POLYLINE, RECTANGLE, ARC, CIRCLE, -- shapes
		PORT, 
		TEXT); -- text embedded in a symbol
	
	type type_symbol is new et_symbols.type_symbol_base with record
		appearance	: et_symbols.type_appearance;
		shapes		: type_symbol_shapes; -- the collection of shapes		
		ports		: type_ports_library.list := type_ports_library.empty_list; -- the ports of the symbol

		-- Placeholders for component wide texts. To be filled with content when a symbol is placed in the schematic:
		-- We use the native type for a text placeholder here. 
		-- For things like package or datasheet no placeholder is requried. They have no meaning here.
		name	: et_symbols.type_text_placeholder (meaning => et_symbols.NAME);
		value	: et_symbols.type_text_placeholder (meaning => et_symbols.VALUE);
	end record;

	-- a component unit in the library
	type type_unit_library (appearance : et_symbols.type_appearance) is record
		symbol		: type_symbol := (appearance => appearance, others => <>);
		coordinates	: type_point;
		-- Units that harbor component wide pins have this flag set.
		-- Usually units with power supply pins exclusively.
		-- When building portlists this flag is important.
		global		: boolean := false; -- CS: use a boolean derived type 
	end record;

	package type_units_library is new indefinite_ordered_maps (
		key_type		=> et_devices.type_unit_name.bounded_string, -- like "I/O-Bank 3" "A" or "B"
		"<"				=> et_devices.type_unit_name."<",
		element_type	=> type_unit_library);

	-- For some components (not all !) it is helpful to have an URL to the datasheet.
	-- We limit the URL to reansonable 500 characters. Excessive Google URLs are thus not allowed.
	component_datasheet_characters : character_set := 
		to_set (ranges => (('A','Z'),('a','z'),('0','9'))) or to_set (":/._-&");
	component_datasheet_length_max : constant positive := 500;
	package type_component_datasheet is new generic_bounded_length (component_datasheet_length_max);

	procedure check_datasheet_length (datasheet : in string);
	-- Tests if the given datasheet is longer than allowed.
	
	procedure check_datasheet_characters (
		datasheet	: in type_component_datasheet.bounded_string;
		characters	: in character_set := component_datasheet_characters);
	-- Tests if the given URL contains only valid characters as specified
	-- by given character set. Raises exception if invalid character found.

	
	type type_power_flag is (YES, NO);

	
	-- This is a component as it appears in the library:.
	type type_component_library (appearance : et_symbols.type_appearance) is record
		prefix			: et_devices.type_prefix.bounded_string; -- R, C, IC, ...
		value			: et_devices.type_value.bounded_string; -- 74LS00
		units			: type_units_library.map := type_units_library.empty_map;

		case appearance is

			-- If a component appears in the schematic only, it is a virtual component 
			-- and thus does not have any package variants.
			-- Such components are power symbols or power flags. Later when building netlists
			-- those components enforce net names (like GND or P3V3). Power flags do not
			-- enforce net names. In order to distinguish them from regular power symbols the
			-- power_flag is provided.
			when et_symbols.VIRTUAL => 
				power_flag		: type_power_flag := NO;

			-- If a component appears in both schematic and layout it comes 
			-- with at least one package/footprint variant. We store variants in a map.
			when et_symbols.PCB => 
				package_filter	: type_package_filter.set := type_package_filter.empty_set;
				datasheet		: type_component_datasheet.bounded_string;
				variants		: et_devices.pac_variants.map;
				
		end case;

	end record;

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
		name		: in type_component_generic_name.bounded_string; -- TRANSISTOR_NPN
		characters	: in character_set);

	function to_string (generic_name : in type_component_generic_name.bounded_string) return string;
	

	function strip_tilde (generic_name : in type_component_generic_name.bounded_string) return
		type_component_generic_name.bounded_string;
	-- Removes a possible heading tilde character from a generic component name.
	-- example: ~TRANSISTOR_NPN becomes TRANSISTOR_NPN	
	-- The leading tilde marks a component whose value is set to "invisible".	
	
	-- Library components are stored in a map.
	-- Within the map they are accessed by a key type_component_name (something like "CAPACITOR").
	package type_components_library is new indefinite_ordered_maps (
		key_type		=> type_component_generic_name.bounded_string, -- example: "TRANSISTOR_PNP"
		"<"				=> type_component_generic_name."<",
		element_type	=> type_component_library);

	function first_unit (
	-- Returns the cursor to the first unit of the given component
		component_cursor : in type_components_library.cursor)
		return type_units_library.cursor;

	function first_port (
	-- Returns the cursor to the first port of the given unit
		unit_cursor : in type_units_library.cursor)
		return type_ports_library.cursor;


	
	procedure no_generic_model_found (
		reference		: in et_devices.type_name; -- IC303
		library			: in type_device_library_name.bounded_string; -- ../lib/xilinx/spartan.lib
		generic_name	: in type_component_generic_name.bounded_string);
	
	
	function component_appearance (cursor : in type_components_library.cursor)
	-- Returns the component appearance where cursor points to.
		return et_symbols.type_appearance;

	function to_package_name (
		library_name	: in type_device_library_name.bounded_string; -- ../libraries/transistors.lib
		generic_name	: in type_component_generic_name.bounded_string; -- TRANSISTOR_PNP
		package_variant	: in et_devices.type_variant_name.bounded_string) -- N, D
		return et_packages.type_component_package_name.bounded_string;
	-- Returns the package name of the given component. 
	
	-- Alternative references used in instances of sheets:
	-- example: AR Path="/59F17FDE/5A991D18" Ref="RPH1"  Part="1" 
	package type_alternative_reference_path is new doubly_linked_lists (
		element_type => et_kicad_general.type_timestamp); -- 5A991D18
	
	type type_alternative_reference is record
		path		: type_alternative_reference_path.list; -- 59F17FDE 5A991D18 ...
		reference	: et_devices.type_name; -- R452
		part		: et_devices.type_unit_name.bounded_string; -- CS is this about a unit name ? currently written but never read
	end record;

	package type_alternative_references is new doubly_linked_lists (type_alternative_reference);
	

	function to_component_reference (	
	-- Converts a string like "IC303" to a composite type_name.
	-- If allow_special_character_in_prefix is given true, the first character
	-- is allowed to be a special character (like in #FLG01).
	-- Raises constraint error if prefix contains invalid characters.
	-- Raises constraint error if id contains non-digit characters.
	-- Leading zeroes in the id are removed. R002 becomes R2.
		text_in			: in string;
		leading_hash	: in boolean := false)
		return type_name;


	-- No-connection-flags indicate that a component port is intentionally left unconnected.
	type type_no_connection_flag is record
		coordinates : kicad_coordinates.type_position;
		-- CS: processed flag
	end record;

	-- No-connection-flags can be stored in a simple list:
	package type_no_connection_flags is new doubly_linked_lists (type_no_connection_flag);	

	function to_string (
		no_connection_flag	: in type_no_connection_flag;
		scope				: in kicad_coordinates.type_scope) return string;
	-- Returns the position of the given no-connection-flag as string.

	type type_port_open is new boolean;
	type type_port_connected is (YES, NO);
	
	-- For portlists and netlists we need a component port with its basic elements:
	type type_port is tagged record -- CS: use a controlled type since some selectors do not apply for virtual ports
		name			: et_symbols.type_port_name.bounded_string; -- the port name like GPIO1, GPIO2
		coordinates 	: kicad_coordinates.type_position;
		direction		: type_port_direction; -- example: "passive"
		style			: type_port_style;
		appearance		: et_schematic.type_appearance_schematic;
		intended_open	: type_port_open; -- set while portlist generation. true if port is to be left open intentionally (by a no_connection-flag)
		connected		: type_port_connected; -- set while netlist generation. true when port connected with a net
		power_flag		: type_power_flag; -- indicates if port belongs to a power_flag
	end record;

	-- Ports can be collected in a simple list:
	package type_ports is new doubly_linked_lists (type_port); 
	--use type_ports;

	-- The components with their ports are collected in a map with the component reference as key:
	package type_portlists is new ordered_maps (
		key_type		=> et_devices.type_name,
		element_type	=> type_ports.list,
		"<"				=> et_devices."<",
		"="				=> type_ports."=");

	-- If component ports are to be listed, 
	-- we need additionally the component reference like R102 or IC7
	type type_port_with_reference is new type_port with record
		reference : et_devices.type_name;
	end record;

	function to_string (port : in type_port_with_reference) return string;
	-- Returns the properties of the given port as string.

	function compare_ports (left, right : in type_port_with_reference) return boolean;
	-- Returns true if left comes before right. Compares by component reference and port name.
	-- If left equals right, the return is false.	


	-- Full library names can be stored further-on in a simple list:
	-- We use a simple list because the order of the library names sometimes matters and must be kept.
    package type_full_library_names is new doubly_linked_lists ( -- CS remove
		element_type 	=> type_device_library_name.bounded_string,
		"="				=> type_device_library_name."=");

	package type_libraries is new ordered_maps (
		key_type 		=> type_device_library_name.bounded_string, -- ../../lbr/passive/capacitors.lib
		"<"				=> type_device_library_name."<",
		element_type 	=> type_components_library.map,
		"=" 			=> type_components_library."=");
	-- CS the element could be a record consisting of type_components_library.map, lib_type, options and desrciption
	-- lib_type, options and description are provided in V5 and should be stored here in the future.

	-- All component models of a project/module are collected here temporarily.
	-- This collection is cleared when a new project is read. See procedure read_project_file.
	-- NOTE: tmp_component_libraries is requried for operations (like read_schematic)
	-- within the current module.
	-- CS: in the future tmp_component_libraries should be discarded. update_element and query_element
	-- operations should access the component_libraries of a module directly (see type_module).
	tmp_component_libraries : type_libraries.map;
	
	
	-- LIBRARY SEARCH LISTS ------------------------------------------------------------------
	-- Relevant for V4:
	-- The order of project libraries and their containing directories 
	-- matters (for search operations).
	-- For this reason we keep them in simple lists.
	-- If multiple projects are imported, these lists are always
	-- cleared when a project file is read. See procedure read_project_file.
	-- The search lists are stored for each module (see type_module specs).

	-- Bare library names can be stored further-on in a simple list:
	-- We use a simple list because the order of the library names sometimes matters 
	-- in V4 and must be kept.
	package type_library_names is new doubly_linked_lists (
		element_type	=> type_library_name.bounded_string, -- bel_logic, bel_primitives
		"="				=> type_library_name."=");
	
	-- search list for component library names
	search_list_component_libraries : type_library_names.list; -- bel_logic, bel_primitives, ...

	-- Libraries are stored in directories:
	library_directory_length_max : constant positive := 300; -- CS: increase if necessary
	package type_library_directory is new generic_bounded_length (library_directory_length_max);

	function to_string (dir : in type_library_directory.bounded_string) return string;
	
	-- Search list for library directories.
	-- This list applies for both component and package search operations.
	package type_project_lib_dirs is new doubly_linked_lists (
		element_type	=> type_library_directory.bounded_string,
		"=" 			=> type_library_directory."=");
	search_list_project_lib_dirs : type_project_lib_dirs.list;

	-- SYMBOL-LIBRARY-TABLES AND FOOTPRINT-LIBRARY-TABLES--------------------------	
	-- Relevant for V5:
	type type_lib_type is (
		LEGACY,	-- symbol libs
		KICAD	-- footprints
		); -- CS: others ?

	type type_lib_table_entry is record
		lib_name	: type_library_name.bounded_string;
		lib_type	: type_lib_type;
		lib_uri		: type_device_library_name.bounded_string;
		-- CS to be exact: there should be a distinct type_lib_table_entry for components and packages each.
		-- Currently lib_uri is used for both component and package libraries.
		
		-- CS options
		-- CS description
	end record;

	package type_lib_table is new doubly_linked_lists (type_lib_table_entry);

	-- After reading the sym-lib-tables (local and global) they are stored here temporarily.
	sym_lib_tables : type_lib_table.list;

	-- After reading the fp-lib-tables (local and global) they are stored here temporarily.
	fp_lib_tables : type_lib_table.list;
	-------------------------------------------------------------------------------

	
-- COMPONENT TEXT FIELDS

	-- In compoenent libraries and schematic, a text field is indicated by letter "F":
	component_field_identifier : constant string (1..1) := "F";

	-- We limit the number of fields in the component library to this constant.
	library_component_field_count_max : constant positive := 3;


	type type_component_field_id is range 0..library_component_field_count_max;
	component_field_reference		: constant type_component_field_id := 0;
	component_field_value			: constant type_component_field_id := 1;
	component_field_package			: constant type_component_field_id := 2;
	component_field_datasheet		: constant type_component_field_id := 3;


-- GRID AND COORDINATES
	-- Objects may be placed at a certain angle. The unit is tenth of degree:
	type type_angle is range -3599 .. 3599;


	-- These strange strings are used to define the text style of 
	-- net labels and notes:
	text_library_style_normal	: constant string (1..6) := "Normal";
	text_library_style_italic	: constant string (1..6) := "Italic";	
	
    -- fields
	type type_field_orientation is (H, V); -- horizontal, vertical
	type type_field_alignment_horizontal is (R, C, L); -- right, center, left
	type type_field_alignment_vertical is (T, C, B);   -- top, center, bottom
	type type_library_field_visible is (V, I); -- visible, invisible

	type type_library_draw is (
		P, -- polyline
		S, -- rectangle
		C, -- circel,
		A, -- arcus
		T, -- text
		X); -- pin

	library_fill_none			: constant string (1..1) := "N";
	library_fill_foreground		: constant string (1..1) := "F";
	library_fill_background		: constant string (1..1) := "f";

	library_text_bold_off						: constant string (1..1) := "0";
	library_text_bold_on						: constant string (1..1) := "1";

	type type_library_pin_orientation is (
		U, -- up
		D, -- down
		R, -- right
		L); -- left

	-- electrical direction of COMPONENT PORTS (NOTE: since kicad uses upper and lower case letters here,
	-- an enumeration type is not possible. thus we define constant strings instead:
	library_pin_electrical_type_passive			: constant character := 'P';
	library_pin_electrical_type_input			: constant character := 'I';
	library_pin_electrical_type_output			: constant character := 'O';	
	library_pin_electrical_type_bidir			: constant character := 'B';
	library_pin_electrical_type_tristate		: constant character := 'T';
	library_pin_electrical_type_unspecified		: constant character := 'U';
	library_pin_electrical_type_power_in		: constant character := 'W'; -- mind case !
	library_pin_electrical_type_power_out		: constant character := 'w'; -- mind case !
	library_pin_electrical_type_open_collector	: constant character := 'C';
	library_pin_electrical_type_open_emitter	: constant character := 'E';
	library_pin_electrical_type_not_connected	: constant character := 'N';	
	
	-- The graphical pin style is optional. If not provided, it defaults to "Line".
	-- ET maps "Line" to "NONE" (see et_libraries.type_port_style).
	type type_library_pin_graphical_style is (
		N,		-- line (default if not provided)
		I,		-- inverted
        C,		-- clock		
        IC,		-- inverted_clock
        L,		-- input_low	
        CL,		-- clock_low
        V,		-- output_low
        F,		-- falling_edge_clk
        X,		-- non_logic
		NI,		-- invisible_inverted
		NC,		-- invisible_clock
		NIC,	-- invisible_inverted_clock
		NL,		-- invisible_input_low	
		NCL,	-- invisible_clock_low
		NV,		-- invisible_output_low
		NF,		-- invisible_falling_edge_clk
		NX);	-- invisible_non_logic
	
	-- workaround; prefix V requried to form an enumaration type:
	schematic_field_visibility_prefix : constant character := 'V';
	type type_schematic_field_visible is (V0000, V0001); -- visible, invisible 
	
		
	field_style_default 	: constant string (1..2) := "NN";
	field_style_bold		: constant string (1..2) := "NB";
	field_style_italic		: constant string (1..2) := "IN";
	field_style_italic_bold	: constant string (1..2) := "IB";	
	


	version_header : constant string (1..28) := "EESchema-LIBRARY Version 2.3";

	def			: constant string (1..3) := "DEF";
	enddef		: constant string (1..6) := "ENDDEF";
	draw		: constant string (1..4) := "DRAW";
	enddraw		: constant string (1..7) := "ENDDRAW";
	fplist		: constant string (1..7) := "$FPLIST";
	endfplist	: constant string (1..10) := "$ENDFPLIST";

	-- The distance of the pin name from the pin itself (supply pins only)
	subtype type_supply_pin_name_position_offset is et_coordinates.type_distance
		range 0.00 .. et_coordinates.type_distance'last;

	-- KiCad supports up to 64 units within a component
	unit_count_max : constant positive := 64;
	type type_units_total is new positive range 1..unit_count_max;
	type type_unit_id is new natural range natural'first..unit_count_max;

	type type_library_component_appearance is (N, P); -- normal or power

	
	-- In schematic, a power symbol/component has a hash as first character in a line like "L P3V3 #PWR07"
	schematic_component_power_symbol_prefix: constant character := '#';
	
	-- power flags and symbols have a special prefix which distinguishes 
	-- them from real components:
	power_flag_prefix : constant string (1..4) := "#FLG";
	power_symbol_prefix : constant string (1..4) := "#PWR";	

	-- These are the characters allowed for a component prefix:
	component_prefix_characters : character_set := et_devices.prefix_characters
		or to_set (schematic_component_power_symbol_prefix);

	-- These characters are allowed for a component reference. 
	-- This character set is used for prechecking references (like IC904 or #PWR) if 
	-- provided as string together with procedure check_reference_characters (see et_libraries):
	component_reference_characters : character_set := component_prefix_characters or to_set (span => ('0','9'));

	-- Kicad combines the library and package/footprint name in a single string like bel_capacitors:S_0805
	-- Therefore the character set used here includes the colon additionally.
	component_package_name_characters : character_set := et_packages.component_package_name_characters or to_set (':');

	-- In the library a component name may have a tilde. Therefore we extend the standard character set by a tilde.
	component_generic_name_characters_lib : character_set := component_generic_name_characters or to_set ('~');
	
	type type_symbol_interchangeable is (L, F); -- L means swapping not allowed, F means swapping allowed 
	type type_show_pin_number is (Y, N); -- show pin/pad number yes/no
	type type_show_pin_name is (Y, N); -- show pin (better port) name yes/no

	type type_alternative_representation is new natural range 0..1;
	alternative_representation_yes	: constant type_alternative_representation := 0;
	alternative_representation_no	: constant type_alternative_representation := 1;


-- IMPORT

	schematic_version_valid 	: boolean := false;	
	sheet_header_entered		: boolean := false;	
	description_entered			: boolean := false;
	description_processed		: boolean := false;
	sheet_description_entered	: boolean := false;

	component_entered 			: boolean := false;
	net_segment_entered			: boolean := false;
	simple_label_entered		: boolean := false;
	tag_label_entered 			: boolean := false;	
	note_entered				: boolean := false;	


	function library_name (text : in string) return type_library_name.bounded_string;
	-- extracts from a string like "bel_ic:S_SO14" the library name "bel_ic"

	function package_name (text : in string) return et_packages.type_component_package_name.bounded_string;
	-- extracts from a string like "bel_ic:S_SO14" the package name "S_SO14"
	
	function component_power_flag (cursor : in type_components_library.cursor)
	-- Returns the component power flag status.
		return type_power_flag;

	function find_component (
	-- Searches the given library for the given component. Returns a cursor to that component.
		library		: in type_device_library_name.bounded_string; -- incl. path and file name
		component	: in type_component_generic_name.bounded_string) 
		return type_components_library.cursor;

	procedure write_note_properties (
		note			: in type_text;
		log_threshold	: in et_string_processing.type_log_level := 0);
	-- Writes the properties of the given note
	
	package type_texts is new doubly_linked_lists (type_text);


	-- Units may have alternative representations such as de_Morgan
	type type_de_morgan_representation is (NO, YES);

	function prepend_tilde (generic_name : in type_component_generic_name.bounded_string) return
		type_component_generic_name.bounded_string;
	-- Prepends a heading tilde character to a generic component name.
	-- example: TRANSISTOR_NPN becomes ~TRANSISTOR_NPN
	-- The leading tilde marks a component whose value is set to "invisible".

	-- Reads component libraries.
	procedure read_components_libraries (log_threshold : in type_log_level);
	
	
end et_kicad_libraries;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
