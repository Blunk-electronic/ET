------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                KICAD                                     --
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

with et_general;
with et_project;
with et_schematic;
with et_schematic_sheets;
with et_packages;
with et_pcb;
with et_kicad_general;			use et_kicad_general;
with kicad_coordinates;			use kicad_coordinates;
with et_kicad_pcb;
with et_import;
with et_coordinates;
with et_pcb_coordinates;
with et_string_processing;
with et_text;
with et_symbols;
with et_devices;			use et_devices;
with et_frames;

package et_kicad is
	
	-- CS: a lot of stuff should move from here to et_kicad_general

	project_file_handle : ada.text_io.file_type;
	
    encoding_default 					: constant string (1..5) := "utf-8";	

	file_extension_project   			: constant string (1..3) := "pro";
	file_extension_schematic 			: constant string (1..3) := "sch";
	file_extension_schematic_lib		: constant string (1..3) := "lib";
	file_extension_board	 			: constant string (1..9) := "kicad_pcb";
    
	schematic_version_v4	: constant positive := 2; -- CS use dedicated type for schematic version
    schematic_version_v5	: constant positive := 4;

	top_level_schematic	: type_schematic_file_name.bounded_string; 

	schematic_handle : ada.text_io.file_type;

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

	
	-- Sheet names may have the same length as schematic files.
	package type_sheet_name is new generic_bounded_length (schematic_file_name_length);

	function to_submodule_name (file_name : in type_schematic_file_name.bounded_string)
		return type_submodule_name.bounded_string;
	-- Returns the base name of the given schematic file name as submodule name.


	-- Bare library names can be stored further-on in a simple list:
	-- We use a simple list because the order of the library names sometimes matters 
	-- in V4 and must be kept.
	package type_library_names is new doubly_linked_lists (
		element_type	=> type_library_name.bounded_string, -- bel_logic, bel_primitives
		"="				=> type_library_name."=");
	
	-- SHEET HEADERS
	-- The sheet header is a composite of a list of libraries and other things:
	-- It contains a list of libraries used by a particular schemetic sheet.
	-- We use a simple list because the order of the library names must be kept.
    type type_sheet_header is record
		libraries   : type_library_names.list; -- CS: probably not used by kicad, just information
        eelayer_a   : positive; -- 25 -- CS: meaning not clear, probably not used
        eelayer_b   : natural; -- 0 -- CS: meaning not clear, probably not used
    end record;

	-- Since there are usually many sheets, we need a map from schematic file name to schematic header.
    package type_sheet_headers is new ordered_maps (
        key_type		=> type_schematic_file_name.bounded_string,
		element_type	=> type_sheet_header,
		"<"				=> type_schematic_file_name."<"
		);
	
    sheet_comment_length : constant natural := 100;
	package type_sheet_comment is new generic_bounded_length (sheet_comment_length); -- currently not used

	-- Within a schematic every object can be located by the name of the:
    -- - path to the submodule (first item in path is the top level module)
	-- - submodule name
	-- - sheet number (NOTE: The sheet numbering restarts in a submodule)
	-- - basic coordinates x/y

	-- While reading sheets the path_to_sheet keeps record of current point in the design 
	-- hierarchy. Each time a sheet ABC has been found with nested sub-sheets, the name of ABC is appended here.
	-- Once the parent sheet is entered again, the name ABC is removed from the list. When assigning coordinates
	-- to an object, the path_to_sheet is read. 
	-- So this list (from first to last) provides a full path that tells us
	-- the exact location of the sheet within the design hierarchy.
	path_to_sheet : type_path_to_submodule.list;
	
	-- Here we append a sheet name to the path_to_sheet.
	procedure append_sheet_name_to_path (sheet : in type_submodule_name.bounded_string);

	-- Here we remove the last submodule name form the path_to_sheet.
	procedure delete_last_module_name_from_path; -- CS: unify with append_name_of_parent_module_to_path

	procedure module_not_found (module : in type_submodule_name.bounded_string);
	-- Returns a message stating that the given module does not exist.

	-- Units may have alternative representations such as de_Morgan
	type type_de_morgan_representation is (NO, YES);


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


	text_size_min : constant et_coordinates.geometry.type_distance_positive := 0.1;
	text_size_max : constant et_coordinates.geometry.type_distance_positive := 100.0;
	text_size_default : constant et_coordinates.geometry.type_distance_positive := 1.3;
	
	subtype type_text_line_width is et_coordinates.geometry.type_distance_positive range 0.0 .. 5.0; -- unit is mm -- CS: minimum of 0.0 reasonable ?
	text_line_width_min : constant et_coordinates.geometry.type_distance_positive := 0.1;
	text_line_width_max : constant et_coordinates.geometry.type_distance_positive := 5.0;
	text_line_width_default : constant et_coordinates.geometry.type_distance_positive := 0.3; 
	
	-- Instantiation of the text package:
	package pac_text is new et_text.text (
		type_distance		=> et_coordinates.geometry.type_distance_positive,
		size_min			=> text_size_min,
		size_max			=> text_size_max,
		size_default		=> text_size_default,
		line_width_min		=> text_line_width_min,
		line_width_max		=> text_line_width_max,
		line_width_default	=> text_line_width_default
		);

	-- These are basic properties a text has got:
	type type_text_basic is new pac_text.type_text with record
		style		: et_symbols.type_text_style := et_symbols.type_text_style'first;
		content		: et_text.type_text_content.bounded_string;		
		rotation	: et_coordinates.type_rotation := 0.0;
	end record;

	type type_text_placeholder (meaning : type_placeholder_meaning) is new type_text_basic with record
		position	: et_coordinates.geometry.type_point;		
	end record;	
	
	-- A text/note in the schematic:
	type type_text is new type_text_basic with record
		position	: kicad_coordinates.type_position;
	end record;

	function content (text : in type_text_placeholder) return string;
	-- Returns the content of the given text placeholder as string.
	
	
	-- A kicad unit:
	type type_unit_schematic is record
		appearance	: et_schematic.type_appearance_schematic;
		rotation	: et_coordinates.type_rotation := et_coordinates.geometry.zero_rotation;
		mirror		: et_schematic.type_mirror := et_schematic.NO;
		position	: kicad_coordinates.type_position;		

		-- We use the native type for a text placeholder here.
		-- Placeholders for datasheet and package have no meaning here.
		reference	: et_symbols.type_text_placeholder (meaning => et_symbols.NAME);
		value		: et_symbols.type_text_placeholder (meaning => et_symbols.value);
		
		timestamp	: et_kicad_general.type_timestamp;
		alt_repres	: type_de_morgan_representation;
	end record;

	procedure add_unit (
	-- Adds a unit into the given commponent.
		reference		: in et_devices.type_device_name;
		unit_name		: in et_devices.type_unit_name.bounded_string;
		unit 			: in type_unit_schematic;
		log_threshold	: in et_string_processing.type_log_level);

	
	-- Units of a component are collected in a map.
	-- A unit is accessed by its name like "I/O Bank 3" or "PWR" or "A" or "B" ...	
	package type_units_schematic is new ordered_maps (
		key_type		=> et_devices.type_unit_name.bounded_string,
		"<"				=> et_devices.type_unit_name."<",
		element_type	=> type_unit_schematic);

	function unit_exists (
	-- Returns true if the unit with the given name exists in the given list of units.
		name	: in et_devices.type_unit_name.bounded_string; -- the unit being inquired
		units	: in type_units_schematic.map) -- the list of units
		return boolean;

	function position_of_unit (
	-- Returns the coordinates of the unit with the given name.
	-- It is assumed, the unit in question exists.
	-- The unit is an element in the given list of units.
		name	: in et_devices.type_unit_name.bounded_string; -- the unit being inquired
		units	: in type_units_schematic.map) -- the list of units
		return type_position;
	
	function mirror_style_of_unit (
	-- Returns the mirror style of the given unit.
	-- It is assumed, the unit in question exists.
	-- The unit is an element in the given list of units.
		name	: in et_devices.type_unit_name.bounded_string; -- the unit being inquired
		units 	: in type_units_schematic.map) -- the list of units
		return et_schematic.type_mirror;
	
	function orientation_of_unit (
	-- Returns the orientation of the given unit.
	-- It is assumed, the unit in question exists.
	-- The unit is an element in the given list of units.
		name 	: in et_devices.type_unit_name.bounded_string; -- the unit being inquired
		units 	: in type_units_schematic.map) -- the list of units
		return et_coordinates.type_rotation;
	
	procedure write_unit_properties (
	-- Writes the properties of the unit indicated by the given cursor.
		unit			: in type_units_schematic.cursor;
		log_threshold	: in et_string_processing.type_log_level);

	-- POWER FLAGS
	type type_power_flag is (YES, NO);

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

	function to_string (
		direction	: in type_port_direction;
		preamble	: in boolean := true) return string;
	-- Returns the given port direction as string.

	
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
		element_type	=> et_coordinates.geometry.type_point,
		"="				=> et_coordinates.geometry."=");

	type type_symbol_polyline is record
		width	: et_symbols.type_line_width;
		fill	: type_fill;
		points	: type_symbol_points.list;
	end record;
	package type_symbol_polylines is new doubly_linked_lists (type_symbol_polyline);

	-- rectangles of a symbol:
	-- It is sufficient to specifiy the diagonal of the rectangle.
	type type_symbol_rectangle is record
		corner_A	: et_coordinates.geometry.type_point;
		corner_B	: et_coordinates.geometry.type_point;
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
		coordinates	: et_coordinates.geometry.type_point;
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
				variants		: et_devices.type_component_variants.map;
				
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
		reference		: in et_devices.type_device_name; -- IC303
		library			: in type_device_library_name.bounded_string; -- ../lib/xilinx/spartan.lib
		generic_name	: in type_component_generic_name.bounded_string);
	
	
	function component_appearance (cursor : in type_components_library.cursor)
	-- Returns the component appearance where cursor points to.
		return et_symbols.type_appearance;

	function to_package_name (
		library_name	: in type_device_library_name.bounded_string; -- ../libraries/transistors.lib
		generic_name	: in type_component_generic_name.bounded_string; -- TRANSISTOR_PNP
		package_variant	: in et_devices.type_component_variant_name.bounded_string) -- N, D
		return et_packages.type_component_package_name.bounded_string;
	-- Returns the package name of the given component. 




	
	-- Alternative references used in instances of sheets:
	-- example: AR Path="/59F17FDE/5A991D18" Ref="RPH1"  Part="1" 
	package type_alternative_reference_path is new doubly_linked_lists (
		element_type => et_kicad_general.type_timestamp); -- 5A991D18
	
	type type_alternative_reference is record
		path		: type_alternative_reference_path.list; -- 59F17FDE 5A991D18 ...
		reference	: et_devices.type_device_name; -- R452
		part		: et_devices.type_unit_name.bounded_string; -- CS is this about a unit name ? currently written but never read
	end record;

	package type_alternative_references is new doubly_linked_lists (type_alternative_reference);
	
	-- This is a component as it appears in the schematic.
	type type_component_schematic (appearance : et_schematic.type_appearance_schematic) is record
		library_name	: type_device_library_name.bounded_string; -- lib name like ../libraries/transistors.lib
		generic_name	: type_component_generic_name.bounded_string; -- example: "TRANSISTOR_PNP"
		alt_references	: type_alternative_references.list;
		value			: et_devices.type_value.bounded_string; -- 470R
		units			: type_units_schematic.map; -- PWR, A, B, ...
		case appearance is
			-- If a component appears in both schematic and layout it has got:
			when et_symbols.PCB => 
				datasheet			: type_component_datasheet.bounded_string;
				variant				: et_devices.type_component_variant_name.bounded_string; -- D, N

				-- This is layout related. In the layout the package has a position
				-- and placeholders for reference and value.
				position			: et_pcb_coordinates.type_package_position; -- incl. angle and face
				text_placeholders	: et_packages.type_text_placeholders;
				
			-- If a component appears in the schematic only, it does not have any package variants.
			-- Such components are power symbols or power flags. Later when building netlists
			-- those component may enforce net names (like GND or P3V3). Power flags do not
			-- enforce net names. In order to distinguish them from regular power symbols the
			-- power_flag is provided.
			when et_symbols.VIRTUAL => 
				power_flag	: type_power_flag := NO;

		end case;
	end record;

	
	
	procedure add_component (
	-- Adds a component into the the module (indicated by module_cursor).
		reference		: in et_devices.type_device_name;
		component		: in type_component_schematic;
		log_threshold	: in et_string_processing.type_log_level);


	-- The components of a module are collected in a map.
 	package type_components_schematic is new indefinite_ordered_maps (
		key_type 		=> et_devices.type_device_name, -- something like "IC43"
		"<" 			=> et_devices."<",
 		element_type 	=> type_component_schematic);

	function component_reference (cursor : in type_components_schematic.cursor) 
		return type_device_name;
	-- Returns the component reference where cursor points to.
	
	
	function units_of_component (component_cursor : in type_components_schematic.cursor) return type_units_schematic.map;
	-- Returns the units of the given component.

	function to_component_reference (	
	-- Converts a string like "IC303" to a composite type_device_name.
	-- If allow_special_character_in_prefix is given true, the first character
	-- is allowed to be a special character (like in #FLG01).
	-- Raises constraint error if prefix contains invalid characters.
	-- Raises constraint error if id contains non-digit characters.
	-- Leading zeroes in the id are removed. R002 becomes R2.
		text_in			: in string;
		leading_hash	: in boolean := false)
		return type_device_name;

	
	procedure write_component_properties (
	-- Writes the properties of the component indicated by the given cursor.
		component 		: in type_components_schematic.cursor;
		log_threshold	: in et_string_processing.type_log_level);



	-- No-connection-flags indicate that a component port is intentionally left unconnected.
	type type_no_connection_flag is record
		coordinates : type_position;
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
		coordinates 	: type_position;
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
		key_type		=> et_devices.type_device_name,
		element_type	=> type_ports.list,
		"<"				=> et_devices."<",
		"="				=> type_ports."=");

	-- If component ports are to be listed, 
	-- we need additionally the component reference like R102 or IC7
	type type_port_with_reference is new type_port with record
		reference : et_devices.type_device_name;
	end record;

	function to_string (port : in type_port_with_reference) return string;
	-- Returns the properties of the given port as string.

	function to_terminal (
		port 			: in type_port_with_reference;
		module			: in type_submodule_name.bounded_string; -- the name of the module 
		log_threshold 	: in et_string_processing.type_log_level)
		return et_devices.type_terminal;
	-- Returns the terminal and unit name of the given port in a composite type.
	-- Raises error if given port is of a virtual component (appearance sch).

	
	function compare_ports (left, right : in type_port_with_reference) return boolean;
	-- Returns true if left comes before right. Compares by component reference and port name.
	-- If left equals right, the return is false.	
	
	-- When inquiring the net connected with certain component we use this composite:
	type type_port_of_module is record
		module		: type_submodule_name.bounded_string;			-- nucleo_core_3
		reference	: et_devices.type_device_name;		-- N409
		name		: et_symbols.type_port_name.bounded_string;	-- 2
	end record;
	
	-- This is a set of ports as we need in the netlist.
	package type_ports_with_reference is new ordered_sets (
		element_type 	=> type_port_with_reference,
		"<" 			=> compare_ports);

	-- This is the netlist of a single submodule:
	-- It does also contain ports of virtual components (power symbols) except 
	-- so called "power flags".
	package type_netlist is new ordered_maps (
		key_type		=> et_general.type_net_name.bounded_string, -- net name like "MCU_CLOCK"
		"<"				=> et_general.type_net_name."<",
		"="				=> type_ports_with_reference."=",
		element_type	=> type_ports_with_reference.set); -- the list of ports connected with the net


	function simple_name (net_name : in et_general.type_net_name.bounded_string) 
		return et_general.type_net_name.bounded_string;
	-- Returns the simple name of the given net name.
	-- Example: If the given name is "MOTOR_DRIVER/CLOCK" then the return is "CLOCK".

	type type_net_label (label_appearance : et_schematic.type_net_label_appearance) is record
		coordinates	: et_coordinates.geometry.type_point;
		rotation	: et_coordinates.type_rotation;
        text		: et_general.type_net_name.bounded_string;
        size		: et_symbols.pac_text.type_text_size;
        style		: et_symbols.type_text_style;
        width		: et_symbols.type_text_line_width;
		processed	: boolean := false; -- used for associating label with net segment
		case label_appearance is
			when et_schematic.TAG => 
				direction	: et_schematic.type_net_label_direction;
				global		: boolean; -- CS: use only one flag. true -> hierachic, false -> global
				hierarchic	: boolean;
			when et_schematic.SIMPLE => null;
		end case;
	end record;

	type type_net_label_simple is new type_net_label (label_appearance => et_schematic.SIMPLE);
	package type_simple_labels is new doubly_linked_lists (type_net_label_simple);
	
	type type_net_label_tag is new type_net_label (label_appearance => et_schematic.TAG);
	package type_tag_labels is new doubly_linked_lists (type_net_label_tag);

	procedure write_label_properties (label : in type_net_label);
	-- Writes the properties of the given net label in the logfile.

	function to_string (label : in type_net_label) return string;
	-- Returns the coordinates of the given label as string.

	-- A net junction is where segments can be connected with each other.
	type type_net_junction is record -- CS rename to type_junction
		coordinates : type_position;
	end record;

	function to_string (
		junction	: in type_net_junction;
		scope 		: in kicad_coordinates.type_scope) 
		return string;
	-- Returns the position of the given junction as string.
	
	-- Junctions are to be collected in a list.
	package type_junctions is new doubly_linked_lists (type_net_junction);

	type type_net_segment_base is tagged record
		coordinates_start 	: kicad_coordinates.type_position;
		coordinates_end   	: kicad_coordinates.type_position; -- CS et_coordinates.geometry.type_point ?
	end record;

	function length (segment : in type_net_segment_base) 
		return et_coordinates.type_distance;
	-- Returns the length of the given net segment.
	
	type type_net_segment is new type_net_segment_base with record
		label_list_simple 	: type_simple_labels.list;
		label_list_tag    	: type_tag_labels.list;
		junctions			: type_junctions.list;
	end record;

	function to_string (
		segment	: in type_net_segment_base'class;
		scope	: in kicad_coordinates.type_scope := kicad_coordinates.SHEET) return string;
	-- Returns the start and end coordinates of the given net segment.
	
	package type_net_segments is new doubly_linked_lists (type_net_segment);
	
	-- In a GUI a net may be visible within a submodule (local) or 
	-- it may be seen from the parent module (hierachical net) or
	-- it is visible across the whole scheamtic (global).
	-- In reality a net can only be either local or global. Hierarchic nets
	-- are just extensions of local or global nets with a different name.
	type type_strand_scope is (UNKNOWN, HIERARCHIC, LOCAL, GLOBAL);
	subtype type_net_scope is type_strand_scope range LOCAL..GLOBAL; 
	
	function to_string (scope : in type_strand_scope) return string;
	-- Returns the given scope as string.
	
	-- A strand is a collection of net segments which belong to each other. 
	-- Segments belong to each other because their start/end points meet.
	-- A strand has coordinates. 
	-- x/y position are the lowest values within the strand. see function lowest_xy.
	-- As long as strands are independed of each other they must 
	-- have a name and their own scope.
	type type_strand is record
		position	: kicad_coordinates.type_position; -- lowest x/y
		name		: et_general.type_net_name.bounded_string; -- example "CPU_CLOCK"		
		scope 		: type_strand_scope := type_strand_scope'first; -- example "local"
		segments	: type_net_segments.list;
	end record;

	function lowest_xy (
	-- Returns the lowest x/y position of the given strand.
		strand 			: in type_strand;
		log_threshold	: in et_string_processing.type_log_level
		) return et_coordinates.geometry.type_point;

	procedure add_strand (
	-- Adds a strand into the module (indicated by module_cursor).
		strand : in type_strand);
	
	-- Strands are collected in a list:
	package type_strands is new doubly_linked_lists (type_strand);
	
	type type_net is new et_schematic.type_net_base with record 
		scope 		: type_net_scope := type_net_scope'first; -- example "local"
		strands		: type_strands.list;
		-- CS differential status
	end record;
	
	-- Nets are collected in a map:
	package type_nets is new ordered_maps (
		key_type		=> et_general.type_net_name.bounded_string, -- example "CPU_CLOCK"
		"<"				=> et_general.type_net_name."<",
		element_type	=> type_net);
	
	
	procedure import_design (
		--first_instance 	: in boolean := false;
		project			: in et_project.type_project_name.bounded_string;								
		log_threshold	: in et_string_processing.type_log_level); 
	-- Imports the design as specified by project_name.
	-- Inserts the created submodule in container "modules".
	-- Leaves the module_cursor pointing where the module was inserted.

-- COMMENT MARKS
	comment_mark							: constant string := "#";

-- PROJECT FILE RELATED KEYWORDS AND VARIABLES
    project_header_eeschema                 : constant string (1..10) := "[eeschema]";
    project_header_eeschema_libraries       : constant string (1..20) := "[eeschema/libraries]";
    project_keyword_version                 : constant string (1..7)  := "version";
	project_keyword_library_directory       : constant string (1..6)  := "LibDir";
    project_keyword_library_name            : constant string (1..7)  := "LibName"; -- with index like "LibName1"


-- LIBRARIES

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


-- SCHEMATIC

    schematic_header_keyword_sys_name      : constant string (1..8) := "EESchema";
    schematic_header_keyword_schematic     : constant string (1..9) := "Schematic";
    schematic_header_keyword_file          : constant string (1..4) := "File";
    schematic_header_keyword_version       : constant string (1..7) := "Version";
    
    schematic_library                      : constant string (1..4) := "LIBS";
    schematic_eelayer                      : constant string (1..7) := "EELAYER";
    schematic_eelayer_end                  : constant string (1..3) := "END";   

	schematic_description_header           : constant string (1..6) := "$Descr";
	schematic_description_footer           : constant string (1..9) := "$EndDescr";
	schematic_sheet_header                 : constant string (1..6) := "$Sheet";
	schematic_sheet_footer                 : constant string (1..9) := "$EndSheet";	
	schematic_component_header             : constant string (1..5) := "$Comp";
	schematic_component_footer             : constant string (1..8) := "$EndComp";
	
    schematic_keyword_sheet                : constant string (1..5) := "Sheet";
    schematic_keyword_title                : constant string (1..5) := "Title";
    schematic_keyword_encoding             : constant string (1..8) := "encoding";    
    schematic_keyword_date                 : constant string (1..4) := "Date";
    schematic_keyword_revision             : constant string (1..3) := "Rev";
    schematic_keyword_company              : constant string (1..4) := "Comp";
	schematic_keyword_wire		           : constant string (1..4) := "Wire";
	schematic_keyword_connection           : constant string (1..10) := "Connection";	
	schematic_keyword_line                 : constant string (1..4) := "Line";	
	schematic_keyword_text                 : constant string (1..4) := "Text";
	schematic_keyword_label_simple         : constant string (1..5) := "Label";
	schematic_keyword_label_hierarchic     : constant string (1..6) := "HLabel";
	schematic_keyword_label_global         : constant string (1..6) := "GLabel";
	schematic_keyword_label_dir_bidir      : constant string (1..4) := "BiDi";	
	schematic_keyword_label_dir_input      : constant string (1..5) := "Input";
	schematic_keyword_label_dir_output     : constant string (1..6) := "Output";
	schematic_keyword_label_dir_passive	   : constant string (1..5) := "UnSpc";
    schematic_keyword_label_dir_tristate   : constant string (1..6) := "3State";
	schematic_keyword_note					: constant string (1..5) := "Notes";
	schematic_keyword_no_connection			: constant string (1..6) := "NoConn";    

    schematic_keyword_comment_1            : constant string (1..8) := "Comment1";
    schematic_keyword_comment_2            : constant string (1..8) := "Comment2";
    schematic_keyword_comment_3            : constant string (1..8) := "Comment3";
	schematic_keyword_comment_4            : constant string (1..8) := "Comment4";    
    
    schematic_keyword_sheet_pos_and_size   : constant string (1..1) := "S";
    schematic_keyword_sheet_timestamp      : constant string (1..1) := "U";    
	schematic_keyword_sheet_name           : constant string (1..2) := "F0";
	schematic_keyword_sheet_file           : constant string (1..2) := "F1";
	schematic_keyword_sheet_port           : constant string (1..2) := "F2";
	schematic_component_identifier_name    : constant string (1..1) := "L";
	schematic_component_identifier_unit	   : constant string (1..1) := "U";
	schematic_component_identifier_coord   : constant string (1..1) := "P";
	schematic_component_identifier_path    : constant string (1..2) := "AR"; -- CS: meaning unclear

	type type_schematic_unit_orientation	is range -1..1;
	type type_schematic_unit_mirror_style	is range -1..1;
   
    type type_label_orientation is range 0..3; -- also used for notes

    schematic_tilde : constant string (1..1) := "~";

	-- These strange strings are used to define the text style of 
	-- net labels and notes:
	text_schematic_style_normal : constant string (1..1) := "~";
    text_schematic_style_italic : constant string (1..6) := "Italic";    
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
	
	-- electrical direction of HIERARCHICAL SHEET PORTS
	type type_sheet_port_direction is (
		I, -- input
		O, -- output
		B, -- bidir
		T, -- tristate
		U); -- passive

	-- orientation of HIERARCHICAL SHEET PORTS
	type type_sheet_port_orientation is (
		L, 	-- on the left edge of the box
		R);	-- on the right edge of the box



-- LIBRARY
	
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

	
-- NET SEGMENT AND STRAND PROCESSING
	
	type type_wild_net_segment is new type_net_segment with record
		s, e	: boolean := false; -- flag indicates the end point being assumed
		picked	: boolean := false; -- flag indicates that the segment has been added to the anonymous net
	end record;

	type type_segment_side is (START_POINT, END_POINT); -- the end point of a segment	

	package type_wild_segments is new doubly_linked_lists (type_wild_net_segment);

	-- The function search_for_same_coordinates returns this type:
	type type_same_coord_result is record
		valid : boolean; -- indicates that a segment with matching coordinates has been found. When false, no segment found -> consider id and side invalid
		cursor : type_wild_segments.cursor; -- cursor of the segment found
		side : type_segment_side; -- end point of the segment found
	end record;

	-- An anonymous strand is a list of net segments that are connected with each other (by their start or end points):
	type type_anonymous_strand is record
		segments 	: type_net_segments.list; -- the net segments
		name 		: et_general.type_net_name.bounded_string;	-- the strand name (derived from net labels)
		scope 		: type_strand_scope := type_strand_scope'first; -- the scope (derived from net labels)
		processed	: boolean := false;	-- set once a label has been found on the net
	end record;

	-- We collect anonymous strands in a simple list:
	package type_anonymous_strands is new doubly_linked_lists (
		element_type => type_anonymous_strand);


    -- A hierachic sheet is a box with coordinates and length x/y.
	-- On the box edges are ports. 
	-- It serves as link between a hierachical net and the parent module.
	type type_hierarchic_sheet_port is record
		direction	: type_port_direction;
		text_size	: et_symbols.pac_text.type_text_size;
		coordinates	: et_coordinates.geometry.type_point;
        orientation	: et_coordinates.type_rotation;
        processed   : boolean; -- used when linking hierarchic nets
	end record;

	package type_hierarchic_sheet_ports is new ordered_maps (
		key_type		=> et_general.type_net_name.bounded_string,
		"<"				=> et_general.type_net_name."<",
		element_type	=> type_hierarchic_sheet_port);

	-- A hierachic sheet is identified by the file name and the sheet name itself.
	type type_hierarchic_sheet_name is record 
		file	: type_schematic_file_name.bounded_string; -- sensor.sch
		name	: type_submodule_name.bounded_string := type_submodule_name.to_bounded_string ("n/a"); -- sensor_outside
		-- "n/a" because the top level schematic never has a sheet name
	end record;
	
	type type_hierarchic_sheet is record
        text_size_of_name   : et_symbols.pac_text.type_text_size;
        text_size_of_file   : et_symbols.pac_text.type_text_size;
		coordinates		    : type_position;
        size_x, size_y      : et_coordinates.type_distance; -- size x/y of the box
		timestamp           : type_timestamp;
		ports				: type_hierarchic_sheet_ports.map;
	end record;

	procedure add_hierarchic_sheet (
	-- Inserts a hierachic sheet in the module (indicated by module_cursor)
		name		: in type_hierarchic_sheet_name;
		gui_sub_mod	: in type_hierarchic_sheet);

	function compare_hierarchic_sheets (left, right : in type_hierarchic_sheet_name) return boolean;
	-- Returns true if left comes before right. If left equals right, the return is false.			
	
    package type_hierarchic_sheets is new ordered_maps (
        key_type		=> type_hierarchic_sheet_name,
		"<" 			=> compare_hierarchic_sheets,
		element_type	=> type_hierarchic_sheet);


	-- When reading a schematic sheet, hierachic sheets might be discovered.
	-- They are returned to the parent unit in a list of schematic file names:
	type type_hierarchic_sheet_file_name_and_timestamp is record
		sheet		: type_hierarchic_sheet_name;	
		timestamp	: type_timestamp := timestamp_default;
	end record;
	
	package type_hierarchic_sheet_file_names is new vectors ( -- the bare list -- CS: better an ordered set ?
		index_type		=> positive,
		element_type	=> type_hierarchic_sheet_file_name_and_timestamp);

	type type_hierarchic_sheet_file_names_extended is record
		parent_sheet	: type_submodule_name.bounded_string;
		sheets			: type_hierarchic_sheet_file_names.vector;
		id				: positive; -- id of a sheet in the list
	end record;



	

	

	function library_name (text : in string) return type_library_name.bounded_string;
	-- extracts from a string like "bel_ic:S_SO14" the library name "bel_ic"

	function package_name (text : in string) return et_packages.type_component_package_name.bounded_string;
	-- extracts from a string like "bel_ic:S_SO14" the package name "S_SO14"


	function junction_sits_on_segment (
	-- Returns true if the given junction sits on the given net segment.
		junction	: in type_net_junction;
		segment		: in type_net_segment_base'class) 
		return boolean;
	
	function component_power_flag (cursor : in type_components_library.cursor)
	-- Returns the component power flag status.
		return type_power_flag;
	
-- 	function purpose ( -- CS move to et_schematic or et_project
-- 	-- Returns the purpose of the given component in the given module.
-- 	-- If no purpose specified for the component, an empty string is returned.						 
-- 		module_name		: in et_coordinates.type_submodule_name.bounded_string; -- led_matrix_2
-- 		reference		: in et_libraries.type_device_name; -- X701
-- 		log_threshold	: in et_string_processing.type_log_level)
-- 		return et_libraries.type_component_purpose.bounded_string;

	function first_strand return type_strands.cursor;
	-- Returns a cursor pointing to the first strand of the module (indicated by module_cursor).

	
	procedure update_strand_names (log_threshold : in et_string_processing.type_log_level);
	-- Tests if a power out port is connected to a strand and renames the strand if necessary.	
	-- Depending on the CAE system power-out or power-in ports may enforce their name on a strand.

	procedure write_strands (log_threshold : in et_string_processing.type_log_level);
	-- Writes a nice overview of strands, net segments and labels

	procedure write_nets (log_threshold : in et_string_processing.type_log_level);
	-- Writes a nice overview of all nets, strands, segments and labels.
	
	function components_in_net (
		module 			: in type_submodule_name.bounded_string; -- nucleo_core
		net				: in et_general.type_net_name.bounded_string; -- motor_on_off
		log_threshold	: in et_string_processing.type_log_level)
		return type_ports_with_reference.set;
	-- Returns a list of component ports that are connected with the given net.
	
	function real_components_in_net (
		module 			: in type_submodule_name.bounded_string; -- nucleo_core
		net				: in et_general.type_net_name.bounded_string; -- motor_on_off
		log_threshold	: in et_string_processing.type_log_level)
		return type_ports_with_reference.set;
	-- Returns a list of real component ports that are connected with the given net.

	
	function find_component (
	-- Searches the given library for the given component. Returns a cursor to that component.
		library		: in type_device_library_name.bounded_string; -- incl. path and file name
		component	: in type_component_generic_name.bounded_string) 
		return type_components_library.cursor;

	procedure reset_component_cursor (cursor : in out type_components_schematic.cursor);
	-- Resets the given component cursor to the begin of the component list
	-- of the module indicated by module_cursor.

	function build_portlists (log_threshold : in et_string_processing.type_log_level) 
		return type_portlists.map;
	-- Returns a list of components with the absolute positions of their ports as they are placed in the schematic.


	procedure check_open_ports (log_threshold : in et_string_processing.type_log_level);
	-- Warns about unintentionally left open ports. That are ports without a no_connection_flag.

	procedure check_non_deployed_units (log_threshold : in et_string_processing.type_log_level);
	-- Warns about not deployed units and open ports thereof.

	function module_count return natural;
	-- Returns the number of modules available in container "modules".
	
	procedure validate_module (
		module_name : in type_submodule_name.bounded_string);
	-- Tests if the given module exists in container "modules". Raises error if not existent.

	procedure add_sheet_header ( -- CS really requried ?
	-- Inserts a sheet header in the module (indicated by module_cursor).
		header	: in type_sheet_header;
		sheet	: in type_schematic_file_name.bounded_string);

	
	--	type type_frame is new et_libraries.type_frame with record
	type type_frame is new et_schematic_sheets.pac_frames.type_frame with record	
		coordinates : type_position; -- the position of the frame -- CS rename to position
	end record;

	procedure add_frame (
	-- Inserts a drawing frame in the module (indicated by module_cursor).
		frame	: in type_frame);
	
--     there are lots of drawing frames in a schematic contained in a list
	package type_frames is new doubly_linked_lists (type_frame);
	

	
	procedure check_junctions (log_threshold : in et_string_processing.type_log_level);
	-- Verifies that junctions are placed where net segments are connected with each other.
	-- NOTE: make_netlist detects if a junction is missing where a port is connected with a net.

	procedure check_orphaned_junctions (log_threshold : in et_string_processing.type_log_level);
	-- Warns about orphaned junctions.

	procedure check_misplaced_junctions (log_threshold : in et_string_processing.type_log_level);
	-- Warns about misplaced junctions.

	procedure check_misplaced_no_connection_flags (log_threshold : in et_string_processing.type_log_level);
	-- Warns about no_connection_flags placed at nets.

	procedure check_orphaned_no_connection_flags (log_threshold : in et_string_processing.type_log_level);
	-- Warns about orphaned no_connection_flags.

	procedure net_test (log_threshold : in et_string_processing.type_log_level);
	-- Tests nets for number of inputs, outputs, bidirs, ...

	function connected_net (
		port			: in type_port_of_module; -- contains something like nucleo_core_1 X701 port 4
		log_threshold	: in et_string_processing.type_log_level)
		return et_general.type_net_name.bounded_string;
	-- Returns the name of the net connected with the given port.
	-- Searches the netlist of the given module for the given port. 
	-- The net which is connected with the port is the net whose name
	-- is to be returned.
	-- If no net connected with the given port, an empty string is returned.
	
	procedure make_netlists (log_threshold : in et_string_processing.type_log_level);
	-- Builds the netlists of all modules.
	-- Currently there is only one module. kicad does not support multiple modules at the same time.	
	-- Addresses ALL components both virtual and real. Virtual components are things like GND or VCC symbols.
	-- Virtual components are filtered out on exporting the netlist in a file.
	-- Bases on the portlists and nets/strands information of the module.
	-- Detects if a junction is missing where a port is connected with a net.
	
	function terminal_count (
		reference		: in et_devices.type_device_name;
		log_threshold	: in et_string_processing.type_log_level)
		return et_devices.type_terminal_count;
	-- Returns the number of terminals of the given component reference.
	-- Requires module_cursor (global variable) to point to the current module.
	
	function connected_net (
	-- Returns the name of the net connected with the given component and terminal.
		module			: in type_submodule_name.bounded_string;	-- nucleo_core
		reference		: in et_devices.type_device_name;	-- IC45
		terminal		: in et_packages.type_terminal_name.bounded_string; -- E14
		log_threshold	: in et_string_processing.type_log_level)		
		return et_general.type_net_name.bounded_string;

-- 	procedure write_statistics (log_threshold : in et_string_processing.type_log_level);  -- CS this is general and should be in et_schematic
-- 	-- Writes the statistics on components and nets of the modules.
-- 	-- Distinguishes between CAD and CAM related things.

	-- This is general board stuff:
	type type_board is new et_pcb.type_board with record
		paper_size	: et_frames.type_paper_size;
	end record;

-- 	procedure multiple_purpose_warning ( -- CS move to et_schematic or et_project
-- 	-- Outputs an warning message on multiple usage of a purpose of a component category.
-- 		category		: in conventions.type_component_category; -- CONNECTOR, LIGHT_EMMITTING_DIODE, ...
-- 		purpose			: in et_libraries.type_component_purpose.bounded_string; -- PWR_IN, SYS_FAIL, ...
-- 		log_threshold	: in et_string_processing.type_log_level);
	
-- 	function multiple_purpose ( -- CS move to et_schematic or et_project
-- 	-- Returns the number of occurences of components with the given purpose and category.
-- 	-- Example: If there are two connectors with purpose "PWR_IN" the return is 2.
-- 		category		: in conventions.type_component_category; -- CONNECTOR, LIGHT_EMMITTING_DIODE, ...
-- 		purpose			: in et_libraries.type_component_purpose.bounded_string; -- PWR_IN, SYS_FAIL, ...
-- 		log_threshold	: in et_string_processing.type_log_level)
-- 		return natural;

	


	procedure write_note_properties (
		note			: in type_text;
		log_threshold	: in et_string_processing.type_log_level := 0);
	-- Writes the properties of the given note

	procedure add_note (note : in type_text);
	-- Inserts a note in the the module (indicated by module_cursor).
	
	package type_texts is new doubly_linked_lists (type_text);


	
-- MODULES
	
	type type_module is record
		board_available	: et_schematic.type_board_available := et_schematic.false;

		-- V4 uses search lists:
		-- The search list of project library directories and names:
		search_list_library_dirs	: type_project_lib_dirs.list; 	-- search list for library directories (active, passive, ...)
		search_list_library_comps	: type_library_names.list; 		-- search list for component libraries (bel_logic, bel_primitives, ...)
		-- NOTE: There is no search list for packages, because they are nowhere declared (not even in the project conf. file)

		-- V5 uses sym-lib-tables and fp-lib-tables to locate libraries:
		sym_lib_tables		: type_lib_table.list; -- symbols
		fp_lib_tables		: type_lib_table.list; -- footprints/packages		
		
		component_libraries	: type_libraries.map; -- V4 and V5
		footprints			: et_kicad_pcb.type_libraries.map;	-- V5 only. V4 packages are in et_kicad_pcb.package_libraries
		
		strands	    		: type_strands.list;				-- the strands of the module (incl. net names and segments)
		junctions			: type_junctions.list;				-- net junctions (for ERC, statistics, ...)

		components			: type_components_schematic.map;	-- the components of the module
		net_classes			: et_pcb.type_net_classes.map;		-- the net classes
		no_connections		: type_no_connection_flags.list;	-- the list of no-connection-flags
		portlists			: type_portlists.map;				-- the portlists of the module (components with their ports)
		netlist				: type_netlist.map;					-- net names and connected component ports (incl. position of port)
		hierarchic_sheets	: type_hierarchic_sheets.map;		-- hierarchic sheets. Serve as interface between hierarchic sheets.

		-- CS Drawing frames: not completely modelled. Still under construction.
		-- Thereis probably no need for a list of frames. Schematic has a single template for all sheets.
		-- Layout also has a template. 
		-- So the name of the templates for schematic and layout should suffice.
		frames      		: type_frames.list;					-- schematic frames (of both schematic and layout)
		
		notes       		: type_texts.list;					-- notes
	
		sheet_headers		: type_sheet_headers.map;			-- the list of sheet headers
		-- CS: images

		-- The nets of the module: net names, class, 
		-- schematic related stuff: strands, segments, labels, junctions
		-- board related stuff: lines, arcs, vias, polygons
		nets 	    	: type_nets.map; 
		
		-- General non-component related board stuff:
		-- paper size, silk screen, documentation, ...
		board			: type_board;
	end record;


	-- A collection of modules.
	-- CS: Currently kicad does not support multiple modules (so called multi-board support).
	-- Therefore the collection contains only one module.
	package type_modules is new ordered_maps (
		-- This is the module name like "MY_MOTOR_DRIVER" or "BLOOD_SAMPLE_ANALYZER"
		key_type 		=> type_submodule_name.bounded_string,
		"<" 			=> type_submodule_name."<",											 
		element_type 	=> type_module);

	modules : type_modules.map;
	module_cursor : type_modules.cursor;


end et_kicad;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
