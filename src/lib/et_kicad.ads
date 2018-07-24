------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET KICAD                                --
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
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;

with et_schematic;
with et_pcb;
with et_import;
with et_coordinates;			use et_coordinates;
with et_libraries;
with et_string_processing;

package et_kicad is

	system_name	: constant string (1..5) := "KiCad";
	
	-- If lines of a file are to be collected we use this simple list:
	package type_lines is new doubly_linked_lists (
		element_type => et_string_processing.type_fields_of_line,
		"=" => et_string_processing.lines_equally);

	lines : type_lines.list := type_lines.empty_list;
	line_cursor : type_lines.cursor;
	
    encoding_default 					: constant string (1..5) := "utf-8";	

	file_extension_project   			: constant string (1..3) := "pro";
	file_extension_schematic 			: constant string (1..3) := "sch";
	file_extension_schematic_lib		: constant string (1..3) := "lib";
	file_extension_board	 			: constant string (1..9) := "kicad_pcb";
    
    schematic_version                   : constant positive := 2;



	
	procedure import_design (
		first_instance 	: in boolean := false;
		project			: in et_schematic.type_project_name.bounded_string;								
		log_threshold	: in et_string_processing.type_log_level); 
	-- Imports the design as specified by project_name.
	-- Inserts the created submodule in the rig (see et_schematic.type_rig).
	-- Leaves the global module_cursor pointing where the module was inserted.
	-- If first_instance is false, the module gets the name as defined in the kicad project file.
	-- For a regular single design import this is the default.
	-- If first_instance is true, the module name further-on gets the instance appended.
	-- This is required for multiple design instantiations. (things like nucleo_core_1).

-- COMMENT MARKS
	comment_mark							: constant string (1..1) := "#";

-- PROJECT FILE RELATED KEYWORDS AND VARIABLES
    project_header_eeschema                 : constant string (1..10) := "[eeschema]";
    project_header_eeschema_libraries       : constant string (1..20) := "[eeschema/libraries]";
    project_keyword_version                 : constant string (1..7)  := "version";
	project_keyword_library_directory       : constant string (1..6)  := "LibDir";
    project_keyword_library_name            : constant string (1..7)  := "LibName"; -- with index like "LibName1"


-- LIBRARIES
	package type_libraries is new ordered_maps (
		key_type 		=> et_libraries.type_full_library_name.bounded_string, -- ../../lbr/passive/capacitors
		"<"				=> et_libraries.type_full_library_name."<",
		element_type 	=> et_libraries.type_components.map,
		"=" 			=> et_libraries.type_components."=");

	-- All component models are collected here.
	component_libraries : type_libraries.map;
	
	
-- LIBRARY SEARCH LISTS
	-- The order of project libraries and their containing directories 
	-- matters (for search operations).
	-- For this reason we keep them in simple lists.
	-- If multiple projects are imported, these lists are always
	-- cleared when a project file is read. See procedure read_project_file.
	-- The search lists are stored for each module (see type_module specs).
	
	-- search list for component library names
	search_list_component_libraries : et_libraries.type_library_names.list; -- bel_logic, bel_primitives, ...

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

	
-- COMPONENT TEXT FIELDS

	-- In compoenent libraries and schematic, a text field is indicated by letter "F":
	component_field_identifier				: constant string (1..1) := "F";

	-- We limit the number of fields in the component library to this constant.
	library_component_field_count_max 	: constant positive := 9;


	type type_component_field_id is range 0..library_component_field_count_max;
	component_field_reference		: constant type_component_field_id := 0;
	component_field_value			: constant type_component_field_id := 1;
	component_field_package			: constant type_component_field_id := 2;
	component_field_datasheet		: constant type_component_field_id := 3;
	component_field_commissioned	: constant type_component_field_id := 4;
	component_field_updated			: constant type_component_field_id := 5;
	component_field_author			: constant type_component_field_id := 6;
	component_field_partcode		: constant type_component_field_id := 7;
	component_field_bom				: constant type_component_field_id := 8;
	component_field_purpose			: constant type_component_field_id := 9;


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
	subtype type_supply_pin_name_position_offset is type_distance range 0.00 .. type_distance'last;

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
	component_prefix_characters : character_set := et_libraries.component_prefix_characters 
		or to_set (schematic_component_power_symbol_prefix);

	-- These characters are allowed for a component reference. 
	-- This character set is used for prechecking references (like IC904 or #PWR) if 
	-- provided as string together with procedure check_reference_characters (see et_libraries):
	component_reference_characters : character_set := component_prefix_characters or to_set (span => ('0','9'));

	-- Kicad combines the library and package/footprint name in a single string like bel_capacitors:S_0805
	-- Therefore the character set used here includes the colon additionally.
	component_package_name_characters : character_set := et_libraries.component_package_name_characters or to_set (':');

	-- In the library a component name may have a tilde. Therefore we extend the standard character set by a tilde.
	component_generic_name_characters : character_set := et_libraries.component_generic_name_characters or to_set ('~');
	
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
	type type_wild_net_segment is new et_schematic.type_net_segment with record
		s, e : boolean := false; -- flag indicates the end point beeing assumed
		picked : boolean := false; -- flag indicates that the segment has been added to the anonymous net
	end record;

	type type_segment_side is (start_point, end_point); -- the end point of a segment	

	package type_wild_segments is new doubly_linked_lists ( 
		element_type => type_wild_net_segment);

	-- The function search_for_same_coordinates returns this type:
	type type_same_coord_result is record
		valid : boolean; -- indicates that a segment with matching coordinates has been found. When false, no segment found -> consider id and side invalid
		cursor : type_wild_segments.cursor; -- cursor of the segment found
		side : type_segment_side; -- end point of the segment found
	end record;

	-- An anonymous strand is a list of net segments that are connected with each other (by their start or end points):
	type type_anonymous_strand is record
		segments 	: et_schematic.type_net_segments.list;			-- the net segments
		name 		: et_schematic.type_net_name.bounded_string;	-- the strand name (derived from net labels)
		scope 		: et_schematic.type_strand_scope := et_schematic.type_strand_scope'first; -- the scope (derived from net labels)
		processed	: boolean := false;	-- set once a label has been found on the net
	end record;

	-- We collect anonymous strands in a simple list:
	package type_anonymous_strands is new doubly_linked_lists (
		element_type => type_anonymous_strand);



	function library_name (text : in string) return et_libraries.type_library_name.bounded_string;
	-- extracts from a string like "bel_ic:S_SO14" the library name "bel_ic"

	function package_name (text : in string) return et_libraries.type_component_package_name.bounded_string;
	-- extracts from a string like "bel_ic:S_SO14" the package name "S_SO14"


	function junction_sits_on_segment (
	-- Returns true if the given junction sits on the given net segment.
		junction	: in et_schematic.type_net_junction;
		segment		: in et_schematic.type_net_segment'class) 
		return boolean;
	
	function component_power_flag (cursor : in et_schematic.type_components.cursor)
	-- Returns the component power flag status.
		return et_libraries.type_power_flag;
	
	function purpose (
	-- Returns the purpose of the given component in the given module.
	-- If no purpose specified for the component, an empty string is returned.						 
		module_name		: in type_submodule_name.bounded_string; -- led_matrix_2
		reference		: in et_libraries.type_component_reference; -- X701
		log_threshold	: in et_string_processing.type_log_level)
		return et_libraries.type_component_purpose.bounded_string;

	procedure add_strand (
	-- Adds a strand into the module (indicated by module_cursor).
		strand : in et_schematic.type_strand);

	function first_strand return et_schematic.type_strands.cursor;
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
		net				: in et_schematic.type_net_name.bounded_string; -- motor_on_off
		log_threshold	: in et_string_processing.type_log_level)
		return et_schematic.type_ports_with_reference.set;
	-- Returns a list of component ports that are connected with the given net.
	
	function real_components_in_net (
		module 			: in type_submodule_name.bounded_string; -- nucleo_core
		net				: in et_schematic.type_net_name.bounded_string; -- motor_on_off
		log_threshold	: in et_string_processing.type_log_level)
		return et_schematic.type_ports_with_reference.set;
	-- Returns a list of real component ports that are connected with the given net.

	
	function find_component (
	-- Searches the given library for the given component. Returns a cursor to that component.
		library		: in et_libraries.type_full_library_name.bounded_string; -- consists of group name and actual library name
		component	: in et_libraries.type_component_generic_name.bounded_string) 
		return et_libraries.type_components.cursor;

	procedure reset_component_cursor (cursor : in out et_schematic.type_components.cursor);
	-- Resets the given component cursor to the begin of the component list
	-- of the module indicated by module_cursor.

	function build_portlists (log_threshold : in et_string_processing.type_log_level) 
		return et_schematic.type_portlists.map;
	-- Returns a list of components with the absolute positions of their ports as they are placed in the schematic.


	procedure check_open_ports (log_threshold : in et_string_processing.type_log_level);
	-- Warns about unintentionally left open ports. That are ports without a no_connection_flag.

	procedure check_non_deployed_units (log_threshold : in et_string_processing.type_log_level);
	-- Warns about not deployed units and open ports thereof.

	function module_count return natural;
	-- Returns the number of modules of the rig.
	
	procedure copy_module (
	-- Copyies a rig module. 
	-- If copy_last is true (default) the last module in the rig is copied. 
	-- If copy_last is false, the module with given name_origin is copied.
	-- The module instance is always incremented automatically.
		copy_last		: in boolean := true;						  
		name_origin		: in et_coordinates.type_submodule_name.bounded_string := et_coordinates.type_submodule_name.to_bounded_string (""); -- nucleo_core_3
		log_threshold	: in et_string_processing.type_log_level);

	procedure validate_module (
		module_name : in et_coordinates.type_submodule_name.bounded_string);
	-- Tests if the given module exists in the rig. Raises error if not existent.

	procedure add_gui_submodule (
	-- Inserts a gui submodule in the module (indicated by module_cursor)
		name		: in et_coordinates.type_submodule_name.bounded_string;
		gui_sub_mod	: in et_schematic.type_gui_submodule);
	
	procedure add_sheet_header ( -- CS really requried ?
	-- Inserts a sheet header in the module (indicated by module_cursor).
		header	: in et_schematic.type_sheet_header;
		sheet	: in et_schematic.type_schematic_file_name.bounded_string);

	procedure add_frame ( -- CS really requried ?
	-- Inserts a drawing frame in the module (indicated by module_cursor).
		frame	: in et_schematic.type_frame);

	procedure add_title_block ( -- CS really requried ?
	-- Inserts a title block in the module (indicated by module_cursor).
		tblock	: in et_schematic.type_title_block);

	procedure add_note (
	-- Inserts a note in the the module (indicated by module_cursor).
		note	: in et_schematic.type_note);

	procedure add_component (
	-- Adds a component into the the module (indicated by module_cursor).
		reference		: in et_libraries.type_component_reference;
		component		: in et_schematic.type_component;
		log_threshold	: in et_string_processing.type_log_level);

	procedure add_unit (
	-- Adds a unit into the given commponent.
		reference		: in et_libraries.type_component_reference;
		unit_name		: in et_libraries.type_unit_name.bounded_string;
		unit 			: in et_schematic.type_unit;
		log_threshold	: in et_string_processing.type_log_level);

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
		port			: in et_schematic.type_port_of_module; -- contains something like nucleo_core_1 X701 port 4
		log_threshold	: in et_string_processing.type_log_level)
		return et_schematic.type_net_name.bounded_string;
	-- Returns the name of the net connected with the given port.
	-- Searches the netlist of the given module for the given port. 
	-- The net which is connected with the port is the net whose name
	-- is to be returned.
	-- If no net connected with the given port, an empty string is returned.
	
	procedure make_netlists (log_threshold : in et_string_processing.type_log_level);
	-- Builds the netlists of all modules of the rig.
	-- Addresses ALL components both virtual and real. Virtual components are things like GND or VCC symbols.
	-- Virtual components are filtered out on exporting the netlist in a file.
	-- Bases on the portlists and nets/strands information of the module.

	function terminal_count (
		reference		: in et_libraries.type_component_reference;
		log_threshold	: in et_string_processing.type_log_level)
		return et_libraries.type_terminal_count;
	-- Returns the number of terminals of the given component reference.
	-- Requires module_cursor (global variable) to point to the current module.
	
	function to_terminal (
		port 			: in et_schematic.type_port_with_reference;
		module			: in type_submodule_name.bounded_string; -- the name of the module							 
		log_threshold 	: in et_string_processing.type_log_level)
		return et_libraries.type_terminal;
	-- Returns the terminal and unit name of the given port in a composite type.
	-- Raises error if given port is of a virtual component (appearance sch).

	function connected_net (
	-- Returns the name of the net connected with the given component and terminal.
		module			: in type_submodule_name.bounded_string;	-- nucleo_core
		reference		: in et_libraries.type_component_reference;	-- IC45
		terminal		: in et_libraries.type_terminal_name.bounded_string; -- E14
		log_threshold	: in et_string_processing.type_log_level)		
		return et_schematic.type_net_name.bounded_string;

	procedure export_netlists (log_threshold : in et_string_processing.type_log_level);
	-- Exports/Writes the netlists of the rig in separate files.
	-- Netlists are exported in individual project directories in the work directory of ET.
	-- These project directories have the same name as the module indicated by module_cursor.
	-- Addresses real components exclusively. Virtual things like GND symbols are not exported.
	-- Call this procedure after executing procedure make_netlist !

	procedure write_statistics (log_threshold : in et_string_processing.type_log_level);
	-- Writes the statistics on components and nets of the rig.
	-- Distinguishes between CAD and CAM related things.

	procedure export_bom (log_threshold : in et_string_processing.type_log_level);
	-- Generates a bom file. This file is csv formatted and is to be processed by
	-- other ERP tools (like stock_manager, see <https://github.com/Blunk-electronic/stock_manager>)


-- MODULES
	
	type type_module is record
		generic_name	: et_coordinates.type_submodule_name.bounded_string;
		instance		: type_submodule_instance;

		-- The search list of project library directories and names:
		library_dirs 	: type_project_lib_dirs.list; 				-- search list for library directories (active, passive, ...)
		libraries_comp	: et_libraries.type_library_names.list; 	-- search list for component libraries (bel_logic, bel_primitives, ...)
		-- NOTE: There is no search list for packages, because they are nowhere declared (not even in the project conf. file)
		
		strands	    	: et_schematic.type_strands.list;			-- the strands of the module
		junctions		: et_schematic.type_junctions.list;			-- net junctions

		components		: et_schematic.type_components.map;			-- the components of the module
		net_classes		: et_pcb.type_net_classes.map;				-- the net classes
		no_connections	: et_schematic.type_no_connection_flags.list;-- the list of no-connection-flags
		portlists		: et_schematic.type_portlists.map;			-- the portlists of the module
		netlist			: et_schematic.type_netlist.map;			-- the netlist
		submodules  	: et_schematic.type_gui_submodules.map;		-- graphical representations of submodules. -- GUI relevant
        frames      	: et_schematic.type_frames.list;			-- frames -- GUI relevant
        title_blocks	: et_schematic.type_title_blocks.list;		-- title blocks -- GUI relevant
		notes       	: et_schematic.type_texts.list;				-- notes

		sheet_headers	: et_schematic.type_sheet_headers.map;		-- the list of sheet headers
		-- CS: images

		-- the nets of the module (incl. routing information from the board):
		nets 	    	: et_schematic.type_nets.map;				
		
		-- General non-component related board stuff (silk screen, documentation, ...):
		board			: et_pcb.type_board;
	end record;


	-- A rig is a set of modules:
	package type_rig is new ordered_maps (
	-- CS: package type_modules is new ordered_maps (
		key_type 		=> et_coordinates.type_submodule_name.bounded_string, -- example "MOTOR_DRIVER"
		"<" 			=> et_coordinates.type_submodule_name."<",											 
		element_type 	=> type_module);

	rig : type_rig.map;
	module_cursor : type_rig.cursor;
	
end et_kicad;

-- Soli Deo Gloria
