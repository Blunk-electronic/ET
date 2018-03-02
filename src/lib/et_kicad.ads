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

with et_schematic;
with et_import;
with et_coordinates;			use et_coordinates;
with et_libraries;
with et_string_processing;

package et_kicad is

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
	--CS: file_extension_board	 			: constant string (1..3) := "brd";
    
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

	-- when reading the projec file, the project library names are collected here temporarily:
	tmp_project_libraries					: et_libraries.type_full_library_names.list;

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


	
end et_kicad;

-- Soli Deo Gloria
