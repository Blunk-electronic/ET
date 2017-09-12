------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET KICAD                                --
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

with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;
with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;

with et_general;				use et_general;
with et_schematic;
with et_import;		            use et_import;
with et_libraries;
with et_string_processing;

package et_kicad is

    encoding_default 					: constant string (1..5) := "utf-8";	

	file_extension_project   			: constant string (1..3) := "pro";
	file_extension_schematic 			: constant string (1..3) := "sch";
	file_extension_schematic_lib		: constant string (1..3) := "lib";
	--CS: file_extension_board	 			: constant string (1..3) := "brd";


    
    schematic_version                   : constant positive := 2;
    
	procedure import_design; 

-- COMMENT MARKS
	comment_mark							: constant string (1..1) := "#";

-- PROJECT FILE RELATED KEYWORDS AND VARIABLES
    project_header_eeschema                 : constant string (1..10) := "[eeschema]";
    project_header_eeschema_libraries       : constant string (1..20) := "[eeschema/libraries]";
    project_keyword_version                 : constant string (1..7)  := "version";
	project_keyword_library_directory       : constant string (1..6)  := "LibDir";
    project_keyword_library_name            : constant string (1..7)  := "LibName"; -- with index like "LibName1"

	-- when reading the projec file, the project library names are collected here temporarily:
	tmp_project_libraries					: et_libraries.type_library_names.list;

-- COMPONENT TEXT FIELDS

	-- In compoenent libraries and schematic, a text field is indicated by letter "F":
	component_field_identifier				: constant string (1..1) := "F";

	library_component_field_count_max 	: constant positive := 8;
	-- CS: we limit the number of fields in the compoentn library. Useful limit ?

	type type_component_field_id is range 0..library_component_field_count_max;
	component_field_reference		: constant type_component_field_id := 0;
	component_field_value			: constant type_component_field_id := 1;
	component_field_footprint		: constant type_component_field_id := 2;
	component_field_datasheet		: constant type_component_field_id := 3;
	component_field_commissioned	: constant type_component_field_id := 4;
	component_field_updated			: constant type_component_field_id := 5;
	component_field_author			: constant type_component_field_id := 6;
	component_field_partcode		: constant type_component_field_id := 7;
	component_field_function		: constant type_component_field_id := 8;

-- GRID AND COORDINATES
	-- Objects may be placed at a certain angle. The unit is tenth of degree:
	type type_angle is range -3599 .. 3599;


    -- headers, footers, keywords 

--     EESchema Schematic File Version 2
--     LIBS:nucleo_core-rescue
--     LIBS:power
--     LIBS:bel_connectors_and_jumpers
--     LIBS:bel_primitives
--     LIBS:bel_stm32
--     LIBS:nucleo_core-cache
--     EELAYER 25 0
--     EELAYER END

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
	schematic_keyword_note                 : constant string (1..5) := "Notes";    

    schematic_keyword_comment_1            : constant string (1..8) := "Comment1";
    schematic_keyword_comment_2            : constant string (1..8) := "Comment2";
    schematic_keyword_comment_3            : constant string (1..8) := "Comment3";
	schematic_keyword_comment_4            : constant string (1..8) := "Comment4";    
    
    schematic_keyword_sheet_pos_and_size   : constant string (1..1) := "S";
    schematic_keyword_sheet_timestamp      : constant string (1..1) := "U";    
	schematic_keyword_sheet_name           : constant string (1..2) := "F0";
	schematic_keyword_sheet_file           : constant string (1..2) := "F1";
	schematic_component_identifier_name    : constant string (1..1) := "L";
	schematic_component_identifier_unit	   : constant string (1..1) := "U";
	schematic_component_identifier_coord   : constant string (1..1) := "P";

	-- In schematic, a power symbol/component has a hash as first character like "L P3V3 #PWR07"
	schematic_component_power_symbol_prefix: constant character := '#';

	-- The name of a sheet, the title (and optionally the file) may have 100 characters which seems sufficient for now.
	-- If sheets are stored as files, the file name may have the same length.
 	sheet_name_length	: constant natural := 100;
	package type_sheet_name is new generic_bounded_length(sheet_name_length); use type_sheet_name;
	package type_sheet_file is new generic_bounded_length(sheet_name_length); use type_sheet_file;	
    
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

	-- electrical direction (NOTE: since kicad uses upper and lower case letters here,
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
	
	
    -- SHEET HEADERS
    -- sheet files have a header with meta information:
    -- stuff like:
    --     EESchema Schematic File Version 2
    --     LIBS:nucleo_core-rescue
    --     LIBS:power
    --     LIBS:bel_connectors_and_jumpers
    --     LIBS:bel_primitives
    --     LIBS:bel_stm32
    --     LIBS:nucleo_core-cache
    --     EELAYER 25 0
	--     EELAYER END

	-- The library names in the header are mapped to a list of components.
	-- So for each library in the header we have a components listing.
	-- 	use type_library_name;
	-- 	use type_list_of_components;
-- 	package type_component_library is new ordered_maps (
-- 		key_type => et_general.type_library_name.bounded_string, -- like bel_primitives
-- 			"<" => et_general.type_library_name."<",
-- 		element_type => et_kicad_libraries.type_list_of_components.map, -- the components like R, C, L, ...
-- 			"=" => et_kicad_libraries.type_list_of_components."="
-- 		);
-- 		
	-- The sheet header in turn is a composite of a list of libraries and other things:
	-- The sheet header contains the libraries and their content.
	-- We use a doubly linked list because the order of the library names must be kept.
    type type_sheet_header is record
        version     : positive; -- 2    
		libraries   : et_libraries.type_library_names.list; -- CS: probably not used by kicad, just information
		--libraries	: type_list_of_library_names.map;
        eelayer_a   : positive; -- 25 -- CS: meaning not clear, probably not used
        eelayer_b   : natural; -- 0 -- CS: meaning not clear, probably not used
    end record;

	-- Since there are usually many sheets, we need a map of headers.
	-- The headers are finally stored in a map and accessed by the name of the sheet file.
	-- Why ? When schematic files are exported, their headers must be restored to the original state.
    package type_list_of_sheet_headers is new ordered_maps (
        key_type => type_sheet_file.bounded_string,
        element_type => type_sheet_header);
    list_of_sheet_headers : type_list_of_sheet_headers.map;


-- LIBRARY
	
	version_header : constant string (1..28) := "EESchema-LIBRARY Version 2.3";

	def			: constant string (1..3) := "DEF";
	enddef		: constant string (1..6) := "ENDDEF";
	draw		: constant string (1..4) := "DRAW";
	enddraw		: constant string (1..7) := "ENDDRAW";
	fplist		: constant string (1..7) := "$FPLIST";
	endfplist	: constant string (1..10) := "$ENDFPLIST";

	-- The distance of the pin name from the pin itself (supply pins only)
	subtype type_supply_pin_name_position_offset is et_libraries.type_grid range 0.00 .. et_libraries.type_grid'last;

	-- KiCad supports up to 64 units within a component
	unit_count_max : constant positive := 64;
	type type_units_total is new positive range 1..unit_count_max;
	type type_unit_id is new natural range natural'first..unit_count_max;

	type type_library_component_appearance is (N, P); -- normal or power

	type type_symbol_interchangeable is (L, F); -- L means swapping not allowed, F means swapping allowed 
	type type_show_pin_number is (Y, N); -- show pin/pad number yes/no
	type type_show_pin_name is (Y, N); -- show pin (better port) name yes/no

	type type_alternative_representation is new natural range 0..1;
	alternative_representation_yes	: constant type_alternative_representation := 0;
	alternative_representation_no	: constant type_alternative_representation := 1;


-- IMPORT

	-- section flags
	description_entered			: boolean := false;
	description_processed		: boolean := false;
	sheet_description_entered	: boolean := false;

	component_entered 			: boolean := false;
	net_segment_entered			: boolean := false;
	simple_label_entered		: boolean := false;
	tag_label_entered 			: boolean := false;	
	note_entered				: boolean := false;	

	-- temporarily storage
	tmp_frame 				: et_schematic.type_frame; -- a single drawing frame
	tmp_title_block_text 	: et_schematic.type_title_block_text; -- a single text within the title block
	tmp_title_block_texts 	: et_schematic.type_list_of_title_block_texts.vector; -- a list of title block texts
	tmp_title_block 		: et_schematic.type_title_block; -- a full title block	
	tmp_junction			: et_schematic.type_net_junction;
	tmp_simple_net_label	: et_schematic.type_net_label_simple;
	tmp_tag_net_label		: et_schematic.type_net_label_tag;
	tmp_note				: et_schematic.type_note;	

	tmp_module_name 				: et_schematic.type_submodule_name.bounded_string;	
	tmp_component_name_in_lib		: et_libraries.type_component_name.bounded_string;
	tmp_component_appearance		: et_libraries.type_component_appearance := et_libraries.sch;
	tmp_component_reference			: et_libraries.type_component_reference;
	tmp_component_unit_name			: et_libraries.type_unit_name.bounded_string;
	tmp_component_alt_repres		: et_schematic.type_alternative_representation;
	tmp_component_timestamp			: et_string_processing.type_timestamp;
	tmp_component_position			: et_schematic.type_coordinates;

	tmp_component_text_reference	: et_libraries.type_text (meaning => et_libraries.reference);
	tmp_component_text_value		: et_libraries.type_text (meaning => et_libraries.value);
	tmp_component_text_commissioned : et_libraries.type_text (meaning => et_libraries.commissioned);
	tmp_component_text_updated		: et_libraries.type_text (meaning => et_libraries.updated);
	tmp_component_text_author		: et_libraries.type_text (meaning => et_libraries.author);
	tmp_component_text_packge		: et_libraries.type_text (meaning => et_libraries.packge); -- like "SOT23"
	tmp_component_text_datasheet	: et_libraries.type_text (meaning => et_libraries.datasheet); -- might be useful for some special components
	tmp_component_text_purpose		: et_libraries.type_text (meaning => et_libraries.purpose); -- to be filled in schematic later by the user
	tmp_component_text_partcode		: et_libraries.type_text (meaning => et_libraries.partcode); -- like "R_PAC_S_0805_VAL_"			
	
	-- These are the "field found" flags. They signal if a particular text field has been found.
	-- They are cleared by procdure "init_temp_variables" once a new compoenent is entered.
	-- They are evaluated when a component section is left.
	tmp_component_text_reference_found		: boolean;
	tmp_component_text_value_found			: boolean;
	tmp_component_text_commissioned_found	: boolean;
	tmp_component_text_updated_found		: boolean;
	tmp_component_text_author_found			: boolean;
	tmp_component_text_packge_found			: boolean;
	tmp_component_text_datasheet_found		: boolean;
	tmp_component_text_purpose_found		: boolean;
	tmp_component_text_partcode_found		: boolean;

	
	
	type type_wild_net_segment is new et_schematic.type_net_segment with record
		s, e : boolean := false; -- flag indicates the end point beeing assumed
		picked : boolean := false; -- flag indicates that the segment has been added to the anonymous net
	end record;

	tmp_segment				: type_wild_net_segment;
	
	type type_segment_side is (start_point, end_point ); -- the end point of a segment	


	package type_wild_segments is new doubly_linked_lists ( 
		element_type => type_wild_net_segment);
		

	-- The function search_for_same_coordinates returns this type:
	type type_same_coord_result is record
		valid : boolean; -- indicates that a segment with matching coordinates has been found. When false, no segment found -> consider id and side invalid
		cursor : type_wild_segments.cursor; -- cursor of the segment found
		side : type_segment_side; -- end point of the segment found
	end record;



	
end et_kicad;

-- Soli Deo Gloria
