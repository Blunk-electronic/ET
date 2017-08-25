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
-- with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;

with et_general;				use et_general;
with et_schematic;				use et_schematic;
with et_import;		            use et_import;
with et_libraries;

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

	-- project libraries:
	project_keyword_library_directory       : constant string (1..6)  := "LibDir";

    project_keyword_library_name            : constant string (1..7)  := "LibName"; -- with index like "LibName1"


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
	
    -- fields
	type type_field_orientation is (H, V); -- horizontal, vertical
	type type_field_alignment_horizontal is (R, C, L); -- right, center, left
	type type_field_alignment_vertical is (T, C, B);   -- top, center, bottom
	type type_library_field_visible is (V, I); -- visible, invisible

-- 	library_draw_identifier_polyline	: constant string (1..1) := "P";
-- 	library_draw_identifier_rectangle	: constant string (1..1) := "S";
-- 	library_draw_identifier_circle		: constant string (1..1) := "C";	
-- 	library_draw_identifier_arc			: constant string (1..1) := "A";
-- 	library_draw_identifier_pin			: constant string (1..1) := "X";
-- 	library_draw_identifier_text		: constant string (1..1) := "T";	

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

	library_pin_electrical_type_passive			: constant string (1..1) := "P";
	library_pin_electrical_type_input			: constant string (1..1) := "I";
	library_pin_electrical_type_output			: constant string (1..1) := "O";	
	library_pin_electrical_type_bidir			: constant string (1..1) := "B";
	library_pin_electrical_type_tristate		: constant string (1..1) := "T";
	library_pin_electrical_type_unspecified		: constant string (1..1) := "U";
	library_pin_electrical_type_power_in		: constant string (1..1) := "W";
	library_pin_electrical_type_power_out		: constant string (1..1) := "W";
	library_pin_electrical_type_open_collector	: constant string (1..1) := "C";
	library_pin_electrical_type_open_emitter	: constant string (1..1) := "E";
	library_pin_electrical_type_not_connected	: constant string (1..1) := "N";	

	library_pin_graphical_style_inverted		: constant string (1..1) := "I";
	library_pin_graphical_style_clock			: constant string (1..1) := "C";
	library_pin_graphical_style_inverted_clock	: constant string (1..2) := "IC";
	library_pin_graphical_style_input_low		: constant string (1..1) := "L";
	library_pin_graphical_style_clock_low		: constant string (1..2) := "CL";
	library_pin_graphical_style_output_low		: constant string (1..1) := "V";
	library_pin_graphical_style_falling_edge_clk: constant string (1..1) := "F";
	library_pin_graphical_style_non_logic		: constant string (1..1) := "X";

	library_pin_graphical_style_invisible_inverted			: constant string (1..2) := "NI";
	library_pin_graphical_style_invisible_clock				: constant string (1..2) := "NC";
	library_pin_graphical_style_invisible_inverted_clock	: constant string (1..3) := "NIC";
	library_pin_graphical_style_invisible_input_low			: constant string (1..2) := "NL";
	library_pin_graphical_style_invisible_clock_low			: constant string (1..3) := "NCL";
	library_pin_graphical_style_invisible_output_low		: constant string (1..2) := "NV";
	library_pin_graphical_style_invisible_falling_edge_clk	: constant string (1..2) := "NF";
	library_pin_graphical_style_invisible_non_logic			: constant string (1..2) := "NX";
	
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
		libraries   : et_libraries.type_list_of_library_names.list; -- CS: probably not used by kicad, just information
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

end et_kicad;

-- Soli Deo Gloria
