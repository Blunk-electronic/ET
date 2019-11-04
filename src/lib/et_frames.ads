------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               FRAMES                                     --
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

with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.maps;			use ada.strings.maps;
with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;


with et_coordinates;			use et_coordinates;
with et_general;				use et_general;
with et_text;


package et_frames is

-- PAPER SIZES
    type type_paper_size is (A3, A4); -- CS: others ?
    paper_size_default : constant type_paper_size := A4;

	function to_paper_size (paper_size : in string) return type_paper_size;
	function to_string (paper_size : in type_paper_size) return string;

	type type_paper_orientation is (PORTRAIT, LANDSCAPE);
	orientation_default : constant type_paper_orientation := LANDSCAPE;

	
	template_file_name_length_max : constant positive := 300;
	template_file_name_dummy : constant string := "dummy_frame";

	
	-- A drawing frame is divided in columns and rows. The columns run from 1 to maximal 26.
	-- The rows run from A to Z.
	type type_rows is new positive range 1..26;
	rows_default : constant type_rows := 7;

	type type_columns is new positive range 1..26;
	columns_default : constant type_columns := 10;
	
	row_characters : constant character_set := to_set (span => ('A','Z')); -- CS currently not used
	-- CS row numbers must be mapped to row characters 
	
	type type_sectors is record
		rows	: type_rows := rows_default; 
		columns	: type_columns := columns_default;
	end record;
	
	type type_distance is new natural range 1 .. 10000;
	
	type type_size is record
		x	: type_distance := 280;
		y	: type_distance := 200;
	end record;

	subtype type_border_width is type_distance range 5 .. 20;
	border_width_default : constant type_border_width := 7;

	paper_size_A3_x : constant type_distance := 420;
	paper_size_A3_y : constant type_distance := 297;
	
	paper_size_A4_x : constant type_distance := 297;
	paper_size_A4_y : constant type_distance := 210;

	function paper_dimension (
	-- Returns for the given paper size, orientation and axis the correspoinding size in mm.
		paper_size	: in type_paper_size;
		orientation	: in type_paper_orientation := LANDSCAPE;
		axis		: in type_axis_2d)
		return type_distance;

	
	type type_position is record
		x, y : type_distance := border_width_default;
	end record;

	position_default : constant type_position := (1,1);


	
	type type_title_block_line is record
		start_point	: type_position := position_default;
		end_point	: type_position := position_default;
	end record;

	package pac_title_block_lines is new doubly_linked_lists (type_title_block_line);



	
	subtype type_text_size is type_distance range 1 .. 50;
	text_size_default : constant type_text_size := 3;


	
	type type_placeholder is tagged record
		size			: type_text_size := text_size_default;
		position		: type_position := position_default;
	end record;

	-- General placeholders used in both schematic and pcb title blocks:
	type type_placeholders is record
		company			: type_placeholder;
		customer		: type_placeholder;
		partcode		: type_placeholder;
		drawing_number	: type_placeholder;
		assembly_variant: type_placeholder; -- CS good idea to have it here ?
			
		project			: type_placeholder;
		file			: type_placeholder;
		revision		: type_placeholder;
		
		drawn_by		: type_placeholder;
		checked_by		: type_placeholder;
		approved_by		: type_placeholder;

		drawn_date		: type_placeholder;
		checked_date	: type_placeholder;
		approved_date	: type_placeholder;

		created_date	: type_placeholder;
		edited_date		: type_placeholder;
	end record;


	
	type type_text is new type_placeholder with record
		content			: et_text.type_text_content.bounded_string;
	end record;

	package pac_texts is new doubly_linked_lists (type_text);


	-- The basic title block:
	type type_title_block is tagged record
		position		: type_position := position_default;
		lines			: pac_title_block_lines.list;
		placeholders	: type_placeholders;
		texts			: pac_texts.list;
	end record;

	
	




	


-- SCHEMATIC RELATED

	-- $ET_FRAMES/drawing_frame_version_1.frs
	schematic_template_file_extension : constant string := "frs";
	
	package pac_schematic_template_name is new generic_bounded_length (template_file_name_length_max);

	schematic_frame_template_name_dummy : constant pac_schematic_template_name.bounded_string := 
		pac_schematic_template_name.to_bounded_string (template_file_name_dummy); -- cs compose extension
	
	function to_string (name : in pac_schematic_template_name.bounded_string) return string;
	function to_template_name (name : in string) return pac_schematic_template_name.bounded_string;


	
	type type_placeholders_schematic is record
		sheet_number	: type_placeholder;
		description		: type_placeholder;
		category		: type_placeholder; -- development, routing, product
	end record;
	
	type type_title_block_schematic is new type_title_block with record
		additional_placeholders : type_placeholders_schematic;
	end record;

	



	
-- PCB RELATED

	-- $ET_FRAMES/drawing_frame_version_1.frb
	pcb_template_file_extension : constant string := "frb";
	
	package pac_pcb_template_name is new generic_bounded_length (template_file_name_length_max);

	pcb_frame_template_name_dummy : constant pac_pcb_template_name.bounded_string := 
		pac_pcb_template_name.to_bounded_string (template_file_name_dummy); -- cs compose extension
	
	function to_string (name : in pac_pcb_template_name.bounded_string) return string;
	function to_template_name (name : in string) return pac_pcb_template_name.bounded_string;


	type type_placeholders_pcb is record
		silk_screen, assy_doc, 

		keepout, plated_millings, pcb_outline, 

		route_restrict, via_restrict, signal_layer	: type_placeholder;
		-- CS add more
	end record;
	
	type type_pcb_title_block is new type_title_block with record
		additional_placeholders : type_placeholders_pcb;
	end record;



	
-- THE GENERAL PARAMETERIZED FRAME
	
	type type_domain is (SCHEMATIC, PCB);
	
	type type_frame (domain : type_domain) is record
		paper			: type_paper_size := paper_size_default;
		orientation		: type_paper_orientation := orientation_default;
		border_width	: type_border_width := border_width_default;
		size			: type_size;
		sectors			: type_sectors;
	
		case domain is
			when SCHEMATIC =>
				title_block_schematic : type_title_block_schematic;

			when PCB =>
				title_block_pcb : type_pcb_title_block;
		end case;
	end record;



	
-- PCB RELATED
	-- This is the drawing frame used in a pcb layout:
	type type_frame_pcb is record
		template	: pac_pcb_template_name.bounded_string := pcb_frame_template_name_dummy;
			-- like $ET_FRAMES/drawing_frame_A3_landscape.frm

		frame		: type_frame (PCB);
	end record;

	


-- SCHEMATIC RELATED
	
	type type_schematic_sheet_category is (
		DEVELOPMENT,
		ROUTING,
		PRODUCT
		);

	schematic_sheet_category_default : constant type_schematic_sheet_category := DEVELOPMENT;
		
	type type_schematic_description is record
		content			: et_text.type_text_content.bounded_string;
		sheet_category	: type_schematic_sheet_category := schematic_sheet_category_default;
	end record;
		
	-- For each sheet a description is required. The descriptions 
	-- are ordered by the sheet numbers:
	package pac_schematic_descriptions is new ordered_maps (
		key_type		=> et_coordinates.type_sheet,
		element_type	=> type_schematic_description);


	-- The final drawing frames:
	type type_frames_schematic is record
		template		: pac_schematic_template_name.bounded_string := schematic_frame_template_name_dummy;
			-- like $ET_FRAMES/drawing_frame_A4_landscape.frs

		frame			: type_frame (SCHEMATIC);
		
		descriptions	: pac_schematic_descriptions.map;
	end record;

	
end et_frames;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
