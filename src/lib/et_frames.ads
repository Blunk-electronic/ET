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


	
	type type_title_block is tagged record
		position		: type_position := position_default;
		lines			: pac_title_block_lines.list;
		placeholders	: type_placeholders;
		texts			: pac_texts.list;
	end record;

	
	
	type type_frame is tagged record
		paper			: type_paper_size := paper_size_default;
		orientation		: type_paper_orientation := orientation_default;
		border_width	: type_border_width := border_width_default;
		size			: type_size;
		sectors			: type_sectors;
	end record;



	
	function paper_dimension (
	-- Returns for the given paper size, orientation and axis the correspoinding size in mm.
		paper_size	: in type_paper_size;
		orientation	: in type_paper_orientation := LANDSCAPE;
		axis		: in type_axis_2d)
		return type_distance;


	
end et_frames;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
