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
with ada.containers.indefinite_doubly_linked_lists;

with et_general;				use et_general;
with et_geometry;
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
	
	type type_dimension is new natural range 1 .. 10000;
	
	type type_size is record
		x	: type_dimension := 280;
		y	: type_dimension := 200;
	end record;

	subtype type_border_width is type_dimension range 5 .. 20;
	border_width_default : constant type_border_width := 7;

	paper_size_A3_x : constant type_dimension := 420;
	paper_size_A3_y : constant type_dimension := 297;
	
	paper_size_A4_x : constant type_dimension := 297;
	paper_size_A4_y : constant type_dimension := 210;
	
	type type_position_2 is record
		x, y : type_dimension := border_width_default;
	end record;

	position_default : constant type_position_2 := (1,1);
	
	type type_title_block_line is record
		start_point	: type_position_2 := position_default;
		end_point	: type_position_2 := position_default;
	end record;

	package pac_title_block_lines is new doubly_linked_lists (type_title_block_line);

	subtype type_text_size is type_dimension range 1 .. 50;
	text_size_default : constant type_text_size := 3;
	
	type type_text_placeholder_2 is tagged record
		size			: type_text_size := text_size_default;
		position		: type_position_2 := position_default;
	end record;
	
	type type_text_placeholders_2 is record
		company			: type_text_placeholder_2;
		customer		: type_text_placeholder_2;
		partcode		: type_text_placeholder_2;
		drawing_number	: type_text_placeholder_2;
		assembly_variant: type_text_placeholder_2; -- CS good idea to have it here ?
			
		project			: type_text_placeholder_2;
		file			: type_text_placeholder_2;
		revision		: type_text_placeholder_2;
		
		drawn_by		: type_text_placeholder_2;
		checked_by		: type_text_placeholder_2;
		approved_by		: type_text_placeholder_2;

		drawn_date		: type_text_placeholder_2;
		checked_date	: type_text_placeholder_2;
		approved_date	: type_text_placeholder_2;

		created_date	: type_text_placeholder_2;
		edited_date		: type_text_placeholder_2;
	end record;

	
	type type_text_2 is new type_text_placeholder_2 with record
		content			: et_text.type_text_content.bounded_string;
	end record;

	package pac_texts_2 is new doubly_linked_lists (type_text_2);

	
	type type_title_block is tagged record
		position		: type_position_2 := position_default;
		lines			: pac_title_block_lines.list;
		placeholders	: type_text_placeholders_2;
		texts			: pac_texts_2.list;
	end record;

	
	type type_frame is tagged record
		paper			: type_paper_size := paper_size_default;
		orientation		: type_paper_orientation := orientation_default;
		border_width	: type_border_width := border_width_default;
		size			: type_size;
		sectors			: type_sectors;
	end record;



	
	generic
		with package shapes is new et_geometry.shapes_2d (<>);
		with package text is new et_text.text (<>);
		
	package frames is
		use shapes.geometry;
		use shapes;
		use text;

		function paper_dimension (
		-- Returns for the given paper size, orientation and axis the correspoinding size in mm.
			paper_size	: in type_paper_size;
			orientation	: in type_paper_orientation := LANDSCAPE;
			axis		: in type_axis_2d)
			return type_distance_positive;

		-- The title block consists of lots of lines:
		type type_line is new shapes.type_line with null record;
		package pac_lines is new doubly_linked_lists (type_line);

		-- Inside the title block are placeholders for texts such as project, file, date ...
		type type_text_placeholder is new text.type_text with record
			position	: type_point; -- x/y relative to the position of the title block
			rotation	: type_rotation;
			-- CS: font, ...
		end record;

		-- These basic placeholders are required in both schematic and layout frames.
		-- The user may use them or not. In the schematic these placeholders are
		-- replaced by the module or project wide attributes:
		type type_text_placeholders is record
			company			: type_text_placeholder;
			customer		: type_text_placeholder;
			partcode		: type_text_placeholder;
			drawing_number	: type_text_placeholder;
			assembly_variant: type_text_placeholder; -- CS good idea to have it here ?
			
			project		: type_text_placeholder;
			file		: type_text_placeholder;
			revision	: type_text_placeholder;
		
			drawn_by	: type_text_placeholder;
			checked_by	: type_text_placeholder;
			approved_by	: type_text_placeholder;

			drawn_date		: type_text_placeholder;
			checked_date	: type_text_placeholder;
			approved_date	: type_text_placeholder;

			created_date	: type_text_placeholder;
			edited_date		: type_text_placeholder;
		end record;

		-- A real text with content:
		type type_text is new type_text_placeholder with record
			content		: et_text.type_text_content.bounded_string;
		end record;

		-- Real texts are stored in a simple list:
		package pac_texts is new doubly_linked_lists (type_text);

		-- This is the basic title block of a frame. On instantiation it can
		-- be extended with other properties.
		type type_title_block is tagged record
			position		: type_point; -- relative to the position of the frame
			lines			: pac_lines.list;
			texts			: pac_texts.list;
			placeholders	: type_text_placeholders;
		end record;

		-- The final drawing frame. To be extended with other properties on instantiation.
		-- NOTE: The native drawing frame has its lower left corner at position x/y 0/0. always.
		type type_frame is tagged record
			paper_size      : type_paper_size; -- the size of the paper
			sectors			: type_sectors;
			orientation		: type_paper_orientation := LANDSCAPE;
			lines           : pac_lines.list;
		end record;
		
	end frames;
	
end et_frames;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
