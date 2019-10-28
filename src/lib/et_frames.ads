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

with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;


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

	
	-- $ET_FRAMES/drawing_frame_version_1.frm
	frame_template_fil_name_length_max : constant positive := 300;
	package type_frame_template_name is new generic_bounded_length (frame_template_fil_name_length_max);

	frame_template_name_dummy : constant type_frame_template_name.bounded_string := 
		type_frame_template_name.to_bounded_string ("dummy_frame");
	
	function to_string (name : in type_frame_template_name.bounded_string) return string;
	function to_template_name (name : in string) return type_frame_template_name.bounded_string;


	type type_title_block_text_meaning is ( 
		PROJECT, TITLE, 
        DRAWN_BY, CHECKED_BY, APPROVED_BY, 
        DRAWN_DATE, CHECKED_DATE, APPROVED_DATE,
        COMPANY,
		REVISION, MISC);
	
	
	generic
		with package shapes is new et_geometry.shapes_2d (<>);
		with package text is new et_text.text (<>);
		
	package frames is
		use shapes.geometry;
		use shapes;
		use text;

		-- PAPER SIZES
		-- As default we assume landscape format for all sheets.
		paper_size_A3_x : constant type_distance_positive := 420.0;
		paper_size_A3_y : constant type_distance_positive := 297.0;
		
		paper_size_A4_x : constant type_distance_positive := 297.0;
		paper_size_A4_y : constant type_distance_positive := 210.0;

		function paper_dimension (
		-- Returns for the given paper size, orientation and axis the correspoinding size in mm.
			paper_size	: in type_paper_size;
			orientation	: in type_paper_orientation := LANDSCAPE;
			axis		: in type_axis_2d)
			return type_distance_positive;

		
		type type_title_block_text is new text.type_text with record
			meaning			: type_title_block_text_meaning;
			position		: type_point; -- relative to the position of the title block
			content			: et_text.type_text_content.bounded_string;
			rotation		: type_rotation;
			-- CS: font, ...
		end record;

		package pac_title_block_texts is new doubly_linked_lists (type_title_block_text);
		
		type type_line is new shapes.type_line with null record;
		package pac_lines is new doubly_linked_lists (type_line);

		type type_title_block is record
			position	: type_point; -- relative to the position of the frame
			lines		: pac_lines.list;
			texts		: pac_title_block_texts.list;
		end record;

		-- the final drawing frame
		-- NOTE: The native drawing frame has its lower left corner at position x/y 0/0. always.
		type type_frame is tagged record
			paper_size      : type_paper_size; -- the size of the paper
			orientation		: type_paper_orientation := LANDSCAPE;
			lines           : pac_lines.list;
			title_block		: type_title_block;
		end record;

		
	end frames;
	
	
end et_frames;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
