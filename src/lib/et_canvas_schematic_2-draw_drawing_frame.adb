------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         SCHEMATIC DRAW FRAME                             --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your editor to 4.

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
with et_text;
with et_schematic_ops.sheets;
-- with et_meta;

separate (et_canvas_schematic_2)

procedure draw_drawing_frame is

	use et_colors.schematic;
	use et_frames;
	use pac_drawing_frame;
	

	-- Get the frames of the schematic:
	frames : type_frames_schematic := element (active_module).frames;
	-- CS use query_element instead


	frame_general : type_frame_general := type_frame_general (frames.frame);
		

	title_block : type_title_block_schematic renames frames.frame.title_block_schematic;

	

	-- The position of the title block.
	-- The block has no rotation. The x/y position will be
	-- extracted later:
	title_block_position : pac_geometry.type_position := 
		(rotation => 0.0, others => <>);
	
	
	
	-- This procedure draws the lines of the title block:
	procedure draw_title_block_lines is

		use pac_lines;
	
		procedure query_line (c : in pac_lines.cursor) is
			l1 : et_frames.type_line renames element (c);
			l2 : pac_geometry.type_line;
		begin
			l2 := to_line (l1);

			-- The width of 0.0 has no meaning because 
			-- the argument do_stroke is false by default
			-- (see specs of draw_line):
			draw_line (l2, title_block_position, 0.0);
		end query_line;

		
	begin
		set_linewidth (linewidth_1);
		
		iterate (
			container	=> frames.frame.title_block_schematic.lines, 
			process		=> query_line'access);
			
		stroke;
	end draw_title_block_lines;



	
	
	procedure draw_additional_placeholders is

		-- get placeholders:
		phs : type_placeholders_schematic renames
			frames.frame.title_block_schematic.placeholders_additional;
		
		use et_text;
		use pac_draw_text;

		-- A temporarily storage place for the
		-- position of a placeholder:
		pos : type_vector_model;		

		
		procedure draw_sheet_description is
			use et_project;
			use et_schematic_ops.sheets;

			-- Get the description of the current active sheet:
			des : constant type_schematic_description := 
					get_sheet_description (active_module, active_sheet);
		begin
			-- category (development, product, routing)
			pos := to_vector (phs.sheet_category.position);
			
			draw_text (
				content		=> to_content (to_string (des.category)),
				size		=> to_distance (phs.sheet_category.size),
				font		=> font_placeholders,
				anchor		=> add (pos, title_block_position.place),
				origin		=> false,
				rotation	=> 0.0,
				alignment	=> (LEFT, BOTTOM));

			
			-- description			
			pos := to_vector (phs.sheet_description.position);
			
			draw_text (
				content		=> to_content (to_string (des.content)),
				size		=> to_distance (phs.sheet_description.size),
				font		=> font_placeholders,
				anchor		=> add (pos, title_block_position.place),
				origin		=> false,
				rotation	=> 0.0,
				alignment	=> (LEFT, BOTTOM));
						
		end draw_sheet_description;



		procedure draw_sheet_number is begin
			pos := to_vector (phs.sheet_number.position);
			
			draw_text (
				content		=> to_content (to_sheet (active_sheet)), -- CS complete with "/of total"
				size		=> to_distance (phs.sheet_number.size),
				font		=> font_placeholders,
				anchor		=> add (pos, title_block_position.place),
				origin		=> false,
				rotation	=> 0.0,
				alignment	=> (LEFT, BOTTOM));
		end draw_sheet_number;
		
		
	begin
		draw_sheet_number;
		draw_sheet_description;		
	end draw_additional_placeholders;


	
begin
	-- put_line ("draw_drawing_frame (schematic)");

	set_color_frame;

	-- outer border:
	draw_frame (frames.frame);

	
	-- TITLE BLOCK

	-- Extract the position of the title block:
	title_block_position.place := to_vector (title_block.position);

	draw_title_block_lines;	

	draw_common_placeholders (
		placeholders			=> title_block.placeholders_common,
		title_block_position	=> title_block_position);


	draw_additional_placeholders;

	draw_static_texts (
		texts					=> title_block.static_texts,
		title_block_position	=> title_block_position);
	
		-- draw common placeholders and other texts
		-- draw_texts (
		-- 	ph_common	=> self.get_frame.title_block_schematic.placeholders,
		-- 	ph_basic	=> type_placeholders_basic (self.get_frame.title_block_schematic.additional_placeholders),
		-- 	texts		=> self.get_frame.title_block_schematic.texts,
		-- 	meta		=> et_meta.type_basic (element (active_module).meta.board),
		-- 	tb_pos		=> title_block_position);


	-- draw_basic_meta_information (
	-- 	meta					=> get_meta_information (active_module),
	-- 	title_block_position	=> title_block_position);
	
	
end draw_drawing_frame;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
