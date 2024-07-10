------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         SCHEMATIC DRAW FRAME                             --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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
--                                                                          --
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
-- with et_text;
-- with et_meta;

separate (et_canvas_schematic_2)

procedure draw_drawing_frame is
	use et_frames;

	-- Get the frames of the schematic:
	f : type_frames_schematic := element (current_active_module).frames;
	-- CS use query_element instead

	-- This is a temporarily line that is used to
	-- draw all the individual lines of the frame:
	l : pac_geometry_2.type_line;


	-- This procedure draws the outer border of the frame:
	procedure draw_border is
		-- Get the width of the frame:
		w : constant type_distance_positive := 
			type_distance_positive (f.frame.size.x);

		-- Get the height of the frame:
		h : constant type_distance_positive := 
			type_distance_positive (f.frame.size.y);


		-- Draws the temporarily line. Assumes that the 
		-- lower-left corner of the frame is at (0;0) as it
		-- is custom for schematic frames:
		procedure draw_line is begin
			-- The width of 0.0 has no meaning because 
			-- the argument do_stroke is false by default
			-- (see specs of draw_line):
			draw_line (
				line		=> l,
				pos			=> origin_zero_rotation, -- lower-left corner
				width		=> 0.0);
		end draw_line;
		
	begin
		set_linewidth (linewidth_2);

		-- Assemble the lower line:
		l.start_point := (0.0, 0.0);
		l.end_point := (w, 0.0);
		draw_line;

		-- Assemble the right line:
		l.start_point := (w, 0.0);
		l.end_point := (w, h);
		draw_line;

		-- Assemble the upper line:
		l.start_point := (w, h);
		l.end_point := (0.0, h);
		draw_line;

		-- Assemble the left line:
		l.start_point := (0.0, h);
		l.end_point := (0.0, 0.0);
		draw_line;

		stroke;
	end draw_border;

	
-- 	procedure draw_additional_placeholders is
-- 		
-- 		-- get placeholders:
-- 		phs : constant type_placeholders_schematic := 
-- 			self.get_frame.title_block_schematic.additional_placeholders;
-- 
-- 		use et_text;
-- 
-- 		
-- 		procedure draw_sheet_description is
-- 			use et_project;
-- 
-- 			-- Get the description of the current active sheet:
-- 			des : constant type_schematic_description := 
-- 					sheet_description (current_active_module, current_active_sheet);
-- 		begin
-- 			-- category (development, product, routing)
-- 			draw_text (
-- 				content	=> to_content (to_string (des.category)),
-- 				size	=> phs.category.size,
-- 				font	=> font_placeholders,
-- 				pos		=> phs.category.position,
-- 				tb_pos	=> title_block_position);
-- 
-- 			-- description
-- 			draw_text (
-- 				content	=> to_content (to_string (des.content)),
-- 				size	=> phs.description.size,
-- 				font	=> font_placeholders,
-- 				pos		=> phs.description.position,
-- 				tb_pos	=> title_block_position);
-- 						
-- 		end draw_sheet_description;
-- 
-- 		
-- 	begin -- draw_additional_placeholders
-- 		
-- 		-- sheet number n of m
-- 		draw_text (
-- 			content	=> to_content (to_sheet (current_active_sheet)), -- CS complete with "/of total"
-- 			size	=> phs.sheet_number.size,
-- 			font	=> font_placeholders,
-- 			pos		=> phs.sheet_number.position,
-- 			tb_pos	=> title_block_position);
-- 
-- 
-- 		draw_sheet_description;
-- 		
-- 	end draw_additional_placeholders;
-- 
	
begin
	put_line ("draw_drawing_frame (schematic)");

	set_color_frame;

	-- outer border:
	draw_border;

		
		-- TITLE BLOCK
		-- lines
		--iterate (self.get_frame.title_block_schematic.lines, query_line'access);
		--cairo.stroke (context.cr);

		-- draw_title_block_lines (
		-- 	lines		=> self.get_frame.title_block_schematic.lines,
		-- 	tb_pos		=> title_block_position);

		
		-- draw common placeholders and other texts
		-- draw_texts (
		-- 	ph_common	=> self.get_frame.title_block_schematic.placeholders,
		-- 	ph_basic	=> type_placeholders_basic (self.get_frame.title_block_schematic.additional_placeholders),
		-- 	texts		=> self.get_frame.title_block_schematic.texts,
		-- 	meta		=> et_meta.type_basic (element (current_active_module).meta.board),
		-- 	tb_pos		=> title_block_position);
  -- 

		-- draw_additional_placeholders;
		
		-- draw the sector delimiters
		-- draw_sector_delimiters (
		-- 	sectors			=> self.get_frame.sectors,
		-- 	frame_size		=> frame_size,
		-- 	border_width	=> self.get_frame.border_width);

	
end draw_drawing_frame;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
