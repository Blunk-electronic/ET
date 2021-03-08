------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          CANVAS DRAW FRAME                               --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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

with et_general;
with et_project.modules;
use et_project.modules.pac_generic_modules;

with et_meta;
with et_canvas_schematic;

package body et_canvas_draw_frame is

package body pac_draw_frame is

	type type_line is new draw_ops.pac_shapes.type_line with null record;
	line : type_line;

	procedure draw_line is begin
		draw_ops.draw_line (
			area		=> in_area,
			context		=> context,
			line		=> line,
			width		=> type_distance_positive (line_width_thin),
			height		=> type_distance_positive (frame_size.y));
	end draw_line;

	procedure draw_border is begin
	-- OUTER BORDER
		-- left line from bottom to top
		line.start_point := type_point (set (0.0, 0.0));
		
		line.end_point := type_point (set (
			x => 0.0,
			y => type_distance_positive (frame_size.y)));
		
		draw_line;

		-- right line from bottom to top
		line.start_point := type_point (set (
			x => type_distance_positive (frame_size.x),
			y => 0.0));
		
		line.end_point := type_point (set (
			x => type_distance_positive (frame_size.x),
			y => type_distance_positive (frame_size.y)));
		
		draw_line;

		-- lower line from left to right
		line.start_point := type_point (set (0.0, 0.0));
		line.end_point := type_point (set (
			x => type_distance_positive (frame_size.x),
			y => 0.0));
		
		draw_line;

		-- upper line from left to right
		line.start_point := type_point (set (
			x => 0.0,
			y => type_distance_positive (frame_size.y)));
		
		line.end_point := type_point (set (
			x => type_distance_positive (frame_size.x),
			y => type_distance_positive (frame_size.y)));
		
		draw_line;

		
	-- INNER BORDER
		-- left line from bottom to top
		line.start_point := type_point (set (
			x => type_distance_positive (border_width),
			y => type_distance_positive (border_width)));
		
		line.end_point := type_point (set (
			x => type_distance_positive (border_width),
			y => type_distance_positive (frame_size.y - border_width)));
		
		draw_line;

		-- right line from bottom to top
		line.start_point := type_point (set (
			x => type_distance_positive (frame_size.x - border_width),
			y => type_distance_positive (border_width)));
		
		line.end_point := type_point (set (
			x => type_distance_positive (frame_size.x - border_width),
			y => type_distance_positive (frame_size.y - border_width)));
		
		draw_line;

		-- lower line from left to right
		line.start_point := type_point (set (
			x => type_distance_positive (border_width),
			y => type_distance_positive (border_width)));
		
		line.end_point := type_point (set (
			x => type_distance_positive (frame_size.x - border_width),
			y => type_distance_positive (border_width)));
		
		draw_line;

		-- upper line from left to right
		line.start_point := type_point (set (
			x => type_distance_positive (border_width),
			y => type_distance_positive (frame_size.y - border_width)));
		
		line.end_point := type_point (set (
			x => type_distance_positive (frame_size.x - border_width),
			y => type_distance_positive (frame_size.y - border_width)));
		
		draw_line;

	end draw_border;

	procedure draw_sector_delimiters is
		use draw_ops.pac_shapes;
		
		sector_width  : constant et_frames.type_distance := 
			(frame_size.x - 2 * border_width) / et_frames.type_distance (sectors.columns);
		
		sector_height : constant et_frames.type_distance := 
			(frame_size.y - 2 * border_width) / et_frames.type_distance (sectors.rows);
		
		use et_text;
		procedure draw_index (
			content	: in type_text_content.bounded_string;
			pos		: in pac_geometry.type_point) is
		begin
			draw_ops.draw_text (
				area		=> in_area,
				context		=> context,
				content		=> content,
				size		=> pac_shapes.pac_geometry.type_distance_positive (font_indexes_size),
				font		=> font_indexes,
				position	=> pos,
				origin		=> false,
				rotation	=> pac_geometry.zero_rotation,
				alignment	=> (CENTER, CENTER),
				height		=> type_distance_positive (frame_size.y));
		end draw_index;
		
		x, y  	: type_distance_positive;
		xo, yo	: pac_geometry.type_distance;
		
	begin -- draw_sector_delimiters
		-- COLUMN DELIMITERS:
		-- The lines are drawn upwards, from bottom to top.
		for i in 1 .. sectors.columns - 1 loop

			-- compute x coordinate
			x := type_distance_positive (et_frames.type_distance (i) * sector_width)
				 + type_distance_positive (border_width); -- offset to the right

			-- LOWER BORDER
			
			-- draw the line bottom-up:
			-- lower end:
			line.start_point := type_point (set (
				x => x,
				y => zero));

			-- upper end:
			line.end_point := type_point (set (
				x => x,
				y => type_distance_positive (border_width)));

			draw_line;


			
			-- UPPER BORDER
			-- draw the line bottom-up:
			-- lower end:
			line.start_point := type_point (set (
				x => x,
				y => type_distance_positive (frame_size.y - border_width)));

			-- upper end:
			line.end_point := type_point (set (
				x => x,
				y => type_distance_positive (frame_size.y)));
			
			draw_line;
		end loop;

		-- ROW DELIMITERS:
		-- The lines are drawn from the left to the right.
		for i in 1 .. sectors.rows - 1 loop

			-- compute y coordinate
			y := type_distance_positive (et_frames.type_distance (i) * sector_height)
				 + type_distance_positive (border_width); -- offset upwards

			-- LEFT BORDER
			
			-- draw the line from the left to the right:
			-- left end:
			line.start_point := type_point (set (
				x => zero,
				y => y));

			-- right end:
			line.end_point := type_point (set (
				x => type_distance_positive (border_width),
				y => y));

			draw_line;
			
			-- RIGHT BORDER
			-- draw the line from the left to the right:
			-- left end:
			line.start_point := type_point (set (
				x => type_distance_positive (frame_size.x - border_width),
				y => y));

			-- right end:
			line.end_point := type_point (set (
				x => type_distance_positive (frame_size.x),
				y => y));
			
			draw_line;
		end loop;

		-- COLUMN INDEX
		y := type_distance_positive (border_width / 2);

		-- x requires offset to the right
		xo := type_distance_positive (border_width) - type_distance_positive (sector_width / 2);
		
		for i in 1 .. sectors.columns loop

			-- compute x coordinate
			x := type_distance_positive (et_frames.type_distance (i) * sector_width) + xo;
			
			-- draw index in lower border
			draw_index (
				content	=> to_content (to_string (i)),
				pos		=> type_point (set (x, y)));

			-- draw index in upper border
			draw_index (
				content	=> to_content (to_string (i)),
				pos		=> type_point (set (
							x => x,
							y => type_distance_positive (frame_size.y) - y)));
			
		end loop;

		-- ROW INDEX
		x := type_distance_positive (border_width / 2);

		-- y requires offset upwards
		yo := type_distance_positive (border_width) - type_distance_positive (sector_height / 2);
		
		for i in 1 .. sectors.rows loop

			-- compute y coordinate
			y := type_distance_positive (et_frames.type_distance (i) * sector_height) + yo;
			
			-- draw index in left border
			draw_index (
				content	=> to_content (to_string (i)),
				pos		=> type_point (set (x, y)));

			-- draw index in right border
			draw_index (
				content	=> to_content (to_string (i)),
				pos		=> type_point (set (
							x => type_distance_positive (frame_size.x) - x,
							y => y)));
			
		end loop;
		
	end draw_sector_delimiters;
				
	-- Draw a line of the title block.
	procedure query_line (cursor : in pac_lines.cursor) is begin

		line.start_point := type_point (set (
			x => type_distance_positive (element (cursor).start_point.x + title_block.position.x),
			y => type_distance_positive (element (cursor).start_point.y + title_block.position.y)));

		line.end_point := type_point (set (
			x => type_distance_positive (element (cursor).end_point.x + title_block.position.x),
			y => type_distance_positive (element (cursor).end_point.y + title_block.position.y)));

		draw_line;
	end query_line;

	procedure draw_text (
		content	: in type_text_content.bounded_string;
		size	: in type_text_size;
		font	: in type_font;
		pos		: in et_frames.type_position) is

		-- The given position is given in frame coordinates and must be 
		-- converted to schematic coordinates and shifted by the position
		-- of the title block.
		ps : constant pac_shapes.pac_geometry.type_point := type_point (set (
				x => type_distance_positive (pos.x + title_block.position.x),
				y => type_distance_positive (pos.y + title_block.position.y)));

		use pac_shapes;
	begin
		draw_ops.draw_text (
			area		=> in_area,
			context		=> context,
			content		=> content,
			size		=> pac_geometry.type_distance_positive (size),
			font		=> font,
			position	=> ps,
			origin		=> true,
			rotation	=> zero_rotation,
			alignment	=> (LEFT, BOTTOM),
			height		=> type_distance_positive (frame_size.y));
	end draw_text;
	
	procedure draw_texts is

		use et_general;

		procedure draw_other_texts is
			use pac_texts;

			procedure query_text (cursor : in pac_texts.cursor) is begin
				draw_text (
					content	=> element (cursor).content,
					size	=> element (cursor).size,
					font	=> font_texts,
					pos		=> element (cursor).position);
	
			end query_text;
		
		begin -- draw_other_texts
			iterate (title_block.texts, query_text'access);
		end draw_other_texts;

		use et_project;
		use et_meta;
		use et_canvas_schematic;
		
	begin -- draw_texts
	-- COMMON PLACEHOLDERS
		-- project name:
		draw_text (
			content	=> to_content (to_string (current_active_project)), -- blood_sample_analyzer
			size	=> title_block.placeholders.project_name.size,
			font	=> font_placeholders,
			pos		=> title_block.placeholders.project_name.position);
		
		-- module file name:
		draw_text (
			content	=> to_content (to_string (key (current_active_module))), -- motor_driver
			size	=> title_block.placeholders.module_file_name.size,
			font	=> font_placeholders,
			pos		=> title_block.placeholders.module_file_name.position);

		-- active assembly variant:
		draw_text (
			content	=> to_content (to_variant (element (current_active_module).active_variant)), -- low_cost
			size	=> title_block.placeholders.active_assembly_variant.size,
			font	=> font_placeholders,
			pos		=> title_block.placeholders.active_assembly_variant.position);

	-- BASIC PLACEHOLDERS
		-- company
		draw_text (
			content	=> to_content (to_string (meta.company)), -- BEL
			size	=> placeholders.company.size,
			font	=> font_placeholders,
			pos		=> placeholders.company.position);

		-- customer
		draw_text (
			content	=> to_content (to_string (meta.customer)), -- medlab
			size	=> placeholders.customer.size,
			font	=> font_placeholders,
			pos		=> placeholders.customer.position);

		-- partcode
		draw_text (
			content	=> to_content (to_string (meta.partcode)), -- TR4452
			size	=> placeholders.partcode.size,
			font	=> font_placeholders,
			pos		=> placeholders.partcode.position);

		-- drawing number
		draw_text (
			content	=> to_content (to_string (meta.drawing_number)), -- NCC1701
			size	=> placeholders.drawing_number.size,
			font	=> font_placeholders,
			pos		=> placeholders.drawing_number.position);

		-- revision
		draw_text (
			content	=> to_content (to_string (meta.revision)), -- V2.0
			size	=> placeholders.revision.size,
			font	=> font_placeholders,
			pos		=> placeholders.revision.position);

		-- drawn by
		draw_text (
			content	=> to_content (to_string (meta.drawn_by)), -- Dieter Krause
			size	=> placeholders.drawn_by.size,
			font	=> font_placeholders,
			pos		=> placeholders.drawn_by.position);

		-- checked by
		draw_text (
			content	=> to_content (to_string (meta.checked_by)), -- John Carpenter
			size	=> placeholders.checked_by.size,
			font	=> font_placeholders,
			pos		=> placeholders.checked_by.position);

		-- approved by
		draw_text (
			content	=> to_content (to_string (meta.approved_by)), -- Wasily Mishin
			size	=> placeholders.approved_by.size,
			font	=> font_placeholders,
			pos		=> placeholders.approved_by.position);

		-- drawn date
		draw_text (
			content	=> to_content (to_string (meta.drawn_date)), -- 2010-04-23
			size	=> placeholders.drawn_date.size,
			font	=> font_placeholders,
			pos		=> placeholders.drawn_date.position);

		-- checked date
		draw_text (
			content	=> to_content (to_string (meta.checked_date)), -- 2010-04-23
			size	=> placeholders.checked_date.size,
			font	=> font_placeholders,
			pos		=> placeholders.checked_date.position);

		-- approved date
		draw_text (
			content	=> to_content (to_string (meta.approved_date)), -- 2010-04-23
			size	=> placeholders.approved_date.size,
			font	=> font_placeholders,
			pos		=> placeholders.approved_date.position);
		
			
	-- OTHER TEXTS
		draw_other_texts;

	end draw_texts;

end pac_draw_frame;

end et_canvas_draw_frame;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
