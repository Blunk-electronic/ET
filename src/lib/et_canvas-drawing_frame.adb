------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          CANVAS DRAWING FRAME                            --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2024                                                       --
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

-- with ada.strings.bounded;
-- with ada.strings;
-- with ada.strings.fixed;
-- 

-- with et_canvas.text;


package body et_canvas.drawing_frame is


	
	procedure draw_frame (
		frame : in type_frame_general)
	is
		-- This is a temporarily line that is used to
		-- draw all the individual lines of the frame:
		l : pac_geometry.type_line;

		-- Get the width of the frame:
		w : constant type_distance_positive := 
			type_distance_positive (frame.size.x);

		-- Get the height of the frame:
		h : constant type_distance_positive := 
			type_distance_positive (frame.size.y);

		-- Get the position of the lower-left corner
		-- of the frame:
		p : constant pac_geometry.type_position := (
			place => (
				x => type_distance_positive (frame.position.x),
				y => type_distance_positive (frame.position.y)),
			rotation => zero_rotation);

		b : constant type_distance_positive := 
			type_distance_positive (frame.border_width);

		
		-- Draws the temporarily line. Takes into account
		-- the position of the lower-left corner of the frame:
		procedure draw_line is begin
			-- The width of 0.0 has no meaning because 
			-- the argument do_stroke is false by default
			-- (see specs of draw_line):
			draw_line (
				line		=> l,
				pos			=> p,
				width		=> 0.0);
		end draw_line;


		procedure outer_border is begin
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
		end outer_border;


		procedure inner_border is begin
			set_linewidth (linewidth_2);

			-- Assemble the lower line:
			l.start_point := (b, b);
			l.end_point := (w - b, b);
			draw_line;

			-- Assemble the right line:
			l.start_point := (w - b, b);
			l.end_point := (w - b, h - b);
			draw_line;

			-- Assemble the upper line:
			l.start_point := (w - b, h - b);
			l.end_point := (b, h - b);
			draw_line;

			-- Assemble the left line:
			l.start_point := (b, h - b);
			l.end_point := (b, b);
			draw_line;

			stroke;
		end inner_border;


		procedure sector_delimiters is

			sector_width  : constant type_distance_positive := 
				(w - 2 * b) / type_distance_positive (frame.sectors.columns);
			
			sector_height : constant type_distance_positive := 
				(h - 2 * b) / type_distance_positive (frame.sectors.rows);
			
			use et_text;

			
			procedure draw_index (
				content	: in pac_text_content.bounded_string;
				pos		: in type_vector_model) 
			is begin
				draw_text (
					content		=> content,
					size		=> type_distance_positive (font_indexes_size),
					font		=> font_indexes,
					anchor		=> pos,
					origin		=> false,
					rotation	=> 0.0,
					alignment	=> (CENTER, CENTER));
			end draw_index;
			
			x, y  	: type_distance_positive;
			xo, yo	: pac_geometry.type_distance;
			
			
		begin -- draw_sector_delimiters
			
			set_linewidth (linewidth_1);

			
			-- COLUMN DELIMITERS:
			-- The lines are drawn upwards, from bottom to top.
			for i in 1 .. frame.sectors.columns - 1 loop

				-- compute x coordinate
				x := type_distance_positive (i) * sector_width
					+ b; -- offset to the right

				-- LOWER BORDER
				
				-- draw the line bottom-up:
				-- lower end:
				l.start_point := type_vector_model (set (
					x => x,
					y => zero));

				-- upper end:
				l.end_point := type_vector_model (set (
					x => x,
					y => b));

				draw_line;


				
				-- UPPER BORDER
				-- draw the line bottom-up:
				-- lower end:
				l.start_point := type_vector_model (set (
					x => x,
					y => h - b));

				-- upper end:
				l.end_point := type_vector_model (set (
					x => x,
					y => h));
				
				draw_line;
			end loop;

			
			-- ROW DELIMITERS:
			-- The lines are drawn from the left to the right.
			for i in 1 .. frame.sectors.rows - 1 loop

				-- compute y coordinate
				y := type_distance_positive (i) * sector_height
					+ b; -- offset upwards

				-- LEFT BORDER
				
				-- draw the line from the left to the right:
				-- left end:
				l.start_point := type_vector_model (set (
					x => zero,
					y => y));

				-- right end:
				l.end_point := type_vector_model (set (
					x => b,
					y => y));

				draw_line;
				
				-- RIGHT BORDER
				-- draw the line from the left to the right:
				-- left end:
				l.start_point := type_vector_model (set (
					x => w - b,
					y => y));

				-- right end:
				l.end_point := type_vector_model (set (
					x => w,
					y => y));
				
				draw_line;
			end loop;

			stroke;

			
			
			-- COLUMN INDEX:
			y := b / 2;

			-- x requires offset to the right
			xo := b - (sector_width / 2);
			
			for i in 1 .. frame.sectors.columns loop

				-- compute x coordinate
				x := type_distance_positive (i) * sector_width + xo;
				
				-- draw index in lower border
				draw_index (
					content	=> to_content (to_string (i)),
					pos		=> type_vector_model (set (x, y)));

				-- draw index in upper border
				draw_index (
					content	=> to_content (to_string (i)),
					pos		=> type_vector_model (set (
								x => x,
								y => h - y)));
				
			end loop;

			
			-- ROW INDEX:
			x := b / 2;

			-- y requires offset upwards
			yo := b - (sector_height / 2);
			
			for i in 1 .. frame.sectors.rows loop

				-- compute y coordinate
				y := type_distance_positive (i) * sector_height + yo;
				
				-- draw index in left border
				draw_index (
					content	=> to_content (to_string (i)),
					pos		=> type_vector_model (set (x, y)));

				-- draw index in right border
				draw_index (
					content	=> to_content (to_string (i)),
					pos		=> type_vector_model (set (
								x => w - x,
								y => y)));
				
			end loop;
			
		end sector_delimiters;

		
		
	begin
		outer_border;

		inner_border;

		sector_delimiters;
	end draw_frame;


	
end et_canvas.drawing_frame;
