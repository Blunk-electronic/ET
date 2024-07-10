------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                CANVAS                                    --
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


package body et_canvas.drawing_frame_general is

	procedure dummy_2 is begin null; end;

	
	
	-- This procedure draws the outer border of the frame:
	procedure draw_border (
		size	: in type_frame_size)
	is
		-- This is a temporarily line that is used to
		-- draw all the individual lines of the frame:
		l : pac_geometry.type_line;

		-- Get the width of the frame:
		w : constant type_distance_positive := 
			type_distance_positive (size.x);

		-- Get the height of the frame:
		h : constant type_distance_positive := 
			type_distance_positive (size.y);


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


	
end et_canvas.drawing_frame_general;
