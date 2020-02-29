------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              DRAW GRID                                   --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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
with cairo;						use cairo;
with et_frames;

separate (et_canvas_schematic)

procedure draw_grid (
	self    : not null access type_view;
	style   : gtkada.style.drawing_style;
	context : type_draw_context;
	area    : type_rectangle) -- the area of the drawing to be displayed
is
	x, y  : type_view_coordinate;
	
	dot_size : constant type_view_coordinate := type_distance'small; -- the size of a dot
	dot_line_width : constant type_view_coordinate := type_distance'small; -- the width of the lines that form the dot

	-- The grid must be aligned with the frame.
	-- NOTE: Frame and grid are drawn directly in cairo, means with y-axis going downwards. 
	-- See procedure draw_frame.
	start_y : type_view_coordinate;

	grid : geometry.type_grid := et_project.type_modules.element (self.drawing.module).grid;
	
begin -- draw_grid
	if style.get_fill /= null_pattern then
		set_source (context.cr, style.get_fill);
		paint (context.cr);
	end if;
		
	new_path (context.cr);
	cairo.set_line_width (context.cr, dot_line_width);

	-- The start point on the x-axis is aligned with the left frame border:
	x := type_view_coordinate (self.drawing.frame_bounding_box.x) - lower_grid_coordinate (area.width, grid.x);
	-- CS: Currently the start point is at -area.width. Means very far on the left outside the given area.
	-- On drawing the grid this consumes useless computing power.

	-- The start point on the y-axis is aligned with the lower frame border (bounding box.y + frame height).
	start_y := type_view_coordinate (self.drawing.frame_bounding_box.y) 
			   + type_view_coordinate (self.drawing.frame.size.y)
			   + lower_grid_coordinate (area.height, grid.y);
	-- CS: Currently the start point is at -area.height. Means very far below the given area.
	-- On drawing the grid this consumes useless computing power.

	-- We draw the grid in x-axis from left to right:
	while x < type_view_coordinate (area.x + area.width) loop

		-- We draw the grid in y-axis upwards:
		y := start_y;
		
		while y > type_view_coordinate (area.y) loop

			-- draw a very small cross (so that it seems like a dot):
			cairo.move_to (context.cr, x - dot_size, y);
			cairo.line_to (context.cr, x + dot_size, y);

			cairo.move_to (context.cr, x, y - dot_size);
			cairo.line_to (context.cr, x, y + dot_size);

			-- advance to next upper row on y-axis
			y := y - type_view_coordinate (grid.y);
		end loop;

		-- advance to next column on the right on x-axis
		x := x + type_view_coordinate (grid.x);
	end loop;

	style.finish_path (context.cr);

end draw_grid;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
