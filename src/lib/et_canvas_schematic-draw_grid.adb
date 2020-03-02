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
with et_frames;

separate (et_canvas_schematic)

procedure draw_grid (
	self    : not null access type_view;
	context : type_draw_context;
	area    : type_rectangle) -- the area of the drawing to be displayed
is
	-- The grid must be aligned with the frame.
	-- NOTE: Frame and grid are drawn directly in cairo, means with y-axis going downwards. 
	-- See procedure draw_frame.
	start_x, start_y : type_view_coordinate;
	
begin
	-- The start point on the x-axis is aligned with the left frame border:
	start_x := type_view_coordinate (self.drawing.frame_bounding_box.x) 
			   - lower_grid_coordinate (area.width, self.drawing.grid.x);
	-- CS: Currently the start point is at -area.width. Means very far on the left outside the given area.
	-- On drawing the grid this consumes useless computing power.

	-- The start point on the y-axis is aligned with the lower frame border (bounding box.y + frame height).
	start_y := type_view_coordinate (self.drawing.frame_bounding_box.y) 
			   + type_view_coordinate (self.drawing.frame.size.y)
			   + lower_grid_coordinate (area.height, self.drawing.grid.y);
	-- CS: Currently the start point is at -area.height. Means very far below the given area.
	-- On drawing the grid this consumes useless computing power.

	pac_canvas.draw_grid (context, area, self.drawing.grid, start_x, start_y);
	
end draw_grid;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
