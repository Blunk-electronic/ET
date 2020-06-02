------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           DRAW GRID IN BOARD                             --
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

separate (et_canvas_board)

procedure draw_grid (
	self    : not null access type_view;
	context : type_draw_context;
	area    : type_rectangle)  -- the area of the drawing to be displayed
is
	-- IMPORTANT !
	-- THE GRID MUST BE ALIGNED WITH THE BOARD ORIGIN.
	
	-- NOTE: The grid is drawn directly in cairo, means with y-axis going downwards. 
	-- See procedure draw_frame.
	start_x, start_y : type_view_coordinate;

	use et_project.type_modules;
	use et_canvas_schematic;
	used_grid : constant type_grid := element (current_active_module).board.grid;
begin
	-- The start point on the x-axis is aligned with the left frame border
	-- and shifted right by the board origin x position.
	start_x := type_view_coordinate (self.frame_bounding_box.x) 
			   - lower_grid_coordinate (area.width, used_grid.x)
			   + type_view_coordinate (x (self.board_origin));

	-- CS: Currently the start point is very far on the left outside the given area.
	-- On drawing the grid this circumstance may waste computing time.


	
	-- The start point on the y-axis is aligned with the lower frame border 
	-- (bounding box.y + frame height)
	-- and shifted up by the board origin y position.
	start_y := type_view_coordinate (self.frame_bounding_box.y) 
			   + type_view_coordinate (self.frame_height)
			   + lower_grid_coordinate (area.height, used_grid.y)
			   - type_view_coordinate (y (self.board_origin));
	
	-- CS: Currently the start point is very far below the given area.
	-- On drawing the grid this circumstance may waste computing time.

	pac_canvas.draw_grid (context, area, used_grid, start_x, start_y,
						  grid -- color
						 );

end draw_grid;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
