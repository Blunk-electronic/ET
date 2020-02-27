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
	area    : type_rectangle) 
is
	x, y  : type_view_coordinate;
	dot_size : constant type_view_coordinate := 0.1; -- the size of a dot
	dot_line_width : constant type_view_coordinate := 0.1; -- the width of the lines that form the dot

	-- The grid must be aligned with the frame. So the initial offset of the grid is
	-- the position of the drawing frame. That is the position of the frame bounding box.
	-- NOTE: The frame is drawn directly in cairo, means in y-axis going downwards. 
	-- See procedure draw_frame.
	offset_x : type_view_coordinate := type_view_coordinate (self.drawing.frame_bounding_box.x);
	offset_y : type_view_coordinate := type_view_coordinate (self.drawing.frame_bounding_box.y);
	-- Later, the offset_y will get a fine adjustment according to the frame height.

	function lower_grid_coordinate (coordinate : in type_distance) 
	-- This function calculates the grid coordinate on the axis that comes
	-- before the given coordinate.
	-- Example 1: If coordinate is 215.6 and grid_size is 10, then x becomes 210.
	-- Example 2: If coordinate is 166.5 and grid_size is 5, then x becomes 165.
		return type_view_coordinate is 
	begin
		return    type_view_coordinate (gint (coordinate / self.grid_size)) -- 166.5 / 5 = 33.3 -> 33
				* type_view_coordinate (self.grid_size); -- 33 * 5 = 165
	end;

	-- This procedure calculates the addional offset in y. This is necessary because
	-- the grid must be aligned with the lower left corner of the frame. The lower left corner
	-- depends on the heigth of the frame.
	procedure fine_tune_y_offset is
		use et_frames;
		dy : type_view_coordinate;
	begin
		-- put_line ("fh " & to_string (model.frame.size.y));

		-- Calculate the next lower y-grid-coordinate that comes before the frame height.
		-- Example: If the frame is 207mm high and grid size is 10 then dy becomes 200.
		dy := type_view_coordinate (
				gint (self.drawing.frame.size.y / et_frames.type_distance (self.grid_size))) 
				* type_view_coordinate (self.grid_size);

		-- put_line ("y1 " & type_view_coordinate'image (y));

		-- Calculate the distance between the lower frame border and the 
		-- next lower y-grid-coordinate.
		-- Example: If the lower border of the frame is at 207mm and the next lower y-grid-coordinate 
		-- is 200 then the dy becomes 7.
		dy := type_view_coordinate (self.drawing.frame.size.y) - dy;

		-- put_line ("y2 " & type_view_coordinate'image (y));

		-- Add dy to the already existing offset_y so that the grid is moved by dy downwards:
		offset_y := offset_y + dy;
	end fine_tune_y_offset;
	
begin -- draw_grid
	if style.get_fill /= null_pattern then
		set_source (context.cr, style.get_fill);
		paint (context.cr);
	end if;

	if self.grid_size /= 0.0 then -- CS use module.grid
		
		new_path (context.cr);
		cairo.set_line_width (context.cr, dot_line_width);

		-- Calculate additional y offset due to the frame height:
		fine_tune_y_offset;

		-- The grid must be shifted to the right so that it is aligned
		-- with the left frame border:
		x := lower_grid_coordinate (area.x) -- the next grid point before area.x
			 - type_view_coordinate (self.grid_size) -- start at one grid point earlier
			 + offset_x;
		
		while x < type_view_coordinate (area.x + area.width) loop
			
			-- The grid must be shifted downwards so that it is aligned
			-- with the lower frame border:
			y := lower_grid_coordinate (area.y)  -- the next grid point before area.y
				 - type_view_coordinate (self.grid_size) -- start at one grid point earlier
				 + offset_y;
			
			while y < type_view_coordinate (area.y + area.height) loop

				-- draw a very small cross (so that it seems like a dot):
				cairo.move_to (context.cr, x - dot_size, y);
				cairo.line_to (context.cr, x + dot_size, y);

				cairo.move_to (context.cr, x, y - dot_size);
				cairo.line_to (context.cr, x, y + dot_size);

				-- advance to next position on y-axis
				y := y + type_view_coordinate (self.grid_size);
			end loop;

			-- advance to next position in x-axis
			x := x + type_view_coordinate (self.grid_size);
		end loop;

		style.finish_path (context.cr);
	end if;
end draw_grid;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
