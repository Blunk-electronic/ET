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

separate (et_canvas.schematic)

procedure draw_grid (
	self    : not null access type_view'class;
	style   : gtkada.style.drawing_style;
	context : type_draw_context;
	area    : type_model_rectangle) 
is
	x, y  : type_view_coordinate;
	width : constant type_view_coordinate := 0.1; -- the size of a dot
begin
	if style.get_fill /= null_pattern then
		set_source (context.cr, style.get_fill);
		paint (context.cr);
	end if;

	if self.grid_size /= 0.0 then
		new_path (context.cr);
		cairo.set_line_width (context.cr, 0.1);
		
		x := type_view_coordinate (gint (area.x / self.grid_size)) * type_view_coordinate (self.grid_size);

		-- The grid must be shifted to the right so that it starts at the frame x position:
		x := x + type_view_coordinate (model.frame_bounding_box.x);
		
		while x < type_view_coordinate (area.x + area.width) loop
			
			y := type_view_coordinate (gint (area.y / self.grid_size)) * type_view_coordinate (self.grid_size);

			-- The grid must be shifted downwards so that it starts at the frame y position:
			y := y + type_view_coordinate (model.frame_bounding_box.y + model.frame_height); -- CS frame height ?
			
			while y < type_view_coordinate (area.y + area.height) loop

				-- draw a very small cross (so that it seems like a dot):
				cairo.move_to (context.cr, x - width, y);
				cairo.line_to (context.cr, x + width, y);

				cairo.move_to (context.cr, x, y - width);
				cairo.line_to (context.cr, x, y + width);

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
