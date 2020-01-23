------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              DRAW FRAME                                  --
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
with pango.layout;				use pango.layout;

with et_coordinates;			use et_coordinates;
use et_coordinates.geometry;

separate (et_canvas.schematic)

procedure draw_frame (
	model	: not null access type_model;
	in_area	: in type_model_rectangle := no_rectangle;
	context : in type_draw_context) is

	use et_frames;
	use pac_lines;

	-- Draw the line of the title block. The line is offset by the position of the
	-- title block. The y-cooordinate is converted to the y-axis going downwards.
	procedure draw_line (cursor : in pac_lines.cursor) is begin

		-- start point
		cairo.move_to 
			(
			context.cr,

			-- x position
			convert_x (et_coordinates.type_distance
				(
				element (cursor).start_point.x 
				+ model.title_block_position.x -- x position of title block
				)),
				
			-- y position
			convert_and_shift_y (model, et_coordinates.type_distance 
				(
				element (cursor).start_point.y 
				+ model.title_block_position.y -- y position of title block
				))
			);

			
		-- end point
		cairo.line_to 
			(
			context.cr,

			-- x position	
			convert_x (et_coordinates.type_distance
				(
				element (cursor).end_point.x 
				+ model.title_block_position.x -- x position of title block
				)),

			-- y position
			convert_and_shift_y (model, et_coordinates.type_distance 
				(
				element (cursor).end_point.y 
				+ model.title_block_position.y -- y position of title block
				))
			);
	end;

begin
--		put_line ("draw frame ...");

	if (in_area = no_rectangle)
		or else intersects (in_area, model.frame_bounding_box) 
	then
		-- CS test size 
-- 			if not size_above_threshold (self, context.view) then
-- 				return;
-- 			end if;
		
		save (context.cr);

		-- Prepare the current transformation matrix (CTM) so that
		-- all following drawing is relative to the upper left frame corner.
		translate (
			context.cr,
			convert_x (model.frame_bounding_box.x),
			convert_y (model.frame_bounding_box.y));

		cairo.set_line_width (context.cr, 1.0);

		cairo.set_source_rgb (context.cr, gdouble (1), gdouble (0), gdouble (0)); -- red

		-- FRAME BORDER
		-- draw the outer border
		cairo.rectangle (
			context.cr,
			convert_x (et_coordinates.type_distance (0.0)),
			convert_y (et_coordinates.type_distance (0.0)),
			convert_x (et_coordinates.type_distance (model.frame.size.x)),
			convert_y (et_coordinates.type_distance (model.frame.size.y)));

		-- draw the inner border
		cairo.rectangle (
			context.cr,
			convert_x (et_coordinates.type_distance (model.frame.border_width)),
			convert_y (et_coordinates.type_distance (model.frame.border_width)),
			convert_x (et_coordinates.type_distance (model.frame.size.x - 2 * model.frame.border_width)),
			convert_y (et_coordinates.type_distance (model.frame.size.y - 2 * model.frame.border_width)));

		-- TITLE BLOCK
		-- lines
		iterate (model.frame.title_block_schematic.lines, draw_line'access);

		
		-- CS draw the sector delimiters

		-- CS draw the sector rows and columns

		-- CS texts according to current model.sheet
		
		cairo.stroke (context.cr);
		
		restore (context.cr);
	end if;
	
end draw_frame;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
