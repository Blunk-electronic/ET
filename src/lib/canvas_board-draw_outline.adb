------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW OUTLINE                              --
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

with et_general;				use et_general;
with et_schematic;				use et_schematic;
with et_project;				use et_project;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_packages;				--use et_packages;
use et_pcb_coordinates.geometry;

with et_pcb;					use et_pcb;

separate (canvas_board)

procedure draw_outline (
	self    : not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context) is
	
	use type_pcb_contour_lines;
	
	procedure query_line (c : in type_pcb_contour_lines.cursor) is
		use et_packages;
		use et_packages.shapes;
		boundaries : type_boundaries := shapes.boundaries (type_line (element (c)));
		bounding_box : type_rectangle := make_bounding_box (self, boundaries);
	begin
		-- We draw the segment if:
		--  - no area given or
		--  - if the bounding box of the segment intersects the given area
		if (in_area = no_rectangle
			or else intersects (in_area, bounding_box)) 
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
				convert_x (self.drawing.frame_bounding_box.x),
				convert_y (self.drawing.frame_bounding_box.y));

			cairo.set_line_width (context.cr, type_view_coordinate (0.5)); -- cS

			cairo.set_source_rgb (context.cr, gdouble (1), gdouble (1), gdouble (1)); -- white

			
			-- start point
			cairo.move_to (
				context.cr,
				convert_x (element (c).start_point.x),
				convert_and_shift_y (self, element (c).start_point.y)
				);

			-- end point
			cairo.line_to (
				context.cr,
				convert_x (element (c).end_point.x),
				convert_and_shift_y (self, element (c).end_point.y)
				);

			cairo.stroke (context.cr);
			restore (context.cr);
			
		end if;
	end query_line;
	
	procedure query_segments (
		module_name	: in type_module_name.bounded_string;
		module		: in type_module) is
	begin
		iterate (module.board.contours.lines, query_line'access);
	end query_segments;
	
begin -- draw_outline
-- 	put_line ("draw board outline ...");
	
	type_modules.query_element (
		position	=> self.drawing.module,
		process		=> query_segments'access);
	
end draw_outline;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
