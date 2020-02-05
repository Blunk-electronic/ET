------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW SILK SCREEN                          --
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
with et_packages;				use et_packages;
use et_pcb_coordinates.geometry;

with et_pcb;					use et_pcb;

with et_canvas_primitive_draw_ops;

separate (et_canvas_board)

procedure draw_silk_screen (
	self    : not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context;
	face	: in type_face) is
	
	use type_silk_lines;
	use type_silk_arcs;
	use type_silk_circles;
	
	procedure query_line (c : in type_silk_lines.cursor) is begin
		cairo.set_line_width (context.cr, type_view_coordinate (element (c).width));
		
		pac_draw_package.draw_line (
			area		=> in_area,
			context		=> context,
			line		=> element (c),
			height		=> self.drawing.frame_bounding_box.height);

		cairo.stroke (context.cr);
	end query_line;

	procedure query_arc (c : in type_silk_arcs.cursor) is begin
		cairo.set_line_width (context.cr, type_view_coordinate (element (c).width));
		
		pac_draw_package.draw_arc (
			area		=> in_area,
			context		=> context,
			arc			=> element (c),
			height		=> self.drawing.frame_bounding_box.height);

		cairo.stroke (context.cr);		
	end query_arc;

	procedure query_circle (c : in type_silk_circles.cursor) is 
		use et_packages.shapes;
	begin
		case element (c).filled is
			when NO =>
				-- We draw a normal non-filled circle:
				cairo.set_line_width (context.cr, type_view_coordinate (element (c).border_width));

				pac_draw_package.draw_circle (
					area		=> in_area,
					context		=> context,
					circle		=> element (c),
					height		=> self.drawing.frame_bounding_box.height);
				
			when YES =>
				-- We draw a filled circle with a certain fill style:
				case element (c).fill_style is
					when SOLID 		=> null; -- CS
					when HATCHED 	=> null; -- CS
				end case;
		end case;

		cairo.stroke (context.cr);
	end query_circle;
	
	procedure query_itemss (
		module_name	: in type_module_name.bounded_string;
		module		: in type_module) is
	begin
		save (context.cr);
		
		-- Prepare the current transformation matrix (CTM) so that
		-- all following drawing is relative to the upper left frame corner.
		translate (
			context.cr,
			convert_x (self.drawing.frame_bounding_box.x),
			convert_y (self.drawing.frame_bounding_box.y));

		-- All outline segments will be drawn with the same color:
		cairo.set_source_rgb (context.cr, gdouble (1), gdouble (1), gdouble (1)); -- white

		case face is
			when TOP =>
				iterate (module.board.silk_screen.top.lines, query_line'access);
				iterate (module.board.silk_screen.top.arcs, query_arc'access);
				iterate (module.board.silk_screen.top.circles, query_circle'access);
				-- CS iterate (module.board.silk_screen.top.polygons, query_polygon'access);
				-- CS iterate (module.board.silk_screen.top.cutouts, query_polygon'cutout);
				-- CS iterate (module.board.silk_screen.top.placeholders, query_placeholder'access);
				-- CS iterate (module.board.silk_screen.top.texts, query_text'access);

			when BOTTOM =>
				iterate (module.board.silk_screen.bottom.lines, query_line'access);
				iterate (module.board.silk_screen.bottom.arcs, query_arc'access);
				iterate (module.board.silk_screen.bottom.circles, query_circle'access);
				-- CS see above
		end case;

		-- CS query packages
		
		restore (context.cr);
	end query_itemss;
	
begin -- draw_silk_screen
	put_line ("draw board silk screen ...");
	
	type_modules.query_element (
		position	=> self.drawing.module,
		process		=> query_itemss'access);
	
end draw_silk_screen;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
