------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      BOARD DRAW SOLDER STOP MASK                         --
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
with et_packages;				use et_packages;
with et_pcb;					use et_pcb;

with et_canvas_primitive_draw_ops;

separate (et_canvas_board)

procedure draw_stop (
	self    : not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context;
	face	: in type_face) is
	
	use type_stop_lines;
	use type_stop_arcs;
	use type_stop_circles;
	use type_stop_polygons;
	use pac_stop_cutouts;
	
	procedure query_line (c : in type_stop_lines.cursor) is begin
		cairo.set_line_width (context.cr, type_view_coordinate (element (c).width));
		
		pac_draw_package.draw_line (
			area		=> in_area,
			context		=> context,
			line		=> element (c),
			height		=> self.frame_height);

		cairo.stroke (context.cr);
	end query_line;

	procedure query_arc (c : in type_stop_arcs.cursor) is begin
		cairo.set_line_width (context.cr, type_view_coordinate (element (c).width));
		
		pac_draw_package.draw_arc (
			area		=> in_area,
			context		=> context,
			arc			=> element (c),
			height		=> self.frame_height);

		cairo.stroke (context.cr);		
	end query_arc;

	procedure query_circle (c : in type_stop_circles.cursor) is 
		use et_packages.pac_shapes;
	begin
		case element (c).filled is
			when NO =>
				-- We draw a normal non-filled circle:
				cairo.set_line_width (context.cr, type_view_coordinate (element (c).border_width));

				pac_draw_package.draw_circle (
					area		=> in_area,
					context		=> context,
					circle		=> element (c),
					filled		=> NO,
					height		=> self.frame_height);
				
			when YES =>
				-- We draw a filled circle with a certain fill style:
				case element (c).fill_style is
					when SOLID =>
						pac_draw_package.draw_circle (
							area		=> in_area,
							context		=> context,
							circle		=> element (c),
							filled		=> YES,
							height		=> self.frame_height);

					when HATCHED 	=> null; -- CS
				end case;
		end case;

		cairo.stroke (context.cr);
	end query_circle;

	procedure query_polygon (c : in type_stop_polygons.cursor) is 
		use et_packages.pac_shapes;
	begin
		case element (c).fill_style is
			when SOLID =>
				pac_draw_package.draw_polygon (
					area	=> in_area,
					context	=> context,
					polygon	=> element (c),
					filled	=> YES,
					height	=> self.frame_height);

			when HATCHED =>
				set_line_width (context.cr, type_view_coordinate (element (c).hatching.border_width));

				pac_draw_package.draw_polygon (
					area	=> in_area,
					context	=> context,
					polygon	=> element (c),
					filled	=> NO,
					height	=> self.frame_height);

				-- CS hatching ?
		end case;

		cairo.stroke (context.cr);
	end query_polygon;

	procedure query_cutout (c : in pac_stop_cutouts.cursor) is 
		use et_packages.pac_shapes;
	begin
		set_color_background (context.cr);
		
		pac_draw_package.draw_polygon (
			area	=> in_area,
			context	=> context,
			polygon	=> element (c),
			filled	=> YES,
			height	=> self.frame_height);

		cairo.stroke (context.cr);
	end query_cutout;

	
	procedure query_items (
		module_name	: in type_module_name.bounded_string;
		module		: in type_module) is
	begin
		-- All stop mask segments will be drawn with the same color:
		set_color_stop_mask (context.cr, face);

		case face is
			when TOP =>
				iterate (module.board.stop_mask.top.lines, query_line'access);
				iterate (module.board.stop_mask.top.arcs, query_arc'access);
				iterate (module.board.stop_mask.top.circles, query_circle'access);
				iterate (module.board.stop_mask.top.polygons, query_polygon'access);
				iterate (module.board.stop_mask.top.cutouts, query_cutout'access);
				-- CS iterate (module.board.stop_mask.top.texts, query_text'access);

			when BOTTOM =>
				iterate (module.board.stop_mask.bottom.lines, query_line'access);
				iterate (module.board.stop_mask.bottom.arcs, query_arc'access);
				iterate (module.board.stop_mask.bottom.circles, query_circle'access);
				iterate (module.board.stop_mask.bottom.polygons, query_polygon'access);
				iterate (module.board.stop_mask.bottom.cutouts, query_cutout'access);

				-- CS see above
		end case;

	end query_items;
	
begin -- draw_stop
-- 	put_line ("draw solder stop mask ...");
	
	type_modules.query_element (
		position	=> et_canvas_schematic.current_active_module,
		process		=> query_items'access);
	
end draw_stop;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
