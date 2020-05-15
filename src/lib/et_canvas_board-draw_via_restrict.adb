------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      BOARD DRAW VIA RESTRICT                             --
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
with et_display.board;			use et_display.board;

with et_canvas_primitive_draw_ops;

separate (et_canvas_board)

procedure draw_via_restrict (
	self    : not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context) is
	
	use type_via_restrict_lines;
	use type_via_restrict_arcs;
	use type_via_restrict_circles;
	use type_via_restrict_polygons;
	use pac_via_restrict_cutouts;
	
	procedure query_line (c : in type_via_restrict_lines.cursor) is begin

		-- Draw the line if restrict layer is enabled:
		if via_restrict_layer_enabled (element (c).layers) then
		
			pac_draw_package.draw_line (
				area		=> in_area,
				context		=> context,
				line		=> element (c),
				height		=> self.frame_height);

		end if;
	end query_line;

	procedure query_arc (c : in type_via_restrict_arcs.cursor) is begin

		-- Draw the arc if restrict layer is enabled:
		if via_restrict_layer_enabled (element (c).layers) then
		
			pac_draw_package.draw_arc (
				area		=> in_area,
				context		=> context,
				arc			=> element (c),
				height		=> self.frame_height);

			-- CS For some reason the arc is drawn filled. Should not be filled instead.

		end if;
	end query_arc;

	procedure query_circle (c : in type_via_restrict_circles.cursor) is 
		use et_packages.pac_shapes;
	begin

		-- Draw the circle if restrict layer is enabled:
		if via_restrict_layer_enabled (element (c).layers) then

			case element (c).filled is
				when NO =>
					-- We draw a normal non-filled circle:
					pac_draw_package.draw_circle (
						area		=> in_area,
						context		=> context,
						circle		=> element (c),
						filled		=> NO,
						height		=> self.frame_height);
					
				when YES =>
					-- We draw a solid filled circle:
					pac_draw_package.draw_circle (
						area		=> in_area,
						context		=> context,
						circle		=> element (c),
						filled		=> YES,
						height		=> self.frame_height);

					-- restore line width (draw_circle has set it to zero)
					set_line_width (context.cr, type_view_coordinate (route_restrict_line_width));
					
			end case;

		end if;
	end query_circle;

	procedure query_polygon (c : in type_via_restrict_polygons.cursor) is begin

		-- Draw the polygon if restrict layer is enabled:
		if via_restrict_layer_enabled (element (c).layers) then
			
			pac_draw_package.draw_polygon (
				area	=> in_area,
				context	=> context,
				polygon	=> element (c),
				filled	=> YES,
				height	=> self.frame_height);

		end if;
	end query_polygon;

	procedure query_cutout (c : in pac_via_restrict_cutouts.cursor) is begin

		-- Draw the zone if restrict layer is enabled:
		if via_restrict_layer_enabled (element (c).layers) then

			set_color_background (context.cr);
			
			pac_draw_package.draw_polygon (
				area	=> in_area,
				context	=> context,
				polygon	=> element (c),
				filled	=> YES,
				height	=> self.frame_height);

		end if;
	end query_cutout;

	procedure query_items (
		module_name	: in type_module_name.bounded_string;
		module		: in type_module) is
	begin
		cairo.set_line_width (context.cr, type_view_coordinate (via_restrict_line_width));

		set_color_via_restrict (context.cr);
		
		iterate (module.board.via_restrict.lines, query_line'access);
		iterate (module.board.via_restrict.arcs, query_arc'access);
		iterate (module.board.via_restrict.circles, query_circle'access);
		iterate (module.board.via_restrict.polygons, query_polygon'access);
		iterate (module.board.via_restrict.cutouts, query_cutout'access);

		cairo.stroke (context.cr);
	end query_items;
	
begin -- draw_via_restrict
-- 	put_line ("draw via restrict ...");
	
	type_modules.query_element (
		position	=> et_canvas_schematic.current_active_module,
		process		=> query_items'access);
	
end draw_via_restrict;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
