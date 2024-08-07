------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      BOARD DRAW VIA RESTRICT                             --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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

with et_display.board;			use et_display.board;
with et_colors;					use et_colors;
with et_conductor_text.boards;	use et_conductor_text.boards;
with et_route_restrict;			use et_route_restrict;
with et_via_restrict;
with et_via_restrict.boards;	use et_via_restrict.boards;

separate (et_canvas_board)


procedure draw_via_restrict (
	self    : not null access type_view) 
is
	use pac_via_restrict_lines;
	use pac_via_restrict_arcs;
	use pac_via_restrict_circles;
	use pac_via_restrict_contours;
	use pac_via_restrict_cutouts;


	-- CS must be overwritten according to select status:
	brightness : type_brightness := NORMAL;

	
	
	procedure query_line (c : in pac_via_restrict_lines.cursor) is begin

		-- Draw the line if restrict layer is enabled:
		if via_restrict_layer_enabled (element (c).layers) then
			set_line_width (context.cr, type_view_coordinate (element (c).width));
		
			draw_line (
				line	=> to_line_fine (element (c)),
				width	=> et_via_restrict.via_restrict_line_width);

		end if;
	end query_line;

	
	procedure query_arc (c : in pac_via_restrict_arcs.cursor) is begin

		-- Draw the arc if restrict layer is enabled:
		if via_restrict_layer_enabled (element (c).layers) then
			set_line_width (context.cr, type_view_coordinate (element (c).width));
			
			draw_arc (
				arc		=> to_arc_fine (element (c)),
				width	=> et_via_restrict.via_restrict_line_width);

		end if;
	end query_arc;

	
	procedure query_circle (c : in pac_via_restrict_circles.cursor) is begin

		-- Draw the circle if restrict layer is enabled:
		if via_restrict_layer_enabled (element (c).layers) then
			set_line_width (context.cr, type_view_coordinate (element (c).width));
			
			draw_circle (
				circle	=> element (c),
				filled	=> NO,
				width	=> element (c).width);
					
		end if;
	end query_circle;

	
	procedure query_polygon (c : in pac_via_restrict_contours.cursor) is
		drawn : boolean := false;
	begin
		-- Draw the polygon if restrict layer is enabled:
		if via_restrict_layer_enabled (element (c).layers) then
			
			draw_contour (
				contour	=> element (c),
				filled	=> YES,
				width	=> zero,
				drawn	=> drawn);

		end if;
	end query_polygon;

	
	procedure query_cutout (c : in pac_via_restrict_cutouts.cursor) is 
		drawn : boolean := false;
	begin
		-- Draw the zone if restrict layer is enabled:
		if via_restrict_layer_enabled (element (c).layers) then

			set_color_background (context.cr);
			
			draw_contour (
				contour	=> element (c),
				filled	=> YES,
				width	=> zero,
				drawn	=> drawn);

		end if;
	end query_cutout;

	
	procedure query_items (
		module_name	: in pac_module_name.bounded_string;
		module		: in et_schematic.type_module) is
	begin
		cairo.set_line_width (context.cr, type_view_coordinate (et_via_restrict.via_restrict_line_width));

		set_color_via_restrict (context.cr, brightness);
		
		iterate (module.board.via_restrict.lines, query_line'access);
		iterate (module.board.via_restrict.arcs, query_arc'access);
		iterate (module.board.via_restrict.circles, query_circle'access);
		iterate (module.board.via_restrict.contours, query_polygon'access);
		iterate (module.board.via_restrict.cutouts, query_cutout'access);
		
		cairo.stroke (context.cr);
	end query_items;


	

	use et_pcb_stack;
	
	-- The top conductor layer 1 is always there:
	top_layer		: constant type_signal_layer := type_signal_layer'first;

	-- The deepest conductor layer towards bottom is defined by the layer stack:
	bottom_layer	: constant type_signal_layer := 
		deepest_conductor_layer (current_active_module);
	

	
begin -- draw_via_restrict
-- 	put_line ("draw via restrict ...");
	
	pac_generic_modules.query_element (
		position	=> current_active_module,
		process		=> query_items'access);
	
end draw_via_restrict;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
