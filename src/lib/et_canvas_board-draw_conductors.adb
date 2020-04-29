------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW CONDUCTORS                           --
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
use et_schematic.type_nets;

with et_project;				use et_project;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_packages;				use et_packages;
use et_pcb_coordinates.geometry;

with et_pcb;					use et_pcb;
use et_pcb.pac_vias;

with et_display.board;			use et_display.board;

with et_canvas_primitive_draw_ops;

separate (et_canvas_board)

procedure draw_conductors (
	self    : not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context) is
	
	use pac_copper_lines;
	use pac_copper_arcs;
	use et_pcb.pac_copper_circles;

	-- For diplaying net names and classes we need this stuff:
	is_signal : boolean := false;
	net_name : type_net_name.bounded_string;
	net_class : type_net_class_name.bounded_string;

	
	procedure query_line (c : in pac_copper_lines.cursor) is begin

		-- Draw the line if the conductor layer is enabled:
		if conductor_enabled (element (c).layer) then
			
			cairo.set_line_width (context.cr, type_view_coordinate (element (c).width));

			-- set color according to layer
			set_color_conductor (context.cr, element (c).layer);
			
			pac_draw_package.draw_line (
				area		=> in_area,
				context		=> context,
				line		=> element (c),
				height		=> self.frame_height);

			cairo.stroke (context.cr);
		end if;
	end query_line;

	procedure query_arc (c : in pac_copper_arcs.cursor) is begin

		-- Draw the arc if the conductor layer is enabled:
		if conductor_enabled (element (c).layer) then

			cairo.set_line_width (context.cr, type_view_coordinate (element (c).width));

			-- set color according to layer
			set_color_conductor (context.cr, element (c).layer);
			
			pac_draw_package.draw_arc (
				area		=> in_area,
				context		=> context,
				arc			=> element (c),
				height		=> self.frame_height);

			cairo.stroke (context.cr);
		end if;
	end query_arc;

	procedure query_circle (c : in et_pcb.pac_copper_circles.cursor) is 
		use et_packages.pac_shapes;
	begin
		-- Draw the circle if the conductor layer is enabled:
		if conductor_enabled (element (c).layer) then
			
			-- set color according to layer
			set_color_conductor (context.cr, element (c).layer);

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
		end if;
	end query_circle;

	procedure query_via (v : in pac_vias.cursor) is begin

		if vias_enabled then
			set_color_vias (context.cr);
		end if;

-- 		type type_via is new type_drill with record
-- 			restring_outer	: type_restring_width;	-- restring in outer layers (top/bottom)
-- 			restring_inner	: type_restring_width;	-- restring in inner layers (mostly wider than restring_outer)
-- 			layer_start		: type_signal_layer;
-- 			layer_end		: type_signal_layer;
-- 		end record;

		
	end query_via;
	
	procedure query_net (n : in type_nets.cursor) is begin
		is_signal := true;
		net_name := key (n);
		net_class := element (n).class;

		iterate (element (n).route.lines, query_line'access);
		iterate (element (n).route.arcs, query_arc'access);
		-- CS ? iterate (element (n).route.circles, query_circle'access);
		iterate (element (n).route.vias, query_via'access);
		--iterate (element (n).route.polygons_2, query_polygon'access);
		--iterate (element (n).route.cutouts, query_cutout'access);
	end query_net;
	
	procedure query_items (
		module_name	: in type_module_name.bounded_string;
		module		: in type_module) is
	begin
		-- freetracks:
		is_signal := false;
		iterate (module.board.copper.lines, query_line'access);
		iterate (module.board.copper.arcs, query_arc'access);
		iterate (module.board.copper.circles, query_circle'access);

		-- tracks:
		iterate (module.nets, query_net'access);

		
		-- CS iterate (module.board.silk_screen.top.polygons, query_polygon'access);
		-- CS iterate (module.board.silk_screen.top.cutouts, query_polygon'cutout);
		-- CS iterate (module.board.silk_screen.top.placeholders, query_placeholder'access);
		-- CS iterate (module.board.silk_screen.top.texts, query_text'access);

	end query_items;
	
begin -- draw_conductors
-- 	put_line ("draw conductor layers ...");
	
	type_modules.query_element (
		position	=> et_canvas_schematic.current_active_module,
		process		=> query_items'access);
	
end draw_conductors;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
