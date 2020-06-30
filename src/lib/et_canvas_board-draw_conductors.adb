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

with et_pcb;					use et_pcb;
use et_pcb.pac_vias;

with et_pcb_stack;				use et_pcb_stack;
with et_display.board;			use et_display.board;
with et_canvas_primitive_draw_ops;

separate (et_canvas_board)

procedure draw_conductors (
	self    : not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context) is

	use et_schematic;
	use et_schematic.type_nets;
	
	use et_general;
	use et_terminals.pac_shapes;	
	use et_packages;
	use pac_copper_lines;
	use pac_copper_arcs;
	use et_pcb.pac_copper_circles;
	use et_pcb.pac_copper_cutouts;
	use pac_copper_polygons_floating_solid;
	use pac_copper_polygons_floating_hatched;
	use et_pcb.type_text_placeholders_copper;
	use et_pcb.pac_texts;
	
	-- For diplaying net names and classes we need this stuff:
	is_signal : boolean := false;
	net_name : type_net_name.bounded_string;
	net_class : type_net_class_name.bounded_string;

	-- The conductor layers are drawn in the order bottom-to-top so that
	-- the upper layers always obscure the layers underneath.

	-- The top conductor layer 1 is always there:
	top_layer		: constant type_signal_layer := type_signal_layer'first;

	-- The deepest conductor layer towards bottom is defined by the layer stack:
	bottom_layer	: constant type_signal_layer := 
		deepest_conductor_layer (et_canvas_schematic.current_active_module);
	
	-- The layer being drawn:
	current_layer : type_signal_layer;
	
	procedure query_line (c : in pac_copper_lines.cursor) is begin

		-- Draw the line if it is in the current layer:
		if element (c).layer = current_layer then
			
			cairo.set_line_width (context.cr, type_view_coordinate (element (c).width));
			
			pac_draw_package.draw_line (
				area		=> in_area,
				context		=> context,
				line		=> element (c),
				height		=> self.frame_height);

		end if;
	end query_line;

	procedure query_arc (c : in pac_copper_arcs.cursor) is begin

		-- Draw the arc if it is in theh current layer:
		if element (c).layer = current_layer then

			cairo.set_line_width (context.cr, type_view_coordinate (element (c).width));

			pac_draw_package.draw_arc (
				area		=> in_area,
				context		=> context,
				arc			=> element (c),
				height		=> self.frame_height);

		end if;
	end query_arc;

	procedure query_circle (c : in et_pcb.pac_copper_circles.cursor) is begin
		-- Draw the circle if it is in the current layer:
		if element (c).layer = current_layer then
			
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

		end if;
	end query_circle;

	procedure query_polygon (c : in et_pcb.pac_copper_polygons_floating_solid.cursor) is
	begin
		-- Draw the polygon if it is in the current layer:
		if element (c).layer = current_layer then
			
			pac_draw_package.draw_polygon (
				area	=> in_area,
				context	=> context,
				polygon	=> element (c),
				filled	=> YES,
				height	=> self.frame_height);

-- CS
-- 			easing : type_easing;
-- 		width_min		: type_track_width; -- the minimum width
-- 		isolation		: type_track_clearance := type_track_clearance'first; -- the space between foreign pads and the polygon
-- 		layer 			: type_signal_layer;
-- 		priority_level	: type_polygon_priority := type_polygon_priority'first;
			
		end if;

	end query_polygon;

	procedure query_polygon (c : in et_pcb.pac_copper_polygons_floating_hatched.cursor) is 
	begin
		null; -- CS
		
-- 		-- Draw the polygon if it is in the current layer:
-- 		if element (c).layer = current_layer then
-- 			
-- 			pac_draw_package.draw_polygon (
-- 				area	=> in_area,
-- 				context	=> context,
-- 				polygon	=> element (c),
-- 				filled	=> YES,
-- 				height	=> self.frame_height);
-- 			

-- 		easing : type_easing;
-- 		hatching : type_hatching;
-- 		width_min		: type_track_width; -- the minimum width
-- 		isolation		: type_track_clearance := type_track_clearance'first; -- the space between foreign pads and the polygon
-- 		layer 			: type_signal_layer;
-- 		priority_level	: type_polygon_priority := type_polygon_priority'first;
		
-- 		end if;
	end query_polygon;

	procedure query_cutout (c : in et_pcb.pac_copper_cutouts.cursor) is
	begin
		-- Draw the zone if it is in the current layer:
		if element (c).layer = current_layer then

			save (context.cr);
			set_color_background (context.cr);
			
			pac_draw_package.draw_polygon (
				area	=> in_area,
				context	=> context,
				polygon	=> element (c),
				filled	=> YES,
				height	=> self.frame_height);

			restore (context.cr);
		end if;
	end query_cutout;

	procedure query_placeholder (c : in et_pcb.type_text_placeholders_copper.cursor) is 
		use et_terminals.pac_text.pac_vector_text_lines;
		vector_text : et_terminals.pac_text.pac_vector_text_lines.list;
	begin
		-- Draw the placeholder if it is in theh current layer:
		if element (c).layer = current_layer then

			draw_text_origin (self, element (c).position, in_area, context);

			-- Set the line width of the vector text:
			set_line_width (context.cr, type_view_coordinate (element (c).line_width));

			-- Vectorize the text:
			vector_text := et_terminals.pac_text.vectorize (
				content		=> to_placeholder_content (element (c).meaning),
				size		=> element (c).size,
				rotation	=> rot (element (c).position),
				position	=> type_point (element (c).position),

				-- Mirror the text only if it is in the bottom layer:
				mirror		=> signal_layer_to_mirror (element (c).layer, bottom_layer),
				
				line_width	=> element (c).line_width,
				alignment	=> element (c).alignment -- right, bottom
				);

			-- Draw the text:
			pac_draw_package.draw_vector_text (in_area, context, vector_text, self.frame_height);

		end if;
	end query_placeholder;

	procedure query_text (c : in et_pcb.pac_texts.cursor) is 
		use et_terminals.pac_text.pac_vector_text_lines;
		vector_text : et_terminals.pac_text.pac_vector_text_lines.list;
	begin
		-- Draw the text if it is in theh current layer:
		if element (c).layer = current_layer then

			draw_text_origin (self, element (c).position, in_area, context);

			-- Set the line width of the vector text:
			set_line_width (context.cr, type_view_coordinate (element (c).line_width));

			-- Vectorize the text:
			vector_text := et_terminals.pac_text.vectorize (
				content		=> element (c).content,
				size		=> element (c).size,
				rotation	=> rot (element (c).position),
				position	=> type_point (element (c).position),

				-- Mirror the text only if it is in the bottom layer:
				mirror		=> signal_layer_to_mirror (element (c).layer, bottom_layer),
				
				line_width	=> element (c).line_width,
				alignment	=> element (c).alignment -- right, bottom
				);

			-- Draw the text:
			pac_draw_package.draw_vector_text (in_area, context, vector_text, self.frame_height);

		end if;
	end query_text;
	
	
	procedure query_via (v : in pac_vias.cursor) is 
		type type_circle is new et_terminals.pac_shapes.type_circle with null record;
		circle : type_circle;

		function greatest_restring return type_restring_width is begin
			if element (v).restring_inner > element (v).restring_outer then
				return element (v).restring_inner;
			else
				return element (v).restring_outer;
			end if;
		end greatest_restring;
		
	begin -- query_via
		circle.center := element (v).position;

		if vias_enabled then
			set_color_vias (context.cr);

			-- Draw a filled circle with the greatest available restring:
			circle.radius := element (v).diameter / 2.0 + greatest_restring;
		else
			if current_layer = bottom_layer or current_layer = top_layer then

				-- Draw a filled circle with the restring of outer layers:
				circle.radius := element (v).diameter / 2.0 + element (v).restring_outer;
				--put_line ("outer " & to_string (distance => circle.radius * 2.0));
				
			else
				-- Draw a filled circle with the restring of inner layers:
				circle.radius := element (v).diameter / 2.0 + element (v).restring_inner;
				--put_line ("inner " & to_string (distance => circle.radius * 2.0));
			end if;

		end if;

		-- Draw a large filled circle to show the restring:
		pac_draw_package.draw_circle (
			area		=> in_area,
			context		=> context,
			circle		=> circle,
			filled		=> YES,
			height		=> self.frame_height);

		-- Draw a small filled circle to show the drill:
		
		-- Draw the drill hole. It is a filled circle with background color
		-- and diameter as given by the drill:
		set_color_background (context.cr);

		circle.radius := element (v).diameter / 2.0;
		--put_line ("drill " & to_string (distance => circle.radius * 2.0));

		pac_draw_package.draw_circle (
			area		=> in_area,
			context		=> context,
			circle		=> circle,
			filled		=> YES,
			height		=> self.frame_height);

		-- CS draw layer numbers
		
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
		--iterate (element (n).route.polygons_2, query_polygon'access);
		--iterate (element (n).route.cutouts, query_cutout'access);

		iterate (element (n).route.vias, query_via'access);
	end query_net;
	
	procedure query_items (
		module_name	: in type_module_name.bounded_string;
		module		: in type_module) is
	begin
		-- Iterate all conductor layers starting at the bottom layer and ending
		-- with the top layer:
		for ly in reverse top_layer .. bottom_layer loop

			-- Draw the layer only if it is enabled. Otherwise skip the layer:
			if conductor_enabled (ly) then
				
				-- Set the layer being drawn:
				current_layer := ly;
				--put_line (to_string (current_layer));

				-- set color according to layer
				set_color_conductor (context.cr, current_layer);

				
				-- freetracks:
				is_signal := false;
				iterate (module.board.copper.lines, query_line'access);
				iterate (module.board.copper.arcs, query_arc'access);
				iterate (module.board.copper.circles, query_circle'access);
				iterate (module.board.copper.polygons.solid, query_polygon'access);
				iterate (module.board.copper.polygons.hatched, query_polygon'access);
				iterate (module.board.copper.cutouts, query_cutout'access);

				-- texts
				iterate (module.board.copper.placeholders, query_placeholder'access);
				iterate (module.board.copper.texts, query_text'access);

				-- tracks:
				iterate (module.nets, query_net'access);
				
			end if;
		end loop;
	end query_items;
	
begin -- draw_conductors
-- 	put_line ("draw conductor layers ...");
	
	et_project.modules.type_modules.query_element (
		position	=> et_canvas_schematic.current_active_module,
		process		=> query_items'access);
	
end draw_conductors;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
