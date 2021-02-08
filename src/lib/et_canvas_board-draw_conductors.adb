------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW CONDUCTORS                           --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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

with et_vias;					use et_vias;
use et_vias.pac_vias;

with et_pcb_stack;				use et_pcb_stack;
with et_design_rules;			use et_design_rules;
with et_display.board;			use et_display.board;
with et_canvas_primitive_draw_ops;

separate (et_canvas_board)

procedure draw_conductors (
	self    : not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context) 
is
	use et_schematic;
	use et_schematic.pac_nets;

	use pac_draw_fab;
	use et_board_shapes_and_text;
	use et_board_shapes_and_text.pac_text_fab;
	use et_board_shapes_and_text.pac_shapes;	

	use pac_conductor_lines;
	use pac_conductor_arcs;
	use pac_conductor_circles;
	use pac_conductor_cutouts;
	use pac_conductor_polygons_floating_solid;
	use pac_conductor_polygons_floating_hatched;
	use pac_signal_polygons_solid;
	use pac_signal_polygons_hatched;
	
	use et_pcb.pac_text_placeholders_conductors;
	use et_packages.pac_conductor_texts;
	
	-- For diplaying net names and classes we need this stuff:
	is_signal : boolean := false;
	net_name : pac_net_name.bounded_string;
	net_class : pac_net_class_name.bounded_string;

	-- The conductor layers are drawn in the order bottom-to-top so that
	-- the upper layers always obscure the layers underneath.

	-- The top conductor layer 1 is always there:
	top_layer		: constant type_signal_layer := type_signal_layer'first;

	-- The deepest conductor layer towards bottom is defined by the layer stack:
	bottom_layer	: constant type_signal_layer := 
		deepest_conductor_layer (et_canvas_schematic.current_active_module);

	function is_double_layer_board return boolean is begin
		if bottom_layer = 2 then
			return true;
		else 
			return false;
		end if;
	end is_double_layer_board;
	
	function is_inner_layer (layer : in type_signal_layer) return boolean is begin
		if layer > top_layer and layer < bottom_layer then
			return true;
		else
			return false;
		end if;
	end is_inner_layer;		
	
	-- The layer being drawn:
	current_layer : type_signal_layer;
	
	procedure query_line (c : in pac_conductor_lines.cursor) is begin

		-- Draw the line if it is in the current layer:
		if element (c).layer = current_layer then
			
			cairo.set_line_width (context.cr, type_view_coordinate (element (c).width));
			
			draw_line (
				area		=> in_area,
				context		=> context,
				line		=> element (c),
				height		=> self.frame_height);

		end if;
	end query_line;

	procedure query_arc (c : in pac_conductor_arcs.cursor) is begin

		-- Draw the arc if it is in theh current layer:
		if element (c).layer = current_layer then

			cairo.set_line_width (context.cr, type_view_coordinate (element (c).width));

			draw_arc (
				area		=> in_area,
				context		=> context,
				arc			=> element (c),
				height		=> self.frame_height);

		end if;
	end query_arc;

	procedure query_circle (c : in et_pcb.pac_conductor_circles.cursor) is 
		use et_packages;
	begin
		-- Draw the circle if it is in the current layer:
		if element (c).layer = current_layer then
			
			case element (c).filled is
				when NO =>
					-- We draw a normal non-filled circle:
					cairo.set_line_width (context.cr, type_view_coordinate (element (c).border_width));

					draw_circle (
						area		=> in_area,
						context		=> context,
						circle		=> element (c),
						filled		=> NO,
						height		=> self.frame_height);
					
				when YES =>
					-- We draw a filled circle with a certain fill style:
					case element (c).fill_style is
						when SOLID =>
							draw_circle (
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

	procedure query_polygon (c : in pac_conductor_polygons_floating_solid.cursor) is
	begin
		-- Draw the polygon if it is in the current layer:
		if element (c).properties.layer = current_layer then
			
			draw_polygon (
				area	=> in_area,
				context	=> context,
				polygon	=> element (c),
				filled	=> NO,
				height	=> self.frame_height);

			-- CS iterate filled_areas
		end if;
	end query_polygon;

	procedure query_polygon (c : in pac_conductor_polygons_floating_hatched.cursor) is 
	begin
		-- Draw the polygon if it is in the current layer:
		if element (c).properties.layer = current_layer then
			
			draw_polygon (
				area	=> in_area,
				context	=> context,
				polygon	=> element (c),
				filled	=> NO,
				height	=> self.frame_height);

			-- CS iterate filled_areas
			
		end if;
	end query_polygon;

	procedure query_polygon (c : in pac_signal_polygons_solid.cursor) is 
	begin
		
		-- Draw the polygon if it is in the current layer:
		if element (c).properties.layer = current_layer then
			
			draw_polygon (
				area	=> in_area,
				context	=> context,
				polygon	=> element (c),
				filled	=> NO,
				height	=> self.frame_height);

			-- CS iterate filled_areas
			
		end if;
	end query_polygon;

	procedure query_polygon (c : in pac_signal_polygons_hatched.cursor) is 
	begin
		
		-- Draw the polygon if it is in the current layer:
		if element (c).properties.layer = current_layer then
			
			draw_polygon (
				area	=> in_area,
				context	=> context,
				polygon	=> element (c),
				filled	=> NO,
				height	=> self.frame_height);

			-- CS iterate filled_areas
			
		end if;
	end query_polygon;
	
	procedure query_cutout (c : in et_pcb.pac_conductor_cutouts.cursor) is
	begin
		-- Draw the zone if it is in the current layer:
		if element (c).layer = current_layer then

			--save (context.cr);
			--set_color_background (context.cr);
			
			draw_polygon (
				area	=> in_area,
				context	=> context,
				polygon	=> element (c),
				filled	=> NO,
				height	=> self.frame_height);

			--restore (context.cr);
		end if;
	end query_cutout;

	procedure query_placeholder (c : in et_pcb.pac_text_placeholders_conductors.cursor) is 
		use pac_vector_text_lines;
		vector_text : pac_vector_text_lines.list;
	begin
		-- Draw the placeholder if it is in theh current layer:
		if element (c).layer = current_layer then

			draw_text_origin (self, element (c).position, in_area, context);

			-- Set the line width of the vector text:
			set_line_width (context.cr, type_view_coordinate (element (c).line_width));

			-- Vectorize the text:
			vector_text := vectorize_text (
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
			draw_vector_text (in_area, context, vector_text, self.frame_height);

		end if;
	end query_placeholder;

	procedure query_text (c : in et_packages.pac_conductor_texts.cursor) is 
		use pac_vector_text_lines;
		vector_text : pac_vector_text_lines.list;
	begin
		-- Draw the text if it is in theh current layer:
		if element (c).layer = current_layer then

			draw_text_origin (self, element (c).position, in_area, context);

			-- Set the line width of the vector text:
			set_line_width (context.cr, type_view_coordinate (element (c).line_width));
			
			-- Vectorize the text:
			vector_text := vectorize_text (
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
			draw_vector_text (in_area, context, vector_text, self.frame_height);

		end if;
	end query_text;

	procedure query_net_track (n : in pac_nets.cursor) is begin
		is_signal := true;
		net_name := key (n);
		net_class := element (n).class;

		iterate (element (n).route.lines, query_line'access);
		iterate (element (n).route.arcs, query_arc'access);
		-- CS ? iterate (element (n).route.circles, query_circle'access);
		
		iterate (element (n).route.polygons.solid, query_polygon'access);
		iterate (element (n).route.polygons.hatched, query_polygon'access);
		
		-- CS iterate (element (n).route.cutouts, query_cutout'access);
	end query_net_track;

	procedure query_via (v : in pac_vias.cursor) is 
		type type_circle is new pac_shapes.type_circle with null record;
		circle : type_circle;

		radius_base : type_distance_positive;

		procedure set_width_and_radius (r : in type_restring_width) is begin
			set_line_width (context.cr, type_view_coordinate (r));
			circle.radius := radius_base + r / 2.0;
		end set_width_and_radius;

		procedure draw_restring is begin
			set_color_vias (context.cr);
			
			draw_circle (
				area		=> in_area,
				context		=> context,
				circle		=> circle,
				filled		=> NO,
				height		=> self.frame_height);
		end draw_restring;

		
		-- These flags are used to prevent objects from being drawn
		-- multple times at the same place:
		outer_restring_drawn, inner_restring_drawn, net_name_drawn,
		numbers_drawn, drill_size_drawn, cancel : boolean := false;

		-- CS display restring width ?			

		
		-- Draws the net name right in the center of the via (no offset).
		-- The text size is set automatically with the radius of the drill:
		procedure draw_net_name is 
			use et_text;
			position : type_point := circle.center;
		begin
			if not net_name_drawn then
				
				-- The net name is displayed in a special color:
				set_color_via_net_name (context.cr);

				pac_draw_doc.draw_text (
					area		=> in_area,
					context		=> context,
					content		=> to_content (to_string (net_name)),
					size		=> radius_base * text_size_factor,
					font		=> via_text_font,
					position	=> position,
					origin		=> false,
					rotation	=> zero_rotation,
					alignment	=> (center, center),
					height		=> self.frame_height);

				net_name_drawn := true;
			end if;
		end draw_net_name;

		-- Draws the layer numbers above the net name.
		-- The text size is set automatically with the radius of the drill:
		procedure draw_numbers (from, to : in string) is 
			use et_text;
			position : type_point := circle.center;
			offset : type_point := type_point (
				set (zero, + radius_base * text_position_layer_and_drill_factor));
		begin
			move_by (position, offset);
			
			-- The layer numbers are displayed in a special color:
			set_color_via_layers (context.cr);

			pac_draw_doc.draw_text (
				area		=> in_area,
				context		=> context,
				content		=> to_content (from & "-" & to),
				size		=> radius_base * text_size_factor,
				font		=> via_text_font,
				position	=> position,
				origin		=> false,
				rotation	=> zero_rotation,
				alignment	=> (center, center),
				height		=> self.frame_height);
			
		end draw_numbers;

		-- Draws the drill size below the net name.
		-- The text size is set automatically with the radius of the drill:
		procedure draw_drill_size is 
			use et_text;
			position : type_point := circle.center;
			offset : type_point;
		begin
			if not drill_size_drawn then

				offset := type_point (
					set (zero, - radius_base * text_position_layer_and_drill_factor));
				
				move_by (position, offset);
						
				-- The drill size is displayed in a special color:
				set_color_via_drill_size (context.cr); -- CS

				pac_draw_doc.draw_text (
					area		=> in_area,
					context		=> context,
					content		=> to_content (to_string (element (v).diameter)),
					size		=> radius_base * text_size_factor,
					font		=> via_text_font,
					position	=> position,
					origin		=> false,
					rotation	=> zero_rotation,
					alignment	=> (center, center),
					height		=> self.frame_height);

				drill_size_drawn := true;
			end if;
		end draw_drill_size;
		
		
		procedure query_category is 
			
			procedure draw_numbers_blind_top is begin
				-- Draw the layer numbers only once:
				if not numbers_drawn then
					draw_numbers (
						from	=> "T",
						to		=> to_string (element (v).lower));

					numbers_drawn := true;
				end if;
			end draw_numbers_blind_top;

			procedure draw_numbers_blind_bottom is begin
				-- Draw the layer numbers only once:
				if not numbers_drawn then
					draw_numbers (
						from	=> "B",
						to		=> to_string (element (v).upper));

					numbers_drawn := true;
				end if;
			end draw_numbers_blind_bottom;		
			
		begin -- query_category	
			case element (v).category is
				when THROUGH =>
					if is_inner_layer (current_layer) then
						-- current_layer is an inner layer
						set_width_and_radius (element (v).restring_inner);

						inner_restring_drawn := true;
					else
						-- current_layer is an outer layer
						set_width_and_radius (element (v).restring_outer);

						outer_restring_drawn := true;
					end if;

					draw_restring;

					-- For a double layer board it is sufficent to draw 
					-- the restring of the top or bottom layer. Double layer boards
					-- do not have inner restrings for vias.
					if is_double_layer_board then
						if outer_restring_drawn then
							cancel := true; -- causes the layer iterator to cancel
						end if;
					else 
					-- For a multilayer board we need to draw only one outer restring
					-- (top or bottom, which one does not matter) and one inner restring.
					-- Once that is done, there is no need to draw the via again.	
						if outer_restring_drawn and inner_restring_drawn then
							cancel := true; -- causes the layer iterator to cancel
						end if;
					end if;

					draw_net_name;
					draw_drill_size;
					
					-- NOTE: For a through via, no layer numbers are displayed.
					
				when BURIED =>
					if element (v).layers.upper = current_layer 
					or element (v).layers.lower = current_layer then
						set_width_and_radius (element (v).restring_inner);
					
						draw_restring;

						-- Since the inner restring width is the same for all
						-- inner signal layers, it is sufficent to draw only one
						-- restring.
						cancel := true;  -- causes the layer iterator to cancel
					
						-- Draw the layer numbers only once (cancel flag already set)
						draw_numbers (
							from	=> to_string (element (v).layers.upper),
							to		=> to_string (element (v).layers.lower));
					
						draw_net_name;
						draw_drill_size;
					end if;
					
				when BLIND_DRILLED_FROM_TOP =>
					if current_layer = top_layer then
						set_width_and_radius (element (v).restring_top);
						outer_restring_drawn := true;
						draw_restring;
						draw_numbers_blind_top;
						draw_net_name;
						draw_drill_size;
					end if;

					if current_layer = element (v).lower then
						set_width_and_radius (element (v).restring_inner);
						inner_restring_drawn := true;
						draw_restring;
						draw_numbers_blind_top;
						draw_net_name;
						draw_drill_size;
					end if;

					-- At least the top restring AND one inner restring 
					-- must have been drawn. After that no more restring
					-- shall be drawn.
					if outer_restring_drawn and inner_restring_drawn then
						cancel := true; -- causes the layer iterator to cancel
					end if;

				when BLIND_DRILLED_FROM_BOTTOM =>
					if current_layer = bottom_layer then
						set_width_and_radius (element (v).restring_bottom);
						outer_restring_drawn := true;
						draw_restring;
						draw_numbers_blind_bottom;
						draw_net_name;
						draw_drill_size;
					end if;

					if current_layer = element (v).upper then
						set_width_and_radius (element (v).restring_inner);
						inner_restring_drawn := true;
						draw_restring;
						draw_numbers_blind_bottom;
						draw_net_name;
						draw_drill_size;
					end if;

					-- At least the bottom restring AND one inner restring 
					-- must have been drawn. After that no more restring
					-- shall be drawn.
					if outer_restring_drawn and inner_restring_drawn then
						cancel := true;
					end if;
					
			end case;
		end query_category;
		
	begin -- query_via
		circle.center := element (v).position;
		radius_base := element (v).diameter / 2.0;
		
		if vias_enabled then

			-- Iterate all conductor layers starting at the bottom layer and ending
			-- with the top layer:
			for ly in reverse top_layer .. bottom_layer loop

				-- Draw the layer only if it is enabled. Otherwise skip the layer:
				if conductor_enabled (ly) then
					
					-- Set the layer being drawn:
					current_layer := ly;

					query_category;
				end if;

				-- If the cancel flag has been set after drawing the via,
				-- then exit this iteration. This prevents objects from begin
				-- drawn multiple times:
				if cancel then
					exit;
				end if;

			end loop;
		end if;
	end query_via;

	
	-- Draws the vias of the current net:
	procedure query_net_via (n : in pac_nets.cursor) is begin
		net_name := key (n);
		net_class := element (n).class;

		iterate (element (n).route.vias, query_via'access);
	end query_net_via;

	
	-- Draws the tracks, vias and texts in conductor layers:
	procedure query_items (
		module_name	: in pac_module_name.bounded_string;
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
				iterate (module.board.conductors.lines, query_line'access);
				iterate (module.board.conductors.arcs, query_arc'access);
				iterate (module.board.conductors.circles, query_circle'access);
				iterate (module.board.conductors.polygons.solid, query_polygon'access);
				iterate (module.board.conductors.polygons.hatched, query_polygon'access);
				iterate (module.board.conductors.cutouts, query_cutout'access);

				-- texts
				iterate (module.board.conductors.placeholders, query_placeholder'access);
				iterate (module.board.conductors.texts, query_text'access);

				-- tracks:
				iterate (module.nets, query_net_track'access);

				
				draw_text_being_placed_in_conductors (
					self, in_area, context, et_packages.LAYER_CAT_CONDUCTOR, ly);
				
			end if;
		end loop;

		-- Draw the vias that exist in the nets:
		iterate (module.nets, query_net_via'access);
	end query_items;

	
	-- CS: Currently this list will contain only the single via
	-- that is being placed. It could be useful for copy/paste/move of
	-- groups of vias.
	vias_being_placed : pac_vias.list;

	-- Draws the via that is attached to the primary tool while the
	-- operator is placing the via.
	-- Uses list vias_being_placed as storage place for the single via.
	procedure draw_via_being_placed is 
		-- The place where the via shall be placed:
		position : type_point;
	begin
		if via_place.being_moved then

			-- Set the point where the via is to be drawn:
			position := self.tool_position;

			-- Get the name of the targeted net:
			net_name := get_name (via_place.net);
			
			case via_place.category is
				when THROUGH =>
					append (
						container	=> vias_being_placed,
						new_item	=> (
							category		=> THROUGH,
							diameter		=> via_place.drill.diameter,
							position		=> position,
							restring_inner	=> via_place.restring_inner,
							restring_outer	=> via_place.restring_outer));

				when BLIND_DRILLED_FROM_TOP =>
					append (
						container	=> vias_being_placed,
						new_item	=> (
							category		=> BLIND_DRILLED_FROM_TOP,
							diameter		=> via_place.drill.diameter,
							position		=> position,
							restring_inner	=> via_place.restring_inner,
							restring_top	=> via_place.restring_outer,
							lower			=> via_place.destination_blind));
					
				when BLIND_DRILLED_FROM_BOTTOM =>
					append (
						container	=> vias_being_placed,
						new_item	=> (
							category		=> BLIND_DRILLED_FROM_BOTTOM,
							diameter		=> via_place.drill.diameter,
							position		=> position,
							restring_inner	=> via_place.restring_inner,
							restring_bottom	=> via_place.restring_outer,
							upper			=> via_place.destination_blind));
					
				when BURIED =>
					append (
						container	=> vias_being_placed,
						new_item	=> (
							category		=> BURIED,
							diameter		=> via_place.drill.diameter,
							position		=> position,
							layers			=> via_place.layers_buried,
							restring_inner	=> via_place.restring_inner));
					
			end case;

			-- draw the single via that is in container vias_being_placed:
			vias_being_placed.iterate (query_via'access); 
		end if;
	end draw_via_being_placed;
	
begin -- draw_conductors
-- 	put_line ("draw conductor layers ...");
	
	pac_generic_modules.query_element (
		position	=> et_canvas_schematic.current_active_module,
		process		=> query_items'access);

	draw_via_being_placed;
	
end draw_conductors;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
