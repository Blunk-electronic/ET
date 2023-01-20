------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW CONDUCTORS                           --
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

--with ada.text_io;					use ada.text_io;

with et_pcb;						use et_pcb;
with et_conductor_segment.boards;	use et_conductor_segment.boards;
with et_fill_zones;					use et_fill_zones;
with et_fill_zones.boards;			use et_fill_zones.boards;
with et_conductor_text.boards;		use et_conductor_text.boards;
with et_vias;						use et_vias;
use et_vias.pac_vias;

with et_thermal_relief;				use et_thermal_relief;
with et_pcb_stack;					use et_pcb_stack;
with et_design_rules;				use et_design_rules;
with et_display.board;				use et_display.board;
with et_colors;						use et_colors;

separate (et_canvas_board)

procedure draw_conductors (
	self    : not null access type_view) 
is
	use et_schematic;
	use et_schematic.pac_nets;

	use pac_geometry_2;
	use pac_polygons;
	
	use pac_conductor_lines;
	use pac_conductor_arcs;
	use pac_conductor_circles;
	use pac_cutouts;
	use pac_floating_solid;
	use pac_floating_hatched;
	use pac_route_solid;
	use pac_route_hatched;
	
	use et_pcb.pac_text_placeholders_conductors;
	use pac_conductor_texts;

	-- CS must be overwritten according to select status:
	brightness : type_brightness := NORMAL;

	
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
		deepest_conductor_layer (current_active_module);

	
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


	
-- LINES, ARCS, CIRCLES
	
	procedure query_line (c : in pac_conductor_lines.cursor) is begin

		-- Draw the line if it is in the current layer:
		if element (c).layer = current_layer then
			
			set_line_width (context.cr, type_view_coordinate (element (c).width));
			
			draw_line (
				line		=> to_line_fine (element (c)),
				width		=> element (c).width);

		end if;
	end query_line;

	
	procedure query_arc (c : in pac_conductor_arcs.cursor) is begin

		-- Draw the arc if it is in theh current layer:
		if element (c).layer = current_layer then

			set_line_width (context.cr, type_view_coordinate (element (c).width));

			draw_arc (
				arc		=> to_arc_fine (element (c)),
				width	=> element (c).width);

		end if;
	end query_arc;

	
	procedure query_circle (c : in pac_conductor_circles.cursor) is begin
		-- Draw the circle if it is in the current layer:
		if element (c).layer = current_layer then
			
		-- We draw a normal non-filled circle:
		set_line_width (context.cr, type_view_coordinate (element (c).width));

		draw_circle (
			circle	=> element (c),
			filled	=> NO,
			width	=> element (c).width);

		end if;
	end query_circle;



	
-- CONDUCTOR FILL ZONES
	
	-- The width of the borders and stripes of a fill zone:
	fill_line_width : type_track_width;
	
	use pac_islands;
	
	procedure query_island (i : in pac_islands.cursor) is
		use pac_edges;
		use pac_lakes;
		use pac_stripes;

		island : type_island renames element (i);

		procedure draw_edge (e : in pac_edges.cursor) is begin
			draw_line (
				line	=> type_line_fine (element (e)),
				width	=> fill_line_width);
		end draw_edge;

		procedure query_lake (l : in pac_lakes.cursor) is begin
			element (l).centerline.edges.iterate (draw_edge'access);
		end query_lake;

		procedure draw_stripe (s : in pac_stripes.cursor) is begin
			draw_line (
				line	=> element (s),
				width	=> fill_line_width);
		end draw_stripe;
		
	begin
		island.shore.centerline.edges.iterate (draw_edge'access);
		island.lakes.iterate (query_lake'access);
		island.stripes.iterate (draw_stripe'access);
	end query_island;


	procedure query_relief (c : in pac_reliefes.cursor) is
		use pac_reliefes;
		use pac_spokes;
		
		relief : type_relief renames element (c);

		procedure query_spoke (s : in pac_spokes.cursor) is begin
			draw_line (
				line	=> element (s),
				width	=> relief.width);
		end query_spoke;
		
	begin
		iterate (relief.spokes, query_spoke'access);
	end query_relief;

	
	procedure query_fill_zone (c : in pac_floating_solid.cursor) is 
		drawn : boolean := false;
	begin
		-- Draw the zone if it is in the current layer:
		if element (c).properties.layer = current_layer then

			draw_contour (
				contour	=> element (c),
				style	=> DASHED,
				filled	=> NO, -- because this is merely the contour of the zone !
				width	=> zero, -- CS should be the dynamically calculated width of the contours
				drawn	=> drawn);

			-- Draw the islands if contour has been drawn:
			if drawn then
				-- All borders and fill lines will be drawn with the same width:
				fill_line_width := element (c).linewidth;			
				set_line_width (context.cr, type_view_coordinate (fill_line_width));
				iterate (element (c).islands, query_island'access);
			end if;
		end if;
	end query_fill_zone;

	
	procedure query_fill_zone (c : in pac_floating_hatched.cursor) is 
		drawn : boolean := false;
	begin
		-- Draw the zone if it is in the current layer:
		if element (c).properties.layer = current_layer then
			
			draw_contour (
				contour	=> element (c),
				style	=> DASHED,
				filled	=> NO, -- because this is merely the contour of the zone !
				width	=> zero, -- CS should be the dynamically calculated width of the contours
				drawn	=> drawn);

			-- Draw the islands if contour has been drawn:
			if drawn then
				-- All borders and fill lines will be drawn with the same width:
				fill_line_width := element (c).linewidth;			
				set_line_width (context.cr, type_view_coordinate (fill_line_width));
				iterate (element (c).islands, query_island'access);
			end if;

		end if;
	end query_fill_zone;

	
	procedure query_fill_zone (c : in pac_route_solid.cursor) is 
		zone : type_route_solid renames element (c);
		use pac_reliefes;
		drawn : boolean := false;
	begin
		-- Draw the zone if it is in the current layer:
		if zone.properties.layer = current_layer then
	
			draw_contour (
				contour	=> zone,
				style	=> DASHED,
				filled	=> NO, -- because this is merely the contour of the zone !
				width	=> zero, -- CS should be the dynamically calculated width of the contours
				drawn	=> drawn);

			-- Draw islands if contour has been drawn:
			if drawn then
				-- All borders and fill lines will be drawn with the same width:
				fill_line_width := zone.linewidth;			
				set_line_width (context.cr, type_view_coordinate (fill_line_width));
				iterate (zone.islands, query_island'access);

				if zone.connection = THERMAL then
					iterate (zone.reliefes, query_relief'access);
				end if;
			end if;
		end if;
	end query_fill_zone;

	
	procedure query_fill_zone (c : in pac_route_hatched.cursor) is
		zone : type_route_hatched renames element (c);
		use pac_reliefes;
		drawn : boolean := false;
	begin		
		-- Draw the zone if it is in the current layer:
		if zone.properties.layer = current_layer then

			draw_contour (
				contour	=> zone,
				style	=> DASHED,
				filled	=> NO, -- because this is merely the contour of the zone !
				width	=> zero, -- CS should be the dynamically calculated width of the contours
				drawn	=> drawn);

			-- Draw islands if contour has been drawn:
			if drawn then
				-- All borders and fill lines will be drawn with the same width:
				fill_line_width := zone.linewidth;			
				set_line_width (context.cr, type_view_coordinate (fill_line_width));
				iterate (zone.islands, query_island'access);

				if zone.connection = THERMAL then
					iterate (zone.reliefes, query_relief'access);
				end if;
			end if;
		end if;
	end query_fill_zone;


	
	procedure query_cutout (c : in pac_cutouts.cursor) is 
		drawn : boolean := false;
	begin
		-- Draw the zone if it is in the current layer:
		if element (c).layer = current_layer then

			--save (context.cr);
			--set_color_background (context.cr);
			
			draw_contour (
				contour	=> element (c),
				style	=> DASHED,
				filled	=> NO,
				width	=> zero,
				drawn	=> drawn);

			--restore (context.cr);
		end if;
	end query_cutout;



-- TEXTS
	
	procedure query_placeholder (c : in et_pcb.pac_text_placeholders_conductors.cursor) is 
		v_text : type_vector_text;
	begin
		-- Draw the placeholder if it is in the current layer:
		if element (c).layer = current_layer then

			draw_text_origin (self, element (c).position, area);

			-- Set the line width of the vector text:
			set_line_width (context.cr, type_view_coordinate (element (c).line_width));

			-- Vectorize the text:
			v_text := vectorize_text (
				content		=> to_placeholder_content (element (c).meaning),
				size		=> element (c).size,
				rotation	=> get_rotation (element (c).position),
				position	=> element (c).position.place,

				-- Mirror the text only if it is in the bottom layer:
				mirror		=> signal_layer_to_mirror (element (c).layer, bottom_layer),
				
				line_width	=> element (c).line_width,
				alignment	=> element (c).alignment -- right, bottom
				);

			-- Draw the text:
			draw_vector_text (area, v_text, element (c).line_width);

		end if;
	end query_placeholder;

	
	procedure query_text (c : in pac_conductor_texts.cursor) is begin
		-- Draw the text if it is in the current layer:
		if element (c).layer = current_layer then

			draw_text_origin (self, element (c).position, area);

			-- Set the line width of the vector text:
			set_line_width (context.cr, type_view_coordinate (element (c).line_width));
			
			-- Draw the text:
			draw_vector_text (area, element (c).vectors, element (c).line_width);

		end if;
	end query_text;

	
	procedure query_net_track (n : in pac_nets.cursor) is begin
		is_signal := true;
		net_name := key (n);
		net_class := element (n).class;
	
		iterate (element (n).route.lines, query_line'access);
		iterate (element (n).route.arcs, query_arc'access);
		-- CS ? iterate (element (n).route.circles, query_circle'access);
		
		iterate (element (n).route.fill_zones.solid, query_fill_zone'access);
		iterate (element (n).route.fill_zones.hatched, query_fill_zone'access);

		-- user defined cutout areas:
		-- CS iterate (element (n).route.cutouts, query_cutout'access);
	end query_net_track;



	
-- VIAS
	
	procedure query_via (v : in pac_vias.cursor) is 
		circle : type_circle;

		radius_base : type_distance_positive;

		procedure set_width_and_radius (r : in type_restring_width) is begin
			set_line_width (context.cr, type_view_coordinate (r));
			circle.radius := type_float_positive (radius_base + r / 2.0);
		end set_width_and_radius;

		procedure draw_restring is begin
			set_color_vias (context.cr);
			
			draw_circle (
				circle		=> circle,
				filled		=> NO,
				width		=> zero -- CS ?
				);
			
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

				draw_text (
					content		=> to_content (to_string (net_name)),
					size		=> radius_base * text_size_factor,
					font		=> via_text_font,
					position	=> position,
					origin		=> false,
					rotation	=> zero_rotation,
					alignment	=> (center, center));

				net_name_drawn := true;
			end if;
		end draw_net_name;

		
		-- Draws the layer numbers above the net name.
		-- The text size is set automatically with the radius of the drill:
		procedure draw_numbers (from, to : in string) is 
			use et_text;
			position : type_point := circle.center;
			offset : constant type_distance_relative := to_distance_relative (
				set (zero, + radius_base * text_position_layer_and_drill_factor));
		begin
			move_by (position, offset);
			
			-- The layer numbers are displayed in a special color:
			set_color_via_layers (context.cr);

			draw_text (
				content		=> to_content (from & "-" & to),
				size		=> radius_base * text_size_factor,
				font		=> via_text_font,
				position	=> position,
				origin		=> false,
				rotation	=> zero_rotation,
				alignment	=> (center, center));
			
		end draw_numbers;

		
		-- Draws the drill size below the net name.
		-- The text size is set automatically with the radius of the drill:
		procedure draw_drill_size is 
			use et_text;
			position : type_point := circle.center;
			offset : type_distance_relative;
		begin
			if not drill_size_drawn then

				offset := to_distance_relative (
					set (zero, - radius_base * text_position_layer_and_drill_factor));
				
				move_by (position, offset);
						
				-- The drill size is displayed in a special color:
				set_color_via_drill_size (context.cr); -- CS

				draw_text (
					content		=> to_content (to_string (element (v).diameter)),
					size		=> radius_base * text_size_factor,
					font		=> via_text_font,
					position	=> position,
					origin		=> false,
					rotation	=> zero_rotation,
					alignment	=> (center, center));

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
		module		: in type_module) 
	is

		procedure draw_ratsnest is
			use et_ratsnest;
			
			procedure query_net (n : in pac_nets.cursor) is 
				use pac_airwires;
				
				procedure query_airwire (c : in pac_airwires.cursor) is begin
					
					draw_line (
						line		=> type_line_fine (element (c)),
						width		=> type_distance (airwire_line_width));
					
				end query_airwire;


			begin -- query_net
				if not element (n).route.airwires.hidden then
					iterate (element (n).route.airwires.lines, query_airwire'access);
				end if;
			end query_net;

		begin
			if ratsnest_enabled then
				
				-- All airwires of all nets are drawn with the same
				-- color and width:
				set_color_ratsnest (context.cr);
				set_line_width (context.cr, type_view_coordinate (airwire_line_width));
				
				pac_nets.iterate (module.nets, query_net'access);
			end if;
		end draw_ratsnest;

		
		
	begin -- query_items
		
		-- Iterate all conductor layers starting at the bottom layer and ending
		-- with the top layer:
		for ly in reverse top_layer .. bottom_layer loop

			-- Draw the layer only if it is enabled. Otherwise skip the layer:
			if conductor_enabled (ly) then
				
				-- Set the layer being drawn:
				current_layer := ly;
				--put_line (to_string (current_layer));

				-- set color according to layer
				set_color_conductor (context.cr, current_layer, brightness);

				
				-- freetracks, floating stuff:
				is_signal := false;
				iterate (module.board.conductors.lines, query_line'access);
				iterate (module.board.conductors.arcs, query_arc'access);
				iterate (module.board.conductors.circles, query_circle'access);
				iterate (module.board.conductors.fill_zones.solid, query_fill_zone'access);
				iterate (module.board.conductors.fill_zones.hatched, query_fill_zone'access);
				iterate (module.board.conductors.cutouts, query_cutout'access);

				-- texts
				iterate (module.board.conductors.placeholders, query_placeholder'access);
				iterate (module.board.conductors.texts, query_text'access);

				-- tracks:
				iterate (module.nets, query_net_track'access);

				
				draw_text_being_placed_in_conductors (
					self, area, LAYER_CAT_CONDUCTOR, ly);
				
			end if;
		end loop;

		-- draw unrouted stuff (airwires)
		draw_ratsnest;
		
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
		position	=> current_active_module,
		process		=> query_items'access);

	draw_via_being_placed;
	
end draw_conductors;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
