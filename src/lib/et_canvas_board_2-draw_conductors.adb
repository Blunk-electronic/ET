------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW CONDUCTORS                           --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
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

with et_geometry;

with et_pcb;						use et_pcb;
with et_conductor_segment.boards;	use et_conductor_segment.boards;
with et_fill_zones;					use et_fill_zones;
with et_fill_zones.boards;			use et_fill_zones.boards;
with et_conductor_text.boards;		use et_conductor_text.boards;
with et_vias;						use et_vias;
use et_vias.pac_vias;

with et_nets;						use et_nets;

with et_thermal_relief;				use et_thermal_relief;
with et_pcb_stack;					use et_pcb_stack;
with et_design_rules;				use et_design_rules;
with et_display.board;				use et_display.board;
with et_colors;						use et_colors;

with et_canvas_board_tracks;		use et_canvas_board_tracks;
with et_board_ops.text;

with et_modes.board;				use et_modes.board;

with et_schematic;
with et_net_names;

with et_ratsnest;

with et_canvas_tool;



separate (et_canvas_board_2)

procedure draw_conductors is

	-- This procedure draws the text that is being placed in a
	-- conductor layer.
	-- The properties are taken from variable et_canvas_board_texts.preliminary_text.
	-- The verb must be VERB_PLACE and the noun must be NOUN_TEXT. 
	-- Otherwise nothing happens here:
	procedure draw_text_being_placed_in_conductors (
		layer : in et_pcb_stack.type_signal_layer)
	is 
		use et_pcb;
		use et_pcb_stack;

		use et_text;
		use pac_text;

		use et_canvas_board_texts;
		use et_board_shapes_and_text;
		
		v_text : type_vector_text;

		mirror : type_vector_text_mirrored;
		
		-- The place where the text shall be placed:
		point : type_vector_model;

		-- The place where the text origin will be drawn:
		origin : type_position;
	begin
		if verb = VERB_PLACE and noun = NOUN_TEXT and preliminary_text.ready then
			
			if preliminary_text.category = LAYER_CAT_CONDUCTOR 
			and preliminary_text.signal_layer = layer then

				-- Set the point where the text is to be drawn:
				-- CS point := canvas.tool_position;

				-- Draw the origin of the text:
				origin := type_position (to_position (point, zero_rotation));
				draw_origin (origin);

				-- Set the line width of the vector text:
				set_linewidth (preliminary_text.text.line_width);

				mirror := signal_layer_to_mirror (layer, deepest_conductor_layer (current_active_module));

				
				-- Vectorize the text on the fly:
				v_text := vectorize_text (
					content		=> preliminary_text.text.content,
					size		=> preliminary_text.text.size,
					rotation	=> get_rotation (preliminary_text.text.position),
					position	=> point,
					mirror		=> mirror,
					line_width	=> preliminary_text.text.line_width,
					alignment	=> preliminary_text.text.alignment -- right, bottom
					);

				-- Draw the text:
				draw_vector_text (v_text, preliminary_text.text.line_width);
			end if;
		end if;
	end draw_text_being_placed_in_conductors;


	
	use et_schematic;
	use et_net_names;
	
	use pac_nets;

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


	procedure set_default_brightness is 
		use et_colors.board;
	begin
		set_color_conductor (current_layer, NORMAL);
	end set_default_brightness;

	
	procedure set_highlight_brightness is 
		use et_colors.board;
	begin
		set_color_conductor (current_layer, BRIGHT);
	end set_highlight_brightness;
	

	
-- LINES, ARCS, CIRCLES
	
	procedure query_line (c : in pac_conductor_lines.cursor) is
		use et_canvas_tool;
		
		line : type_conductor_line renames element (c);

		procedure draw_unchanged is begin
			draw_line (line => line, width => line.width, do_stroke => true);
		end draw_unchanged;

	begin
		-- Draw the line if it is in the current layer:
		if line.layer = current_layer then
			
			-- If the segment is selected, then it must be drawn highlighted:
			if is_selected (line) then
				set_highlight_brightness;

				case verb is
					-- If the segment is being moved, then a temporarily
					-- segment must be drawn instead of the original one:
					when VERB_MOVE =>
						if preliminary_segment.ready then
							declare
								line_tmp : type_conductor_line := line;
								POA : type_vector_model renames preliminary_segment.point_of_attack;
							begin
								case preliminary_segment.tool is
									when MOUSE =>
										move_line_to (line_tmp, POA, snap_to_grid (get_mouse_position));

									when KEYBOARD =>
										move_line_to (line_tmp, POA, get_cursor_position);
								end case;

								draw_line (line => line_tmp, width => line.width, do_stroke => true);
							end;
						else
							draw_unchanged;
						end if;

						
					when VERB_RIPUP =>
						if preliminary_segment.ready then
							draw_line (line => line, width => line.width, do_stroke => true);
						else
							draw_unchanged;
						end if;
						
					when others =>
						draw_unchanged;
						
				end case;

				set_default_brightness;
			else
				draw_unchanged;
			end if;

		end if;
	end query_line;

	
	
	procedure query_arc (c : in pac_conductor_arcs.cursor) is 
		arc : type_conductor_arc renames element (c);
	begin
		-- Draw the arc if it is in theh current layer:
		if arc.layer = current_layer then

			draw_arc (
				arc			=> arc,
				width		=> arc.width,
				do_stroke	=> true);

		end if;
	end query_arc;

	
	procedure query_circle (c : in pac_conductor_circles.cursor) is 
		circle : type_conductor_circle renames element (c);
	begin
		-- Draw the circle if it is in the current layer:
		if circle.layer = current_layer then
			
			-- We draw a normal non-filled circle:
			draw_circle (
				circle		=> circle,
				filled		=> et_geometry.NO,
				width		=> circle.width,
				do_stroke	=> true);

		end if;
	end query_circle;



	
-- CONDUCTOR FILL ZONES
	
	-- The width of the borders and stripes of a fill zone:
	fill_line_width : type_track_width;
	
	use pac_islands;

	
	procedure query_island (i : in pac_islands.cursor) is
		use pac_geometry_1;
		
		use pac_edges;
		use pac_lakes;
		use pac_stripes;

		island : type_island renames element (i);

		
		procedure draw_edge (e : in pac_edges.cursor) is 
			edge : type_edge renames element (e);
			use et_geometry;
		begin
			draw_line (
				line	=> to_line_coarse (type_line_fine (edge)),
				width	=> 0.0, -- don't care
				style	=> DASHED);
		end draw_edge;

		
		procedure query_lake (l : in pac_lakes.cursor) is begin
			element (l).centerline.edges.iterate (draw_edge'access);
		end query_lake;

		
		procedure draw_stripe (s : in pac_stripes.cursor) is 
			stripe : type_line_fine renames element (s);
		begin
			draw_line (
				line	=> to_line_coarse (element (s)),
				width	=> 0.0); -- don't care
		end draw_stripe;
		
	begin
		island.shore.centerline.edges.iterate (draw_edge'access);
		island.lakes.iterate (query_lake'access);
		island.stripes.iterate (draw_stripe'access);
	end query_island;


	
	
	procedure query_fill_zone (c : in pac_floating_solid.cursor) is 
		-- CS use rename ?
		use pac_draw_contours;
		use et_geometry;
	begin
		-- Draw the zone if it is in the current layer:
		if element (c).properties.layer = current_layer then

			-- NOTE: Because this is merely the contour of the zone
			-- it will not be filled:
			
			draw_contour (
				contour	=> element (c),
				style	=> DASHED,
				filled	=> NO, 
				width	=> element (c).linewidth); -- CS should be the dynamically calculated width of the contours
   
			-- All edges and fill lines have the same linewidth:
			set_linewidth (element (c).linewidth);
			iterate (element (c).islands, query_island'access);
			stroke;

		end if;
	end query_fill_zone;


	
	procedure query_fill_zone (c : in pac_floating_hatched.cursor) is 
		-- CS use rename ?
		use pac_draw_contours;
		use et_geometry;
	begin
		-- Draw the zone if it is in the current layer:
		if element (c).properties.layer = current_layer then

			-- NOTE: Because this is merely the contour of the zone
			-- it will not be filled:
			
			draw_contour (
				contour	=> element (c),
				style	=> DASHED,
				filled	=> NO, 
				width	=> element (c).linewidth); -- CS should be the dynamically calculated width of the contours
   
			-- All edges and fill lines have the same linewidth:
			set_linewidth (element (c).linewidth);
			iterate (element (c).islands, query_island'access);
			stroke;

		end if;
	end query_fill_zone;



	procedure query_relief (c : in pac_reliefes.cursor) is
		use pac_geometry_1;

		use pac_reliefes;
		use pac_spokes;
		
		relief : type_relief renames element (c);

		procedure query_spoke (s : in pac_spokes.cursor) is 
			spoke : type_line_fine renames element (s);
		begin
			draw_line (
				line	=> to_line_coarse (spoke),
				width	=> 0.0);  -- don't care
		end query_spoke;
		
	begin
		iterate (relief.spokes, query_spoke'access);
	end query_relief;

	
	
	procedure query_fill_zone (c : in pac_route_solid.cursor) is 
		zone : type_route_solid renames element (c);

		use et_geometry;
		use pac_reliefes;
		use pac_draw_contours;
	begin
		-- Draw the zone if it is in the current layer:
		if zone.properties.layer = current_layer then

			-- NOTE: Because this is merely the contour of the zone
			-- it will not be filled:
			
			draw_contour (
				contour	=> element (c),
				style	=> DASHED,
				filled	=> NO, 
				width	=> element (c).linewidth); -- CS should be the dynamically calculated width of the contours
   
			-- All edges and fill lines have the same linewidth:
			set_linewidth (element (c).linewidth);
			iterate (element (c).islands, query_island'access);

			if zone.connection = THERMAL then
				iterate (zone.reliefes, query_relief'access);
			end if;

			stroke;
		end if;
	end query_fill_zone;


	
	procedure query_fill_zone (c : in pac_route_hatched.cursor) is
		zone : type_route_hatched renames element (c);

		use et_geometry;
		use pac_reliefes;
		use pac_draw_contours;
	begin		
		-- Draw the zone if it is in the current layer:
		if zone.properties.layer = current_layer then

			-- NOTE: Because this is merely the contour of the zone
			-- it will not be filled:
			
			draw_contour (
				contour	=> element (c),
				style	=> DASHED,
				filled	=> NO, 
				width	=> element (c).linewidth); -- CS should be the dynamically calculated width of the contours
   
			-- All edges and fill lines have the same linewidth:
			set_linewidth (element (c).linewidth);
			iterate (element (c).islands, query_island'access);

			if zone.connection = THERMAL then
				iterate (zone.reliefes, query_relief'access);
			end if;

			stroke;
		end if;
	end query_fill_zone;


	
	procedure query_cutout (c : in pac_cutouts.cursor) is 
		-- CS use rename ?
		use et_geometry;
		use pac_draw_contours;
	begin
		-- Draw the zone if it is in the current layer:
		if element (c).layer = current_layer then

			-- CS
			--save (context.cr);
			--set_color_background (context.cr);

			-- NOTE: Because this is merely the contour of the zone
			-- it will not be filled:
			
			draw_contour (
				contour	=> element (c),
				style	=> DASHED,
				filled	=> NO, 
				width	=> zero);
   
		end if;
	end query_cutout;



-- TEXTS
	
	procedure query_placeholder (c : in et_pcb.pac_text_placeholders_conductors.cursor) is 
		use et_board_ops.text;
		use pac_text;
		v_text : type_vector_text;

		-- CS use rename ?
	begin
		-- Draw the placeholder if it is in the current layer:
		if element (c).layer = current_layer then

			draw_origin (element (c).position);

			-- Set the line width of the vector text:
			set_linewidth (element (c).line_width);

			-- Vectorize the text:
			v_text := vectorize_text (
				content		=> to_placeholder_content (current_active_module, element (c).meaning),
				size		=> element (c).size,
				rotation	=> get_rotation (element (c).position),
				position	=> element (c).position.place,

				-- Mirror the text only if it is in the bottom layer:
				mirror		=> signal_layer_to_mirror (element (c).layer, bottom_layer),
				
				line_width	=> element (c).line_width,
				alignment	=> element (c).alignment -- right, bottom
				);

			-- Draw the text:
			draw_vector_text (v_text, element (c).line_width);

		end if;
	end query_placeholder;


	
	procedure query_text (c : in pac_conductor_texts.cursor) is
		text : type_conductor_text renames element (c);

		-- Draws the given text as it is given:
		procedure draw_unchanged is begin
			draw_origin (text.position);

			-- Set the line width of the vector text:
			set_linewidth (text.line_width);
			draw_vector_text (text.vectors, text.line_width);
		end draw_unchanged;

		
		use et_canvas_board_texts;
		use et_colors.board;
		use et_canvas_tool;
		
	begin
		-- Draw the text if it is in the current layer:
		if text.layer = current_layer then

			if is_selected (c) then
				-- The selected text must be drawn highlighted:
				set_color_conductor (current_layer, BRIGHT);

				case verb is
					when VERB_MOVE =>
						if preliminary_text.ready then
							-- Draw a temporarily copy of the original text at
							-- the place where the tool is pointing at:
							declare
								text_tmp	: type_conductor_text := text;
								destination	: type_vector_model;
								offset		: type_distance_relative;
							begin
								case preliminary_text.tool is
									when MOUSE =>
										destination := snap_to_grid (get_mouse_position);
														
									when KEYBOARD =>
										destination := get_cursor_position;
								end case;

								-- Get the relative distance of the destination to the original
								-- text position:
								offset := get_distance_relative (get_place (text_tmp), destination);

								-- Move the text (incl. vector text):
								move_text (text_tmp, offset);					

								draw_origin (text_tmp.position);

								-- Set the line width of the vector text:
								set_linewidth (text_tmp.line_width);

								-- Draw the text:
								draw_vector_text (text_tmp.vectors, text_tmp.line_width);
							end;
						else
							draw_unchanged;
						end if;

					when others =>
						draw_unchanged;
						
				end case;

				-- After drawing a selected (highlighted) text, the brightness
				-- must be set back to normal:
				set_color_conductor (current_layer, NORMAL);

			else -- not selected
				draw_unchanged;
			end if;
			
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
	
	procedure query_via (via_cursor : in pac_vias.cursor) is 
		via : type_via renames element (via_cursor);

		brightness : type_brightness := NORMAL;
		
		circle : type_circle;

		radius_base : type_distance_positive;

		procedure set_width_and_radius (r : in type_restring_width) is begin
			set_linewidth (r);
			circle.radius := (radius_base + r / 2.0);
		end set_width_and_radius;

		
		procedure draw_restring is 
			use et_colors.board;
		begin
			set_color_vias (brightness);
			
			draw_circle (
				circle		=> circle,
				filled		=> et_geometry.NO,
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
			use et_colors.board;
			use et_text;

			use et_net_names;
			use pac_net_name;
			
			position : type_vector_model := circle.center;
		begin
			if not net_name_drawn then
				
				-- The net name is displayed in a special color:
				set_color_via_net_name;

				draw_text (
					content		=> to_content (to_string (net_name)),
					size		=> radius_base * text_size_factor,
					font		=> via_text_font,
					anchor		=> position,
					origin		=> false,
					rotation	=> zero_rotation,
					alignment	=> (center, center));

				net_name_drawn := true;
			end if;
		end draw_net_name;

		
		-- Draws the layer numbers above the net name.
		-- The text size is set automatically with the radius of the drill:
		procedure draw_numbers (from, to : in string) is 
			use et_colors.board;
			use et_text;
			position : type_vector_model := circle.center;
			offset : constant type_distance_relative := to_distance_relative (
				set (zero, + radius_base * text_position_layer_and_drill_factor));
		begin
			move_by (position, offset);
			
			-- The layer numbers are displayed in a special color:
			set_color_via_layers;

			draw_text (
				content		=> to_content (from & "-" & to),
				size		=> radius_base * text_size_factor,
				font		=> via_text_font,
				anchor		=> position,
				origin		=> false,
				rotation	=> zero_rotation,
				alignment	=> (center, center));
			
		end draw_numbers;

		
		-- Draws the drill size below the net name.
		-- The text size is set automatically with the radius of the drill:
		procedure draw_drill_size is 
			use et_colors.board;
			use et_text;
			position : type_vector_model := circle.center;
			offset : type_distance_relative;
		begin
			if not drill_size_drawn then

				offset := to_distance_relative (
					set (zero, - radius_base * text_position_layer_and_drill_factor));
				
				move_by (position, offset);
						
				-- The drill size is displayed in a special color:
				set_color_via_drill_size; -- CS

				draw_text (
					content		=> to_content (to_string (via.diameter)),
					size		=> radius_base * text_size_factor,
					font		=> via_text_font,
					anchor		=> position,
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
						to		=> to_string (via.lower));

					numbers_drawn := true;
				end if;
			end draw_numbers_blind_top;

			
			procedure draw_numbers_blind_bottom is begin
				-- Draw the layer numbers only once:
				if not numbers_drawn then
					draw_numbers (
						from	=> "B",
						to		=> to_string (via.upper));

					numbers_drawn := true;
				end if;
			end draw_numbers_blind_bottom;		

			
		begin -- query_category	
			case via.category is
				when THROUGH =>
					if is_inner_layer (current_layer) then
						-- current_layer is an inner layer
						set_width_and_radius (via.restring_inner);

						inner_restring_drawn := true;
					else
						-- current_layer is an outer layer
						set_width_and_radius (via.restring_outer);

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
					if via.layers.upper = current_layer 
					or via.layers.lower = current_layer then
						set_width_and_radius (via.restring_inner);
					
						draw_restring;

						-- Since the inner restring width is the same for all
						-- inner signal layers, it is sufficent to draw only one
						-- restring.
						cancel := true;  -- causes the layer iterator to cancel
					
						-- Draw the layer numbers only once (cancel flag already set)
						draw_numbers (
							from	=> to_string (via.layers.upper),
							to		=> to_string (via.layers.lower));
					
						draw_net_name;
						draw_drill_size;
					end if;
					
				when BLIND_DRILLED_FROM_TOP =>
					if current_layer = top_layer then
						set_width_and_radius (via.restring_top);
						outer_restring_drawn := true;
						draw_restring;
						draw_numbers_blind_top;
						draw_net_name;
						draw_drill_size;
					end if;

					if current_layer = via.lower then
						set_width_and_radius (via.restring_inner);
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
						set_width_and_radius (via.restring_bottom);
						outer_restring_drawn := true;
						draw_restring;
						draw_numbers_blind_bottom;
						draw_net_name;
						draw_drill_size;
					end if;

					if current_layer = via.upper then
						set_width_and_radius (via.restring_inner);
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


		use et_canvas_board_vias;
		use et_canvas_tool;
		
		
	begin -- query_via

		radius_base := via.diameter / 2.0;
		circle.center := via.position;


		-- Overwrite the via position (circle.center) if the
		-- via is selected and being moved:
		if via_is_selected (via_cursor, net_name) then

			-- A selected via must be highlighted:
			brightness := BRIGHT;

			case verb is
				when VERB_MOVE =>
					if preliminary_via.ready then

						case preliminary_via.tool is
							when MOUSE =>
								circle.center := snap_to_grid (get_mouse_position);

							when KEYBOARD =>
								circle.center := get_cursor_position;
						end case;

					end if;

				when others => null;
			end case;
		end if;

		
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
		use et_colors.board;
		

		-- Draw airwires:
		procedure draw_ratsnest is
			use et_ratsnest;

			
			procedure query_net (n : in pac_nets.cursor) is 
				use pac_airwires;
				
				procedure query_airwire (c : in pac_airwires.cursor) is 
					airwire : type_airwire renames element (c);
					restore_brightness : boolean := false;
					skip : boolean := false;

				begin
					-- If the candidate airwire is selected, then draw it highlighted:
					if airwire_is_selected (c, key (n)) then
						set_color_ratsnest (BRIGHT);
						restore_brightness := true;

						-- If a path is being drawn, then the selected
						-- airwire shall not be visible:
						if preliminary_track.ready then
							skip := true;
						end if;
					end if;

					if not skip then
						draw_line (
							line	=> to_line_coarse (pac_geometry_1.type_line_fine (airwire)),
							width	=> 0.0); -- don't care
					end if;
					
					-- restore normal brightness
					if restore_brightness then
						set_color_ratsnest;
					end if;

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
				set_color_ratsnest;
				set_linewidth (airwire_line_width);
				
				pac_nets.iterate (module.nets, query_net'access);

				stroke;
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
				set_color_conductor (current_layer, brightness);

				
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
				
				draw_text_being_placed_in_conductors (ly);				
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
		use et_canvas_board_vias;
		
		-- The place where the via shall be placed:
		position : type_vector_model;
	begin
		if preliminary_via.ready then

			-- Set the point where the via is to be drawn:
			-- CS position := canvas.tool_position;

			-- Get the name of the targeted net:
			net_name := preliminary_via.net_name;
			
			case preliminary_via.category is
				when THROUGH =>
					append (
						container	=> vias_being_placed,
						new_item	=> (
							category		=> THROUGH,
							diameter		=> preliminary_via.drill.diameter,
							position		=> position,
							restring_inner	=> preliminary_via.restring_inner,
							restring_outer	=> preliminary_via.restring_outer));

				when BLIND_DRILLED_FROM_TOP =>
					append (
						container	=> vias_being_placed,
						new_item	=> (
							category		=> BLIND_DRILLED_FROM_TOP,
							diameter		=> preliminary_via.drill.diameter,
							position		=> position,
							restring_inner	=> preliminary_via.restring_inner,
							restring_top	=> preliminary_via.restring_outer,
							lower			=> preliminary_via.destination_blind));
					
				when BLIND_DRILLED_FROM_BOTTOM =>
					append (
						container	=> vias_being_placed,
						new_item	=> (
							category		=> BLIND_DRILLED_FROM_BOTTOM,
							diameter		=> preliminary_via.drill.diameter,
							position		=> position,
							restring_inner	=> preliminary_via.restring_inner,
							restring_bottom	=> preliminary_via.restring_outer,
							upper			=> preliminary_via.destination_blind));
					
				when BURIED =>
					append (
						container	=> vias_being_placed,
						new_item	=> (
							category		=> BURIED,
							diameter		=> preliminary_via.drill.diameter,
							position		=> position,
							layers			=> preliminary_via.layers_buried,
							restring_inner	=> preliminary_via.restring_inner));
					
			end case;

			-- draw the single via that is in container vias_being_placed:
			vias_being_placed.iterate (query_via'access); 
		end if;
	end draw_via_being_placed;



	-- Draws a conducting track path being drawn.
	-- Uses the parameters in variable preliminary_track.
	-- Computes the bend point (if required) and sets it accordingly
	-- in preliminary_track.
	procedure draw_track is
		PT : type_preliminary_track renames preliminary_track;	


		-- Computes the path from given start to given end point.
		-- Takes the bend style given in preliminary_track into account.
		-- Draws the path.
		procedure compute_and_draw (
			start_point, end_point : in type_vector_model) 
		is
			use pac_path_and_bend;
			use et_colors.board;
			
			line : type_line;

			-- Do the actual path calculation.
			path : constant type_path := to_path (start_point, end_point, PT.path.bend_style);

			-- Draws the line:
			procedure draw is begin
				draw_line (
					line	=> line,
					width	=> 0.0); -- don't care
			end draw;

			
		begin
			-- The calculated path may require a bend point.
			-- Set/clear the "bended" flag of the line being drawn.
			PT.path.bended := path.bended;

			-- set linewidth:			
			set_linewidth (PT.width);

			-- Set the color according to the current signal layer:
			set_color_conductor (PT.signal_layer, NORMAL);

			
			-- If the path does not require a bend point, draw a single line
			-- from start to end point:
			if path.bended = NO then
				
				line.start_point := path.start_point;
				line.end_point := path.end_point;

				draw;

			-- If the path DOES require a bend point, then draw first a line
			-- from start point to bend point. Then draw a second line from
			-- bend point end point:
			else
				PT.path.bend_point := path.bend_point;

				line.start_point := path.start_point;
				line.end_point := path.bend_point;
				
				draw;

				line.start_point := path.bend_point;
				line.end_point := path.end_point;
				
				draw;				
			end if;

			stroke;
		end compute_and_draw;


		use et_canvas_tool;
		
	begin -- draw_track
		
		if verb = VERB_ROUTE and noun = NOUN_NET and PT.ready then
			case PT.tool is
				when MOUSE => 
					compute_and_draw (
						start_point	=> PT.path.start_point,	-- start of path
						end_point	=> snap_to_grid (get_mouse_position));	-- end of route
					
				when KEYBOARD =>
					compute_and_draw (
						start_point	=> PT.path.start_point,	-- start of path
						end_point	=> get_cursor_position);	-- end of path

			end case;
		end if;
	end draw_track;

	
	
begin -- draw_conductors
-- 	put_line ("draw conductor layers ...");
	
	pac_generic_modules.query_element (
		position	=> current_active_module,
		process		=> query_items'access);


	-- Draw a via that is being placed. If none is being placed,
	-- nothing happens:
	case verb is
		when VERB_PLACE =>
			draw_via_being_placed;

		when others => null;
	end case;


	-- Draw a freetrack being drawn. If no freetrack is being drawn,
	-- nothing happens:
	-- CS draw_path (LAYER_CAT_CONDUCTOR);


	-- This is about a track that is connected to a net:
	-- Draw a track that is being drawn. If none is being drawn, nothing happens:
	draw_track;
	
end draw_conductors;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
