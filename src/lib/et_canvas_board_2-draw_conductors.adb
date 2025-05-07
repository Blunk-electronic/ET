------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW CONDUCTORS                           --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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

with et_conductor_segment.boards;	use et_conductor_segment.boards;
with et_fill_zones;					use et_fill_zones;
with et_fill_zones.boards;			use et_fill_zones.boards;
with et_conductor_text.boards;		use et_conductor_text.boards;
with et_vias;						use et_vias;
use et_vias.pac_vias;

with et_nets;						use et_nets;

with et_thermal_relief;				use et_thermal_relief;
with et_pcb_stack;					use et_pcb_stack;
with et_design_rules_board;			use et_design_rules_board;
with et_display.board;				use et_display.board;
with et_colors;						use et_colors;

with et_board_ops.text;

with et_modes.board;				use et_modes.board;

with et_net_names;
with et_net_class;

with et_ratsnest;
with et_alignment;
with et_canvas_tool;
with et_mirroring;
with et_pcb_placeholders;			use et_pcb_placeholders;


separate (et_canvas_board_2)

procedure draw_conductors is

	use et_pcb;
	use et_canvas_board_preliminary_object;


	
	-- This procedure draws the text that is being placed in a
	-- conductor layer.
	-- The properties are taken from variable 
	-- et_canvas_board_texts.preliminary_text.
	-- The verb must be VERB_PLACE and the noun must be NOUN_TEXT. 
	-- Otherwise nothing happens here:
	procedure draw_text_being_placed (
		layer : in et_pcb_stack.type_signal_layer)
	is 
		use et_canvas_board_texts;
		use pac_draw_text;

		point : type_vector_model;

		use et_text;
	begin
		-- put_line ("draw_text_being_placed");
		
		if verb = VERB_PLACE and noun = NOUN_TEXT then
			
			if object_layer_category = LAYER_CAT_CONDUCTOR 
			and object_signal_layer = layer then

				-- Set the point where the text is to be drawn
				-- while the operator is moving the tool:
				point := get_primary_tool_position;

				preliminary_text.text.position := 
					type_position (to_position (point, zero_rotation));

				-- Draw the text:
				draw_vector_text (preliminary_text.text);
			end if;
		end if;
	end draw_text_being_placed;


	

	use et_net_names;
	use et_net_class;
	
	use pac_nets;

	use et_board_coordinates.pac_polygons;
	
	use pac_conductor_lines;
	use pac_conductor_arcs;
	use pac_conductor_circles;
	use pac_cutouts;
	use pac_floating_solid;
	use pac_floating_hatched;
	use pac_route_solid;
	use pac_route_hatched;
	
	use pac_text_placeholders_conductors;
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
		et_board_ops.get_deepest_conductor_layer (active_module);

	
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
		line : type_conductor_line renames element (c);

		procedure draw is begin
			draw_line (line => line, width => line.width, do_stroke => true);
		end draw;
		
	begin
		-- Draw the line if it is in the current layer:
		if get_layer (c) = current_layer then
			
			-- If the segment is selected, then it must be drawn highlighted:
			if is_selected (line) then
				set_highlight_brightness;
				draw;
				set_default_brightness;
			else
				draw;
			end if;

		end if;
	end query_line;



	
	
	procedure query_arc (c : in pac_conductor_arcs.cursor) is 
		arc : type_conductor_arc renames element (c);

		procedure draw is begin
			draw_arc (arc => arc, width => arc.width, do_stroke => true);
		end draw;

	begin
		-- Draw the arc if it is in theh current layer:
		if get_layer (c) = current_layer then

			-- If the segment is selected, then it must be drawn highlighted:
			if is_selected (arc) then
				set_highlight_brightness;
				draw;
				set_default_brightness;
			else
				draw;
			end if;

		end if;
	end query_arc;


	
	
	procedure query_circle (c : in pac_conductor_circles.cursor) is 
		circle : type_conductor_circle renames element (c);
	begin
		-- Draw the circle if it is in the current layer:
		if get_layer (c) = current_layer then
			
			-- We draw a normal non-filled circle:
			draw_circle (
				circle		=> circle,
				filled		=> NO,
				width		=> circle.width,
				do_stroke	=> true);

		end if;
	end query_circle;



	
-- CONDUCTOR FILL ZONES
	
	
	use pac_islands;



	
	procedure query_island (i : in pac_islands.cursor) is
		use pac_geometry_1;
		
		use pac_edges;
		use pac_lakes;
		use pac_stripes;

		island : type_island renames element (i);

		
		procedure draw_edge (e : in pac_edges.cursor) is 
			edge : type_edge renames element (e);
		begin
			draw_line (
				line	=> to_line_coarse (edge),
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
				line	=> to_line_coarse (stripe),
				width	=> 0.0); -- don't care
		end draw_stripe;
		
	begin
		island.shore.centerline.edges.iterate (draw_edge'access);
		island.lakes.iterate (query_lake'access);
		island.stripes.iterate (draw_stripe'access);
	end query_island;



	
	
	procedure query_fill_zone (c : in pac_floating_solid.cursor) is 
		zone : type_floating_solid renames element (c);
		use pac_draw_contours;
	begin
		-- Draw the zone if it is in the current layer:
		if zone.properties.layer = current_layer then

			-- NOTE: Because this is merely the contour of the zone
			-- it will not be filled:
			
			draw_contour (
				contour	=> zone,
				style	=> DASHED,
				filled	=> NO, 
				width	=> zone.linewidth);
   
			-- All edges of islands and their fill lines 
			-- have the same linewidth:
			set_linewidth (zone.linewidth);
			
			iterate (zone.islands, query_island'access);
			stroke;

		end if;
	end query_fill_zone;


	

	
	procedure query_fill_zone (c : in pac_floating_hatched.cursor) is 
		zone : type_floating_hatched renames element (c);
		use pac_draw_contours;
	begin
		-- Draw the zone if it is in the current layer:
		if zone.properties.layer = current_layer then

			-- NOTE: Because this is merely the contour of the zone
			-- it will not be filled:
			
			draw_contour (
				contour	=> zone,
				style	=> DASHED,
				filled	=> NO, 
				width	=> zone.linewidth);
   
			-- All edges of islands and their fill lines 
			-- have the same linewidth:
			set_linewidth (zone.linewidth);
			
			iterate (zone.islands, query_island'access);
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
		use pac_reliefes;
		use pac_draw_contours;
	begin
		-- Draw the zone if it is in the current layer:
		if zone.properties.layer = current_layer then

			-- NOTE: Because this is merely the contour of the zone
			-- it will not be filled:
			
			draw_contour (
				contour	=> zone,
				style	=> DASHED,
				filled	=> NO, 
				width	=> zone.linewidth);
   
			-- All edges of islands and their fill lines 
			-- have the same linewidth:
			set_linewidth (zone.linewidth);
			iterate (zone.islands, query_island'access);

			if zone.connection = THERMAL then
				iterate (zone.reliefes, query_relief'access);
			end if;

			stroke;
		end if;
	end query_fill_zone;

	

	
	
	procedure query_fill_zone (c : in pac_route_hatched.cursor) is
		zone : type_route_hatched renames element (c);
		use pac_reliefes;
		use pac_draw_contours;
	begin		
		-- Draw the zone if it is in the current layer:
		if zone.properties.layer = current_layer then

			-- NOTE: Because this is merely the contour of the zone
			-- it will not be filled:
			
			draw_contour (
				contour	=> zone,
				style	=> DASHED,
				filled	=> NO, 
				width	=> zone.linewidth);
   
			-- All edges of islands and their fill lines 
			-- have the same linewidth:
			set_linewidth (zone.linewidth);
			iterate (zone.islands, query_island'access);

			if zone.connection = THERMAL then
				iterate (zone.reliefes, query_relief'access);
			end if;

			stroke;
		end if;
	end query_fill_zone;



	
	
	procedure query_cutout (c : in pac_cutouts.cursor) is 
		cutout : type_cutout renames element (c);
		use pac_draw_contours;
	begin
		-- Draw the zone if it is in the current layer:
		if cutout.layer = current_layer then

			-- CS
			--save (context.cr);
			--set_color_background (context.cr);

			-- NOTE: Because this is merely the contour of the zone
			-- it will not be filled:
			
			draw_contour (
				contour	=> cutout,
				style	=> DASHED,
				filled	=> NO, 
				width	=> zero);
   
		end if;
	end query_cutout;




	
-- 	TEXT PLACEHOLDERS AND TEXTS:
	
	procedure query_placeholder (
		c : in pac_text_placeholders_conductors.cursor) 
	is 
		use et_board_ops.text;
		use pac_text;		
		use pac_draw_text;
		use et_colors.board;

		use et_text;
		content : pac_text_content.bounded_string;

		t : type_text_fab_with_content;
	begin
		-- Draw the placeholder if it is in the current layer:
		if get_layer (c) = current_layer then

			-- Build the final content to be drawn:
			content := to_placeholder_content (active_module, element (c).meaning);
			-- put_line ("content " & to_string (content));

			-- Build the text to be drawn:
			t := (type_text_fab (element (c)) with content);

			-- Draw the placeholder highlighted if it is selected:
			if is_selected (c) then
				set_color_conductor (current_layer, BRIGHT);				
				draw_vector_text (t);
				set_color_conductor (current_layer, NORMAL);
			else
				-- not selected
				draw_vector_text (t);
			end if;
			
		end if;
	end query_placeholder;

	



	procedure query_text (c : in pac_conductor_texts.cursor) is
		use pac_draw_text;
		use et_colors.board;
	begin
		-- Draw the text if it is in the current layer:
		if get_layer (c) = current_layer then

			if is_selected (c) then
				-- The selected text must be drawn highlighted:
				set_color_conductor (current_layer, BRIGHT);

				draw_vector_text (element (c));

				-- After drawing a selected (highlighted) text, the brightness
				-- must be set back to normal:
				set_color_conductor (current_layer, NORMAL);

			else -- not selected
				draw_vector_text (element (c));
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
		
		iterate (element (n).route.zones.solid, query_fill_zone'access);
		iterate (element (n).route.zones.hatched, query_fill_zone'access);

		-- user defined cutout areas:
		-- CS iterate (element (n).route.cutouts, query_cutout'access);
	end query_net_track;



	
-- VIAS
	
	procedure query_via (via_cursor : in pac_vias.cursor) is 
		via : type_via renames element (via_cursor);

		brightness : type_brightness := NORMAL;
		
		-- When the restring is to be drawn then
		-- we just use a circle with a certain linewidth.
		-- The center of the circle is the position
		-- of the via.
		-- If the via is being moved, then the center will
		-- be overwritten by the tool position:
		circle : type_circle;
		linewidth : type_distance_positive;
		
		radius_base : type_distance_positive;


		-- This procedure sets the linewidth
		-- and radius of the circle to be drawn:
		procedure set_width_and_radius (
			r : in type_restring_width) 
		is begin
			linewidth := r;
			set_radius (circle, (radius_base + r / 2.0));
		end set_width_and_radius;


		
		-- This procedure draws the restring using
		-- the circle as described above:
		procedure draw_restring is 
			use et_colors.board;
		begin
			set_color_via_restring (brightness);
			
			draw_circle (
				circle		=> circle,
				filled		=> NO,
				width		=> linewidth,
				do_stroke	=> true);			
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
			use et_alignment;

			use et_net_names;
			use pac_net_name;
			
			position : type_vector_model := get_center (circle);

			use pac_draw_text;
		begin
			if not net_name_drawn then
				
				-- The net name is displayed in a special color:
				set_color_via_net_name;

				draw_text (
					content		=> to_content (to_string (net_name)),
					size		=> via.diameter * ratio_diameter_to_text_size,
					font		=> via_text_font,
					anchor		=> position,
					origin		=> false,
					rotation	=> zero_rotation,
					alignment	=> (ALIGN_CENTER, ALIGN_CENTER));

				net_name_drawn := true;
			end if;
		end draw_net_name;

		
		
		-- Draws the layer numbers above the net name.
		-- The text size is set automatically with the radius of the drill:
		procedure draw_numbers (from, to : in string) is 
			use et_colors.board;
			use et_text;
			use et_alignment;
			position : type_vector_model := get_center (circle);
			offset : constant type_distance_relative := to_distance_relative (
				set (zero, + radius_base * text_position_layer_and_drill_factor));

			use pac_draw_text;
		begin
			move_by (position, offset);
			
			-- The layer numbers are displayed in a special color:
			set_color_via_layers;

			draw_text (
				content		=> to_content (from & "-" & to),
				size		=> via.diameter * ratio_diameter_to_text_size,
				font		=> via_text_font,
				anchor		=> position,
				origin		=> false,
				rotation	=> zero_rotation,
				alignment	=> (ALIGN_CENTER, ALIGN_CENTER));
			
		end draw_numbers;


		
		-- Draws the drill size below the net name.
		-- The text size is set automatically with the radius of the drill:
		procedure draw_drill_size is 
			use et_colors.board;
			use et_text;
			use et_alignment;
			position : type_vector_model := get_center (circle);
			offset : type_distance_relative;

			use pac_draw_text;
		begin
			if not drill_size_drawn then

				offset := to_distance_relative (
					set (zero, - radius_base * text_position_layer_and_drill_factor));
				
				move_by (position, offset);
						
				-- The drill size is displayed in a special color:
				set_color_via_drill_size; -- CS

				draw_text (
					content		=> to_content (to_string (via.diameter)),
					size		=> via.diameter * ratio_diameter_to_text_size,
					font		=> via_text_font,
					anchor		=> position,
					origin		=> false,
					rotation	=> zero_rotation,
					alignment	=> (ALIGN_CENTER, ALIGN_CENTER));

				drill_size_drawn := true;
			end if;
		end draw_drill_size;
		

		
		-- Depening on the category of the via, the order in
		-- which things are to be drawn differs:
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

			
			procedure through_hole_via is begin
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
			end through_hole_via;


			procedure buried_via is begin
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
			end buried_via;
			

			procedure blind_via_from_top is begin
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
			end blind_via_from_top;


			procedure blind_via_from_bottom is begin
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
			end blind_via_from_bottom;
			
			
		begin
			case via.category is
				when THROUGH =>
					through_hole_via;
					
				when BURIED =>
					buried_via;
					
				when BLIND_DRILLED_FROM_TOP =>
					blind_via_from_top;

				when BLIND_DRILLED_FROM_BOTTOM =>
					blind_via_from_bottom;
					
			end case;
		end query_category;


		use et_canvas_board_vias;
		use et_canvas_tool;
		
		
	begin -- query_via
		--put_line ("via.diameter" & to_string (via.diameter));

		-- Set the radius and the center of the circle:
		radius_base := via.diameter / 2.0;
		set_center (circle, via.position);

		-- Overwrite the via position (circle.center) if the
		-- via is selected and being moved:
		if is_selected (via_cursor) then

			-- A selected via must be highlighted:
			brightness := BRIGHT;

			if is_moving (via_cursor) then
				case object_tool is
					when MOUSE =>
						set_center (circle, snap_to_grid (get_mouse_position));

					when KEYBOARD =>
						set_center (circle, get_cursor_position);
				end case;
			end if;

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
		module		: in type_generic_module) 
	is
		use et_colors.board;
		

		-- Draw airwires:
		procedure draw_ratsnest is
			use et_ratsnest;

			
			procedure query_net (n : in pac_nets.cursor) is 
				use pac_airwires;
				
				procedure query_airwire (c : in pac_airwires.cursor) is 
					use et_canvas_board_tracks;
					airwire : type_airwire renames element (c);
					restore_brightness : boolean := false;
				begin
					-- If the candidate airwire is selected, then draw it highlighted:
					if is_selected (c) then
						set_color_ratsnest (BRIGHT);
						restore_brightness := true;
					end if;

					 -- put_line (to_string (c));
					
					draw_line (
						line		=> to_line_coarse (airwire),
						width		=> 0.0, -- use minimal linewidth
						do_stroke	=> true);
					
					-- restore normal brightness
					if restore_brightness then
						set_color_ratsnest;
					end if;
				end query_airwire;


			begin
				-- put_line ("draw ratsnest net " & to_string (n));				
				
				if not element (n).route.airwires.hidden then
					iterate (element (n).route.airwires.lines, query_airwire'access);
				end if;
			end query_net;

			
		begin
			if ratsnest_enabled then
				
				-- All airwires of all nets are drawn with the same color:
				set_color_ratsnest;
				
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
				set_color_conductor (current_layer, brightness);

				
				-- freetracks, floating stuff:
				is_signal := false;
				iterate (module.board.conductors_floating.lines, query_line'access);
				iterate (module.board.conductors_floating.arcs, query_arc'access);
				iterate (module.board.conductors_floating.circles, query_circle'access);
				iterate (module.board.conductors_floating.zones.solid, query_fill_zone'access);
				iterate (module.board.conductors_floating.zones.hatched, query_fill_zone'access);
				iterate (module.board.conductors_floating.cutouts, query_cutout'access);

				-- texts
				iterate (module.board.conductors_floating.placeholders, query_placeholder'access);
				iterate (module.board.conductors_floating.texts, query_text'access);

				-- tracks:
				iterate (module.nets, query_net_track'access);
				
				draw_text_being_placed (ly);				
			end if;
		end loop;

		-- draw unrouted stuff (airwires)
		draw_ratsnest;
		
		-- Draw the vias that exist in the nets:
		iterate (module.nets, query_net_via'access);
	end query_items;


	

	
	-- Draws the via that is being placed with the
	-- properties according to variable preliminary_via:
	procedure draw_via_being_placed is 
		use et_canvas_board_vias;

		-- This list will contain only the single via
		-- that is being placed:
		vias_being_placed : pac_vias.list;
		
		-- The place where the via shall be placed:
		position : type_vector_model;
	begin
		-- put_line ("draw_via_being_placed");
		if verb = VERB_PLACE and noun = NOUN_VIA then

			-- Set the point where the via is to be drawn:
			position := get_primary_tool_position;

			-- Get the name of the targeted net:
			net_name := object_net_name;
			
			case preliminary_via.category is
				when THROUGH =>
					append (
						container	=> vias_being_placed,
						new_item	=> (
							category		=> THROUGH,
							diameter		=> preliminary_via.drill.diameter,
							position		=> position,
							restring_inner	=> preliminary_via.restring_inner,
							restring_outer	=> preliminary_via.restring_outer,
							others			=> <>));

				when BLIND_DRILLED_FROM_TOP =>
					append (
						container	=> vias_being_placed,
						new_item	=> (
							category		=> BLIND_DRILLED_FROM_TOP,
							diameter		=> preliminary_via.drill.diameter,
							position		=> position,
							restring_inner	=> preliminary_via.restring_inner,
							restring_top	=> preliminary_via.restring_outer,
							lower			=> preliminary_via.destination_blind,
							others			=> <>));
					
				when BLIND_DRILLED_FROM_BOTTOM =>
					append (
						container	=> vias_being_placed,
						new_item	=> (
							category		=> BLIND_DRILLED_FROM_BOTTOM,
							diameter		=> preliminary_via.drill.diameter,
							position		=> position,
							restring_inner	=> preliminary_via.restring_inner,
							restring_bottom	=> preliminary_via.restring_outer,
							upper			=> preliminary_via.destination_blind,
							others			=> <>));
					
				when BURIED =>
					append (
						container	=> vias_being_placed,
						new_item	=> (
							category		=> BURIED,
							diameter		=> preliminary_via.drill.diameter,
							position		=> position,
							layers			=> preliminary_via.layers_buried,
							restring_inner	=> preliminary_via.restring_inner,
							others			=> <>));
					
			end case;

			-- draw the single via that is in container vias_being_placed:
			vias_being_placed.iterate (query_via'access); 
		end if;
	end draw_via_being_placed;



	-- Draws a conducting track path being drawn.
	-- Uses the parameters in global variable live_path.
	-- Computes the bend point (if required) of live_path
	-- and sets it accordingly:
	procedure draw_track is
		use et_canvas_board_tracks;

		-- Computes the path from given start to given end point.
		-- Takes the bend style into account. Draws the path:
		procedure compute_and_draw (
			A, B : in type_vector_model) 
		is
			use pac_path_and_bend;
			use et_colors.board;
			
			line : type_line;

			-- Do the actual path calculation.
			path : constant type_path := to_path (A, B, live_path.bend_style);

			-- Draws the line:
			procedure draw is begin
				draw_line (
					line	=> line,
					width	=> 0.0); -- don't care
			end draw;

			
		begin
			-- The calculated path may require a bend point.
			-- Set/clear the "bended" flag of the line being drawn.
			live_path.bended := path.bended;

			-- set linewidth:			
			set_linewidth (object_linewidth);

			-- Set the color according to the current signal layer:
			set_color_conductor (object_signal_layer, NORMAL);

			
			-- If the path does not require a bend point, draw a single line
			-- from start to end point:
			if path.bended = NO then
				
				line.A := path.A;
				line.B := path.B;

				draw;

			-- If the path DOES require a bend point, then draw first a line
			-- from start point to bend point. Then draw a second line from
			-- bend point end point:
			else
				live_path.bend_point := path.bend_point;

				line.A := path.A;
				line.B := path.bend_point;
				
				draw;

				line.A := path.bend_point;
				line.B := path.B;
				
				draw;				
			end if;

			stroke;
		end compute_and_draw;


		use et_canvas_tool;
		
	begin
		-- Draw the path only after the actual editing process has started:
		if verb = VERB_ROUTE and noun = NOUN_NET and edit_process_running then

			compute_and_draw (
				A	=> live_path.A,	-- start of path
				B	=> get_object_tool_position);	-- end of route

		end if;
	end draw_track;

	
	
begin
	-- put_line ("draw conductors ...");

	-- Draw objects that already exist in the
	-- database, such as tracks, vias, airwires:
	pac_generic_modules.query_element (
		position	=> active_module,
		process		=> query_items'access);


	-- Draw a via that is being placed. 
	-- If none is being placed,
	-- nothing happens:
	draw_via_being_placed;


	-- Draw a freetrack being drawn. 
	-- If no freetrack is being drawn,
	-- nothing happens:
    draw_path (LAYER_CAT_CONDUCTOR);


	-- Draw a track that is being drawn. 
	-- If none is being drawn, nothing happens.
	-- This is about a track that is connected to a net:
	draw_track;
	
end draw_conductors;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
