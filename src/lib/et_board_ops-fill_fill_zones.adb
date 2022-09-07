------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                  BOARD OPERATIONS / FILLING FILL ZONES                   --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
------------------------------------------------------------------------------

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.strings;					use ada.strings;
with ada.exceptions;
with et_exceptions;					use et_exceptions;

with et_contour_to_polygon;			use et_contour_to_polygon;
with et_routing;					use et_routing;

separate (et_board_ops)

procedure fill_fill_zones (
	module_cursor	: in pac_generic_modules.cursor;
	log_category	: in type_log_category;
	log_threshold	: in type_log_level;
	nets 			: in pac_net_names.list := no_net_names)
is 
	use pac_geometry_brd;
	use pac_polygons;
	use pac_polygon_clipping;
	use pac_polygon_cropping;
	use pac_polygon_offsetting;
	use pac_polygon_union;
	
	use pac_net_names;

	all_zones : boolean;

	
	-- Get the design rules:
	design_rules : constant type_design_rules := get_pcb_design_rules (module_cursor);

	-- The deepest conductor layer towards bottom is defined by the layer stack:
	bottom_layer	: constant type_signal_layer := deepest_conductor_layer (module_cursor);


	use pac_islands;
	--use pac_cropped;
	--use pac_clipped;

	
	-- The outer contour of the board. After shrinking by the
	-- conductor-to-edge clearance this serves as master for
	-- filling zones of nets. Each net may have an individual setting for 
	-- the with of the fill lines.
	board_outer_contour_master : type_polygon;

	-- The holes inside the board area. After expanding by the
	-- conductor-to-edge clearance this serves as master for
	-- filling zones of nets. Each net may have an individual setting for 
	-- the with of the fill lines.
	use pac_polygon_list;
	board_holes_master : pac_polygon_list.list;

	-- The holes inside the board area after expanding 
	-- the board_holes_master by half the
	-- line width of a particular fill zone:
	holes : pac_polygon_list.list;


	
	-- Expands the board_holes_master by half the given line width
	-- and loads variable "holes" with the result:
	procedure expand_holes (
		line_width	: in type_track_width)
	is begin
		holes := board_holes_master;
		
		log (text => "expanding holes (" & count_type'image (holes.length) & ") ...", 
			 level => log_threshold + 4);
				
		-- Expand the holes by half the line width of the fill lines:
		offset_holes (holes, line_width * 0.5);

		multi_union (holes);
	end expand_holes;


	-- Crops the given zone by the outer board contours and the holes.
	-- Assumes that variable "holes" has been updated by procedure expand_holes:
	function zone_to_polygons (
		zone		: in type_polygon;
		line_width	: in type_track_width)
		return pac_polygon_list.list
	is		
		outer_contour : type_polygon := board_outer_contour_master;

		-- After clipping the zone by the outer board contours
		-- we get a list of islands:
		islands : pac_polygon_list.list;				
	begin
		log (text => "processing board contours ...", level => log_threshold + 4);
		log_indentation_up;
		
		-- Shrink the outer contour (of the board) by half the line 
		-- width of the zone border:
		log (text => "outer contour ...", level => log_threshold + 4);
		offset_polygon (outer_contour, - type_float_internal_positive (line_width) * 0.5);

		-- Clip the fill zone by the outer contour of the board.
		-- The zone may shrink or disintegrate into smaller islands:
		islands := clip (zone, outer_contour);

		-- Now we crop the islands by the holes.
		-- This adresses holes that
		-- - cause islands to shrink
		-- - cause a fragmentation of islands
		-- Holes that are completely inside islands are ignored here.
		islands := multi_crop_2 (
			polygon_B_list	=> islands,
			polygon_A_list	=> holes,
			debug			=> false);
			--debug			=> true);

		log_indentation_down;

		return islands;
	end zone_to_polygons;
	
	
	-- Returns a list of polygons caused by conductor
	-- objects in the given signal layer.
	-- The polygons are expanded by the zone_clearance or by
	-- the clearance of a particular net (the greater value of them is applied):
	function conductors_to_polygons (
		zone_clearance	: in type_track_clearance;
		layer 			: in type_signal_layer)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;

		layer_category : type_signal_layer_category;

		
		procedure query_net (n : in pac_nets.cursor) is
			
			net_class : constant type_net_class := get_net_class (module_cursor, n);			
			clearance : constant type_track_clearance := get_greatest (zone_clearance, net_class.clearance);

			-- The polygons of the candidate net are collected here:
			polygons : pac_polygon_list.list;
			
			route : et_pcb.type_route renames element (n).route;

			procedure query_line (l : in pac_conductor_lines.cursor) is
				use pac_conductor_lines;
				line : type_conductor_line renames element (l);
			begin
				if line.layer = layer then
					polygons.append (to_polygon (line, fab_tolerance));
				end if;
			end query_line;


			procedure query_via (v : in pac_vias.cursor) is
				use pac_vias;
				via : type_via renames element (v);
			begin
				case via.category is
					when THROUGH =>
						if layer_category = OUTER_TOP or layer_category = OUTER_BOTTOM then
							polygons.append (to_polygon (via.position, via.restring_outer, via.diameter));
						end if;

					when BLIND_DRILLED_FROM_TOP =>
						if layer_category = OUTER_TOP then
							polygons.append (to_polygon (via.position, via.restring_top, via.diameter));
						elsif blind_via_uses_layer (via, layer, bottom_layer) then
							polygons.append (to_polygon (via.position, via.restring_inner, via.diameter));
						end if;

					when BLIND_DRILLED_FROM_BOTTOM =>
						if layer_category = OUTER_BOTTOM then
							polygons.append (to_polygon (via.position, via.restring_bottom, via.diameter));
						elsif blind_via_uses_layer (via, layer, bottom_layer) then
							polygons.append (to_polygon (via.position, via.restring_inner, via.diameter));
						end if;
						
					when BURIED =>
						if layer_category = INNER and then
						   buried_via_uses_layer (via, layer) then
							polygons.append (to_polygon (via.position, via.restring_inner, via.diameter));
						end if;
				end case;
			end query_via;
			
			
		begin
			-- Query track segments:
			route.lines.iterate (query_line'access);

			-- CS route.arcs.iterate (query_arc'access);
			
			-- Query vias:
			route.vias.iterate (query_via'access);
			
			-- CS fill zones, ... see et_pcb.type_route
			-- CS pads

			offset_polygons (polygons, type_float_internal_positive (clearance));

			-- CS union polygons ?
			--multi_union (polygons);
			
			result.splice (before => pac_polygon_list.no_element, source => polygons);
		end query_net;


		use et_conductor_text.boards.pac_conductor_texts;
		procedure query_text (t : in pac_conductor_texts.cursor) is 
			text : type_conductor_text renames element (t);
		begin
			if text.layer = layer then
				null;
			end if;
		end query_text;
		
		
	begin
		-- Set the layer category:
		if layer = 1 then
			layer_category := OUTER_TOP;
		elsif layer = bottom_layer then
			layer_category := OUTER_BOTTOM;
		else
			layer_category := INNER;
		end if;
		
		
		element (module_cursor).nets.iterate (query_net'access);

		element (module_cursor).board.conductors.texts.iterate (query_text'access);
		-- CS non electrical conductor stuff (floating fill zones, text, fiducials, ...)

		
		return result;
	end conductors_to_polygons;
	


	-- Returns a list of polygons caused by route restrict
	-- objects in the given signal layer.
	function restrict_to_polygons (
		layer : in type_signal_layer)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;
	begin
		-- CS iterate global restrict areas
		-- CS iterate net specific cutouts ?
		return result;
	end restrict_to_polygons;


	-- Returns a list of polygons caused by cutout areas
	-- in the given signal layer.
	function cutouts_to_polygons (
		layer : in type_signal_layer)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;
	begin
		-- CS iterate global cutout areas
		
		return result;
	end cutouts_to_polygons;


	

	-- Clears the given basket:
	procedure empty_basket (
		basket : in out pac_polygon_list.list)
	is begin
		basket.clear;
	end empty_basket;
	

	-- Appends a list of polygons to the given basket.
	-- NOTE: The given list will be emptied.
	procedure put_into_basket (
		basket		: in out pac_polygon_list.list;
		polygons	: in out pac_polygon_list.list)
	is begin
		basket.splice (before => pac_polygon_list.no_element, source => polygons);

		-- Merge overlapping polygons:
		--multi_union (basket);
	end put_into_basket;

	
	procedure fill_zone (
		zone		: in out type_zone'class;
		linewidth	: in type_track_width;
		layer 		: in et_pcb_stack.type_signal_layer;
		clearance	: in type_track_clearance)
	is
		zone_polygon : type_polygon;
		
		islands : pac_polygon_list.list;
		conductors, restrict, cutouts : pac_polygon_list.list;
		cropping_basket : pac_polygon_list.list;

		
		procedure set_islands is
			
			procedure query_island (i : in pac_polygon_list.cursor) is begin
				zone.islands.append ((
					outer_border => element (i),
					others		 => <>));					 
			end query_island;
			
		begin
			islands.iterate (query_island'access);
		end set_islands;


		procedure set_inner_borders is 
			island_cursor : pac_islands.cursor := zone.islands.first;

			procedure make_inner_borders (
				island : in out type_island)
			is begin
				island.inner_borders := get_inside_polygons (island.outer_border, cropping_basket);
			end make_inner_borders;					
				
		begin
			while island_cursor /= pac_islands.no_element loop
				--put_line ("i");
				zone.islands.update_element (island_cursor, make_inner_borders'access);
				next (island_cursor);
			end loop;					
		end set_inner_borders;

			
		procedure fill_islands is 
			island_cursor : pac_islands.cursor := zone.islands.first;
		begin
			case zone.fill_style is
				when SOLID =>
					declare
						style : constant type_style := (
							style		=> SOLID,
	   						linewidth	=> linewidth);
					begin
						while island_cursor /= pac_islands.no_element loop
							fill_island (
								islands		=> zone.islands, 
								position	=> island_cursor,
								style		=> style,
								process		=> make_stripes'access);

							next (island_cursor);
						end loop;					

					end;
					
				when HATCHED =>
					declare
						style : constant type_style := (
							style		=> HATCHED,
							linewidth	=> linewidth,						   
							spacing		=> zone.spacing);
					begin
						while island_cursor /= pac_islands.no_element loop
							fill_island (
								islands		=> zone.islands, 
								position	=> island_cursor,
								style		=> style,
								process		=> make_stripes'access);

							next (island_cursor);
						end loop;					

					end;					
			end case;
			
		end fill_islands;
		
		
	begin -- fill_zone
		log (text => "zone with corner nearest to origin:" 
			 & to_string (get_corner_nearest_to_origin (zone)),
			level => log_threshold + 3);

		--put_line ("fill zone");
		
		log_indentation_up;
		
		
		-- Remove the old fill:
		zone.islands := no_islands;
		
		-- Convert the contour of the candidate fill zone to a polygon.
		-- Shrink the zone by half the line width so that the border of the zone
		-- does not extend beyond than the user defined contour:
		zone_polygon := to_polygon (zone, fab_tolerance);
		offset_polygon (zone_polygon, - type_float_internal_positive (linewidth) * 0.5);

		-- CS log lowest left vertex

		expand_holes (linewidth); -- updates variable "holes"
		
		-- Crop the zone by the outer board edges and the holes (stored in
		-- variable "holes").
		-- The zone gets fragmented into islands:
		islands := zone_to_polygons (
			zone		=> zone_polygon,
			line_width	=> linewidth);

		--put_line ("A");

		
		-- Now we start collecting polygons caused by conductor objects,
		-- polygon cutouts, restrict objects etc. in the cropping basket.
		-- Later everything in the basket will be used to crop the islands
		-- and to create inner borders inside the islands:
		empty_basket (cropping_basket);

		
		-- Collect holes in basket:
		put_into_basket (cropping_basket, holes);
		--put_line ("A2");
		
		
		-- Crop the islands by all conductor objects in the affected layer.
		-- The clearance of these objects to the zone is determined by
		-- the zone isolation or the net clearance. The greater value is applied:
		conductors := conductors_to_polygons (
			zone_clearance	=> clearance,
			layer			=> layer);

		put_into_basket (cropping_basket, conductors);
		--put_line ("A3");

		
		-- Crop the islands by all cutout areas in the affected layer.
		cutouts := cutouts_to_polygons (layer);
		put_into_basket (cropping_basket, cutouts);
		--put_line ("A4");
		
		
		-- Crop the islands by all route restrict objects in the affected layer.
		restrict := restrict_to_polygons (layer);
		put_into_basket (cropping_basket, restrict);
		--put_line ("A5");
		

		-- Now the basket is complete to crop its content with the islands.
		-- The result are even more islands:

		--put_line ("B");

		--multi_union (cropping_basket,true); -- debug messages on
		multi_union (cropping_basket);

		--put_line ("B1");
		
		islands := multi_crop_2 (
			polygon_B_list	=> islands,
			polygon_A_list	=> cropping_basket,
			debug			=> false);

		--put_line ("C");
		
		-- Assign the islands to the zone:
		set_islands;

		-- Assign inner borders to the islands of the zone:
		set_inner_borders;

		-- Fill the islands with stripes:
		fill_islands;

		--put_line ("E");
		
		log_indentation_down;
		
	end fill_zone;

	
	

	
	procedure floating_zones is
		use pac_floating_solid;
		use pac_floating_hatched;
		

		procedure floating_solid (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			zone_cursor : pac_floating_solid.cursor := module.board.conductors.fill_zones.solid.first;

			procedure do_it (
				zone : in out type_floating_solid)
			is begin
				fill_zone (
					zone		=> zone,
					linewidth	=> element (zone_cursor).linewidth,
					layer		=> zone.properties.layer,
					clearance	=> zone.isolation);
			end do_it;

		begin
			while zone_cursor /= pac_floating_solid.no_element loop
				module.board.conductors.fill_zones.solid.update_element (zone_cursor, do_it'access);
				next (zone_cursor);
			end loop;
		end floating_solid;

		
		procedure floating_hatched (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			zone_cursor : pac_floating_hatched.cursor := module.board.conductors.fill_zones.hatched.first;

			procedure do_it (
				zone : in out type_floating_hatched)
			is begin
				fill_zone (
					zone		=> zone,
					linewidth	=> element (zone_cursor).linewidth,
					layer		=> zone.properties.layer,
					clearance	=> zone.isolation);
			end do_it;
			
		begin
			while zone_cursor /= pac_floating_hatched.no_element loop
				module.board.conductors.fill_zones.hatched.update_element (zone_cursor, do_it'access);
				next (zone_cursor);
			end loop;
		end floating_hatched;

		
	begin -- floating_zones
		log (text => "floating zones ...", level => log_threshold + 1);
		update_element (generic_modules, module_cursor, floating_solid'access);
		update_element (generic_modules, module_cursor, floating_hatched'access);
	end floating_zones;


	
	-- Fills polygons that are connected with a net:
	procedure connected_zones is
		use et_nets;
		
		use pac_nets;
		use pac_route_solid;
		use pac_route_hatched;

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			net_cursor : pac_nets.cursor;
			net_class : type_net_class;		
		
			
			procedure route_solid (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is 
				-- The cursor that points to the zone being filled:
				use pac_route_solid;
				zone_cursor : pac_route_solid.cursor := net.route.fill_zones.solid.first;


				procedure do_it (
					zone : in out type_route_solid)
				is begin
					fill_zone (
						zone		=> zone,
						linewidth	=> element (zone_cursor).linewidth,
						layer		=> zone.properties.layer,
						clearance	=> get_greatest (zone.isolation, net_class.clearance));
				end do_it;
				
				
			begin -- route_solid
				while zone_cursor /= pac_route_solid.no_element loop

					-- do the filling
					net.route.fill_zones.solid.update_element (zone_cursor, do_it'access);
				
					next (zone_cursor);
				end loop;
			end route_solid;

			
			procedure route_hatched (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is 
				use pac_route_hatched;
				zone_cursor : pac_route_hatched.cursor := net.route.fill_zones.hatched.first;


				procedure do_it (
					zone : in out type_route_hatched)
				is begin
					fill_zone (
						zone		=> zone,
						linewidth	=> element (zone_cursor).linewidth,
						layer		=> zone.properties.layer,
						clearance	=> get_greatest (zone.isolation, net_class.clearance)
						);
				end do_it;
					
				
			begin -- route_hatched				
				while zone_cursor /= pac_route_hatched.no_element loop

					-- do the filling
					net.route.fill_zones.hatched.update_element (zone_cursor, do_it'access);

					next (zone_cursor);
				end loop;
			end route_hatched;


			procedure query_net is begin
				log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 2);
				
				log_indentation_up;

				net_class := get_net_class (module_cursor, net_cursor);
				
				update_element (module.nets, net_cursor, route_solid'access);
				update_element (module.nets, net_cursor, route_hatched'access);

				log_indentation_down;
			end query_net;
			

			procedure query_given_net (gn : pac_net_names.cursor) is begin
				-- Locate the given net in the module.
				-- If if does not exist, issue a warning.
				net_cursor := find (module.nets, element (gn));

				if net_cursor /= pac_nets.no_element then
					query_net;
				else
					log (
						importance => WARNING, 
						text => "Net " & enclose_in_quotes (to_string (element (gn))) 
							& " does not exist !", 
						level => log_threshold + 2);
				end if;
			end query_given_net;
			
			
		begin -- query_nets
			if all_zones then

				-- we must query all nets:
				net_cursor := module.nets.first;

				-- Iterate through all nets of the module:
				while net_cursor /= pac_nets.no_element loop
					query_net;
					next (net_cursor);
				end loop;

			else
				-- we query only the nets given by argument "nets":
				nets.iterate (query_given_net'access);
			end if;
		end query_nets;

		
	begin 
		log (text => "connected zones ...", level => log_threshold + 1);
		log_indentation_up;
		update_element (generic_modules, module_cursor, query_nets'access);
		log_indentation_down;
	end connected_zones;


	offset_scratch : type_distance;
	
begin -- fill_fill_zones

	log (text => "module " 
		& enclose_in_quotes (to_string (key (module_cursor)))
		& " filling zones. Log category " 
		& to_string (log_category) & " ...",
		level => log_threshold);

	log_indentation_up;

	log (text => "converting outer board contour to polygon ...", level => log_threshold + 1);
	
	board_outer_contour_master := to_polygon (
		contour		=> get_outline (module_cursor),
		tolerance	=> fab_tolerance);
	
	-- Shrink the outer board edge by the conductor-to-edge clearance
	-- as given by the design rules:

	offset_scratch := - design_rules.clearances.conductor_to_board_edge;
	
	log (text => "offsetting by DRU parameter " -- CS use predefined string
		& enclose_in_quotes (dru_parameter_clearance_conductor_to_board_edge) 
		& to_string (offset_scratch),
		level => log_threshold + 1);
	
	offset_polygon (board_outer_contour_master, type_float_internal (offset_scratch));
	-- for debuggin use:
	--offset_polygon (board_outer_contour_master, offset_scratch, true);
	-- CS consider half the line width !


	
	log (text => "converting holes to polygons ...", level => log_threshold + 1);
	
	board_holes_master := to_polygons (
		holes		=> get_holes (module_cursor),
		tolerance	=> fab_tolerance);

	-- Expand the holes by the conductor-to-edge clearance
	-- as given by the design rules:

	offset_scratch := - offset_scratch;
	
	log (text => "offsetting by DRU parameter " -- CS use predefined string 
		& enclose_in_quotes (dru_parameter_clearance_conductor_to_board_edge) 
		& to_string (offset_scratch),
		level => log_threshold + 1);

	offset_holes (board_holes_master, offset_scratch);
	-- for debugging use:
	--offset_holes (board_holes, offset_scratch, true);
	-- CS consider half the line width !

	
	if is_empty (nets) then
		
		-- Fill all zones if no explicit net names given:
		
		log (text => "filling all zones ...", level => log_threshold + 1);

		all_zones := true;
		
		log_indentation_up;
		connected_zones;

		floating_zones;

		log_indentation_down;
					
	else
		log (text => "filling zones of dedicated nets ...", level => log_threshold + 1);

		all_zones := false;
		
		log_indentation_up;
		connected_zones;
		log_indentation_down;
		
	end if;

	log_indentation_down;
	
end fill_fill_zones;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
