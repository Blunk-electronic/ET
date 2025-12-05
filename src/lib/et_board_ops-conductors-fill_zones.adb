------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                  BOARD OPERATIONS / FILLING FILL ZONES                   --
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

with ada.characters.latin_1;

with et_pcb_contour;				use et_pcb_contour;
with et_contour_to_polygon;			use et_contour_to_polygon;
with et_routing;					use et_routing;
with et_vias;						use et_vias;
with et_board_ops.devices;
with et_board_ops.board_contour;	use et_board_ops.board_contour;
with et_board_ops.fill_zones;		use et_board_ops.fill_zones;
with et_thermal_relief;				use et_thermal_relief;

with et_devices_electrical; 			use et_devices_electrical;
with et_devices_electrical.packages;	use et_devices_electrical.packages;
with et_devices_non_electrical;			use et_devices_non_electrical;


with et_net_ports;
with et_route;						use et_route;


separate (et_board_ops.conductors)

procedure fill_zones (
	module_cursor	: in pac_generic_modules.cursor;
	log_category	: in type_log_category;
	log_threshold	: in type_log_level;
	nets 			: in pac_net_names.list := no_net_names)
is 
	-- CS: Most of this stuff can be moved to et_fill_zones.boards
	-- so that solid and hatched zones inherit from primitive operations
	-- defined for type_zone. 
	
	use pac_geometry_brd;
	use pac_polygons;
	use pac_polygon_clipping;
	use pac_polygon_cropping;
	use pac_polygon_offsetting;
	use pac_polygon_union;
	
	use pac_net_names;

	all_zones : boolean;

	
	-- Get the design rules:
	design_rules : constant type_design_rules_board := get_pcb_design_rules (module_cursor);

	clearance_conductor_to_edge : type_distance_positive renames 
		design_rules.clearances.conductor_to_board_edge;
	
	


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


	-- The default set to test overlap statuses contains B_INSIDE_A and A_OVERLAPS_B.
	-- This function creates such a set:
	function make_overlap_status_set return pac_overlap_status.set is
		use pac_overlap_status;
		S : pac_overlap_status.set := to_set (B_INSIDE_A);
	begin
		S.insert (A_OVERLAPS_B);
		return S;
	end make_overlap_status_set;
	
	
	overlap_status_set : constant pac_overlap_status.set := make_overlap_status_set;

	
	
	-- Expands the board_holes_master by half the given linewidth.
	-- Extracts the holes that are in the given zone and those with 
	-- overlap the zone.
	-- Loads variable "holes" with the result:
	procedure expand_holes (
		zone		: in type_polygon;
		linewidth	: in type_track_width)
	is begin
		holes := board_holes_master;
		
		log (text => "expanding holes (" & count_type'image (holes.length) & ") ...", 
			 level => log_threshold + 4);
				
		-- Expand the holes by half the line width of the fill lines:
		offset_holes (holes, linewidth * 0.5);

		-- Extract those holes that are inside the zone and those
		-- with overlap the zone:
		holes := get_polygons (zone, holes, overlap_status_set);
				
		multi_union (holes);
	end expand_holes;



	
	
	-- Clips the given zone by the outer board contours.
	-- The result - a list of islands - will be cropped by the holes.
	-- Assumes that variable "holes" has been updated by procedure expand_holes.
	-- The result is a list of polygons because the given zone may disintegrate
	-- into smaller fragments:
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
		offset_polygon (outer_contour, - type_float_positive (line_width) * 0.5);

		-- Clip the fill zone by the outer contour of the board.
		-- The zone may shrink or disintegrate into smaller islands:
		islands := clip (zone, outer_contour);

		-- Now we crop the islands by the holes. The outcome are even more islands.
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

	

	-- Temporarily storage for properties of zones connected with a net:
	relief_properties	: type_relief_properties;
	terminal_reliefes	: pac_reliefes.list;
	terminal_connection	: type_pad_connection := pad_connection_default;
	terminal_technology	: type_pad_technology := pad_technology_default;
	native_tracks_embedded : type_native_tracks_embedded := false;


	-- type type_conductor_to_polygons_result is record
	-- 	polygons				: pac_polygon_list.list;
	-- 	terminals_with_relief	: pac_terminals_with_relief.list;
	-- end record;
	


	
	-- Returns a list of polygons caused by conductor
	-- objects (tracks, terminals, vias, texts, fiducials) in 
	-- the given signal layer.
	-- Returns only those polygons which are inside the given zone
	-- or which touch the given zone.
	-- As a byproduct, the return also contains a list of terminals
	-- that require thermal reliefes. If the zone is not connected 
	-- with the given parent_net, then the list "terminals_with_relief" 
	-- is empty:
	

	

	-- Returns a list of polygons caused by route restrict
	-- objects in the given signal layer.
	function restrict_to_polygons (
		layer : in type_signal_layer)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;
	begin
		log (text => "processing restrict area ...", level => log_threshold + 4);
		log_indentation_up;

		-- CS iterate global restrict areas
		-- CS iterate net specific cutouts ?

		log_indentation_down;
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
		log (text => "processing cutout area ...", level => log_threshold + 4);
		log_indentation_up;
		
		-- CS iterate global cutout areas

		log_indentation_down;
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


	

	-- Fills the given zone that is in the given layer
	-- with the given linewidth and clearance to foreign conductor
	-- objects. If a certain conductor object requires a greater
	-- clearance, then that clearance will take precedence.
	-- If a parent net is given - via cursor - then the conductor objects of this
	-- net will be treated in a special way. Then the given zone must be a 
	-- type_route_hatched or a type_route_solid.
	-- - terminals may be embedded in the zone or may get connected via thermal relieves.
	-- - tracks may be embedded in the zone or the zone will be filled around them.
	-- - see specification of type_route_solid and type_route_hatched.
	-- If something goes wrong, an exception is raised.
	procedure fill_zone (
		zone		: in out type_zone'class;
		linewidth	: in type_track_width;
		layer 		: in et_pcb_stack.type_signal_layer;
		clearance	: in type_track_clearance;
		parent_net	: in pac_nets.cursor := pac_nets.no_element;
		debug		: in boolean := false)
	is
		zone_polygon : type_polygon;
		
		islands : pac_polygon_list.list;
		restrict, cutouts : pac_polygon_list.list;
		conductors_to_polygons_result : type_conductor_to_polygons_result;
		cropping_basket : pac_polygon_list.list;

		half_linewidth_float : constant type_float_positive :=
			0.5 * type_float_positive (linewidth);

		use ada.characters.latin_1;


		
		
		-- Iterates the islands and assigns them to the given zone.
		-- Computes the outer edges of the islands according to the
		-- border-width of the candidate zone:
		procedure set_islands is
			
			procedure query_island (i : in pac_polygon_list.cursor) is 
				island_centerline : type_polygon renames element (i);
			begin
				zone.islands.append ((
					shore		 => (
						centerline => island_centerline,
						outer_edge => offset_polygon (island_centerline, half_linewidth_float)),
					others		 => <>)); 

				-- CS: Currently, the outer edge is just an expansion of the centerline.
				-- The border is drawn with lines that have round caps. These caps cause
				-- the outer edge to have rounded corners. The computation of the outer edge
				-- should take this into account.
				
				--put_line ("islands total" & count_type'image (zone.islands.length));

				exception when constraint_error =>
					log (importance => WARNING,
						text => "Offsetting centerline of island failed !" & LF
							& to_string (polygon => island_centerline, lower_left_only => true),
						level => log_threshold + 3);
					raise;			
			end query_island;
			
		begin
			islands.iterate (query_island'access);
		end set_islands;


		

		-- Iterates the islands, detects polygons that
		-- form the lakes (inside the islands). Lakes are caused by objects
		-- that are completely inside a particular island.
		procedure set_lakes is 
			island_cursor : pac_islands.cursor := zone.islands.first;

			procedure make_lakes (
				island : in out type_island)
			is 
				use pac_overlap_status;
				centerlines : pac_polygon_list.list;
				
				procedure query_centerline (cl : in pac_polygon_list.cursor) is
					centerline : type_polygon renames element (cl);
				begin
					if debug then
						put_line ("   centerline");
					end if;

					-- The inner edges are obtained by shrinking the centerline
					-- by half the fill linewidth:
					island.lakes.append ((
						centerline	=> centerline,
						inner_edge	=> offset_polygon (centerline, - half_linewidth_float)));
					--inner_edge	=> offset_polygon (centerline, - half_linewidth_float, true)));

					exception when constraint_error =>
						log (importance => WARNING,
							text => "Offsetting centerline of lake failed !" & LF
								& to_string (polygon => centerline, lower_left_only => true),
							level => log_threshold + 3);
						raise;
				end query_centerline;
				
			begin
				-- Get lakes inside the candidate island.
				-- These are the centerlines of the lakes:
				centerlines := get_polygons (
					area		=> island.shore.centerline, 
					polygons	=> cropping_basket,
					status		=> to_set (B_INSIDE_A));

				-- Iterate the centerlines and compute the inner edges.
				centerlines.iterate (query_centerline'access);
			end make_lakes;					
				
		begin
			while island_cursor /= pac_islands.no_element loop

				if debug then
					put_line ("  island");
				end if;

				zone.islands.update_element (island_cursor, make_lakes'access);
				next (island_cursor);
			end loop;					
		end set_lakes;


		
		-- Fills the islands according to the fill style
		-- of the given zone:
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


		
		procedure make_thermal_reliefes is begin
			log (text => "make_thermal_reliefes", level => log_threshold + 4);

			-- Delete all reliefes left over from the previous zone:
			terminal_reliefes.clear;
			
			terminal_reliefes := make_reliefes (
				zone				=> zone,
				relief_properties	=> relief_properties,								   
				terminals			=> conductors_to_polygons_result.terminals_with_relief,
				zone_clearance		=> clearance,
				zone_linewidth		=> linewidth);

		end make_thermal_reliefes;

		
		
	begin -- fill_zone
		log (text => "zone with corner nearest to origin:" 
			 & to_string (get_corner_nearest_to_origin (zone)),
			level => log_threshold + 3);


		if debug then
			put_line ("fill zone");
		end if;
		
		log_indentation_up;

		--if parent_net /= pac_nets.no_element then
			--log (text => "parent net " 
				--& enclose_in_quotes (to_string (key (parent_net))),
				 --level => log_threshold + 3);
		--end if;

		
		-- Remove the old fill (incl. islands, lakes):
		zone.islands := no_islands;

		
		-- Convert the contour of the candidate fill zone to a polygon:
		if debug then
			put_line (" convert contour to polygon");
		end if;
		
		zone_polygon := to_polygon (zone, fill_tolerance, SHRINK); 
		-- NOTE: The SHRINK argument applies to the approximation mode of 
		-- arcs and circles. Has nothing to do with the actual shrinking of the zone
		-- by the follwing statement.


		log (text => "shrinking zone", level => log_threshold + 4);

		-- The border of the zone is drawn with the given linewidth.
		-- The zone must be shrinked by half the linewidth so that the 
		-- outline of the border is congruent to the zone drawn by the operator:
		offset_polygon (zone_polygon, - type_float_positive (linewidth) * 0.5);

		-- CS log lowest left vertex

		-- The holes inside the board area and inside the zone
		-- will crop the zone later. Since the zone border has a linewidth,
		-- the holes must be made greater than they acutually are:
		expand_holes (zone_polygon, linewidth); -- updates variable "holes"
		
		-- Crop the zone by the outer board edges and the holes (stored in
		-- variable "holes").
		-- As a result, the zone disintegrates. It gets fragmented into islands:
		islands := zone_to_polygons (
			zone		=> zone_polygon,
			line_width	=> linewidth);

		
		-- Now we start collecting contours of objects inside the zone.
		-- They will be put in the cropping basket.
		-- Later everything in the basket will be used to crop the islands (of the zone)
		-- and to create inner borders inside the islands:
		empty_basket (cropping_basket);
		
		-- Collect holes in basket:
		put_into_basket (cropping_basket, holes);

		
		-- Get the contours of all conductor objects in the affected layer
		-- and in the zone_polygon.
		-- This is about tracks, terminals, vias, texts and fiducials.
		-- The clearance of these objects to the zone is determined by
		-- the zone isolation or the net clearance. The greater value is applied:
		conductors_to_polygons_result := conductors_to_polygons (
			module_cursor		=> module_cursor,
			zone_polygon		=> zone_polygon,
			zone_clearance		=> clearance,
			linewidth			=> linewidth,									 
			layer				=> layer,
			parent_net			=> parent_net,
			terminal_connection	=> terminal_connection,
			clearance_to_edge	=> clearance_conductor_to_edge,
			log_threshold		=> log_threshold + 5);

		put_into_basket (cropping_basket, conductors_to_polygons_result.polygons);


		
		-- Crop the islands by all cutout areas in the affected layer.
		cutouts := cutouts_to_polygons (layer);
		put_into_basket (cropping_basket, cutouts);
		--put_line ("A4");
		
		
		-- Crop the islands by all route restrict objects in the affected layer.
		restrict := restrict_to_polygons (layer);
		put_into_basket (cropping_basket, restrict);
		--put_line ("A5");
		

		-- Union the content of the cropping basket as much as possible:
		if debug then
			put_line (" union cropping basket");
		end if;
		
		--multi_union (cropping_basket, debug);
		multi_union (cropping_basket);
		
		-- Now the basket is ready to crop its content with the islands.
		-- The result are even more islands:

		--put_line ("B");


		-- CS: experimental
		--multi_union_2 (cropping_basket, debug); -- debug messages on
		--multi_union (cropping_basket);

		if debug then
			put_line (" crop islands with cropping basket");
		end if;
		
		islands := multi_crop_2 (
			polygon_B_list	=> islands,
			polygon_A_list	=> cropping_basket,
			debug			=> false);


		
		-- Assign the islands to the zone:
		if debug then
			put_line (" set islands");
		end if;

		set_islands;


		
		-- Assign lakes to the islands of the zone:
		if debug then
			put_line (" set lakes");
		end if;

		set_lakes;



		
		if parent_net /= pac_nets.no_element then

			if debug then
				put_line (" make thermal reliefes");
			end if;
			
			make_thermal_reliefes; 
			-- bases on the inner borders that we just computed. see statement above
		end if;



		
		-- Fill the islands with stripes:
		if debug then
			put_line (" fill islands");
		end if;

		fill_islands;


		
		log_indentation_down;
		
	end fill_zone;

	
	

	
	
	procedure fill_floating_zones is
		use pac_floating_solid;
		use pac_floating_hatched;
		

		procedure floating_solid (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			zone_cursor : pac_floating_solid.cursor := module.board.conductors_floating.zones.solid.first;

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
			while has_element (zone_cursor) loop
				module.board.conductors_floating.zones.solid.update_element (zone_cursor, do_it'access);
				next (zone_cursor);
			end loop;
		end floating_solid;



		
		procedure floating_hatched (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			zone_cursor : pac_floating_hatched.cursor := module.board.conductors_floating.zones.hatched.first;

			
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
			while has_element (zone_cursor) loop
				module.board.conductors_floating.zones.hatched.update_element (zone_cursor, do_it'access);
				next (zone_cursor);
			end loop;
		end floating_hatched;

		
	begin
		log (text => "fill_floating_zones", level => log_threshold + 1);
		update_element (generic_modules, module_cursor, floating_solid'access);
		update_element (generic_modules, module_cursor, floating_hatched'access);
	end fill_floating_zones;




	
	
	
	-- Fills zones that are connected with a net:
	procedure fill_connected_zones is
		use et_nets;
		
		use pac_nets;
		use pac_route_solid;
		use pac_route_hatched;


		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			net_cursor : pac_nets.cursor;
			net_class : type_net_class;		

			
			
			procedure route_solid (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is 
				-- The cursor that points to the zone being filled:
				use pac_route_solid;
				zone_cursor : pac_route_solid.cursor := net.route.zones.solid.first;


				procedure do_it (
					zone : in out type_route_solid)
				is 
					-- Backup the zone in case something went wrong:
					zone_bakup : constant type_route_solid := zone; 
				begin
					-- load temporarily variables of zone properties:
					relief_properties := zone.relief_properties;
					terminal_connection	:= zone.connection;
					native_tracks_embedded := zone.native_tracks_embedded;

					if zone.connection = SOLID then
						terminal_technology	:= zone.technology;
					end if;

					
					fill_zone (
						zone		=> zone,
						linewidth	=> element (zone_cursor).linewidth,
						layer		=> zone.properties.layer,
						clearance	=> get_greatest (zone.isolation, net_class.clearance),
						parent_net	=> net_cursor,
						debug		=> false
						--debug		=> true
						);

					zone.reliefes := terminal_reliefes;

					-- If something went wrong, output some
					-- helpful information and restore the zone:
					exception when event:
						others =>
						log (
							importance => WARNING,
							text => exception_information (event));
					
						log (
							importance => WARNING,
							text => "Zone at"
								& to_string (get_corner_nearest_to_origin (zone))
								& " has NOT been filled !");
							--level => log_threshold + 3);

						-- CS log zone properties ?
						-- CS write warning in GUI status bar
						
						zone := zone_bakup;			
				end do_it;
				
				
			begin
				while has_element (zone_cursor) loop

					-- do the filling
					net.route.zones.solid.update_element (zone_cursor, do_it'access);
				
					next (zone_cursor);
				end loop;
			end route_solid;


			
			procedure route_hatched (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is 
				use pac_route_hatched;
				zone_cursor : pac_route_hatched.cursor := net.route.zones.hatched.first;


				procedure do_it (
					zone : in out type_route_hatched)
				is begin
					-- load temporarily variables of zone properties:
					relief_properties := zone.relief_properties;
					terminal_connection	:= zone.connection;
					native_tracks_embedded := zone.native_tracks_embedded;

					if zone.connection = SOLID then
						terminal_technology	:= zone.technology;
					end if;
					
					fill_zone (
						zone		=> zone,
						linewidth	=> element (zone_cursor).linewidth,
						layer		=> zone.properties.layer,
						clearance	=> get_greatest (zone.isolation, net_class.clearance),
						parent_net	=> net_cursor);
				end do_it;
					
				
			begin
				while has_element (zone_cursor) loop

					-- do the filling
					net.route.zones.hatched.update_element (zone_cursor, do_it'access);

					next (zone_cursor);
				end loop;
			end route_hatched;


			
			procedure query_net is begin
				log (text => "net " & get_net_name (net_cursor), level => log_threshold + 2);
				
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

				if has_element (net_cursor) then
					query_net;
				else
					log (
						importance => WARNING, 
						text => "Net " & enclose_in_quotes (to_string (element (gn))) 
							& " does not exist !", 
						level => log_threshold + 2);
				end if;
			end query_given_net;

			
			
		begin
			if all_zones then

				-- we must query all nets:
				net_cursor := module.nets.first;

				-- Iterate through all nets of the module:
				while has_element (net_cursor) loop
					query_net;
					next (net_cursor);
				end loop;

			else
				-- we query only the nets given by argument "nets":
				nets.iterate (query_given_net'access);
			end if;
		end query_module;


		
	begin 
		log (text => "fill_connected_zones", level => log_threshold + 1);
		log_indentation_up;
		update_element (generic_modules, module_cursor, query_module'access);
		log_indentation_down;
	end fill_connected_zones;

	

	--offset_scratch : type_distance_model;
	
begin -- fill_zones

	log (text => "module " & to_string (module_cursor)
		& " fill zones. Log category " & to_string (log_category),
		level => log_threshold);

	log_indentation_up;

	log (text => "convert outer board contour to polygon", level => log_threshold + 1);
	
	board_outer_contour_master := to_polygon (
		contour		=> get_outer_contour (module_cursor),
		mode		=> SHRINK,										 
		tolerance	=> fill_tolerance);

	
	-- Shrink the outer board edge by the conductor-to-edge clearance
	-- as given by the design rules:
	log (text => "offset by DRU parameter " -- CS use predefined string
		& enclose_in_quotes (dru_parameter_clearance_conductor_to_board_edge) 
		& to_string (- clearance_conductor_to_edge),
		level => log_threshold + 1);
	
	offset_polygon (board_outer_contour_master, type_float_model (- clearance_conductor_to_edge));



	
	log (text => "convert holes to polygons", level => log_threshold + 1);
	
	board_holes_master := to_polygons (
		holes		=> get_holes (module_cursor),
		tolerance	=> fill_tolerance);

	-- Expand the holes by the conductor-to-edge clearance
	-- as given by the design rules:
	log (text => "offset by DRU parameter " -- CS use predefined string 
		& enclose_in_quotes (dru_parameter_clearance_conductor_to_board_edge) 
		& to_string (clearance_conductor_to_edge),
		level => log_threshold + 1);

	offset_holes (board_holes_master, clearance_conductor_to_edge);


	
	if is_empty (nets) then
		
		-- Fill all zones if no explicit net names given:
		
		log (text => "fill all zones", level => log_threshold + 1);

		all_zones := true;
		
		log_indentation_up;
		fill_connected_zones;

		fill_floating_zones;

		log_indentation_down;
					
	else
		log (text => "fill zones of dedicated nets", level => log_threshold + 1);

		all_zones := false;
		
		log_indentation_up;
		fill_connected_zones;
		log_indentation_down;
		
	end if;

	log_indentation_down;
	
end fill_zones;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
