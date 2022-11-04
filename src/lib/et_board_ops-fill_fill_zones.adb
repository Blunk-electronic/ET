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
with et_board_ops.devices;

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
	-- objects (tracks, terminals, vias, texts, fiducials) in the given signal layer.
	-- The polygons are expanded by the zone_clearance or by
	-- the clearance of a particular net (the greater value of them is applied):
	function conductors_to_polygons (
		zone			: in type_polygon;
		zone_clearance	: in type_track_clearance;
		linewidth		: in type_track_width;								
		layer 			: in type_signal_layer;
		parent_net		: in pac_nets.cursor := pac_nets.no_element)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;

		layer_category : type_signal_layer_category;

		
		half_linewidth_float : constant type_float_internal_positive := 
			type_float_internal_positive (linewidth * 0.5);

		zone_clearance_float : constant type_float_internal_positive :=
			type_float_internal_positive (zone_clearance);


		-- In the course of this function we are mainly interested in polygons
		-- that are inside the given area. Other statuses may be added to this set
		-- later:
		use pac_overlap_status;
		overlap_status : pac_overlap_status.set := to_set (B_INSIDE_A);
		
		
		-- NETS -------------------------------------------------------------

		-- If a parent net was given (via argument parent_net) then
		-- this will hold the actual net name like "GND".
		-- Otherwise it will be left empty:
		parent_net_name : pac_net_name.bounded_string;

		procedure set_parent_net_name is begin
			if parent_net /= pac_nets.no_element then
				parent_net_name := key (parent_net);
			end if;
		end set_parent_net_name;
	

		-- Converts the terminals of the given net to a list of polygons:
		function get_terminal_polygons (net_cursor : in pac_nets.cursor)
			return pac_polygon_list.list
		is
			use et_nets;
			
			result : pac_polygon_list.list; -- a list of polygons to be returned
			ports : et_nets.type_ports;

			use pac_device_ports;
			use pac_terminals;
			use et_board_ops.devices;

			
			-- Converts the terminal, that is linked to the given device port,
			-- to a polygon and appends it to the result:
			procedure query_device_port (d : in pac_device_ports.cursor) is

				port : type_device_port renames element (d);
				-- Now port contains the device name, unit name and port name.
				
				-- Get the cursor to the device in the schematic:
				device_cursor : constant pac_devices_sch.cursor := 
					locate_device (module_cursor, port.device_name);

				-- Get the cursor to the physical terminal (in the package model)
				-- that is linked with the port:
				terminal_cursor : constant pac_terminals.cursor := 
					get_terminal (device_cursor, port.unit_name, port.port_name);

				-- Get the actual terminal as described in the package model:
				terminal : constant et_terminals.type_terminal := 
					element (terminal_cursor);

				
				-- Get the terminal name (like 3 or H5):
				terminal_name : constant pac_terminal_name.bounded_string := 
					key (terminal_cursor);
				
				-- Get the terminal position (incl. rotation and face):
				terminal_position : constant type_terminal_position_fine := 
					get_terminal_position (module_cursor, device_cursor, terminal_name);


				-- intermediate places to store a single polygon and a single contour:
				polygon : type_polygon;
				contour : type_contour;

				-- The displacement required to move the contour to 
				-- its final position:
				terminal_displacement : constant type_distance_relative := 
					to_distance_relative (terminal_position.place);


				-- Converts the contour to a polygon:
				procedure make_polygon is begin
					polygon := to_polygon (
						contour		=> contour,
						tolerance	=> fill_tolerance,
						mode		=> EXPAND, -- CS ?
						debug		=> false);
				end make_polygon;
			

				-- Moves the contour to the final position, converts it to a polygon
				-- and appends the polygon to the result.
				-- Optionally, if required by the caller, offsets the polygon edges
				-- by the width of the inner signal layer:
				procedure finalize (do_offset : in boolean := false) is begin
					move_by (contour, terminal_displacement);
					make_polygon;
					if do_offset then
						offset_polygon (polygon, type_float_internal (terminal.width_inner_layers));
					end if;
					result.append (polygon);
				end finalize;


				-- Mirrors the contour (if terminal is flipped to bottom side) and
				-- rotates the contour:
				procedure mirror_and_rotate is begin
					if terminal_position.face = BOTTOM then
						mirror (contour, Y);

						-- if on bottom side: rotate CW
						rotate_by (contour, - to_rotation (terminal_position.rotation));
					else
						-- if on top side: rotate CCW
						rotate_by (contour, + to_rotation (terminal_position.rotation));
					end if;
				end mirror_and_rotate;

				
			begin -- query_device_port
				
				case terminal.technology is
					when THT => 
						case layer_category is
							when INNER =>								
								case terminal.tht_hole is
									when DRILLED =>
										contour := get_inner_contour (terminal, terminal_position.place);
										make_polygon;										
										result.append (polygon);
										
									when MILLED =>
										contour := terminal.millings;
										mirror_and_rotate;										
										finalize (do_offset => true);
										
								end case;
							
							when OUTER_TOP =>
								if terminal_position.face = TOP then
									contour := terminal.pad_shape_tht.top;
								else
									contour := terminal.pad_shape_tht.bottom;
								end if;
								mirror_and_rotate;
								finalize;

							when OUTER_BOTTOM =>
								if terminal_position.face = BOTTOM then
									contour := terminal.pad_shape_tht.top;
								else
									contour := terminal.pad_shape_tht.bottom;
								end if;
								mirror_and_rotate;
								finalize;
						end case;
						

					when SMT =>
						if layer_category = OUTER_TOP and terminal_position.face = TOP then
							contour := terminal.pad_shape_smt;
							rotate_by (contour, to_rotation (terminal_position.rotation));
							finalize;						
							
						elsif layer_category = OUTER_BOTTOM and terminal_position.face = BOTTOM then
							contour := terminal.pad_shape_smt;
							mirror (contour, Y);
							rotate_by (contour, - to_rotation (terminal_position.rotation));
							finalize;
						end if;
				end case;

			end query_device_port;

			
		begin
			-- Get the ports of all devices connected with the given net.
			-- Therefore we do not pass a specific assembly variant here.
			ports := get_ports (net_cursor); 

			-- In variable "ports" we are interested in selector "devices" exclusively.
			-- Submodule ports and netchangers are just virtual devices
			-- that connect two conductor tracks. They can therefore be ignored:
			ports.devices.iterate (query_device_port'access);
	
			return result;
		end get_terminal_polygons;

		
		-- Extracts all conductor objects connected with the given net
		-- and appends them to the result:
		procedure query_net (net_cursor : in pac_nets.cursor) is

			-- The clearance between net and zone is either the given zone_clearance
			-- or the clearance of the net itself. However, the greater value is applied:
			net_class : constant type_net_class := get_net_class (module_cursor, net_cursor);
			
			clearance : constant type_track_clearance := 
				get_greatest (zone_clearance, net_class.clearance);

			-- The polygons of the candidate net are collected here
			-- (later they will be appended to the result):
			polygons : pac_polygon_list.list;

			
			route : et_pcb.type_route renames element (net_cursor).route;

			procedure query_line (l : in pac_conductor_lines.cursor) is
				use pac_conductor_lines;
				line : type_conductor_line renames element (l);
			begin
				if line.layer = layer then
					polygons.append (to_polygon (line, fill_tolerance));
				end if;
			end query_line;


			procedure query_via (v : in pac_vias.cursor) is
				use pac_vias;
				via : type_via renames element (v);
			begin
				case via.category is
					when THROUGH =>
						if layer_category = OUTER_TOP or layer_category = OUTER_BOTTOM then
							polygons.append (to_polygon (via.position, via.restring_outer, via.diameter, fill_tolerance));
						else
							polygons.append (to_polygon (via.position, via.restring_inner, via.diameter, fill_tolerance));
						end if;

					when BLIND_DRILLED_FROM_TOP =>
						if layer_category = OUTER_TOP then
							polygons.append (to_polygon (via.position, via.restring_top, via.diameter, fill_tolerance));
						elsif blind_via_uses_layer (via, layer, bottom_layer) then
							polygons.append (to_polygon (via.position, via.restring_inner, via.diameter, fill_tolerance));
						end if;

					when BLIND_DRILLED_FROM_BOTTOM =>
						if layer_category = OUTER_BOTTOM then
							polygons.append (to_polygon (via.position, via.restring_bottom, via.diameter, fill_tolerance));
						elsif blind_via_uses_layer (via, layer, bottom_layer) then
							polygons.append (to_polygon (via.position, via.restring_inner, via.diameter, fill_tolerance));
						end if;
						
					when BURIED =>
						if layer_category = INNER and then
						   buried_via_uses_layer (via, layer) then
							polygons.append (to_polygon (via.position, via.restring_inner, via.diameter, fill_tolerance));
						end if;
				end case;
			end query_via;


			procedure do_it is 
				terminals : pac_polygon_list.list;
			begin
				-- Query track segments:
				route.lines.iterate (query_line'access);

				-- CS route.arcs.iterate (query_arc'access);
				
				-- Query vias:
				route.vias.iterate (query_via'access);
				
				-- CS fill zones, ... see et_pcb.type_route
				
				-- terminals of packages
				terminals := get_terminal_polygons (net_cursor);
				polygons.splice (before => pac_polygon_list.no_element, source => terminals);
				
				offset_polygons (polygons, type_float_internal_positive (clearance));

				result.splice (before => pac_polygon_list.no_element, source => polygons);
			end do_it;
			
			
		begin -- query_net
			
			-- If no parent net was given, then query all nets:
			if parent_net = pac_nets.no_element then
				do_it;
			else
				-- Otherwise skip the parent net and process all other nets:
				if key (net_cursor) /= parent_net_name then
					do_it;
				--else
					--log (text => "skipping parent net " 
						--& enclose_in_quotes (to_string (parent_net_name)),
						--level => log_threshold + 4);
				end if;
			end if;
		end query_net;


		procedure query_device (device_cursor : in pac_devices_sch.cursor) is
			use et_board_ops.devices;
			
			terminals : constant pac_terminals.map := 
				get_unconnected_terminals (module_cursor, device_cursor);
		begin
			null; -- CS
		end query_device;

		
		
		-- TEXTS ---------------------------------------------------------------
		use et_conductor_text.boards.pac_conductor_texts;
		
		procedure query_text (t : in pac_conductor_texts.cursor) is 
			text : type_conductor_text renames element (t);
			borders : pac_polygon_list.list;
		begin
			if text.layer = layer then

				borders := get_borders (text.vectors);

				offset_polygons (borders, half_linewidth_float + zone_clearance_float);
				
				-- NOTE: The borders of the characters of the text should not overlap.
				-- Therefore there is no need for unioning the characters at this time.
				
				result.splice (
					before => pac_polygon_list.no_element,
					source => borders);

			end if;
		end query_text;
		
		
	begin -- conductors_to_polygons
		
		log (text => "processing conductor objects ...", level => log_threshold + 4);
		log_indentation_up;
		
		-- Set the layer category:
		if layer = 1 then
			layer_category := OUTER_TOP;
		elsif layer = bottom_layer then
			layer_category := OUTER_BOTTOM;
		else
			layer_category := INNER;
		end if;
		

		-- Assigns to parent_net_name the actual name of the
		-- parent net. Does nothing if no parent net given
		-- by argument parent_net:
		set_parent_net_name;

		-- Query nets. Exempt the parent net (if specified by argument parent_net):
		element (module_cursor).nets.iterate (query_net'access);

		-- Query unconnected terminals of devices:
		element (module_cursor).devices.iterate (query_device'access);
			
		-- board texts:
		element (module_cursor).board.conductors.texts.iterate (query_text'access);
		
		-- CS non electrical conductor stuff (foreign floating fill zones, package text, fiducials, ...)

		-- Now the polygons held in variable "result"
		-- - inside the given zone or
		-- - overlapping the given zone
		-- must be extracted. 
		-- NOTE: Variable overlap_status already contains B_INSIDE_A. See declaration above.
		overlap_status.insert (A_OVERLAPS_B);
		result := get_polygons (zone, result, overlap_status);
		
		log_indentation_down;
		
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


	-- Fill the given zone that is in the given layer
	-- with the given linewidth and clearance to foreign conductor
	-- objects. If a certain conductor object requires a greater
	-- clearance, then that clearance will prevail.
	-- If a parent net is given then the conductor objects of this
	-- net will be ignored so that they are embedded in the fill
	-- zone:
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
			is 
				use pac_overlap_status;
			begin
				island.inner_borders := get_polygons (
					area		=> island.outer_border, 
					polygons	=> cropping_basket,
					status		=> to_set (B_INSIDE_A));
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

		--if parent_net /= pac_nets.no_element then
			--log (text => "parent net " 
				--& enclose_in_quotes (to_string (key (parent_net))),
				 --level => log_threshold + 3);
		--end if;

		
		-- Remove the old fill:
		zone.islands := no_islands;
		
		-- Convert the contour of the candidate fill zone to a polygon.
		zone_polygon := to_polygon (zone, fill_tolerance, SHRINK); 
		-- NOTE: The SHRINK argument applies to the approximation mode of 
		-- arcs and circles. Has nothing to do with the actual shrinking of the zone
		-- by the follwing statement.


		log (text => "shrinking zone", level => log_threshold + 4);

		-- The border of the zone is drawn with the given linewidth.
		-- The zone must be shrinked by half the linewidth so that the 
		-- outline of the border is congruent to the zone drawn by the operator:
		offset_polygon (zone_polygon, - type_float_internal_positive (linewidth) * 0.5);

		-- CS log lowest left vertex

		-- The holes inside the board area will crop the zone later.
		-- Therefore they must be made greater than they acutually are:
		expand_holes (linewidth); -- updates variable "holes"
		
		-- Crop the zone by the outer board edges and the holes (stored in
		-- variable "holes").
		-- As a result, the zone disintegrates. It gets fragmented into islands:
		islands := zone_to_polygons (
			zone		=> zone_polygon,
			line_width	=> linewidth);

		--put_line ("A");

		
		-- Now we start collecting contours of objects inside the zone.
		-- The will be put in the cropping basket.
		-- Later everything in the basket will be used to crop the islands (of the zone)
		-- and to create inner borders inside the islands:
		empty_basket (cropping_basket);
		
		-- Collect holes in basket:
		put_into_basket (cropping_basket, holes);
		--put_line ("A2");


		
		-- Get the contours of all conductor objects in the affected layer.
		-- This is about tracks, terminals, vias, texts and fiducials.
		-- The clearance of these objects to the zone is determined by
		-- the zone isolation or the net clearance. The greater value is applied:
		conductors := conductors_to_polygons (
			zone			=> zone_polygon,
			zone_clearance	=> clearance,
			linewidth		=> linewidth,									 
			layer			=> layer,
			parent_net		=> parent_net);

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
		

		-- Union the content of the cropping basket as much as possible:
		multi_union (cropping_basket, debug);
		
		-- Now the basket is ready to crop its content with the islands.
		-- The result are even more islands:

		--put_line ("B");


		-- CS: experimental
		--multi_union_2 (cropping_basket, debug); -- debug messages on
		--multi_union (cropping_basket);

		--if debug then
			--put_line ("B1");
		--end if;
		
		islands := multi_crop_2 (
			polygon_B_list	=> islands,
			polygon_A_list	=> cropping_basket,
			debug			=> false);

		--if debug then
			--put_line ("C");
		--end if;
		
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
						clearance	=> get_greatest (zone.isolation, net_class.clearance),
						parent_net	=> net_cursor
						--debug		=> true
						);
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
						clearance	=> get_greatest (zone.isolation, net_class.clearance),
						parent_net	=> net_cursor
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
		mode		=> SHRINK,										 
		tolerance	=> fill_tolerance);
	
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
		tolerance	=> fill_tolerance);

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
