------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     BOARD OPERATIONS / FILL ZONES                        --
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

with ada.text_io;					use ada.text_io;
with ada.tags;

with et_mirroring;
with et_schematic_ops.units;
with et_schematic_ops.nets;
with et_pcb_contour;
with et_board_ops.board_contour;
with et_board_ops.devices;
with et_fill_zones.boards;

with et_net_ports;

with et_devices_electrical.packages;
with et_devices_non_electrical;

with et_conductor_text.boards;
with et_conductor_segment.boards;
with et_contour_to_polygon;
with et_vias;
with et_route;



package body et_board_ops.fill_zones is


	
	function to_polygon (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_electrical.cursor;
		terminal_cursor	: in pac_terminals.cursor;
		layer_category	: in type_signal_layer_category;
		tolerance		: in type_distance_positive;
		log_threshold	: in type_log_level)
		return type_terminal_polygon
	is
		exists : boolean := false;
		result : type_polygon; -- to be returned

		use et_board_ops.devices;
		use et_mirroring;
		use pac_contours;
		use pac_terminals;
		
		-- Get the actual terminal as described in the package model:
		terminal : constant et_terminals.type_terminal := element (terminal_cursor);
		
		-- Get the terminal name (like 3 or H5):
		terminal_name : constant pac_terminal_name.bounded_string := key (terminal_cursor);
		
		-- Get the terminal position (incl. rotation and face):
		terminal_position : constant type_terminal_position_fine := 
			get_terminal_position (module_cursor, device_cursor, terminal_name);

		-- The displacement required to move the contour to 
		-- its final position:
		terminal_displacement : constant type_vector_model := to_vector_model (terminal_position.place);

		
		-- intermediate storage place of a contour:
		contour : type_contour;


		-- Converts the contour to a polygon:
		procedure make_polygon is 
			use et_contour_to_polygon;
		begin
			exists := true;
			
			result := to_polygon (
				contour		=> contour,
				tolerance	=> tolerance,
				mode		=> EXPAND, -- CS ?
				debug		=> false);
		end make_polygon;


		-- Mirrors the contour (if terminal is flipped to bottom side) and
		-- rotates the contour:
		procedure mirror_and_rotate is begin
			if terminal_position.face = BOTTOM then
				mirror (contour, MIRROR_ALONG_Y_AXIS);

				-- if on bottom side: rotate CW
				rotate_by (contour, - to_rotation (terminal_position.rotation));
			else
				-- if on top side: rotate CCW
				rotate_by (contour, + to_rotation (terminal_position.rotation));
			end if;
		end mirror_and_rotate;


		-- Moves the contour to the final position and converts it to a polygon.
		-- Optionally, if required by the caller, offsets the polygon edges
		-- by the width of the inner signal layer:
		procedure finalize (do_offset : in boolean := false) is
			use et_board_geometry.pac_polygon_offsetting;
		begin
			move_by (contour, terminal_displacement);
			make_polygon;
			if do_offset then
				offset_polygon (result, type_float_model (terminal.width_inner_layers),
					log_threshold + 1);
			end if;
		end finalize;
				
		
	begin -- to_polygon
		log (text => "module " & to_string (module_cursor)
			 & "get terminal polygon of device " & get_device_name (device_cursor)
			 & " terminal dumy", -- CS terminal name
			 -- CS layer category
			 -- CS tolerance
			 level => log_threshold);

		log_indentation_up;

			 
		case terminal.technology is
			when THT => 
				case layer_category is
					when INNER =>								
						case terminal.tht_hole is
							when DRILLED =>
								contour := get_inner_contour (terminal, terminal_position.place);
								make_polygon;										
								
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
					mirror (contour, MIRROR_ALONG_Y_AXIS);
					rotate_by (contour, - to_rotation (terminal_position.rotation));
					finalize;
				end if;
		end case;

		
		log_indentation_down;

		
		if exists then
			return (
				exists		=> TRUE, 
				polygon		=> result, 
				position	=> terminal_position);
		else
			return (exists => FALSE);
		end if;
	end to_polygon;


	


	

	procedure get_polygons_of_connected_terminals (
		module_cursor			: in pac_generic_modules.cursor;
		layer_category 			: in type_signal_layer_category;
		zone					: in pac_polygons.type_polygon;
		offset					: in type_float_positive;
		net_cursor 				: in pac_nets.cursor;
		polygons				: in out pac_polygon_list.list;		
		with_reliefes			: in boolean;
		terminals_with_relief	: out pac_terminals_with_relief.list;
		log_threshold			: in type_log_level)
	is 
		use et_nets;

		use et_net_ports;
		use pac_device_ports;
		
		-- Here the ports of all devices which are connected
		-- with the given net are stored:
		ports : type_ports;

		
		-- This procedure queries a device port.
		-- From the port we map to the associated physical terminal.
		-- We then convert the terminal to a polygon and 
		-- appends it to the resulting list polygons:
		procedure query_device_port (c : in pac_device_ports.cursor) is
			port : type_device_port renames element (c);
			-- Port contains the device name, unit name and port name.
			
			-- Get the cursor to the device in the schematic:
			use et_devices_electrical;
			use et_devices_electrical.packages;
			use et_schematic_ops.units;
			
			device_cursor : constant pac_devices_electrical.cursor := 
				get_electrical_device (module_cursor, port.device_name);

			
			-- This procedure maps from the candidate port to the
			-- associated terminal and converts the terminal to a polygon:
			procedure query_port is			
				-- Get the cursor to the physical terminal (in the package model)
				-- that is linked with the port:
				terminal_cursor : constant pac_terminals.cursor := 
					get_terminal (device_cursor, port.unit_name, port.port_name);

				-- Convert the terminal outline to a polygon:
				terminal_polygon : constant type_terminal_polygon := to_polygon (
					module_cursor, device_cursor, terminal_cursor, 
					layer_category, fill_tolerance, log_threshold + 2);
				-- CS difficult to debug. move to a subprocedure

				terminal_polygon_expanded : type_polygon;

				terminal_zone_overlap : type_overlap_status;
				
				use pac_polygon_offsetting;

				-- CS: more log messages
			begin
				-- If the terminal does not affect the current signal layer,
				-- then nothing happens here. Otherwise the outline of the terminal
				-- will be appended to the result:
				if terminal_polygon.exists then

					terminal_polygon_expanded := terminal_polygon.polygon;
					offset_polygon (terminal_polygon_expanded, offset, log_threshold + 3);

					-- If the polygon is inside the zone then
					-- we append it to the result:
					if polygon_touches_area (zone, terminal_polygon_expanded) then

						log (text => "terminal " & get_terminal_name (terminal_cursor),
							level => log_threshold + 2);
							
						log_indentation_up;

						log (text => "expanded contour: " & to_string (terminal_polygon_expanded),
							level => log_threshold + 4);

						polygons.append (terminal_polygon_expanded);

						-- If the terminal overlaps the zone or if it is
						-- inside the zone then thermal reliefes are required.
						if with_reliefes then
							if polygon_touches_area (zone, terminal_polygon.polygon) then
								log (text => "thermal relief prepared",
									level => log_threshold + 3);

								terminals_with_relief.append ((
									position => terminal_polygon.position, -- in the board
									outline	 => terminal_polygon.polygon, -- in the board
									terminal => terminal_cursor));  -- in the package model
							end if;
						end if;
					
						log_indentation_down;
					end if;
				end if;				
			end query_port;
			
			
		begin
			-- Only real devices are relevant:
			if is_real (device_cursor) then
				log (text => "device " & get_device_name (device_cursor),
					level => log_threshold + 1);
					
				log_indentation_up;
				query_port;
				log_indentation_down;
			end if;			
		end query_device_port;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " get_polygons_of_connected_terminals"
			& " layer cat: " & to_string (layer_category)
			& " offset: " & to_string (offset)
			& " net: " & get_net_name (net_cursor)
			& " thermal: " & boolean'image (with_reliefes),
			level => log_threshold);
		-- CS log zone ?

		log_indentation_up;

		log (text => "polygons in:  " & get_count (polygons),
			level => log_threshold);
		

		-- Get the ports of ALL devices connected with the given net.
		-- Therefore we do not pass a specific assembly variant here.
		ports := get_ports (net_cursor); 

		-- In variable "ports" we are interested in selector "devices" exclusively.
		-- Submodule ports and netchangers are just virtual devices
		-- that connect two conductor tracks. They can therefore be ignored:
		ports.devices.iterate (query_device_port'access);


		log (text => "polygons out: " & get_count (polygons),
			level => log_threshold);
		
		log_indentation_down;
	end get_polygons_of_connected_terminals;



	
	
	
	
	
	procedure get_polygons_of_nets (
		module_cursor			: in pac_generic_modules.cursor;
		layer_category 			: in type_signal_layer_category;
		zone					: in type_polygon;
		linewidth				: in type_track_width;
		layer 					: in type_signal_layer;
		zone_clearance			: in type_track_clearance;
		bottom_layer			: in type_signal_layer;
		parent_net				: in pac_nets.cursor;
		polygons				: in out pac_polygon_list.list;
		terminal_connection		: in type_pad_connection;
		terminals_with_relief	: out pac_terminals_with_relief.list;
		log_threshold			: in type_log_level)
	is 			
		use pac_nets;
		use pac_net_name;

		-- If a parent net was given (via argument parent_net) then
		-- this will hold the actual net name like "GND".
		-- Otherwise it will be left empty:
		parent_net_name : pac_net_name.bounded_string;

		
		procedure set_parent_net_name is begin
			if has_element (parent_net) then
				parent_net_name := key (parent_net);
				log (text => "parent net: " & to_string (parent_net_name),
					level => log_threshold);
			else
				log (text => "zone is not connected to any net -> no thermals will be generated",
					level => log_threshold);
			end if;
		end set_parent_net_name;
			
			
		collect_terminals_with_relief : boolean := false;
	
	
		procedure query_module (
			module_name		: in pac_module_name.bounded_string;
			module			: in type_generic_module)
		is
			use pac_polygon_union;

			
			procedure query_net (net_cursor : in pac_nets.cursor) is
				net_class : constant type_net_class := get_net_class (module_cursor, net_cursor);
				
				-- The offset by which the polyons of the net
				-- are to be expanded depends on the candidate net class.
				-- The clearance between net and zone is either the given zone_clearance
				-- or the clearance of the net itself. The greatest of them is applied:				
				clearance : constant type_track_clearance := 
					get_greatest (zone_clearance, net_class.clearance);

				offset : constant type_float_positive := 
					type_float_positive (linewidth * 0.5 + clearance);

				-- The polygons of the candidate net are collected here:
				polygons_of_candidate_net : pac_polygon_list.list;

				-- Information about routed stuff of the candidate net
				-- is stored here (lines, arcs, vias, ...):
				use et_route;
				route : type_net_route renames element (net_cursor).route;

				use et_conductor_segment.boards;

				

				
				-- Converts tracks, vias and terminals to polygons:
				procedure convert_conductor_objects_to_polygons is 
					use pac_polygon_offsetting;
					reliefes : pac_terminals_with_relief.list;
				begin
					-- Get the polygons of the route:
					polygons_of_candidate_net := 
						get_polygons (route, layer_category, layer, bottom_layer);
					
					log (text => "polygons of route: " & get_count (polygons_of_candidate_net),
						 level => log_threshold + 2);

					-- Union as much polygons as possible:
					multi_union (polygons_of_candidate_net);

					log (text => "union polygons. remaining: " & get_count (polygons_of_candidate_net),
						 level => log_threshold + 2);

					-- Expand the polygons by the (see offset computation above):
					offset_polygons (polygons_of_candidate_net, offset, log_threshold + 3);
					
					-- CS evaluate native_tracks_embedded ?
					-- CS foreign fill zones, ... see et_route.type_route
					
					-- Get terminal polygons of device packages
					-- which are connected with the candidate net.
					-- These polygons are appended to polygons_of_candidate_net:
					get_polygons_of_connected_terminals (
						module_cursor			=> module_cursor,
						layer_category			=> layer_category,
						zone					=> zone,
						net_cursor				=> net_cursor,
						offset					=> offset,
						polygons				=> polygons_of_candidate_net,
						with_reliefes			=> collect_terminals_with_relief,
						terminals_with_relief	=> reliefes,
						log_threshold			=> log_threshold + 2);

					-- Union the polygons of the route and those of
					-- the terminals as much as possible:
					multi_union (polygons_of_candidate_net);
					
					-- Extract those polygons which are inside the
					-- zone or which touch the zone:
					get_touching_polygons (zone, polygons_of_candidate_net);

					log (text => "polygons in zone: " & get_count (polygons_of_candidate_net),
						 level => log_threshold + 2);
					
					
					-- Append the polygons of the candidate net to the result:
					append (polygons, polygons_of_candidate_net);

					-- Append the relief information to the given
					-- list of terminals_with_relief:
					append_relieves (terminals_with_relief, reliefes);
				end convert_conductor_objects_to_polygons;
				

				-- If we are processing a zone that is connected with a net,
				-- and the candidate is the parent net of the zone, then this
				-- flag is set to true:
				in_parent_net : boolean := false;

				
			begin -- extract_conductor_objects
				log (text => "net " & to_string (get_net_name (net_cursor)),
					level => log_threshold + 1);
					
				log_indentation_up;
				
				-- If no parent net was given, then the given zone is 
				-- assumed to be a floating zone. If a parent net was given,
				-- then the zone is connected with a net:
				if not has_element (parent_net) then
					-- The zone is floating. No thermals will be generated:
					collect_terminals_with_relief := false;
					convert_conductor_objects_to_polygons;
					
				else
					-- Zone is connected with a net:
					-- NOTE: This is relevant exclusively for 
					-- zones connected with a net !

					if key (net_cursor) = parent_net_name then
						in_parent_net := true;
					end if;

					
					case terminal_connection is
						when SOLID =>
							-- Skip the parent net and process all other nets.
							-- Thus all conductor objects of the parent net will
							-- be completely embedded in the fill zone:
							if not in_parent_net then
								collect_terminals_with_relief := false;
								convert_conductor_objects_to_polygons;
							end if;

						when THERMAL =>
							if in_parent_net then
								collect_terminals_with_relief := true;
							else
								collect_terminals_with_relief := false;
							end if;
							convert_conductor_objects_to_polygons;
					end case;
				end if;
				
				log_indentation_down;
			end query_net;
		
		
		begin
			module.nets.iterate (query_net'access);

			-- CS: multi_union (polygons) ?
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor) &
			" get_polygons_of_nets"
			& " layer category: " & to_string (layer_category)
			& " target layer: " & to_string (layer)
			& " bottom layer: " & to_string (bottom_layer)
			& " linewidth: " & to_string (linewidth)
			& " zone clearance: " & to_string (zone_clearance)
			& " terminal connection: " & to_string (terminal_connection),
			level => log_threshold);
		-- CS more log info (zone ?)

		log_indentation_up;
		
		set_parent_net_name;
		
		query_element (module_cursor, query_module'access);

		-- CS log number of polygons
		log_indentation_down;
	end get_polygons_of_nets;




	


	
	procedure get_polygons_of_unconnected_terminals (
		module_cursor			: in pac_generic_modules.cursor;
		layer_category 			: in type_signal_layer_category;
		zone					: in pac_polygons.type_polygon;
		zone_clearance			: in type_track_clearance;
		linewidth				: in type_track_width;
		polygons				: in out pac_polygons.pac_polygon_list.list;
		log_threshold			: in type_log_level)
	is
		use pac_polygon_union;

		offset : constant type_float_positive := 
			type_float_positive (linewidth * 0.5 + zone_clearance);
		-- CS function to_offset (linewidth, zone_clearance)
		-- might already be available

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			use et_devices_electrical;
			
			-- Temporarily we store the polygons here.
			-- In the end of this procedure we extract those
			-- which affect the given zone:
			polygons_tmp : pac_polygon_list.list;


			-- This procedure queries an electrical device and
			-- processes its terminals:
			procedure query_device (
				device_cursor : in pac_devices_electrical.cursor)
			is
				use et_board_ops.devices;
				terminals : pac_terminals.map;
				
				
				-- This procedure queries an unconnected terminal
				-- and converts it to a polyon, expands the polygon
				-- by offset and appends it to the result
				procedure query_terminal (
					terminal_cursor : in pac_terminals.cursor) 
				is
					use pac_polygon_offsetting;
					
					-- Convert the terminal outline to a polygon:
					terminal_polygon : type_terminal_polygon := to_polygon (
						module_cursor, device_cursor, terminal_cursor, 
						layer_category, fill_tolerance, log_threshold + 4);
					-- CS difficult to debug. use subprocedure
				begin
					log (text => "terminal " & get_terminal_name (terminal_cursor),
						level => log_threshold + 3);
						
					if terminal_polygon.exists then
						-- CS more log messages

						-- Expand the polygon by offset:
						offset_polygon (terminal_polygon.polygon, offset,
							log_threshold + 4);

						-- Append the polygon to the temporarily
						-- polygon collection:
						polygons_tmp.append (terminal_polygon.polygon);
					end if;					
				end query_terminal;
				
				
			begin
				-- Since all this is about board related things,
				-- we look at real devices exclusively:
				if is_real (device_cursor) then
					log (text => "device " & get_device_name (device_cursor),
						level => log_threshold + 2);
					
					log_indentation_up;
				
					terminals := get_unconnected_terminals (
						module_cursor, device_cursor, log_threshold + 2);

					terminals.iterate (query_terminal'access);
					
					-- multi_union (polygons_tmp);

					log_indentation_down;
				end if;
			end query_device;
			
		
		begin
			-- Iterate through the electrical devices:
			module.devices.iterate (query_device'access);

			-- CS ? multi_union (polygons_tmp);
			
			-- Extract those polygons from the temporarily collection
			-- which are inside the zone or which touch the zone:
			get_touching_polygons (zone, polygons_tmp);

			-- Log the number of polygons that have been found:
			log (text => "polygons: " & get_count (polygons_tmp),
				 level => log_threshold + 1);
			
			-- Append the temporarily polygon collection to the result:
			append (polygons, polygons_tmp);
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " get_polygons_of_unconnected_terminals"
			& " layer cat: " & to_string (layer_category)
			& " zone clearance: " & to_string (zone_clearance)
			& " offset " & to_string (offset)
			& " zone linewidth: " & to_string (linewidth),			
			level => log_threshold);
		
		log_indentation_up;
	
		query_element (module_cursor, query_module'access);

		-- CS ? multi_union (polygons);
		
		log_indentation_down;
	end get_polygons_of_unconnected_terminals;
	







	procedure get_polygons_of_non_electrical_devices (
		module_cursor			: in pac_generic_modules.cursor;
		layer_category 			: in type_signal_layer_category;
		zone					: in pac_polygons.type_polygon;
		zone_clearance			: in type_track_clearance;
		linewidth				: in type_track_width;
		clearance_to_edge		: in type_distance_positive;
		polygons				: in out pac_polygons.pac_polygon_list.list;
		log_threshold			: in type_log_level)
	is
		use pac_polygon_union;

		offset : constant type_float_positive := 
			type_float_positive (linewidth * 0.5 + zone_clearance);
		-- CS function to_offset (linewidth, zone_clearance)
		-- might already be available

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			use et_devices_non_electrical;
			use pac_devices_non_electrical;

			
			-- This procedure queries a non-electrical device.
			-- 1. It converts all conductor objects, holes and route restrict
			--    objects to polygons.
			-- 2. Expands the polygons, tests whether the given zone is affected
			--    and appends them to the result:
			procedure query_device (d : in pac_devices_non_electrical.cursor) is
				use et_pcb_contour;
				p : pac_polygon_list.list;

				use pac_polygon_offsetting;
			begin
				log (text => "device " & get_device_name (d),
					 level => log_threshold + 1);

				log_indentation_up;
				
				-- Process conductor objects:
				p := get_conductor_polygons (d, layer_category);
				offset_polygons (p, offset, log_threshold + 3);
				log (text => "conductors" & get_count (p), level => log_threshold + 2);

				-- Exract those which are inside the given zone
				-- or which overlap the given zone
				-- and append them to the result:
				append (polygons, get_polygons (zone, p, overlap_mode_1));
				

				-- Process holes:
				p := get_hole_polygons (d);
				offset_holes (p, linewidth * 0.5 + clearance_to_edge, log_threshold + 3);
				log (text => "holes" & get_count (p), level => log_threshold + 2);

				-- Exract those which are inside the given zone
				-- or which overlap the given zone
				-- and append them to the result:
				append (polygons, get_polygons (zone, p, overlap_mode_1));


				-- route restrict:
				p := get_route_restrict_polygons (d, layer_category);
				offset_polygons (p, type_float_positive (linewidth * 0.5), log_threshold + 3);
				log (text => "route restrict" & get_count (p), level => log_threshold + 2);
				
				-- Exract those which are inside the given zone
				-- or which overlap the given zone
				-- and append them to the result:
				append (polygons, get_polygons (zone, p, overlap_mode_1));

				-- multi_union (polygons);

				log_indentation_down;
			end query_device;

			
		begin
			-- Iterate through the non-electrical devices:
			module.devices_non_electric.iterate (query_device'access);
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " get_polygons_of_non_electrical_devices"
			& " layer cat: " & to_string (layer_category)
			& " zone clearance: " & to_string (zone_clearance)
			& " zone linewidth: " & to_string (linewidth)
			& " offset " & to_string (offset)
			& " zone clearance to edge: " & to_string (clearance_to_edge),
			level => log_threshold);

		
		log_indentation_up;
	
		query_element (module_cursor, query_module'access);

		-- multi_union (polygons);

		-- CS log number of polygons
		
		log_indentation_down;
	end get_polygons_of_non_electrical_devices;
	







	procedure get_polygons_of_electrical_devices (
		module_cursor			: in pac_generic_modules.cursor;
		layer_category 			: in type_signal_layer_category;
		zone					: in pac_polygons.type_polygon;
		zone_clearance			: in type_track_clearance;
		linewidth				: in type_track_width;
		clearance_to_edge		: in type_distance_positive;
		polygons				: in out pac_polygons.pac_polygon_list.list;
		log_threshold			: in type_log_level)
	is
		use pac_polygon_union;

		offset : constant type_float_positive := 
			type_float_positive (linewidth * 0.5 + zone_clearance);
		-- CS function to_offset (linewidth, zone_clearance)
		-- might already be available

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			use et_devices_electrical;
			use et_devices_electrical.packages;
			use pac_devices_electrical;

			
			-- This procedure queries an electrical device.
			-- 1. It converts all conductor objects, holes and route restrict
			--    objects to polygons. NOTE: Terminals are not processed here.
			-- 2. Expands the polygons, tests whether the given zone is affected
			--    and appends them to the result:
			procedure query_device (d : in pac_devices_electrical.cursor) is
				use et_pcb_contour;
				p : pac_polygon_list.list;
				use pac_polygon_offsetting;
			begin
				log (text => "device " & get_device_name (d),
					 level => log_threshold + 1);

				log_indentation_up;
				
				-- Process conductor objects:
				p := get_conductor_polygons (d, layer_category);
				offset_polygons (p, offset, log_threshold + 3);
				log (text => "conductors" & get_count (p), level => log_threshold + 2);

				-- Exract those which are inside the given zone
				-- or which overlap the given zone
				-- and append them to the result:
				append (polygons, get_polygons (zone, p, overlap_mode_1));
				

				-- Process holes:
				p := get_hole_polygons (d);
				offset_holes (p, linewidth * 0.5 + clearance_to_edge, log_threshold + 3);
				log (text => "holes" & get_count (p), level => log_threshold + 2);

				-- Exract those which are inside the given zone
				-- or which overlap the given zone
				-- and append them to the result:
				append (polygons, get_polygons (zone, p, overlap_mode_1));


				-- route restrict:
				p := get_route_restrict_polygons (d, layer_category);
				offset_polygons (p, type_float_positive (linewidth * 0.5), log_threshold + 3);
				log (text => "route restrict" & get_count (p), level => log_threshold + 2);
				
				-- Exract those which are inside the given zone
				-- or which overlap the given zone
				-- and append them to the result:
				append (polygons, get_polygons (zone, p, overlap_mode_1));

				-- multi_union (polygons);

				log_indentation_down;
			end query_device;

			
		begin
			-- Iterate through the electrical devices:
			module.devices.iterate (query_device'access);
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " get_polygons_of_electrical_devices"
			& " layer cat: " & to_string (layer_category)
			& " zone clearance: " & to_string (zone_clearance)
			& " zone linewidth: " & to_string (linewidth)
			& " offset " & to_string (offset)
			& " zone clearance to edge: " & to_string (clearance_to_edge),
			level => log_threshold);

		
		log_indentation_up;
	
		query_element (module_cursor, query_module'access);

		-- multi_union (polygons);

		-- CS log number of polygons
		
		log_indentation_down;
	end get_polygons_of_electrical_devices;






	


	procedure get_polygons_of_board_texts (
		module_cursor			: in pac_generic_modules.cursor;
		zone					: in pac_polygons.type_polygon;
		zone_clearance			: in type_track_clearance;
		linewidth				: in type_track_width;
		layer 					: in type_signal_layer;
		polygons				: in out pac_polygons.pac_polygon_list.list;
		log_threshold			: in type_log_level)
	is
		use pac_polygon_union;


		offset : constant type_float_positive := 
			type_float_positive (linewidth * 0.5 + zone_clearance);
		-- CS function to_offset (linewidth, zone_clearance)
		-- might already be available

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			use et_conductor_text.boards;
			use pac_conductor_texts;

			
			procedure query_text (t : in pac_conductor_texts.cursor) is 
				text : type_conductor_text renames element (t);
				borders : pac_polygon_list.list;

				use et_board_text;
				use pac_text_board;
				use pac_polygon_offsetting;
			begin
				if text.layer = layer then

					borders := get_borders (text.vectors);
					
					offset_polygons (borders, offset, log_threshold + 2);
					
					-- NOTE: The borders of the characters of the text should not overlap.
					-- Therefore there is no need for unioning the characters at this time.
					
					-- CS test whether zone is affected
					
					polygons.splice (
						before => pac_polygon_list.no_element,
						source => borders);

					-- multi_union (polygons);
				end if;
			end query_text;

			
		begin
			module.board.conductors_floating.texts.iterate (query_text'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " get_polygons_of_board_texts"
			& " layer: " & to_string (layer)
			& " zone clearance: " & to_string (zone_clearance)
			& " zone linewidth: " & to_string (linewidth)
			& " offset " & to_string (offset),			
			level => log_threshold);


		log_indentation_up;
	
		query_element (module_cursor, query_module'access);

		-- CS log number of polygons

		-- multi_union (polygons);
		
		log_indentation_down;
	end get_polygons_of_board_texts;
	







	procedure get_touching_polygons (
		module_cursor		: in pac_generic_modules.cursor;
		zone_polygon		: in type_polygon;
		zone_clearance		: in type_track_clearance;
		linewidth			: in type_track_width;
		layer 				: in type_signal_layer;
		parent_net			: in pac_nets.cursor := pac_nets.no_element;
		terminal_connection	: in type_pad_connection;
		clearance_to_edge 	: in type_distance_positive;

		polygons				: out pac_polygon_list.list;
		terminals_with_relief	: out pac_terminals_with_relief.list;

		log_threshold		: in type_log_level)
	is
		use pac_polygon_union;

		layer_category : type_signal_layer_category;

		-- The deepest conductor layer towards bottom is 
		-- defined by the layer stack of the module:
		bottom_layer : constant type_signal_layer := 
			get_deepest_conductor_layer (module_cursor);


		use pac_nets;

		-- If a parent net was given (via argument parent_net) then
		-- this will hold the actual net name like "GND".
		-- Otherwise it will be left empty:
		parent_net_name : pac_net_name.bounded_string;

		procedure set_parent_net_name is begin
			if parent_net /= pac_nets.no_element then
				parent_net_name := key (parent_net);
			end if;
		end set_parent_net_name;

	
		
		-- Extracts the polygons of all conductor 
		-- objects (tracks, terminals, vias) which are 
		-- connected with the given net
		-- and appends them to the result:
		procedure process_nets is begin
			log (text => "conductor objects of nets", level => log_threshold + 1);			
			log_indentation_up;
			
			get_polygons_of_nets (
				module_cursor			=> module_cursor,
				layer_category			=> layer_category,
				zone					=> zone_polygon,
				linewidth				=> linewidth,
				layer					=> layer,
				zone_clearance			=> zone_clearance,
				bottom_layer			=> bottom_layer,
				parent_net				=> parent_net,
				polygons				=> polygons,
				terminal_connection		=> terminal_connection,
				terminals_with_relief	=> terminals_with_relief,
				log_threshold			=> log_threshold + 2);
			
			multi_union (polygons);

			log_indentation_down;
		end process_nets;

		
		
		-- This procedure converts the outlines of unconnected terminals
		-- of electrical devices to polygons and appends them to the result:
		procedure process_unconnected_terminals is begin
			log (text => "unconnected terminals", level => log_threshold + 1);
			log_indentation_up;

			get_polygons_of_unconnected_terminals (
			 	module_cursor		=> module_cursor,
			 	layer_category		=> layer_category,
				zone				=> zone_polygon,
				zone_clearance		=> zone_clearance,
				linewidth			=> linewidth,
				polygons			=> polygons,
				log_threshold		=> log_threshold + 2);

			-- multi_union (polygons);

			log_indentation_down;
		end process_unconnected_terminals;
	

		
		-- This procedure converts the texts in conductor
		-- layers to polygons and appends them to the result:
		procedure process_board_texts is begin
			log (text => "board texts", level => log_threshold + 1);
		
			log_indentation_up;
			
			get_polygons_of_board_texts (
				module_cursor	=> module_cursor,
				zone			=> zone_polygon,
				zone_clearance	=> zone_clearance,
				linewidth		=> linewidth,
				layer			=> layer,
				polygons		=> polygons,
				log_threshold	=> log_threshold + 2);
				
			-- multi_union (polygons);

			log_indentation_down;
		end process_board_texts;
		
		
		
		
		-- This procedure converts objects of non-electrial
		-- devices to polygons and appends them to the result:
		procedure process_non_electrical_devices is begin
			log (text => "non-electrical devices", level => log_threshold + 1);
			log_indentation_up;
			
			get_polygons_of_non_electrical_devices (
				module_cursor		=> module_cursor,
				layer_category		=> layer_category,
				zone				=> zone_polygon,
				zone_clearance		=> zone_clearance,
				linewidth			=> linewidth,
				clearance_to_edge	=> clearance_to_edge,
				polygons			=> polygons,
				log_threshold		=> log_threshold + 2);
				
			-- multi_union (polygons);

			log_indentation_down;
		end process_non_electrical_devices;
		
		
		
		
		-- This procedure converts objects of electrial
		-- devices to polygons and appends them to the result.
		-- NOTE: This does not address connected terminals !
		procedure process_electrical_devices is begin
			log (text => "electrial devices", level => log_threshold + 1);
			log_indentation_up;
			
			get_polygons_of_electrical_devices (
				module_cursor		=> module_cursor,
				layer_category		=> layer_category,
				zone				=> zone_polygon,
				zone_clearance		=> zone_clearance,
				linewidth			=> linewidth,
				clearance_to_edge	=> clearance_to_edge,
				polygons			=> polygons,
				log_threshold		=> log_threshold + 2);
				
			-- multi_union (polygons);

			log_indentation_down;
		end process_electrical_devices;
		
	
		
		-- This procedure converts cutout areas to polygons
		-- and appends them to the result.
		-- The purpose of cutout areas is to exempt
		-- certain areas of fill zones from being filled:
		procedure process_cutout_areas is begin
			log (text => "cutout areas", level => log_threshold + 1);
			log_indentation_up;

			-- CS todo. 
			-- iterate global cutout areas in the 
			-- given signal layer.
			-- Test whether the given zone is affected

			-- multi_union (polygons);
			log_indentation_down;
		end process_cutout_areas;



		-- This procedure converts route restrict areas to polygons
		-- and appends them to the result:
		procedure process_restrict_areas is begin
			log (text => "route restrict areas", level => log_threshold + 1);
			log_indentation_up;
			
			-- CS todo. 
			-- iterate global route restrict lines, arcs, areas in the 
			-- given signal layer.
			-- Test whether the given zone is affected
			-- CS iterate net specific cutouts ?

			-- multi_union (polygons);
			log_indentation_down;
		end process_restrict_areas;



		-- This procedure converts holes to polygons
		-- and appends them to the result:
		procedure process_holes is 
			use et_board_ops.board_contour;
			use et_pcb_contour;
			
			holes : pac_holes.list;
			polygons_tmp : pac_polygon_list.list;
		begin
			log (text => "holes", level => log_threshold + 1);
			log_indentation_up;
			
			-- Get the holes of the module:
			holes := get_holes (module_cursor);

			-- Convert the holes to polygons:
			polygons_tmp := to_polygons (holes, fill_tolerance);

			-- Expand the polygons:
			offset_holes (polygons_tmp, linewidth * 0.5 + clearance_to_edge, log_threshold + 2);

			-- Extract those polygons which are inside the
			-- zone or which touch the zone:
			get_touching_polygons (zone_polygon, polygons_tmp);

			-- multi_union (polygons_tmp);

			-- Append the polygons to the result:
			append (polygons, polygons_tmp);

			-- multi_union (polygons);
			log_indentation_down;
		end process_holes;


		
	begin	
		log (text => "module " & to_string (module_cursor)
			& " get_touching_polygons",
			-- CS
			-- & " layer: " & to_string (layer)
			-- & " zone clearance: " & to_string (zone_clearance)
			-- & " zone linewidth: " & to_string (linewidth)
			level => log_threshold);
	
		-- log (text => "process conductor objects", level => log_threshold);
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

		process_holes;

		-- Extract conductor objects of nets.
		process_nets;

		process_unconnected_terminals; -- of electrical devices

		process_board_texts;

		process_non_electrical_devices;
		process_electrical_devices;
		
		-- CS non electrical conductor 
		-- stuff (placeholders, foreign floating fill zones, ...)
		
		process_cutout_areas;
		process_restrict_areas;

		multi_union (polygons);

		log_indentation_down;
	end get_touching_polygons;








	procedure fill_zone (
		module_cursor		: in pac_generic_modules.cursor;
		zone				: in out type_zone'class;
		outer_contour		: in type_polygon;
		linewidth			: in type_track_width;
		layer 				: in et_pcb_stack.type_signal_layer;
		clearance			: in type_track_clearance;
		clearance_to_edge 	: in type_distance_positive;
		parent_net			: in pac_nets.cursor := pac_nets.no_element;
		terminal_connection	: in type_pad_connection;
		relief_properties	: in type_relief_properties;
		reliefes			: out pac_reliefes.list;
		log_threshold		: in type_log_level)
	is
		debug : boolean := false;

		-- The given zone will be converted to a polygon:
		zone_polygon : type_polygon;


		use et_contour_to_polygon;		

		half_linewidth_float : constant type_float_positive :=
			0.5 * type_float_positive (linewidth);

		-- One of the first steps is to shrink (offset) the 
		-- outer board contour by half the linewidth of the zone.
		-- For this reason we take a copy of the given board contour:
		outer_contour_tmp : type_polygon := outer_contour;

		use pac_polygon_offsetting;
		use pac_polygon_clipping;
		use pac_polygon_cropping;
		use pac_polygon_list;

		



		procedure process_zone_fragments is
			use pac_nets;

			-- The zone may disintegrate into smaller fragments
			-- after it has been clipped with the outer board contour.
			-- The fragments are stored in this list:
			zone_fragments : pac_polygon_list.list;

			-- Each fragment must be treated individually.
			-- This cursor points to a candidate fragment:
			fragment_cursor : pac_polygon_list.cursor;

			-- If a fragment is being processed then we store
			-- it here:
			fragment : type_polygon;

			-- Polygons caused by objects that are inside the 
			-- candidate fragment or that touch the fragment:
			polygons : pac_polygon_list.list;

			-- The polygons will be used to crop the fragment.
			-- This causes the fragment to disintegrate into
			-- so called "islands" of conducting material:
			islands : pac_polygon_list.list;

			-- Information about terminals that are to be 
			-- connected via thermal reliefes with the fragment:
			terminals_with_relief : pac_terminals_with_relief.list;


			-- This procedure generates the thermal reliefes
			-- if the zone is connected to a net. Otherwise
			-- nothing happens here:
			procedure make_thermal_reliefes is begin
				
				if has_element (parent_net) then
					log (text => "make_thermal_reliefes", level => log_threshold + 4);
					log_indentation_up;

					-- Delete old reliefes:
					reliefes.clear;
					
					reliefes := make_reliefes (
						zone				=> zone,
						relief_properties	=> relief_properties,								   
						terminals			=> terminals_with_relief,
						zone_clearance		=> clearance,
						zone_linewidth		=> linewidth,
						log_threshold		=> log_threshold + 5);

					-- CS log number of reliefes ?

					log_indentation_down;
				end if;
			end make_thermal_reliefes;


		begin
			-- Clip the zone by the given outer board contour.
			-- The board contour may assume all kindes of irregular shapes.
			-- This may cause the zone to disintegrate into smaller fragments.
			-- NOTE: Holes do NOT affect the follwing clip operation.
			log (text => "process_zone_fragments", level => log_threshold + 1);
			log_indentation_up;

			zone_fragments := clip (zone_polygon, outer_contour_tmp);

			log (text => "fragment count: " & get_count (zone_fragments),
				level => log_threshold + 2);

			-- Because the border of the zone has a width,
			-- the islands must be shrinked by half the linewidth:
			-- offset_polygons (zone_fragments, - type_float_positive (linewidth) * 0.5);

			log_indentation_up;

			fragment_cursor := zone_fragments.first;

			while has_element (fragment_cursor) loop
				log (text => "process fragment", level => log_threshold + 3);
				log_indentation_up;

				fragment := element (fragment_cursor);

				-- Get the polygons of all objects in the
				-- candidate fragment:
				get_touching_polygons (
					module_cursor		=> module_cursor,
					zone_polygon		=> fragment,
					zone_clearance		=> clearance,
					linewidth			=> linewidth,									 
					layer				=> layer,
					parent_net			=> parent_net,
					terminal_connection	=> terminal_connection,
					clearance_to_edge	=> clearance_to_edge,
					polygons				=> polygons,
					terminals_with_relief	=> terminals_with_relief,
					log_threshold		=> log_threshold + 4);


				log (text => "crop fragment with" & get_count (polygons) & " polygons",
					level => log_threshold + 4);

				-- After cropping the fragment with the polygons
				-- even more fragments result. We call them "islands":
				islands := multi_crop_1 (
					polygon_B		=> fragment,
					polygon_A_list	=> polygons,
					debug			=> false);

				log (text => "resulting islands:" & get_count (islands),
					level => log_threshold + 4);

				make_islands_and_lakes (
					zone			=> zone,
					linewidth		=> linewidth,
					islands			=> islands,
					lakes			=> polygons,
					--fill			=> true,
					fill			=> false,
					log_threshold	=> log_threshold + 4);


				make_thermal_reliefes; -- if the zone is connected with a net
				
				next (fragment_cursor);
				log_indentation_down;
			end loop;

			log_indentation_down;
			log_indentation_down;
		end process_zone_fragments;




		
	begin -- fill_zone
		log (text => "module " & to_string (module_cursor)
			& " fill_zone"
			& " linewidth: " & to_string (linewidth)
			& " layer: " & to_string (layer)
			& " isolation: " & to_string (clearance)
			& " clearance to edge: " & to_string (clearance_to_edge)
			& " parent net: dummy" -- CS
			& " terminal connection: dummy"  -- CS
			& " relief properties: dummy", -- CS 
			-- CS number of reliefes ?
			level => log_threshold);
			-- CS output with linebreaks

		log_indentation_up;

		-- log (text => "zone with corner nearest to origin:" 
		-- 	 & to_string (get_corner_nearest_to_origin (zone)),
		-- 	level => log_threshold + 1);

		
		-- Remove the old fill (incl. islands, lakes):
		zone.islands := no_islands;
		
		-- Convert the given zone to a polygon:
		log (text => "convert zone to polygon", level => log_threshold + 1);
		zone_polygon := to_polygon (zone, fill_tolerance, SHRINK); 
		-- NOTE: The SHRINK argument applies to the approximation mode of 
		-- arcs and circles. Has nothing to do with offsetting the zone.


		log (text => "offset (shrink) outer board contour by half linewidth of zone: "
			& to_string (half_linewidth_float),
			level => log_threshold + 1);

		offset_polygon (outer_contour_tmp, - half_linewidth_float, log_threshold + 2);


		log_indentation_up;
		process_zone_fragments;
		
		log_indentation_down;
		log_indentation_down;		
	end fill_zone;











	procedure fill_zones (
		module_cursor	: in pac_generic_modules.cursor;
		log_category	: in type_log_category;
		log_threshold	: in type_log_level;
		nets 			: in pac_net_names.list := no_net_names)
	is 
		use et_contour_to_polygon;
		use et_board_ops.board_contour;
		use et_fill_zones.boards;

		use pac_geometry_brd;
		use pac_polygons;		
		use pac_polygon_offsetting;
		
		use pac_net_names;

		all_zones : boolean;

		
		-- Get the design rules:
		design_rules : constant type_design_rules_board := 
			get_pcb_design_rules (module_cursor);

		clearance_conductor_to_edge : type_distance_positive renames 
			design_rules.clearances.conductor_to_board_edge;
		
		
		-- The outer contour of the board. After shrinking by the
		-- conductor-to-edge clearance this serves as master for
		-- filling zones of nets. Each net may have an individual setting for 
		-- the with of the fill lines.
		board_outer_contour : type_polygon;

		

		-- Temporarily storage for properties of zones connected with a net:
		relief_properties	: type_relief_properties;
		terminal_reliefes	: pac_reliefes.list;
		terminal_connection	: type_pad_connection := pad_connection_default;
		terminal_technology	: type_pad_technology := pad_technology_default;
		native_tracks_embedded : type_native_tracks_embedded := false;



		

		
		
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
				is 
					reliefes : pac_reliefes.list; -- always empty with floating zones
				begin
					fill_zone (
						module_cursor		=> module_cursor,
						zone				=> zone,
						outer_contour		=> board_outer_contour,
						linewidth			=> element (zone_cursor).linewidth,
						layer				=> zone.properties.layer,
						clearance			=> zone.isolation,
						clearance_to_edge	=> clearance_conductor_to_edge,
						terminal_connection	=> terminal_connection,
						relief_properties	=> relief_properties,
						reliefes			=> reliefes,
						log_threshold		=> log_threshold + 2);
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
				is 
					reliefes : pac_reliefes.list; -- always empty with floating zones
				begin
					fill_zone (
						module_cursor		=> module_cursor,
						zone				=> zone,
						outer_contour		=> board_outer_contour,
						linewidth			=> element (zone_cursor).linewidth,
						layer				=> zone.properties.layer,
						clearance			=> zone.isolation,
						clearance_to_edge	=> clearance_conductor_to_edge,
						terminal_connection	=> terminal_connection,
						relief_properties	=> relief_properties,
						reliefes			=> reliefes,
						log_threshold		=> log_threshold + 2);
			
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
							module_cursor		=> module_cursor,
							zone				=> zone,
							outer_contour		=> board_outer_contour,
							linewidth			=> element (zone_cursor).linewidth,
							layer				=> zone.properties.layer,
							clearance			=> get_greatest (zone.isolation, net_class.clearance),
							clearance_to_edge	=> clearance_conductor_to_edge,
							parent_net			=> net_cursor,
							terminal_connection	=> terminal_connection,
							relief_properties	=> relief_properties,
							reliefes			=> zone.reliefes,
							log_threshold		=> log_threshold + 3);
							

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
							module_cursor		=> module_cursor,
							zone				=> zone,
							outer_contour		=> board_outer_contour,
							linewidth			=> element (zone_cursor).linewidth,
							layer				=> zone.properties.layer,
							clearance			=> get_greatest (zone.isolation, net_class.clearance),
							clearance_to_edge	=> clearance_conductor_to_edge,
							parent_net			=> net_cursor,
							terminal_connection	=> terminal_connection,
							relief_properties	=> relief_properties,
							reliefes			=> zone.reliefes,
							log_threshold		=> log_threshold + 3);

							
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
				

				
				procedure query_given_net (gn : pac_net_names.cursor) is 
					use pac_net_name;
				begin
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

		
		
		
	begin -- fill_zones

		log (text => "module " & to_string (module_cursor)
			& " fill zones. Log category " & to_string (log_category),
			level => log_threshold);

		log_indentation_up;

		log (text => "convert outer board contour to polygon", level => log_threshold + 1);
		
		board_outer_contour := to_polygon (
			contour		=> get_outer_contour (module_cursor),
			mode		=> SHRINK,										 
			tolerance	=> fill_tolerance);

		
		-- Shrink the outer board edge by the conductor-to-edge clearance
		-- as given by the design rules:
		log (text => "offset (shrink) outer board contour by clearance to edge (DRU): "
			-- & enclose_in_quotes (dru_parameter_clearance_conductor_to_board_edge) 
			& to_string (- clearance_conductor_to_edge),
			level => log_threshold + 1);	
		
		offset_polygon (board_outer_contour, 
						type_float_model (- clearance_conductor_to_edge),
						log_threshold + 2);

		
		
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









	procedure add_zone (
		module_cursor	: in pac_generic_modules.cursor;
		zone			: in type_zone'class;
		log_threshold	: in type_log_level;
		net_name		: in pac_net_name.bounded_string := et_net_names.no_name)
	is
		use ada.tags;

		use et_nets;
		use pac_nets;
		use pac_net_name;

		use et_fill_zones.boards;		
		

		procedure floating_solid (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_floating_solid;

			p : type_floating_solid := 
				type_floating_solid (zone);
			
		begin
			log (text => to_string (p, p.properties),
				level => log_threshold + 1);

			module.board.conductors_floating.zones.solid.append (p);
		end floating_solid;

		
		procedure floating_hatched (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_floating_hatched;

			p : type_floating_hatched := 
				type_floating_hatched (zone);
			
		begin
			log (text => to_string (p, p.properties),
				level => log_threshold + 1);

			module.board.conductors_floating.zones.hatched.append (p);
		end floating_hatched;


		-- Polygons which are connected with a net are part of a route.
		-- They must be added to the targeted net. So we need a cursor
		-- to the targeted net:
		net_cursor : pac_nets.cursor;

		
		procedure locate_targeted_net is 
			use et_schematic_ops.nets;
		begin
			net_cursor := locate_net (module_cursor, net_name);

			if net_cursor = pac_nets.no_element then
				raise semantic_error_1 with
					"ERROR: Net " & enclose_in_quotes (to_string (net_name)) 
					& " does not exist !";
			end if;
		end locate_targeted_net;

		
		procedure route_solid (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_route_solid;

			p : type_route_solid := 
				type_route_solid (zone);


			procedure add_polygon (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is begin
				net.route.zones.solid.append (p);
			end add_polygon;
		
	
		begin --route_solid
			log (text => to_string (p, p.properties, net_name),
				level => log_threshold + 1);

			update_element (
				container	=> module.nets,
				position	=> net_cursor,
				process		=> add_polygon'access);
			
		end route_solid;

		
		procedure route_hatched (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_route_hatched;

			p : type_route_hatched := 
				type_route_hatched (zone);
		
	
			procedure add_polygon (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is begin
				net.route.zones.hatched.append (p);
			end add_polygon;


		begin
			log (text => to_string (p, p.properties, net_name),
				level => log_threshold + 1);

			update_element (
				container	=> module.nets,
				position	=> net_cursor,
				process		=> add_polygon'access);

		end route_hatched;

		
	begin -- add_zone
		log (text => "module " & to_string (module_cursor)
			& " placing fill zone in conductor layer ...",
			level => log_threshold);

		log_indentation_up;
		
		-- floating:
		if zone'tag = type_floating_solid'tag then

			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> floating_solid'access);

		elsif zone'tag = type_floating_hatched'tag then

			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> floating_hatched'access);


		-- route:
		elsif zone'tag = type_route_solid'tag then

			locate_targeted_net;
						
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> route_solid'access);

		elsif zone'tag = type_route_hatched'tag then

			locate_targeted_net;

			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> route_hatched'access);
			
		else
			null; -- CS ?
		end if;
		
		log_indentation_down;
	end add_zone;










	procedure clear_zones (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level;
		nets 			: in pac_net_names.list := no_net_names)
	is 
		-- CS: Most of this stuff can be moved to et_fill_zones.boards
		-- so that solid and hatched zones inherit from primitive operations
		-- defined for type_zone. 
		
		use et_fill_zones.boards;
		use pac_geometry_brd;
		use pac_polygons;
		use pac_polygon_clipping;
		use pac_polygon_cropping;
		use pac_polygon_offsetting;
		use pac_polygon_union;
		
		use pac_net_names;

		all_zones : boolean;

		

		
		procedure floating_zones is
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
					zone.islands.clear;
				end do_it;

			begin
				while zone_cursor /= pac_floating_solid.no_element loop
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
					zone.islands.clear;
				end do_it;
				
			begin
				while zone_cursor /= pac_floating_hatched.no_element loop
					module.board.conductors_floating.zones.hatched.update_element (zone_cursor, do_it'access);
					next (zone_cursor);
				end loop;
			end floating_hatched;

			
		begin
			log (text => "floating zones ...", level => log_threshold + 1);
			update_element (generic_modules, module_cursor, floating_solid'access);
			update_element (generic_modules, module_cursor, floating_hatched'access);
		end floating_zones;


		
		-- Fills polygons that are connected with a net:
		procedure connected_zones is
			use et_nets;
			use pac_net_name;
			use pac_nets;
			use pac_route_solid;
			use pac_route_hatched;

			
			procedure query_nets (
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
					is begin
						zone.islands.clear;
						zone.reliefes.clear;
					end do_it;
					
					
				begin -- route_solid
					while zone_cursor /= pac_route_solid.no_element loop

						-- do the clearing
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
						zone.islands.clear;
						zone.reliefes.clear;
					end do_it;
						
					
				begin
					while zone_cursor /= pac_route_hatched.no_element loop

						-- do the clearing
						net.route.zones.hatched.update_element (zone_cursor, do_it'access);

						next (zone_cursor);
					end loop;
				end route_hatched;


				procedure query_net is begin
					log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 2);
					
					log_indentation_up;
					
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


		
	begin -- clear_zones

		log (text => "module " 
			& enclose_in_quotes (to_string (key (module_cursor)))
			& " clearing zones.",
			level => log_threshold);

		log_indentation_up;

		
		if is_empty (nets) then
			
			-- Clear all zones if no explicit net names given:
			
			log (text => "clearing all zones ...", level => log_threshold + 1);

			all_zones := true;
			
			log_indentation_up;
			connected_zones;

			floating_zones;

			log_indentation_down;
						
		else
			log (text => "clearing zones of dedicated nets ...", level => log_threshold + 1);

			all_zones := false;
			
			log_indentation_up;
			connected_zones;
			log_indentation_down;
			
		end if;

		log_indentation_down;
		
	end clear_zones;

	
end et_board_ops.fill_zones;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
