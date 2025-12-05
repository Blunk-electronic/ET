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
with ada.characters.latin_1;		use ada.characters.latin_1;

with et_mirroring;
with et_schematic_ops.units;
with et_pcb_contour;
with et_board_text;
with et_board_ops.board_contour;
with et_board_ops.devices;			use et_board_ops.devices;
with et_fill_zones.boards;			use et_fill_zones.boards;

with et_net_ports;
with et_devices_electrical.packages;
with et_devices_non_electrical;

with et_conductor_text.boards;
with et_conductor_segment.boards;	use et_conductor_segment.boards;
with et_contour_to_polygon;			use et_contour_to_polygon;
with et_vias;
with et_route;



package body et_board_ops.fill_zones is

	use pac_nets;
	use pac_net_name;

	use pac_polygon_offsetting;
	-- use pac_polygon_union;

	
	function to_polygon (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_electrical.cursor;
		terminal_cursor	: in pac_terminals.cursor;
		layer_category	: in type_signal_layer_category;
		tolerance		: in type_distance_positive)
		return type_terminal_polygon
	is
		exists : boolean := false;
		result : type_polygon; -- to be returned

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
				offset_polygon (result, type_float_model (terminal.width_inner_layers));
			end if;
		end finalize;
				
		
	begin -- to_polygon

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
					layer_category, fill_tolerance);
				-- CS difficult to debug. move to a subprocedure

				terminal_polygon_expanded : type_polygon;

				terminal_zone_overlap : type_overlap_status;
				
				-- CS: more log messages
			begin
				-- If the terminal does not affect the current signal layer,
				-- then nothing happens here. Otherwise the outline of the terminal
				-- will be appended to the result:
				if terminal_polygon.exists then

					terminal_polygon_expanded := terminal_polygon.polygon;
					offset_polygon (terminal_polygon_expanded, offset);

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
			& " thermal " & boolean'image (with_reliefes),
			level => log_threshold);
		-- CS log zone ?

		log_indentation_up;

		-- Get the ports of ALL devices connected with the given net.
		-- Therefore we do not pass a specific assembly variant here.
		ports := get_ports (net_cursor); 

		-- In variable "ports" we are interested in selector "devices" exclusively.
		-- Submodule ports and netchangers are just virtual devices
		-- that connect two conductor tracks. They can therefore be ignored:
		ports.devices.iterate (query_device_port'access);

		-- CS log number of polygons
		
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
				
				-- 1. The tracks and the vias of the candidate net are 
				--    first converted to polygons and appended to 
				--    polygons_of_candidate_net. 
				-- 2. After that the polygons_of_candidate_net are expanded
				--    by offset.
				-- 3. Now those polygon which are inside the zone or which
				--    touch the zone are extracted from polygons_of_candidate_net.
				-- 4. The polygons_of_candidate_net are then appended
				--    to the result.
				-- 5. Then the terminals of connected device terminals
				--    are collected and also added to polygons_of_candidate_net.

				-- The polygons of the candidate net are collected here:
				polygons_of_candidate_net : pac_polygon_list.list;

				-- Information about routed stuff of the candidate net
				-- is stored here (lines, arcs, vias, ...):
				use et_route;
				route : type_net_route renames element (net_cursor).route;

				
				-- This procedure queries a conductor line, converts it
				-- to a polygon and appends it to the polygons_of_candidate_net:
				procedure query_line (l : in pac_conductor_lines.cursor) is
					use pac_conductor_lines;
					line : type_conductor_line renames element (l);
				begin
					if line.layer = layer then
						polygons_of_candidate_net.append (to_polygon (line, fill_tolerance));
					end if;
				end query_line;

				
				-- This procedure queries a conductor arc, converts it
				-- to a polygon and appends it to the polygons_of_candidate_net:				
				procedure query_arc (a : in pac_conductor_arcs.cursor) is
					use pac_conductor_arcs;
					arc : type_conductor_arc renames element (a);
				begin
					if arc.layer = layer then
						polygons_of_candidate_net.append (to_polygon (arc, fill_tolerance));
					end if;
				end query_arc;

				
				use et_vias;
				use pac_vias;
				
				-- This procedure queries a via, converts it
				-- to a polygon and appends it to the polygons_of_candidate_net:
				procedure query_via (v : in pac_vias.cursor) is
					use pac_vias;
					via : type_via renames element (v);
				begin
					-- CS use function via_to_polyon 
					case via.category is
						when THROUGH =>
							if layer_category = OUTER_TOP or layer_category = OUTER_BOTTOM then
								polygons_of_candidate_net.append (to_polygon (via.position, via.restring_outer, via.diameter, fill_tolerance));
							else
								polygons_of_candidate_net.append (to_polygon (via.position, via.restring_inner, via.diameter, fill_tolerance));
							end if;

						when BLIND_DRILLED_FROM_TOP =>
							if layer_category = OUTER_TOP then
								polygons_of_candidate_net.append (to_polygon (via.position, via.restring_top, via.diameter, fill_tolerance));
							elsif blind_via_uses_layer (via, layer, bottom_layer) then
								polygons_of_candidate_net.append (to_polygon (via.position, via.restring_inner, via.diameter, fill_tolerance));
							end if;

						when BLIND_DRILLED_FROM_BOTTOM =>
							if layer_category = OUTER_BOTTOM then
								polygons_of_candidate_net.append (to_polygon (via.position, via.restring_bottom, via.diameter, fill_tolerance));
							elsif blind_via_uses_layer (via, layer, bottom_layer) then
								polygons_of_candidate_net.append (to_polygon (via.position, via.restring_inner, via.diameter, fill_tolerance));
							end if;
							
						when BURIED =>
							if layer_category = INNER and then
							buried_via_uses_layer (via, layer) then
								polygons_of_candidate_net.append (to_polygon (via.position, via.restring_inner, via.diameter, fill_tolerance));
							end if;
					end case;
				end query_via;



				
				-- Converts tracks, vias and terminals to polygons:
				procedure convert_conductor_objects_to_polygons is 
					use pac_polygon_offsetting;
					reliefes : pac_terminals_with_relief.list;
				begin
					-- Query track segments:
					route.lines.iterate (query_line'access);
					route.arcs.iterate (query_arc'access);
					
					-- Query vias:
					route.vias.iterate (query_via'access);
					
					-- CS evaluate native_tracks_embedded ?
					-- CS foreign fill zones, ... see et_route.type_route

					-- Expand the polygons which we have collected
					-- so far by the net specific offset:
					offset_polygons (polygons_of_candidate_net, offset);

					-- Extract those polygons which are inside the
					-- zone or which touch the zone:
					get_touching_polygons (zone, polygons_of_candidate_net);

					-- Append the polygons of the candidate net to the result:
					append (polygons, polygons_of_candidate_net);

					-- Get terminal polygons of device packages
					-- which are connected with the candidate net:
					get_polygons_of_connected_terminals (
						module_cursor			=> module_cursor,
						layer_category			=> layer_category,
						zone					=> zone,
						net_cursor				=> net_cursor,
						offset					=> offset,
						polygons				=> polygons,
						with_reliefes			=> collect_terminals_with_relief,
						terminals_with_relief	=> reliefes,
						log_threshold			=> log_threshold + 2);
					
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
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor) &
			" get_polygons_of_nets"
			& " layer category: " & to_string (layer_category)
			& " target layer " & to_string (layer)
			& " bottom layer " & to_string (bottom_layer)
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
						layer_category, fill_tolerance);
					-- CS difficult to debug. use subprocedure
				begin
					log (text => "terminal " & get_terminal_name (terminal_cursor),
						level => log_threshold + 2);
						
					if terminal_polygon.exists then
						-- CS more log messages

						-- Expand the polygon by offset:
						offset_polygon (terminal_polygon.polygon, offset);

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
						level => log_threshold + 1);
					
					log_indentation_up;
				
					terminals := get_unconnected_terminals (
						module_cursor, device_cursor, log_threshold + 2);

					terminals.iterate (query_terminal'access);
					
					log_indentation_down;
				end if;
			end query_device;
			
		
		begin
			-- Iterate through the electrical devices:
			module.devices.iterate (query_device'access);

			-- Extract those polygons from the temporarily collection
			-- which are inside the zone or which touch the zone:
			get_touching_polygons (zone, polygons_tmp);

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

		-- CS log number of polygons
		
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
			begin
				log (text => "device " & get_device_name (d),
					 level => log_threshold + 1);

				log_indentation_up;
				
				-- Process conductor objects:
				p := get_conductor_polygons (d, layer_category);
				offset_polygons (p, offset);
				log (text => "conductors" & get_count (p), level => log_threshold + 2);

				-- Exract those which are inside the given zone
				-- or which overlap the given zone
				-- and append them to the result:
				append (polygons, get_polygons (zone, p, overlap_mode_1));
				

				-- Process holes:
				p := get_hole_polygons (d);
				offset_holes (p, linewidth * 0.5 + clearance_to_edge);
				log (text => "holes" & get_count (p), level => log_threshold + 2);

				-- Exract those which are inside the given zone
				-- or which overlap the given zone
				-- and append them to the result:
				append (polygons, get_polygons (zone, p, overlap_mode_1));


				-- route restrict:
				p := get_route_restrict_polygons (d, layer_category);
				offset_polygons (p, type_float_positive (linewidth * 0.5));
				log (text => "route restrict" & get_count (p), level => log_threshold + 2);
				
				-- Exract those which are inside the given zone
				-- or which overlap the given zone
				-- and append them to the result:
				append (polygons, get_polygons (zone, p, overlap_mode_1));

				-- CS union ?

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
			begin
				log (text => "device " & get_device_name (d),
					 level => log_threshold + 1);

				log_indentation_up;
				
				-- Process conductor objects:
				p := get_conductor_polygons (d, layer_category);
				offset_polygons (p, offset);
				log (text => "conductors" & get_count (p), level => log_threshold + 2);

				-- Exract those which are inside the given zone
				-- or which overlap the given zone
				-- and append them to the result:
				append (polygons, get_polygons (zone, p, overlap_mode_1));
				

				-- Process holes:
				p := get_hole_polygons (d);
				offset_holes (p, linewidth * 0.5 + clearance_to_edge);
				log (text => "holes" & get_count (p), level => log_threshold + 2);

				-- Exract those which are inside the given zone
				-- or which overlap the given zone
				-- and append them to the result:
				append (polygons, get_polygons (zone, p, overlap_mode_1));


				-- route restrict:
				p := get_route_restrict_polygons (d, layer_category);
				offset_polygons (p, type_float_positive (linewidth * 0.5));
				log (text => "route restrict" & get_count (p), level => log_threshold + 2);
				
				-- Exract those which are inside the given zone
				-- or which overlap the given zone
				-- and append them to the result:
				append (polygons, get_polygons (zone, p, overlap_mode_1));

				-- CS union ?

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
			begin
				if text.layer = layer then

					borders := get_borders (text.vectors);
					
					offset_polygons (borders, offset);
					
					-- NOTE: The borders of the characters of the text should not overlap.
					-- Therefore there is no need for unioning the characters at this time.
					
					-- CS test whether zone is affected
					
					polygons.splice (
						before => pac_polygon_list.no_element,
						source => borders);

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
		
		log_indentation_down;
	end get_polygons_of_board_texts;
	







	function conductors_to_polygons (
		module_cursor		: in pac_generic_modules.cursor;
		zone_polygon		: in type_polygon;
		zone_clearance		: in type_track_clearance;
		linewidth			: in type_track_width;
		layer 				: in type_signal_layer;
		parent_net			: in pac_nets.cursor := pac_nets.no_element;
		terminal_connection	: in type_pad_connection;
		clearance_to_edge 	: in type_distance_positive;
		log_threshold		: in type_log_level)
		return type_conductor_to_polygons_result
	is
		result : type_conductor_to_polygons_result;

		layer_category : type_signal_layer_category;


		-- The deepest conductor layer towards bottom is 
		-- defined by the layer stack of the module:
		bottom_layer : constant type_signal_layer := 
			get_deepest_conductor_layer (module_cursor);


		-- If a parent net was given (via argument parent_net) then
		-- this will hold the actual net name like "GND".
		-- Otherwise it will be left empty:
		parent_net_name : pac_net_name.bounded_string;

		procedure set_parent_net_name is begin
			if parent_net /= pac_nets.no_element then
				parent_net_name := key (parent_net);
			end if;
		end set_parent_net_name;

	
		
		-- Extracts the polygons of all conductor objects which are 
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
				polygons				=> result.polygons,
				terminal_connection		=> terminal_connection,
				terminals_with_relief	=> result.terminals_with_relief,
				log_threshold			=> log_threshold + 2);
			
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
				polygons			=> result.polygons,
				log_threshold		=> log_threshold + 2);

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
				polygons		=> result.polygons,
				log_threshold	=> log_threshold + 2);
				
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
				polygons			=> result.polygons,
				log_threshold		=> log_threshold + 2);
				
			log_indentation_down;
		end process_non_electrical_devices;
		
		
		
		
		-- This procedure converts objects of electrial
		-- devices to polygons and appends them to the result:
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
				polygons			=> result.polygons,
				log_threshold		=> log_threshold + 2);
				
			log_indentation_down;
		end process_electrical_devices;
		
	
		
		-- This procedure converts cutout areas to polygons
		-- and appends them to the result.
		-- The purpose of cutout areas is to exempt
		-- certain areas of fill zones from being filled:
		procedure process_cutout_areas is 
		begin
			log (text => "cutout areas", level => log_threshold + 1);
			log_indentation_up;

			-- CS todo. 
			-- iterate global cutout areas in the 
			-- given signal layer.
			-- Test whether the given zone is affected

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

			log_indentation_down;
		end process_restrict_areas;



		-- This procedure converts holes to polygons
		-- and appends them to the result:
		procedure process_holes is 
			use et_board_ops.board_contour;
			use et_pcb_contour;
			
			holes : pac_holes.list;
			polygons : pac_polygon_list.list;
		begin
			log (text => "holes", level => log_threshold + 1);
			log_indentation_up;
			
			-- Get the holes of the module:
			holes := get_holes (module_cursor);

			-- Convert the holes to polygons:
			polygons := to_polygons (holes, fill_tolerance);

			-- Expand the polygons:
			offset_holes (polygons, linewidth * 0.5 + clearance_to_edge);

			-- Extract those polygons which are inside the
			-- zone or which touch the zone:
			get_touching_polygons (zone_polygon, polygons);

			-- Append the polygons to the result:
			append (result.polygons, polygons);
			log_indentation_down;
		end process_holes;


		
	begin	
		log (text => "module " & to_string (module_cursor)
			& " conductors_to_polygons",
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

		log_indentation_down;
		
		return result;
	end conductors_to_polygons;








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

		zone_polygon : type_polygon;
		
		use pac_polygon_clipping;
		use pac_polygon_cropping;


		use pac_polygon_list;
		islands : pac_polygon_list.list;

		restrict, cutouts : pac_polygon_list.list;
		conductors_to_polygons_result : type_conductor_to_polygons_result;
		cropping_basket : pac_polygon_list.list;

		half_linewidth_float : constant type_float_positive :=
			0.5 * type_float_positive (linewidth);

		
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
			use pac_islands;
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
			use pac_islands;
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
			reliefes.clear;
			
			reliefes := make_reliefes (
				zone				=> zone,
				relief_properties	=> relief_properties,								   
				terminals			=> conductors_to_polygons_result.terminals_with_relief,
				zone_clearance		=> clearance,
				zone_linewidth		=> linewidth);

		end make_thermal_reliefes;

		
		
	begin -- fill_zone
		log (text => "module " & to_string (module_cursor)
			& " fill_zone",
			-- CS
			-- & " layer: " & to_string (layer)
			-- & " zone clearance: " & to_string (zone_clearance)
			-- & " zone linewidth: " & to_string (linewidth)
			level => log_threshold);


		log_indentation_up;

		-- log (text => "zone with corner nearest to origin:" 
		-- 	 & to_string (get_corner_nearest_to_origin (zone)),
		-- 	level => log_threshold + 1);

		
		-- log_indentation_up;


		
		-- Remove the old fill (incl. islands, lakes):
		zone.islands := no_islands;
		
		-- Convert the contour of the given zone to a polygon:
		if debug then
			put_line (" convert contour to polygon");
		end if;
		
		zone_polygon := to_polygon (zone, fill_tolerance, SHRINK); 
		-- NOTE: The SHRINK argument applies to the approximation mode of 
		-- arcs and circles. Has nothing to do with the actual shrinking of the zone
		-- by the follwing statement.



		log (text => "shrinking zone", level => log_threshold + 1);

		-- The border of the zone is drawn with the given linewidth.
		-- The zone must be shrinked by half the linewidth so that the 
		-- outline of the border is congruent to the zone drawn by the operator:
		offset_polygon (zone_polygon, - type_float_positive (linewidth) * 0.5);

		-- CS log lowest left vertex


		-- Clip the fill zone by the given outer contour.
		-- The zone may shrink or disintegrate into smaller islands:
		islands := clip (zone_polygon, outer_contour);

		-- The holes inside the board area and inside the zone
		-- will crop the zone later. Since the zone border has a linewidth,
		-- the holes must be made greater than they acutually are:
		-- CS ? expand_holes (zone_polygon, linewidth); -- updates variable "holes"
		
		-- Crop the zone by the outer board edges and the holes (stored in
		-- variable "holes").
		-- As a result, the zone disintegrates. It gets fragmented into islands:
		-- CS ? islands := zone_to_polygons (
		-- 	zone		=> zone_polygon,
		-- 	line_width	=> linewidth);

		
		-- Now we start collecting contours of objects inside the zone.
		-- They will be put in the cropping basket.
		-- Later everything in the basket will be used to crop the islands (of the zone)
		-- and to create inner borders inside the islands:
		-- CS ? empty_basket (cropping_basket);
		
		-- Collect holes in basket:
		-- CS ? put_into_basket (cropping_basket, holes);

		
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
			clearance_to_edge	=> clearance_to_edge,
			log_threshold		=> log_threshold + 1);

		
		islands := multi_crop_2 (
			polygon_B_list	=> islands,
			polygon_A_list	=> conductors_to_polygons_result.polygons,
			debug			=> false);
			

		-- -- Union the content of the cropping basket as much as possible:
		-- if debug then
		-- 	put_line (" union cropping basket");
		-- end if;
		
		--multi_union (cropping_basket, debug);
		-- CS ? multi_union (cropping_basket);
		
		-- Now the basket is ready to crop its content with the islands.
		-- The result are even more islands:

		--put_line ("B");


		-- CS: experimental
		--multi_union_2 (cropping_basket, debug); -- debug messages on
		--multi_union (cropping_basket);

		-- if debug then
		-- 	put_line (" crop islands with cropping basket");
		-- end if;
		
		-- CS ? islands := multi_crop_2 (
		-- 	polygon_B_list	=> islands,
		-- 	polygon_A_list	=> cropping_basket,
		-- 	debug			=> false);


		
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



	
end et_board_ops.fill_zones;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
