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


with et_mirroring;
with et_schematic_ops.units;
with et_pcb_contour;
with et_board_text;
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

	-- use pac_polygon_clipping;
	-- use pac_polygon_cropping;
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

				terminal_zone_overlap : type_overlap_status;
				
				-- CS: more log messages
			begin
				log (text => "terminal " & get_terminal_name (terminal_cursor),
					level => log_threshold + 2);
					
				log_indentation_up;				
				
				-- If the terminal does not affect the current signal layer,
				-- then nothing happens here. Otherwise the outline of the terminal
				-- will be appended to the result:
				if terminal_polygon.exists then
					polygons.append (terminal_polygon.polygon);

					-- If the terminals of this net require thermal reliefes, then
					-- collect the necessary information:
					if with_reliefes then

						-- Do a preselection of those terminals that are overlapping
						-- the given zone or are inside the given zone:
						terminal_zone_overlap := get_overlap_status (
							polygon_A => terminal_polygon.polygon,
							polygon_B => zone);

						case terminal_zone_overlap is
							when A_INSIDE_B | A_OVERLAPS_B =>
								terminals_with_relief.append ((
									position => terminal_polygon.position, -- in the board
									outline	 => terminal_polygon.polygon, -- in the board
									terminal => terminal_cursor));  -- in the package model

							when others => null;
						end case;
					end if;

				end if;		
				
				log_indentation_down;
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
		
		half_linewidth_float : constant type_float_positive := 
			type_float_positive (linewidth * 0.5);

			
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
				
				-- The clearance between net and zone is either the given zone_clearance
				-- or the clearance of the net itself. The greatest of them is applied:				
				clearance : constant type_track_clearance := 
					get_greatest (zone_clearance, net_class.clearance);

				-- The polygons of the candidate net are collected here
				-- (later they will be appended to the output polygons):
				polygons_of_candidate_net : pac_polygon_list.list;

				use et_route;
				route : type_net_route renames element (net_cursor).route;

				
				procedure query_line (l : in pac_conductor_lines.cursor) is
					use pac_conductor_lines;
					line : type_conductor_line renames element (l);
				begin
					if line.layer = layer then
						polygons_of_candidate_net.append (to_polygon (line, fill_tolerance));
						-- CS expand and test whether it affects the zone
					end if;
				end query_line;

				
				
				procedure query_arc (a : in pac_conductor_arcs.cursor) is
					use pac_conductor_arcs;
					arc : type_conductor_arc renames element (a);
				begin
					if arc.layer = layer then
						polygons_of_candidate_net.append (to_polygon (arc, fill_tolerance));
						-- CS expand and test whether it affects the zone
					end if;
				end query_arc;

				
				use et_vias;
				use pac_vias;
				
				procedure query_via (v : in pac_vias.cursor) is
					use pac_vias;
					via : type_via renames element (v);
				begin
					-- CS expand and test whether it affects the zone
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



				
				-- Converts tracks, vias and terminals to polygons,
				-- expands them by the clearance and appends them to
				-- the polygons:
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

					-- Get terminal polygons of device packages
					-- which are connected with the candidate net:
					get_polygons_of_connected_terminals (
						module_cursor			=> module_cursor,
						layer_category			=> layer_category,
						zone					=> zone,
						net_cursor				=> net_cursor,
						polygons				=> polygons_of_candidate_net,
						with_reliefes			=> collect_terminals_with_relief,
						terminals_with_relief	=> reliefes,
						log_threshold			=> log_threshold + 6);
					

					-- expand polygons by clearance
					offset_polygons (polygons_of_candidate_net, 
									 half_linewidth_float + type_float_positive (clearance));
					-- CS remove

					polygons.splice (before => pac_polygon_list.no_element, 
								   source => polygons_of_candidate_net);
					
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
	
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			use et_devices_electrical;

			offset : constant type_float_positive := 
				type_float_positive (linewidth * 0.5 + zone_clearance);
			-- CS function to_offset (linewidth, zone_clearance)
			-- might already be available
			
			
			procedure query_device (
				device_cursor : in pac_devices_electrical.cursor)
			is
				terminals : pac_terminals.map;
				
				
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
						-- CS test whether zone is affected

						-- Expand the polygon by offset:
						offset_polygon (terminal_polygon.polygon, offset);

						-- Append the polygon to the result:
						polygons.append (terminal_polygon.polygon);
					end if;
					
				end query_terminal;
				
				
			begin
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
			-- CS log offset
			module.devices.iterate (query_device'access);
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " get_polygons_of_unconnected_terminals"
			& " layer cat: " & to_string (layer_category),
			level => log_threshold);
		-- CS log arguments
		
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

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			use et_devices_non_electrical;
			use pac_devices_non_electrical;


			offset : constant type_float_positive := 
				type_float_positive (linewidth * 0.5 + zone_clearance);
			-- CS function to_offset (linewidth, zone_clearance)
			-- might already be available


			
			procedure query_device (d : in pac_devices_non_electrical.cursor) is
				use et_pcb_contour;
				p : pac_polygon_list.list;
			begin
				log (text => "device " & get_device_name (d),
					 level => log_threshold + 1);

				log_indentation_up;
				
				-- conductors: such as terminals, text, lines, arcs, circles
				p := get_conductor_polygons (d, layer_category);
				offset_polygons (p, offset);
				log (text => "conductors" & get_count (p), level => log_threshold + 2);

				-- Exract those which are inside the given zone
				-- or which overlap the given zone
				-- and append them to the result:
				append (polygons, get_polygons (zone, p, overlap_mode_1));
				
				-- holes:
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
			module.devices_non_electric.iterate (query_device'access);
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " get_polygons_of_non_electrical_devices"
			& " layer cat: " & to_string (layer_category)
			& " zone clearance: " & to_string (zone_clearance)
			& " zone linewidth: " & to_string (linewidth)
			& " zone clearance to edge: " & to_string (clearance_to_edge),
			level => log_threshold);

		
		log_indentation_up;
	
		query_element (module_cursor, query_module'access);

		-- CS log number of polygons
		
		log_indentation_down;
	end get_polygons_of_non_electrical_devices;
	




	


	procedure get_polygons_of_board_texts (
		module_cursor			: in pac_generic_modules.cursor;
		zone					: in pac_polygons.type_polygon;
		zone_clearance			: in type_track_clearance;
		linewidth				: in type_track_width;
		layer 					: in type_signal_layer;
		polygons				: in out pac_polygons.pac_polygon_list.list;
		log_threshold			: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			use et_conductor_text.boards;
			use pac_conductor_texts;

			
			procedure query_text (t : in pac_conductor_texts.cursor) is 
				text : type_conductor_text renames element (t);
				borders : pac_polygon_list.list;

				offset : type_float_positive;
				use et_board_text;
				use pac_text_board;
			begin
				if text.layer = layer then

					borders := get_borders (text.vectors);

					offset := type_float_positive (linewidth * 0.5 + zone_clearance);
					-- CS function to_offset (linewidth, zone_clearance)
					-- might already be available
					
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
			& " layer: " & to_string (layer),
			level => log_threshold);
		-- CS more log messages

		log_indentation_up;
	
		query_element (module_cursor, query_module'access);

		-- CS log number of polygons
		
		log_indentation_down;
	end get_polygons_of_board_texts;
	



	
end et_board_ops.fill_zones;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
