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

with ada.tags;

with et_mirroring;					use et_mirroring;
with et_schematic_ops.units;		use et_schematic_ops.units;
with et_schematic_ops.nets;			use et_schematic_ops.nets;
with et_schematic_ops;				use et_schematic_ops;
with et_board_ops.devices;			use et_board_ops.devices;
with et_board_ops.ratsnest;			use et_board_ops.ratsnest;

with et_fill_zones.boards;			use et_fill_zones.boards;

with et_devices_electrical;

with et_vias;
with et_route;



package body et_board_ops.fill_zones is

	use pac_nets;


	
	
	
	procedure get_polygons_of_nets (
		module_cursor			: in pac_generic_modules.cursor;
		layer_category 			: in type_signal_layer_category;
		zone					: in et_board_geometry.pac_polygons.type_polygon;
		linewidth				: in type_track_width;
		layer 					: in type_signal_layer;
		zone_clearance			: in type_track_clearance;
		bottom_layer			: in type_signal_layer;
		parent_net				: in pac_nets.cursor;
		result					: in out et_board_geometry.pac_polygons.pac_polygon_list.list;
		terminal_connection		: in type_pad_connection;
		terminals_with_relief	: out pac_terminals_with_relief.list;
		log_threshold			: in type_log_level)
	is 
		use pac_geometry_brd;
		use et_board_geometry.pac_polygons;
		
		
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
				-- The clearance between net and zone is either the given zone_clearance
				-- or the clearance of the net itself. However, the greater value is applied:
				net_class : constant type_net_class := get_net_class (module_cursor, net_cursor);
				
				clearance : constant type_track_clearance := 
					get_greatest (zone_clearance, net_class.clearance);

				-- The polygons of the candidate net are collected here
				-- (later they will be appended to the result):
				polygons : pac_polygon_list.list;

				use et_route;
				route : type_net_route renames element (net_cursor).route;

				
				procedure query_line (l : in pac_conductor_lines.cursor) is
					use pac_conductor_lines;
					line : type_conductor_line renames element (l);
				begin
					if line.layer = layer then
						polygons.append (to_polygon (line, fill_tolerance));
					end if;
				end query_line;

				
				
				procedure query_arc (a : in pac_conductor_arcs.cursor) is
					use pac_conductor_arcs;
					arc : type_conductor_arc renames element (a);
				begin
					if arc.layer = layer then
						polygons.append (to_polygon (arc, fill_tolerance));
					end if;
				end query_arc;

				
				use et_vias;
				use pac_vias;
				
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



				
				-- Converts tracks, vias and terminals to polygons,
				-- expands them by the clearance and appends them to
				-- the result:
				procedure convert_conductor_objects_to_polygons is 
					use pac_polygon_offsetting;
					terminals : pac_polygon_list.list;
					reliefes : pac_terminals_with_relief.list;
				begin
					-- Query track segments:
					route.lines.iterate (query_line'access);
					route.arcs.iterate (query_arc'access);
					
					-- Query vias:
					route.vias.iterate (query_via'access);
					
					-- CS evaluate native_tracks_embedded ?

					
					-- CS fill zones, ... see et_route.type_route

					-- Get terminal polygons of device packages
					-- which are connected with the candidate net:
					get_terminal_polygons (
						module_cursor			=> module_cursor,
						layer_category			=> layer_category,
						zone_polygon			=> zone,
						net_cursor				=> net_cursor,
						terminal_polygons		=> terminals,
						with_reliefes			=> collect_terminals_with_relief,
						terminals_with_relief	=> reliefes,
						log_threshold			=> log_threshold + 6);

											
					polygons.splice (before => pac_polygon_list.no_element, source => terminals);

					-- expand polygons by clearance
					offset_polygons (polygons, half_linewidth_float + type_float_positive (clearance));

					result.splice (before => pac_polygon_list.no_element, source => polygons);
					
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

		log_indentation_down;

	end get_polygons_of_nets;

	

end et_board_ops.fill_zones;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
