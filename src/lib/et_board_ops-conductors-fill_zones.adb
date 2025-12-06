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
	use pac_polygons;
	
	use pac_geometry_brd;
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
	log (text => "offset by clearance to edge (DRU) "
		-- & enclose_in_quotes (dru_parameter_clearance_conductor_to_board_edge) 
		& to_string (- clearance_conductor_to_edge),
		level => log_threshold + 1);
	
	offset_polygon (board_outer_contour, type_float_model (- clearance_conductor_to_edge));

	
	
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
