------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                  BOARD OPERATIONS / CLEARING FILL ZONES                  --
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

with ada.strings;					use ada.strings;
with ada.exceptions;

with et_exceptions;					use et_exceptions;


separate (et_board_ops.conductors)

procedure clear_zones (
	module_cursor	: in pac_generic_modules.cursor;
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

	

	
	procedure floating_zones is
		use pac_floating_solid;
		use pac_floating_hatched;
		

		procedure floating_solid (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			zone_cursor : pac_floating_solid.cursor := module.board.conductors.fill_zones.solid.first;

			procedure do_it (
				zone : in out type_floating_solid)
			is begin
				zone.islands.clear;
			end do_it;

		begin
			while zone_cursor /= pac_floating_solid.no_element loop
				module.board.conductors.fill_zones.solid.update_element (zone_cursor, do_it'access);
				next (zone_cursor);
			end loop;
		end floating_solid;

		
		procedure floating_hatched (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			zone_cursor : pac_floating_hatched.cursor := module.board.conductors.fill_zones.hatched.first;

			procedure do_it (
				zone : in out type_floating_hatched)
			is begin
				zone.islands.clear;
			end do_it;
			
		begin
			while zone_cursor /= pac_floating_hatched.no_element loop
				module.board.conductors.fill_zones.hatched.update_element (zone_cursor, do_it'access);
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
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
