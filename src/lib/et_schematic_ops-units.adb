------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    SCHEMATIC OPERATIONS ON UNITS                         --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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
--   ToDo: 

with ada.exceptions;

package body et_schematic_ops.units is

	use et_symbols.pac_geometry_2;
	use et_symbols.pac_text;
	
	use pac_generic_modules;

	procedure delete_unit (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		log_threshold	: in type_log_level) is
		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.pac_devices_sch;
			device_cursor : et_schematic.pac_devices_sch.cursor;

			-- temporarily storage of unit coordinates.
			-- There will be only one unit in this container.
			position_of_unit : pac_unit_positions.map;

			ports : et_symbols.pac_ports.map;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in out type_device_sch) is
				use et_schematic.pac_units;
				unit_cursor : et_schematic.pac_units.cursor;
			begin
				if contains (device.units, unit_name) then
					-- locate unit by its name
					unit_cursor := find (device.units, unit_name);

					-- Load the single unit position and insert in container "position_of_unit"
					pac_unit_positions.insert (
						container	=> position_of_unit, 
						key			=> unit_name,
						new_item	=> element (unit_cursor).position);

					log_unit_positions (position_of_unit, log_threshold);
					
					-- delete the unit
					delete (device.units, unit_name);
				else
					unit_not_found (unit_name);
				end if;
			end query_units;
			
			units_invoked : boolean := true; -- goes false if no unit used anymore

			procedure query_number_of_invoked_units (
				device_name	: in type_device_name;
				device		: in type_device_sch) is
				use et_schematic.pac_units;
			begin
				if length (device.units) = 0 then
					units_invoked := false;
				end if;
			end query_number_of_invoked_units;

		begin -- query_devices
			if contains (module.devices, device_name) then

				-- Before the actual deletion, the coordinates of the
				-- unit must be fetched. These coordinates will later assist
				-- in deleting the port names from connected net segments.
				device_cursor := find (module.devices, device_name); -- the device should be there

				-- locate the unit, load position and then delete the targeted unit
				update_element (
					container	=> module.devices,
					position	=> device_cursor,
					process		=> query_units'access);
				
				log_indentation_up;

				-- Fetch the ports of the unit to be deleted.
				ports := ports_of_unit (device_cursor, unit_name);
				
				-- Delete the ports of the targeted unit from module.nets
				delete_ports (
					module			=> module_cursor,
					device			=> device_name,
					ports			=> ports,
					sheets			=> position_of_unit, -- there is only one unit -> only one sheet to look at
					log_threshold	=> log_threshold);

				-- In case no more units are invoked then the device must be
				-- deleted entirely from module.devices.
				-- First we query the number of still invoked units. If none invoked,
				-- the flag units_invoked goes false.
				query_element (
					position	=> device_cursor,
					process		=> query_number_of_invoked_units'access);

				if not units_invoked then
					delete (module.devices, device_cursor);
				end if;
				
				log_indentation_down;				
			else
				device_not_found (device_name);
			end if;
		end query_devices;
		
	begin -- delete_unit

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end delete_unit;
	
	procedure delete_unit (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module being modified
		
	begin -- delete_unit
		log (text => "module " & to_string (module_name) &
			 " deleting " & to_string (device_name) & " unit " & 
			 to_string (unit_name) & " ...", level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		delete_unit (
			module_cursor	=> module_cursor,
			device_name		=> device_name,
			unit_name		=> unit_name,
			log_threshold	=> log_threshold + 1);

	end delete_unit;
	
	procedure move_unit (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.pac_devices_sch;
			device_cursor : et_schematic.pac_devices_sch.cursor;

			-- temporarily storage of unit coordinates.
			-- There will be only one unit in this container.
			position_of_unit_old : pac_unit_positions.map;

			position_of_unit_new : et_coordinates.type_position;

			ports : et_symbols.pac_ports.map;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in out type_device_sch) is
				use et_schematic.pac_units;
				unit_cursor : et_schematic.pac_units.cursor;

				procedure move_unit (
					unit_name	: in pac_unit_name.bounded_string;
					unit		: in out et_schematic.type_unit) is
					use et_coordinates;
				begin
					case coordinates is
						when ABSOLUTE =>
							-- build the new position while preserving rotation:
							unit.position := to_position (
								point		=> point, 
								sheet		=> type_sheet (sheet),
								rotation	=> rot (unit.position));

						when RELATIVE =>
							move (
								position	=> unit.position,
								offset		=> to_position_relative (point, sheet));
								-- rotation remains as it is
					end case;

					-- store new unit position
					position_of_unit_new := unit.position;
					
					exception
						when event: others =>
							log (ERROR, "coordinates invalid !", console => true); -- CS required more details
							log (text => ada.exceptions.exception_information (event), console => true);
							raise;
					
				end move_unit;
				
			begin -- query_units
				if contains (device.units, unit_name) then
					-- locate unit by its name
					unit_cursor := find (device.units, unit_name);

					-- load unit position and insert in container "position_of_unit_old"
					pac_unit_positions.insert (
						container	=> position_of_unit_old, 
						key			=> unit_name,
						new_item	=> element (unit_cursor).position);

					-- log old unit position
					log_unit_positions (position_of_unit_old, log_threshold + 1); -- there is only one unit
-- 					log (text => "position before " & 
-- 						 et_coordinates.to_string (
-- 							type_ports.first_element (positions)), level => log_threshold + 1);

					update_element (
						container	=> device.units,
						position	=> unit_cursor,
						process		=> move_unit'access);
					
				else
					unit_not_found (unit_name);
				end if;
			end query_units;

		begin -- query_devices
			if contains (module.devices, device_name) then

				-- Before the actual move, the coordinates of the
				-- unit must be fetched. These coordinates will later assist
				-- in deleting the port names from connected net segments.
				device_cursor := find (module.devices, device_name); -- the device should be there

				-- locate the unit, load current position, set new position
				update_element (
					container	=> module.devices,
					position	=> device_cursor,
					process		=> query_units'access);
				
				log_indentation_up;

				-- Fetch the ports of the unit to be moved.
				ports := ports_of_unit (device_cursor, unit_name);
				
				-- Delete the old ports of the targeted unit from module.nets
				delete_ports (
					module			=> module_cursor,
					device			=> device_name,
					ports			=> ports,
					sheets			=> position_of_unit_old,
					log_threshold	=> log_threshold + 1);

				-- Calculate the new positions of the unit ports:
				move_ports (ports, position_of_unit_new);

				-- Insert the new unit ports in the nets (type_module.nets):
				insert_ports (
					module			=> module_cursor,
					device			=> device_name,
					unit			=> unit_name,
					ports			=> ports,
					sheet			=> et_coordinates.sheet (position_of_unit_new),
					log_threshold	=> log_threshold + 1);
				
				log_indentation_down;				
			else
				device_not_found (device_name);
			end if;
		end query_devices;
		
	begin -- move_unit
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" moving " & to_string (device_name) & " unit " & 
					to_string (unit_name) & " to sheet" & to_sheet (sheet) &
					to_string (point), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" moving " & to_string (device_name) & " unit " & 
					to_string (unit_name) & " by" & to_sheet_relative (sheet) & " sheet(s)" &
					to_string (point), level => log_threshold);
		end case;
		
		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end move_unit;

	-- Drags the net segments according to the given drag_list of a unit.
	-- Changes the position of start or end points of segments.
	-- Does NOT create new connections with segments if a port
	-- lands on the start or end point of another segment.
	-- Does NOT create a new connection with a segments if a port
	-- lands between start and end point.
	procedure drag_net_segments (
		module			: in pac_generic_modules.cursor;-- the module
		drag_list		: in type_drags_of_ports.map;	-- the old and new port positions
		sheet			: in type_sheet;				-- the sheet to look at
		log_threshold	: in type_log_level) is

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is

			procedure query_net (net_cursor : in pac_nets.cursor) is
				use pac_nets;
				use et_symbols;

				procedure query_strands (
					net_name	: in pac_net_name.bounded_string;
					net			: in out type_net) is
					use et_coordinates;
					
					use pac_strands;
					strand_cursor : pac_strands.cursor;
					
					use type_drags_of_ports;
					drag_cursor : type_drags_of_ports.cursor := drag_list.first;

					drag_processed : boolean;

					-- We must keep record of segments that have been dragged already.
					-- Each time a segment has been dragged, it will be appended to
					-- this list:
					already_dragged_segments : pac_net_segments.list;
					
					procedure query_segments (strand : in out type_strand) is
						use pac_net_segments;

						procedure query_segment (segment_cursor : in pac_net_segments.cursor) is 

							-- Changes the position of start or end point of a segment 
							-- according to the drag point:
							procedure change_segment (segment : in out type_net_segment) is begin
								
								-- if port sits on a start point of a segment -> move start point
								if segment.start_point = element (drag_cursor).before then
									log (text => "move segment start point from" & 
										to_string (segment.start_point),
										level => log_threshold + 3);

									segment.start_point := element (drag_cursor).after;

									log (text => "to" & 
										to_string (segment.start_point),
										level => log_threshold + 3);

									-- Now the segment has been dragged. Store it
									-- in list of already dragged segments:
									already_dragged_segments.append (segment);
									
									drag_processed := true;
								end if;

								-- if port sits on an end point of a segment -> move end point
								if segment.end_point = element (drag_cursor).before then
									log (text => "move segment end point from" & 
										to_string (segment.end_point),
										level => log_threshold + 3);

									segment.end_point := element (drag_cursor).after;

									log (text => "to" & 
										to_string (segment.end_point),
										level => log_threshold + 3);

									-- Now the segment has been dragged. Store it
									-- in list of already dragged segments:
									already_dragged_segments.append (segment);
									
									drag_processed := true;
								end if;

							end change_segment;

						begin -- query_segment
							-- Probe only those segments which have not been dragged already:
							if not already_dragged_segments.contains (element (segment_cursor)) then
								
								log_indentation_up;
								log (text => "probing " & to_string (segment_cursor), level => log_threshold + 2);
								log_indentation_up;
								
								update_element (
									container	=> strand.segments,
									position	=> segment_cursor,
									process		=> change_segment'access);
												
								log_indentation_down;
								log_indentation_down;
								
							end if;
						end query_segment;
						
					begin -- query_segments
						-- Probe segments of this strand. Skip segments that have been
						-- dragged already:
						iterate (strand.segments, query_segment'access);

						-- Update strand position if any movement took place.
						if drag_processed then
							set_strand_position (strand); 
						end if;						
					end query_segments;
						
				begin -- query_strands
					-- loop in drag list
					while drag_cursor /= type_drags_of_ports.no_element loop
						log (text => "probing port " & to_string (key (drag_cursor)), level => log_threshold + 1);
						log_indentation_up;

						-- If the current drag point sits on a strand, this flag will go true. Other 
						-- strands will then not be looked at because the point can only sit on 
						-- one strand.
						drag_processed := false;
						
						strand_cursor := net.strands.first;
						while strand_cursor /= pac_strands.no_element loop
							
							-- We pick out only the strands on the targeted sheet:
							if et_coordinates.sheet (element (strand_cursor).position) = sheet then
								log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);

								log_indentation_up;
								log (text => "strand " & to_string (position => element (strand_cursor).position),
									level => log_threshold + 1);
							
								-- Iterate in segments of strand. If drag point sits on any segment
								-- the flag drag_processed goes true.
								update_element (
									container	=> net.strands,
									position	=> strand_cursor,
									process		=> query_segments'access);
							
								log_indentation_down;
							end if;

							-- If the drag point has been processed, there is no need to look up
							-- other strands for this port.
							if drag_processed then exit; end if;
							
							next (strand_cursor);
						end loop;

						log_indentation_down;
						next (drag_cursor);
					end loop;
						
				end query_strands;
				
			begin -- query_net
				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);
			end query_net;				
			
		begin -- query_nets
			pac_nets.iterate (module.nets, query_net'access);
		end query_nets;

	begin -- drag_net_segments
		log (text => "dragging net segments with units on sheet" & 
			 to_sheet (sheet) & " ...", level => log_threshold);
		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;
	end drag_net_segments;

	-- Tests whether the given unit ports at their individual location are movable. 
	-- The criteria for movement are: no netchanger port, no device port, no submodule ports there.
	-- The only port allowed at an individual drag point is the port-to-be-dragged itself.
	-- CS: Becomes obsolete once ports at the same x/y position are prevented.
	procedure movable_test (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name;
		unit_name		: in pac_unit_name.bounded_string;
		location 		: in et_coordinates.type_position; -- only sheet number matters
		unit_ports		: in et_symbols.pac_ports.map;
		log_threshold	: in type_log_level)
	is
		use et_symbols;
		use et_symbols.pac_ports;
		port_cursor : et_symbols.pac_ports.cursor := unit_ports.first;

		procedure test_point (port_cursor : in et_symbols.pac_ports.cursor) is
			point : et_coordinates.type_position; -- the point
			ports : type_ports;
			port : et_schematic.type_device_port;
			use pac_submodule_ports;
			use pac_device_ports;
			use et_netlists.pac_netchanger_ports;
		begin
			-- assemble the point to be probed
			point := to_position (
						point	=> element (port_cursor).position,
						sheet	=> sheet (location));
			
			-- If no net segments start or end at given point then this test won't
			-- complain. If segments are meeting this point, no other ports must be
			-- here (except the port-to-be-dragged):
			if net_segment_at_place (module_cursor, point) then

				-- There are net segments starting or ending at point.
				-- Make sure at point are no ports of devices, submodules or other 
				-- netchangers (except the unit port to be dragged):

				port := (device_name, unit_name, key (port_cursor)); -- IC12, CE
				
				-- Collect all ports of possible other devices, submodules and netchangers
				-- at given point:
				ports := ports_at_place (key (module_cursor), point, log_threshold + 1);

				-- If no netchanger and no submodule ports here:
				if is_empty (ports.netchangers) and is_empty (ports.submodules) then

					-- If the ONE and ONLY device/unit port is the 
					-- port-to-be-dragged then everything is fine.
					if length (ports.devices) = 1 then
						
						if contains (ports.devices, port) then
							null; -- fine -> movable test passed
						else
							-- there is another netchanger port
							dragging_not_possible (to_string (key (port_cursor)), point);
						end if;
					
					else
						-- there are more submodule ports
						dragging_not_possible (to_string (key (port_cursor)), point);
					end if;
					
				else -- device or netchanger ports here
					dragging_not_possible (to_string (key (port_cursor)), point);
				end if;
			end if;
		end test_point;
		
	begin -- movable_test
		log (text => "movable test ...", level => log_threshold);
		log_indentation_up;

		while port_cursor /= et_symbols.pac_ports.no_element loop
			test_point (port_cursor);
			next (port_cursor);
		end loop;
		
		log_indentation_down;
	end movable_test;

	function movable (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name;
		unit_name		: in pac_unit_name.bounded_string;
		location 		: in et_coordinates.type_position; -- only sheet number matters
		unit_ports		: in et_symbols.pac_ports.map;
		log_threshold	: in type_log_level)
		return boolean
	is
		result : boolean := false;
		
		use et_symbols;
		use et_symbols.pac_ports;
		port_cursor : et_symbols.pac_ports.cursor := unit_ports.first;

		procedure test_point (port_cursor : in et_symbols.pac_ports.cursor) is
			point : et_coordinates.type_position; -- the point
			ports : type_ports;
			port : et_schematic.type_device_port;
			use pac_submodule_ports;
			use pac_device_ports;
			use et_netlists.pac_netchanger_ports;
		begin
			-- assemble the point to be probed
			point := to_position (
						point	=> element (port_cursor).position,
						sheet	=> sheet (location));
			
			-- If no net segments start or end at given point then this test won't
			-- complain. If segments are meeting this point, no other ports must be
			-- here (except the port-to-be-dragged):
			if net_segment_at_place (module_cursor, point) then

				-- There are net segments starting or ending at point.
				-- Make sure at point are no ports of devices, submodules or other 
				-- netchangers (except the unit port to be dragged):

				port := (device_name, unit_name, key (port_cursor)); -- IC12, CE
				
				-- Collect all ports of possible other devices, submodules and netchangers
				-- at given point:
				ports := ports_at_place (key (module_cursor), point, log_threshold + 1);

				-- If no netchanger and no submodule ports here:
				if is_empty (ports.netchangers) and is_empty (ports.submodules) then

					-- If the ONE and ONLY device/unit port is the 
					-- port-to-be-dragged then everything is fine.
					if length (ports.devices) = 1 then
						
						if contains (ports.devices, port) then
							result := true; -- fine -> movable test passed
						else
							-- there is another netchanger port
							result := false;
						end if;
					
					else
						-- there are more submodule ports
						result := false;
					end if;
					
				else -- device or netchanger ports here
					result := false;
				end if;
			end if;
		end test_point;
		
	begin -- movable
		log (text => "movable test ...", level => log_threshold);
		log_indentation_up;

		while port_cursor /= et_symbols.pac_ports.no_element loop
			test_point (port_cursor);

			-- abort this loop as soon as a non-movable port has been detected:
			if result = false then
				exit;
			end if;
			
			next (port_cursor);
		end loop;
		
		log_indentation_down;

		return result;
	end movable;
	
	procedure drag_unit (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module being modified
		
		function make_drag_list ( 
		-- Merges the two maps ports_old and ports_new to a drag list.
		-- The resulting drag list tells which port is to be moved from old to new position.
			ports_old : in et_symbols.pac_ports.map;
			ports_new : in et_symbols.pac_ports.map) 
			return type_drags_of_ports.map is
			use type_drags_of_ports;
			drag_list : type_drags_of_ports.map;

			-- ports_old and ports_new are both equally long and contain 
			-- equal keys (the port names). So we use two cursors and advance them
			-- simultaneously in a loop (see below).
			use et_symbols.pac_ports;
			cursor_old : et_symbols.pac_ports.cursor := ports_old.first;
			cursor_new : et_symbols.pac_ports.cursor := ports_new.first;
		begin
			-- Loop in ports_old, copy the key to the drag list.
			-- Take the old position from ports_old and the new position from ports_new:
			while cursor_old /= et_symbols.pac_ports.no_element loop
				insert (
					container	=> drag_list,
					key			=> key (cursor_old), -- the port name
					new_item	=> (
								before	=> element (cursor_old).position, -- x/y
								after	=> element (cursor_new).position) -- x/y
					   );
				
				next (cursor_old);
				next (cursor_new);
			end loop;
			
			return drag_list;
		end make_drag_list;
		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.pac_devices_sch;
			device_cursor : et_schematic.pac_devices_sch.cursor;

			-- temporarily storage of unit coordinates.
			-- There will be only one unit in this container.
			--position_of_unit_old : pac_unit_positions.map;
			position_of_unit_old : et_coordinates.type_position;	
			position_of_unit_new : et_coordinates.type_position;

			ports, ports_old, ports_new : et_symbols.pac_ports.map;

			procedure query_unit_location (
				device_name	: in type_device_name;
				device		: in type_device_sch) is
				use et_schematic.pac_units;
				unit_cursor : et_schematic.pac_units.cursor;
			begin
				if contains (device.units, unit_name) then
					unit_cursor := find (device.units, unit_name); -- the unit should be there

					-- store old unit position
					position_of_unit_old := element (unit_cursor).position;
					log (text => "unit position old: " & to_string (position => position_of_unit_old), level => log_threshold + 1);
				else
					unit_not_found (unit_name);
				end if;
			end query_unit_location;
			
			procedure query_units (
				device_name	: in type_device_name;
				device		: in out type_device_sch) is
				use et_schematic.pac_units;
				unit_cursor : et_schematic.pac_units.cursor;

				procedure move_unit (
					unit_name	: in pac_unit_name.bounded_string;
					unit		: in out et_schematic.type_unit) 
				is
					use et_coordinates;

					-- Load the current sheet number where the unit is.
					-- NOTE: The sheet number does not change in drag operations.
					sheet : type_sheet := et_coordinates.sheet (unit.position);
				begin
					-- Set new x/y position. 
					-- Preserve sheet number and rotation.
					case coordinates is
						when ABSOLUTE =>

							unit.position := to_position (
								point		=> point, 
								sheet		=> sheet,
								rotation	=> rot (unit.position));
							
						when RELATIVE =>
							move_by (
								point	=> unit.position,
								offset	=> to_distance_relative (point));
					end case;
					
					exception
						when event: others =>
							log (ERROR, "coordinates invalid !", console => true); -- CS required more details
							log (text => ada.exceptions.exception_information (event), console => true);
							raise;
					
				end move_unit;
				
			begin -- query_units
				unit_cursor := find (device.units, unit_name); -- the unit should be there

				-- move the unit
				update_element (
					container	=> device.units,
					position	=> unit_cursor,
					process		=> move_unit'access);

				-- store new unit position
				position_of_unit_new := element (unit_cursor).position;
				
				log (text => "unit position new: " & to_string (position => position_of_unit_new), level => log_threshold + 1);
			end query_units;
			
		begin -- query_devices
			if contains (module.devices, device_name) then
				device_cursor := find (module.devices, device_name); -- the device should be there

				log_indentation_up;

				-- Before the actual drag, the coordinates of the
				-- unit must be fetched. These coordinates will later assist
				-- in changing the positions of connected net segments.
				
				-- locate the unit, store old position in position_of_unit_old
				query_element (
					position	=> device_cursor,
					process		=> query_unit_location'access);
				
				-- Fetch the ports of the unit to be moved. These are the default port positions
				-- (relative to the symbol origin) as they are defined in the library model.
				ports := ports_of_unit (device_cursor, unit_name);
				
				-- Calculate the old and new positions of the unit ports:
				ports_old := ports;
				rotate_ports (ports_old, rot (position_of_unit_old));
				move_ports (ports_old, position_of_unit_old); 
				-- ports_old now contains the absolute port positions in the schematic BEFORE the move.

				-- Test whether the ports of the unit can be dragged.
				-- CS: Might become obsolete once ports at the same x/y position are prevented.
				-- CS: Before the drag: If a port of the unit sits at the same place
				--     where a port of another unit is, then a net segment should be
				--     inserted between them ?
				movable_test (module_cursor, device_name, unit_name, 
					position_of_unit_old, ports_old, log_threshold + 1);

				-- locate the unit, move it, store new position in position_of_unit_new
				update_element (
					container	=> module.devices,
					position	=> device_cursor,
					process		=> query_units'access);
				
				ports_new := ports;
				rotate_ports (ports_new, rot (position_of_unit_new));
				move_ports (ports_new, position_of_unit_new);
				-- ports_new now contains the absolute port positions in the schematic AFTER the move.
				
				-- Change net segments in the affected nets (type_module.nets):
				drag_net_segments (
					module			=> module_cursor,
					drag_list		=> make_drag_list (ports_old, ports_new),
					sheet			=> et_coordinates.sheet (position_of_unit_new), -- or position_of_unit_old
					log_threshold	=> log_threshold + 1);

				-- The drag operation might result in new port-to-net connections.
				-- So we must insert new ports in segments.
				-- Insert possible new unit ports in the nets (type_module.nets):
				log_indentation_up;
				
				insert_ports (
					module			=> module_cursor,
					device			=> device_name,
					unit			=> unit_name,
					ports			=> ports_new,
					sheet			=> et_coordinates.sheet (position_of_unit_new),
					log_threshold	=> log_threshold + 1);

				log_indentation_down;
				log_indentation_down;				
			else
				device_not_found (device_name);
			end if;
		end query_devices;
		
	begin -- drag_unit
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" dragging " & to_string (device_name) & " unit " & 
					to_string (unit_name) & " to" &
					to_string (point), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" dragging " & to_string (device_name) & " unit " & 
					to_string (unit_name) & " by" & 
					to_string (point), level => log_threshold);
		end case;
		
		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end drag_unit;
	
	procedure rotate_unit (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		rotation		: in et_coordinates.type_rotation; -- 90
		log_threshold	: in type_log_level) 
	is

		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.pac_devices_sch;
			device_cursor : et_schematic.pac_devices_sch.cursor;

			position_of_unit : et_coordinates.type_position;
			rotation_before : et_coordinates.type_rotation;

			ports_lib, ports_scratch : et_symbols.pac_ports.map;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in out type_device_sch)
			is
				use et_schematic.pac_units;
				unit_cursor : et_schematic.pac_units.cursor;

				procedure rotate_unit (
					name	: in pac_unit_name.bounded_string; -- A
					unit	: in out type_unit) is

					preamble : constant string := " placeholder now at";
					
					procedure rotate_placeholders_absolute (rot : in type_rotation) is 

						-- Get the default positions of texts and placeholders as
						-- specified in symbol model. The default positions are
						-- later rotated by the given rotation rot.
						default_positions : et_symbols.type_default_text_positions := 
												default_text_positions (device_cursor, name);
						
						-- Rotates the position by the given rotation rot:
						function add_rot (p : in type_point) return type_rotation is begin
							return get_rotation (p) + rot;
						end;

					begin
						-- The current positions of the placeholders are overwritten by
						-- the defaults as specified in the symbol mode.
						-- Then the position of placeholders around the origin of the unit
						-- are rotated.
						
						-- NAME
						-- Rotate the placeholder around its own anchor point so that it
						-- it is readable from the front or from the right.
						unit.name.rotation := snap (default_positions.name.rotation + rot);

						-- reset the placeholder anchor point to the position as specified in the symbol model
						unit.name.position := default_positions.name.position;
						
						-- rotate the placeholder anchor point around the symbol origin:
						rotate_to (unit.name.position, add_rot (default_positions.name.position));
									
						log (text => "name" & preamble & to_string (unit.name.position), 
								level => log_threshold + 2);


						-- VALUE
						-- Rotate the placeholder around its own anchor point so that it
						-- it is readable from the front or from the right.
						unit.value.rotation := snap (default_positions.value.rotation + rot);
						
						-- reset the placeholder anchor point to the position as specified in the symbol model
						unit.value.position := default_positions.value.position;
						
						-- rotate the placeholder anchor point around the symbol origin:
						rotate_to (unit.value.position, add_rot (default_positions.value.position));

						log (text => "value" & preamble & to_string (unit.value.position), 
								level => log_threshold + 2);


						-- PURPOSE
						-- Rotate the placeholder around its own anchor point so that it
						-- it is readable from the front or from the right.
						unit.purpose.rotation := snap (default_positions.purpose.rotation + rot);

						-- reset the placeholder anchor point to the position as specified in the symbol model
						unit.purpose.position := default_positions.purpose.position;
						
						-- rotate the placeholder anchor point around the symbol origin:
						rotate_to (unit.purpose.position, add_rot (default_positions.purpose.position));

						log (text => "purpose" & preamble & to_string (unit.purpose.position), 
								level => log_threshold + 2);

					end rotate_placeholders_absolute;
					
					procedure rotate_placeholders_relative (rot : in type_rotation) is begin
					-- Rotate position of placeholders around the unit origin. 
					
						-- NAME
						-- Rotate the placeholder around its own anchor point so that it
						-- it is readable from the front or from the right.
						unit.name.rotation := snap (unit.name.rotation + rot);

						-- rotate the placeholder anchor point around the symbol origin:
						rotate_by (unit.name.position, rot);

						log (text => "name" & preamble & to_string (unit.name.position), 
								level => log_threshold + 2);


						-- VALUE
						-- Rotate the placeholder around its own anchor point so that it
						-- it is readable from the front or from the right.
						unit.value.rotation := snap (unit.value.rotation + rot);

						-- rotate the placeholder anchor point around the symbol origin:
						rotate_by (unit.value.position, rot);

						log (text => "value" & preamble & to_string (unit.value.position), 
								level => log_threshold + 2);


						-- PURPOSE
						-- Rotate the placeholder around its own anchor point so that it
						-- it is readable from the front or from the right.
						unit.purpose.rotation := snap (unit.purpose.rotation + rot);

						-- rotate the placeholder anchor point around the symbol origin:
						rotate_by (unit.purpose.position, rot);

						log (text => "purpose" & preamble & to_string (unit.purpose.position), 
								level => log_threshold + 2);

					end rotate_placeholders_relative;

				begin -- rotate_unit
					case coordinates is
						when ABSOLUTE =>
							set (unit.position, rotation);
							rotate_placeholders_absolute (rotation);
							
						when RELATIVE =>
							set (unit.position, add (rotation_before, rotation));
							
							log (text => "rotation now" & to_string (rot (unit.position)),
									level => log_threshold + 1);

							rotate_placeholders_relative (rotation);
					end case;
				end rotate_unit;
				
			begin -- query_units
				if contains (device.units, unit_name) then
					-- locate unit by its name
					unit_cursor := find (device.units, unit_name);

					-- load unit position and current rotation
					position_of_unit := element (unit_cursor).position;
					rotation_before := rot (element (unit_cursor).position);

					-- log unit position and current rotation
					log (text => to_string (position => position_of_unit) &
							" rotation before" & to_string (rotation_before),
							level => log_threshold + 1);

					log_indentation_up;
					
					pac_units.update_element (
						container	=> device.units,
						position	=> unit_cursor,
						process		=> rotate_unit'access);

					log_indentation_down;
				else
					unit_not_found (unit_name);
				end if;
			end query_units;

		begin -- query_devices
			if contains (module.devices, device_name) then

				-- Before the rotation, the coordinates of the
				-- unit must be fetched. These coordinates will later assist
				-- in deleting the port names from connected net segments.
				device_cursor := find (module.devices, device_name); -- the device should be there

				-- rotate the unit
				update_element (
					container	=> module.devices,
					position	=> device_cursor,
					process		=> query_units'access);
				
				log_indentation_up;

				-- Fetch the ports of the unit to be rotated.
				-- The coordinates here are the default positions (in the library model)
				-- relative to the center of the units.
				ports_lib := ports_of_unit (device_cursor, unit_name);
				
				ports_scratch := ports_lib;						 

				-- Calculate the absolute positions of the unit ports in the schematic
				-- as they are BEFORE the rotation:
				rotate_ports (ports_scratch, rotation_before);
				move_ports (ports_scratch, position_of_unit);
				
				-- Delete the old ports of the targeted unit from module.nets.
				-- The unit is on a certain sheet. The procedure delete_ports however
				-- requires a list of unit positions (containing sheet numbers).
				-- So we create a list "sheets", put the unit name and position in it,
				-- and pass it to procedure delete_ports:
				declare
					sheets : pac_unit_positions.map;
				begin
					pac_unit_positions.insert (
						container	=> sheets,
						key			=> unit_name,
						new_item	=> position_of_unit);

					delete_ports (
						module			=> module_cursor,
						device			=> device_name,
						ports			=> ports_scratch,
						sheets			=> sheets, 
						log_threshold	=> log_threshold + 1);
				end;

				-- Calculate the new positions of the unit ports.
				case coordinates is
					when ABSOLUTE =>
						rotate_ports (ports_lib, rotation);
					when RELATIVE =>
							-- The given angle of rotation adds to the rotation_before:
						rotate_ports (ports_lib, add (rotation_before, rotation));
				end case;
				
				move_ports (ports_lib, position_of_unit);
				
				-- Insert the new unit ports in the nets (type_module.nets):
				insert_ports (
					module			=> module_cursor,
					device			=> device_name,
					unit			=> unit_name,
					ports			=> ports_lib,
					sheet			=> et_coordinates.sheet (position_of_unit),
					log_threshold	=> log_threshold + 1);
				
				log_indentation_down;				
			else
				device_not_found (device_name);
			end if;
		end query_devices;
		
	begin -- rotate_unit
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" rotating " & to_string (device_name) & " unit " & 
					to_string (unit_name) & " to" & to_string (rotation), level => log_threshold);

			when RELATIVE =>
				if rotation in type_rotation_relative then
					log (text => "module " & to_string (module_name) &
						" rotating " & to_string (device_name) & " unit " & 
						to_string (unit_name) & " by" & to_string (rotation), level => log_threshold);
				else
					relative_rotation_invalid;
				end if;
		end case;
		
		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end rotate_unit;
	
end et_schematic_ops.units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
