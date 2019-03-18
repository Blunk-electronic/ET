------------------------------------------------------------------------------
--                                                                          --
--                     SYSTEM ET SCHEMATIC OPERATIONS                       --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.text_io;				use ada.text_io;

with ada.exceptions;
with ada.directories;
-- with gnat.directory_operations;

with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;

with et_general;				use et_general;
with et_coordinates;
with et_string_processing;		use et_string_processing;
with et_libraries;				use et_libraries;
with et_schematic;				use et_schematic;
with et_pcb_coordinates;
with et_project;				use et_project;
with et_geometry;

package body schematic_ops is

	use type_modules;

	procedure device_not_found (name : in type_device_name) is begin
		log (message_error & "device " & to_string (name) & " not found !", console => true);
		raise constraint_error;
	end;

	procedure unit_not_found (name : in type_unit_name.bounded_string) is begin
		log (message_error & "unit " & to_string (name) & " not found !", console => true);
		raise constraint_error;
	end;
	
	procedure log_unit_positions (
	-- Writes the positions of the device unis in the log file.
		positions 		: in type_unit_positions.map;
		log_threshold	: in type_log_level) is
		
		procedure write (cursor : in type_unit_positions.cursor) is begin
			log (
				"unit " &
				et_libraries.to_string (type_unit_positions.key (cursor)) & -- unit name
				et_coordinates.to_string (position => type_unit_positions.element (cursor)), -- sheet x y
				log_threshold);
		end;
		
	begin
		log ("location(s) in schematic:", log_threshold);
		log_indentation_up;
		et_schematic.type_unit_positions.iterate (positions, write'access);
		log_indentation_down;
	end;
	
	procedure log_package_position (
	-- Writes the position of the package in the log file. If device is virtual, nothing happens.
		device_cursor	: in et_schematic.type_devices.cursor;
		log_threshold	: in type_log_level) is
		use et_pcb_coordinates;
		use et_schematic.type_devices;
	begin
		if element (device_cursor).appearance = SCH_PCB then
			log ("location in board:" & 
				to_string (point => type_point_2d (element (device_cursor).position)) &
				" face" & 
				to_string (get_face (element (device_cursor).position)),
				log_threshold);
		end if;
	end;

	function positions_of_units (
	-- Collects the positions of all units (in schematic) of the given device and returns
	-- them in a list.
		device_cursor : in et_schematic.type_devices.cursor) 
		return type_unit_positions.map is

		-- temporarily storage of unit coordinates:
		positions : type_unit_positions.map;
		
		procedure get_positions (
			device_name : in type_device_name;
			device		: in et_schematic.type_device) is
		begin
			positions := unit_positions (device.units);
		end;

	begin -- positions_of_units
		et_schematic.type_devices.query_element (
			position	=> device_cursor,
			process		=> get_positions'access);

		return positions;
	end;
	
	procedure delete_ports (
		module			: in type_modules.cursor;		-- the module
		device			: in type_device_name;			-- the device
		ports			: in et_libraries.type_ports.map := et_libraries.type_ports.empty_map; -- the ports (if empty, all ports of the device will be deleted)
		sheets			: in type_unit_positions.map;	-- the sheet numbers where the units can be found. CS implementation required
		log_threshold	: in type_log_level) is

		dedicated_ports : boolean := false; -- goes true if "ports" contains something.
		
		procedure query_nets (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is

			procedure query_net (net_cursor : in type_nets.cursor) is
				use type_nets;

				procedure query_strands (
					net_name	: in type_net_name.bounded_string;
					net			: in out type_net) is
					use type_strands;

					procedure query_strand (strand_cursor : in type_strands.cursor) is
						use et_coordinates;

						procedure query_segments (strand : in out type_strand) is
							use type_net_segments;

							procedure query_segment (segment_cursor : in type_net_segments.cursor) is 
								use type_ports_component;

								procedure query_ports (segment : in out type_net_segment) is
								-- Tests device ports of given segment if their device name matches the given device name.
								-- On match the port is skipped. All other ports are collected in ports_new.	
									ports_new : type_ports_component.list;
									
									procedure query_port (port_cursor : in type_ports_component.cursor) is
										port : type_port_component := element (port_cursor); -- take a copy of the port
										use type_ports_component;
									begin -- query_port
										if port.reference = device then -- on match just report the port and skip it

											log_indentation_up;
											
											if dedicated_ports then
												if et_libraries.type_ports.contains (ports, port.name) then
													log ("delete port " & to_string (port.name), log_threshold + 3);
												else
													ports_new.append (port); -- all other ports are collected in ports_new.
												end if;
											else
												log ("delete port " & to_string (port.name), log_threshold + 3);	
											end if;
																						
											log_indentation_down;
										else
											ports_new.append (port); -- all other ports are collected in ports_new.
										end if;
									end query_port;
									
								begin -- query_ports
									iterate (segment.ports_devices, query_port'access); -- loop in portlist of given segment

									-- overwrite old portlist by new portlist
									segment.ports_devices := ports_new;
								end query_ports;
								
							begin -- query_segment
								log_indentation_up;
								log (to_string (segment_cursor), log_threshold + 2);

								update_element (
									container	=> strand.segments,
									position	=> segment_cursor,
									process		=> query_ports'access);
												   
								log_indentation_down;
							end query_segment;
							
						begin -- query_segments
							iterate (strand.segments, query_segment'access);
						end query_segments;
						
					begin -- query_strand
						log_indentation_up;
						log ("strand " & to_string (position => element (strand_cursor).position),
							 log_threshold + 2);

						update_element (
							container	=> net.strands,
							position	=> strand_cursor,
							process		=> query_segments'access);
						
						log_indentation_down;
					end query_strand;
					
				begin -- query_strands
					iterate (net.strands, query_strand'access);
				end query_strands;
				
			begin -- query_net
				log ("net " & to_string (key (net_cursor)), log_threshold + 1);

				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);
				
			end query_net;				
			
		begin -- query_nets
			type_nets.iterate (module.nets, query_net'access);
		end query_nets;
		
	begin -- delete_ports
		log ("deleting ports in nets ...", log_threshold);

		-- If ports are provided, we have to delete exactly those in list "ports".
		-- The flag dedicated_ports is later required in order to do this job:
		if et_libraries.type_ports.length (ports) > 0 then
			dedicated_ports := true;
		end if;
		
		log_indentation_up;
		
		update_element (
			container	=> modules,
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;
	end delete_ports;
	
	procedure delete_device (
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		log_threshold	: in type_log_level) is

		module_cursor : type_modules.cursor; -- points to the module being modified
		
		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;

			-- temporarily storage of unit coordinates:
			position_of_units : type_unit_positions.map;
			
		begin -- query_devices
			if contains (module.devices, device_name) then

				-- Before the actual deletion, the coordinates of the
				-- units must be fetched. These coordinates will later assist
				-- in deleting the port names from connected net segments.
				device_cursor := find (module.devices, device_name); -- the device should be there
				position_of_units := positions_of_units (device_cursor);

				log_indentation_up;
				log_unit_positions (position_of_units, log_threshold + 1);

				log_package_position (device_cursor, log_threshold + 1);

				-- Delete the targeted device:
				delete (module.devices, device_name);

				-- Delete all ports of the targeted device from module.nets
				delete_ports (
					module			=> module_cursor,
					device			=> device_name,
					sheets			=> position_of_units, -- the sheets to look at
					log_threshold	=> log_threshold + 1);

				log_indentation_down;				
			else
				device_not_found (device_name);
			end if;
		end query_devices;
		
	begin -- delete_device
		log ("module " & to_string (module_name) &
			 " deleting " & to_string (device_name) & " ...", log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end delete_device;

	function ports_of_unit (
	-- Returns a map of ports of the given device and unit.
		device_cursor	: in et_schematic.type_devices.cursor;
		unit_name		: in type_unit_name.bounded_string)
		return et_libraries.type_ports.map is

		ports : et_libraries.type_ports.map; -- to be returned
		
		model : type_device_model_file.bounded_string; -- ../libraries/devices/transistor/pnp.dev
		device_cursor_lib : et_libraries.type_devices.cursor;

		procedure query_internal_units (
			model	: in type_device_model_file.bounded_string;
			device	: in et_libraries.type_device) is
			use type_units_internal;
			unit_cursor : type_units_internal.cursor;
		begin -- query_internal_units
			-- locate the given unit among the internal units
			unit_cursor := find (device.units_internal, unit_name);

			-- Fetch the ports of the internal unit.
			-- Transfer the ports to the portlist to be returned:			
			-- CS: constraint_error arises here if unit can not be located.
			ports := element (unit_cursor).symbol.ports;
		end query_internal_units;

		procedure query_external_units (
			model	: in type_device_model_file.bounded_string;
			device	: in et_libraries.type_device) is
			use type_units_external;
			unit_cursor : type_units_external.cursor;
			sym_model : type_symbol_model_file.bounded_string; -- like /libraries/symbols/NAND.sym

			procedure query_symbol (
			-- Appends the ports names of the external unit to the portlist to 
			-- be returned.
				symbol_name	: in type_symbol_model_file.bounded_string;
				symbol		: in type_symbol ) is
			begin -- query_symbol
				ports := symbol.ports;
			end query_symbol;
			
		begin -- query_external_units
			-- locate the given unit among the external units
			unit_cursor := find (device.units_external, unit_name);

			-- Fetch the symbol model file of the external unit.
			-- CS: constraint_error arises here if unit could not be located.
			sym_model := element (unit_cursor).file;

			-- Fetch the ports of the external unit.
			-- CS: constraint_error arises here if symbol model could not be located.
			type_symbols.query_element (
				position	=> et_libraries.type_symbols.find (et_libraries.symbols, sym_model),
				process		=> query_symbol'access);
			
		end query_external_units;
		
	begin -- ports_of_unit

		-- Fetch the model name of the given device. 
		model := et_schematic.type_devices.element (device_cursor).model;

		-- Get cursor to device in device library (the model name is the key into the device library).
		-- CS: constraint_error will arise here if no associated device exists.
		device_cursor_lib := et_libraries.type_devices.find (et_libraries.devices, model);

		-- Query external units of device (in library). It is most likely that
		-- the unit is among the external units:
		et_libraries.type_devices.query_element (
			position	=> device_cursor_lib,
			process		=> query_external_units'access);

		-- If unit could not be found among external units then look up the internal units:
		if et_libraries.type_ports.length (ports) = 0 then

			-- Query internal units of device (in library):
			et_libraries.type_devices.query_element (
				position	=> device_cursor_lib,
				process		=> query_internal_units'access);
		end if;

		-- If still no ports found, we have a problem:
		if et_libraries.type_ports.length (ports) = 0 then
			raise constraint_error;
		end if;
		
		return ports;

		exception
			when event: others =>
				log_indentation_reset;
				log (ada.exceptions.exception_information (event), console => true);
				raise;
		
	end ports_of_unit;
	
	procedure delete_unit (
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A
		log_threshold	: in type_log_level) is

		module_cursor : type_modules.cursor; -- points to the module being modified
		
		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;

			-- temporarily storage of unit coordinates.
			-- There will be only one unit in this container.
			position_of_unit : type_unit_positions.map;

			ports : et_libraries.type_ports.map;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
				use et_schematic.type_units;
				unit_cursor : et_schematic.type_units.cursor;
			begin
				if contains (device.units, unit_name) then
					-- locate unit by its name
					unit_cursor := find (device.units, unit_name);

					-- Load the single unit position and insert in container "position_of_unit"
					type_unit_positions.insert (
						container	=> position_of_unit, 
						key			=> unit_name,
						new_item	=> element (unit_cursor).position);

					log_unit_positions (position_of_unit, log_threshold + 1);
					
					-- delete the unit
					delete (device.units, unit_name);
				else
					unit_not_found (unit_name);
				end if;
			end query_units;
			
			units_invoked : boolean := true; -- goes false if no unit used anymore

			procedure query_number_of_invoked_units (
				device_name	: in type_device_name;
				device		: in et_schematic.type_device) is
				use et_schematic.type_units;
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
					log_threshold	=> log_threshold + 1);

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
		log ("module " & to_string (module_name) &
			 " deleting " & to_string (device_name) & " unit " & 
			 to_string (unit_name) & " ...", log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end delete_unit;

	function to_string (coordinates : in type_coordinates) return string is begin
		return latin_1.space & to_lower (type_coordinates'image (coordinates));
	end;

	function to_coordinates (coordinates : in string) return type_coordinates is begin
		return type_coordinates'value (coordinates);

		exception
			when event: others =>
				log (ada.exceptions.exception_information (event), console => true);
				raise;
	end;

	function on_segment (
		point 	: in et_coordinates.type_point;
		segment : in type_net_segments.cursor)
		return boolean is
	-- Returns true if given point sits on given segment.
		use et_geometry;
		distance : type_distance_point_from_line;
		use et_coordinates;
		use type_net_segments;
	begin
		distance := distance_of_point_from_line (
			point 		=> point,
			line_start	=> element (segment).coordinates_start,
			line_end	=> element (segment).coordinates_end,
			line_range	=> with_end_points);

		-- start and end points of the segment are inclued in the test
		if not distance.out_of_range and distance.distance = zero_distance then
			return true;
		else
			return false;
		end if;
	end on_segment;

	function between_start_and_end_point (
		point 	: in et_coordinates.type_point;
		segment : in type_net_segments.cursor)
		return boolean is
	-- Returns true if given point sits between start and end 
	-- point of given segment.
		use et_geometry;
		distance : type_distance_point_from_line;
		use et_coordinates;
		use type_net_segments;
	begin
		distance := distance_of_point_from_line (
			point 		=> point,
			line_start	=> element (segment).coordinates_start,
			line_end	=> element (segment).coordinates_end,
			line_range	=> inside_end_points);

		-- start and end points of the segment are inclued in the test
		if not distance.out_of_range and distance.distance = zero_distance then
			return true;
		else
			return false;
		end if;
	end between_start_and_end_point;

	
	procedure move_ports (
	-- Moves the given unit ports by given offset.
		ports	: in out et_libraries.type_ports.map; -- the portlist
		offset	: in et_coordinates.type_coordinates) -- the offset (only x/y matters)
		is
		use et_libraries.type_ports;

		procedure move (
			name	: in type_port_name.bounded_string;
			port	: in out type_port) is
		begin
			move (port.position, offset);
		end;

		procedure query_port (cursor : in type_ports.cursor) is begin
			update_element (
				container	=> ports,
				position	=> cursor,
				process		=> move'access);
		end;
			
	begin -- offset_ports
		iterate (ports, query_port'access);
	end move_ports;

	procedure insert_ports (
	-- Inserts the given ports in the net segments.
	-- If a port lands on a segment, it is regarded as "connected" with the segment.
	-- A junction will be placed:
	--  - if the ports lands between start and end point AND
	--  - no other junction is there already at this place.
		module			: in type_modules.cursor;		-- the module
		device			: in type_device_name;			-- the device
		ports			: in et_libraries.type_ports.map; -- the unit ports
		sheet			: in type_sheet;	-- the sheet to look at
		log_threshold	: in type_log_level) is

		-- We make a copy of the given portlist. Inside the copy ports will be
		-- deleted as soon as they have been processed. So ports_scratch gets
		-- shorter and shorter each time a port has been inserted in a net segment.
		ports_scratch : et_libraries.type_ports.map := ports; -- the unit ports

		-- If this flag goes true, lots of iterations are skipped (improves performance).
		all_ports_processed : boolean := false;
		
		procedure query_nets (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is

			procedure query_net (net_cursor : in type_nets.cursor) is
				use type_nets;

				procedure query_strands (
					net_name	: in type_net_name.bounded_string;
					net			: in out type_net) is
					use type_strands;

					procedure query_strand (strand_cursor : in type_strands.cursor) is
						use et_coordinates;

						procedure query_segments (strand : in out type_strand) is
							use type_net_segments;

							procedure query_segment (segment_cursor : in type_net_segments.cursor) is 

								procedure query_ports (segment : in out type_net_segment) is
									use et_libraries.type_ports;

									port_cursor : type_ports.cursor := ports_scratch.first;
									
								begin -- query_ports
									--iterate (ports_scratch, query_port'access);

									-- loop in ports_scratch and probe ports.
									while port_cursor /= type_ports.no_element loop
										log ("probing port " & to_string (key (port_cursor)) &
											to_string (element (port_cursor).position), log_threshold + 3);

										-- If port sits on segment, append it to the portlist of the segment.
										if on_segment (
											point	=> element (port_cursor).position,
											segment	=> segment_cursor) then

											type_ports_component.append (
												container	=> segment.ports_devices,
												new_item	=> (device, key (port_cursor))); -- IC23, VCC_IO

											log (" sits on segment -> inserted", log_threshold + 3);

											-- Remove port from ports_scratch. The port is not connected elsewhere.
											type_ports.delete (container => ports_scratch, position => port_cursor);

											-- set all_ports_processed true all given ports processed.
											if is_empty (ports_scratch) then all_ports_processed := true; end if;

											-- CS: place junction
										end if;
											
										next (port_cursor);
									end loop;
									
								end query_ports;

							begin -- query_segment
								if not all_ports_processed then
									log_indentation_up;
									log ("probing " & to_string (segment_cursor), log_threshold + 2);

									update_element (
										container	=> strand.segments,
										position	=> segment_cursor,
										process		=> query_ports'access);
													
									log_indentation_down;
								end if;
							end query_segment;
							
						begin -- query_segments
							-- CS if not all_ports_processed then ... could improve performance
							iterate (strand.segments, query_segment'access);
						end query_segments;
						
					begin -- query_strand
						if not all_ports_processed then
							
							-- We pick out only the strands on the targeted sheet:
							if et_coordinates.sheet (element (strand_cursor).position) = sheet then
								log ("net " & to_string (key (net_cursor)), log_threshold + 1);

								log_indentation_up;
								log ("strand " & to_string (position => element (strand_cursor).position),
									log_threshold + 1);

								update_element (
									container	=> net.strands,
									position	=> strand_cursor,
									process		=> query_segments'access);
							
								log_indentation_down;
							end if;
						end if;
					end query_strand;
					
				begin -- query_strands
					-- CS if not all_ports_processed then ... could improve performance
					iterate (net.strands, query_strand'access);
				end query_strands;
				
			begin -- query_net
				-- CS if not all_ports_processed then ... could improve performance
				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);
			end query_net;				
			
		begin -- query_nets
			-- CS if not all_ports_processed then ... could improve performance
			type_nets.iterate (module.nets, query_net'access);
		end query_nets;

	begin --insert_ports
		log ("inserting ports in nets on sheet" & 
			 to_sheet (sheet) & " ...", log_threshold);
		log_indentation_up;
		
		update_element (
			container	=> modules,
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;
	end insert_ports;
	
	procedure move_unit (
	-- Moves the given unit within the schematic.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		point			: in et_coordinates.type_point; -- x/y
		log_threshold	: in type_log_level) is

		module_cursor : type_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;

			-- temporarily storage of unit coordinates.
			-- There will be only one unit in this container.
			position_of_unit_old : type_unit_positions.map;

			position_of_unit_new : et_coordinates.type_coordinates;

			ports : et_libraries.type_ports.map;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
				use et_schematic.type_units;
				unit_cursor : et_schematic.type_units.cursor;

				procedure move_unit (
					unit_name	: in type_unit_name.bounded_string;
					unit		: in out et_schematic.type_unit) is
					use et_coordinates;
				begin
					case coordinates is
						when ABSOLUTE =>
							unit.position := to_coordinates (point, type_sheet (sheet));

						when RELATIVE =>
							move (
								position	=> unit.position,
								offset		=> to_coordinates_relative (point, sheet)
								);
					end case;

					-- store new unit position
					position_of_unit_new := unit.position;
					
					exception
						when event: others =>
							log (message_error & "coordinates invalid !", console => true); -- CS required more details
							log (ada.exceptions.exception_information (event), console => true);
							raise;
					
				end move_unit;
				
			begin -- query_units
				if contains (device.units, unit_name) then
					-- locate unit by its name
					unit_cursor := find (device.units, unit_name);

					-- load unit position and insert in container "position_of_unit_old"
					type_unit_positions.insert (
						container	=> position_of_unit_old, 
						key			=> unit_name,
						new_item	=> element (unit_cursor).position);

					-- log old unit position
					log_unit_positions (position_of_unit_old, log_threshold + 1); -- there is only one unit
-- 					log ("position before " & 
-- 						 et_coordinates.to_string (
-- 							type_ports.first_element (positions)), log_threshold + 1);

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
				log ("module " & to_string (module_name) &
					" moving " & to_string (device_name) & " unit " & 
					to_string (unit_name) & " to sheet" & to_sheet (sheet) &
					et_coordinates.to_string (point), log_threshold);

			when RELATIVE =>
				log ("module " & to_string (module_name) &
					" moving " & to_string (device_name) & " unit " & 
					to_string (unit_name) & " by " & to_sheet_relative (sheet) & " sheet(s)" &
					et_coordinates.to_string (point), log_threshold);
		end case;
		
		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end move_unit;


	procedure drag_net_segments (
	-- Drags the given net segments.
		module			: in type_modules.cursor;		-- the module
		drag_list		: in type_drags_of_ports.map;
		sheet			: in type_sheet;	-- the sheet to look at
		log_threshold	: in type_log_level) is

		procedure query_nets (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is

			procedure query_net (net_cursor : in type_nets.cursor) is
				use type_nets;

				procedure query_strands (
					net_name	: in type_net_name.bounded_string;
					net			: in out type_net) is
					use et_coordinates;
					
					use type_strands;
					strand_cursor : type_strands.cursor;
					
					use type_drags_of_ports;
					drag_cursor : type_drags_of_ports.cursor := drag_list.first;

					drag_processed : boolean;
					
					procedure query_segments (strand : in out type_strand) is
						use type_net_segments;

						procedure query_segment (segment_cursor : in type_net_segments.cursor) is 

							procedure change_segment (segment : in out type_net_segment) is begin
								
								-- if port sits on a start point of a segment -> move start point
								if segment.coordinates_start = element (drag_cursor).before then
									log ("move segment start point from " & 
										to_string (segment.coordinates_start),
										log_threshold + 3);

									segment.coordinates_start := element (drag_cursor).after;

									log ("to" & 
										to_string (segment.coordinates_start),
										log_threshold + 3);

									drag_processed := true;
								end if;

								-- if port sits on an end point of a segment -> move end point
								if segment.coordinates_end = element (drag_cursor).before then
									log ("move segment end point from " & 
										to_string (segment.coordinates_end),
										log_threshold + 3);

									segment.coordinates_end := element (drag_cursor).after;

									log ("to" & 
										to_string (segment.coordinates_end),
										log_threshold + 3);

									drag_processed := true;
								end if;

								-- if ports sits between start and end point of a segment -> delete the segment
								-- and insert two new segments. Move junction to position where the two new
								-- segments meet.
								if between_start_and_end_point (
									point	=> element (drag_cursor).before,
									segment	=> segment_cursor) then

									null;
-- 											log (" sits on segment -> inserted", log_threshold + 3);
-- 
-- 											-- Remove port from ports_scratch. The port is not connected elsewhere.
-- 											type_ports.delete (container => ports_scratch, position => port_cursor);
-- 
-- 											-- set all_ports_processed true all given ports processed.
-- 											if is_empty (ports_scratch) then all_ports_processed := true; end if;

									drag_processed := true;
								end if;
							end change_segment;

						begin -- query_segment
							log_indentation_up;
							log ("probing " & to_string (segment_cursor), log_threshold + 2);

							update_element (
								container	=> strand.segments,
								position	=> segment_cursor,
								process		=> change_segment'access);
											
							log_indentation_down;
						end query_segment;
						
					begin -- query_segments
						iterate (strand.segments, query_segment'access);

						-- Update strand position
						set_strand_position (strand); 
						-- CS write in et_schematic a procedure set_strand_position 
						-- that takes a cursor instead (should improve preformance)
						
					end query_segments;
						
				begin -- query_strands
					-- loop in drag list
					while drag_cursor /= type_drags_of_ports.no_element loop
						log ("probing port " & to_string (key (drag_cursor)), log_threshold + 1);

						-- If the current drag point sits on a strand, this flag goes true. Other 
						-- strand will then not be looked at because the point can only sit on a 
						-- single strand.
						drag_processed := false;
						
						strand_cursor := net.strands.first;
						while strand_cursor /= type_strands.no_element loop
							
							-- We pick out only the strands on the targeted sheet:
							if et_coordinates.sheet (element (strand_cursor).position) = sheet then
								log ("net " & to_string (key (net_cursor)), log_threshold + 1);

								log_indentation_up;
								log ("strand " & to_string (position => element (strand_cursor).position),
									log_threshold + 1);
							
								-- Iterate in segments of strand. If drag point sits on any segment
								-- the flag drag_processed goes true.
								update_element (
									container	=> net.strands,
									position	=> strand_cursor,
									process		=> query_segments'access);
							
								log_indentation_down;
							end if;

							-- If the drag point has been processed, there is no need to look up
							-- other strands.
							if drag_processed then exit; end if;
							
							next (strand_cursor);
						end loop;
						
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
			type_nets.iterate (module.nets, query_net'access);
		end query_nets;

	begin -- drag_net_segments
		log ("dragging net segments on sheet" & 
			 to_sheet (sheet) & " ...", log_threshold);
		log_indentation_up;

		update_element (
			container	=> modules,
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;
	end drag_net_segments;
		
	procedure drag_unit (
	-- Drags the given unit within the schematic.
	-- Already existing connections with net segments are kept.
	-- Net segment positions are modified.
	-- This operation applies to a single sheet. Dragging from one sheet
	-- to another is not possible.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in et_coordinates.type_point; -- x/y
		log_threshold	: in type_log_level) is

		module_cursor : type_modules.cursor; -- points to the module being modified

		function make_drag_list ( 
		-- Merges the two maps ports_old and ports_new to a drag list.
		-- The resulting drag list tells which port is to be moved from old to new position.
			ports_old : in et_libraries.type_ports.map;
			ports_new : in et_libraries.type_ports.map) 
			return type_drags_of_ports.map is
			use type_drags_of_ports;
			drag_list : type_drags_of_ports.map;

			-- ports_old and ports_new are both equally long and contain 
			-- equal keys (the port names). So we use two cursors and advance them
			-- simultaneously in a loop (see below).
			use et_libraries.type_ports;
			cursor_old : et_libraries.type_ports.cursor := ports_old.first;
			cursor_new : et_libraries.type_ports.cursor := ports_new.first;
		begin
			-- Loop in ports_old, copy the key to the drag list.
			-- Take the old position from ports_old and the new position from ports_new:
			while cursor_old /= type_ports.no_element loop
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
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;

			-- temporarily storage of unit coordinates.
			-- There will be only one unit in this container.
			--position_of_unit_old : type_unit_positions.map;
			position_of_unit_old : et_coordinates.type_coordinates;	
			position_of_unit_new : et_coordinates.type_coordinates;

			ports, ports_old, ports_new : et_libraries.type_ports.map;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
				use et_schematic.type_units;
				unit_cursor : et_schematic.type_units.cursor;

				procedure move_unit (
					unit_name	: in type_unit_name.bounded_string;
					unit		: in out et_schematic.type_unit) is
					use et_coordinates;

					-- Load the current sheet number where the unit is.
					-- NOTE: The sheet number does not change in drag operations.
					sheet : type_sheet := et_coordinates.sheet (unit.position);
				begin
					-- Set new x/y position. Sheet number is unchanged.
					case coordinates is
						when ABSOLUTE =>
							unit.position := to_coordinates (point, sheet);

						when RELATIVE =>
							move (
								point	=> unit.position,
								offset	=> point
								);
					end case;
					
					exception
						when event: others =>
							log (message_error & "coordinates invalid !", console => true); -- CS required more details
							log (ada.exceptions.exception_information (event), console => true);
							raise;
					
				end move_unit;
				
			begin -- query_units
				if contains (device.units, unit_name) then
					unit_cursor := find (device.units, unit_name); -- the unit should be there

					-- store old unit position
					position_of_unit_old := element (unit_cursor).position;
					log ("unit position old: " & to_string (position => position_of_unit_old), log_threshold + 1);

					-- move the unit
					update_element (
						container	=> device.units,
						position	=> unit_cursor,
						process		=> move_unit'access);

					-- store new unit position
					position_of_unit_new := element (unit_cursor).position;
					log ("unit position new: " & to_string (position => position_of_unit_new), log_threshold + 1);
				else
					unit_not_found (unit_name);
				end if;
			end query_units;
			
		begin -- query_devices
			if contains (module.devices, device_name) then
				device_cursor := find (module.devices, device_name); -- the device should be there

				log_indentation_up;

				-- Before the actual drag, the coordinates of the
				-- unit must be fetched. These coordinates will later assist
				-- in changing the positions of connected net segments.
				
				-- locate the unit, store old position, move it, store new position
				update_element (
					container	=> module.devices,
					position	=> device_cursor,
					process		=> query_units'access);
				
				-- Fetch the ports of the unit to be moved. These are the default port positions
				-- (relative to the symbol origin) as they are defined in the device model.
				ports := ports_of_unit (device_cursor, unit_name);
				
				-- Calculate the old and new positions of the unit ports:
				ports_old := ports;
				move_ports (ports_old, position_of_unit_old); 
				-- ports_old now contains the absolute port positions in the schematic BEFORE the move.

				ports_new := ports;
				move_ports (ports_new, position_of_unit_new);
				-- ports_old now contains the absolute port positions in the schematic AFTER the move.
				
				-- Change net segments in the affected nets (type_module.nets):
				drag_net_segments (
					module			=> module_cursor,
					drag_list		=> make_drag_list (ports_old, ports_new),
					sheet			=> et_coordinates.sheet (position_of_unit_new), -- or position_of_unit_old
					log_threshold	=> log_threshold + 1);
				
				log_indentation_down;				
			else
				device_not_found (device_name);
			end if;
		end query_devices;
		
	begin -- drag_unit
		case coordinates is
			when ABSOLUTE =>
				log ("module " & to_string (module_name) &
					" dragging " & to_string (device_name) & " unit " & 
					to_string (unit_name) & " to" &
					et_coordinates.to_string (point), log_threshold);

			when RELATIVE =>
				log ("module " & to_string (module_name) &
					" dragging " & to_string (device_name) & " unit " & 
					to_string (unit_name) & " by" & 
					et_coordinates.to_string (point), log_threshold);
		end case;
		
		-- locate module
		module_cursor := locate_module (module_name);
	
		
		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end drag_unit;
		


	
end schematic_ops;
	
-- Soli Deo Gloria
