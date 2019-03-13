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
	-- Collects the positions of units (in schematic) of the given device and returns
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
		ports			: in type_ports.list := type_ports.empty_list; -- the ports (if empty, all ports of the device will be deleted)
		positions		: in type_unit_positions.map;	-- the sheet numbers where the units can be found
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
												if type_ports.contains (ports, port.name) then
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
		log ("deleting ports in net ...", log_threshold);

		-- If ports are provided, we have to delete exactly those in list "ports".
		-- The flag dedicated_ports is later required in order to do this job:
		if type_ports.length (ports) > 0 then
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
			positions : type_unit_positions.map;
			
		begin -- query_devices
			if contains (module.devices, device_name) then

				-- Before the actual deletion, the coordinates of the
				-- units must be fetched. These coordinates will later assist
				-- in deleting the port names from connected net segments.
				device_cursor := find (module.devices, device_name); -- the device should be there
				positions := positions_of_units (device_cursor);

				log_indentation_up;
				log_unit_positions (positions, log_threshold + 1);

				log_package_position (device_cursor, log_threshold + 1);

				-- Delete the targeted device:
				delete (module.devices, device_name);

				-- Delete all ports of the targeted device from module.nets
				delete_ports (
					module			=> module_cursor,
					device			=> device_name,
					positions		=> positions,
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
	-- Returns a simple list of port names of the given device and unit.
		device_cursor	: in et_schematic.type_devices.cursor;
		unit_name		: in type_unit_name.bounded_string)
		return type_ports.list is

		ports : type_ports.list; -- to be returned
		
		model : type_device_model_file.bounded_string; -- ../libraries/devices/transistor/pnp.dev
		device_cursor_lib : et_libraries.type_devices.cursor;

		procedure query_internal_units (
			model	: in type_device_model_file.bounded_string;
			device	: in et_libraries.type_device) is
			use type_units_internal;
			unit_cursor : type_units_internal.cursor;
			ports_lib : et_libraries.type_ports.map; -- the port list of the unit in the library model

			procedure query_ports (cursor : in et_libraries.type_ports.cursor) is begin
				type_ports.append (
					container	=> ports,
					new_item	=> et_libraries.type_ports.key (cursor));
			end;
				
		begin -- query_internal_units
			-- locate the given unit among the internal units
			unit_cursor := find (device.units_internal, unit_name);

			-- Fetch the ports of the internal unit.
			-- CS: constraint_error arises here if unit can not be located.
			ports_lib := element (unit_cursor).symbol.ports;

			-- Transfer the ports to the portlist to be returned:
			et_libraries.type_ports.iterate (ports_lib, query_ports'access);
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

				procedure query_ports (cursor : in et_libraries.type_ports.cursor) is begin
					type_ports.append (
						container	=> ports,
						new_item	=> et_libraries.type_ports.key (cursor));
				end;
				
			begin -- query_symbol
				et_libraries.type_ports.iterate (symbol.ports, query_ports'access);
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
		-- CS: constraint_error will arise here if no associated device exits.
		device_cursor_lib := et_libraries.type_devices.find (et_libraries.devices, model);

		-- Query external units of device (in library). It is most likely that
		-- the unit is among the external units:
		et_libraries.type_devices.query_element (
			position	=> device_cursor_lib,
			process		=> query_external_units'access);

		-- If unit could not be found among external units then look up the internal units:
		if type_ports.length (ports) = 0 then

			-- Query internal units of device (in library):
			et_libraries.type_devices.query_element (
				position	=> device_cursor_lib,
				process		=> query_internal_units'access);
		end if;

		-- If still no ports found, we have a problem:
		if type_ports.length (ports) = 0 then
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
			-- In the end there will be only one unit in this container.
			positions : type_unit_positions.map;

			ports : type_ports.list;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
				use et_schematic.type_units;
				unit_cursor : et_schematic.type_units.cursor;
			begin
				if contains (device.units, unit_name) then
					-- locate unit by its name
					unit_cursor := find (device.units, unit_name);

					-- load unit position and insert in container "positions"
					type_unit_positions.insert (
						container	=> positions, 
						key			=> unit_name,
						new_item	=> element (unit_cursor).position);

					log_unit_positions (positions, log_threshold + 1); -- there is only one unit
					
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
					positions		=> positions, -- there is only one unit 
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

	procedure move_unit_absolute (
	-- Moves the given unit to an absolute position in schematic.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A
		position		: in et_coordinates.type_coordinates;
		log_threshold	: in type_log_level) is

		module_cursor : type_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;

			-- temporarily storage of unit coordinates.
			-- In the end there will be only one unit in this container.
			positions : type_unit_positions.map;

			ports : type_ports.list;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
				use et_schematic.type_units;
				unit_cursor : et_schematic.type_units.cursor;

				procedure move_unit (
					unit_name	: in type_unit_name.bounded_string;
					unit		: in out et_schematic.type_unit) is
				begin
					unit.position := position;
				end move_unit;
				
			begin -- query_units
				if contains (device.units, unit_name) then
					-- locate unit by its name
					unit_cursor := find (device.units, unit_name);

					-- load unit position and insert in container "positions"
					type_unit_positions.insert (
						container	=> positions, 
						key			=> unit_name,
						new_item	=> element (unit_cursor).position);

					-- log old unit position
					log_unit_positions (positions, log_threshold + 1); -- there is only one unit
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
				
				-- Delete the ports of the targeted unit from module.nets
				delete_ports (
					module			=> module_cursor,
					device			=> device_name,
					ports			=> ports,
					positions		=> positions, -- there is only one unit 
					log_threshold	=> log_threshold + 1);

				-- CS update nets
				
				log_indentation_down;				
			else
				device_not_found (device_name);
			end if;
		end query_devices;
		
	begin -- move_unit_absolute
		log ("module " & to_string (module_name) &
			 " moving " & to_string (device_name) & " unit " & 
			 to_string (unit_name) & " to" &
			 et_coordinates.to_string (position => position), log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end move_unit_absolute;
		


	
end schematic_ops;
	
-- Soli Deo Gloria
