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
with et_pcb;
with et_pcb_coordinates;
with et_project;				use et_project;
with submodules;
with conventions;
with et_geometry;

package body schematic_ops is

	use type_modules;

	procedure device_not_found (name : in type_device_name) is begin
		log (message_error & "device " & to_string (name) & " not found !", console => true);
		raise constraint_error;
	end;

	procedure device_already_exists (name : in type_device_name) is begin
		log (message_error & "device " & to_string (name) & " already exists !", console => true);
		raise constraint_error;
	end;

-- 	procedure device_prefix_invalid (name : in type_device_name) is begin
-- 		log (message_warning & "prefix of device name " & to_string (name) & " invalid !");
-- 	end;
	
	procedure unit_not_found (name : in type_unit_name.bounded_string) is begin
		log (message_error & "unit " & to_string (name) & " not found !", console => true);
		raise constraint_error;
	end;

	procedure submodule_not_found (name : in et_general.type_module_instance_name.bounded_string) is begin
		log (message_error & "submodule " & et_general.to_string (name) & " not found !", console => true);
		raise constraint_error;
	end;

	procedure netchanger_not_found (index : in submodules.type_netchanger_id) is begin
		log (message_error & "netchanger " & submodules.to_string (index) & " not found !", console => true);
		raise constraint_error;
	end;

	procedure net_not_found (name : in et_general.type_net_name.bounded_string) is begin
		log (message_error & "net " & to_string (name) & " not found !", console => true);
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
								use type_ports_device;

								procedure query_ports (segment : in out type_net_segment) is
								-- Tests device ports of given segment if their device name matches the given device name.
								-- On match the port is skipped. All other ports are collected in ports_new.	
									ports_new : type_ports_device.set;
									
									procedure query_port (port_cursor : in type_ports_device.cursor) is
										port : type_port_device := element (port_cursor); -- take a copy of the port
									begin -- query_port
										if port.device_name = device then -- on match just report the port and skip it

											log_indentation_up;
											
											if dedicated_ports then
												if et_libraries.type_ports.contains (ports, port.port_name) then
													log ("delete port " & to_string (port.port_name), log_threshold + 3);
												else
													ports_new.insert (port); -- all other ports are collected in ports_new.
												end if;
											else
												log ("delete port " & to_string (port.port_name), log_threshold + 3);	
											end if;
																						
											log_indentation_down;
										else
											ports_new.insert (port); -- all other ports are collected in ports_new.
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
	-- The coordinates of the ports are default xy-positions relative
	-- to the center of the unit.
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
			-- If unit could not be located, nothing happens -> ports remains empty.
			if unit_cursor /= type_units_external.no_element then
				sym_model := element (unit_cursor).file;

				-- Fetch the ports of the external unit.
				-- CS: constraint_error arises here if symbol model could not be located.
				type_symbols.query_element (
					position	=> et_libraries.type_symbols.find (et_libraries.symbols, sym_model),
					process		=> query_symbol'access);
			end if;
			
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
			
	begin -- move_ports
		iterate (ports, query_port'access);
	end move_ports;
	
	function position (
	-- Returns the sheet/x/y position of the given device and port.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC34
		port_name		: in type_port_name.bounded_string; -- CE
		log_threshold	: in type_log_level)
		return et_coordinates.type_coordinates is

		port_position : et_coordinates.type_coordinates; -- to be returned		
		
		module_cursor : type_modules.cursor; -- points to the module being inquired

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;
			unit_position : et_coordinates.type_coordinates;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in et_schematic.type_device) is
				
				use et_schematic.type_units;
				unit_cursor : type_units.cursor := device.units.first;
				unit_name : type_unit_name.bounded_string;
				
				use et_libraries.type_ports;
				ports : et_libraries.type_ports.map;
				port_cursor : et_libraries.type_ports.cursor;
			begin
				-- Locate unit in schematic device:
				while unit_cursor /= type_units.no_element loop

					-- Load the default xy-positions of ports relative to the center of the unit.
					unit_name := key (unit_cursor);
					ports := ports_of_unit (device_cursor, unit_name);

					-- If the unit has a port named port_name: 
					if contains (ports, port_name) then -- port found
						
						-- calculate the port position in the schematic
						unit_position := element (unit_cursor).position; -- unit pos. in schematic

						port_cursor := find (ports, port_name);
						port_position := et_coordinates.to_coordinates (
									sheet	=> sheet (unit_position), -- the sheet where the unit is
									point	=> element (port_cursor).position -- default xy pos of port
									);														 

						-- Calculate the absolute port position in schematic by
						-- first rotating port_xy, and then moving port_xy:
						
						et_coordinates.rotate (
							point	=> port_position,
							angle	=> element (unit_cursor).rotation);
						
						-- CS mirror ?
						
						-- Calculate the absolute port position in the schematic:
						et_coordinates.move (
							point 	=> port_position,
							offset	=> unit_position);
						
						exit; -- no need to look at other units
					end if;
					
					next (unit_cursor);
				end loop;
			end query_units;
			
		begin -- query_devices
			if contains (module.devices, device_name) then
				device_cursor := find (module.devices, device_name); -- the device should be there

				log_indentation_up;
				
				et_schematic.type_devices.query_element (
					position	=> device_cursor,
					process		=> query_units'access);

				log_indentation_down;				
			else
				device_not_found (device_name);
			end if;
		end query_devices;
		
	begin -- position
		log ("module " & to_string (module_name) &
			 " locating device " & to_string (device_name) & 
			 " port " & to_string (port_name) & " ...", log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		query_element (
			position	=> module_cursor,
			process		=> query_devices'access);
		
		return port_position;
	end position;

	function position (
	-- Returns the sheet/x/y position of the given submodule port.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		submod_name		: in et_general.type_module_instance_name.bounded_string; -- MOT_DRV_3
		port_name		: in type_net_name.bounded_string; -- RESET
		log_threshold	: in type_log_level)
		return et_coordinates.type_coordinates is

		port_position : et_coordinates.type_coordinates; -- to be returned		
		
		module_cursor : type_modules.cursor; -- points to the module being inquired

		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use submodules;			
			use type_submodules;
			submod_cursor : type_submodules.cursor;
			submod_position : et_coordinates.type_coordinates;

			procedure query_ports (
				submod_name	: in et_general.type_module_instance_name.bounded_string;
				submodule	: in type_submodule) is
				use type_submodule_ports;
				port_xy : type_point;
				cursor : type_submodule_ports.cursor := find (submodule.ports, port_name);
			begin
				if cursor /= type_submodule_ports.no_element then

					-- If the port exits, get its relative x/y position (relative to submodule position).
					port_xy := element (cursor).position;

					-- Calculate the absolute port position:
					et_coordinates.move (
						point	=> port_xy,
						offset	=> submod_position);

					-- Now port_xy holds the absolute x/y of the port in the schematic.

					-- Assemble the port_position to be returned:
					port_position := to_coordinates (
						point	=> port_xy,
						sheet	=> sheet (submod_position)
						);

				else
					log_indentation_reset;
					log (message_error & "port " & et_general.to_string (port_name) & " not found !",
						 console => true);
				end if;
			end query_ports;
			
		begin -- query_submodules
			if contains (module.submods, submod_name) then
				submod_cursor := find (module.submods, submod_name); -- the submodule should be there

				log_indentation_up;

				-- get submodule position (sheet/x/y)
				submod_position := element (submod_cursor).position;

				-- look for the given port
				query_element (
					position	=> submod_cursor,
					process		=> query_ports'access);

				log_indentation_down;				
			else
				submodule_not_found (submod_name);
			end if;
		end query_submodules;
		
	begin -- position
		log ("module " & to_string (module_name) &
			 " locating submodule " & to_string (submod_name) & 
			 " port " & to_string (port_name) & " ...", log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		query_element (
			position	=> module_cursor,
			process		=> query_submodules'access);
		
		return port_position;
	end position;

	function position (
	-- Returns the sheet/x/y position of the given netchanger port.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in submodules.type_netchanger_id; -- 1,2,3,...
		port			: in submodules.type_netchanger_port_name; -- SLAVE/MASTER
		log_threshold	: in type_log_level)
		return et_coordinates.type_coordinates is

		use submodules;
		port_position : et_coordinates.type_coordinates; -- to be returned		
		
		module_cursor : type_modules.cursor; -- points to the module being inquired

		procedure query_netchangers (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use type_netchangers;
			nc_cursor : type_netchangers.cursor;
			nc_position : et_coordinates.type_coordinates;
			nc_rotation : et_coordinates.type_rotation;
			port_xy : type_point;
		begin -- query_netchangers
			if contains (module.netchangers, index) then
				nc_cursor := find (module.netchangers, index); -- the netchanger should be there

				log_indentation_up;

				-- get netchanger position (sheet/x/y) and rotation in schematic
				nc_position := element (nc_cursor).position_sch;
				nc_rotation := element (nc_cursor).rotation;

				-- get the port position relative to the center of the netchanger
				case port is
					when MASTER =>
						port_xy := position_master_port_default;

					when SLAVE =>
						port_xy := position_slave_port_default;
				end case;

				-- Calculate the absolute port position in schematic by
				-- first rotating port_xy, and then moving port_xy:
				
				et_coordinates.rotate (
					point	=> port_xy,
					angle	=> nc_rotation);
				
				et_coordinates.move (
					point	=> port_xy,
					offset	=> nc_position);

				-- Now port_xy holds the absolute x/y of the port in the schematic.

				-- Assemble the port_position to be returned:
				port_position := to_coordinates (
					point	=> port_xy,
					sheet	=> sheet (nc_position)
					);
				
				log_indentation_down;				
			else
				netchanger_not_found (index);
			end if;
		end query_netchangers;
		
	begin -- position
		log ("module " & to_string (module_name) &
			 " locating netchanger " & to_string (index) & 
			 " port " &  to_string (port) & " ...", log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		query_element (
			position	=> module_cursor,
			process		=> query_netchangers'access);
		
		return port_position;
	end position;
	
	procedure delete_unit (
	-- Deletes a unit of a device. 
	-- In case the last unit has been delete, then the device is 
	-- deleted entirely from module.devices.
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

	procedure insert_ports (
	-- Inserts the given device ports in the net segments.
	-- If a port lands on either the start or end point of a segment, it will
	-- be regarded as "connected" with the segment.
	-- If a ports lands between start or end point of a segment, nothing happens
	-- as the docking to net segments is possible on segment ends/starts only.
	-- CS: Automatic splitting the segment into two and placing a junction is not supported
	-- jet and probably not a good idea.
		module			: in type_modules.cursor;		-- the module
		device			: in type_device_name;			-- the device
		ports			: in et_libraries.type_ports.map; -- the ports to be inserted
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

					use type_ports;
					port_cursor : type_ports.cursor := ports.first;

					port_processed : boolean;
					
					procedure query_segments (strand : in out type_strand) is
						use type_net_segments;
						segment_cursor : type_net_segments.cursor := strand.segments.first;

						procedure change_segment (segment : in out type_net_segment) is
						begin -- change_segment
							-- If port sits on start OR end point of segment AND if it
							-- is not already in the segment then append it to segment.ports_devices.
							-- append it to the portlist of the segment.
							if 	segment.coordinates_start = element (port_cursor).position or
								segment.coordinates_end = element (port_cursor).position then

								-- If port not already in segment, append it.
								-- Otherwise it must not be appended again.
								if type_ports_device.contains (
									container	=> segment.ports_devices,
									item		=> (device, key (port_cursor)) -- IC23, VCC_IO
									) then 

									log (" already there -> skipped", log_threshold + 3);
								else
									type_ports_device.insert (
										container	=> segment.ports_devices,
										new_item	=> (device, key (port_cursor))); -- IC23, VCC_IO

									log (" sits on segment -> inserted", log_threshold + 3);

								end if;
								
								port_processed := true;
							end if;
								
						end change_segment;

					begin -- query_segments
						-- On the first segment, where the port sits on, this loop ends prematurely.
						while not port_processed and segment_cursor /= type_net_segments.no_element loop

							log_indentation_up;
							log ("probing " & to_string (segment_cursor), log_threshold + 2);
							
							type_net_segments.update_element (
								container	=> strand.segments,
								position	=> segment_cursor,
								process		=> change_segment'access);

							log_indentation_down;
							next (segment_cursor);
						end loop;

					end query_segments;
				
				begin -- query_strands
					-- loop in portlist
					while port_cursor /= type_ports.no_element loop
						log ("probing port " & to_string (key (port_cursor)), log_threshold + 1);
						log_indentation_up;

						-- If the current port sits on a strand, this flag will go true. Other 
						-- strands will then not be looked at because the port can only sit on 
						-- one strand.
						port_processed := false;
						
						strand_cursor := net.strands.first;
						while strand_cursor /= type_strands.no_element loop

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

							-- If the port has been processed, there is no need to look up
							-- other strands for this port.
							if port_processed then exit; end if;
							
							next (strand_cursor);
						end loop;

						log_indentation_down;
						next (port_cursor);
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
	-- Moves the given unit within the schematic. Disconnects the unit from
	-- start or end points of net segments.
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

	procedure move_unit_placeholder (
	-- Moves the name placeholder of the given unit.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in et_coordinates.type_point; -- x/y
		meaning			: in et_libraries.type_text_meaning; -- name, value, purpose
		log_threshold	: in type_log_level) is

		module_cursor : type_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
				use et_schematic.type_units;
				unit_cursor : et_schematic.type_units.cursor;

				procedure move_placeholder (
					unit_name	: in type_unit_name.bounded_string;
					unit		: in out et_schematic.type_unit) is
					use et_coordinates;
				begin
					-- The given meaning determines the placeholder to be moved:
					case meaning is
						when REFERENCE =>
							case coordinates is
								when ABSOLUTE =>
									unit.reference.position := point;

								when RELATIVE =>
									move (
										point	=> unit.reference.position,
										offset	=> point);
							end case;
							
						when VALUE =>
							case coordinates is
								when ABSOLUTE =>
									unit.value.position := point;

								when RELATIVE =>
									move (
										point	=> unit.value.position,
										offset	=> point);
							end case;
							
						when PURPOSE =>
							case coordinates is
								when ABSOLUTE =>
									unit.purpose.position := point;

								when RELATIVE =>
									move (
										point	=> unit.purpose.position,
										offset	=> point);
							end case;

						when others =>
							raise constraint_error; -- CS no longer required once et_libraries.type_text_meaning has been reworked.
					end case;
					
					exception
						when event: others =>
							log (message_error & "coordinates invalid !", console => true); -- CS required more details
							log (ada.exceptions.exception_information (event), console => true);
							raise;
					
				end move_placeholder;
				
			begin -- query_units
				if contains (device.units, unit_name) then

					-- locate unit by its name. it should be there.
					unit_cursor := find (device.units, unit_name);

					update_element (
						container	=> device.units,
						position	=> unit_cursor,
						process		=> move_placeholder'access);
					
				else
					unit_not_found (unit_name);
				end if;
			end query_units;
			
		begin -- query_devices
			if contains (module.devices, device_name) then

				-- Locate the device. It should be there.
				device_cursor := find (module.devices, device_name);

				-- locate the unit
				update_element (
					container	=> module.devices,
					position	=> device_cursor,
					process		=> query_units'access);
				
			else
				device_not_found (device_name);
			end if;
		end query_devices;

	begin -- move_unit_placeholder
		case coordinates is
			when ABSOLUTE =>
				log ("module " & to_string (module_name) &
					" moving " & to_string (device_name) & " unit " & 
					to_string (unit_name) & " placeholder" & to_string (meaning) & " to" &
					et_coordinates.to_string (point), log_threshold);

			when RELATIVE =>
				log ("module " & to_string (module_name) &
					" moving " & to_string (device_name) & " unit " & 
					to_string (unit_name) & " placeholder" & to_string (meaning) & " by" &
					et_coordinates.to_string (point), log_threshold);
		end case;

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_devices'access);
		
	end move_unit_placeholder;

	procedure rotate_ports (
	-- Rotates the given unit ports by given angle around the origin.
		ports	: in out et_libraries.type_ports.map; -- the portlist
		angle	: in et_coordinates.type_rotation) -- 90
		is
		use et_libraries.type_ports;

		procedure rotate (
			name	: in type_port_name.bounded_string;
			port	: in out type_port) is
		begin
			rotate (port.position, angle);
		end;

		procedure query_port (cursor : in type_ports.cursor) is begin
			update_element (
				container	=> ports,
				position	=> cursor,
				process		=> rotate'access);
		end;
			
	begin -- rotate_ports
		iterate (ports, query_port'access);
	end rotate_ports;
	
	procedure rotate_unit (
	-- Rotates the given unit within the schematic. Disconnects the unit from
	-- start or end points of net segments.
	-- Rotates the placeholders around the unit.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		rotation		: in et_coordinates.type_rotation; -- 90
		log_threshold	: in type_log_level) is

		module_cursor : type_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;

			position_of_unit : et_coordinates.type_coordinates;
			rotation_before : et_coordinates.type_rotation;

			ports_lib, ports_scratch : et_libraries.type_ports.map;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
				use et_schematic.type_units;
				unit_cursor : et_schematic.type_units.cursor;

				procedure rotate_unit (
					name	: in type_unit_name.bounded_string; -- A
					unit	: in out type_unit) is

					procedure rotate_placeholders (rot : in type_rotation) is begin
					-- Rotate position of placeholders around the unit origin. 
						rotate (unit.reference.position, rot);
						rotate (unit.value.position, rot);
						rotate (unit.purpose.position, rot);

						-- CS set rotation of placeholders ?
					end rotate_placeholders;
	
				begin -- rotate_unit
					case coordinates is
						when ABSOLUTE =>
							unit.rotation := rotation;
							rotate_placeholders (unit.rotation);
							
						when RELATIVE =>
							unit.rotation := add (rotation_before, rotation);
							rotate_placeholders (unit.rotation);
					end case;
				end rotate_unit;
				
			begin -- query_units
				if contains (device.units, unit_name) then
					-- locate unit by its name
					unit_cursor := find (device.units, unit_name);

					-- load unit position and current rotation
					position_of_unit := element (unit_cursor).position;
					rotation_before := element (unit_cursor).rotation;

					-- log unit position and current rotation
					log (to_string (position => position_of_unit) &
						 et_coordinates.to_string (rotation_before), log_threshold + 1);

					type_units.update_element (
						container	=> device.units,
						position	=> unit_cursor,
						process		=> rotate_unit'access);
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
					sheets : type_unit_positions.map;
				begin
					type_unit_positions.insert (
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
				log ("module " & to_string (module_name) &
					" rotating " & to_string (device_name) & " unit " & 
					to_string (unit_name) & " to" & et_coordinates.to_string (rotation), log_threshold);

			when RELATIVE =>
				if rotation in type_rotation_relative then
					log ("module " & to_string (module_name) &
						" rotating " & to_string (device_name) & " unit " & 
						to_string (unit_name) & " by" & et_coordinates.to_string (rotation), log_threshold);
				else
					log (message_error & "Relative rotation must be in range" & 
						et_coordinates.to_string (rotation_relative_min) &
						" .." & 
						et_coordinates.to_string (rotation_relative_max),
						console => true
						);
					
					raise constraint_error;
				end if;
		end case;
		
		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end rotate_unit;

	procedure rotate_unit_placeholder (
	-- Rotates the given unit placeholder around its origin.
	-- The rotation is absolute.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A
		rotation		: in et_coordinates.type_rotation_text; -- absolute ! -- 90
		meaning			: in et_libraries.type_text_meaning; -- name, value, purpose		
		log_threshold	: in type_log_level) is

		module_cursor : type_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
				use et_schematic.type_units;
				unit_cursor : et_schematic.type_units.cursor;

				procedure rotate_placeholder (
					name	: in type_unit_name.bounded_string; -- A
					unit	: in out type_unit) is
				begin -- rotate_placeholder
					case meaning is
						when REFERENCE =>
							unit.reference.rotation := rotation;
							
						when VALUE =>
							unit.value.rotation := rotation;
							
						when PURPOSE =>
							unit.purpose.rotation := rotation;

						when others =>
							raise constraint_error; -- CS no longer required once et_libraries.type_text_meaning has been reworked.
					end case;
				end rotate_placeholder;
				
			begin -- query_units
				if contains (device.units, unit_name) then

					-- locate unit by its name
					unit_cursor := find (device.units, unit_name);

					type_units.update_element (
						container	=> device.units,
						position	=> unit_cursor,
						process		=> rotate_placeholder'access);
				else
					unit_not_found (unit_name);
				end if;
			end query_units;

		begin -- query_devices
			if contains (module.devices, device_name) then

				-- locate the device. it should be there
				device_cursor := find (module.devices, device_name);

				update_element (
					container	=> module.devices,
					position	=> device_cursor,
					process		=> query_units'access);
				
			else
				device_not_found (device_name);
			end if;
		end query_devices;
		
	begin -- rotate_unit_placeholder
		log ("module " & to_string (module_name) &
			" rotating " & to_string (device_name) & " unit " &
			to_string (unit_name) & " placeholder" & to_string (meaning) & " to" &
			et_coordinates.to_string (rotation), log_threshold);
		
		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end rotate_unit_placeholder;
	
	procedure drag_net_segments (
	-- Drags the net segments according to the given drag_list.
	-- Changes the position of start or end points of segments.
	-- Does NOT create new connections with segments if a port
	-- lands on the start or end point of another segment.
	-- Does NOT create a new connection with a segments if a port
	-- lands between start and end point.
		module			: in type_modules.cursor;		-- the module
		drag_list		: in type_drags_of_ports.map;	-- the old and new port positions
		sheet			: in type_sheet;				-- the sheet to look at
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

							procedure change_segment (segment : in out type_net_segment) is 
							-- Changes the position of start or end point of a segment according to the drag point.
							begin -- change_segment
								
								-- if port sits on a start point of a segment -> move start point
								if segment.coordinates_start = element (drag_cursor).before then
									log ("move segment start point from" & 
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
									log ("move segment end point from" & 
										to_string (segment.coordinates_end),
										log_threshold + 3);

									segment.coordinates_end := element (drag_cursor).after;

									log ("to" & 
										to_string (segment.coordinates_end),
										log_threshold + 3);

									drag_processed := true;
								end if;

							end change_segment;

						begin -- query_segment
							log_indentation_up;
							log ("probing " & to_string (segment_cursor), log_threshold + 2);
							log_indentation_up;
							
							update_element (
								container	=> strand.segments,
								position	=> segment_cursor,
								process		=> change_segment'access);
											
							log_indentation_down;
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
						log_indentation_up;

						-- If the current drag point sits on a strand, this flag will go true. Other 
						-- strands will then not be looked at because the point can only sit on 
						-- one strand.
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

				-- The drag operation might result in new port-to-net connections.
				-- So we must insert new ports in segments.
				-- Insert possible new unit ports in the nets (type_module.nets):
				log_indentation_up;
				
				insert_ports (
					module			=> module_cursor,
					device			=> device_name,
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

	procedure rename_ports (
		module			: in type_modules.cursor;		-- the module
		device_before	: in type_device_name;			-- the device name before like IC1
		device_after	: in type_device_name;			-- the device name after like IC23
		sheets			: in type_unit_positions.map;	-- the sheet numbers where the units can be found. CS implementation required
		log_threshold	: in type_log_level) is

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
								use type_ports_device;

								procedure query_ports (segment : in out type_net_segment) is
								-- Tests device ports of given segment if their device name matches the given device name.
								-- On match replace the old device name by the new device name.
									
									procedure query_port (port_cursor : in type_ports_device.cursor) is begin
										if element (port_cursor).device_name = device_before then -- IC1

											replace_element (
												container	=> segment.ports_devices,
												position	=> port_cursor,
												new_item	=> (
														device_name	=> device_after, -- IC23
														port_name 	=> element (port_cursor).port_name) -- unchanged
												);
										end if;
									end query_port;
									
								begin -- query_ports
									iterate (segment.ports_devices, query_port'access); -- loop in portlist of given segment
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
		
	begin -- rename_ports
		log ("renaming ports in nets ...", log_threshold);

		log_indentation_up;
		
		update_element (
			container	=> modules,
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;
	end rename_ports;
	
	procedure rename_device (
	-- Renames the given device.
		module_name			: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name_before	: in type_device_name; -- IC1
		device_name_after	: in type_device_name; -- IC23
		log_threshold		: in type_log_level) is

		module_cursor : type_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;

			device_cursor_before : et_schematic.type_devices.cursor;
			device_cursor_after  : et_schematic.type_devices.cursor;
			inserted : boolean;

			-- temporarily storage of unit coordinates:
			position_of_units : type_unit_positions.map;

		begin -- query_devices
			-- locate the device by the old name
			device_cursor_before := find (module.devices, device_name_before); -- IC1

			if device_cursor_before /= et_schematic.type_devices.no_element then -- the device should be there

				-- copy elements and properties of the old device to a new one:
				et_schematic.type_devices.insert (
					container	=> module.devices,
					key			=> device_name_after, -- IC23
					new_item	=> element (device_cursor_before), -- all elements and properties of IC1
					inserted	=> inserted,
					position	=> device_cursor_after);

				if not inserted then
					device_already_exists (device_name_after);
				end if;

				-- check conformity of prefix
				if not conventions.prefix_valid (device_name_after) then
					null;
					--device_prefix_invalid (device_name_after);
				end if;

				-- Before deleting the old device, the coordinates of the
				-- units must be fetched. These coordinates will later assist
				-- in renaming the port names in connected net segments.
				position_of_units := positions_of_units (device_cursor_before);
				
				-- delete the old device
				et_schematic.type_devices.delete (
					container	=> module.devices,
					position	=> device_cursor_before);

				-- rename all ports in module.nets
				rename_ports (
					module			=> module_cursor,
					device_before	=> device_name_before,
					device_after	=> device_name_after,
					sheets			=> position_of_units, -- the sheets to look at
					log_threshold	=> log_threshold + 1);

			else
				device_not_found (device_name_before);
			end if;
		end query_devices;

	begin -- rename_device
		log ("module " & to_string (module_name) &
			" renaming " & to_string (device_name_before) & " to " & 
			to_string (device_name_after),
			log_threshold);

		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_devices'access);

		log_indentation_down;
	end rename_device;

	procedure set_value (
	-- Sets the value of a device.
		module_name			: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name			: in type_device_name; -- R2
		value				: in type_value.bounded_string; -- 470R
		log_threshold		: in type_log_level) is
		
		module_cursor : type_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;

			device_cursor : et_schematic.type_devices.cursor;

			procedure set_value (
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
			begin
				device.value := value;
			end;
			
		begin -- query_devices
			-- locate the device
			device_cursor := find (module.devices, device_name); -- R1

			if device_cursor /= et_schematic.type_devices.no_element then -- the device should be there

				-- Only real devices have a value. Issue warning if targeted device is virtual.
				if element (device_cursor).appearance = SCH_PCB then

					-- Check value regarding the device category:
					if conventions.value_valid (value, prefix (device_name)) then 
					
						update_element (
							container	=> module.devices,
							position	=> device_cursor,
							process		=> set_value'access);

					else
						log (message_error & "value " & enclose_in_quotes (to_string (value)) 
							 & " invalid for this kind of device !", console => true);
						-- CS more details ?
						raise constraint_error;
					end if;
				else
					log (message_warning & "device " & to_string (device_name) &
						 " is virtual and has no value !");
				end if;

			else
				device_not_found (device_name);
			end if;
		end query_devices;
		
	begin -- set_value
		log ("module " & to_string (module_name) &
			" setting " & to_string (device_name) & " value to " &
			enclose_in_quotes (to_string (value)),
			log_threshold);

		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_devices'access);

		log_indentation_down;
	end set_value;

	procedure set_purpose (
	-- Sets the purpose of a device.
		module_name			: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name			: in type_device_name; -- R2
		purpose				: in type_device_purpose.bounded_string; -- brightness_control
		log_threshold		: in type_log_level) is

		module_cursor : type_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;

			device_cursor : et_schematic.type_devices.cursor;

			procedure set_purpose (
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
			begin
				device.purpose := purpose;
			end;
			
		begin -- query_devices
			-- locate the device
			device_cursor := find (module.devices, device_name); -- R1

			if device_cursor /= et_schematic.type_devices.no_element then -- the device should be there

				-- Only real devices have a purpose. Issue warning if targeted device is virtual.
				if element (device_cursor).appearance = SCH_PCB then

					update_element (
						container	=> module.devices,
						position	=> device_cursor,
						process		=> set_purpose'access);

				else
					log (message_warning & "device " & to_string (device_name) &
						 " is virtual and has no practical purpose !");
				end if;

			else
				device_not_found (device_name);
			end if;
		end query_devices;

	begin -- set_purpose
		log ("module " & to_string (module_name) &
			" setting " & to_string (device_name) & " purpose to " &
			enclose_in_quotes (to_string (purpose)),
			log_threshold);

		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_devices'access);

		log_indentation_down;
	end set_purpose;

	procedure set_partcode (
	-- Sets the partcode of a device.
		module_name			: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name			: in type_device_name; -- R2
		partcode			: in type_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
		log_threshold		: in type_log_level) is

		module_cursor : type_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;

			device_cursor : et_schematic.type_devices.cursor;

			procedure set_partcode (
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
			begin
				device.partcode := partcode;
			end;
			
		begin -- query_devices
			-- locate the device
			device_cursor := find (module.devices, device_name); -- R1

			if device_cursor /= et_schematic.type_devices.no_element then -- the device should be there

				-- Only real devices have a purpose. Issue warning if targeted device is virtual.
				if element (device_cursor).appearance = SCH_PCB then

					update_element (
						container	=> module.devices,
						position	=> device_cursor,
						process		=> set_partcode'access);

				else
					log (message_warning & "device " & to_string (device_name) &
						 " is virtual and has no partcode !");
				end if;

			else
				device_not_found (device_name);
			end if;
		end query_devices;

	begin -- set_partcode
		log ("module " & to_string (module_name) &
			" setting " & to_string (device_name) & " partcode to " &
			enclose_in_quotes (to_string (partcode)),
			log_threshold);

		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_devices'access);

		log_indentation_down;
	end set_partcode;

	function exists_device_port (
	-- Returns true if given device with the given port exists in module indicated by module_cursor.
		module_cursor	: in type_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		port_name		: in type_port_name.bounded_string) -- CE
		return boolean is

		result : boolean := false; -- to be returned. goes true once the target has been found
		
		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use et_libraries.type_unit_name;
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in et_schematic.type_device) is
				use et_schematic.type_units;
				unit_cursor : et_schematic.type_units.cursor := device.units.first;
				use et_libraries.type_ports;
				ports : et_libraries.type_ports.map;
				use et_libraries.type_port_name;
			begin
				while unit_cursor /= type_units.no_element loop
					--log ("unit " & type_unit_name.to_string (key (unit_cursor)));
					--log ("port " & type_port_name.to_string (port_name));
					
					-- fetch the unit ports from the library model
					ports := ports_of_unit (device_cursor, key (unit_cursor));

					-- if the unit has a port named port_name then we have
					-- a match. no further search required.
					if contains (ports, port_name) then
						result := true;
						exit;
					end if;
										
					next (unit_cursor);
				end loop;
			end query_units;
			
		begin -- query_devices
			if contains (module.devices, device_name) then -- device found

				device_cursor := find (module.devices, device_name);
					
				query_element (
					position	=> device_cursor,
					process		=> query_units'access);

			end if;
		end query_devices;
		
	begin -- exists_device_port
		query_element (
			position	=> module_cursor,
			process		=> query_devices'access);

		return result;
	end exists_device_port;
	
	function exists_device_unit_port (
	-- Returns true if given device exists in module indicated by module_cursor.
	-- The unit and port names are optionally.
		module_cursor	: in type_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		unit_name		: in type_unit_name.bounded_string := to_unit_name (""); -- A
		port_name		: in type_port_name.bounded_string := to_port_name ("")) -- CE
		return boolean is

		result : boolean := false; -- to be returned, goes true once the target has been found
		
		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use et_libraries.type_unit_name;
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in et_schematic.type_device) is
				use et_schematic.type_units;
				use et_libraries.type_ports;
				ports : et_libraries.type_ports.map;
				use et_libraries.type_port_name;
			begin
				if contains (device.units, unit_name) then
					if length (port_name) > 0 then -- search for port in unit

						-- fetch the unit ports from the library model
						ports := ports_of_unit (device_cursor, unit_name);

						if contains (ports, port_name) then
							result := true;
						end if;
						
					else
						result := true;
					end if;
				end if;
			end query_units;
			
		begin -- query_devices
			if contains (module.devices, device_name) then -- device found

				-- If unit name given, search for the unit.
				if length (unit_name) > 0 then
					device_cursor := find (module.devices, device_name);
					
					query_element (
						position	=> device_cursor,
						process		=> query_units'access);

				else
					result := true;
				end if;
				
			end if;
		end query_devices;
		
	begin -- exists_device_unit_port
		query_element (
			position	=> module_cursor,
			process		=> query_devices'access);

		return result;
	end exists_device_unit_port;

	function exists_submodule_port (
	-- Returns true if given submodule with the given port exists in module indicated by module_cursor.
		module_cursor	: in type_modules.cursor; -- motor_driver
		submod_instance	: in et_general.type_module_instance_name.bounded_string; -- MOT_DRV_3
		port_name		: in et_general.type_net_name.bounded_string) -- RESET
		return boolean is

		use et_general.type_module_instance_name;
		use submodules;
		
		result : boolean := false; -- to be returned, goes true once the target has been found

		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use type_submodules;
			submod_cursor : type_submodules.cursor;

			procedure query_ports (
			-- Searches the portlist of the submodule for a port having the port_name.
			-- Exits prematurely on match.
				submod_name	: in type_module_instance_name.bounded_string;
				submodule	: in type_submodule) is
				use type_net_name;
				use type_submodule_ports;
				port_cursor : type_submodule_ports.cursor := submodule.ports.first;
			begin
				while port_cursor /= type_submodule_ports.no_element loop
					if key (port_cursor) = port_name then
						result := true;
						exit;
					end if;

					next (port_cursor);
				end loop;
			end query_ports;
			
		begin -- query_submodules
			if contains (module.submods, submod_instance) then -- submodule found
				submod_cursor := find (module.submods, submod_instance);
				
				query_element (
					position	=> submod_cursor,
					process		=> query_ports'access);
								  
			end if;
		end query_submodules;
		
	begin -- exists_submodule_port
		query_element (
			position	=> module_cursor,
			process		=> query_submodules'access);

		return result;
	end exists_submodule_port;

	function exists_netchanger (
	-- Returns true if given netchanger exists in module indicated by module_cursor.
		module_cursor	: in type_modules.cursor; -- motor_driver
		index			: in submodules.type_netchanger_id) -- 1, 2, 3, ...
		return boolean is

		result : boolean := false; -- to be returned, goes true once the target has been found		

		procedure query_netchangers (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use submodules.type_netchangers;
		begin -- query_netchangers
			if contains (module.netchangers, index) then
				result := true;
			end if;
		end query_netchangers;
		
	begin -- exists_netchanger
		query_element (
			position	=> module_cursor,
			process		=> query_netchangers'access);

		return result;
	end exists_netchanger;

	procedure place_junction (
	-- Places a net junction at the given position.
	-- If the junction is to be placed between start and end point of a segment, then the segment 
	-- is split in two new segments with the junction between them.
	-- If there is no net segment at the given position, no junction is placed and warning issued.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		place			: in et_coordinates.type_coordinates; -- sheet/x/y
		log_threshold	: in type_log_level) is
		use et_coordinates;
		
		module_cursor : type_modules.cursor; -- points to the module being checked

		segment_found : boolean := false; -- goes true if a net segment has been found to place the junction at
	
		procedure query_nets (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use type_nets;
			net_cursor : type_nets.cursor := module.nets.first;

			procedure query_strands (
				net_name	: in type_net_name.bounded_string;
				net			: in out type_net) is
				use et_coordinates;
				
				use type_strands;
				strand_cursor : type_strands.cursor := net.strands.first;
				
				procedure query_segments (strand : in out type_strand) is
					use type_net_segments;
					segment_cursor : type_net_segments.cursor := strand.segments.first;
					old_segment : type_net_segment; -- here a backup of the old segment lives
					
					procedure insert_two_new_segments is
						segment_1, segment_2 : type_net_segment;

						procedure update_device_ports is 
						-- Queries the positions of the device ports in the old_segment. 
						-- By the position assigns the ports to the new segments. 
							use type_ports_device;

							procedure query_ports (cursor : in type_ports_device.cursor) is
								device_name 	: type_device_name; -- IC23
								port_name		: type_port_name.bounded_string; -- CE
								port_position 	: et_coordinates.type_point; -- the xy-position of the port
							begin
								device_name	:= element (cursor).device_name;
								port_name	:= element (cursor).port_name;

								-- locate the port by module, device and port name:
								port_position := type_point (position (module_name, device_name, port_name, log_threshold + 1));
								log_indentation_up;
								
								log ("device " & to_string (device_name) & " port " & to_string (port_name) &
									" at" & et_coordinates.to_string (point => port_position),
									log_threshold + 1);

								-- If the port was at the start point of the old segment, then
								-- it goes into segment_1.
								if port_position = old_segment.coordinates_start then
									insert (segment_1.ports_devices, element (cursor));

								-- If the port was at the end point of the old segment, then
								-- it goes into segment_2.
								elsif port_position = old_segment.coordinates_end then
									insert (segment_2.ports_devices, element (cursor));

								-- If port was somewhere else, we have a problem. This should never happen.
								else
									log_indentation_reset;
									log (message_error & "port not on segment !");
									raise constraint_error;
								end if;
								
								log_indentation_down;
							end query_ports;
							
						begin -- update_device_ports
							log ("updating device ports ...", log_threshold + 1);
							log_indentation_up;
							
							iterate (old_segment.ports_devices, query_ports'access);
							log_indentation_down;
						end update_device_ports;

						procedure update_submodule_ports is 
						-- Queries the positions of the submodule ports in the old_segment. 
						-- By the position assigns the ports to the new segments. 
							use type_ports_submodule;

							procedure query_ports (cursor : in type_ports_submodule.cursor) is
								submod_name 	: et_general.type_module_instance_name.bounded_string; -- MOT_DRV_3
								port_name		: type_net_name.bounded_string; -- RESET
								port_position 	: et_coordinates.type_point; -- the xy-position of the port
							begin
								submod_name	:= element (cursor).module_name; -- CLOCK_GENERATOR
								port_name	:= element (cursor).port_name;	-- RESET

								-- locate the port by module, submodule and port name:
								port_position := type_point (position (module_name, submod_name, port_name, log_threshold + 1));
								log_indentation_up;
								
								log ("submodule " & to_string (submod_name) & " port " & to_string (port_name) &
									" at" & et_coordinates.to_string (point => port_position),
									log_threshold + 1);

								-- If the port was at the start point of the old segment, then
								-- it goes into segment_1.
								if port_position = old_segment.coordinates_start then
									insert (segment_1.ports_submodules, element (cursor));

								-- If the port was at the end point of the old segment, then
								-- it goes into segment_2.
								elsif port_position = old_segment.coordinates_end then
									insert (segment_2.ports_submodules, element (cursor));

								-- If port was somewhere else, we have a problem. This should never happen.
								else
									log_indentation_reset;
									log (message_error & "port not on segment !");
									raise constraint_error;
								end if;
								
								log_indentation_down;
							end query_ports;
							
						begin -- update_submodule_ports
							log ("updating submodule ports ...", log_threshold + 1);
							log_indentation_up;
							
							iterate (old_segment.ports_submodules, query_ports'access);
							log_indentation_down;
						end update_submodule_ports;

						procedure update_netchanger_ports is 
						-- Queries the positions of the netchanger ports in the old_segment. 
						-- By the position assigns the ports to the new segments. 
							use type_ports_netchanger;
							use submodules;

							procedure query_ports (cursor : in type_ports_netchanger.cursor) is
								index	: type_netchanger_id; -- 1,2,3,...
								port	: type_netchanger_port_name; -- SLAVE/MASTER
								port_position 	: et_coordinates.type_point; -- the xy-position of the port
							begin
								index := element (cursor).index;
								port := element (cursor).port;

								-- locate the port by module, netchanger index and port:
								port_position := type_point (position (module_name, index, port, log_threshold + 1));
								log_indentation_up;
								
								log ("netchanger " & to_string (index) & " port " & to_string (port) &
									" at" & et_coordinates.to_string (point => port_position),
									log_threshold + 1);

								-- If the port was at the start point of the old segment, then
								-- it goes into segment_1.
								if port_position = old_segment.coordinates_start then
									insert (segment_1.ports_netchangers, element (cursor));

								-- If the port was at the end point of the old segment, then
								-- it goes into segment_2.
								elsif port_position = old_segment.coordinates_end then
									insert (segment_2.ports_netchangers, element (cursor));

								-- If port was somewhere else, we have a problem. This should never happen.
								else
									log_indentation_reset;
									log (message_error & "port not on segment !");
									raise constraint_error;
								end if;
								
								log_indentation_down;
							end query_ports;
							
						begin -- update_netchanger_ports
							log ("updating netchanger ports ...", log_threshold + 1);
							log_indentation_up;
							
							iterate (old_segment.ports_netchangers, query_ports'access);
							log_indentation_down;
						end update_netchanger_ports;
						
					begin -- insert_two_new_segments
						-- set start and end points of new segments
						segment_1.coordinates_start := old_segment.coordinates_start;
						segment_1.coordinates_end := type_point (place);
						segment_2.coordinates_start := type_point (place);
						segment_2.coordinates_end := old_segment.coordinates_end;

						-- set junctions
						segment_1.junctions.start_point := old_segment.junctions.start_point;
						segment_1.junctions.end_point := true; -- because there is the new junction
						segment_2.junctions.start_point := false; -- no need for another junction at the same place
						segment_2.junctions.end_point := old_segment.junctions.end_point;

						-- Ports which were part of the old segment must now be assigned to the 
						-- two new segments.
						update_device_ports;
						update_submodule_ports;
						update_netchanger_ports;
						
						type_net_segments.insert (
							container	=> strand.segments,
							before		=> segment_cursor,
							new_item	=> segment_1);

						type_net_segments.insert (
							container	=> strand.segments,
							before		=> segment_cursor,
							new_item	=> segment_2);
					end insert_two_new_segments;

					procedure junction_at_start_point (segment : in out type_net_segment) is begin
						segment.junctions.start_point := true;
					end;

					procedure junction_at_end_point (segment : in out type_net_segment) is begin
						segment.junctions.end_point := true;
					end;
					
				begin -- query_segments
					while segment_cursor /= type_net_segments.no_element loop

						-- The junction can be placed at the start or end point of a segment OR
						-- between start and end point of a segment. If none of these conditions
						-- is positive, go to next segment.
						
						--log_indentation_up;
						--log ("probing " & to_string (segment_cursor), log_threshold + 2);

						if type_point (place) = element (segment_cursor).coordinates_start then

							-- place junction at start point of segment
							update_element (
								container	=> strand.segments,
								position	=> segment_cursor,
								process		=> junction_at_start_point'access);

							segment_found := true;
							exit; -- no need to search for other segments
							
						elsif type_point (place) = element (segment_cursor).coordinates_end then

							-- place junction at end point of segment
							update_element (
								container	=> strand.segments,
								position	=> segment_cursor,
								process		=> junction_at_end_point'access);

							segment_found := true;
							exit; -- no need to search for other segments
							
						elsif between_start_and_end_point (
							point	=> type_point (place),
							segment	=> segment_cursor) then -- targeted segment found

							log ("net " & to_string (net_name) & " strand" &
								 to_string (position => strand.position), log_threshold + 1);
							log (to_string (segment_cursor), log_threshold + 1);

							-- backup old segment (it contains port and junction information)
							old_segment := element (segment_cursor);

							-- delete the targeted segment. it will later be replaced by two new segments.
							delete (strand.segments, segment_cursor);

							-- Insert two new segments in the strand
							-- and rearrange the ports of devices, submodules and netchangers.
							insert_two_new_segments;
							
							-- no further search required
							segment_found := true; 
							exit;
						end if;
							
 						--log_indentation_down;
						next (segment_cursor);
					end loop;

				end query_segments;
					
			begin -- query_strands
				while (not segment_found) and strand_cursor /= type_strands.no_element loop
					
					-- We pick out only the strands on the targeted sheet:
					if sheet (element (strand_cursor).position) = sheet (place) then
						--log ("net " & to_string (key (net_cursor)), log_threshold + 1);
						--log_indentation_up;
						
						--log ("strand " & to_string (position => element (strand_cursor).position),
						--	log_threshold + 1);
					
						update_element (
							container	=> net.strands,
							position	=> strand_cursor,
							process		=> query_segments'access);
					
						--log_indentation_down;
					end if;

					next (strand_cursor);
				end loop;
			end query_strands;
			
		begin -- query_nets
			while (not segment_found) and net_cursor /= type_nets.no_element loop

				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);

				next (net_cursor);
			end loop;
		end query_nets;

	begin -- place_junction
		log ("module " & to_string (module_name) & " placing junction at" &
			 to_string (position => place) & " ...", log_threshold);
		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_nets'access);

		if not segment_found then
			log (message_warning & "attempt to place junction in the void. Junction not placed !");
		end if;
		
		log_indentation_down;
	end place_junction;

	function next_device_name (
	-- Returns for the given device prefix the next available device name in the module.
	-- Example: prefix is C. If there are C1, C12, C1034 and C1035 the return will be C2.
		module_cursor	: in type_modules.cursor;
		prefix			: in et_libraries.type_device_name_prefix.bounded_string) -- C
		return et_libraries.type_device_name is -- C2
		
		next_name : et_libraries.type_device_name; -- to be returned

		procedure search_gap (
		-- Searches for the lowest available device name. Looks at devices
		-- whose prefix equals the given prefix. Example: If given prefix is R, it looks
		-- for the lowest available resistor index.
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor := module.devices.first;
			use et_libraries.type_device_name_prefix;

			-- We start the search with index 1. Not 0 because this would result in a zero based
			-- numbering order. Index zero is allowed but not automatically choosen.
			index_expected : et_libraries.type_device_name_index := type_device_name_index'first + 1;

			gap_found : boolean := false; -- goes true once a gap has been found
		begin -- search_gap
			while device_cursor /= et_schematic.type_devices.no_element loop
				if et_libraries.prefix (key (device_cursor)) = prefix then -- category match
					
					if index (key (device_cursor)) /= index_expected then -- we have a gap

						-- build the next available device name and exit
						next_name := to_device_name (prefix, index_expected);
						gap_found := true;
						exit;
					end if;

					index_expected := index_expected + 1;
				end if;
				
				next (device_cursor);
			end loop;

			-- If no gap has been found, then the device name must be assembled
			-- using the latest index_expected.
			if not gap_found then
				next_name := to_device_name (prefix, index_expected);
			end if;
			
		end search_gap;
		
	begin -- next_device_name
		query_element (
			position	=> module_cursor,
			process		=> search_gap'access);
		
		return next_name;
	end next_device_name;

	type type_unit_cursors_lib is record
		int : et_libraries.type_units_internal.cursor;
		ext : et_libraries.type_units_external.cursor;
	end record;

	function first_unit (device_cursor : in et_libraries.type_devices.cursor) return type_unit_cursors_lib is
	-- Returns the cursor of the first internal or external unit. Searches first in internal and
	-- then in external units. The search order is further-on determined
	-- by the add levels of the units. Priority is add level MUST, then ALWAYS, then NEXT, then REQUEST, then CAN.
	-- If no suitable internal unit found, the cursor of internal units in the return is no_element.
	-- If no suitable external unit found, the cursor of external units in the return is no_element.
		cursors : type_unit_cursors_lib; -- to be returned
		use et_libraries;
		use et_libraries.type_devices;
		use et_libraries.type_units_internal;
		use et_libraries.type_units_external;

		procedure query_units (
			device_name	: in type_device_model_file.bounded_string;
			device		: in et_libraries.type_device) is

			function first_internal (add_level : in et_libraries.type_unit_add_level) 
				return et_libraries.type_units_internal.cursor is
			-- Searches for a unit with given add_level. Returns the cursor of that unit.
			-- If no suitable unit found, returns cursor with no_element.
				cursor : type_units_internal.cursor := device.units_internal.first;
			begin
				while cursor /= type_units_internal.no_element loop
					if element (cursor).add_level = add_level then
						return cursor; -- unit found, no further search required. exit prematurely.
					end if;
					next (cursor);
				end loop;
				-- no unit found. return no_element:
				return type_units_internal.no_element;
			end;

			function first_external (add_level : in et_libraries.type_unit_add_level) 
				return et_libraries.type_units_external.cursor is
			-- Searches for a unit with given add_level. Returns the cursor of that unit.
			-- If no suitable unit found, returns cursor with no_element.
				cursor : type_units_external.cursor := device.units_external.first;
			begin
				while cursor /= type_units_external.no_element loop
					if element (cursor).add_level = add_level then
						return cursor; -- unit found, no further search required. exit prematurely.
					end if;
					next (cursor);
				end loop;
				-- no unit found. return no_element:
				return type_units_external.no_element;
			end;
			
		begin -- query_units
			-- First search among the internal units for a MUST-unit:
			cursors.int := first_internal (MUST);

			-- if no MUST-unit found, search for an ALWAYS-unit:
			if cursors.int = type_units_internal.no_element then
				cursors.int := first_internal (ALWAYS);

				-- if no ALWAYS-unit found, search for a NEXT-unit:
				if cursors.int = type_units_internal.no_element then
					cursors.int := first_internal (NEXT);

					-- if no NEXT-unit found, search for a REQUEST-unit
					if cursors.int = type_units_internal.no_element then
						cursors.int := first_internal (REQUEST);

						-- if no REQUEST-unit found, search for a CAN-unit
						if cursors.int = type_units_internal.no_element then
							cursors.int := first_internal (et_libraries.CAN);
						end if;
					end if;					
				end if;
			end if;

			-- if no suitable internal unit found, search among the external units:
			if cursors.int = type_units_internal.no_element then

				-- search among the external units for a MUST-unit
				cursors.ext := first_external (MUST);

				-- if no MUST-unit found, search for an ALWAYS-unit:
				if cursors.ext = type_units_external.no_element then
					cursors.ext := first_external (ALWAYS);

					-- if no ALWAYS-unit found, search for a NEXT-unit:
					if cursors.ext = type_units_external.no_element then
						cursors.ext := first_external (NEXT);

						-- if no NEXT-unit found, search for a REQUEST-unit
						if cursors.ext = type_units_external.no_element then
							cursors.ext := first_external (REQUEST);

							-- if no REQUEST-unit found, search for a CAN-unit
							if cursors.ext = type_units_external.no_element then
								cursors.ext := first_external (et_libraries.CAN);
							end if;
						end if;					
					end if;
				end if;
			
				-- if no suitable external unit found, we have a problem:
				if cursors.ext = type_units_external.no_element then
					log (message_error & " Device model has no units !", console => true);
					raise constraint_error;
				end if;

			end if;
			
		end query_units;
		
	begin -- first_unit
		query_element (
			position	=> device_cursor,
			process		=> query_units'access);
		
		return cursors;
	end first_unit;

	function any_unit (
	-- Returns the cursor of the desired internal or external unit.
		device_cursor	: in et_libraries.type_devices.cursor;
		unit_name		: in type_unit_name.bounded_string)
		return type_unit_cursors_lib is

		cursors : type_unit_cursors_lib; -- to be returned
		
		use et_libraries;
		use et_libraries.type_devices;
		use et_libraries.type_unit_name;
		use et_libraries.type_units_internal;
		use et_libraries.type_units_external;

		procedure query_units (
			device_name	: in type_device_model_file.bounded_string;
			device		: in et_libraries.type_device) is
		begin -- query_units
			-- First search among the internal units:
			cursors.int := device.units_internal.first;

			while cursors.int /= type_units_internal.no_element loop
				if key (cursors.int) = unit_name then
					exit; -- unit found, no further search required. exit prematurely.
				end if;
				next (cursors.int);
			end loop;

			-- if no suitable internal unit found, search among the external units:
			if cursors.int = type_units_internal.no_element then

				cursors.ext := device.units_external.first;

				while cursors.ext /= type_units_external.no_element loop
					if key (cursors.ext) = unit_name then
						exit; -- unit found, no further search required. exit prematurely.
					end if;
					next (cursors.ext);
				end loop;
				
				-- if no suitable external unit found, we have a problem:
				if cursors.ext = type_units_external.no_element then
					log (message_error & "unit " & et_libraries.to_string (unit_name) &
						 " not found in device model !", console => true);
					raise constraint_error;
				end if;

			end if;
			
		end query_units;
		
	begin -- any_unit
		query_element (
			position	=> device_cursor,
			process		=> query_units'access);
		
		return cursors;
	end any_unit;

	
	function placeholders_of_package (
	-- Returns the placeholders of the package of a device. The package is indirectly selected
	-- by the given variant name. The given device is accessed by the given device cursor.
		device	: in et_libraries.type_devices.cursor;
		variant	: in et_libraries.type_component_variant_name.bounded_string) -- N, D, S_0805
		return et_pcb.type_text_placeholders is
		use et_libraries;
		use et_libraries.type_devices;
		use et_libraries.type_component_variants;
		placeholders		: et_pcb.type_text_placeholders; -- to be returned

		-- fetch the package variants available for the given device:
		variants_available	: type_component_variants.map := element (device).variants;
		
		variant_cursor		: type_component_variants.cursor;
		package_model		: type_package_model_file.bounded_string; -- ../lbr/smd/SO15.pac

		use et_pcb;		
		use et_pcb.type_packages;
		package_cursor		: et_pcb.type_packages.cursor;

	begin -- placeholders_of_package
		
		-- locate the given variant in the device:
		variant_cursor := type_component_variants.find (variants_available, variant);

		-- get the package model name:
		package_model := element (variant_cursor).package_model; -- ../lbr/smd/SO15.pac

		-- locate the package model in the package library:
		package_cursor := et_pcb.type_packages.find (et_pcb.packages, package_model);

		-- fetch the placeholders of silk screen top and bottom
		placeholders.silk_screen.top := element (package_cursor).silk_screen.top.placeholders;
		placeholders.silk_screen.bottom := element (package_cursor).silk_screen.bottom.placeholders;

		-- fetch the placeholders of assembly documentation top and bottom
		placeholders.assy_doc.top := element (package_cursor).assembly_documentation.top.placeholders;
		placeholders.assy_doc.bottom := element (package_cursor).assembly_documentation.bottom.placeholders;
		
		return placeholders;
	end placeholders_of_package;
	
	procedure add_device (
	-- Adds a device to the schematic. The unit is determined by the unit add levels.
	-- If the given variant is empty (zero length) the the device is assumed to be virtual.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_model	: in type_device_model_file.bounded_string; -- ../libraries/devices/logic_ttl/7400.dev
		variant			: in et_libraries.type_component_variant_name.bounded_string; -- N, D, S_0805
		place			: in et_coordinates.type_coordinates; -- sheet/x/y
		rotation		: in et_coordinates.type_rotation; -- 90		
		log_threshold	: in type_log_level) is

		use et_coordinates;
		
		module_cursor : type_modules.cursor; -- points to the targeted module

		use et_libraries.type_devices;
		device_cursor_lib : et_libraries.type_devices.cursor; -- points to the device in the library

		procedure add (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;
			device_cursor_sch : et_schematic.type_devices.cursor;
			inserted : boolean;

			-- build the next available device name:
			next_name : et_libraries.type_device_name := 
				next_device_name (module_cursor, element (device_cursor_lib).prefix);

			unit_cursors : type_unit_cursors_lib;

			use et_libraries.type_units_internal;
			use et_libraries.type_units_external;

			procedure add_unit_internal (
			-- Add an internal unit to the schematic device.
			-- The unit to be added is accessed by unit_cursors.int.
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
			begin
				log ("adding internal unit " & to_string (key (unit_cursors.int)), log_threshold + 2);
				
				case element (device_cursor_lib).appearance is
					when SCH =>
						type_units.insert (
							container	=> device.units,
							key			=> key (unit_cursors.int), -- the unit name like A, B
							new_item	=> (
								appearance	=> SCH,
								position	=> place, -- the coordinates provided by the calling unit (sheet,x,y)
								rotation	=> rotation, -- the rotation provided by the calling unit
								others 		=> <>) -- rotation and mirror
								);
						
					when SCH_PCB =>
						type_units.insert (
							container	=> device.units,
							key			=> key (unit_cursors.int), -- the unit name like A, B, VCC_IO_BANK_1
							new_item	=> (
								appearance	=> SCH_PCB,
								position	=> place, -- the coordinates provided by the calling unit (sheet,x,y)
								rotation	=> rotation, -- the rotation provided by the calling unit								
								reference	=> element (unit_cursors.int).symbol.reference, -- placeholder for device name
								value		=> element (unit_cursors.int).symbol.value,		-- placeholder for device value
								purpose		=> element (unit_cursors.int).symbol.purpose,	-- placeholder for device purpose
								others 		=> <>)
								);
						
					when others => null; -- CS
				end case;
				
			end add_unit_internal;

			procedure add_unit_external (
			-- Add an external unit to the schematic device.
			-- The unit to be added is accessed by unit_cursors.ext.
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
				use et_libraries.type_symbols;
				symbol_cursor : et_libraries.type_symbols.cursor;
				symbol_file : et_libraries.type_symbol_model_file.bounded_string; -- *.sym
			begin
				log ("adding external unit " & to_string (key (unit_cursors.ext)), log_threshold + 2);
				
				case element (device_cursor_lib).appearance is
					when SCH =>
						type_units.insert (
							container	=> device.units,
							key			=> key (unit_cursors.ext), -- the unit name like A, B
							new_item	=> (
								appearance	=> SCH,
								position	=> place, -- the coordinates provided by the calling unit (sheet,x,y)
								rotation	=> rotation, -- the rotation provided by the calling unit								
								others 		=> <>) -- rotation and mirror
								);
						
					when SCH_PCB =>
						-- The symbol file name is provided by unit_cursors.ext.
						symbol_file := element (unit_cursors.ext).file; -- *.sym
						
						-- Locate the external symbol in container "symbols".
						-- The key into symbols is the file name (*.sym).
						symbol_cursor := et_libraries.type_symbols.find (symbols, symbol_file);

						-- CS: The symbol should be there now. Otherwise symbol_cursor would assume no_element
						-- and constraint_error would arise here:
						
						type_units.insert (
							container	=> device.units,
							key			=> key (unit_cursors.ext), -- the unit name like A, B, VCC_IO_BANK_1
							new_item	=> (
								appearance	=> SCH_PCB,
								position	=> place, -- the coordinates provided by the calling unit (sheet,x,y)
								rotation	=> rotation, -- the rotation provided by the calling unit								
								reference	=> element (symbol_cursor).reference,	-- placeholder for device name
								value		=> element (symbol_cursor).value,		-- placeholder for device value
								purpose		=> element (symbol_cursor).purpose,		-- placeholder for device purpose
								others 		=> <>)
								);

					when others => null; -- CS
				end case;

			end add_unit_external;

			ports : et_libraries.type_ports.map;
			
		begin -- add
			log ("adding device " & to_string (next_name), log_threshold + 1);
			log_indentation_up;
			
			case element (device_cursor_lib).appearance is
				when SCH =>
					et_schematic.type_devices.insert (
						container	=> module.devices,
						inserted	=> inserted,
						position	=> device_cursor_sch,
						key			=> next_name,
						new_item	=> (
							appearance 	=> SCH,
							model		=> key (device_cursor_lib),
							units		=> type_units.empty_map
							));

				when SCH_PCB =>
					-- A real device requires a package variant.
					if type_component_variant_name.length (variant) > 0 then

						if variant_available (device_cursor_lib, variant) then
							et_schematic.type_devices.insert (
								container	=> module.devices,
								inserted	=> inserted,
								position	=> device_cursor_sch,
								key			=> next_name,
								new_item	=> (
									appearance 	=> SCH_PCB,
									model		=> key (device_cursor_lib),
									units		=> type_units.empty_map,
									value		=> element (device_cursor_lib).value, -- if predefined in dev. model
									bom			=> YES,
									variant		=> variant,

									-- Initially, the text placeholders are copies of the placeholders 
									-- defined in the package.
									-- Extract them from the device model and the variant:
									text_placeholders	=> placeholders_of_package (device_cursor_lib, variant),
									
									others		=> <>
									));
							
						else -- variant not available
							log (message_error & "package variant " & enclose_in_quotes (to_string (variant)) &
								 " not available in the specified device model !", console => true);
							raise constraint_error;
						end if;
						
					else -- no variant specified
						log (message_error & "device requires specification of package variant !",
							 console => true);
						raise constraint_error;
					end if;
					
				when others => null; -- CS
			end case;

			-- Add first available unit (according to search order specified in function first_unit)
			-- to device in schematic.
			unit_cursors := first_unit (device_cursor_lib);

			-- If a internal unit available, add it to device. If no internal unit available
			-- but an external, add it to the device. So the operator will not take notice
			-- whether an internal or external unit is placed.
			if unit_cursors.int /= type_units_internal.no_element then

				et_schematic.type_devices.update_element (
					container	=> module.devices,
					position	=> device_cursor_sch,
					process		=> add_unit_internal'access);

				-- fetch ports of unit and their positions relative to the unit origin
				log_indentation_up;				
				log ("fetching relative port positions of internal unit " &
					 to_string (key (unit_cursors.int)) & " ...", log_threshold + 2);
				
				ports := ports_of_unit (
					device_cursor	=> device_cursor_sch,
					unit_name		=> key (unit_cursors.int));

			-- no internal unit available -> add external unit
			elsif unit_cursors.ext /= type_units_external.no_element then
				
				et_schematic.type_devices.update_element (
					container	=> module.devices,
					position	=> device_cursor_sch,
					process		=> add_unit_external'access);

				-- fetch ports of unit and their positions relative to the unit origin
				log_indentation_up;
				log ("fetching relative port positions of external unit " &
					 to_string (key (unit_cursors.ext)) & " ...", log_threshold + 2);

				ports := ports_of_unit (
					device_cursor	=> device_cursor_sch,
					unit_name		=> key (unit_cursors.ext));
				
			else
				raise constraint_error; -- CS should never happen. function first_unit excludes this case.
			end if;

			-- Calculate the absolute positions of the unit ports. Rotate first if required:
			log ("calculating absolute port positions ...", log_threshold + 2);
			if rotation /= rotation_zero then
				rotate_ports (ports, rotation);
			end if;

			move_ports (ports, place);
			
			-- Insert the new unit ports in the nets (type_module.nets):
			insert_ports (
				module			=> module_cursor,
				device			=> next_name,
				ports			=> ports,
				sheet			=> et_coordinates.sheet (place),
				log_threshold	=> log_threshold + 2);
			
			log_indentation_down;
			log_indentation_down;
		end add;
			
	begin -- add_device
		if type_component_variant_name.length (variant) > 0 then -- real device
			log ("module " & to_string (module_name) &
				" adding device " & to_string (device_model) &
				" package variant " & to_string (variant) &
				" at" &
				to_string (position => place) &
				" rotation" & et_coordinates.to_string (rotation),				
				log_threshold);
			
		else -- virtual device
			log ("module " & to_string (module_name) &
				" adding device " & to_string (device_model) &
				" at" &
				to_string (position => place) &
				" rotation" & et_coordinates.to_string (rotation),				
				log_threshold);
		end if;
			
		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);

		-- Read the device file and store it in container et_libraries.devices.
		-- If the device is already in et_libraries.devices, nothing happpens.
		et_project.read_device_file (
			file_name		=> device_model, -- ../lbr/logic_ttl/7400.dev
			log_threshold	=> log_threshold + 1);

		-- locate the device in the library
		device_cursor_lib := find (et_libraries.devices, device_model);
		
		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> add'access);

		log_indentation_down;
	end add_device;

	procedure invoke_unit (
	-- Invokes a unit of a device into the schematic.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC1
		unit_name		: in type_unit_name.bounded_string; -- A, B, IO_BANK_2
		place			: in et_coordinates.type_coordinates; -- sheet/x/y
		rotation		: in et_coordinates.type_rotation; -- 90		
		log_threshold	: in type_log_level) is

		use et_coordinates;
		
		module_cursor : type_modules.cursor; -- points to the targeted module

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;
			device_cursor_sch : et_schematic.type_devices.cursor;

			procedure query_units_in_use (
				device_name	: in type_device_name;
				device		: in et_schematic.type_device) is
				use et_schematic.type_units;
			begin
				if contains (device.units, unit_name) then
					log (message_error & 
						 to_string (device_name) &
						 " unit " & to_string (unit_name) &
						 " already in use !", console => true);
					raise constraint_error;
				end if;
			end query_units_in_use;

			device_model : type_device_model_file.bounded_string; -- ../libraries/devices/logic_ttl/7400.dev
			device_cursor_lib : et_libraries.type_devices.cursor;
			unit_cursors : type_unit_cursors_lib;

			use et_libraries.type_units_external;
			use et_libraries.type_units_internal;
			use et_libraries.type_devices;
			
			procedure add_unit_internal (
			-- Add an internal unit to the schematic device.
			-- The unit to be added is accessed by unit_cursors.int.
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
			begin
				log ("adding internal unit " & to_string (key (unit_cursors.int)), log_threshold + 2);
				
				case element (device_cursor_lib).appearance is
					when SCH =>
						type_units.insert (
							container	=> device.units,
							key			=> key (unit_cursors.int), -- the unit name like A, B
							new_item	=> (
								appearance	=> SCH,
								position	=> place, -- the coordinates provided by the calling unit (sheet,x,y)
								rotation	=> rotation, -- the rotation provided by the calling unit								
								others 		=> <>) -- rotation and mirror
								);
						
					when SCH_PCB =>
						type_units.insert (
							container	=> device.units,
							key			=> key (unit_cursors.int), -- the unit name like A, B, VCC_IO_BANK_1
							new_item	=> (
								appearance	=> SCH_PCB,
								position	=> place, -- the coordinates provided by the calling unit (sheet,x,y)
								rotation	=> rotation, -- the rotation provided by the calling unit
								reference	=> element (unit_cursors.int).symbol.reference, -- placeholder for device name
								value		=> element (unit_cursors.int).symbol.value,		-- placeholder for device value
								purpose		=> element (unit_cursors.int).symbol.purpose,	-- placeholder for device purpose
								others 		=> <>)
								);
						
					when others => null; -- CS
				end case;
				
			end add_unit_internal;

			procedure add_unit_external (
			-- Add an external unit to the schematic device.
			-- The unit to be added is accessed by unit_cursors.ext.
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
				use et_libraries.type_symbols;
				symbol_cursor : et_libraries.type_symbols.cursor;
				symbol_file : et_libraries.type_symbol_model_file.bounded_string; -- *.sym
			begin
				log ("adding external unit " & to_string (key (unit_cursors.ext)), log_threshold + 2);
				
				case element (device_cursor_lib).appearance is
					when SCH =>
						type_units.insert (
							container	=> device.units,
							key			=> key (unit_cursors.ext), -- the unit name like A, B
							new_item	=> (
								appearance	=> SCH,
								position	=> place, -- the coordinates provided by the calling unit (sheet,x,y)
								rotation	=> rotation, -- the rotation provided by the calling unit								
								others 		=> <>) -- rotation and mirror
								);
						
					when SCH_PCB =>
						-- The symbol file name is provided by unit_cursors.ext.
						symbol_file := element (unit_cursors.ext).file; -- *.sym
						
						-- Locate the external symbol in container "symbols".
						-- The key into symbols is the file name (*.sym).
						symbol_cursor := et_libraries.type_symbols.find (symbols, symbol_file);

						-- CS: The symbol should be there now. Otherwise symbol_cursor would assume no_element
						-- and constraint_error would arise here:
						
						type_units.insert (
							container	=> device.units,
							key			=> key (unit_cursors.ext), -- the unit name like A, B, VCC_IO_BANK_1
							new_item	=> (
								appearance	=> SCH_PCB,
								position	=> place, -- the coordinates provided by the calling unit (sheet,x,y)
								rotation	=> rotation, -- the rotation provided by the calling unit								
								reference	=> element (symbol_cursor).reference,	-- placeholder for device name
								value		=> element (symbol_cursor).value,		-- placeholder for device value
								purpose		=> element (symbol_cursor).purpose,		-- placeholder for device purpose
								others 		=> <>)
								);

					when others => null; -- CS
				end case;

			end add_unit_external;

			ports : et_libraries.type_ports.map; -- the positions of the unit ports
			
		begin -- query_devices
			if contains (module.devices, device_name) then -- device exists in schematic

				device_cursor_sch := find (module.devices, device_name);
				
				-- Test whether desired unit is already used by the device (in schematic).
				-- Abort if unit already in use.
				query_element (
					position	=> device_cursor_sch,
					process		=> query_units_in_use'access);

				-- Locate the device model in the library:
				device_model := element (device_cursor_sch).model;
				device_cursor_lib := et_libraries.type_devices.find (et_libraries.devices, device_model);

				-- Get cursor to the desired unit in device model.
				-- The unit can be internal or external.
				unit_cursors := any_unit (device_cursor_lib, unit_name);

				-- If the unit is internal, add it to the device in the schematic:
				if unit_cursors.int /= type_units_internal.no_element then

					et_schematic.type_devices.update_element (
						container	=> module.devices,
						position	=> device_cursor_sch,
						process		=> add_unit_internal'access);

					-- fetch ports of unit and their positions relative to the unit origin
					log_indentation_up;				
					log ("fetching relative port positions of internal unit " &
						to_string (key (unit_cursors.int)) & " ...", log_threshold + 1);
					
					ports := ports_of_unit (
						device_cursor	=> device_cursor_sch,
						unit_name		=> key (unit_cursors.int));

				-- Unit is external -> add external unit to device in schematic:
				elsif unit_cursors.ext /= type_units_external.no_element then
					
					et_schematic.type_devices.update_element (
						container	=> module.devices,
						position	=> device_cursor_sch,
						process		=> add_unit_external'access);

					-- fetch ports of unit and their positions relative to the unit origin
					log_indentation_up;
					log ("fetching relative port positions of external unit " &
						to_string (key (unit_cursors.ext)) & " ...", log_threshold + 1);

					ports := ports_of_unit (
						device_cursor	=> device_cursor_sch,
						unit_name		=> key (unit_cursors.ext));
					
				else
					raise constraint_error; -- CS should never happen. function any_unit excludes this case.
				end if;

				-- Calculate the absolute positions of the unit ports. Rotate first if required:
				log ("calculating absolute port positions ...", log_threshold + 1);
				if rotation /= rotation_zero then
					rotate_ports (ports, rotation);
				end if;

				move_ports (ports, place);
				
				-- Insert the new unit ports in the nets (type_module.nets):
				insert_ports (
					module			=> module_cursor,
					device			=> device_name,
					ports			=> ports,
					sheet			=> et_coordinates.sheet (place),
					log_threshold	=> log_threshold + 2);

				log_indentation_down;
			else
				device_not_found (device_name);
			end if;
		end query_devices;
		
	begin -- invoke_unit
		log ("module " & to_string (module_name) &
			" device " & to_string (device_name) &
			" invoking unit " & to_string (unit_name) &
			" at" &
			to_string (position => place) &
			" rotation" & et_coordinates.to_string (rotation),
			log_threshold);

		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_devices'access);
		
		log_indentation_down;		
	end invoke_unit;

	function locate_net (
	-- Yields a cursor to the requested net in the given module. If the net could
	-- not be found, returns no_element.
		module		: in type_modules.cursor;
		net_name	: in et_general.type_net_name.bounded_string)		
		return type_nets.cursor is
		cursor : et_schematic.type_nets.cursor;

		procedure query_nets (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
		begin
			cursor := et_schematic.type_nets.find (module.nets, net_name);
		end query_nets;
		
	begin -- locate_net
		query_element (
			position	=> module,
			process		=> query_nets'access);
		
		return cursor;
	end locate_net;
	
	procedure rename_net (
	-- Renames a net. The scope determines whether to rename a certain strand,
	-- all strands on a certain sheet or on all sheets.
	-- CS If a particular strand on a sheet is to be renamed, the argument "place"
	-- must provide sheet and x/y start position of strand. In the future x/y can be
	-- any point on any segment of the strand. See comment in procedure locate_strand.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name_before	: in et_general.type_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		net_name_after	: in et_general.type_net_name.bounded_string; -- RESET_N, MOTOR_ON_OFF_N	
		scope			: in type_net_scope; -- strand, sheet, everywhere
		place			: in et_coordinates.type_coordinates; -- sheet/x/y
		log_threshold	: in type_log_level) is

		module_cursor : type_modules.cursor; -- points to the module

		use et_schematic.type_nets;
		net_cursor_old : type_nets.cursor; -- points to the old net
		net_cursor_new : type_nets.cursor; -- points to the new net

		procedure create_net (
		-- Creates a new empty net named net_name_after. 
		-- Sets the cursor net_cursor_new to the new net.
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			inserted : boolean;
		begin
			insert (
				container	=> module.nets,
				key			=> net_name_after,

				-- The scope of the net assumes the default value LOCAL.
				-- CS: It could be reasonable to assume the scope of the old net.
				new_item	=> (others => <>),
				
				inserted	=> inserted,
				position	=> net_cursor_new
				);
		end create_net;
		
		procedure rename_everywhere (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is

			-- backup the old net
			net_old	: et_schematic.type_net := element (net_cursor_old);

			procedure copy_net_content (
				net_name	: in et_general.type_net_name.bounded_string;
				net			: in out et_schematic.type_net) is
			begin
				net := net_old;
			end copy_net_content;
			
		begin -- rename_everywhere
			
			-- delete the old net entirely:
			delete (
				container	=> module.nets,
				position	=> net_cursor_old);

			-- copy the old net to the new net:
			update_element (	
				container	=> module.nets,
				position	=> net_cursor_new,
				process		=> copy_net_content'access);
			
		end rename_everywhere;

		procedure rename_on_sheet (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is

			-- temporarily collection of strands
			use et_schematic.type_strands;
			strands_on_sheet : et_schematic.type_strands.list;
			
			procedure collect_strands_of_sheet (
			-- Collects all strands on the targeted sheet in container strands_on_sheet.
			-- Deletes the affected strands from the old net.
				net_name	: in et_general.type_net_name.bounded_string;
				net			: in out et_schematic.type_net) is

				strand_cursor : et_schematic.type_strands.cursor := net.strands.first;
				strand : et_schematic.type_strand;
			begin
				-- Look at the strands that are on the targeted sheet.
				while strand_cursor /= type_strands.no_element loop
					if sheet (element (strand_cursor).position) = sheet (place) then

						-- append strand to temporarily collection of strands on this sheet
						append (strands_on_sheet, element (strand_cursor));

						-- delete strand in old net
						delete (net.strands, strand_cursor);
					end if;
					next (strand_cursor);
				end loop;

			end collect_strands_of_sheet;

			procedure move_strands (
			-- Moves the temporarily collection of strands strands_on_sheet 
			-- to the targeted net.
				net_name	: in et_general.type_net_name.bounded_string;
				net			: in out et_schematic.type_net) is
			begin
				move (target => net.strands, source => strands_on_sheet);
			end;
			
		begin -- rename_on_sheet

			-- collect strands in old net
			update_element (
				container	=> module.nets,
				position	=> net_cursor_old,
				process		=> collect_strands_of_sheet'access);

			-- Issue warning if no strands have been collected. This can result:
			-- - from an attempt to rename on a sheet that does not exist 
			-- - from the fact that the targeted sheet does not contain the targeted net 
			if is_empty (strands_on_sheet) then
				log (message_warning & "no strands have been renamed on sheet" & to_sheet (sheet (place)) &
					 ". Check net name and sheet number !");

				-- A net without strands is useless. So the just created net must be discarded.
				log ("deleting net " & to_string (net_name_after), log_threshold + 1);
				delete (module.nets, net_cursor_new);
				
			else
				-- move strands to new net
				update_element (
					container	=> module.nets,
					position	=> net_cursor_new,
					process		=> move_strands'access);
			end if;

			-- If the old net has no strands anymore, delete it.
			if is_empty (element (net_cursor_old).strands) then
				delete (module.nets, net_cursor_old);
			end if;
			
		end rename_on_sheet;

		procedure rename_strand (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is

			use et_schematic.type_strands;			
			strand_temp : et_schematic.type_strand;
			strand_found : boolean := false;

			procedure locate_strand (
			-- Locates the strand that starts at place and stores it in strand_temp.
				net_name	: in et_general.type_net_name.bounded_string;
				net			: in out et_schematic.type_net) is
				strand_cursor : et_schematic.type_strands.cursor := net.strands.first;
			begin
				-- Find the strand that starts at the given position.
				while strand_cursor /= type_strands.no_element loop
					if element (strand_cursor).position = place then
						-- CS: if place is not exactly the start position of the strand,
						-- search for any other point on the strand instead.

						-- fetch strand from old net
						strand_temp := element (strand_cursor);

						-- delete strand in old net
						delete (net.strands, strand_cursor);

						strand_found := true;
						-- no need for further searching
						exit;
					end if;
					next (strand_cursor);
				end loop;
			end locate_strand;

			procedure move_strand (
			-- Moves strand_temp to the targeted net.
				net_name	: in et_general.type_net_name.bounded_string;
				net			: in out et_schematic.type_net) is
			begin
				append (net.strands, strand_temp);
			end;
			
		begin -- rename_strand

			-- locate the targeted strand and store it in strand_temp:
			update_element (
				container	=> module.nets,
				position	=> net_cursor_old,
				process		=> locate_strand'access);

			if not strand_found then
				log (message_warning & "strand not found at" & to_string (position => place) &
					 ". Check net name and position !");

				-- A net without strands is useless. So the just created net must be discarded.
				log ("deleting net " & to_string (net_name_after), log_threshold + 1);
				delete (module.nets, net_cursor_new);
				
			else -- strand found
				-- move strand_temp to the targeted net
				update_element (
					container	=> module.nets,
					position	=> net_cursor_new,
					process		=> move_strand'access);
			end if;

			-- If the old net has no strands anymore, delete it.
			if is_empty (element (net_cursor_old).strands) then
				delete (module.nets, net_cursor_old);
			end if;
			
		end rename_strand;
					
	begin -- rename_net
		
		log ("module " & to_string (module_name) &
			 " renaming net " & to_string (net_name_before) &
			 " to " & to_string (net_name_after),
			log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- locate the requested nets in the module
		net_cursor_old := locate_net (module_cursor, net_name_before);
		net_cursor_new := locate_net (module_cursor, net_name_after);		

		-- issue error if old net does not exist:
		if net_cursor_old = type_nets.no_element then
			net_not_found (net_name_before);
		end if;

		-- if there is no net named net_name_after, notify operator about a new
		-- net being created. 
		if net_cursor_new = type_nets.no_element then
			log ("creating new net " & to_string (net_name_after), log_threshold + 1);

			update_element (
				container	=> modules,
				position	=> module_cursor,
				process		=> create_net'access);
		end if;
		-- Now net_cursor_new points to the new net.
		
		log_indentation_up;

		-- show where the renaming will be taking place:
		case scope is
			when EVERYWHERE =>
				log ("scope: everywhere -> all strands on all sheets", log_threshold);

				update_element (
					container	=> modules,
					position	=> module_cursor,
					process		=> rename_everywhere'access);

			when SHEET =>
				log ("scope: all strands on sheet" & et_coordinates.to_sheet (sheet (place)), log_threshold);

				update_element (
					container	=> modules,
					position	=> module_cursor,
					process		=> rename_on_sheet'access);

			when STRAND => 
				log ("scope: strand at" & to_string (position => place), log_threshold);

				update_element (
					container	=> modules,
					position	=> module_cursor,
					process		=> rename_strand'access);
				
		end case;
		
		log_indentation_down;		
	end rename_net;

	procedure delete_net (
	-- Deletes a net. The scope determines whether to delete a certain strand,
	-- all strands on a certain sheet or on all sheets.
	-- CS If a particular strand on a sheet is to be deleted, the argument "place"
	-- must provide sheet and x/y start position of strand. In the future x/y can be
	-- any point on any segment of the strand. See comment in procedure locate_strand.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in et_general.type_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		scope			: in type_net_scope; -- strand, sheet, everywhere
		place			: in et_coordinates.type_coordinates; -- sheet/x/y
		log_threshold	: in type_log_level) is

		module_cursor : type_modules.cursor; -- points to the module

		use et_schematic.type_nets;
		net_cursor : type_nets.cursor; -- points to the net

		use et_schematic.type_strands;
		
		procedure delete_everywhere (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
		begin
			delete (
				container	=> module.nets,
				position	=> net_cursor);
		end;

		procedure delete_on_sheet (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is

			procedure delete_strands_of_sheet (
			-- Removes the affected strands from the net.
				net_name	: in et_general.type_net_name.bounded_string;
				net			: in out et_schematic.type_net) is
				strand_cursor : et_schematic.type_strands.cursor := net.strands.first;
				strand_count_before : count_type := length (net.strands);
			begin
				-- Look at the strands that are on the targeted sheet.
				while strand_cursor /= type_strands.no_element loop
					if sheet (element (strand_cursor).position) = sheet (place) then
						delete (net.strands, strand_cursor);
					end if;
					next (strand_cursor);
				end loop;

				-- Issue warning if no strands have been deleted. This can result:
				-- - from an attempt to rename on a sheet that does not exist 
				-- - from the fact that the targeted sheet does not contain the targeted net 
				-- This simple check is a compare of the number of strands before with the
				-- number of strands after the deletion:
				if length (net.strands) = strand_count_before then -- nothing deleted
					log (message_warning & "no strands have been deleted on sheet" & to_sheet (sheet (place)) &
						". Check net name and sheet number !");
				end if;
			end;
			
		begin -- delete_on_sheet

			-- delete strands in net
			update_element (
				container	=> module.nets,
				position	=> net_cursor,
				process		=> delete_strands_of_sheet'access);

			-- If the net has no strands anymore, delete it.
			if is_empty (element (net_cursor).strands) then
				delete (module.nets, net_cursor);
			end if;
			
		end delete_on_sheet;

		procedure delete_strand (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is

			use et_schematic.type_strands;			

			strand_found : boolean := false;

			procedure locate_strand (
			-- Locates the strand that starts at place.
				net_name	: in et_general.type_net_name.bounded_string;
				net			: in out et_schematic.type_net) is
				strand_cursor : et_schematic.type_strands.cursor := net.strands.first;
			begin
				-- Find the strand that starts at the given position.
				while strand_cursor /= type_strands.no_element loop
					if element (strand_cursor).position = place then
						-- CS: if place is not exactly the start position of the strand,
						-- search for any other point on the strand instead.

						-- delete strand in net
						delete (net.strands, strand_cursor);

						strand_found := true;
						-- no need for further searching
						exit;
					end if;
					next (strand_cursor);
				end loop;
			end locate_strand;
		
		begin -- delete_strand

			-- locate the targeted strand
			update_element (
				container	=> module.nets,
				position	=> net_cursor,
				process		=> locate_strand'access);

			if not strand_found then
				log (message_warning & "strand not found at" & to_string (position => place) &
					 ". Check net name and position !");
			end if;

			-- If the net has no strands anymore, delete it.
			if is_empty (element (net_cursor).strands) then
				delete (module.nets, net_cursor);
			end if;
			
		end delete_strand;
					
	begin -- delete_net
		
		log ("module " & to_string (module_name) &
			 " deleting net " & to_string (net_name),
			log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- locate the requested nets in the module
		net_cursor := locate_net (module_cursor, net_name);

		-- issue error if net does not exist:
		if net_cursor = type_nets.no_element then
			net_not_found (net_name);
		end if;

		log_indentation_up;

		-- show where the deletion will be taking place:
		case scope is
			when EVERYWHERE =>
				log ("scope: everywhere -> all strands on all sheets", log_threshold);

				update_element (
					container	=> modules,
					position	=> module_cursor,
					process		=> delete_everywhere'access);

			when SHEET =>
				log ("scope: all strands on sheet" & et_coordinates.to_sheet (sheet (place)), log_threshold);

				update_element (
					container	=> modules,
					position	=> module_cursor,
					process		=> delete_on_sheet'access);

			when STRAND => 
				log ("scope: strand at" & to_string (position => place), log_threshold);

				update_element (
					container	=> modules,
					position	=> module_cursor,
					process		=> delete_strand'access);
				
		end case;
		
		log_indentation_down;		
	end delete_net;

	procedure delete_segment (
	-- Deletes a segment of a net.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in et_general.type_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		place			: in et_coordinates.type_coordinates; -- sheet/x/y
		log_threshold	: in type_log_level) is

		module_cursor : type_modules.cursor; -- points to the module

		use et_schematic.type_nets;
		net_cursor : type_nets.cursor; -- points to the net

		use et_schematic.type_strands;

		procedure no_segment is begin
			log (message_warning & "segment not found at" & to_string (position => place) &
			 ". Check net name and position !");
		end;

		procedure query_net (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is

			procedure query_strands (
			-- Searches the strands of the net for a segment that sits on given place.
				net_name	: in et_general.type_net_name.bounded_string;
				net			: in out et_schematic.type_net) is
				strand_cursor : et_schematic.type_strands.cursor := net.strands.first;
				segment_found, strand_found : boolean := false;

				use type_net_segments;				
				
				procedure query_segments (strand : in out type_strand) is
					segment_cursor : type_net_segments.cursor := strand.segments.first;
				begin
					while segment_cursor /= type_net_segments.no_element loop

						-- If segment crosses the given x/y position (in place),
						-- delete the segment.
						if between_start_and_end_point (
							point	=> type_point (place),
							segment	=> segment_cursor) then

							delete (strand.segments, segment_cursor);

							-- signal the calling unit to abort the search
							segment_found := true;

							-- no further search required
							exit;
						end if;

						next (segment_cursor);
					end loop;

					if not segment_found then
						no_segment;
					end if;
					
				end query_segments;
				
			begin -- query_strands
				
				-- Look at strands that are on the given sheet. This loop ends prematurely
				-- as soon as a segment has been found.
				while not segment_found and strand_cursor /= type_strands.no_element loop
					
					if sheet (element (strand_cursor).position) = sheet (place) then

						-- signal the calling unit that a strand has been found:
						strand_found := true;

						update_element (
							container	=> net.strands,
							position	=> strand_cursor,
							process		=> query_segments'access);

						-- In case no more segments are left in the strand,
						-- remove the now useless strand entirely.
						if is_empty (element (strand_cursor).segments) then
							delete (net.strands, strand_cursor);
							null;
						end if;
						
 					end if;
					next (strand_cursor);
				end loop;

				-- Issue warning if no strand has been found.
				if not strand_found then
					no_segment;
				end if;
				
			end query_strands;
		
		begin -- query_net

			-- query the affected strands
			update_element (
				container	=> module.nets,
				position	=> net_cursor,
				process		=> query_strands'access);

			-- If the net has no strands anymore, delete it entirely because a
			-- net without strands is useless.
			if is_empty (element (net_cursor).strands) then
				delete (module.nets, net_cursor);
			end if;
			
		end query_net;
							
	begin -- delete_segment
		log ("module " & to_string (module_name) &
			 " deleting in net " & to_string (net_name) &
			 " segment at" & to_string (position => place),
			log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- locate the requested nets in the module
		net_cursor := locate_net (module_cursor, net_name);

		-- issue error if net does not exist:
		if net_cursor = type_nets.no_element then
			net_not_found (net_name);
		end if;

		log_indentation_up;

		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_net'access);
		
		log_indentation_down;		
	end delete_segment;

	procedure drag_segment (
	-- Drags a segment of a net.
	-- Place adresses the segment within the schematic. 
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in et_general.type_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		place			: in et_coordinates.type_coordinates; -- sheet/x/y, this addresses the segment
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in et_coordinates.type_point; -- x/y, the new position 
		log_threshold	: in type_log_level) is

		module_cursor : type_modules.cursor; -- points to the module

		use et_schematic.type_nets;
		net_cursor : type_nets.cursor; -- points to the net

		use et_schematic.type_strands;

		procedure no_segment is begin
			log (message_warning & "segment not found at" & to_string (position => place) &
			 ". Check net name and position !");
		end;

		function movable (
			segment	: in type_net_segment;
			zone	: in type_zone) 
			return boolean is

			result : boolean := true; -- to be returned. true means the zone is movable.
			-- Goes false once a port has been found in the given zone.

			point : et_coordinates.type_coordinates;

			procedure search_ports is
			-- Searches ports of devices, netchangers and submodules that sit on
			-- the point of interest.	
			-- On the first finding, sets result to false and finishes. If no 
			-- finding, result remains true.	
				use type_ports_device;
				use type_ports_submodule;
				use type_ports_netchanger;

				device : type_ports_device.cursor := segment.ports_devices.first;
				submodule : type_ports_submodule.cursor := segment.ports_submodules.first;
				netchanger : type_ports_netchanger.cursor := segment.ports_netchangers.first;
			begin -- search_ports
				while device /= type_ports_device.no_element loop

					if position (
						module_name		=> module_name,
						device_name		=> element (device).device_name,
						port_name		=> element (device).port_name,
						log_threshold	=> log_threshold + 2) 
						
						= point then

						result := false; -- not movable
						exit;

					end if;
					
					next (device);
				end loop;

				-- if no device port found, search in submodule ports
				if result = true then

					while submodule /= type_ports_submodule.no_element loop

						if position (
							module_name		=> module_name,
							submod_name		=> element (submodule).module_name,
							port_name		=> element (submodule).port_name,
							log_threshold	=> log_threshold + 2) 
							
							= point then

							result := false; -- not movable
							exit;

						end if;
						
						next (submodule);
					end loop;

				end if;

				-- if no submodule port found, search in netchanger ports
				if result = true then

					while netchanger /= type_ports_netchanger.no_element loop

						if position (
							module_name		=> module_name,
							index			=> element (netchanger).index,
							port			=> element (netchanger).port,
							log_threshold	=> log_threshold + 2) 
							
							= point then

							result := false; -- not movable
							exit;

						end if;
						
						next (netchanger);
					end loop;

				end if;

				-- if no port found, result is still true
			end search_ports;
			
		begin -- movable
			log_indentation_up;
			
			-- The point of interest is on the sheet specified in argument "place".
			-- The x/y coordinates are taken from the segment start or end point.
			
			case zone is
				when START_POINT =>
					point := to_coordinates (
							point => segment.coordinates_start,
							sheet => sheet (place));

					search_ports; -- sets result to false if a port is connected with the start point
					
				when END_POINT =>
					point := to_coordinates (
							point => segment.coordinates_end,
							sheet => sheet (place));

					search_ports; -- sets result to false if a port is connected with the end point
					
				when CENTER =>
					-- Both start and end point must be checked for any ports.
					-- First check the start point of the segment.
					-- If start point is movable, then the end point must be checked too.
					point := to_coordinates (
							point => segment.coordinates_start,
							sheet => sheet (place));

					search_ports; -- sets result to false if a port is connected with the start point

					-- If start point is movable, check end point.
					if result = true then
						point := to_coordinates (
								point => segment.coordinates_end,
								sheet => sheet (place));

						search_ports; -- sets result to false if a port is connected with the end point
					end if;
			end case;

			log_indentation_down;
			
			return result;
		end movable;
		
		procedure query_net (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is

			procedure query_strands (
			-- Searches the strands of the net for a segment that sits on given place.
				net_name	: in et_general.type_net_name.bounded_string;
				net			: in out et_schematic.type_net) is
				strand_cursor : et_schematic.type_strands.cursor := net.strands.first;
				segment_found, strand_found : boolean := false;

				use type_net_segments;				
				
				procedure query_segments (strand : in out type_strand) is
					segment_cursor : type_net_segments.cursor := strand.segments.first;
					segment_cursor_target : type_net_segments.cursor;
					target_segment_before : type_net_segment;
					zone : type_zone;

					procedure move_targeted_segment (segment : in out type_net_segment) is begin
						case zone is
							when START_POINT =>
								case coordinates is
									when ABSOLUTE =>
										segment.coordinates_start := point; -- given position is absolute

									when RELATIVE =>
										move (
											point	=> segment.coordinates_start,
											offset	=> point -- the given position is relative
											);
								end case;
								
							when END_POINT =>
								case coordinates is
									when ABSOLUTE =>
										segment.coordinates_end := point; -- given position is absolute

									when RELATIVE =>
										move (
											point	=> segment.coordinates_end,
											offset	=> point -- the given position is relative
											);
								end case;

							when CENTER =>
								case coordinates is
									when ABSOLUTE =>
										-- CS: currently absolute dragging at the center is not possible.
										log (message_warning & "absolute dragging at center not possible !");

									when RELATIVE =>
										move (
											point	=> segment.coordinates_start,
											offset	=> point -- the given position is relative
											);

										move (
											point	=> segment.coordinates_end,
											offset	=> point -- the given position is relative
											);
										
								end case;
						end case;
					end move_targeted_segment;

					procedure move_connected_segment (connected_segment : in out type_net_segment) is 
					-- This procedure moves the start/end points of segments that are connected
					-- with the target_segment_before.

						procedure copy_start_point is begin
							if connected_segment.coordinates_start = target_segment_before.coordinates_start then
								connected_segment.coordinates_start := element (segment_cursor_target).coordinates_start;
							end if;

							if connected_segment.coordinates_end = target_segment_before.coordinates_start then
								connected_segment.coordinates_end := element (segment_cursor_target).coordinates_start;
							end if;
						end;

						procedure copy_end_point is begin
							if connected_segment.coordinates_start = target_segment_before.coordinates_end then
								connected_segment.coordinates_start := element (segment_cursor_target).coordinates_end;
							end if;

							if connected_segment.coordinates_end = target_segment_before.coordinates_end then
								connected_segment.coordinates_end := element (segment_cursor_target).coordinates_end;
							end if;
						end;
						
					begin -- move_connected_segment
						case zone is
							when START_POINT => 
								-- The segment start or end point moves to the targeted segment start point.
								copy_start_point; 
								
							when END_POINT => 
								-- The segment start or end point moves to the targeted segment end point.
								copy_end_point;
								
							when CENTER => 
								-- The segment start or end point moves to the targeted segment start point.
								copy_start_point; 

								-- The segment start or end point moves to the targeted segment end point.
								copy_end_point;
						end case;
					end move_connected_segment;
					
				begin -- query_segments
					-- MOVE TARGETED SEGMENT
					while segment_cursor /= type_net_segments.no_element loop

						-- If segment crosses the given x/y position (in place) then
						-- the segment has been found:
						if between_start_and_end_point (
							point	=> type_point (place),
							segment	=> segment_cursor) then

							-- Calculate the zone of attack. This is where place is.
							zone := which_zone (
								point	=> type_point (place),
								segment	=> segment_cursor);

							-- depending on zone, drag start point, end point or both
							log ("dragging at " & type_zone'image (zone), log_threshold + 2);

							-- Test whether the zone is movable. If not movable, nothing happens.
							if movable (element (segment_cursor), zone) then

								-- Backup the cursor of the targeted segment.
								-- Backup the segment as it was BEFORE the dragging.
								-- They are required later.
								segment_cursor_target := segment_cursor;
								target_segment_before := element (segment_cursor);

								-- move the targeted segment
								et_schematic.type_net_segments.update_element (
									container	=> strand.segments,
									position	=> segment_cursor,
									process		=> move_targeted_segment'access);
							else
								log (message_warning & "segment is tied to a port. Dragging not possible !");
							end if;

							-- signal the calling unit to abort the search
							segment_found := true;

							-- no further search required
							exit;
						end if;

						next (segment_cursor);
					end loop;

					if not segment_found then no_segment; end if;

					-- MOVE SEGMENTS CONNECTED WITH THE TARGETED SEGMENT. 
					-- Iterate in segments. skip targeted segment because it has been dragged
					-- already (see above).
					segment_cursor := strand.segments.first; -- reset segment cursor
					while segment_cursor /= type_net_segments.no_element loop
						if segment_cursor /= segment_cursor_target then

							et_schematic.type_net_segments.update_element (
								container	=> strand.segments,
								position	=> segment_cursor,
								process		=> move_connected_segment'access);

						end if;

						next (segment_cursor);
					end loop;

					-- update strand position
					set_strand_position (strand);
				end query_segments;
				
			begin -- query_strands
				
				-- Look at strands that are on the given sheet. This loop ends prematurely
				-- as soon as a segment has been found.
				while not segment_found and strand_cursor /= type_strands.no_element loop
					
					if sheet (element (strand_cursor).position) = sheet (place) then

						-- signal the calling unit that a strand has been found:
						strand_found := true;

						update_element (
							container	=> net.strands,
							position	=> strand_cursor,
							process		=> query_segments'access);

 					end if;
					next (strand_cursor);
				end loop;

				-- Issue warning if no strand has been found.
				if not strand_found then
					no_segment;
				end if;
				
			end query_strands;
		
		begin -- query_net

			-- query the affected strands
			update_element (
				container	=> module.nets,
				position	=> net_cursor,
				process		=> query_strands'access);
			
		end query_net;
		
	begin -- drag_segment
		case coordinates is
			when ABSOLUTE =>
				log ("module " & to_string (module_name) &
					" dragging in net " & to_string (net_name) &
					" segment at" & to_string (position => place) &
					" to" & et_coordinates.to_string (point), log_threshold);

			when RELATIVE =>
				log ("module " & to_string (module_name) &
					" dragging in net " & to_string (net_name) &
					" segment at" & to_string (position => place) &
					" by" & et_coordinates.to_string (point), log_threshold);
		end case;
		
		-- locate module
		module_cursor := locate_module (module_name);

		-- locate the requested nets in the module
		net_cursor := locate_net (module_cursor, net_name);

		-- issue error if net does not exist:
		if net_cursor = type_nets.no_element then
			net_not_found (net_name);
		end if;

		log_indentation_up;

		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_net'access);
		
		log_indentation_down;		
	end drag_segment;


	
	procedure check_integrity (
	-- Performs an in depth check on the schematic of the given module.
	-- Tests:
	-- 1. for device/submodule/netchanger port that do not have a same named device/submodule/netchanger.
	-- 2. for device/submodule/netchanger port that occur more than once.
	-- 3. CS: for net junctions sitting on top of each other
	-- 4. CS: for device/submodule/netchanger port that do not have a visual connection to the net
	-- 5. CS: for overlapping net segments
	-- 6. CS: unconnected ports of R, C, L (category depended)
	-- 6.1 CS: unconnected inputs
	-- 7. CS: devices with empty values
	-- 8. CS: interactive devices with empty purpose
	-- 9. CS: check partcode (conventions.validate_partcode)
	-- 10. CS: units sitting on to of each other (same origin position)
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		log_threshold	: in type_log_level) is

		module_cursor : type_modules.cursor; -- points to the module being checked

		errors : natural := 0;
		warnings : natural := 0;

		procedure error is begin errors := errors + 1; end;
		procedure warning is begin warnings := warnings + 1; end;

		procedure query_nets (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use type_nets;

			-- Here we collect all ports of devices (like IC4 CE, R2 1, ...) across all the nets.
			-- Since device_port_collector is an ordered set, an exception will be raised if
			-- a port is to be inserted more than once. Something like IC4 port CE must
			-- occur only ONCE throughout the module.
			use type_ports_device;
			device_port_collector : type_ports_device.set;

			procedure collect_device_port (
				port	: in type_port_device;
				net		: in type_net_name.bounded_string)
			is begin
			-- Collect device ports. exception will be raised of port occurs more than once.
				insert (device_port_collector, port);

				exception when event: others =>
					log (message_error & "net " & to_string (net) &
						" device " & et_libraries.to_string (port.device_name) &
						" port " & et_libraries.to_string (port.port_name) &
						" already used !",
						console => true);
					-- CS: show the net, sheet, xy where the port is in use already

					log (ada.exceptions.exception_message (event), console => true);
			end collect_device_port;

			-- Here we collect all ports of submodules (like MOT_DRV reset) across all the nets.
			-- Since submodule_port_collector is an ordered set, an exception will be raised if
			-- a port is to be inserted more than once. Something like "MOT_DRV reset" must
			-- occur only ONCE throughout the module.
			use type_ports_submodule;
			submodule_port_collector : type_ports_submodule.set;

			procedure collect_submodule_port (
				port	: in type_port_submodule;
				net		: in type_net_name.bounded_string)
			is begin
			-- Collect submodule ports. exception will be raised of port occurs more than once.
				insert (submodule_port_collector, port);

				exception when event: others =>
					log (message_error & "net " & to_string (net) &
						" submodule " & et_general.to_string (port.module_name) &
						" port " & et_general.to_string (port.port_name) &
						" already used !",
						console => true);
					-- CS: show the net, sheet, xy where the port is in use already

					log (ada.exceptions.exception_message (event), console => true);
			end collect_submodule_port;

			-- Here we collect all ports of netchangers (like netchanger port master/slave) across all the nets.
			-- Since netchanger_ports_collector is an ordered set, an exception will be raised if
			-- a port is to be inserted more than once. Something like "netchanger port master" must
			-- occur only ONCE throughout the module.
			use type_ports_netchanger;
			netchanger_ports_collector : type_ports_netchanger.set;

			procedure collect_netchanger_port (
				port	: in type_port_netchanger;
				net		: in type_net_name.bounded_string)
			is begin
			-- Collect netchanger ports. exception will be raised of port occurs more than once.
				insert (netchanger_ports_collector, port);

				exception when event: others =>
					log (message_error & "net " & to_string (net) &
						" netchanger" & submodules.to_string (port.index) &
						" port" & submodules.to_string (port.port) &
						" already used !",
						console => true);
					-- CS: show the net, sheet, xy where the port is in use already

					log (ada.exceptions.exception_message (event), console => true);
			end collect_netchanger_port;
			
			procedure query_net (net_cursor : in type_nets.cursor) is
				use et_general.type_net_name;

				procedure query_strands (
					net_name	: in type_net_name.bounded_string;
					net			: in type_net) is
					use type_strands;

					procedure query_strand (strand_cursor : in type_strands.cursor) is

						procedure query_segments (strand : in type_strand) is
							use type_net_segments;

							procedure query_segment (segment_cursor : in type_net_segments.cursor) is

								procedure query_ports_devices (segment : in type_net_segment) is
									procedure query_port (port_cursor : in type_ports_device.cursor) is begin
										log ("device " & et_libraries.to_string (element (port_cursor).device_name) &
											 " port " & et_libraries.to_string (element (port_cursor).port_name), log_threshold + 4);

										if not exists_device_port (
											module_cursor	=> module_cursor,
											device_name		=> element (port_cursor).device_name,
											port_name		=> element (port_cursor).port_name) then

											error;
											
											log (message_error & "device " & et_libraries.to_string (element (port_cursor).device_name) &
												 " port " & et_libraries.to_string (element (port_cursor).port_name) &
												 " does not exist !");
										end if;

										collect_device_port (port => element (port_cursor), net => net_name);
									end query_port;
										
								begin -- query_ports_devices
									log_indentation_up;
									iterate (segment.ports_devices, query_port'access);
									log_indentation_down;
								end query_ports_devices;

								procedure query_ports_submodules (segment : in type_net_segment) is
									procedure query_port (port_cursor : in type_ports_submodule.cursor) is begin
										log ("submodule " & et_general.to_string (element (port_cursor).module_name) &
											 " port " & et_general.to_string (element (port_cursor).port_name), log_threshold + 4);

										if not exists_submodule_port (
											module_cursor	=> module_cursor,
											submod_instance	=> element (port_cursor).module_name, -- MOT_DRV_3
											port_name		=> element (port_cursor).port_name) then -- RESET

											error;
											
											log (message_error & "submodule " & et_general.to_string (element (port_cursor).module_name) &
												 " port " & et_general.to_string (element (port_cursor).port_name) &
												 " does not exist !");
										end if;

										collect_submodule_port (port => element (port_cursor), net => net_name);
									end query_port;
									
								begin -- query_ports_submodules
									log_indentation_up;
									iterate (segment.ports_submodules, query_port'access);
									log_indentation_down;
								end query_ports_submodules;

								procedure query_ports_netchangers (segment : in type_net_segment) is
									procedure query_port (port_cursor : in type_ports_netchanger.cursor) is begin
										log ("netchanger " & submodules.to_string (element (port_cursor).index) &
											 " port " & submodules.to_string (element (port_cursor).port), log_threshold + 4);

										if not exists_netchanger (
											module_cursor	=> module_cursor,
											index			=> element (port_cursor).index) then -- 1, 2, 3, ...

											error;
											
											log (message_error & "netchanger" & submodules.to_string (element (port_cursor).index) &
												 " does not exist !");
										end if;

										collect_netchanger_port (port => element (port_cursor), net => net_name);
									end query_port;

								begin -- query_ports_netchangers
									log_indentation_up;
									iterate (segment.ports_netchangers, query_port'access);
									log_indentation_down;
								end query_ports_netchangers;
								
							begin -- query_segment
								log (to_string (segment_cursor), log_threshold + 3);

								-- Check ports of devices. Issues error if device and port
								-- not found in module.devices.
								query_element (
									position	=> segment_cursor,
									process		=> query_ports_devices'access);

								-- Check ports of submodules. Issue error if submodule and port
								-- not found in module.submodules
								query_element (
									position	=> segment_cursor,
									process		=> query_ports_submodules'access);

								-- Check netchangers. Issue error if netchanger not
								-- found in module.netchangers.
								query_element (
									position	=> segment_cursor,
									process		=> query_ports_netchangers'access);
								
							end query_segment;
							
						begin -- query_segments
							iterate (strand.segments, query_segment'access);
						end query_segments;

					begin -- query_strand
						log ("strand " & to_string (position => element (strand_cursor).position), log_threshold + 2);
						log_indentation_up;
						
						query_element (
							position	=> strand_cursor,
							process		=> query_segments'access);

						log_indentation_down;
					end query_strand;
					
				begin -- query_strands
					log_indentation_up;
					iterate (net.strands, query_strand'access);
					log_indentation_down;
				end query_strands;

			begin -- query_net
				log ("net " & et_general.to_string (key (net_cursor)), log_threshold + 1);

				query_element (
					position	=> net_cursor,
					process		=> query_strands'access);
				
			end query_net;
			
		begin -- query_nets
			iterate (module.nets, query_net'access);
		end query_nets;

	begin -- check_integrity
		log ("module " & to_string (module_name) & " integrity check ...", log_threshold);
		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);
		
		query_element (
			position	=> module_cursor,
			process		=> query_nets'access);


		if errors > 0 then
			log (message_warning & "integrity check found errors !");
			log ("errors   :" & natural'image (errors));
		end if;

		if warnings > 0 then
			log (message_warning & "integrity check issued warnings !");
			log ("warnings :" & natural'image (warnings));
		end if;

		log_indentation_down;
	end check_integrity;


	
end schematic_ops;
	
-- Soli Deo Gloria
