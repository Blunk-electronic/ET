------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         SCHEMATIC OPERATIONS                             --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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
with ada.strings.unbounded;			use ada.strings.unbounded;
with ada.directories;
with ada.exceptions;				use ada.exceptions;

with et_exceptions;					use et_exceptions;

with et_modes;						use et_modes;
with et_conventions;
with et_pcb_coordinates;
with et_terminals;
with et_packages;
with et_device_rw;

with et_canvas_schematic;

package body et_schematic_ops is

	use pac_generic_modules;
	use et_canvas_schematic.pac_canvas;
	
	procedure device_not_found (name : in type_device_name) is begin
		raise semantic_error_1 
			with "ERROR: Device " & to_string (name) & " not found !";
	end device_not_found;

	procedure device_already_exists (name : in type_device_name) is begin
		raise semantic_error_1
			with "ERROR: Device " & to_string (name) & " already exists !";
	end device_already_exists;

-- 	procedure device_prefix_invalid (name : in type_device_name) is begin
-- 		log (message_warning & "prefix of device name " & to_string (name) & " invalid !");
-- 	end;

	procedure relative_rotation_invalid is begin
		log (ERROR, "Relative rotation must be in range" & 
			to_string (rotation_relative_min) &
			" .." & 
			to_string (rotation_relative_max),
			console => true
			);
		raise constraint_error;
	end;
	
	procedure unit_not_found (name : in pac_unit_name.bounded_string) is begin
		raise semantic_error_1 with
			"ERROR: Unit " & to_string (name) & " not found !";
	end unit_not_found;

	procedure submodule_not_found (name : in et_general.type_module_instance_name.bounded_string) is begin
		log (ERROR, "submodule instance " & enclose_in_quotes (et_general.to_string (name)) &
			 " not found !", console => true);
		raise constraint_error;
	end;

	procedure netchanger_not_found (index : in et_submodules.type_netchanger_id) is begin
		log (ERROR, "netchanger" & et_submodules.to_string (index) & " not found !", console => true);
		raise constraint_error;
	end;

	procedure net_not_found (name : in type_net_name.bounded_string) is begin
		raise semantic_error_1 with
			"ERROR ! Net " & enclose_in_quotes (to_string (name)) & " not found !";
	end;

	procedure submodule_port_not_found (name : in et_general.type_net_name.bounded_string) is begin
		log (ERROR, "port " &
			enclose_in_quotes (to_string (name)) & " not found !", console => true);
		raise constraint_error;
	end;

	procedure assembly_variant_not_found (variant : in et_general.type_variant_name.bounded_string) is 
	begin
		log (ERROR, "assembly variant " &
			 enclose_in_quotes (to_variant (variant)) & " not found !", console => true);
		raise constraint_error;
	end;

	procedure port_not_provided (port_name : in et_general.type_net_name.bounded_string) is begin
		log (ERROR, "submodule does not provide a port named " &
			 enclose_in_quotes (to_string (port_name)) & " with the desired direction (master/slave) !", console => true);
		raise constraint_error;
	end;
	
	procedure dragging_not_possible (
		port 		: in string;
		position	: in et_coordinates.type_position) is
	begin
		log (ERROR, "port " & enclose_in_quotes (port) &
			 " is directly connected with other ports at" &
			to_string (position => position) &
			 ". Dragging not possible !",
			 console => true);
		raise constraint_error;
	end;
	
	procedure set_grid (
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		grid			: in type_grid;
		log_threshold	: in type_log_level) is

		use pac_generic_modules;
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure do_it (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
		begin
			module.grid := grid;
		end;
		
	begin -- set_grid
		log (text => "module " & enclose_in_quotes (to_string (module_name))
			& " setting schematic grid" & to_string (grid),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

	end set_grid;

	procedure set_grid (
		module_cursor	: in pac_generic_modules.cursor;
		grid			: in type_grid;
		log_threshold	: in type_log_level) is

		use pac_generic_modules;

		procedure do_it (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
		begin
			module.grid := grid;
		end;
		
	begin -- set_grid
		log (text => "module " & enclose_in_quotes (to_string (key (module_cursor)))
			& " setting schematic grid" & to_string (grid),
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

	end set_grid;
	
	procedure log_unit_positions (
		positions 		: in type_unit_positions.map;
		log_threshold	: in type_log_level) is
		
		procedure write (cursor : in type_unit_positions.cursor) is begin
			log (text => 
				"unit " &
				to_string (type_unit_positions.key (cursor)) & -- unit name
				et_coordinates.to_string (position => type_unit_positions.element (cursor)), -- sheet x y
				level => log_threshold);
		end;
		
	begin
		log (text => "location(s) in schematic:", level => log_threshold);
		log_indentation_up;
		et_schematic.type_unit_positions.iterate (positions, write'access);
		log_indentation_down;
	end;
	
	procedure log_package_position (
	-- Writes the position of the package in the log file. If device is virtual, nothing happens.
		device_cursor	: in et_schematic.type_devices.cursor;
		log_threshold	: in type_log_level) is
		use et_pcb_coordinates;
		use et_pcb_coordinates.pac_geometry_brd;
		use et_schematic.type_devices;
		use et_symbols;
	begin
		if element (device_cursor).appearance = PCB then
			log (text => "location in board:" & 
				to_string (pac_geometry_brd.type_point (element (device_cursor).position)) &
				" face" & 
				to_string (get_face (element (device_cursor).position)),
				level => log_threshold);
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
		module			: in pac_generic_modules.cursor;		-- the module
		device			: in type_device_name;			-- the device
		ports			: in et_symbols.type_ports.map := et_symbols.type_ports.empty_map; -- the ports (if empty, all ports of the device will be deleted)
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
										use et_symbols;
										port : type_port_device := element (port_cursor); -- take a copy of the port
									begin -- query_port
										if port.device_name = device then -- on match just report the port and skip it

											log_indentation_up;
											
											if dedicated_ports then
												if et_symbols.type_ports.contains (ports, port.port_name) then
													log (text => "delete port " & to_string (port.port_name), level => log_threshold + 3);
												else
													ports_new.insert (port); -- all other ports are collected in ports_new.
												end if;
											else
												log (text => "delete port " & to_string (port.port_name), level => log_threshold + 3);	
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
								log (text => to_string (segment_cursor), level => log_threshold + 2);

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
						log (text => "strand " & to_string (position => element (strand_cursor).position),
							 level => log_threshold + 2);

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
				log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);

				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);
				
			end query_net;				
			
		begin -- query_nets
			type_nets.iterate (module.nets, query_net'access);
		end query_nets;
		
	begin -- delete_ports
		log (text => "deleting device ports in nets ...", level => log_threshold);

		-- If ports are provided, we have to delete exactly those in list "ports".
		-- The flag dedicated_ports is later required in order to do this job:
		if et_symbols.type_ports.length (ports) > 0 then
			dedicated_ports := true;
		end if;
		
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;
	end delete_ports;
	
	procedure delete_device (
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module being modified
		
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
		log (text => "module " & to_string (module_name) &
			 " deleting " & to_string (device_name) & " ...", level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end delete_device;
	
	function ports_of_unit (
	-- Returns a map of ports of the given device and unit.
	-- The coordinates of the ports are default xy-positions relative
	-- to the center of the unit.
		device_cursor	: in et_schematic.type_devices.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return et_symbols.type_ports.map is

		use et_symbols;
		ports : et_symbols.type_ports.map; -- to be returned
		
		model : pac_device_model_file.bounded_string; -- ../libraries/devices/transistor/pnp.dev
		device_cursor_lib : pac_devices_lib.cursor;

		procedure query_internal_units (
			model	: in pac_device_model_file.bounded_string;
			device	: in et_devices.type_device) is
			use pac_units_internal;
			unit_cursor : pac_units_internal.cursor;
		begin -- query_internal_units
			-- locate the given unit among the internal units
			unit_cursor := find (device.units_internal, unit_name);

			-- Fetch the ports of the internal unit.
			-- Transfer the ports to the portlist to be returned:			
			-- CS: constraint_error arises here if unit can not be located.
			ports := element (unit_cursor).symbol.ports;
		end query_internal_units;

		procedure query_external_units (
			model	: in pac_device_model_file.bounded_string;
			device	: in et_devices.type_device) is
			use pac_units_external;
			unit_cursor : pac_units_external.cursor;
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
			if unit_cursor /= pac_units_external.no_element then
				sym_model := element (unit_cursor).model;

				-- Fetch the ports of the external unit.
				-- CS: constraint_error arises here if symbol model could not be located.
				type_symbols.query_element (
					position	=> type_symbols.find (symbols, sym_model),
					process		=> query_symbol'access);
			end if;
			
		end query_external_units;
		
	begin -- ports_of_unit

		-- Fetch the model name of the given device. 
		model := et_schematic.type_devices.element (device_cursor).model;

		-- Get cursor to device in device library (the model name is the key into the device library).
		-- CS: constraint_error will arise here if no associated device exists.
		device_cursor_lib := pac_devices_lib.find (devices, model);

		-- Query external units of device (in library). It is most likely that
		-- the unit is among the external units:
		pac_devices_lib.query_element (
			position	=> device_cursor_lib,
			process		=> query_external_units'access);

		-- If unit could not be found among external units then look up the internal units:
		if et_symbols.type_ports.length (ports) = 0 then

			-- Query internal units of device (in library):
			pac_devices_lib.query_element (
				position	=> device_cursor_lib,
				process		=> query_internal_units'access);
		end if;

		-- If still no ports found, we have a problem:
		if et_symbols.type_ports.length (ports) = 0 then
			raise constraint_error;
		end if;
		
		return ports;

		exception
			when event: others =>
				log_indentation_reset;
				log (text => ada.exceptions.exception_information (event), console => true);
				raise;
		
	end ports_of_unit;

	procedure move_ports (
		ports	: in out et_symbols.type_ports.map; -- the portlist
		offset	: in et_coordinates.type_position) -- the offset (only x/y matters)
	is
		use et_symbols;
		use et_symbols.type_ports;

		procedure move (
			name	: in type_port_name.bounded_string;
			port	: in out type_port) is
		begin
			move_by (port.position, offset);
		end;

		procedure query_port (cursor : in et_symbols.type_ports.cursor) is begin
			update_element (
				container	=> ports,
				position	=> cursor,
				process		=> move'access);
		end;
			
	begin -- move_ports
		iterate (ports, query_port'access);
	end move_ports;
	
	function position (
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC34
		port_name		: in et_symbols.type_port_name.bounded_string; -- CE
		log_threshold	: in type_log_level)
		return et_coordinates.type_position 
	is
		port_position : et_coordinates.type_position; -- to be returned		
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being inquired

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;
			unit_position : et_coordinates.type_position;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in et_schematic.type_device) is
				
				use et_schematic.type_units;
				unit_cursor : type_units.cursor := device.units.first;
				unit_name : pac_unit_name.bounded_string;
				
				use et_symbols.type_ports;
				ports : et_symbols.type_ports.map;
				port_cursor : et_symbols.type_ports.cursor;
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
						port_position := et_coordinates.to_position (
									sheet	=> sheet (unit_position), -- the sheet where the unit is
									point	=> element (port_cursor).position -- default xy pos of port
									);														 

						-- Calculate the absolute port position in schematic by
						-- first rotating port_xy, and then moving port_xy:
						
						rotate_by (
							point		=> port_position,
							rotation	=> rot (element (unit_cursor).position));
						
						-- CS mirror ?
						
						-- Calculate the absolute port position in the schematic:
						move_by (
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

		use et_symbols;
		
	begin -- position
		log (text => "module " & to_string (module_name) &
			 " locating device " & to_string (device_name) & 
			 " port " & to_string (port_name) & " ...", level => log_threshold);

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
		return et_coordinates.type_position
	is
		port_position : et_coordinates.type_position; -- to be returned		
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being inquired

		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use et_submodules;			
			use type_submodules;
			submod_cursor : type_submodules.cursor;
			submod_position : et_coordinates.type_position;

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
					move_by (
						point	=> port_xy,
						offset	=> submod_position);

					-- Now port_xy holds the absolute x/y of the port in the schematic.

					-- Assemble the port_position to be returned:
					port_position := to_position (
						point	=> port_xy,
						sheet	=> sheet (submod_position)
						);

				else
					log (ERROR, "port " & et_general.to_string (port_name) & " not found !",
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
		log (text => "module " & to_string (module_name) &
			 " locating submodule " & to_string (submod_name) & 
			 " port " & to_string (port_name) & " ...", level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		query_element (
			position	=> module_cursor,
			process		=> query_submodules'access);
		
		return port_position;
	end position;

	function position (
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		port			: in et_submodules.type_netchanger_port_name; -- SLAVE/MASTER
		log_threshold	: in type_log_level)
		return et_coordinates.type_position
	is
		use et_submodules;
		port_position : et_coordinates.type_position; -- to be returned		
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being inquired

		procedure query_netchangers (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use type_netchangers;
			nc_cursor : type_netchangers.cursor;
			nc_position : et_coordinates.type_position;
			port_xy : type_point;
		begin -- query_netchangers
			if contains (module.netchangers, index) then
				nc_cursor := find (module.netchangers, index); -- the netchanger should be there

				log_indentation_up;

				-- get netchanger position (sheet/x/y) and rotation in schematic
				nc_position := element (nc_cursor).position_sch;

				-- get the port position relative to the center of the netchanger
				case port is
					when MASTER =>
						port_xy := position_master_port_default;

					when SLAVE =>
						port_xy := position_slave_port_default;
				end case;

				-- Calculate the absolute port position in schematic by
				-- first rotating port_xy, and then moving port_xy:
				
				rotate_by (
					point		=> port_xy,
					rotation	=> rot (nc_position));
				
				move_by (
					point	=> port_xy,
					offset	=> nc_position);

				-- Now port_xy holds the absolute x/y of the port in the schematic.

				-- Assemble the port_position to be returned:
				port_position := to_position (
					point	=> port_xy,
					sheet	=> sheet (nc_position)
					);
				
				log_indentation_down;				
			else
				netchanger_not_found (index);
			end if;
		end query_netchangers;
		
	begin -- position
		log (text => "module " & to_string (module_name) &
			 " locating netchanger " & to_string (index) & 
			 " port " &  to_string (port) & " ...", level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		query_element (
			position	=> module_cursor,
			process		=> query_netchangers'access);
		
		return port_position;
	end position;
	
-- 	procedure delete_unit (
-- 		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
-- 		device_name		: in type_device_name; -- IC45
-- 		unit_name		: in pac_unit_name.bounded_string; -- A
-- 		log_threshold	: in type_log_level) is
-- 
-- 		module_cursor : pac_generic_modules.cursor; -- points to the module being modified
-- 		
-- 		procedure query_devices (
-- 			module_name	: in type_module_name.bounded_string;
-- 			module		: in out type_module) is
-- 			use et_schematic.type_devices;
-- 			device_cursor : et_schematic.type_devices.cursor;
-- 
-- 			-- temporarily storage of unit coordinates.
-- 			-- There will be only one unit in this container.
-- 			position_of_unit : type_unit_positions.map;
-- 
-- 			ports : et_symbols.type_ports.map;
-- 
-- 			procedure query_units (
-- 				device_name	: in type_device_name;
-- 				device		: in out et_schematic.type_device) is
-- 				use et_schematic.type_units;
-- 				unit_cursor : et_schematic.type_units.cursor;
-- 			begin
-- 				if contains (device.units, unit_name) then
-- 					-- locate unit by its name
-- 					unit_cursor := find (device.units, unit_name);
-- 
-- 					-- Load the single unit position and insert in container "position_of_unit"
-- 					type_unit_positions.insert (
-- 						container	=> position_of_unit, 
-- 						key			=> unit_name,
-- 						new_item	=> element (unit_cursor).position);
-- 
-- 					log_unit_positions (position_of_unit, log_threshold + 1);
-- 					
-- 					-- delete the unit
-- 					delete (device.units, unit_name);
-- 				else
-- 					unit_not_found (unit_name);
-- 				end if;
-- 			end query_units;
-- 			
-- 			units_invoked : boolean := true; -- goes false if no unit used anymore
-- 
-- 			procedure query_number_of_invoked_units (
-- 				device_name	: in type_device_name;
-- 				device		: in et_schematic.type_device) is
-- 				use et_schematic.type_units;
-- 			begin
-- 				if length (device.units) = 0 then
-- 					units_invoked := false;
-- 				end if;
-- 			end query_number_of_invoked_units;
-- 
-- 		begin -- query_devices
-- 			if contains (module.devices, device_name) then
-- 
-- 				-- Before the actual deletion, the coordinates of the
-- 				-- unit must be fetched. These coordinates will later assist
-- 				-- in deleting the port names from connected net segments.
-- 				device_cursor := find (module.devices, device_name); -- the device should be there
-- 
-- 				-- locate the unit, load position and then delete the targeted unit
-- 				update_element (
-- 					container	=> module.devices,
-- 					position	=> device_cursor,
-- 					process		=> query_units'access);
-- 				
-- 				log_indentation_up;
-- 
-- 				-- Fetch the ports of the unit to be deleted.
-- 				ports := ports_of_unit (device_cursor, unit_name);
-- 				
-- 				-- Delete the ports of the targeted unit from module.nets
-- 				delete_ports (
-- 					module			=> module_cursor,
-- 					device			=> device_name,
-- 					ports			=> ports,
-- 					sheets			=> position_of_unit, -- there is only one unit -> only one sheet to look at
-- 					log_threshold	=> log_threshold + 1);
-- 
-- 				-- In case no more units are invoked then the device must be
-- 				-- deleted entirely from module.devices.
-- 				-- First we query the number of still invoked units. If none invoked,
-- 				-- the flag units_invoked goes false.
-- 				query_element (
-- 					position	=> device_cursor,
-- 					process		=> query_number_of_invoked_units'access);
-- 
-- 				if not units_invoked then
-- 					delete (module.devices, device_cursor);
-- 				end if;
-- 				
-- 				log_indentation_down;				
-- 			else
-- 				device_not_found (device_name);
-- 			end if;
-- 		end query_devices;
-- 		
-- 	begin -- delete_unit
-- 		log (text => "module " & to_string (module_name) &
-- 			 " deleting " & to_string (device_name) & " unit " & 
-- 			 to_string (unit_name) & " ...", level => log_threshold);
-- 
-- 		-- locate module
-- 		module_cursor := locate_module (module_name);
-- 		
-- 		update_element (
-- 			container	=> generic_modules,
-- 			position	=> module_cursor,
-- 			process		=> query_devices'access);
-- 
-- 	end delete_unit;

-- 	function to_string (coordinates : in type_coordinates) return string is begin
-- 		return latin_1.space & to_lower (type_coordinates'image (coordinates));
-- 	end;
-- 
-- 	function to_coordinates (coordinates : in string) return type_coordinates is begin
-- 		return type_coordinates'value (coordinates);
-- 
-- 		exception
-- 			when event: others =>
-- 				log (text => ada.exceptions.exception_information (event), console => true);
-- 				raise;
-- 	end;

	procedure insert_ports (
		module			: in pac_generic_modules.cursor;		-- the module
		device			: in type_device_name;					-- the device
		unit			: in pac_unit_name.bounded_string;	-- the unit name like A, C, PWR
		ports			: in et_symbols.type_ports.map; -- the ports to be inserted
		sheet			: in type_sheet;				-- the sheet to look at
		log_threshold	: in type_log_level)
	is

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

					use et_symbols;
					use et_symbols.type_ports;
					port_cursor : et_symbols.type_ports.cursor := ports.first;

					port_processed : boolean;
					
					procedure query_segments (strand : in out type_strand) is
						use type_net_segments;
						segment_cursor : type_net_segments.cursor := strand.segments.first;

						procedure change_segment (segment : in out type_net_segment) is
						begin -- change_segment
							-- If port sits on start OR end point of segment AND if it
							-- is not already in the segment then append it to segment.ports_devices.
							-- append it to the portlist of the segment.
							if 	segment.start_point = element (port_cursor).position or
								segment.end_point = element (port_cursor).position then

								-- If port not already in segment, append it.
								-- Otherwise it must not be appended again. constraint_error would arise.
								if type_ports_device.contains (
									container	=> segment.ports_devices,
									item		=> (
											device_name	=> device,
											unit_name	=> unit, -- A, C, PWR
											port_name	=> key (port_cursor)) -- IC23, VCC_IO
									) then 

									log (text => " already there -> skipped", level => log_threshold + 3);
								else
									type_ports_device.insert (
										container	=> segment.ports_devices,
										new_item	=> (
											device_name	=> device,
											unit_name	=> unit, -- A, C, PWR
											port_name	=> key (port_cursor)) -- IC23, VCC_IO
											);

									log (text => " sits on segment -> inserted", level => log_threshold + 3);
								end if;

								-- signal iterations in upper levels to cancel
								port_processed := true;
							end if;
								
						end change_segment;

					begin -- query_segments
						-- On the first segment, where the port sits on, this loop ends prematurely.
						while not port_processed and segment_cursor /= type_net_segments.no_element loop

							log_indentation_up;
							log (text => "probing " & to_string (segment_cursor), level => log_threshold + 2);
							
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
					while port_cursor /= et_symbols.type_ports.no_element loop
						-- CS: If the current net is not on the targeted sheet then this log message
						-- is issued many times without providing any useful information. Rework required:
						log (text => "probing port " & to_string (key (port_cursor))
							& " at" & to_string (element (port_cursor).position), level => log_threshold + 1);
						log_indentation_up;
						
						-- If the current port sits on a strand, this flag will go true. Other 
						-- strands will then not be looked at because the port can only sit on 
						-- one strand.
						port_processed := false;
						
						strand_cursor := net.strands.first;
						while strand_cursor /= type_strands.no_element loop

							-- We pick out only the strands on the targeted sheet:
							if et_coordinates.sheet (element (strand_cursor).position) = sheet then
								log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);

								log_indentation_up;
								log (text => "strand " & to_string (position => element (strand_cursor).position),
									level => log_threshold + 1);

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
		log (text => "inserting device ports in nets on sheet" & 
			 to_sheet (sheet) & " ...", level => log_threshold);
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;
	end insert_ports;
	

	procedure move_unit_placeholder (
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_point; -- x/y
		meaning			: in et_symbols.type_placeholder_meaning; -- name, value, purpose
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use et_symbols;
		
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
					unit_name	: in pac_unit_name.bounded_string;
					unit		: in out et_schematic.type_unit) is
					use et_coordinates;

					-- In case absolute movement is required, calculate the
					-- new position of the placeholder relative to the unit origin:
					pos_abs : constant type_point := 
						type_point (distance_relative (type_point (unit.position), point));
						
				begin -- move_placeholder
					
					-- The given meaning determines the placeholder to be moved:
					case meaning is
						when NAME =>
							case coordinates is
								when ABSOLUTE =>
									--log (text => "pos " & to_string (point));
									unit.name.position := pos_abs;

								when RELATIVE =>
									move_by (
										point	=> unit.name.position,
										offset	=> point);
							end case;
							
						when VALUE =>
							case coordinates is
								when ABSOLUTE =>
									unit.value.position := pos_abs;

								when RELATIVE =>
									move_by (
										point	=> unit.value.position,
										offset	=> point);
							end case;
							
						when PURPOSE =>
							case coordinates is
								when ABSOLUTE =>
									unit.purpose.position := pos_abs;

								when RELATIVE =>
									move_by (
										point	=> unit.purpose.position,
										offset	=> point);
							end case;

						when others =>
							raise constraint_error; -- CS no longer required
					end case;
					
					exception
						when event: others =>
							log (ERROR, "coordinates invalid !", console => true); -- CS required more details
							log (text => ada.exceptions.exception_information (event), console => true);
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
				log (text => "module " & enclose_in_quotes (to_string (module_name))
					& " moving " & to_string (device_name) 
					& " unit " & to_string (unit_name) 
					& " placeholder " & enclose_in_quotes (to_string (meaning))
					& " to" & to_string (point),
					level => log_threshold);

			when RELATIVE =>
				log (text => "module " & enclose_in_quotes (to_string (module_name))
					& " moving " & to_string (device_name) 
					& " unit " & to_string (unit_name) 
					& " placeholder " & enclose_in_quotes (to_string (meaning))
					& " by" & to_string (point),
					level => log_threshold);
		end case;

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);
		
	end move_unit_placeholder;

	procedure rotate_ports (
		ports	: in out et_symbols.type_ports.map; -- the portlist
		angle	: in et_coordinates.type_rotation) is -- 90

		use et_symbols;
		use et_symbols.type_ports;

		procedure rotate (
			name	: in type_port_name.bounded_string;
			port	: in out type_port) is
		begin
			rotate_by (port.position, angle);
		end;

		procedure query_port (cursor : in et_symbols.type_ports.cursor) is begin
			update_element (
				container	=> ports,
				position	=> cursor,
				process		=> rotate'access);
		end;
			
	begin -- rotate_ports
		iterate (ports, query_port'access);
	end rotate_ports;

	function default_text_positions (
	-- Returns the default positions of placeholders and texts of a unit
	-- as they are defined in the symbol model.
		device_cursor	: in et_schematic.type_devices.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return et_symbols.type_default_text_positions is
		
		use et_symbols;
		use et_schematic.type_devices;

		-- The positions to be returned depend on the appearance of the requested device:
		result : type_default_text_positions (element (device_cursor).appearance); -- to be returned
		
		model : pac_device_model_file.bounded_string; -- ../libraries/devices/transistor/pnp.dev
		device_cursor_lib : pac_devices_lib.cursor;
		
		use et_symbols.type_texts;

		procedure query_text (c : in et_symbols.type_texts.cursor) is 
		-- Appends a text position (x/y) the the result.
			use pac_text_positions;
		begin
			append (result.texts, element (c).position);
		end;

		-- Indicates whether the unit is internal or external:
		unit_status : type_unit_ext_int := EXT;
		
		procedure query_internal_units (
			model	: in pac_device_model_file.bounded_string;
			device	: in et_devices.type_device) is
			use pac_units_internal;
			unit_cursor : pac_units_internal.cursor;
		begin
			-- locate the given unit among the internal units
			unit_cursor := find (device.units_internal, unit_name);

			-- if the unit exists among the internal units:
			if unit_cursor /= pac_units_internal.no_element then
				unit_status := INT;
				
				-- Collect the positions of texts and store them in result.text
				-- in the same order as they are listed in symbol.texts:
				iterate (element (unit_cursor).symbol.texts, query_text'access);
				-- CS: constraint_error arises here if unit can not be located.

				-- If it is about a real device, take a copy of the default 
				-- placeholders as they are specified in the symbol model:
				case result.appearance is
					when PCB =>
						result.name 	:= element (unit_cursor).symbol.name;
						result.value	:= element (unit_cursor).symbol.value;
						result.purpose	:= element (unit_cursor).symbol.purpose;
					when others => null;
				end case;

			else
				unit_status := EXT;
			end if;
		end query_internal_units;

		procedure query_external_units (
			model	: in pac_device_model_file.bounded_string;
			device	: in et_devices.type_device) is
			use pac_units_external;
			unit_cursor : pac_units_external.cursor;
			sym_model : type_symbol_model_file.bounded_string; -- like /libraries/symbols/NAND.sym

			procedure query_symbol (
				symbol_name	: in type_symbol_model_file.bounded_string;
				symbol		: in type_symbol ) is
			begin
				-- Collect the positions of texts and store them in result.text
				-- in the same order as they are listed in symbol.texts:
				iterate (symbol.texts, query_text'access);

				-- If it is about a real device, take a copy of the default 
				-- placeholders as they are specified in the symbol model:
				case result.appearance is
					when PCB =>
						result.name 	:= symbol.name;
						result.value	:= symbol.value;
						result.purpose	:= symbol.purpose;
					when others => null;
				end case;
			end query_symbol;
			
		begin -- query_external_units
			-- locate the given unit among the external units
			unit_cursor := find (device.units_external, unit_name);

			-- Fetch the symbol model file of the external unit.
			-- If unit could not be located then it must be internal.
			if unit_cursor /= pac_units_external.no_element then
				unit_status := EXT;
				
				sym_model := element (unit_cursor).model;

				-- Fetch the ports of the external unit.
				-- CS: constraint_error arises here if symbol model could not be located.
				type_symbols.query_element (
					position	=> type_symbols.find (symbols, sym_model),
					process		=> query_symbol'access);
			else
				unit_status := INT;
			end if;
			
		end query_external_units;
		
	begin -- default_text_positions

		-- Fetch the model name of the given device. 
		model := et_schematic.type_devices.element (device_cursor).model;

		-- Get cursor to device in device library (the model name is the key into the device library).
		-- CS: constraint_error will arise here if no associated device exists.
		device_cursor_lib := pac_devices_lib.find (devices, model);

		-- Query external units of device (in library). It is most likely that
		-- the unit is among the external units:
		pac_devices_lib.query_element (
			position	=> device_cursor_lib,
			process		=> query_external_units'access);

		-- If unit could not be found among external units then look up the internal units:
		if unit_status = INT then

			-- Query internal units of device (in library):
			pac_devices_lib.query_element (
				position	=> device_cursor_lib,
				process		=> query_internal_units'access);
		end if;
		
		-- CS raise error if unit could not be located at all.
			
		return result;

		exception
			when event: others =>
				log_indentation_reset;
				log (text => ada.exceptions.exception_information (event), console => true);
				raise;
		
	end default_text_positions;

	

	
-- 	procedure rotate_unit (
-- 		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
-- 		device_name		: in type_device_name; -- IC45
-- 		unit_name		: in pac_unit_name.bounded_string; -- A
-- 		coordinates		: in type_coordinates; -- relative/absolute
-- 		rotation		: in et_coordinates.type_rotation; -- 90
-- 		log_threshold	: in type_log_level) is separate;	

	procedure rotate_unit_placeholder (
	-- Rotates the given unit placeholder around its origin.
	-- The rotation is absolute.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		rotation		: in et_text.type_rotation_documentation; -- absolute ! -- 90
		meaning			: in et_symbols.type_placeholder_meaning; -- name, value, purpose		
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use et_symbols;
		
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
					name	: in pac_unit_name.bounded_string; -- A
					unit	: in out type_unit) is
				begin
					case meaning is
						when et_symbols.NAME =>
							unit.name.rotation := rotation;
							
						when VALUE =>
							unit.value.rotation := rotation;
							
						when PURPOSE =>
							unit.purpose.rotation := rotation;

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
		log (text => "module " & to_string (module_name) &
			" rotating " & to_string (device_name) & " unit " &
			to_string (unit_name) & " placeholder" & to_string (meaning) & " to" &
			et_schematic.pac_text.to_string (rotation), level => log_threshold);
		
		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end rotate_unit_placeholder;

	function locate_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in type_net_name.bounded_string)		
		return et_schematic.type_nets.cursor is
		
		cursor : et_schematic.type_nets.cursor;

		procedure query_nets (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
		begin
			cursor := et_schematic.type_nets.find (module.nets, net_name);
		end query_nets;
		
	begin -- locate_net
		query_element (
			position	=> module_cursor,
			process		=> query_nets'access);
		
		return cursor;
	end locate_net;
	
-- 	procedure drag_net_segments (
-- 	-- Drags the net segments according to the given drag_list of a unit.
-- 	-- Changes the position of start or end points of segments.
-- 	-- Does NOT create new connections with segments if a port
-- 	-- lands on the start or end point of another segment.
-- 	-- Does NOT create a new connection with a segments if a port
-- 	-- lands between start and end point.
-- 		module			: in pac_generic_modules.cursor;		-- the module
-- 		drag_list		: in type_drags_of_ports.map;	-- the old and new port positions
-- 		sheet			: in type_sheet;				-- the sheet to look at
-- 		log_threshold	: in type_log_level) is
-- 
-- 		procedure query_nets (
-- 			module_name	: in type_module_name.bounded_string;
-- 			module		: in out type_module) is
-- 
-- 			procedure query_net (net_cursor : in type_nets.cursor) is
-- 				use type_nets;
-- 				use et_symbols;
-- 
-- 				procedure query_strands (
-- 					net_name	: in type_net_name.bounded_string;
-- 					net			: in out type_net) is
-- 					use et_coordinates;
-- 					
-- 					use type_strands;
-- 					strand_cursor : type_strands.cursor;
-- 					
-- 					use type_drags_of_ports;
-- 					drag_cursor : type_drags_of_ports.cursor := drag_list.first;
-- 
-- 					drag_processed : boolean;
-- 					
-- 					procedure query_segments (strand : in out type_strand) is
-- 						use type_net_segments;
-- 
-- 						procedure query_segment (segment_cursor : in type_net_segments.cursor) is 
-- 
-- 							procedure change_segment (segment : in out type_net_segment) is 
-- 							-- Changes the position of start or end point of a segment according to the drag point.
-- 							begin -- change_segment
-- 								
-- 								-- if port sits on a start point of a segment -> move start point
-- 								if segment.start_point = element (drag_cursor).before then
-- 									log (text => "move segment start point from" & 
-- 										to_string (segment.start_point),
-- 										level => log_threshold + 3);
-- 
-- 									segment.start_point := element (drag_cursor).after;
-- 
-- 									log (text => "to" & 
-- 										to_string (segment.start_point),
-- 										level => log_threshold + 3);
-- 
-- 									drag_processed := true;
-- 								end if;
-- 
-- 								-- if port sits on an end point of a segment -> move end point
-- 								if segment.end_point = element (drag_cursor).before then
-- 									log (text => "move segment end point from" & 
-- 										to_string (segment.end_point),
-- 										level => log_threshold + 3);
-- 
-- 									segment.end_point := element (drag_cursor).after;
-- 
-- 									log (text => "to" & 
-- 										to_string (segment.end_point),
-- 										level => log_threshold + 3);
-- 
-- 									drag_processed := true;
-- 								end if;
-- 
-- 							end change_segment;
-- 
-- 						begin -- query_segment
-- 							log_indentation_up;
-- 							log (text => "probing " & to_string (segment_cursor), level => log_threshold + 2);
-- 							log_indentation_up;
-- 							
-- 							update_element (
-- 								container	=> strand.segments,
-- 								position	=> segment_cursor,
-- 								process		=> change_segment'access);
-- 											
-- 							log_indentation_down;
-- 							log_indentation_down;
-- 						end query_segment;
-- 						
-- 					begin -- query_segments
-- 						iterate (strand.segments, query_segment'access);
-- 
-- 						-- Update strand position if any movement took place.
-- 						if drag_processed then
-- 							set_strand_position (strand); 
-- 						end if;
-- 						
-- 					end query_segments;
-- 						
-- 				begin -- query_strands
-- 					-- loop in drag list
-- 					while drag_cursor /= type_drags_of_ports.no_element loop
-- 						log (text => "probing port " & to_string (key (drag_cursor)), level => log_threshold + 1);
-- 						log_indentation_up;
-- 
-- 						-- If the current drag point sits on a strand, this flag will go true. Other 
-- 						-- strands will then not be looked at because the point can only sit on 
-- 						-- one strand.
-- 						drag_processed := false;
-- 						
-- 						strand_cursor := net.strands.first;
-- 						while strand_cursor /= type_strands.no_element loop
-- 							
-- 							-- We pick out only the strands on the targeted sheet:
-- 							if et_coordinates.sheet (element (strand_cursor).position) = sheet then
-- 								log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);
-- 
-- 								log_indentation_up;
-- 								log (text => "strand " & to_string (position => element (strand_cursor).position),
-- 									level => log_threshold + 1);
-- 							
-- 								-- Iterate in segments of strand. If drag point sits on any segment
-- 								-- the flag drag_processed goes true.
-- 								update_element (
-- 									container	=> net.strands,
-- 									position	=> strand_cursor,
-- 									process		=> query_segments'access);
-- 							
-- 								log_indentation_down;
-- 							end if;
-- 
-- 							-- If the drag point has been processed, there is no need to look up
-- 							-- other strands for this port.
-- 							if drag_processed then exit; end if;
-- 							
-- 							next (strand_cursor);
-- 						end loop;
-- 
-- 						log_indentation_down;
-- 						next (drag_cursor);
-- 					end loop;
-- 						
-- 				end query_strands;
-- 				
-- 			begin -- query_net
-- 				update_element (
-- 					container	=> module.nets,
-- 					position	=> net_cursor,
-- 					process		=> query_strands'access);
-- 			end query_net;				
-- 			
-- 		begin -- query_nets
-- 			type_nets.iterate (module.nets, query_net'access);
-- 		end query_nets;
-- 
-- 	begin -- drag_net_segments
-- 		log (text => "dragging net segments with units on sheet" & 
-- 			 to_sheet (sheet) & " ...", level => log_threshold);
-- 		log_indentation_up;
-- 
-- 		update_element (
-- 			container	=> generic_modules,
-- 			position	=> module,
-- 			process		=> query_nets'access);
-- 
-- 		log_indentation_down;
-- 	end drag_net_segments;

	function net_segment_at_place (
	-- Returns true if at given place a net segment starts or ends.
		module_cursor	: in pac_generic_modules.cursor;
		place			: in et_coordinates.type_position)
		return boolean is

		-- This flag goes true once a segment has been found.
		segment_found : boolean := false; -- to be returned
		
		procedure query_nets (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is

			use type_nets;			
			net_cursor : type_nets.cursor := module.nets.first;

			procedure query_strands (
				net_name	: in type_net_name.bounded_string;
				net			: in type_net) is
				use et_coordinates;
				use type_strands;
				strand_cursor : type_strands.cursor := net.strands.first;
				
				procedure query_segments (strand : in type_strand) is
					use type_net_segments;

					segment_cursor : type_net_segments.cursor := strand.segments.first;

					procedure probe_segment (segment : in type_net_segment) is begin
						-- if place is a start point of a segment
						if segment.start_point = type_point (place) then
							-- signal iterations in upper level to cancel
							segment_found := true;
						end if;

						-- if place is an end point of a segment
						if segment.end_point = type_point (place) then
							-- signal iterations in upper level to cancel
							segment_found := true;
						end if;
					end probe_segment;
					
				begin -- query_segments
					while not segment_found and segment_cursor /= type_net_segments.no_element loop
						query_element (
							position	=> segment_cursor,
							process		=> probe_segment'access);
						
						next (segment_cursor);
					end loop;
				end query_segments;
				
			begin -- query_strands
				while not segment_found and strand_cursor /= type_strands.no_element loop
					
					-- We pick out only the strands on the targeted sheet:
					if et_coordinates.sheet (element (strand_cursor).position) = sheet (place) then

						query_element (
							position	=> strand_cursor,
							process		=> query_segments'access);
					
					end if;
					
					next (strand_cursor);
				end loop;
			end query_strands;
			
		begin -- query_nets
			while not segment_found and net_cursor /= type_nets.no_element loop

				query_element (
					position	=> net_cursor,
					process		=> query_strands'access);

				next (net_cursor);
			end loop;
		end query_nets;

	begin -- net_segment_at_place

		query_element (
			position	=> module_cursor,
			process		=> query_nets'access);
		
		return segment_found;
	end net_segment_at_place;

	function ports_at_place (
	-- Returns lists of device, netchanger and submodule ports at the given place.
		module_name		: in type_module_name.bounded_string;
		place			: in et_coordinates.type_position;
		log_threshold	: in type_log_level)
		return type_ports is
		ports : type_ports; -- to be returned

		module_cursor : pac_generic_modules.cursor; -- points to the module

		procedure query_module (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use et_schematic.type_devices;
			use et_submodules.type_submodules;
			use et_submodules.type_netchangers;			
			
			procedure query_devices (device_cursor : in et_schematic.type_devices.cursor) is

				procedure query_units (unit_cursor : in et_schematic.type_units.cursor) is
					use et_schematic.type_units;
					use pac_unit_name;
					unit_position : et_coordinates.type_position;
					ports : et_symbols.type_ports.map;

					procedure query_port (port_cursor : in et_symbols.type_ports.cursor) is
						use et_symbols;
						use et_symbols.type_ports;
					begin
						log (text => "unit " & et_devices.to_string (key (unit_cursor)) &
							 " port " & to_string (key (port_cursor)) &
							 " at" & to_string (element (port_cursor).position),
							 level => log_threshold + 2);

						-- If the port sits at x/y of place then we have a match:
						if element (port_cursor).position = type_point (place) then
							log (text => " match", level => log_threshold + 2);
							
							-- Insert the port in the portlist to be returned:
							et_schematic.type_ports_device.insert 
								(
								container	=> ports_at_place.ports.devices,
								new_item	=> 
									(
									device_name => key (device_cursor),
									unit_name	=> key (unit_cursor),
									port_name	=> key (port_cursor)
									)
								);

						end if;
					end query_port;
					
				begin -- query_units
					unit_position := element (unit_cursor).position;

					-- Look at units on the given sheet of place:
					if sheet (unit_position) = sheet (place) then
						log (text => "device " & to_string (key (device_cursor)) & " unit " &
							 pac_unit_name.to_string (key (unit_cursor)), level => log_threshold + 1);
						log_indentation_up;

						ports := ports_of_unit (
							device_cursor	=> device_cursor,
							unit_name		=> key (unit_cursor));

						-- CS mirror before rotate or after rotate ?
						--rotate_ports (ports, unit_rotation);
						rotate_ports (ports, rot (unit_position));

						move_ports (ports, unit_position);

						et_symbols.type_ports.iterate (ports, query_port'access);

						log_indentation_down;
					end if;

				end query_units;
				
			begin -- query_devices
				--log (text => "device " & to_string (key (device_cursor)), level => log_threshold + 1);
				--log_indentation_up;

				et_schematic.type_units.iterate (
					container	=> element (device_cursor).units,
					process		=> query_units'access);
				
				--log_indentation_down;
			end query_devices;

			procedure query_submodules (submodule_cursor : in et_submodules.type_submodules.cursor) is
				submodule_position : et_coordinates.type_position;
				ports : et_submodules.type_submodule_ports.map;

				procedure query_port (port_cursor : in et_submodules.type_submodule_ports.cursor) is
					use et_submodules.type_submodule_ports;
					use et_general.type_net_name;
				begin
					log (text => "port " & type_net_name.to_string (key (port_cursor)) &
							" at" & to_string (element (port_cursor).position),
							level => log_threshold + 2);

					-- If the port sits at x/y of place then we have a match:
					if element (port_cursor).position = type_point (place) then
						log (text => " match", level => log_threshold + 2);
						
						-- Insert the port in the portlist to be returned:
						et_schematic.type_ports_submodule.insert 
							(
							container	=> ports_at_place.ports.submodules,
							new_item	=> 
								(
								module_name => key (submodule_cursor),
								port_name	=> key (port_cursor)
								)
							);
					end if;
				end query_port;

			begin -- query_submodules
				submodule_position := element (submodule_cursor).position;

				-- Look at submodules on the given sheet of place:
				if sheet (submodule_position) = sheet (place) then
					log (text => "submodule " & to_string (key (submodule_cursor)), level => log_threshold + 1);
					log_indentation_up;

					ports := element (submodule_cursor).ports;
					
					et_submodules.move_ports (ports, submodule_position);

					et_submodules.type_submodule_ports.iterate (ports, query_port'access);

					log_indentation_down;
				end if;
				
			end query_submodules;

			procedure query_netchangers (netchanger_cursor : in et_submodules.type_netchangers.cursor) is
				netchanger_position : et_coordinates.type_position;
				ports : et_submodules.type_netchanger_ports;
				use et_netlists;
			begin -- query_netchangers
				netchanger_position := element (netchanger_cursor).position_sch;

				-- Look at netchangers on the given sheet of place:
				if sheet (netchanger_position) = sheet (place) then
					log (text => "netchanger " & et_submodules.to_string (key (netchanger_cursor)), level => log_threshold + 1);
					log_indentation_up;	
					
					-- get the absolute port positions of the netchanger
					ports := et_submodules.netchanger_ports (netchanger_cursor);

					-- If the port sits at x/y of place then we have a match.
					-- The match can either be at the msster or slave port.
					-- The match can/should NEVER be at both ports simultaneously.
					
					-- First test whether the master port sits here:
					if ports.master = type_point (place) then

						log (text => "port " & et_submodules.to_string (et_submodules.MASTER) &
							" at" & to_string (ports.master),
							level => log_threshold + 2);
						
						-- Insert the port in the portlist to be returned:
						type_ports_netchanger.insert 
							(
							container	=> ports_at_place.ports.netchangers,
							new_item	=> 
								(
								index	=> key (netchanger_cursor),
								port	=> et_submodules.MASTER
								)
							);

					-- Second, test wheter slave port sits here:
					elsif ports.slave = type_point (place) then

						log (text => "port " & et_submodules.to_string (et_submodules.SLAVE) &
							" at" & to_string (ports.slave),
							level => log_threshold + 2);
						
						-- Insert the port in the portlist to be returned:
						type_ports_netchanger.insert 
							(
							container	=> ports_at_place.ports.netchangers,
							new_item	=> 
								(
								index	=> key (netchanger_cursor),
								port	=> et_submodules.SLAVE
								)
							);
					end if;

					log_indentation_down;
				end if;
			end query_netchangers;
						
		begin -- query_module
			iterate (module.devices, query_devices'access);
			iterate (module.submods, query_submodules'access);
			iterate (module.netchangers, query_netchangers'access);
		end query_module;
		
	begin -- ports_at_place
		log (text => "module " & to_string (module_name) &
			 " locating ports at" & to_string (position => place),
			 level => log_threshold);

		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);

		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
		return ports;
	end ports_at_place;
	

	-- Renames the device ports of the net segments affected by a rename operation.
	-- Leaves the unit and port names as they are because this is solely about device names.
	procedure rename_ports (
		module			: in pac_generic_modules.cursor;		-- the module
		device_before	: in type_device_name;					-- the device name before like IC1
		device_after	: in type_device_name;					-- the device name after like IC23
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

									port_cursor : type_ports_device.cursor := segment.ports_devices.first;
									
								begin -- query_ports
									while port_cursor /= type_ports_device.no_element loop

										if element (port_cursor).device_name = device_before then -- IC1

											replace_element (
												container	=> segment.ports_devices,
												position	=> port_cursor,
												new_item	=> (
													device_name	=> device_after, -- IC23
													unit_name	=> element (port_cursor).unit_name, -- unchanged
													port_name 	=> element (port_cursor).port_name) -- unchanged
												);
										end if;
										
										next (port_cursor);
									end loop;
								end query_ports;
								
							begin -- query_segment
								log_indentation_up;
								log (text => to_string (segment_cursor), level => log_threshold + 2);

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
						log (text => "strand " & to_string (position => element (strand_cursor).position),
							 level => log_threshold + 2);

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
				log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);

				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);
				
			end query_net;				
			
		begin -- query_nets
			type_nets.iterate (module.nets, query_net'access);
		end query_nets;
		
	begin -- rename_ports
		log (text => "renaming ports in nets ...", level => log_threshold);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;
	end rename_ports;
	
	procedure rename_device (
		module_name			: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name_before	: in type_device_name; -- IC1
		device_name_after	: in type_device_name; -- IC23
		log_threshold		: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

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
				if not et_conventions.prefix_valid (device_name_after) then
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
		log (text => "module " & enclose_in_quotes (to_string (module_name)) &
			" renaming device " & to_string (device_name_before) & " to " & 
			to_string (device_name_after),
			level => log_threshold);

		log_indentation_up;

		-- The old and new name must not be the same:
		if device_name_after /= device_name_before then

			-- The old and new prefix must be the same in order to
			-- prevent an inadvertently category change:
			if same_prefix (device_name_after, device_name_before) then
			
				-- locate module
				module_cursor := locate_module (module_name);

				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> query_devices'access);

			else
				raise semantic_error_1 with "ERROR: Changing the prefix is not allowed !";
			end if;
		else
			raise semantic_error_1 with "ERROR: Old and new device name are equal !";
		end if;
		
		log_indentation_down;
	end rename_device;

	procedure set_value (
	-- Sets the value of a device.
		module_name			: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name			: in type_device_name; -- R2
		value				: in type_value.bounded_string; -- 470R
		log_threshold		: in type_log_level) is
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

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

			use et_symbols;
			
		begin -- query_devices
			-- locate the device
			device_cursor := find (module.devices, device_name); -- R1

			if device_cursor /= et_schematic.type_devices.no_element then -- the device should be there

				-- Only real devices have a value.
				if element (device_cursor).appearance = PCB then

					-- Check value regarding the device category:
					if et_conventions.value_valid (value, prefix (device_name)) then 
					
						update_element (
							container	=> module.devices,
							position	=> device_cursor,
							process		=> set_value'access);

					else
						--log (ERROR, "value " & enclose_in_quotes (to_string (value)) 
						--& " invalid for this kind of device !", console => true);

						log_indentation_down;
						
						raise semantic_error_1 with 
							"ERROR: Value " & enclose_in_quotes (to_string (value)) 
							& " invalid for this kind of device !";
							-- CS more details ?
							
					end if;

				else -- virtual device
					log_indentation_down;
						
					raise semantic_error_1 with -- CS semantic_error_2 for warning ?
						"ERROR: Device " & to_string (device_name) 
						& " is virtual and has no value !";
				end if;

			else
				log_indentation_down;
				device_not_found (device_name);
			end if;
		end query_devices;
		
	begin -- set_value
		log (text => "module " 
			 & enclose_in_quotes (to_string (module_name)) 
			 & " setting " & to_string (device_name) 
			 & " value to " & to_string (value),
			level => log_threshold);

		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

		log_indentation_down;
	end set_value;

	procedure set_purpose (
	-- Sets the purpose of a device.
		module_name			: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name			: in type_device_name; -- R2
		purpose				: in pac_device_purpose.bounded_string; -- brightness_control
		log_threshold		: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

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

			use et_symbols;
			
		begin -- query_devices
			-- locate the device
			device_cursor := find (module.devices, device_name); -- R1

			if device_cursor /= et_schematic.type_devices.no_element then -- the device should be there

				-- Only real devices have a purpose. Issue warning if targeted device is virtual.
				if element (device_cursor).appearance = PCB then

					update_element (
						container	=> module.devices,
						position	=> device_cursor,
						process		=> set_purpose'access);

				else
					log (WARNING, "device " & to_string (device_name) &
						 " is virtual and has no practical purpose !");
				end if;

			else
				device_not_found (device_name);
			end if;
		end query_devices;

	begin -- set_purpose
		log (text => "module " & to_string (module_name) &
			" setting " & to_string (device_name) & " purpose to " &
			enclose_in_quotes (to_string (purpose)),
			level => log_threshold);

		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

		log_indentation_down;
	end set_purpose;

	procedure set_partcode (
	-- Sets the partcode of a device.
		module_name			: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name			: in type_device_name; -- R2
		partcode			: in et_material.type_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
		log_threshold		: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

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

			use et_symbols;
			
		begin -- query_devices
			-- locate the device
			device_cursor := find (module.devices, device_name); -- R1

			if device_cursor /= et_schematic.type_devices.no_element then -- the device should be there

				-- Only real devices have a purpose. Issue warning if targeted device is virtual.
				if element (device_cursor).appearance = PCB then

					update_element (
						container	=> module.devices,
						position	=> device_cursor,
						process		=> set_partcode'access);

				else
					log (WARNING, "Device " & to_string (device_name) &
						 " is virtual and has no partcode !");
				end if;

			else
				device_not_found (device_name);
			end if;
		end query_devices;

	begin -- set_partcode
		log (text => "module " & to_string (module_name) &
			" setting " & to_string (device_name) & " partcode to " &
			enclose_in_quotes (et_material.to_string (partcode)),
			level => log_threshold);

		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

		log_indentation_down;
	end set_partcode;

	function exists (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name)
		return boolean is

		device_found : boolean := false; -- to be returned
		
		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
			use et_schematic.type_devices;
		begin
			if contains (module.devices, device) then
				device_found := true;
			end if;
		end query_devices;
		
	begin -- exists
		pac_generic_modules.query_element (
			position	=> module,
			process		=> query_devices'access);

		return device_found;
	end exists;

	function locate_device (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return et_schematic.type_devices.cursor 
	is
		result : et_schematic.type_devices.cursor;
		
		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
			use et_schematic.type_devices;
		begin
			result := find (module.devices, device);
		end;

	begin
		pac_generic_modules.query_element (
			position	=> module,
			process		=> query_devices'access);

		return result;
	end locate_device;

	function locate_device (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_devices_lib.cursor
	is
		cursor_sch : et_schematic.type_devices.cursor;
	begin
		-- find the device in the module
		cursor_sch := locate_device (module, device);

		-- find the device in the library
		return get_device (cursor_sch);
	end locate_device;

	
	function locate_unit (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name; -- R2
		unit	: in pac_unit_name.bounded_string)
		return et_schematic.type_units.cursor
	is
		use et_schematic.type_devices;
		use et_schematic.type_units;
		
		device_cursor : et_schematic.type_devices.cursor;
		unit_cursor : et_schematic.type_units.cursor; -- to be returned

		procedure query_units (
			device_name	: in et_devices.type_device_name; -- R2
			device		: in et_schematic.type_device)
		is begin
			unit_cursor := find (device.units, unit);
		end query_units;
	
	begin -- locate_unit
		device_cursor := locate_device (module, device);

		-- locate the unit
		query_element (device_cursor, query_units'access);

		return unit_cursor;
	end locate_unit;

	function deployed (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name; -- R2
		unit	: in pac_unit_name.bounded_string)
		return boolean
	is
		use et_schematic.type_units;
		unit_cursor : et_schematic.type_units.cursor;
	begin
		unit_cursor := locate_unit (module, device, unit);

		if unit_cursor = et_schematic.type_units.no_element then
			return false;
		else
			return true;
		end if;
	end deployed;

	function device_model_name (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_device_model_file.bounded_string
	is 
		use et_schematic.type_devices;
	begin
		return element (locate_device (module, device)).model;
	end device_model_name;

	function get_available_variants (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_variants.map
	is
		cursor_lib : pac_devices_lib.cursor;	
	begin
		cursor_lib := locate_device (module, device);
		return available_variants (cursor_lib);
	end get_available_variants;
	
	function get_variant (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_package_variant_name.bounded_string -- D, N
	is
		cursor_sch : et_schematic.type_devices.cursor;
	begin
		cursor_sch := locate_device (module, device);
		
		return et_schematic.type_devices.element (cursor_sch).variant;
	end get_variant;

	procedure set_variant (
		module	: in pac_generic_modules.cursor;
		device	: in et_schematic.type_devices.cursor;
		variant	: in pac_package_variant_name.bounded_string)
	is
		use et_schematic.type_devices;

		procedure query_device (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is

			procedure do_it (
				name	: in et_devices.type_device_name;
				dev		: in out et_schematic.type_device)
			is 
				cursor_lib : pac_devices_lib.cursor;
			begin
				cursor_lib := locate_device (dev.model);

				if variant_available (cursor_lib, variant) then
					dev.variant := variant;
				else
					raise semantic_error_1 with
						"ERROR: Variant " & to_string (variant) 
						& " not defined in device model !"; -- CS output file name ?
				end if;
			end do_it;

		begin
			update_element (
				container	=> module.devices,
				position	=> device,
				process		=> do_it'access);
		end query_device;
		
	begin
		if is_real (device) then
			
			update_element (
				container	=> generic_modules,
				position	=> module,
				process		=> query_device'access);

		else
			raise semantic_error_1 with
			 "ERROR: Device is virtual and does not have a package !";
		end if;
	end set_variant;

	procedure set_variant (
		module			: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device			: in et_devices.type_device_name; -- R2
		variant			: in pac_package_variant_name.bounded_string; -- N, D
		log_threshold	: in type_log_level)
	is
		use pac_generic_modules;
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified
		device_cursor : et_schematic.type_devices.cursor;
	begin
		log (text => "module " & enclose_in_quotes (to_string (module))
			 & " setting package variant of " & to_string (device)
			 & " to " & to_string (variant) & " ...",
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module);

		if exists (module_cursor, device) then
			device_cursor := locate_device (module_cursor, device);			
			set_variant (module_cursor, device_cursor, variant);
		else
			device_not_found (device);
		end if;
		
	end set_variant;
	
	function device_model_cursor (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_devices_lib.cursor
	is
		cursor_sch : et_schematic.type_devices.cursor;
		device_model : pac_device_model_file.bounded_string;
	begin
		cursor_sch := locate_device (module, device);
		device_model := et_schematic.type_devices.element (cursor_sch).model;
		return locate_device (device_model);
	end device_model_cursor;


	
	function exists_device_port (
	-- Returns true if given device with the given port exists in module indicated by module_cursor.
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		port_name		: in et_symbols.type_port_name.bounded_string) -- CE
		return boolean is

		result : boolean := false; -- to be returned. goes true once the target has been found
		
		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use pac_unit_name;
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;

			use et_symbols;
			
			procedure query_units (
				device_name	: in type_device_name;
				device		: in et_schematic.type_device) is
				use et_schematic.type_units;
				unit_cursor : et_schematic.type_units.cursor := device.units.first;
				use et_symbols.type_ports;
				ports : et_symbols.type_ports.map;
				use type_port_name;
			begin
				while unit_cursor /= type_units.no_element loop
					--log (text => "unit " & pac_unit_name.to_string (key (unit_cursor)));
					--log (text => "port " & type_port_name.to_string (port_name));
					
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
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string := to_unit_name (""); -- A
		port_name		: in et_symbols.type_port_name.bounded_string := et_symbols.to_port_name ("")) -- CE
		return boolean is

		result : boolean := false; -- to be returned, goes true once the target has been found
		
		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use pac_unit_name;
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;

			use et_symbols;
			
			procedure query_units (
				device_name	: in type_device_name;
				device		: in et_schematic.type_device) is
				use et_schematic.type_units;
				use et_symbols.type_ports;
				ports : et_symbols.type_ports.map;
				use type_port_name;
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
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		submod_instance	: in et_general.type_module_instance_name.bounded_string; -- MOT_DRV_3
		port_name		: in et_general.type_net_name.bounded_string) -- RESET
		return boolean is

		use et_general.type_module_instance_name;
		use et_submodules;
		
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
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		index			: in et_submodules.type_netchanger_id) -- 1, 2, 3, ...
		return boolean is

		result : boolean := false; -- to be returned, goes true once the target has been found		

		procedure query_netchangers (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use et_submodules.type_netchangers;
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

	function next_device_name (
	-- Returns for the given device prefix the next available device name in the module.
	-- Example: prefix is C. If there are C1, C12, C1034 and C1035 the return will be C2.

	-- CS: look up non-electric devices

		module_cursor	: in pac_generic_modules.cursor;
		prefix			: in type_prefix.bounded_string; -- R, L, C, IC, FD, H, ...
		category		: in type_device_category := ELECTRICAL)
		return type_device_name is -- C2
		
		next_name : type_device_name; -- to be returned

		use type_prefix;
		
		procedure search_gap_electric (
		-- Searches for the lowest available device name. Looks at devices
		-- whose prefix equals the given prefix. Example: If given prefix is R, it looks
		-- for the lowest available resistor index.
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor := module.devices.first;

			-- We start the search with index 1. Not 0 because this would result in a zero based
			-- numbering order. Index zero is allowed but not automatically choosen.
			index_expected : type_name_index := type_name_index'first + 1;

			gap_found : boolean := false; -- goes true once a gap has been found
		begin
			while device_cursor /= et_schematic.type_devices.no_element loop
				if et_devices.prefix (key (device_cursor)) = prefix then -- prefix match
					
					if index (key (device_cursor)) /= index_expected then -- we have a gap

						-- build the next available device name (like IC12)
						next_name := to_device_name (prefix, index_expected);

						-- The proposed next_name must not be occupied by a non-electrical device.
						-- Look up the list of non-electrical devices. If the name
						-- is already in use, discard it and try the next name.
						if not module.devices_non_electric.contains (next_name) then
							gap_found := true;
							exit;
						end if;
					end if;

					index_expected := index_expected + 1;
				end if;
				
				next (device_cursor);
			end loop;

			-- If no gap has been found, then the device name must be assembled
			-- using the latest index_expected.
			if not gap_found then
				next_name := to_device_name (prefix, index_expected);

				-- The proposed next_name must not be occupied by a non-electrical device.
				-- Increment index and propose a new next_name until it can not be
				-- found among the non-electric devices anymore.
				while module.devices_non_electric.contains (next_name) loop
					index_expected := index_expected + 1;

					-- propose a new next_name
					next_name := to_device_name (prefix, index_expected);
				end loop;
			end if;
			
		end search_gap_electric;

		procedure search_gap_non_electric (
		-- Searches for the lowest available non-electrical device name. Looks at devices
		-- whose prefix equals the given prefix. Example: If given prefix is MH, it looks
		-- for the lowest available mounting hole index.
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use pac_devices_non_electric;
			device_cursor : pac_devices_non_electric.cursor := module.devices_non_electric.first;

			-- We start the search with index 1. Not 0 because this would result in a zero based
			-- numbering order. Index zero is allowed but not automatically choosen.
			index_expected : type_name_index := type_name_index'first + 1;

			gap_found : boolean := false; -- goes true once a gap has been found
		begin -- search_gap
			while device_cursor /= pac_devices_non_electric.no_element loop
				if et_devices.prefix (key (device_cursor)) = prefix then -- prefix match
					
					if index (key (device_cursor)) /= index_expected then -- we have a gap

						-- build the next available device name and exit
						next_name := to_device_name (prefix, index_expected);

						-- The proposed next_name must not be occupied by an electrical device.
						-- Look up the list of electrical devices. If the name
						-- is already in use, discard it and try the next name.
						if not module.devices.contains (next_name) then
							gap_found := true;
							exit;
						end if;
					end if;

					index_expected := index_expected + 1;
				end if;
				
				next (device_cursor);
			end loop;

			-- If no gap has been found, then the device name must be assembled
			-- using the latest index_expected.
			if not gap_found then
				next_name := to_device_name (prefix, index_expected);

				-- The proposed next_name must not be occupied by an electrical device.
				-- Increment index and propose a new next_name until it can not be
				-- found among the electrical devices anymore.
				while module.devices.contains (next_name) loop
					index_expected := index_expected + 1;

					-- propose a new next_name
					next_name := to_device_name (prefix, index_expected);
				end loop;
			end if;
			
		end search_gap_non_electric;
		
	begin -- next_device_name

		-- The device category decides where to look first for a free device name.
		case category is
			when ELECTRICAL =>
				
				query_element (
					position	=> module_cursor,
					process		=> search_gap_electric'access);

			when NON_ELECTRICAL =>
				
				query_element (
					position	=> module_cursor,
					process		=> search_gap_non_electric'access);
		end case;
				
		return next_name;
	end next_device_name;

	
	function placeholders_of_package (
	-- Returns the placeholders of the package of a device. The package is indirectly selected
	-- by the given variant name. The given device is accessed by the given device cursor.
		device	: in pac_devices_lib.cursor;
		variant	: in pac_package_variant_name.bounded_string) -- N, D, S_0805
		return et_packages.type_text_placeholders is
		use et_packages;
		use pac_devices_lib;
		use pac_variants;
		placeholders		: et_packages.type_text_placeholders; -- to be returned

		-- fetch the package variants available for the given device:
		variants_available	: pac_variants.map := element (device).variants;
		
		variant_cursor		: pac_variants.cursor;
		package_model		: type_package_model_file.bounded_string; -- ../lbr/smd/SO15.pac

		use et_packages;		
		use type_packages;
		package_cursor		: type_packages.cursor;

	begin -- placeholders_of_package
		
		-- locate the given variant in the device:
		variant_cursor := pac_variants.find (variants_available, variant);

		-- get the package model name:
		package_model := element (variant_cursor).package_model; -- ../lbr/smd/SO15.pac

		-- locate the package model in the package library:
		package_cursor := type_packages.find (packages, package_model);

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
		device_model	: in pac_device_model_file.bounded_string; -- ../libraries/devices/logic_ttl/7400.dev
		variant			: in pac_package_variant_name.bounded_string; -- N, D, S_0805
		destination		: in et_coordinates.type_position; -- sheet/x/y,rotation
		log_threshold	: in type_log_level) is separate;

	procedure copy_device (
	-- Copies the given device. Places the first unit of the device (according to add level)
	-- at the given destination in the schematic.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		destination		: in et_coordinates.type_position; -- sheet/x/y/rotation
		log_threshold	: in type_log_level) is separate;

	function available_units (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC1
		log_threshold	: in type_log_level)
		return et_devices.pac_unit_names.list
	is
		use et_schematic.type_devices;
		device_cursor_sch : et_schematic.type_devices.cursor;

		device_model : pac_device_model_file.bounded_string;
		device_cursor_lib : pac_devices_lib.cursor;

		use pac_unit_names;
		all_unit_names : pac_unit_names.list;
		names_of_available_units : pac_unit_names.list;

		procedure query_in_use (c : in pac_unit_names.cursor) is 

			in_use : boolean := false;

			-- Sets the in_use flag if given unit is already in use:
			procedure query_in_use (
				device_name	: in type_device_name;
				device		: in et_schematic.type_device) 
			is
				use et_schematic.type_units;
			begin
				if contains (device.units, element (c)) then
					in_use := true;
				end if;
			end query_in_use;

		begin
			-- Test whether the unit is already in use.
			query_element (
				position	=> device_cursor_sch,
				process		=> query_in_use'access);

			-- If the unit is available then append it to the result:
			if not in_use then -- unit is available
				log (text => "unit " & to_string (element (c)) & " available.",
					 level => log_threshold + 2);
				
				names_of_available_units.append (element (c));
			end if;
		end query_in_use;

		procedure get_device_model (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module)
		is begin
			-- locate the device in the schematic:
			device_cursor_sch := find (module.devices, device_name);

			device_model := element (device_cursor_sch).model;

			log (text => "device model " & to_string (device_model),
				level => log_threshold + 1);
		end get_device_model;
		
	begin -- available_units
		log (text => "looking up available units of " 
			 & to_string (device_name) & " ...",
			 level => log_threshold);

		log_indentation_up;

		-- get the device model:
		query_element (module_cursor, get_device_model'access);
				
		-- locate the device in the library
		device_cursor_lib := locate_device (device_model);

		log_indentation_up;
		
		-- get the names of all units of the device
		all_unit_names := all_units (device_cursor_lib);

		-- extract available units
		all_unit_names.iterate (query_in_use'access);

		log_indentation_down;
		log_indentation_down;
		
		return names_of_available_units;

		--exception when event: others =>
			--put_line (exception_information (event));

			--return names_of_available_units;
	end available_units;

	function unit_available (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC1
		unit_name		: in pac_unit_name.bounded_string)
		return boolean
	is
		available : boolean := true; -- to be returned

		use et_schematic.type_devices;
		device_cursor_sch : et_schematic.type_devices.cursor;
		
		device_cursor_lib : pac_devices_lib.cursor;
		
		use pac_unit_names;
		all_unit_names : pac_unit_names.list;

		-- Clears the "available" flag if given unit is already in use:
		procedure query_in_use (
			device_name	: in type_device_name;
			device		: in et_schematic.type_device) 
		is
			use et_schematic.type_units;
		begin
			if contains (device.units, unit_name) then
				available := false;
			end if;
		end query_in_use;
		
	begin -- unit_available
		device_cursor_lib := device_model_cursor (module_cursor, device_name);

		-- get the names of all units of the device
		all_unit_names := all_units (device_cursor_lib);

		-- test whether the given unit is defined in the model:
		if contains (all_unit_names, unit_name) then
			
			-- locate the device in the schematic:
			device_cursor_sch := locate_device (module_cursor, device_name);

			-- Test whether the unit is already in use.
			-- If device does not exist, a constraint_error will arise here.
			query_element (
				position	=> device_cursor_sch,
				process		=> query_in_use'access);
			
		else
			raise constraint_error;
		end if;
		
		return available;
	end unit_available;

	function units_on_sheet (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC1
		sheet			: in type_sheet;
		log_threshold	: in type_log_level)
		return et_devices.pac_unit_names.list
	is
		use et_schematic.type_devices;
		device_cursor_sch : et_schematic.type_devices.cursor;

		names_of_units : pac_unit_names.list;

		procedure query_units (
			device_name	: in type_device_name;
			device		: in et_schematic.type_device)
		is 
			procedure query_unit (c : in type_units.cursor) is 
				use et_devices.pac_unit_name;
				use et_schematic.type_units;
				use pac_unit_names;
			begin
				-- If the unit is on the given sheet then append it to the result:
				if et_coordinates.sheet (element (c).position) = sheet then
					log (text => "unit " & et_devices.to_string (key (c)) & " on sheet.",
						level => log_threshold + 2);
					
					names_of_units.append (key (c));
				end if;
			end query_unit;
		begin
			device.units.iterate (query_unit'access);
		end query_units;
			
	begin -- units_on_sheet
		log (text => "looking up units of " 
			 & to_string (device_name) 
			 & " on sheet " & to_sheet (sheet) & " ...",
			 level => log_threshold);

		log_indentation_up;

		-- locate the device in the schematic:
		device_cursor_sch := locate_device (module_cursor, device_name);

		-- Test whether the unit is already in use.
		-- If device does not exist, a constraint_error will arise here.
		query_element (
			position	=> device_cursor_sch,
			process		=> query_units'access);


		log_indentation_down;
		
		return names_of_units;

	end units_on_sheet;

	function position (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name; -- R2
		unit	: in pac_unit_name.bounded_string)
		return et_coordinates.type_position
	is
		use et_schematic.type_devices;
		device_cursor_sch : et_schematic.type_devices.cursor;

		unit_position : et_coordinates.type_position;
		
		procedure query_unit (
			device_name	: in type_device_name;
			device		: in et_schematic.type_device)
		is 
			use et_schematic.type_units;
			unit_cursor : type_units.cursor;
		begin
			-- locate the given unit in the given device
			unit_cursor := find (device.units, unit);

			-- get the coordinates of the unit
			unit_position := element (unit_cursor).position;
		end query_unit;

	begin
		-- locate the device in the schematic:
		device_cursor_sch := locate_device (module, device);

		query_element (
			position	=> device_cursor_sch,
			process		=> query_unit'access);

		return unit_position;
	end position;

	function position (
		device	: in et_schematic.type_devices.cursor; -- R2
		unit	: in et_schematic.type_units.cursor)
		return et_coordinates.type_position
	is
		use et_schematic.type_devices;
		unit_position : et_coordinates.type_position;
		
		procedure query_unit (
			device_name	: in type_device_name;
			device		: in et_schematic.type_device)
		is 
			use et_schematic.type_units;
		begin
			-- get the coordinates of the unit
			unit_position := element (unit).position;
		end query_unit;
	begin
		query_element (
			position	=> device,
			process		=> query_unit'access);

		return unit_position;
	end position;

	function position (
		device		: in et_schematic.type_devices.cursor; -- R2
		unit		: in et_schematic.type_units.cursor;
		category	: in et_symbols.type_placeholder_meaning)
		return pac_geometry_sch.type_point
	is
		placeholder_position : pac_geometry_sch.type_point; -- to be returned

		use et_schematic.type_devices;
		unit_position : et_coordinates.type_position;
		
		procedure query_unit (
			device_name	: in type_device_name;
			device		: in et_schematic.type_device)
		is 
			use et_schematic.type_units;
			use et_symbols;
		begin
			-- get the coordinates of the unit
			unit_position := element (unit).position;

			-- get the coordinates of the placeholder:
			case category is
				when NAME =>
					placeholder_position := element (unit).name.position;

				when PURPOSE =>
					placeholder_position := element (unit).purpose.position;

				when VALUE =>
					placeholder_position := element (unit).value.position;
			end case;

			move_by (placeholder_position, unit_position);
			
		end query_unit;
	begin
		query_element (
			position	=> device,
			process		=> query_unit'access);

		return placeholder_position;
	end position;


	
	function sheet (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name; -- R2
		unit	: in pac_unit_name.bounded_string)
		return et_coordinates.type_sheet
	is begin		
		return et_coordinates.sheet (position (module, device, unit));
	end sheet;
		
	
	procedure invoke_unit (
	-- Invokes a unit of a device into the schematic.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC1
		unit_name		: in pac_unit_name.bounded_string; -- A, B, IO_BANK_2
		destination		: in et_coordinates.type_position; -- sheet/x/y/rotation
		log_threshold	: in type_log_level) is separate;

	function next_netchanger_index (
	-- Returns the next available netchanger index in the module.
		module_cursor	: in pac_generic_modules.cursor)
		return et_submodules.type_netchanger_id is

		use et_submodules;
		next_idx : type_netchanger_id; -- to be returned

		procedure search_gap (
		-- Searches for the lowest available index.
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use type_netchangers;
			cursor : type_netchangers.cursor := module.netchangers.first;

			-- We start the search with index 1.
			index_expected : type_netchanger_id := type_netchanger_id'first;

			gap_found : boolean := false; -- goes true once a gap has been found
			
		begin -- search_gap
			while cursor /= type_netchangers.no_element loop
					
				if key (cursor) /= index_expected then -- we have a gap
					next_idx := index_expected;
					gap_found := true;
					exit;
				end if;

				index_expected := index_expected + 1;
				next (cursor);
			end loop;

			-- If no gap has been found, then the index is the latest expected index.
			if not gap_found then
				next_idx := index_expected;
			end if;
			
		end search_gap;
		
	begin -- next_netchanger_index
		query_element (
			position	=> module_cursor,
			process		=> search_gap'access);
		
		return next_idx;
	end next_netchanger_index;

	procedure insert_ports (
	-- Inserts the given netchanger ports in the net segments.
	-- If a port lands on either the start or end point of a segment, it will
	-- be regarded as "connected" with the segment.
	-- If a ports lands between start or end point of a segment, nothing happens
	-- because the docking to net segments is possible on segment ends/starts only.
	-- CS: Automatic splitting the segment into two and placing a junction is not supported
	-- jet and probably not a good idea.
		module			: in pac_generic_modules.cursor;		-- the module
		index			: in et_submodules.type_netchanger_id;	-- the netchanger id
		ports			: in et_submodules.type_netchanger_ports; -- the ports to be inserted
		sheet			: in type_sheet;	-- the sheet to look at
		log_threshold	: in type_log_level) is

		use et_submodules;
		
		procedure query_nets (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is

			procedure probe_port (
				port : in type_point; -- x/y
				name : in type_netchanger_port_name) -- master/slave
				is

				-- This flag goes true on the first match. It signals
				-- all iterations to cancel prematurely.
				port_processed : boolean := false;
					
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

						procedure change_segment (segment : in out type_net_segment) is
							use et_netlists;
						begin
							-- If port sits on start OR end point of segment AND if it
							-- is not already in the segment then append it to the 
							-- portlist of the segment.
							if 	segment.start_point = port or
								segment.end_point = port then

								-- If port not already in segment, append it.
								-- Otherwise it must not be appended again. constraint_error would arise.
								if type_ports_netchanger.contains (
									container	=> segment.ports_netchangers,
									item		=> (index, name)
									) then

									log (text => " already there -> skipped", level => log_threshold + 5);
								else
									type_ports_netchanger.insert (
										container	=> segment.ports_netchangers,
										new_item	=> (index, name)); -- 1,2,3, .. / master/slave

									log (text => " sits on segment -> inserted", level => log_threshold + 5);
								end if;
								
								-- signal iterations in upper levels to cancel
								port_processed := true;
							end if;
							
						end change_segment;

					begin -- query_segments
						log_indentation_up;

						-- On the first segment, where the port sits on, this loop ends prematurely.
						while not port_processed and segment_cursor /= type_net_segments.no_element loop
							log (text => "probing " & to_string (segment_cursor), level => log_threshold + 4);
							
							type_net_segments.update_element (
								container	=> strand.segments,
								position	=> segment_cursor,
								process		=> change_segment'access);

							next (segment_cursor);
						end loop;

						log_indentation_down;
					end query_segments;
					
				begin -- query_strands
					log_indentation_up;
					
					while not port_processed and strand_cursor /= type_strands.no_element loop

						-- We pick out only the strands on the targeted sheet:
						if et_coordinates.sheet (element (strand_cursor).position) = sheet then
							log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 3);

							log_indentation_up;
							log (text => "strand " & to_string (position => element (strand_cursor).position),
								level => log_threshold + 3);

							update_element (
								container	=> net.strands,
								position	=> strand_cursor,
								process		=> query_segments'access);
						
							log_indentation_down;
						end if;
							
						next (strand_cursor);
					end loop;
					
					log_indentation_down;
				end query_strands;
				
			begin -- probe_port
				log_indentation_up;
				log (text => "at" & to_string (port), level => log_threshold + 2);
				
				while not port_processed and net_cursor /= type_nets.no_element loop
					
					update_element (
						container	=> module.nets,
						position	=> net_cursor,
						process		=> query_strands'access);
				
					next (net_cursor);
				end loop;

				log_indentation_down;
			end probe_port;
			
		begin -- query_nets
			log (text => "master port", level => log_threshold + 1);
			probe_port (ports.master, MASTER);

			log (text => "slave port", level => log_threshold + 1);			
			probe_port (ports.slave, SLAVE);
		end query_nets;

	begin --insert_ports
		log (text => "inserting netchanger ports in nets on sheet" & 
			 to_sheet (sheet) & " ...", level => log_threshold);
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;
	end insert_ports;
	
	procedure add_netchanger (
	-- Adds a netchanger to the schematic.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		place			: in et_coordinates.type_position; -- sheet/x/y/rotation
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		procedure query_netchangers (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_submodules;
			use type_netchangers;
			cursor : type_netchangers.cursor;
			index : type_netchanger_id;
			netchanger : type_netchanger;
			inserted : boolean;
			ports : type_netchanger_ports;
		begin -- query_netchangers

			-- set the index to be used for the new netchanger
			index := next_netchanger_index (module_cursor);
			log (text => "netchanger index is" & to_string (index), level => log_threshold + 1);
			
			-- build the new netchanger
			netchanger.position_sch := place;

			-- insert the new netchanger in the module
			insert (
				container 	=> module.netchangers,
				key			=> index,
				new_item	=> netchanger,
				position	=> cursor,
				inserted	=> inserted -- CS not further evaluated. should always be true
				);

			-- Get the absolute positions of the netchanger ports according to 
			-- location and rotation in schematic.
			ports := netchanger_ports (cursor);

			-- Inserts the given netchanger ports in the net segments.
			insert_ports (
				module			=> module_cursor,
				index			=> index,
				ports			=> ports,
				sheet			=> sheet (place),
				log_threshold	=> log_threshold + 1);
			
		end query_netchangers;
		
	begin -- add_netchanger
		log (text => "module " & to_string (module_name) &
			" adding netchanger at" & to_string (position => place) &
			" rotation" & to_string (rot (place)),
			level => log_threshold);

		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_netchangers'access);
		
		log_indentation_down;		
	end add_netchanger;

	procedure delete_ports (
	-- Deletes ports of the given netchanger in nets.
		module			: in pac_generic_modules.cursor;			-- the module
		index			: in et_submodules.type_netchanger_id;	-- the netchanger id
		sheet			: in et_coordinates.type_sheet;		-- the sheet where the netchanger is
		log_threshold	: in type_log_level) is

		procedure query_nets (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use type_nets;
			net_cursor : type_nets.cursor := module.nets.first;

			-- In order to speed up things we have two flags that indicate
			-- whether the master or slave port has been deleted from the nets.
			type type_deleted_ports is record
				master	: boolean := false;
				slave	: boolean := false;
			end record;

			deleted_ports : type_deleted_ports;

			-- This function returns true if master and slave port have been deleted.
			-- All iterations abort prematurely once all ports have been deleted.
			function all_ports_deleted return boolean is begin
				return deleted_ports.master and deleted_ports.slave;
			end;
			
			procedure query_strands (
				net_name	: in type_net_name.bounded_string;
				net			: in out type_net) is
				use et_coordinates;
				use type_strands;
				strand_cursor : type_strands.cursor := net.strands.first;

				procedure query_segments (strand : in out type_strand) is
					use type_net_segments;
					segment_cursor : type_net_segments.cursor := strand.segments.first;

					procedure query_ports (segment : in out type_net_segment) is
						use et_netlists;
						use type_ports_netchanger;
						use et_submodules;
						port_cursor : type_ports_netchanger.cursor;

						procedure delete_port is begin
							log (text => "sheet" & to_sheet (sheet) & " net " &
								to_string (key (net_cursor)) & space &
								to_string (segment_cursor),
								level => log_threshold + 1);
							delete (segment.ports_netchangers, port_cursor);
						end;
	
					begin -- query_ports
						-- Search for the master port if it has not been deleted yet:
						if not deleted_ports.master then
							port_cursor := find (segment.ports_netchangers, (index, MASTER));
							if port_cursor /= type_ports_netchanger.no_element then
								delete_port;
								deleted_ports.master := true;
							end if;
						end if;

						-- Search for the slave port if it has not been deleted yet:
						if not deleted_ports.slave then
							port_cursor := find (segment.ports_netchangers, (index, SLAVE));
							if port_cursor /= type_ports_netchanger.no_element then
								delete_port;
								deleted_ports.slave := true;
							end if;
						end if;
					end query_ports;
					
				begin -- query_segments
					while not all_ports_deleted and segment_cursor /= type_net_segments.no_element loop

						type_net_segments.update_element (
							container	=> strand.segments,
							position	=> segment_cursor,
							process		=> query_ports'access);
						
						next (segment_cursor);
					end loop;
				end query_segments;
				
			begin -- query_strands
				while not all_ports_deleted and strand_cursor /= type_strands.no_element loop

					if et_coordinates.sheet (element (strand_cursor).position) = sheet then

						update_element (
							container	=> net.strands,
							position	=> strand_cursor,
							process		=> query_segments'access);

					end if;
					
					next (strand_cursor);
				end loop;
			end query_strands;
			
		begin -- query_nets
			while not all_ports_deleted and net_cursor /= type_nets.no_element loop

				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);
				
				next (net_cursor);
			end loop;

			-- CS: warning if all_ports_deleted still true ?
		end query_nets;
		
	begin -- delete_ports
		log (text => "deleting netchanger ports in nets ...", level => log_threshold);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;
	end delete_ports;

	procedure delete_netchanger (
	-- Deletes a netchanger.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module
		use et_submodules;

		procedure query_netchangers (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_coordinates;
			use type_netchangers;
			cursor : type_netchangers.cursor;
			location : et_coordinates.type_position;
		begin -- query_netchangers

			-- locate given netchanger
			cursor := find (module.netchangers, index);

			if cursor /= type_netchangers.no_element then 
				-- netchanger exists

				-- Get coordinates of netchanger master port.
				-- Since the ports of a netchanger are all on the same sheet,
				-- the sheet is now provided by location.
				location := position (
					module_name		=> module_name,
					index			=> index,
					port			=> MASTER,
					log_threshold	=> log_threshold + 1);

				log_indentation_up;

				-- Delete netchanger ports in nets:
				delete_ports (
	 				module			=> module_cursor,
					index			=> index,

					-- Get sheet number from location:
					sheet			=> et_coordinates.sheet (location),
					
					log_threshold	=> log_threshold + 1);

				-- Delete the netchanger itself:
				delete (module.netchangers, cursor);
				
				log_indentation_down;
			else
				-- netchanger does not exist
				netchanger_not_found (index);
			end if;
			
		end query_netchangers;
		
	begin -- delete_netchanger
		log (text => "module " & to_string (module_name) &
			" deleting netchanger" & to_string (index),
			level => log_threshold);

		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_netchangers'access);
		
		log_indentation_down;		
	end delete_netchanger;

	procedure move_netchanger (
	-- Moves the given netchanger. Disconnects the netchanger from
	-- start or end points of net segments BEFORE the move. 
	-- Connects netchanger ports with segment end or strart points AFTER the move.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level) is

		use et_submodules;
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure query_netchangers (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_coordinates;
			use type_netchangers;
			cursor : type_netchangers.cursor;
			location : et_coordinates.type_position;
			ports : type_netchanger_ports;
			
			procedure move (
				index		: in type_netchanger_id;
				netchanger	: in out type_netchanger) is
			begin
				netchanger.position_sch := location;
			end move;
			
		begin -- query_netchangers

			-- locate given netchanger
			cursor := find (module.netchangers, index);

			if cursor /= type_netchangers.no_element then 
				-- netchanger exists

				-- Get coordinates of netchanger master port.
				-- Since the ports of a netchanger are all on the same sheet,
				-- the sheet is now provided by location.
				location := position (
					module_name		=> module_name,
					index			=> index,
					port			=> MASTER,
					log_threshold	=> log_threshold + 1);

				-- CS this would be easier:
				-- location := element (cursor).position_sch;
				
				log_indentation_up;

				-- Delete netchanger ports in nets:
				delete_ports (
	 				module			=> module_cursor,
					index			=> index,

					-- Get sheet number from location:
					sheet			=> et_coordinates.sheet (location),
					
					log_threshold	=> log_threshold + 1);

				-- calculate the new position 
				case coordinates is
					when ABSOLUTE =>
						-- The absolute position is defined by the given point (x/y) 
						-- and the given sheet number:
						location := to_position (point, type_sheet (sheet));

					when RELATIVE =>
						-- The relative position is the netchanger position BEFORE 
						-- the move operation shifted by the given point (x/y)
						-- and the given sheet number:
						location := element (cursor).position_sch;
						
						move (
							position	=> location,
							offset		=> to_position_relative (point, sheet));
				end case;

				-- move the netchanger to the new position
				update_element (
					container	=> module.netchangers,
					position	=> cursor,
					process		=> move'access);

				-- Get the NEW absolute positions of the netchanger ports AFTER
				-- the move operation according to location and rotation in schematic.
				ports := netchanger_ports (cursor);

				-- Inserts the netchanger ports in the net segments.
				insert_ports (
					module			=> module_cursor,
					index			=> index,
					ports			=> ports,
					sheet			=> et_coordinates.sheet (location),
					log_threshold	=> log_threshold + 1);

				log_indentation_down;
			else
				-- netchanger does not exist
				netchanger_not_found (index);
			end if;
			
		end query_netchangers;
		
	begin -- move_netchanger
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" moving netchanger" & to_string (index) &
					" to sheet" & to_sheet (sheet) &
					to_string (point), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" moving netchanger" & to_string (index) &
					" by " & to_sheet_relative (sheet) & " sheet(s)" &
					to_string (point), level => log_threshold);
		end case;
		
		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_netchangers'access);

	end move_netchanger;

	procedure drag_net_segments (
	-- Drags the net segments according to the given netchanger ports.
	-- Changes the position of start or end points of segments.
	-- Does NOT create new connections with segments if a port
	-- lands on the start or end point of another segment.
	-- Does NOT create a new connection with a segments if a port
	-- lands between start and end point.
		module			: in pac_generic_modules.cursor;	-- the module
		ports_before	: in et_submodules.type_netchanger_ports;	-- the old port positions
		ports_after		: in et_submodules.type_netchanger_ports;	-- the new port positions
		sheet			: in type_sheet;			-- the sheet to look at
		log_threshold	: in type_log_level) is

		port_before, port_after : type_point;
		
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

				-- This flag goes true once port_before has been found the first time
				-- and affected end points of segments have been moved to port_after.
				drag_processed : boolean := false;
				
				procedure query_segments (strand : in out type_strand) is
					use type_net_segments;

					segment_cursor : type_net_segments.cursor := strand.segments.first;

					procedure change_segment (segment : in out type_net_segment) is 
					-- Changes the position of start or end point of a segment according to the drag point.
					begin -- change_segment
						log_indentation_up;
						
						-- if port sits on a start point of a segment -> move start point
						if segment.start_point = port_before then
							log (text => "move segment start point from" & 
								to_string (segment.start_point),
								level => log_threshold + 3);

							segment.start_point := port_after;

							log (text => "to" & 
								to_string (segment.start_point),
								level => log_threshold + 3);

							-- signal iterations in upper level to cancel
							drag_processed := true;
						end if;

						-- if port sits on an end point of a segment -> move end point
						if segment.end_point = port_before then
							log (text => "move segment end point from" & 
								to_string (segment.end_point),
								level => log_threshold + 3);

							segment.end_point := port_after;

							log (text => "to" & 
								to_string (segment.end_point),
								level => log_threshold + 3);
							
							-- signal iterations in upper level to cancel
							drag_processed := true;
						end if;
						
						log_indentation_down;
					end change_segment;
					
				begin -- query_segments
					log_indentation_up;

					-- Probe all segments of strand for port_before. This loop must not
					-- abort even if drag_processed goes true. Reason: All segements
					-- meeting here must be dragged.
					while segment_cursor /= type_net_segments.no_element loop

						log (text => "probing " & to_string (segment_cursor), level => log_threshold + 2);
						
						update_element (
							container	=> strand.segments,
							position	=> segment_cursor,
							process		=> change_segment'access);
						
						next (segment_cursor);
					end loop;

					-- Update strand position if any movement took place.
					if drag_processed then
						set_strand_position (strand);
					end if;
					
					log_indentation_down;
				end query_segments;
				
			begin -- query_strands
				log_indentation_up;
				while strand_cursor /= type_strands.no_element loop
					
					-- We pick out only the strands on the targeted sheet:
					if et_coordinates.sheet (element (strand_cursor).position) = sheet then
						log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);

						log_indentation_up;
						log (text => "strand " & to_string (position => element (strand_cursor).position),
							level => log_threshold + 1);
					
						-- Iterate in segments of strand. If point sits on any segment
						-- the flag drag_processed goes true.
						update_element (
							container	=> net.strands,
							position	=> strand_cursor,
							process		=> query_segments'access);
					
						log_indentation_down;
					end if;
					-- All segments of strand probed (and maybe moved).

					-- If the drag point has been processed, there is no need to look up
					-- other strands for port_before.
					if drag_processed then exit; end if;
					
					next (strand_cursor);
				end loop;

				log_indentation_down;
			end query_strands;
			
		begin -- query_nets
			while net_cursor /= type_nets.no_element loop

				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);

				next (net_cursor);
			end loop;
		end query_nets;

	begin -- drag_net_segments
		log (text => "dragging net segments with netchangers on sheet" & 
			 to_sheet (sheet) & " ...", level => log_threshold);
		log_indentation_up;

		--------------
		port_before := ports_before.master;
		log (text => "probing port " & to_string (port_before), level => log_threshold + 1);

		port_after := ports_after.master;
		
		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);

		---------------
		port_before := ports_before.slave;
		log (text => "probing port " & to_string (port_before), level => log_threshold + 1);

		port_after := ports_after.slave;		
		
		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);

		
		log_indentation_down;
	end drag_net_segments;
	
	procedure drag_netchanger (
	-- Drags the given netchanger within the schematic.
	-- Already existing connections with net segments are kept.
	-- Net segment positions are modified.
	-- This operation applies to a single sheet. Dragging from one sheet
	-- to another is not possible.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level) is

		use et_submodules;
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure movable_test (
		-- Tests whether the given netchanger ports of the netchanger at location 
		-- are movable. 
		-- The criteria for movement are: no device, no submodule ports there.
		-- The ports allowed here are the ports-to-be-dragged itself.
			location 			: in et_coordinates.type_position; -- only sheet number matters
			netchanger_ports	: in et_submodules.type_netchanger_ports) -- x/y of master and slave port
			is			

			procedure test_point (
				point		: in et_coordinates.type_position; -- sheet/x/y -- the point to be probed
				port_name	: in et_submodules.type_netchanger_port_name) -- master/slave
				is 
				use et_netlists;
				ports : type_ports;
				port : type_port_netchanger;

				use type_ports_submodule;
				use type_ports_device;
				use type_ports_netchanger;
			begin
				-- If no net segments start or end at given point then this test won't
				-- complain. If segments are meeting this point, no other ports must be
				-- here (except the port-to-be-dragged):
				if net_segment_at_place (module_cursor, point) then

					-- There are net segments starting or ending at point.
					-- Make sure at point are no ports of devices, submodules or other 
					-- netchangers (except the submodule port to be dragged):

					port := (index, port_name); -- the port to be dragged, like netchanger 12 port master

					-- Collect all ports of possible other devices, submodules and netchangers
					-- at given point:
					ports := ports_at_place (module_name, point, log_threshold + 2);

					-- If no device and no submodule ports here:
					if is_empty (ports.devices) and is_empty (ports.submodules) then

						-- If the ONE and ONLY netchanger port is the 
						-- port-to-be-dragged then everything is fine.
						if length (ports.netchangers) = 1 then
							
							if contains (ports.netchangers, port) then
								null; -- fine -> movable test passed
							else
								-- there is another netchanger port
								dragging_not_possible (to_string (port_name), point);
							end if;
						
						else
							-- there are more submodule ports
							dragging_not_possible (to_string (port_name), point);
						end if;
						
					else -- device or netchanger ports here
						dragging_not_possible (to_string (port_name), point);
					end if;
				end if;
			end test_point;
			
		begin -- movable_test
			log (text => "movable test ...", level => log_threshold + 1);
			log_indentation_up;

			-- Test point where the master port is:
			test_point 
				(
				point		=> to_position (
								point => netchanger_ports.master,
								sheet => sheet (location)),
				port_name	=> MASTER
				);

			-- Test point where the slave port is:			
			test_point 
				(
				point		=> to_position (
								point => netchanger_ports.slave,
								sheet => sheet (location)),
				port_name	=> SLAVE
				);
		
			log_indentation_down;
		end movable_test;
		
		procedure query_netchangers (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_coordinates;
			use type_netchangers;
			cursor : type_netchangers.cursor;
			location : et_coordinates.type_position;
			ports_old : type_netchanger_ports;
			ports_new : type_netchanger_ports;
			
			procedure move (
				index		: in type_netchanger_id;
				netchanger	: in out type_netchanger) is
			begin
				netchanger.position_sch := location;
			end move;
			
		begin -- query_netchangers

			-- locate given netchanger
			cursor := find (module.netchangers, index);

			if cursor /= type_netchangers.no_element then 
				-- netchanger exists

				-- Before the actual drag, the coordinates of the
				-- netchanger ports must be fetched. These coordinates will later assist
				-- in changing the positions of connected net segments.
				ports_old := netchanger_ports (cursor);

				-- Fetch the netchanger position BEFORE the move.
				location := element (cursor).position_sch;

				-- Test whether the port at the current position can be dragged:
				movable_test (location, ports_old);
				
				-- calculate the new position the netchanger will have AFTER the move:
				case coordinates is
					when ABSOLUTE =>
						-- The absolute position is defined by the given point (x/y).
						-- The sheet number does not change.
						set (location, point);

					when RELATIVE =>
						-- The new relative position is the netchanger position BEFORE 
						-- the move operation shifted by the given point (x/y).
						-- The sheet number does not change.
						move_by (
							point		=> location,
							offset		=> point);
				end case;

				-- move the netchanger to the new position
				update_element (
					container	=> module.netchangers,
					position	=> cursor,
					process		=> move'access);

				-- Get the NEW absolute positions of the netchanger ports AFTER
				-- the move operation according to location and rotation in schematic.
				ports_new := netchanger_ports (cursor);

				-- Change net segments in the affected nets (type_module.nets):
				drag_net_segments (
					module			=> module_cursor,
					ports_before	=> ports_old,
					ports_after		=> ports_new,
					sheet			=> et_coordinates.sheet (location),
					log_threshold	=> log_threshold + 1);

				-- The drag operation might result in new port-to-net connections.
				-- So we must insert new ports in segments.
				-- Insert possible new netchanger ports in the nets (type_module.nets):
				log_indentation_up;
				
				-- Inserts the netchanger ports in the net segments.
				insert_ports (
					module			=> module_cursor,
					index			=> index,
					ports			=> ports_new,
					sheet			=> et_coordinates.sheet (location),
					log_threshold	=> log_threshold + 1);

				log_indentation_down;
			else
				-- netchanger does not exist
				netchanger_not_found (index);
			end if;
			
		end query_netchangers;
		
	begin -- drag_netchanger
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" dragging netchanger" & to_string (index) &
					" to" & to_string (point), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" dragging netchanger" & to_string (index) &
					" by" & to_string (point), level => log_threshold);
		end case;
		
		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_netchangers'access);

	end drag_netchanger;

	procedure rotate_netchanger (
	-- Rotates the given netchanger. Disconnects it from
	-- start or end points of net segments.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		rotation		: in et_coordinates.type_rotation; -- 90
		log_threshold	: in type_log_level) is

		use et_submodules;
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure query_netchangers (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_coordinates;
			use type_netchangers;
			cursor : type_netchangers.cursor;
			location : et_coordinates.type_position;
			rotation : et_coordinates.type_rotation;
			ports_old : type_netchanger_ports;
			ports_new : type_netchanger_ports;
			
			procedure rotate (
				index		: in type_netchanger_id;
				netchanger	: in out type_netchanger) is
			begin
				set (netchanger.position_sch, rotation);
			end;
			
		begin -- query_netchangers

			-- locate given netchanger
			cursor := find (module.netchangers, index);

			if cursor /= type_netchangers.no_element then 
				-- netchanger exists

				log_indentation_up;

				-- Before the actual rotation, the coordinates of the
				-- netchanger ports must be fetched.
				ports_old := netchanger_ports (cursor);
			
				-- Fetch the current netchanger position and rotation:
				location := element (cursor).position_sch;
				rotation := rot (location);

				-- Delete netchanger ports in nets:
				delete_ports (
	 				module			=> module_cursor,
					index			=> index,

					-- Get sheet number from location:
					sheet			=> et_coordinates.sheet (location),
					
					log_threshold	=> log_threshold + 1);
				
				-- calculate the rotation the netchanger will have AFTER the move:
				case coordinates is
					when ABSOLUTE =>
						rotation := rotate_netchanger.rotation;

					when RELATIVE =>
						rotation := add (rotation, rotate_netchanger.rotation);
				end case;

				-- rotate the netchanger to the new rotation
				update_element (
					container	=> module.netchangers,
					position	=> cursor,
					process		=> rotate'access);

				-- Get the NEW absolute positions of the netchanger ports AFTER
				-- the rotation according to location and rotation in schematic.
				ports_new := netchanger_ports (cursor);

				-- Inserts the netchanger ports in the net segments.
				insert_ports (
					module			=> module_cursor,
					index			=> index,
					ports			=> ports_new,
					sheet			=> et_coordinates.sheet (location),
					log_threshold	=> log_threshold + 1);

				log_indentation_down;
			else
				-- netchanger does not exist
				netchanger_not_found (index);
			end if;
			
		end query_netchangers;
		
	begin -- rotate_netchanger
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					 " rotating netchanger" & to_string (index) &
					 " to" & to_string (rotation), level => log_threshold);

			when RELATIVE =>
				if rotation in type_rotation_relative then
					log (text => "module " & to_string (module_name) &
						" rotating netchanger" & to_string (index) &
						" by" & to_string (rotation), level => log_threshold);
				else
					relative_rotation_invalid;
				end if;
		end case;
		
		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_netchangers'access);

	end rotate_netchanger;
		
	procedure add_submodule (
	-- Adds a submodule instance to the schematic.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		file			: in et_submodules.type_submodule_path.bounded_string; -- the file name of the submodule like templates/oscillator.mod
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		position		: in et_coordinates.type_position; -- sheet, lower left corner x/y 
		size			: in et_submodules.type_submodule_size; -- the size of the box in x and y
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		full_file_name : constant string := expand (et_submodules.to_string (file));
		
		use et_submodules;

		procedure add (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use type_submodules;
			submod_cursor : type_submodules.cursor;
			inserted : boolean;
			submodule : type_submodule;
		begin -- add
			-- THE FOLLOWING IS ABOUT THE GRAPHICAL REPRESENTATION OF A SUBMODULE.
			-- THIS IS THE RECTANGULAR BOX AT THE SHEET WHERE IT THE SUBMODULE IS INSTANTIATED.
			
			-- initialize the submodule with basic properties
			submodule.file := file;
			submodule.position := position;
			submodule.size := size;
			-- Other properties like view mode, device name offset and ports
			-- have to be set via other commands.

			insert (
				container	=> module.submods,
				key			=> instance,
				new_item	=> submodule,
				position	=> submod_cursor,
				inserted	=> inserted);

			if not inserted then
				log (ERROR, "submodule instance " &
					enclose_in_quotes (to_string (instance)) &
					" already exists !", console => true);
				raise constraint_error;
			end if;
		end add;

		use et_general.type_module_name;
		
	begin -- add_submodule
		log (text => "module " & enclose_in_quotes (et_general.to_string (module_name)) &
			" adding submodule " & to_string (file) & 
			" instance " & enclose_in_quotes (to_string (instance)),
			level => log_threshold);

		log (text => " at" & to_string (position => position) &
			to_submodule_size (size),
			level => log_threshold);

		-- Make sure the parent module does not use itself as submodule:
		if module_name = to_module_name (file) then
			log (ERROR, "Circular dependency: A module can not have itself as submodule !", console => true);
		end if;

		-- locate module
		module_cursor := locate_module (module_name);

		-- Make sure the submodule file exists. The file is 
		-- identified by its full path and name. If the file exists
		-- then a submodule is inserted in the targeted module.
		-- NOTE: This is the rectangular box at the targeted sheet that
		-- represents the submodule:
		if ada.directories.exists (full_file_name) then
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> add'access);

		else
			log (ERROR, "submodule file " & to_string (file) & " not found !",
				 console => true);
			raise constraint_error;
		end if;

		-- THIS IS ABOUT THE ACTUAL SCHEMATIC AND LAYOUT STUFF OF THE SUBMODULE:
		-- Read the submodule file and store its content in container et_project.modules:
		et_project.modules.read_module (to_string (file), log_threshold + 1);		

	end add_submodule;

	procedure insert_port (
	-- Inserts the given submodule port in the net segments.
	-- If the port lands on either the start or end point of a segment, it will
	-- be regarded as "connected" with the segment.
	-- If the port lands between start or end point of a segment, nothing happens
	-- because the docking to net segments is possible on segment ends/starts only.
	-- CS: Automatic splitting the segment into two and placing a junction is not supported
	-- jet and probably not a good idea.
		module			: in pac_generic_modules.cursor;		-- the module
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC
		port			: in et_general.type_net_name.bounded_string; -- clock_output
		position		: in et_coordinates.type_position; -- the port position
		log_threshold	: in type_log_level) is

		procedure query_nets (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is

			-- This flag goes true on the first match. It signals
			-- all iterations to cancel prematurely.
			port_processed : boolean := false;
			
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

					procedure change_segment (segment : in out type_net_segment) is
					begin
						-- If port sits on start OR end point of segment AND if it
						-- is not already in the segment then append it to the 
						-- portlist of the segment.
						if 	segment.start_point = type_point (position) or
							segment.end_point = type_point (position) then

							-- If port not already in segment, append it.
							-- Otherwise it must not be appended again. constraint_error would arise.
							if type_ports_submodule.contains (
								container	=> segment.ports_submodules,
								item		=> (instance, port) -- OSC1, clock_output
								) then

								log (text => " already there -> skipped", level => log_threshold + 3);
							else
								type_ports_submodule.insert (
									container	=> segment.ports_submodules,
									new_item	=> (instance, port)); -- OSC1, clock_output

								log (text => " sits on segment -> inserted", level => log_threshold + 3);
							end if;
							
							-- signal iterations in upper levels to cancel
							port_processed := true;
						end if;
						
					end change_segment;

				begin -- query_segments
					log_indentation_up;

					-- On the first segment, where the port sits on, this loop ends prematurely.
					while not port_processed and segment_cursor /= type_net_segments.no_element loop
						log (text => "probing " & to_string (segment_cursor), level => log_threshold + 2);
						
						type_net_segments.update_element (
							container	=> strand.segments,
							position	=> segment_cursor,
							process		=> change_segment'access);

						next (segment_cursor);
					end loop;

					log_indentation_down;
				end query_segments;
				
			begin -- query_strands
				log_indentation_up;
				
				while not port_processed and strand_cursor /= type_strands.no_element loop

					-- We pick out only the strands on the targeted sheet:
					if et_coordinates.sheet (element (strand_cursor).position) = sheet (position) then
						log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);

						log_indentation_up;
						log (text => "strand " & to_string (position => element (strand_cursor).position),
							level => log_threshold + 1);

						update_element (
							container	=> net.strands,
							position	=> strand_cursor,
							process		=> query_segments'access);
					
						log_indentation_down;
					end if;
						
					next (strand_cursor);
				end loop;
				
				log_indentation_down;
			end query_strands;
			
		begin -- query_nets
			while not port_processed and net_cursor /= type_nets.no_element loop
				
				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);
			
				next (net_cursor);
			end loop;
		end query_nets;
		
	begin -- insert_port
		log (text => "inserting submodule port " & enclose_in_quotes (to_string (port)) & " in net at" & 
			 to_string (position => position) & " ...", level => log_threshold);
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;

	end insert_port;
	
	procedure add_port (
	-- Adds a port to a submodule instance (the box in the parent sheet).
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		port_name		: in type_net_name.bounded_string; -- clk_out
		position		: in type_point; -- x/y along the edge of the box
		
		direction		: in et_submodules.type_netchanger_port_name; -- master/slave. 
		-- NOTE: has nothing to do with direction of energy flow. It is relevant when 
		-- a netlist is exported. See specification et_submodules.type_submodule_port.
		
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_submodules;

		-- The place where the box is in the parent module:
		submodule_position : et_coordinates.type_position;

		-- Handling the absolute position of the port requires this variable:
		port_position : et_coordinates.type_position;
		
		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use type_submodules;
			submod_cursor : type_submodules.cursor;

			procedure query_ports (
				submod_name	: in et_general.type_module_instance_name.bounded_string;
				submodule	: in out type_submodule) is
				use type_submodule_ports;
				cursor : type_submodule_ports.cursor;
				inserted : boolean;
				port : type_submodule_port;
			begin
				-- Test whether the submodule provides the given port.
				if exists (
					module		=> submod_cursor,
					port 		=> port_name, -- clock_output
					direction	=> direction -- master/slave
					) then
				
					-- The given port position must be somewhere at the edge
					-- of the submodule. position is relative to the lower left
					-- corner of the box:
					if at_edge (position, submodule.size) then
						port.position := position;
					else
						port_not_at_edge (port_name);
					end if;

					-- set the naming direction of the port:
					port.direction := direction; -- master/slave 
					
					-- Insert the new port in the submodule:
					insert (
						container	=> submodule.ports,
						key			=> port_name,
						new_item	=> port,
						position	=> cursor,
						inserted	=> inserted);

					if not inserted then
						log (ERROR, "port " & 
							enclose_in_quotes (to_string (port_name)) &
							" already in submodule !", console => true);
						raise constraint_error;
					end if;

				else -- port not provided
					port_not_provided (port_name);
				end if;
					
			end query_ports;
			
		begin -- query_submodules
			if contains (module.submods, instance) then

				submod_cursor := find (module.submods, instance); -- the submodule should be there

				-- For inserting the submodule port in the nets
				-- we take a copy of the coordinates of the submodule (the box):
				submodule_position := element (submod_cursor).position;

				log_indentation_up;

 				-- insert the given port in the submodule
				update_element (
					container	=> module.submods,
					position	=> submod_cursor,
					process		=> query_ports'access);

				log_indentation_down;				
			else
				submodule_not_found (instance);
			end if;
		end query_submodules;

	begin -- add_port
		log (text => "module " & to_string (module_name) &
			" submodule instance " & enclose_in_quotes (to_string (instance)) & 
			" adding port " & enclose_in_quotes (to_string (port_name)) &
			" at" & to_string (position) &
			" direction" & to_string (direction),
			level => log_threshold);

		-- locate parent module
		module_cursor := locate_module (module_name);

		-- add the port to the box in the parent module
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_submodules'access);

		-- insert the submodule port in the nets:
		-- We know the absolute position of the box by submodule_position.
		-- We know the position of the port relative to the submodule_position
		-- which is in variable "position".

		-- Build the absoltue port_position:

		-- 1. assume x/y as given by position (which is the relative position):
		set (
			point		=> port_position,
			position	=> position); -- the relative port position

		-- 2. move port_position by x/y of submodule_position:
		move_by (
			point		=> port_position,
			offset		=> submodule_position);

		-- x/y of port_position is now absolute

		-- 3. set sheet number as given by submodule_position:
		set_sheet (
			position	=> port_position,
			sheet		=> sheet (submodule_position));

		-- port_position is now ready to insert the submodule port in the nets:
		insert_port (
			module			=> module_cursor,
			instance		=> instance,
			port			=> port_name,
			position		=> port_position,
			log_threshold	=> log_threshold + 1);
		
	end add_port;

	procedure delete_submodule_port (
	-- Removes a port from the net segments.
		module			: in pac_generic_modules.cursor;		-- the module
		port			: in et_schematic.type_port_submodule; -- OSC1 / clock_output
		position		: in et_coordinates.type_position; -- the submodule position (only sheet matters)
		log_threshold	: in type_log_level) is

		procedure query_nets (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is

			-- This flag goes true on the first match. It signals
			-- all iterations to cancel prematurely.
			port_processed : boolean := false;
			
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

					procedure change_segment (segment : in out type_net_segment) is
						use type_ports_submodule;
						port_cursor : type_ports_submodule.cursor;
					begin
						-- Search for the port and delete it if existing:
						port_cursor := find (
							container	=> segment.ports_submodules,
							item		=> port); -- OSC1, clock_output

						if port_cursor /= type_ports_submodule.no_element then
							delete (segment.ports_submodules, port_cursor);
							port_processed := true;
						end if;
						
					end change_segment;

				begin -- query_segments
					log_indentation_up;

					-- On the first segment, where the port sits on, this loop ends prematurely.
					while not port_processed and segment_cursor /= type_net_segments.no_element loop
						log (text => "probing " & to_string (segment_cursor), level => log_threshold + 2);
						
						type_net_segments.update_element (
							container	=> strand.segments,
							position	=> segment_cursor,
							process		=> change_segment'access);

						next (segment_cursor);
					end loop;

					log_indentation_down;
				end query_segments;
				
			begin -- query_strands
				log_indentation_up;
				
				while not port_processed and strand_cursor /= type_strands.no_element loop

					-- We pick out only the strands on the targeted sheet:
					if et_coordinates.sheet (element (strand_cursor).position) = sheet (position) then
						log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);

						log_indentation_up;
						log (text => "strand " & to_string (position => element (strand_cursor).position),
							level => log_threshold + 1);

						update_element (
							container	=> net.strands,
							position	=> strand_cursor,
							process		=> query_segments'access);
					
						log_indentation_down;
					end if;
						
					next (strand_cursor);
				end loop;
				
				log_indentation_down;
			end query_strands;
			
		begin -- query_nets
			while not port_processed and net_cursor /= type_nets.no_element loop
				
				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);
			
				next (net_cursor);
			end loop;

			-- CS warning if port_processed still false ?
		end query_nets;
		
	begin -- delete_submodule_port
		log (text => "deleting submodule port in nets ...", level => log_threshold);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;

	end delete_submodule_port;
	
	procedure delete_port (
	-- Deletes a port of a submodule instance (the box in the parent sheet).
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		port_name		: in type_net_name.bounded_string; -- clk_out
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_submodules;

		-- The place where the box is in the parent module:
		submodule_position : et_coordinates.type_position;
		
		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use type_submodules;
			submod_cursor : type_submodules.cursor;

			procedure query_ports (
				submod_name	: in et_general.type_module_instance_name.bounded_string;
				submodule	: in out type_submodule) is
				use type_submodule_ports;
				port_cursor : type_submodule_ports.cursor;
			begin
				-- Test whether the submodule provides the given port.
				port_cursor := find (submodule.ports, port_name);

				-- If the port is available (at the edge of the box) then
				-- it must be removed from the box.
				if port_cursor /= type_submodule_ports.no_element then
					delete (submodule.ports, port_cursor);
				else
					submodule_port_not_found (port_name);
				end if;
					
			end query_ports;
			
		begin -- query_submodules
			if contains (module.submods, instance) then

				submod_cursor := find (module.submods, instance); -- the submodule should be there

				-- For removing the submodule port from the nets
				-- we take a copy of the coordinates of the submodule (the box):
				submodule_position := element (submod_cursor).position;

				log_indentation_up;

 				-- insert the given port in the submodule
				update_element (
					container	=> module.submods,
					position	=> submod_cursor,
					process		=> query_ports'access);

				log_indentation_down;				
			else
				submodule_not_found (instance);
			end if;
		end query_submodules;

	begin -- delete_port
		log (text => "module " & to_string (module_name) &
			" submodule instance " & enclose_in_quotes (to_string (instance)) & 
			" deleting port " & enclose_in_quotes (to_string (port_name)),
			level => log_threshold);

		-- locate parent module
		module_cursor := locate_module (module_name);

		-- remove the port from the box in the parent module
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_submodules'access);

		-- now the port must be removed from the nets.
		delete_submodule_port (
			module			=> module_cursor,
			port			=> (instance, port_name), -- OSC1 / clock_output
			position		=> submodule_position, -- the submodule position (only sheet matters)
			log_threshold	=> log_threshold + 1);
		
	end delete_port;

	procedure move_port (
	-- Moves the given submodule port. Disconnects the port from
	-- start or end points of net segments BEFORE the move. 
	-- Connects submodule port with segment end or start points AFTER the move.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC
		port_name		: in et_general.type_net_name.bounded_string; -- clock_output
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level) is

		use et_submodules;

		-- The place where the box is in the parent module:
		submodule_position : et_coordinates.type_position;

		-- Handling the absolute position of the port requires this variable:
		port_position : et_coordinates.type_position;
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use type_submodules;
			submod_cursor : type_submodules.cursor;

			procedure query_ports (
				submod_name	: in et_general.type_module_instance_name.bounded_string;
				submodule	: in out type_submodule) is
				use type_submodule_ports;
				port_cursor : type_submodule_ports.cursor;

				procedure move (
					port_name	: in et_general.type_net_name.bounded_string;
					port		: in out type_submodule_port) is
					submod_pos_tmp : type_point := type_point (submodule_position);
					point_tmp : type_point := point;
				begin
					case coordinates is
						when ABSOLUTE =>
							-- From the given point the absolute submodule position must 
							-- be subtracted. This requires inversion of x/y of submodule position.
							-- We accompish that by mirroring along x and y axis.
							mirror (submod_pos_tmp, X);
							mirror (submod_pos_tmp, Y);

							-- Subtract from given point the absolute submodule position:
							move_by (
								point	=> point_tmp,
								offset	=> submod_pos_tmp);

							-- assign the new port position
							port.position := point_tmp;

						when RELATIVE =>
							move_by (
								point	=> port.position,
								offset	=> point);
							
					end case;
					
					-- The port must be somewhere at the edge of the box
					-- of the submodule. The port position is relative to 
					-- the lower left corner of the box:
					if at_edge (port.position, submodule.size) then

						-- Later, for inserting the new port in the nets the
						-- absolute port position must be built:
						port_position := to_position (
									point	=> port.position, -- relative x/y to submodule position
									sheet	=> sheet (submodule_position));

						move_by (
							point	=> port_position,
							offset	=> submodule_position);
						-- now port_position contains the absolute port position

					else
						port_not_at_edge (port_name);
					end if;
					
				end move;
								
			begin -- query_ports
				-- Test whether the submodule provides the given port.
				port_cursor := find (submodule.ports, port_name);

				-- If the port is available (at the edge of the box) then
				-- it can be moved:
				if port_cursor /= type_submodule_ports.no_element then

					update_element (
						container	=> submodule.ports,
						position	=> port_cursor,
						process		=> move'access);
					
				else
					submodule_port_not_found (port_name);
				end if;
					
			end query_ports;
			
		begin -- query_submodules
			if contains (module.submods, instance) then

				submod_cursor := find (module.submods, instance); -- the submodule should be there

				-- For moving the submodule port
				-- we take a copy of the coordinates of the submodule (the box):
				submodule_position := element (submod_cursor).position;

				log_indentation_up;

				update_element (
					container	=> module.submods,
					position	=> submod_cursor,
					process		=> query_ports'access);

				log_indentation_down;				
			else
				submodule_not_found (instance);
			end if;
		end query_submodules;
		
	begin -- move_port
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" moving port " & enclose_in_quotes (to_string (port_name)) &
					" to" & to_string (point),
					level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" moving port " & enclose_in_quotes (to_string (port_name)) &
					" by" & to_string (point),
					level => log_threshold);

		end case;

		-- locate module
		module_cursor := locate_module (module_name);

		-- move the port along the edge of the box:
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_submodules'access);

		-- now the old port must be removed from the nets.
		delete_submodule_port (
			module			=> module_cursor,
			port			=> (instance, port_name), -- OSC1 / clock_output
			position		=> submodule_position, -- the submodule position (only sheet matters)
			log_threshold	=> log_threshold + 1);

		-- Now, port_position contains the new absolute port position in the schematic.
		-- So we insert the new submodule port in the net segments:
		insert_port (
			module			=> module_cursor,
			instance		=> instance,
			port			=> port_name,
			position		=> port_position,
			log_threshold	=> log_threshold + 1);
		
	end move_port;

	procedure drag_net_segments (
	-- Drags the net segments according to the given submodule ports.
	-- Changes the position of start or end points of segments.
	-- Does NOT create new connections with segments if the port
	-- lands on the start or end point of another segment.
	-- Does NOT create a new connection with segments if the port
	-- lands between start and end point.
		module			: in pac_generic_modules.cursor;				-- the module
		port			: in et_schematic.type_port_submodule;	-- instance and port name
		pos_before		: in et_coordinates.type_position;	-- the old port position
		pos_after		: in et_coordinates.type_position;	-- the new port position
		log_threshold	: in type_log_level) is

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

				-- This flag goes true once the port has been found the first time
				-- and affected end points of segments have been moved.
				drag_processed : boolean := false;
				
				procedure query_segments (strand : in out type_strand) is
					use type_net_segments;

					segment_cursor : type_net_segments.cursor := strand.segments.first;

					procedure change_segment (segment : in out type_net_segment) is 
					-- Changes the position of start or end point of a segment according to the drag point.
					begin -- change_segment
						log_indentation_up;
						
						-- if port sits on a start point of a segment -> move start point
						if segment.start_point = type_point (pos_before) then
							log (text => "move segment start point from" & 
								to_string (segment.start_point),
								level => log_threshold + 3);

							segment.start_point := type_point (pos_after);

							log (text => "to" & 
								to_string (segment.start_point),
								level => log_threshold + 3);

							-- signal iterations in upper level to cancel
							drag_processed := true;
						end if;

						-- if port sits on an end point of a segment -> move end point
						if segment.end_point = type_point (pos_before) then
							log (text => "move segment end point from" & 
								to_string (segment.end_point),
								level => log_threshold + 3);

							segment.end_point := type_point (pos_after);

							log (text => "to" & 
								to_string (segment.end_point),
								level => log_threshold + 3);
							
							-- signal iterations in upper level to cancel
							drag_processed := true;
						end if;
						
						log_indentation_down;
					end change_segment;
					
				begin -- query_segments
					log_indentation_up;

					-- Probe all segments of strand for pos_before. This loop must not
					-- abort even if drag_processed goes true. Reason: All segements
					-- meeting here must be dragged.
					while segment_cursor /= type_net_segments.no_element loop

						log (text => "probing " & to_string (segment_cursor), level => log_threshold + 2);
						
						update_element (
							container	=> strand.segments,
							position	=> segment_cursor,
							process		=> change_segment'access);
						
						next (segment_cursor);
					end loop;

					-- Update strand position if any movement took place.
					if drag_processed then
						set_strand_position (strand);
					end if;
					
					log_indentation_down;
				end query_segments;
				
			begin -- query_strands
				log_indentation_up;
				while strand_cursor /= type_strands.no_element loop
					
					-- We pick out only the strands on the targeted sheet:
					if et_coordinates.sheet (element (strand_cursor).position) = sheet (pos_before) then
						log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);

						log_indentation_up;
						log (text => "strand " & to_string (position => element (strand_cursor).position),
							level => log_threshold + 1);
					
						-- Iterate in segments of strand. If point sits on any segment
						-- the flag drag_processed goes true.
						update_element (
							container	=> net.strands,
							position	=> strand_cursor,
							process		=> query_segments'access);
					
						log_indentation_down;
					end if;
					-- All segments of strand probed (and maybe moved).

					-- If the drag point has been processed, there is no need to look up
					-- other strands for port_before.
					if drag_processed then exit; end if;
					
					next (strand_cursor);
				end loop;

				log_indentation_down;
			end query_strands;
			
		begin -- query_nets
			while net_cursor /= type_nets.no_element loop

				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);

				next (net_cursor);
			end loop;
		end query_nets;

	begin -- drag_net_segments
		log (text => "dragging net segments with submodule ports on sheet" & 
			 to_sheet (sheet (pos_before)) & " ...", level => log_threshold);
		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);
		
		log_indentation_down;
	end drag_net_segments;

	procedure movable_test (
	-- Tests whether the submodule port at the given point is movable. The criteria
	-- for movement are: no device, no netchanger ports there.
	-- The ONE and ONLY port allowed here is the port-to-be-dragged itself.
		module_cursor	: in pac_generic_modules.cursor;
		instance		: in et_general.type_module_instance_name.bounded_string;
		port_name		: in et_general.type_net_name.bounded_string;
		point 			: in et_coordinates.type_position;
		log_threshold	: in et_string_processing.type_log_level) is 
		ports : type_ports;
		port : et_schematic.type_port_submodule;

		use type_ports_submodule;
		use type_ports_device;

		use et_netlists;
		use type_ports_netchanger;
		
	begin -- movable_test
		log (text => "movable test ...", level => log_threshold);
		log_indentation_up;

		-- If no net segments start or end at given point then this test won't
		-- complain. If segments are meeting this point, no other ports must be
		-- here (except the port-to-be-dragged):
		if net_segment_at_place (module_cursor, point) then

			-- There are net segments starting or ending at point.
			-- Make sure at point are no ports of devices, netchangers or other 
			-- submodules (except the submodule port to be dragged):

			port := (instance, port_name); -- the port to be dragged, like instance OSC port 'clock_out'

			-- Collect all ports of possible other devices, submodules and netchangers
			-- at given point:
			ports := ports_at_place (key (module_cursor), point, log_threshold + 1);

			-- If no device or netchanger ports here:
			if is_empty (ports.devices) and is_empty (ports.netchangers) then

				-- If the ONE and ONLY submodule port is the 
				-- port-to-be-dragged then everything is fine.
				if length (ports.submodules) = 1 then
					
					if contains (ports.submodules, port) then
						null; -- fine -> movable test passed
					else
						-- there is another submodule port
						dragging_not_possible (to_string (port.port_name), point);
					end if;
				
				else
					-- there are more submodule ports
					dragging_not_possible (to_string (port.port_name), point);
				end if;
				
			else -- device or netchanger ports here
				dragging_not_possible (to_string (port.port_name), point);
			end if;
		end if;
		
		log_indentation_down;
	end movable_test;
	
	procedure drag_port (
	-- Drags the given submodule port along the edge of the box.
	-- Already existing connections with net segments are kept.
	-- Net segment positions are modified.
	-- This operation applies to a single sheet. Dragging from one sheet
	-- to another is not possible.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC
		port_name		: in et_general.type_net_name.bounded_string; -- clock_output
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level) is

		use et_submodules;

		-- The place where the box is in the parent module:
		submodule_position : et_coordinates.type_position;

		-- Handling the absolute position of the port requires these variables:
		port_position_before : et_coordinates.type_position;
		port_position_after  : et_coordinates.type_position;
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified
	
		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use type_submodules;
			submod_cursor : type_submodules.cursor;

			procedure query_ports (
				submod_name	: in et_general.type_module_instance_name.bounded_string;
				submodule	: in out type_submodule) is
				use type_submodule_ports;
				port_cursor : type_submodule_ports.cursor;

				procedure move (
					port_name	: in et_general.type_net_name.bounded_string;
					port		: in out type_submodule_port) is
					submod_pos_tmp : type_point := type_point (submodule_position);
					point_tmp : type_point := point;
				begin
					-- BACKUP THE PORT POSITION BEFORE THE DRAG OPERATION:
					port_position_before := to_position (
								point	=> port.position, -- relative x/y to submodule position
								sheet	=> sheet (submodule_position)); -- same sheet as submodule box

					move_by (
						point	=> port_position_before,
						offset	=> submodule_position);
					-- Now port_position_before contains the absolute port position of 
					-- the port BEFORE the drag operation.

					-- Test whether the port at the current position can be dragged:
					movable_test (
						module_cursor		=> module_cursor,
						instance			=> instance,
						port_name			=> port_name,
						point				=> port_position_before,
						log_threshold		=> log_threshold + 1);

					-- move port along edge of box
					case coordinates is
						when ABSOLUTE =>
							-- From the given point the absolute submodule position must 
							-- be subtracted. This requires inversion of x/y of submodule position.
							-- We accompish that by mirroring along x and y axis.
							mirror (submod_pos_tmp, X);
							mirror (submod_pos_tmp, Y);

							-- Subtract from given point the absolute submodule position:
							move_by (
								point	=> point_tmp,
								offset	=> submod_pos_tmp);

							-- assign the new port position
							port.position := point_tmp;

						when RELATIVE =>
							move_by (
								point	=> port.position,
								offset	=> point);
							
					end case;
					
					-- The port must be somewhere at the edge of the box
					-- of the submodule. The port position is relative to 
					-- the lower left corner of the box:
					if at_edge (port.position, submodule.size) then

						-- Later, for inserting the new port in the nets the
						-- absolute port position must be built:
						port_position_after := to_position (
									point	=> port.position, -- relative x/y to submodule position
									sheet	=> sheet (submodule_position)); -- same sheet as submodule box

						move_by (
							point	=> port_position_after,
							offset	=> submodule_position);
						-- Now port_position_after contains the absolute port position of 
						-- the port AFTER the drag operation.

					else
						port_not_at_edge (port_name);
					end if;
					
				end move;
								
			begin -- query_ports
				-- Test whether the submodule provides the given port.
				port_cursor := find (submodule.ports, port_name);

				-- If the port is available (at the edge of the box) then
				-- it can be moved:
				if port_cursor /= type_submodule_ports.no_element then
									
					update_element (
						container	=> submodule.ports,
						position	=> port_cursor,
						process		=> move'access);
					
				else
					submodule_port_not_found (port_name);
				end if;
					
			end query_ports;
			
		begin -- query_submodules
			if contains (module.submods, instance) then

				submod_cursor := find (module.submods, instance); -- the submodule should be there

				-- For moving the submodule port
				-- we take a copy of the coordinates of the submodule (the box):
				submodule_position := element (submod_cursor).position;

				log_indentation_up;

				update_element (
					container	=> module.submods,
					position	=> submod_cursor,
					process		=> query_ports'access);

				log_indentation_down;				
			else
				submodule_not_found (instance);
			end if;
		end query_submodules;
		
	begin -- drag_port
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" dragging port " & enclose_in_quotes (to_string (port_name)) &
					" to" & to_string (point),
					level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" dragging port " & enclose_in_quotes (to_string (port_name)) &
					" by" & to_string (point),
					level => log_threshold);

		end case;

		-- locate module
		module_cursor := locate_module (module_name);

		-- move the port along the edge of the box:
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_submodules'access);

		drag_net_segments (
			module			=> module_cursor,
			port			=> (instance, port_name),
			pos_before		=> port_position_before,
			pos_after		=> port_position_after,
			log_threshold	=> log_threshold + 1);
		
	end drag_port;

	procedure delete_ports (
	-- Deletes all references to the given submodule in the nets.
		module_cursor	: in pac_generic_modules.cursor;					-- the module
		instance		: in et_general.type_module_instance_name.bounded_string; -- the submodule instance
		position		: in et_coordinates.type_position; 		-- the location in the schematic (only sheet matters)
		log_threshold	: in type_log_level) is

		procedure query_nets (
		-- Removes all references to the submodule instance from the net segments.
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

					procedure change_segment (segment : in out type_net_segment) is
						use type_module_instance_name;
						use type_ports_submodule;
						port_cursor : type_ports_submodule.cursor := segment.ports_submodules.first;
					begin
						while port_cursor /= type_ports_submodule.no_element loop
							if element (port_cursor).module_name = instance then -- OSC1
								delete (segment.ports_submodules, port_cursor);
							end if;
							next (port_cursor);
						end loop;
					end change_segment;

				begin -- query_segments
					log_indentation_up;

					while segment_cursor /= type_net_segments.no_element loop
						log (text => "probing " & to_string (segment_cursor), level => log_threshold + 2);
						
						type_net_segments.update_element (
							container	=> strand.segments,
							position	=> segment_cursor,
							process		=> change_segment'access);

						next (segment_cursor);
					end loop;

					log_indentation_down;
				end query_segments;
				
			begin -- query_strands
				log_indentation_up;
				
				while strand_cursor /= type_strands.no_element loop

					-- We pick out only the strands on the targeted sheet:
					if et_coordinates.sheet (element (strand_cursor).position) = sheet (position) then
						log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);

						log_indentation_up;
						log (text => "strand " & to_string (position => element (strand_cursor).position),
							level => log_threshold + 1);

						update_element (
							container	=> net.strands,
							position	=> strand_cursor,
							process		=> query_segments'access);
					
						log_indentation_down;
					end if;
						
					next (strand_cursor);
				end loop;
				
				log_indentation_down;
			end query_strands;
			
		begin -- query_nets
			while net_cursor /= type_nets.no_element loop
				
				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);
			
				next (net_cursor);
			end loop;
		end query_nets;
		
	begin -- delete_ports
		log (text => "deleting submodule ports in nets ...", level => log_threshold);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_nets'access);

	end delete_ports;
	
	procedure delete_submodule (
	-- Removes a submodule instance from the schematic.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		log_threshold	: in type_log_level) is

		use et_submodules;

		-- The place where the box is in the parent module:
		submodule_position : et_coordinates.type_position;
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use type_submodules;
			submod_cursor : type_submodules.cursor;
		begin -- query_submodules
			if contains (module.submods, instance) then

				submod_cursor := find (module.submods, instance); -- the submodule should be there

				-- For removing the submodule ports
				-- we take a copy of the coordinates of the submodule (the box):
				submodule_position := element (submod_cursor).position;

				-- delete the submodule (the box)
				delete (module.submods, submod_cursor);
			else
				submodule_not_found (instance);
			end if;
		end query_submodules;
	
	begin -- delete_submodule
		log (text => "module " & to_string (module_name) &
			" deleting submodule instance " & enclose_in_quotes (to_string (instance)),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- load submodule_position and delete submodule
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_submodules'access);

		-- delete all references to the submodule in the nets
		delete_ports (
			module_cursor	=> module_cursor,
			instance		=> instance,
			position		=> submodule_position,
			log_threshold	=> log_threshold + 1);
		
	end delete_submodule;

	procedure move_submodule (
	-- Moves the given submodule instance (the box). Disconnects the ports from
	-- start or end points of net segments BEFORE the move. 
	-- Connects submodule ports with segment end or start points AFTER the move.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level) is

		use et_submodules;

		-- The place where the box is in the parent module BEFORE and AFTER the move:
		submodule_position_before : et_coordinates.type_position;
		submodule_position_after : et_coordinates.type_position;		
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use type_submodules;
			submod_cursor : type_submodules.cursor;

			-- the submodule ports to be moved
			ports : type_submodule_ports.map; -- port names and relative x/y positions

			procedure move (
				instance	: in et_general.type_module_instance_name.bounded_string;
				submodule	: in out type_submodule) is
			begin
				case coordinates is
					when ABSOLUTE =>
						submodule.position := to_position (point, sheet);

					when RELATIVE =>
						move (
							position	=> submodule.position,
							offset		=> to_position_relative (point, sheet)
							);
				end case;

				-- store new submodule position
				submodule_position_after := submodule.position;

				exception
					when event: others =>
						log (ERROR, "coordinates invalid !", console => true); -- CS required more details
						log (text => ada.exceptions.exception_information (event), console => true);
						raise;
				
			end move;

			procedure insert_ports is 
			-- Inserts the ports into the nets. The sheet number is taken
			-- from the submodule_position_after (or submodule_position_before).
				use type_submodule_ports;
				port_cursor : type_submodule_ports.cursor := ports.first;
				position : et_coordinates.type_position;
			begin
				while port_cursor /= type_submodule_ports.no_element loop

					-- build the port position (sheet/x/y)
					position := to_position 
							(
							point	=> element (port_cursor).position,
							sheet	=> et_coordinates.sheet (submodule_position_after)
							);

					-- insert the port
					insert_port (
						module			=> module_cursor,
						instance		=> instance, -- OSC1
						port			=> key (port_cursor), -- port name like CE
						position		=> position, -- sheet/x/y
						log_threshold	=> log_threshold + 1);
					
					next (port_cursor);
				end loop;
			end insert_ports;
			
		begin -- query_submodules
			if contains (module.submods, instance) then

				submod_cursor := find (module.submods, instance); -- the submodule should be there

				-- For moving the submodule ports
				-- we take a copy of the coordinates of the submodule (the box)
				-- BEFORE the move operation:
				submodule_position_before := element (submod_cursor).position;

				log_indentation_up;
				
				-- delete all references to the submodule in the nets
				delete_ports (
					module_cursor	=> module_cursor,
					instance		=> instance,
					position		=> submodule_position_before, -- only sheet number matters
					log_threshold	=> log_threshold + 1);

				-- move the submodule (the box). Load submodule_position_after
				-- with the coordinates AFTER the move operation:
				update_element (
					container	=> module.submods,
					position	=> submod_cursor,
					process		=> move'access);
				
				-- Get the port positions relative to the lower left 
				-- corner of the submodule box.
				ports := element (submod_cursor).ports;

				-- calculate the absolute port positions AFTER the move:
				et_submodules.move_ports (ports, submodule_position_after);

				-- ports now provides port names and absoltue x/y positions.
				-- The new ports will be inserted in the nets now:
				insert_ports;

				log_indentation_down;				
			else
				submodule_not_found (instance);
			end if;
		end query_submodules;
		
	begin -- move_submodule
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" moving submodule instance " & to_string (instance) &
					" to sheet" & to_sheet (sheet) &
					to_string (point), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" moving submodule instance " & to_string (instance) &
					" by " & to_sheet_relative (sheet) & " sheet(s)" &
					to_string (point), level => log_threshold);
		end case;
		
		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_submodules'access);
		
	end move_submodule;
	
	procedure drag_submodule (
	-- Drags the given submodule instance (the box) within the schematic.
	-- Already existing connections with net segments are kept.
	-- Net segment positions are modified.
	-- This operation applies to a single sheet. Dragging from one sheet
	-- to another is not possible.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module being modified
		
		use et_submodules;

		-- This type describes a submodule port before and after the drag operation:
		type type_drag is record
			name	: et_general.type_net_name.bounded_string;
			before	: et_coordinates.type_position;
			after 	: et_coordinates.type_position;
		end record;

		-- Since there are lots of submodule ports we store the drag points in a simple list:
		package type_drags is new doubly_linked_lists (type_drag);
		drag_list : type_drags.list;

		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use type_submodules;
			submod_cursor : type_submodules.cursor;

			-- the submodule ports to be moved
			use type_submodule_ports;
			ports : type_submodule_ports.map; -- port names and relative x/y positions
			port_cursor : type_submodule_ports.cursor := ports.first;

			procedure query_ports (
				submod_name	: in et_general.type_module_instance_name.bounded_string;
				submodule	: in type_submodule) is
				use type_submodule_ports;
				port_cursor : type_submodule_ports.cursor := submodule.ports.first;

				procedure build_drag_point (
					port_name	: in et_general.type_net_name.bounded_string;
					port		: in type_submodule_port) is
					drag : type_drag;
				begin
					-- Set the name of the drag according to the port name:
					drag.name := port_name; -- CE, WE, ...
					
					-- Build and the absolute port position BEFORE the drag operation.
					-- The result is stored in drag.before.
					drag.before := submodule.position;

					move_by (
						point	=> drag.before,
						offset	=> port.position);
			
					-- Now drag.before contains the absolute port position of 
					-- the port BEFORE the drag operation.

					-- Test whether the port at the current position can be dragged:
					movable_test (
						module_cursor		=> module_cursor,
						instance			=> instance,
						port_name			=> port_name,
						point				=> drag.before,
						log_threshold		=> log_threshold + 1);

					-- Compute the absolute port position on the sheet AFTER the drag operation.
					-- The result is stored in drag.after:
					case coordinates is
						when ABSOLUTE =>

							drag.after := to_position (
								point	=> point,
								sheet	=> sheet (submodule.position));

						when RELATIVE =>

							drag.after := submodule.position;

							move_by (
								point	=> drag.after,
								offset	=> point);

					end case;

					move_by (
						point	=> drag.after,
						offset	=> port.position);

					-- Now drag.after contains the absolute port position of 
					-- the port AFTER the drag operation.
					
					type_drags.append (drag_list, drag);
					
				end build_drag_point;
				
			begin -- query_ports
				while port_cursor /= type_submodule_ports.no_element loop

					query_element (
						position	=> port_cursor,
						process		=> build_drag_point'access);
										
					next (port_cursor);
				end loop;
			end query_ports;

			procedure move_box (
			-- Moves the box on the sheet according to given target position.
				submod_name	: in et_general.type_module_instance_name.bounded_string;
				submodule	: in out type_submodule) is
			begin
				-- NOTE: The sheet number does not change in drag operations.
				case coordinates is
					when ABSOLUTE =>
						set (submodule.position, point);

					when RELATIVE =>
						move_by (
							point	=> submodule.position,
							offset	=> point
							);
				end case;

				exception
					when event: others =>
						log (ERROR, "coordinates invalid !", console => true); -- CS required more details
						log (text => ada.exceptions.exception_information (event), console => true);
						raise;

			end move_box;
			
		begin -- query_submodules
			if contains (module.submods, instance) then

				submod_cursor := find (module.submods, instance); -- the submodule should be there

				log_indentation_up;

				-- build the drag list (movable_test included)
				query_element (
					position	=> submod_cursor,
					process		=> query_ports'access);
				
				-- move the submodule (the box):
				update_element (
					container	=> module.submods,
					position	=> submod_cursor,
					process		=> move_box'access);

				log_indentation_down;				
			else
				submodule_not_found (instance);
			end if;
		end query_submodules;

		procedure drag_segments is
		-- Drags the net segments according to the drag_list that has been
		-- created earlier.
			use type_drags;
			drag_cursor : type_drags.cursor := drag_list.first;
		begin
			while drag_cursor /= type_drags.no_element loop

				drag_net_segments (
					module			=> module_cursor,
					port			=> (instance, element (drag_cursor).name),
					pos_before		=> element (drag_cursor).before,
					pos_after		=> element (drag_cursor).after,
					log_threshold	=> log_threshold + 1);
				
				next (drag_cursor);
				
			end loop;
		end drag_segments;
		
	begin -- drag_submodule
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" dragging submodule instance " & to_string (instance) &
					" to" & to_string (point), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" dragging submodule instance " & to_string (instance) &
					" by " & to_string (point), level => log_threshold);
		end case;
		
		-- locate module
		module_cursor := locate_module (module_name);

		-- move the ports of the submodule,
		-- create drag_list
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_submodules'access);

		-- drag the connected net segments (by drag_list)
		drag_segments;

	end drag_submodule;

	procedure copy_submodule (
	-- Copies a submodule instance.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance_origin	: in et_general.type_module_instance_name.bounded_string; -- OSC1
		instance_new	: in et_general.type_module_instance_name.bounded_string; -- CLOCK_GENERATOR
		destination		: in et_coordinates.type_position; -- sheet/x/y
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module being modified
		
		use et_submodules;

		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use type_submodules;
			submod_cursor : type_submodules.cursor;
			inserted : boolean;
			submodule : type_submodule;

			-- the submodule ports to be inserted in the nets
			ports : type_submodule_ports.map; -- port names and relative x/y positions

			procedure insert_ports is 
			-- Inserts the ports into the nets. The sheet number is taken
			-- from the submodule position.
				use type_submodule_ports;
				port_cursor : type_submodule_ports.cursor := ports.first;
				position : et_coordinates.type_position;
			begin
				while port_cursor /= type_submodule_ports.no_element loop

					-- build the port position (sheet/x/y)
					position := to_position 
							(
							point	=> element (port_cursor).position,
							sheet	=> et_coordinates.sheet (element (submod_cursor).position)
							);

					-- insert the port
					insert_port (
						module			=> module_cursor,
						instance		=> instance_new, -- CLOCK_GENERATOR
						port			=> key (port_cursor), -- port name like CE
						position		=> position, -- sheet/x/y
						log_threshold	=> log_threshold + 1);
					
					next (port_cursor);
				end loop;
			end insert_ports;
			
		begin -- query_submodules
			-- locate the submodule of origin
			if contains (module.submods, instance_origin) then

				submod_cursor := find (module.submods, instance_origin); -- the submodule of origin should be there

				-- THE FOLLOWING IS ABOUT THE GRAPHICAL REPRESENTATION OF A SUBMODULE.
				-- THIS IS THE RECTANGULAR BOX AT THE SHEET WHERE IT THE SUBMODULE IS INSTANTIATED.

				-- copy submodule of origin to temporarily submodule
				submodule := element (submod_cursor); 
 
				-- overwrite position as given by destination
				submodule.position := destination;

				-- Overwrite position in schematic by zero so that the new instance sits at 
				-- the lower left corner of the layout drawing:
				submodule.position_in_board := et_pcb_coordinates.pac_geometry_brd.origin_zero_rotation;

				insert (
					container	=> module.submods,
					key			=> instance_new,
					position	=> submod_cursor,
					inserted	=> inserted,
					new_item	=> submodule);
					
				if not inserted then
					log (ERROR, "submodule instance " &
						enclose_in_quotes (to_string (instance_new)) &
						" already exists !", console => true);
					raise constraint_error;
				end if;
				
				log_indentation_up;

				-- Get the port positions of the new instance relative to the lower left 
				-- corner of the submodule box.
				ports := element (submod_cursor).ports;

				-- calculate the absolute port positions:
				et_submodules.move_ports (ports, element (submod_cursor).position);

				-- ports now provides port names and absoltue x/y positions.
				-- The new ports will be inserted in the nets now:
				insert_ports;

				log_indentation_down;				
			else
				submodule_not_found (instance_origin);
			end if;
		end query_submodules;
		
	begin -- copy_submodule
		log (text => "module " & to_string (module_name) &
			 " copying submodule instance " & enclose_in_quotes (to_string (instance_origin)) &
			 " to instance " & enclose_in_quotes (to_string (instance_new)) &
			" at" & et_coordinates.to_string (position => destination), level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_submodules'access);
		
	end copy_submodule;

	procedure rename_submodule (
	-- Renames a submodule instance.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance_old	: in et_general.type_module_instance_name.bounded_string; -- OSC1
		instance_new	: in et_general.type_module_instance_name.bounded_string; -- CLOCK_GENERATOR
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module being modified
		
		use et_submodules;

		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use type_submodules;
			submod_cursor : type_submodules.cursor;
			submodule_old : type_submodule;

			-- the submodule ports to be inserted in the nets
			ports : type_submodule_ports.map; -- port names and relative x/y positions

			procedure insert_ports is 
			-- Inserts the ports into the nets. The sheet number is taken
			-- from the submodule position.
				use type_submodule_ports;
				port_cursor : type_submodule_ports.cursor := submodule_old.ports.first;
				position : et_coordinates.type_position;
			begin
				while port_cursor /= type_submodule_ports.no_element loop

					-- build the port position (sheet/x/y)
					position := to_position 
							(
							point	=> element (port_cursor).position,
							sheet	=> et_coordinates.sheet (submodule_old.position)
							);

					-- insert the port
					insert_port (
						module			=> module_cursor,
						instance		=> instance_new, -- CLOCK_GENERATOR
						port			=> key (port_cursor), -- port name like CE
						position		=> position, -- sheet/x/y
						log_threshold	=> log_threshold + 1);
					
					next (port_cursor);
				end loop;
			end insert_ports;
			
		begin -- query_submodules
			-- locate the submodule to be renamed
			if contains (module.submods, instance_old) then
				submod_cursor := find (module.submods, instance_old); -- the submodule should be there

				-- take a copy of the old submodule
				submodule_old := element (submod_cursor);

				-- insert the old module with the new name in the module list
				insert (
					container	=> module.submods,
					key			=> instance_new,
					new_item	=> submodule_old);

				-- delete all references to the old submodule in the nets
				delete_ports (
					module_cursor	=> module_cursor,
					instance		=> instance_old,
					position		=> submodule_old.position,
					log_threshold	=> log_threshold + 1);

				-- calculate the absolute port positions:
				et_submodules.move_ports (submodule_old.ports, submodule_old.position);
				
				-- submodule_old.ports provides port names and absolute x/y positions.
				-- The new ports will be inserted in the nets now:
				insert_ports;

				-- delete the old submodule in the module list
				delete (
					container	=> module.submods,
					position	=> submod_cursor);

			else
				submodule_not_found (instance_old);
			end if;
		end query_submodules;
		
	begin -- rename_submodule
		log (text => "module " & to_string (module_name) &
			 " renaming submodule instance " & enclose_in_quotes (to_string (instance_old)) &
			 " to " & to_string (instance_new),
			 level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- The new name must not be in use already:
		if exists (module_cursor, instance_new) then
			log (ERROR, "submodule instance " & enclose_in_quotes (to_string (instance_new)) &
				 " already exists !", console => true);
			raise constraint_error;
		else
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> query_submodules'access);

		end if;
		
	end rename_submodule;
	
	procedure set_submodule_file (
	-- Sets the file name of a submodule instance.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		file			: in et_submodules.type_submodule_path.bounded_string; -- the file name of the submodule like templates/oscillator.mod
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		full_file_name : constant string := expand (et_submodules.to_string (file));
		
		use et_submodules;

		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use type_submodules;
			submod_cursor : type_submodules.cursor;

			procedure set_file (
				submod_name	: in et_general.type_module_instance_name.bounded_string;
				submodule	: in out type_submodule) is

				-- Prior to assigning the file, we create a test submodule and test
				-- whether it provides all the ports as required by the graphical 
				-- representation of the submodule (the box). 
				-- The test module is a copy of the targeted submodule except that it
				-- get the given file assigned. The test submodule will then be stored in a
				-- map and will be the only item in the map:
				test_mods : type_submodules.map;
				test_mod : type_submodule := submodule;
				test_mod_cursor : type_submodules.cursor;

				-- For iterating the ports of the submodule box, we need a cursor:
				use type_submodule_ports;
				port_cursor : type_submodule_ports.cursor := submodule.ports.first;

			begin -- set_file
				log (text => "testing ports ...", level => log_threshold + 1);
				log_indentation_up;
				
				test_mod.file := file; -- assign the file to the test submodule

				-- insert the test submodule in map test_mods:
				insert (
					container	=> test_mods,
					key			=> instance,
					new_item	=> test_mod);

				test_mod_cursor := test_mods.first;

				-- Test ports of targeted submodule whether they are provided by
				-- the test module (indicated by test_mod_cursor). If all ports
				-- have been found in the submodule schematic, overwrite the given 
				-- submodule with the test module.
				while port_cursor /= type_submodule_ports.no_element loop
					log (text => to_string (key (port_cursor)), level => log_threshold + 2);

					if not exists (
						module		=> test_mod_cursor,
						port 		=> key (port_cursor), -- clock_output
						direction	=> element (port_cursor).direction -- master/slave
						) then
						
 						port_not_provided (key (port_cursor));
					end if;
					
					next (port_cursor);
				end loop;

				log_indentation_down;

				-- Overwrite submodule with test module:
				submodule := test_mod;
				
				exception
					when event: others =>
						log_indentation_reset;
						log (text => ada.exceptions.exception_information (event), console => true);
						raise;
				
			end;
				
		begin -- query_submodules
			if contains (module.submods, instance) then

				submod_cursor := find (module.submods, instance); -- the submodule should be there
				
				update_element (
					container	=> module.submods,
					position	=> submod_cursor,
					process		=> set_file'access);
				
			else
				submodule_not_found (instance);
			end if;

		end query_submodules;
		
	begin -- set_submodule_file
		log (text => "module " & to_string (module_name) &
			" setting instance " & enclose_in_quotes (to_string (instance)) &
			" file to " & to_string (file),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- Make sure the submodule file exists. The file is 
		-- identified by its full path and name. If the file exists
		-- then the given submodule instance gets the file assigned.
		-- NOTE: This is the rectangular box at the targeted sheet that
		-- represents the submodule:
		if ada.directories.exists (full_file_name) then

			-- THIS IS ABOUT THE ACTUAL SCHEMATIC AND LAYOUT STUFF OF THE SUBMODULE:
			-- Read the submodule file and store its content in container et_project.modules:
			read_module (to_string (file), log_threshold + 1);		
			
			log_indentation_up;

			-- THIS IS ABOUT THE GRAPHICAL REPRESENTATION OF THE SUBMODULE:
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> query_submodules'access);

			log_indentation_down;
		else
			log (ERROR, "submodule file " & to_string (file) & " not found !",
				 console => true);
			raise constraint_error;
		end if;
		
	end set_submodule_file;

	procedure create_assembly_variant (
	-- Creates a new assembly variant.
		module_name		: in type_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in et_general.type_variant_name.bounded_string; -- low_cost
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;

		procedure create (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			inserted : boolean;
			cursor : et_assembly_variants.pac_variants.cursor;
		begin
			-- create the variant
			et_assembly_variants.pac_variants.insert (
				container	=> module.variants,
				key			=> variant_name,
				position	=> cursor,
				inserted	=> inserted);

			if not inserted then
				log (ERROR, "assembly variant " & enclose_in_quotes (to_variant (variant_name)) &
					 " already exists !", console => true);
				raise constraint_error;
			end if;
		end create;
			
	begin -- create_assembly_variant
		log (text => "module " & to_string (module_name) &
			" creating new assembly variant " & enclose_in_quotes (to_variant (variant_name)),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> create'access);
		
	end create_assembly_variant;
	
	procedure delete_assembly_variant (
	-- Deletes an assembly variant.
		module_name		: in type_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in et_general.type_variant_name.bounded_string; -- low_cost
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;

		procedure delete (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_assembly_variants.pac_variants;
			cursor : et_assembly_variants.pac_variants.cursor;
		begin
			-- before deleting, the variant must be located
			cursor := find (module.variants, variant_name);

			if cursor /= et_assembly_variants.pac_variants.no_element then
				
				delete (
					container	=> module.variants,
					position	=> cursor);

			else
				assembly_variant_not_found (variant_name);
			end if;
		end delete;
		
	begin -- delete_assembly_variant
		log (text => "module " & to_string (module_name) &
			" deleting assembly variant " & enclose_in_quotes (to_variant (variant_name)),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> delete'access);
		
	end delete_assembly_variant;

	procedure describe_assembly_variant (
	-- Describes an assembly variant. Overwrites the previous description.
		module_name		: in type_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in et_general.type_variant_name.bounded_string; -- low_cost											
		description		: in et_assembly_variants.type_description; -- "this is the low budget variant"
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;

		procedure describe (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_assembly_variants.pac_variants;
			cursor : et_assembly_variants.pac_variants.cursor;

			procedure assign_description (
				name		: in et_general.type_variant_name.bounded_string;
				variant		: in out et_assembly_variants.type_variant) is
			begin
				variant.description := description;
			end assign_description;
			
		begin -- describe
			-- before describing, the variant must be located
			cursor := et_assembly_variants.pac_variants.find (module.variants, variant_name);

			if cursor /= et_assembly_variants.pac_variants.no_element then

				et_assembly_variants.pac_variants.update_element (
					container	=> module.variants,
					position	=> cursor,
					process		=> assign_description'access);

			else
				assembly_variant_not_found (variant_name);
			end if;

		end describe;

	begin -- describe_assembly_variant
		log (text => "module " & to_string (module_name) &
			 " variant " & enclose_in_quotes (to_variant (variant_name)) &
			 " description " & enclose_in_quotes (to_string (description)),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> describe'access);
		
	end describe_assembly_variant;

	procedure mount_device (
		module_name		: in type_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in et_general.type_variant_name.bounded_string; -- low_cost
		device			: in type_device_name; -- R1
		value			: in type_value.bounded_string; -- 220R
		partcode		: in et_material.type_partcode.bounded_string; -- R_PAC_S_0805_VAL_220R
		purpose			: in pac_device_purpose.bounded_string := pac_device_purpose.to_bounded_string (""); -- set temperature
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;

		function write_purpose return string is
			use pac_device_purpose;
		begin
			if length (purpose) = 0 then
				return "";
			else
				return " purpose " & enclose_in_quotes (et_devices.to_string (purpose));
			end if;
		end;

		procedure mount (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_assembly_variants.pac_variants;
			cursor : et_assembly_variants.pac_variants.cursor;

			procedure insert_device (
				name		: in et_general.type_variant_name.bounded_string;
				variant		: in out et_assembly_variants.type_variant) is
				use et_assembly_variants.type_devices;
				cursor : et_assembly_variants.type_devices.cursor;
				inserted : boolean;
			begin
				-- Locate the device in the variant.
				-- If already there, delete it and insert it anew
				-- as specified by the operator.
				cursor := find (variant.devices, device);

				if cursor /= et_assembly_variants.type_devices.no_element then -- device already there
					delete (variant.devices, cursor);
				end if;
					
				insert (
					container	=> variant.devices,
					position	=> cursor,
					inserted	=> inserted,
					key			=> device, -- R1
					new_item	=> (
							mounted		=> YES,
							value		=> value,		-- 220R
							partcode	=> partcode,	-- R_PAC_S_0805_VAL_220R
							purpose		=> purpose)		-- set temperature
				   );
					
			end insert_device;
			
		begin -- mount
			-- the variant must exists
			cursor := et_assembly_variants.pac_variants.find (module.variants, variant_name);

			if cursor /= et_assembly_variants.pac_variants.no_element then

				et_assembly_variants.pac_variants.update_element (
					container	=> module.variants,
					position	=> cursor,
					process		=> insert_device'access);

			else
				assembly_variant_not_found (variant_name);
			end if;

		end mount;

	begin -- mount_device
		log (text => "module " & to_string (module_name) &
			 " variant " & enclose_in_quotes (to_variant (variant_name)) &
			 " mount device " & to_string (device) &
			 " value " & to_string (value) &
			 " partcode " & et_material.to_string (partcode) &
			 write_purpose,
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- Test whether the given device exists in the module.
		if exists (module_cursor, device) then
		
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> mount'access);

		else
			device_not_found (device);
		end if;
	end mount_device;

	procedure unmount_device (
	-- Sets the gvien device as not mounted in 
	-- the given assembly variant. An already existing device will be overwritten
	-- without warning.
		module_name		: in type_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in et_general.type_variant_name.bounded_string; -- low_cost
		device			: in type_device_name; -- R1
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;

		procedure unmount (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_assembly_variants.pac_variants;
			cursor : et_assembly_variants.pac_variants.cursor;

			procedure insert_device (
				name		: in et_general.type_variant_name.bounded_string;
				variant		: in out et_assembly_variants.type_variant) is
				use et_assembly_variants.type_devices;
				cursor : et_assembly_variants.type_devices.cursor;
				inserted : boolean;
			begin
				-- Locate the device in the variant.
				-- If already there, delete it and insert it anew.
				cursor := find (variant.devices, device);

				if cursor /= et_assembly_variants.type_devices.no_element then -- device already there
					delete (variant.devices, cursor);
				end if;
					
				insert (
					container	=> variant.devices,
					position	=> cursor,
					inserted	=> inserted,
					key			=> device, -- R1
					new_item	=> (
							mounted		=> NO)
				   );
					
			end insert_device;
			
		begin -- unmount
			-- the variant must exists
			cursor := et_assembly_variants.pac_variants.find (module.variants, variant_name);

			if cursor /= et_assembly_variants.pac_variants.no_element then

				et_assembly_variants.pac_variants.update_element (
					container	=> module.variants,
					position	=> cursor,
					process		=> insert_device'access);

			else
				assembly_variant_not_found (variant_name);
			end if;

		end unmount;

	begin -- unmount_device
		log (text => "module " & to_string (module_name) &
			 " variant " & enclose_in_quotes (to_variant (variant_name)) &
			 " unmounting device " & to_string (device),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- Test whether the given device exists in the module.
		if exists (module_cursor, device) then
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> unmount'access);

		else
			device_not_found (device);
		end if;
			
	end unmount_device;

	procedure remove_device (
	-- Removes the gvien device from the given assembly variant.
		module_name		: in type_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in et_general.type_variant_name.bounded_string; -- low_cost
		device			: in type_device_name; -- R1
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;

		procedure remove (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_assembly_variants.pac_variants;
			cursor : et_assembly_variants.pac_variants.cursor;

			procedure delete_device (
				name		: in et_general.type_variant_name.bounded_string;
				variant		: in out et_assembly_variants.type_variant) is
				use et_assembly_variants.type_devices;
				cursor : et_assembly_variants.type_devices.cursor;
			begin
				-- Locate the device in the variant. Issue error message
				-- if not found.
				cursor := find (variant.devices, device);

				if cursor /= et_assembly_variants.type_devices.no_element then  -- device in assembly variant
					delete (variant.devices, cursor); -- delete device
				else
					log (ERROR, "device " & to_string (device) &
						" not found in assembly variant " &
						enclose_in_quotes (to_variant (variant_name)) & " !",
						 console => true);
					raise constraint_error;
				end if;
					
			end delete_device;
			
		begin -- remove
			-- the variant must exist
			cursor := et_assembly_variants.pac_variants.find (module.variants, variant_name);

			if cursor /= et_assembly_variants.pac_variants.no_element then

				et_assembly_variants.pac_variants.update_element (
					container	=> module.variants,
					position	=> cursor,
					process		=> delete_device'access);

			else
				assembly_variant_not_found (variant_name);
			end if;

		end remove;

	begin -- remove_device
		log (text => "module " & to_string (module_name) &
			 " variant " & enclose_in_quotes (to_variant (variant_name)) &
			 " removing device " & to_string (device),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- Test whether the given device exists in the module.
		if exists (module_cursor, device) then
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> remove'access);

		else
			device_not_found (device);
		end if;
		
	end remove_device;

	procedure mount_submodule (
	-- Sets the assembly variant of a submodule instance. An already existing submodule
	-- will be overwritten without warning.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		variant_parent	: in et_general.type_variant_name.bounded_string; -- low_cost								  
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		variant_submod	: in et_general.type_variant_name.bounded_string; -- fixed_frequency
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;

		procedure query_variants (
		-- Locates the targeted assembly variant of the parent module.
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_assembly_variants.pac_variants;
			cursor : et_assembly_variants.pac_variants.cursor;

			procedure mount (
				name		: in et_general.type_variant_name.bounded_string; -- low_cost (parent module)
				variant		: in out et_assembly_variants.type_variant) is
				use et_assembly_variants.type_submodules;
				cursor : type_submodules.cursor;
			begin
				-- Locate the submodule instance in the variant of the parent module.
				-- If already there, delete it and insert it anew
				-- as specified by the operator.
				cursor := find (variant.submodules, instance);

				if cursor /= type_submodules.no_element then -- submodule already in assembly variant
					delete (variant.submodules, cursor); -- delete submodule instance
				end if;

				-- insert submodule instance anew with given submodule variant
				insert (
					container	=> variant.submodules,
					key			=> instance, -- OSC1
					new_item	=> (variant => variant_submod) -- fixed_frequency
					);
				
			end mount;
			
		begin -- query_variants
			-- the variant (low_cost) must exist in the parent module
			cursor := et_assembly_variants.pac_variants.find (module.variants, variant_parent);

			if cursor /= et_assembly_variants.pac_variants.no_element then

				-- Insert the submodule instance with the desired variant:
				et_assembly_variants.pac_variants.update_element (
					container	=> module.variants,
					position	=> cursor,
					process		=> mount'access);

			else
				assembly_variant_not_found (variant_parent);
			end if;

		end query_variants;
		
	begin -- mount_submodule
		log (text => "module " & to_string (module_name) &
			 " variant " & enclose_in_quotes (to_variant (variant_parent)) &
			 " submodule instance " & enclose_in_quotes (to_string (instance)) &
			 " mounting variant " & enclose_in_quotes (to_variant (variant_submod)),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- Test whether the given parent module contains the given submodule instance (OSC1)
		if exists (module_cursor, instance) then

			if exists (module_cursor, instance, variant_submod) then
			
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> query_variants'access);
			else
				log (ERROR, "submodule instance " &
					 enclose_in_quotes (to_string (instance)) &
					 " does not provide assembly variant " &
					 enclose_in_quotes (to_variant (variant_submod)) & " !",
					 console => true);
				raise constraint_error;
			end if;

		else
			submodule_not_found (instance);
		end if;
		
	end mount_submodule;

	procedure remove_submodule (
	-- Removes the assembly variant of a submodule. This results in all devices
	-- of the submodule being mounted.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		variant_parent	: in et_general.type_variant_name.bounded_string; -- low_cost
		instance		: in et_general.type_module_instance_name.bounded_string; -- OSC1
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;

		procedure query_variants (
		-- Locates the targeted assembly variant of the parent module.
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_assembly_variants.pac_variants;
			cursor : et_assembly_variants.pac_variants.cursor;

			procedure remove (
				name		: in et_general.type_variant_name.bounded_string; -- low_cost (parent module)
				variant		: in out et_assembly_variants.type_variant) is
				use et_assembly_variants.type_submodules;
				cursor : type_submodules.cursor;
			begin
				-- Locate the submodule instance in the variant of the parent module.
				-- Issue error message if not found.
				cursor := find (variant.submodules, instance);

				if cursor /= type_submodules.no_element then -- submodule in assembly variant
					delete (variant.submodules, cursor); -- delete submodule instance
				else
					log (ERROR, "submodule " & to_string (instance) &
						" not found in assembly variant " &
						enclose_in_quotes (to_variant (variant_parent)) & " !",
						 console => true);
					raise constraint_error;
				end if;
			end remove;
			
		begin -- query_variants
			-- the variant (low_cost) must exist in the parent module
			cursor := et_assembly_variants.pac_variants.find (module.variants, variant_parent);

			if cursor /= et_assembly_variants.pac_variants.no_element then

				-- Remove the submodule instance
				et_assembly_variants.pac_variants.update_element (
					container	=> module.variants,
					position	=> cursor,
					process		=> remove'access);

			else
				assembly_variant_not_found (variant_parent);
			end if;

		end query_variants;
		
	begin -- remove_submodule
		log (text => "module " & to_string (module_name) &
			" variant " & enclose_in_quotes (to_variant (variant_parent)) &
			" removing variant of submodule instance " & enclose_in_quotes (to_string (instance)),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- Test whether the given parent module contains the given submodule instance (OSC1)
		if exists (module_cursor, instance) then

			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> query_variants'access);

		else
			submodule_not_found (instance);
		end if;
		
	end remove_submodule;

	function rename_device (
	-- Renames the given device. Returns true if device has been renamed.
	-- Assumes that the device with name device_name_before exists.
	-- Does not perform any conformity checks on given device names.
		module_cursor		: in pac_generic_modules.cursor; -- the cursor to the module
		device_name_before	: in type_device_name; -- IC1
		device_name_after	: in type_device_name; -- IC101
		log_threshold		: in type_log_level) 
		return boolean is

		result : boolean := false;

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

			-- copy elements and properties of the old device to a new one:
			et_schematic.type_devices.insert (
				container	=> module.devices,
				key			=> device_name_after, -- IC23
				new_item	=> element (device_cursor_before), -- all elements and properties of IC1
				inserted	=> inserted,
				position	=> device_cursor_after);

			if inserted then

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

				result := true; -- successful renaming
			else
				log (text => "device name " & to_string (device_name_after) & 
					" already used -> skipped", level => log_threshold + 1);
			end if;
		end query_devices;

	begin -- rename_device
		log (text => "renaming " & to_string (device_name_before) & " to " & 
			to_string (device_name_after),
			level => log_threshold);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

		log_indentation_down;

		return result;

		exception when event: others =>
			return false;

	end rename_device;

	function sort_by_coordinates (
		module_cursor 	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level) 
		return et_numbering.type_devices.map is
		use et_numbering;
		devices : et_numbering.type_devices.map; -- to be returned

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is

			procedure query_units (device_cursor : in et_schematic.type_devices.cursor) is
				use et_schematic.type_units;
				device_name : type_device_name := et_schematic.type_devices.key (device_cursor); -- R1

				procedure sort (unit_cursor : in et_schematic.type_units.cursor) is 
					unit_name : pac_unit_name.bounded_string := key (unit_cursor);  -- 1, C, IO_BANK1
					unit_position : et_coordinates.type_position := element (unit_cursor).position;
					inserted : boolean := false;
					cursor_sort : et_numbering.type_devices.cursor;
				begin -- sort
					log (text => "unit " & to_string (unit_name) &
						" at " & to_string (position => unit_position),
						 level => log_threshold + 2);
					
					et_numbering.type_devices.insert 
						(
						container	=> devices,
						key			=> unit_position, -- sheet/x/y
						inserted	=> inserted,
						position	=> cursor_sort,
						new_item	=> (
									name => device_name, -- R1, IC3
									unit => unit_name, -- 1, C, IO_BANK1
									done => false -- not renumbered yet
									)
						);

					if not inserted then
						log (ERROR, "device " & to_string (device_name) &
							 " unit " & to_string (unit_name) &
							 " at " & to_string (position => unit_position) &
							 " sits on top of another unit !",
							 console => true);
						raise constraint_error;
					end if;
							 
				end sort;
				
			begin -- query_units
				
				log (text => "device " & to_string (device_name), -- R1, IC3
					 level => log_threshold + 1);
				
				log_indentation_up;
				
				et_schematic.type_units.iterate (
					container	=> et_schematic.type_devices.element (device_cursor).units,
					process		=> sort'access);

				log_indentation_down;
			end query_units;
			
		begin -- query_devices
			et_schematic.type_devices.iterate (
				container	=> module.devices,
				process		=> query_units'access);
		end query_devices;

	begin -- sort_by_coordinates
		log (text => "sorting devices/units by schematic coordinates ...", level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_devices'access);


		log_indentation_down;
		
		return devices;
	end sort_by_coordinates;

	function unit_positions_valid (
	-- Returns true if no unit sits on top of another.
		module_cursor 	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
		return boolean is
		use et_numbering;
		devices : et_numbering.type_devices.map;
	begin
		devices := sort_by_coordinates (module_cursor, log_threshold);
		-- If a unit sits on top of another unit, sort_by_coordinates throws a
		-- constraint_error which will be catched here.

		return true;
		
		exception when event: others => 
			return false;
		
	end unit_positions_valid;
	
	procedure renumber_devices (
	-- Renumbers devices according to the sheet number.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		step_width		: in type_name_index;
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_conventions;
		use et_numbering;

		-- The list of devices sorted by their coordinates.
		-- By their order in this list the devices will be renumbered.
		devices : et_numbering.type_devices.map;

		function renumber (cat : in et_conventions.type_device_category) return boolean is
		-- Renumbers devices of given category. Returns true if all devices
		-- have been renamed.
		-- Marks every renamed unit in the device list so that the second
		-- run of this function does not try to renumber them again.
			result : boolean := true;
			
			use et_numbering.type_devices;
			cursor : et_numbering.type_devices.cursor := devices.first;
			name_before, name_after : type_device_name; -- R1
			use et_coordinates;
			sheet_before, sheet_now : type_sheet := type_sheet'first;

			index_on_sheet : type_name_index := type_name_index'first;
			device_index : type_name_index;

			procedure update_index is begin
			-- Detects when the sheet number changes. In this case
			-- resets the index_on_sheet so that the indexing starts anew.
				sheet_now := sheet (key (cursor));

				if sheet_now = sheet_before then -- no change
					index_on_sheet := index_on_sheet + 1;
				else -- sheet has changed
					sheet_before := sheet_now;
					index_on_sheet := type_name_index'first + 1;
				end if;
			end update_index;

			procedure mark_units_done is
			-- Sets the "done" flag of all devices with name_before in the device list.

				-- Start with the unit indicated by cursor. All other
				-- units of the device come after this one.
				cursor_done : et_numbering.type_devices.cursor := cursor;

				procedure set_done (
					coordinates : in et_coordinates.type_position;
					device		: in out et_numbering.type_device) is
				begin
					device.done := true;
				end;
				
			begin -- mark_units_done
				log (text => "marking all units done ...", level => log_threshold + 2);
				
				while cursor_done /= et_numbering.type_devices.no_element loop
					
					if element (cursor_done).name = name_before then -- IC5
						
						log (text => " unit " & to_string (element (cursor_done).unit),
							 level => log_threshold + 2);

						update_element (
							container	=> devices,
							position	=> cursor_done,
							process		=> set_done'access);
						
					end if;
					
					next (cursor_done);
				end loop;
			end mark_units_done;
			
		begin -- renumber
			while cursor /= et_numbering.type_devices.no_element loop

				if not element (cursor).done then
					name_before := element (cursor).name; -- R1

					-- If the current device is in given category:
					if category (name_before) = cat then

						log (text => "device " & to_string (name_before) &
							" unit " & to_string (element (cursor).unit), level => log_threshold +1);
						log_indentation_up;
						
						update_index;
						
						-- step width times sheet number: like 100 * 4 = 400
						device_index := step_width * type_name_index (sheet_now);

						-- 400 plus index on sheet: like 407
						device_index := device_index + index_on_sheet;

						-- build the new device name
						name_after := to_device_name (
								prefix	=> prefix (name_before), -- R, C, IC
								index 	=> device_index); -- 407

						-- Do the renaming if the new name differs from the old name.
						-- If the renaming fails, set result false. Result remains false
						-- even if other renamings succeed.
						if name_after /= name_before then

							if rename_device (
								module_cursor		=> module_cursor,
								device_name_before	=> name_before, -- R1
								device_name_after	=> name_after, -- R407
								log_threshold		=> log_threshold + 2) then

								-- Mark all units of the device as done:
								mark_units_done;
							else
								result := false;
							end if;
							
						end if;

						log_indentation_down;
					end if;
				end if;
				
				next (cursor);
			end loop;

			return result;

			exception when event:
				others => 
					log (text => ada.exceptions.exception_message (event), console => true);
				raise;
			
		end renumber;
		
	begin -- renumber_devices
		log (text => "module " & to_string (module_name) &
			" renumbering devices." &
			" step width per sheet" & to_string (step_width),
			level => log_threshold);

		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);

		-- Get a list of devices and units where they are sorted by their coordinates.
		devices := sort_by_coordinates (module_cursor, log_threshold + 2);

		-- Renumber for each device category. If the first run fails, start another
		-- iteration. If that fails too, issue error and abort.
		-- Devices of unknown category are exampted from renumbering.
		for cat in et_conventions.type_device_category'pos (et_conventions.type_device_category'first) .. 
			et_conventions.type_device_category'pos (et_conventions.type_device_category'last) loop

			case et_conventions.type_device_category'val (cat) is
				when UNKNOWN => null;
				
				when others =>

					log (text => "category" & to_string (et_conventions.type_device_category'val (cat)),
						 level => log_threshold + 1);

					log_indentation_up;
					if renumber (et_conventions.type_device_category'val (cat)) = false then
						-- first iteration failed. start a second:
						
						log (text => "another iteration required", level => log_threshold + 2);
						log_indentation_up;
						
						if renumber (et_conventions.type_device_category'val (cat)) = false then
							-- second iteration failed: abort
							log (ERROR, "renumbering failed !", console => true);
							raise constraint_error;
						end if;
						
						log_indentation_down;
					end if;

					log_indentation_down;
			end case;
		end loop;

		log_indentation_down;
	end renumber_devices;
		
	function device_index_range (
	-- Returns the lowest and highest device index of the given module.
	-- NOTE: This is about the indexes used by the generic module.
		module_cursor		: in pac_generic_modules.cursor; -- the cursor to the module
		log_threshold		: in type_log_level) 
		return et_numbering.type_index_range is

		index_range : et_numbering.type_index_range; -- to be returned

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use et_schematic.type_devices;

			device_cursor : et_schematic.type_devices.cursor := module.devices.first;
			index_current : type_name_index;
		begin -- query_devices
			while device_cursor /= et_schematic.type_devices.no_element loop

				index_current := index (key (device_cursor));
				
				if index_current < index_range.lowest then -- see specs of type_index_range for defaults
					index_range.lowest := index_current;
				end if;

				if index_current > index_range.highest then -- see specs of type_index_range for defaults
					index_range.highest := index_current;
				end if;
				
				next (device_cursor);
			end loop;

			if length (module.devices) > 0 then
				log (text => et_numbering.to_index_range (module_name, index_range),
					 level => log_threshold + 1);
			else
				log (WARNING, "no devices found in module " &
					 enclose_in_quotes (to_string (module_name)) & " !");

				index_range.lowest := type_name_index'first;
				index_range.highest := type_name_index'first;

			end if;

		end query_devices;
		
	begin -- device_index_range
		log (text => "module " & enclose_in_quotes (to_string (key (module_cursor))),
			--" obtaining device index range ...",
			level => log_threshold);
		
		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_devices'access);

		log_indentation_down;

		return index_range;
		
	end device_index_range;

	procedure autoset_device_name_offsets (
	-- Calculates the device index ranges of the given top module and all its submodules.
	-- Assigns the device names offset of the instantiated submodules.
	-- Assumes that all devices of the modules are mounted -> assembly variants ignored.
		module_name		: in type_module_name.bounded_string; -- the top module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor := generic_modules.first;
		index_range : et_numbering.type_index_range;

		use et_numbering;
		
		package type_ranges is new ordered_maps (
			key_type		=> type_module_name.bounded_string, -- motor_driver (generic module name)
			"<"				=> type_module_name."<",
			element_type	=> et_numbering.type_index_range); -- 3..190

		-- The device name indexes of all (sub)modules are stored here:
		ranges : type_ranges.map;

		function query_range (module : in type_module_name.bounded_string)
		-- Returns the index range of the given generic module.
		-- NOTE: This is about the indexes used by the generic module.			
			return et_numbering.type_index_range is
			cursor : type_ranges.cursor;
		begin
			cursor := type_ranges.find (ranges, module);
			return type_ranges.element (cursor);
		end query_range;
		
		submod_tree : et_numbering.type_modules.tree;
		tree_cursor : et_numbering.type_modules.cursor;

		-- A stack keeps record of the submodule level where tree_cursor is pointing at.
		package stack is new et_general.stack_lifo (
			item	=> et_numbering.type_modules.cursor,
			max 	=> et_submodules.nesting_depth_max);

		-- For calculating the device index offset of submodule instances:
		-- The highest used device name index across the module is stored here.
		-- It increases each time a submodule is entered by procedure set_offset.
		-- The increase is the highest index used by that submodule.
		-- The next submodule will then get an offset of index_max + 1.
		index_max : type_name_index := 0;

		procedure increase_index_max (index : in type_name_index) is begin
			index_max := index_max + index;
		end;
		
		procedure set_offset is 
		-- Reads the submodule tree submod_tree. It is recursive, means it calls itself
		-- until the deepest submodule (the bottom of the design structure) has been reached.
			use et_numbering.type_modules;
			module_name 	: type_module_name.bounded_string; -- motor_driver
			parent_name 	: type_module_name.bounded_string; -- water_pump
			module_range 	: type_index_range;
			module_instance	: et_general.type_module_instance_name.bounded_string; -- MOT_DRV_3

			procedure assign_offset (module : in out et_numbering.type_module) is begin
			-- assign the device name offset to the current submodule according to the latest index_max.
				module.device_names_offset := index_max + 1;
				
				log (text => "module " & enclose_in_quotes (to_string (module_name)) &
					" submodule instance " & enclose_in_quotes (to_string (module_instance)) &
					" setting device names offset to" &
					to_string (module.device_names_offset),
					level => log_threshold + 2);
			end;
			
		begin -- set_offset
			log_indentation_up;

			-- start with the first submodule on the current hierarchy level
			tree_cursor := first_child (tree_cursor);

			-- iterate through the submodules on this level
			while tree_cursor /= et_numbering.type_modules.no_element loop
				module_name := element (tree_cursor).name;
				module_instance := element (tree_cursor).instance;
				module_range := query_range (module_name);

				log (text => "instance " & enclose_in_quotes (to_string (module_instance)) &
					" of generic " & to_index_range (module_name, module_range), level => log_threshold + 1);

				-- In case we are on the first level, the parent module is the given top module.				
				if parent (tree_cursor) = root (submod_tree) then
					parent_name := autoset_device_name_offsets.module_name;
				else
					parent_name := element (parent (tree_cursor)).name;
				end if;

				-- assign the offset to the submodule
				update_element (submod_tree, tree_cursor, assign_offset'access);

				-- For the next submodule (wherever it is) the index_max must be increased the the highest
				-- index used by the current submodule:
				increase_index_max (module_range.highest);
				
				log (text => "index max" & to_string (index_max), level => log_threshold + 1);
				
				if first_child (tree_cursor) = et_numbering.type_modules.no_element then 
					-- no submodules on the current level. means we can't go deeper.
					
					log_indentation_up;
					log (text => "no submodules here -> bottom reached", level => log_threshold + 1);
					log_indentation_down;
				else
					-- there are submodules on the current level
					
					-- backup the cursor to the current submodule on this level
					stack.push (tree_cursor);

					-- iterate through submodules on the level below
					set_offset; -- this is recursive !

					-- restore cursor to submodule (see stack.push above)
					tree_cursor := stack.pop;
				end if;

				next_sibling (tree_cursor);
			end loop;
			
			log_indentation_down;

			exception
				when event: others =>
					log_indentation_reset;
					log (text => ada.exceptions.exception_information (event), console => true);
					raise;
			
		end set_offset;

		procedure replace_tree (
			module_name	: in type_module_name.bounded_string;
			module		: in out et_schematic.type_module) is
		begin
			module.submod_tree := submod_tree;
		end;

		procedure query_submodules (submod_cursor : in et_numbering.type_modules.cursor) is
			use et_numbering.type_modules;
			-- Map from submodule_cursor to module in et_project.modules:

			-- submod_cursor points to a submodule in the submod_tree:
			module_name	: type_module_name.bounded_string := element (submod_cursor).name; -- motor_driver
			-- module_name now contains the generic module name like motor_driver
			
			module_cursor : et_project.modules.pac_generic_modules.cursor := locate_module (module_name);
			-- module_cursor now points to the generic module
		begin
			-- If the range for this generic module has not been computed already, then do
			-- it now. Otherwise there is no need to do that all over:
			if not type_ranges.contains (ranges, module_name) then
				
				index_range := device_index_range (module_cursor, log_threshold + 1);

				type_ranges.insert (
					container	=> ranges,
					key			=> key (module_cursor), -- generic name
					new_item	=> index_range);
				
			end if;
		end query_submodules;
									   
	begin -- autoset_device_name_offsets
		log (text => "module " & enclose_in_quotes (to_string (module_name)) &
			" exploring current ranges of device indexes ...", level => log_threshold);
		log_indentation_up;

		-- locate the given top module
		module_cursor := locate_module (module_name);

		-----------------
		-- Calculate the index range per module and store it in 
		-- container "ranges":
		-- NOTE: This is about the indexes used by the generic modules.

		-- top module:
		index_range := device_index_range (module_cursor, log_threshold + 1);

		type_ranges.insert (
			container	=> ranges,
			key			=> module_name,
			new_item	=> index_range);

		-- submodules:		
		et_numbering.type_modules.iterate (element (module_cursor).submod_tree, query_submodules'access);
		
		-- calculation of index ranges complete
		----------------
		
		log_indentation_down;

		log (text => "autosetting device name offset of submodules instances ...", level => log_threshold);
		log_indentation_up;
		
		-- locate the given top module
-- 		module_cursor := locate_module (module_name);

		-- The first module being processed now is the given top module.
		-- Its highest used device index extends the total index range.
		increase_index_max (query_range (module_name).highest);

		-- Show the index range used by the top module:
		log (text => "top" & to_index_range (module_name, query_range (module_name)), level => log_threshold + 1);

		log (text => "index max" & to_string (index_max), level => log_threshold + 1);
		
		-- take a copy of the submodule tree of the given top module:
		submod_tree := element (module_cursor).submod_tree;

		-- set the cursor inside the tree at root position:
		tree_cursor := et_numbering.type_modules.root (submod_tree);
		
		stack.init;

		-- start reading the submodule tree. set_offset is recursive.
		set_offset;

		-- Replace the old submodule tree by the new submod_tree. The new submod_tree now
		-- contains the device name offsets for the instantiated submodules.
		et_project.modules.pac_generic_modules.update_element (generic_modules, module_cursor, replace_tree'access);
		
		log_indentation_down;

		exception
			when event: others =>
				log_indentation_reset;
				log (text => ada.exceptions.exception_information (event), console => true);
				raise;

	end autoset_device_name_offsets;

	procedure dump_tree (
	-- Dumps submodule names, instances and device name offsets:
		module_name		: in type_module_name.bounded_string;
		log_threshold	: in type_log_level) is
		
		module_cursor : pac_generic_modules.cursor;

		procedure query_submodules (
   			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
			use et_numbering;

			procedure query (cursor : in et_numbering.type_modules.cursor) is
				use et_numbering.type_modules;
			begin
				log (text => "instance " & to_string (element (cursor).instance) &
					 " offset " & to_string (element (cursor).device_names_offset),
					 level => log_threshold
					);
			end query;
			
		begin
			et_numbering.type_modules.iterate (module.submod_tree, query'access);
		end query_submodules;

	begin
		log (text => "SUBMODULES TREE DUMP", level => log_threshold);
		log_indentation_up;
		
		module_cursor := locate_module (module_name);
		et_project.modules.pac_generic_modules.query_element (module_cursor, query_submodules'access);

		log_indentation_down;
	end dump_tree;
	
	procedure build_submodules_tree (
	-- Re(builds) the submodule tree of the given parent module.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level) is

		-- the cursor to the given top module
		module_cursor : pac_generic_modules.cursor;
		
		submod_tree : et_numbering.type_modules.tree := et_numbering.type_modules.empty_tree;
		tree_cursor : et_numbering.type_modules.cursor := et_numbering.type_modules.root (submod_tree);

		-- A stack keeps record of the submodule level where tree_cursor is pointing at.
		package stack is new et_general.stack_lifo (
			item	=> et_numbering.type_modules.cursor,
			max 	=> et_submodules.nesting_depth_max);
		
		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
			use et_submodules;
			use et_submodules.type_submodules;
			submod_cursor	: et_submodules.type_submodules.cursor := module.submods.first;
			submod_name		: type_module_name.bounded_string; -- $ET_TEMPLATES/motor_driver
			submod_instance	: type_module_instance_name.bounded_string; -- OSC1
		begin -- query_submodules in given top module
			while submod_cursor /= et_submodules.type_submodules.no_element loop
				submod_name := to_module_name (remove_extension (to_string (element (submod_cursor).file)));
				submod_instance := key (submod_cursor);
				log (text => "submodule " & enclose_in_quotes (to_string (submod_name)) &
					 " instance " & to_string (submod_instance), level => log_threshold + 1);

				-- Before inserting the submodule in the tree, the current tree cursor
				-- at this level must be saved on the stack:
				stack.push (tree_cursor);

				et_numbering.type_modules.insert_child (
					container	=> submod_tree,
					parent		=> tree_cursor,
					before		=> et_numbering.type_modules.no_element,
					new_item	=> (
							name				=> submod_name,
							instance			=> submod_instance,
							device_names_offset	=> type_name_index'first
							), -- templates/CLOCK_GENERATOR OSC1 100
					position	=> tree_cursor
					);

				-- tree_cursor points now to the submodule that has been inserted last.
				-- Submodules of this submodule will be inserted as childs.
				log_indentation_up;
				
				-- locate the current submodule
				module_cursor := locate_module (submod_name);

				-- search for submodules at deeper levels. Here the procedure query_submodules
				-- calls itself (recursive).
				query_element (
					position	=> module_cursor,
					process		=> query_submodules'access);

				log_indentation_down;

				-- Restore the tree cursor. See stack.push statement above.
				tree_cursor := stack.pop;
				
				next (submod_cursor);
			end loop;
		end query_submodules;

		procedure assign_tree (
			module_name	: in type_module_name.bounded_string;
			module		: in out et_schematic.type_module) is
		begin
			module.submod_tree := submod_tree;

			log_indentation_up;
			
			log (text => "submodules total" & 
				 count_type'image (et_numbering.type_modules.node_count (module.submod_tree) - 1),
				 level => log_threshold + 1
				);

			log_indentation_down;
		end assign_tree;
		
	begin -- build_submodules_tree
		log (text => "module " & enclose_in_quotes (to_string (module_name)) &
			" building submodules tree ...", level => log_threshold);
		log_indentation_up;
		
		stack.init;
		
		-- locate the given top module
		module_cursor := locate_module (module_name);

		-- build the submodule tree in container submod_tree:
		query_element (
			position	=> module_cursor,
			process		=> query_submodules'access);
		
		log_indentation_down;

		-- relocate the given top module
		module_cursor := locate_module (module_name);
		
		-- assign the submod_tree to the given top module
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> assign_tree'access);

		-- update device name offsets of submodules
		autoset_device_name_offsets (module_name, log_threshold + 1);
		
	end build_submodules_tree;

	procedure apply_offset (
	-- Adds the offset to the device index of the given device_name.
		device_name		: in out type_device_name; -- IC3
		offset			: in type_name_index; -- 100
		log_threshold	: in et_string_processing.type_log_level) is
		device_name_instance : type_device_name;
	begin
		-- apply offset if it is greater zero. If offset is zero, we
		-- are dealing with the top module.
		if offset > 0 then
			device_name_instance := device_name; -- take copy of original name
			
			-- apply device name offset
			offset_index (device_name_instance, offset);

			-- log original name and name in instanciated submodule
			log (text => "device name origin " & to_string (device_name) &
				" -> in submodule instance " & to_string (device_name_instance),
				level => log_threshold);

			device_name := device_name_instance; -- overwrite orignial name
		else
			-- no offset to apply:
			log (text => "device name " & to_string (device_name) & " -> no offset",
				level => log_threshold);
		end if;
	end;

	procedure make_boms (
	-- Generates the BOM files of all assembly variants from the given top module.
	-- The files are named after the module name and the variant name.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;
		use et_assembly_variants.pac_variants;
		use et_general.type_variant_name;

		procedure make_for_variant (variant_name : in et_general.type_variant_name.bounded_string) is

			use et_material;
			bill_of_material : et_material.type_devices.map;

			procedure collect (
			-- Collects devices of the given module and its variant in container bill_of_material.
			-- Adds to the device index the given offset.
			-- If offset is zero, we are dealing with the top module.
				module_cursor	: in pac_generic_modules.cursor;
				variant			: in et_general.type_variant_name.bounded_string;
				offset			: in type_name_index) is
				
				procedure query_devices (
					module_name	: in type_module_name.bounded_string;
					module		: in et_schematic.type_module) is

					device_name : type_device_name;
					inserted : boolean;
					
					procedure test_inserted is begin
						if not inserted then
							log (ERROR, "multiple occurence of device " & to_string (device_name),
									console => true);
							raise constraint_error;
						end if;
					end;

					procedure test_partcode (partcode : in et_material.type_partcode.bounded_string) is
					begin
						if type_partcode.length (partcode) = 0 then
							log (WARNING, text => "device " & to_string (device_name) &
								" has no partcode !");
						end if;
					end;
					
					procedure query_properties_default (cursor_schematic : in et_schematic.type_devices.cursor) is 
						cursor_bom : et_material.type_devices.cursor;

						use et_schematic.type_devices;
						use et_assembly_variants.type_devices;
						use et_symbols;
					begin -- query_properties_default

						-- the device must be real
						--if element (cursor_schematic).appearance = PCB then -- skip virtual devices
						if is_real (cursor_schematic) then -- skip virtual devices

							-- the package must be real
							if has_real_package (cursor_schematic) then

								device_name := et_schematic.type_devices.key (cursor_schematic);

								-- issue warning if device has no partcode
								test_partcode (element (cursor_schematic).partcode);
								
								-- Store device in bill_of_material as it is:

								apply_offset (device_name, offset, log_threshold + 2);
								
								et_material.type_devices.insert (
									container	=> bill_of_material,
									key			=> device_name, -- IC4, R3
									new_item	=> (
										value		=> element (cursor_schematic).VALUE,
										partcode	=> element (cursor_schematic).PARTCODE,
										purpose		=> element (cursor_schematic).PURPOSE,
										packge		=> get_package_model (cursor_schematic)),
									position	=> cursor_bom,
									inserted	=> inserted);
								
								test_inserted;
								
							end if;
						end if;
					end query_properties_default;

					procedure query_properties_variants (cursor_schematic : in et_schematic.type_devices.cursor) is 
						cursor_bom : et_material.type_devices.cursor;

						use et_schematic.type_devices;
						alt_dev_cursor : et_assembly_variants.type_devices.cursor;
						use et_assembly_variants.type_devices;
						use et_symbols;
					begin -- query_properties_variants

						-- the device must be real (appearance SCH_PCB)
						if element (cursor_schematic).appearance = PCB then -- skip virtual devices

							-- the package must be real
							if has_real_package (cursor_schematic) then
							
								device_name := et_schematic.type_devices.key (cursor_schematic);
								
								-- Get a cursor to the alternative device as specified in the assembly variant:
								alt_dev_cursor := alternative_device (module_cursor, variant, device_name); 
								
								if alt_dev_cursor = et_assembly_variants.type_devices.no_element then
								-- Device has no entry in the assembly variant. -> It is to be stored in bill_of_material as it is:

									-- issue warning if device has no partcode
									test_partcode (element (cursor_schematic).partcode);
									
									apply_offset (device_name, offset, log_threshold + 2);
									
									et_material.type_devices.insert (
										container	=> bill_of_material,
										key			=> device_name, -- IC4, R3
										new_item	=> (
											value		=> element (cursor_schematic).value,
											partcode	=> element (cursor_schematic).partcode,	
											purpose		=> element (cursor_schematic).purpose,
											packge		=> get_package_model (cursor_schematic)),
										position	=> cursor_bom,
										inserted	=> inserted);

									test_inserted;

								else
								-- Device has an entry in the assembly variant. Depending on the mounted-flag
								-- it is to be skipped or inserted in bill_of_material with alternative properties.
								-- NOTE: The package model is not affected by the assembly variant.
									case element (alt_dev_cursor).mounted is
										when NO =>
											log (text => to_string (device_name) & " not mounted -> skipped",
												level => log_threshold + 2);
											
										when YES =>
											-- issue warning if device has no partcode
											test_partcode (element (alt_dev_cursor).partcode);

											apply_offset (device_name, offset, log_threshold + 2);
											
											-- Insert the device in bill with alternative properties as defined
											-- in the assembly variant:
											et_material.type_devices.insert (
												container	=> bill_of_material,
												key			=> device_name, -- IC4, R3
												new_item	=> (
													value		=> element (alt_dev_cursor).value,
													partcode	=> element (alt_dev_cursor).partcode,
													purpose		=> element (alt_dev_cursor).purpose,
													packge		=> get_package_model (cursor_schematic)),
												position	=> cursor_bom,
												inserted	=> inserted);

											test_inserted;

											-- check partcode content
											et_conventions.validate_partcode (
												partcode		=> et_material.type_devices.element (cursor_bom).partcode,
												device_name		=> device_name,
												packge			=> et_packages.to_package_name (ada.directories.base_name 
																	(et_packages.to_string (et_material.type_devices.element (cursor_bom).packge))),
												value			=> et_material.type_devices.element (cursor_bom).value,
												log_threshold	=> log_threshold + 3);

									end case;
								end if;

							end if;
						end if;
					end query_properties_variants;
					
				begin -- query_devices
					-- if default variant given, then assembly variants are irrelevant:
					if is_default (variant) then

						log (text => "collecting devices from module " &
								enclose_in_quotes (to_string (module_name)) &
								" default variant by applying device index offset" & 
								to_string (offset), -- 100
							level => log_threshold + 1);
						
						log_indentation_up;
						
						et_schematic.type_devices.iterate (
							container	=> module.devices,
							process		=> query_properties_default'access);

					-- if a particular variant given, then collect devices accordingly:
					else
						log (text => "collecting devices from module " &
								enclose_in_quotes (to_string (module_name)) &
								" variant " & enclose_in_quotes (to_variant (variant)) &
								" by applying device index offset" & 
								to_string (offset), -- 100
							level => log_threshold + 1);
						
						log_indentation_up;
					
						et_schematic.type_devices.iterate (
							container	=> module.devices,
							process		=> query_properties_variants'access);

					end if;
					
					log_indentation_down;
				end query_devices;

			begin -- collect
				et_project.modules.pac_generic_modules.query_element (
					position	=> module_cursor,
					process		=> query_devices'access);
				
			end collect;
				
			submod_tree : et_numbering.type_modules.tree := et_numbering.type_modules.empty_tree;
			tree_cursor : et_numbering.type_modules.cursor := et_numbering.type_modules.root (submod_tree);

			-- A stack keeps record of the submodule level where tree_cursor is pointing at.
			package stack_level is new et_general.stack_lifo (
				item	=> et_numbering.type_modules.cursor,
				max 	=> et_submodules.nesting_depth_max);

			-- Another stack keeps record of the assembly variant on submodule levels.
			package stack_variant is new et_general.stack_lifo (
				item	=> et_general.type_variant_name.bounded_string,
				max 	=> et_submodules.nesting_depth_max);
			
			variant : et_general.type_variant_name.bounded_string; -- low_cost
			
			procedure query_submodules is 
			-- Reads the submodule tree submod_tree. It is recursive, means it calls itself
			-- until the deepest submodule (the bottom of the design structure) has been reached.
				use et_numbering.type_modules;
				module_name 	: type_module_name.bounded_string; -- motor_driver
				parent_name 	: type_module_name.bounded_string; -- water_pump
				module_instance	: et_general.type_module_instance_name.bounded_string; -- MOT_DRV_3
				offset			: type_name_index;

				use et_assembly_variants.type_submodules;
				alt_submod : et_assembly_variants.type_submodules.cursor;
			begin
				log_indentation_up;

				-- start with the first submodule on the current hierarchy level
				tree_cursor := first_child (tree_cursor);

				-- iterate through the submodules on this level
				while tree_cursor /= et_numbering.type_modules.no_element loop
					module_name := element (tree_cursor).name;
					module_instance := element (tree_cursor).instance;

					log (text => "instance " & enclose_in_quotes (to_string (module_instance)) &
						" of generic module " & enclose_in_quotes (to_string (module_name)),
						level => log_threshold + 1);

					-- In case we are on the first level, the parent module is the given top module.
					-- In that case the parent variant is the given variant of the top module.
					-- If the top module has the default variant, all submodules in all levels
					-- assume default variant too.
					if parent (tree_cursor) = root (submod_tree) then
						parent_name := make_boms.module_name;
						variant := variant_name;
					else
						parent_name := element (parent (tree_cursor)).name;
					end if;

					-- Get the device name offset of the current submodule;
					offset := element (tree_cursor).device_names_offset;

					if not is_default (variant) then
						-- Query in parent module: Is there any assembly variant specified for this submodule ?

						alt_submod := alternative_submodule (
									module	=> locate_module (parent_name),
									variant	=> variant,
									submod	=> module_instance);

						if alt_submod = et_assembly_variants.type_submodules.no_element then
						-- no variant specified for this submodule -> collect devices of default variant

							variant := default;
						else
						-- alternative variant specified for this submodule
							variant := element (alt_submod).variant;
						end if;

					end if;

					-- collect devices from current module
					collect (
						module_cursor	=> locate_module (module_name),
						variant			=> variant,
						offset			=> offset);

					
					if first_child (tree_cursor) = et_numbering.type_modules.no_element then 
					-- No submodules on the current level. means we can't go deeper:
						
						log_indentation_up;
						log (text => "no submodules here -> bottom reached", level => log_threshold + 1);
						log_indentation_down;
					else
					-- There are submodules on the current level:
						
						-- backup the cursor to the current submodule on this level
						stack_level.push (tree_cursor);

						-- backup the parent assembly variant
						stack_variant.push (variant);

						-- iterate through submodules on the level below
						query_submodules; -- this is recursive !

						-- restore cursor to submodule (see stack_level.push above)
						tree_cursor := stack_level.pop;

						-- restore the parent assembly variant (see stack_variant.push above)
						variant := stack_variant.pop;
					end if;

					next_sibling (tree_cursor); -- next submodule on this level
				end loop;
				
				log_indentation_down;

				exception
					when event: others =>
						log_indentation_reset;
						log (text => ada.exceptions.exception_information (event), console => true);
						raise;
				
			end query_submodules;
			
		begin -- make_for_variant
			if is_default (variant_name) then
				log (text => "default assembly variant ", level => log_threshold + 1);
			else
				log (text => "assembly variant " &
					enclose_in_quotes (to_string (variant_name)), level => log_threshold + 1);
			end if;

			log_indentation_up;

			-- collect devices of the given top module. the top module has no device index offset
			collect (module_cursor, variant_name, 0); 

			-- take a copy of the submodule tree of the given top module:
			submod_tree := element (module_cursor).submod_tree;

			-- set the cursor inside the tree at root position:
			tree_cursor := et_numbering.type_modules.root (submod_tree);
			
			stack_level.init;
			stack_variant.init;

			-- collect devices of the submodules
			query_submodules;

			-- write the bom
			et_material.write_bom (
				bom				=> bill_of_material,	-- the container that holds the bom
				module_name		=> module_name,			-- motor_driver
				variant_name	=> variant_name,		-- low_cost
				--format			=> NATIVE,				-- CS should be an argument in the future
				format			=> EAGLE,				-- CS should be an argument in the future
				log_threshold	=> log_threshold + 1);
			
			log_indentation_down;
		end make_for_variant;
		
		procedure query_variant (variant_cursor : in et_assembly_variants.pac_variants.cursor) is
			use et_general.type_variant_name;
		begin
			make_for_variant (key (variant_cursor));
		end query_variant;
		
	begin -- make_boms
		log (text => "generating BOM ...", level => log_threshold);
		log_indentation_up;

		-- locate the given top module
		module_cursor := locate_module (module_name);

		-- Build the submodule tree of the module according to the current design structure.
		-- All further operations rely on this tree:
		et_schematic_ops.build_submodules_tree (
			module_name 	=> module_name,
			log_threshold	=> log_threshold + 1);

		-- make netlist of default variant
		make_for_variant (default);

		-- make netlists of other variants
		et_assembly_variants.pac_variants.iterate (element (module_cursor).variants, query_variant'access);
				
		log_indentation_down;
	end make_boms;
	
	function port_properties (
	-- Returns properties of the given device port in module indicated by module_cursor.
	-- Properties are things like: terminal name, direction, sensitivity, power level, ...
	-- The device must exist in the module and must be real. Run intergrity check
	-- in case exception occurs here.
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A, B, IO_BANK_2
		port_name		: in et_symbols.type_port_name.bounded_string) -- CE
		return type_port_properties_access is

		properties : type_port_properties_access; -- to be returned
		
		terminal_name : et_terminals.type_terminal_name.bounded_string;

		use et_symbols;
		port_direction : type_port_direction := PASSIVE;
		port_properties_cursor : et_symbols.type_ports.cursor;

		use et_devices;
		
		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use et_schematic.type_devices;
			device_cursor_sch	: et_schematic.type_devices.cursor;
			variant 			: pac_package_variant_name.bounded_string; -- D, N
			device_cursor_lib	: pac_devices_lib.cursor;

			procedure query_variants (
				model	: in pac_device_model_file.bounded_string;
				device	: in et_devices.type_device) is
				variant_cursor : pac_variants.cursor;

				procedure query_ports (
					variant_name	: in pac_package_variant_name.bounded_string;
					variant			: in et_devices.type_variant) is
					use pac_terminal_port_map;
					terminal_cursor : pac_terminal_port_map.cursor := variant.terminal_port_map.first;
					use type_port_name;
					use et_devices.pac_unit_name;
				begin
					while terminal_cursor /= pac_terminal_port_map.no_element loop
						if	element (terminal_cursor).unit = unit_name and then
							element (terminal_cursor).name = port_name then
								terminal_name := key (terminal_cursor);
								exit;
						end if;
						next (terminal_cursor);
					end loop;
						
				end query_ports;
				
			begin -- query_variants
				variant_cursor := pac_variants.find (device.variants, variant);

				pac_variants.query_element (
					position	=> variant_cursor,
					process		=> query_ports'access);
				
			end query_variants;

			use et_symbols.type_ports;
			
		begin -- query_devices
			-- locate device in schematic
			device_cursor_sch := find (module.devices, device_name);

-- 			if device_cursor_sch /= et_schematic.type_devices.no_element then
			
				variant := element (device_cursor_sch).variant;

				-- get the name of the device model (or the generic name)
				device_cursor_lib := locate_device (element (device_cursor_sch).model);

				-- Get the name of the terminal (the pin or pad) according to the device variant.
				-- Store it in variable terminal_name:
				pac_devices_lib.query_element (
					position	=> device_cursor_lib,
					process		=> query_variants'access);

				-- Get the electrical properties of the port of the current device:
				port_properties_cursor := et_devices.properties (device_cursor_lib, port_name);

				-- Create the port where pointer "properties" is pointing at.
				-- It is created with the direction obtained from port_properties_cursor:
				properties := new type_port_properties (
					direction 	=> element (port_properties_cursor).direction);

				-- Assign the terminal name:
				properties.terminal := terminal_name;

				-- Assign electrical properties provided by port_properties_cursor:
				properties.properties := element (port_properties_cursor);

-- 			else
-- 				log (importance => ERROR, text => "Found terminal of device " & enclose_in_quotes (to_string (device_name)) &
-- 					 " , but this device does not exist !");
-- 				raise constraint_error;
-- 			end if;
			
		end query_devices;

	begin -- port_properties 
		query_element (
			position	=> module_cursor,
			process		=> query_devices'access);
		
		return properties;
	end port_properties;
	
	function extend_ports (
	-- Adds further properties to the given device ports.
	-- Additional properties are electrical characteristics (see et_libraries.type_port)
	-- and the terminal name).
		module_cursor	: in pac_generic_modules.cursor;
		ports 			: in et_schematic.type_ports_device.set)
		return et_netlists.type_device_ports_extended.set is

		use et_netlists;
		ports_extended : et_netlists.type_device_ports_extended.set; -- to be returned

		use et_schematic.type_ports_device;
		
		procedure query_ports (port_cursor : in et_schematic.type_ports_device.cursor) is
			port_sch		: et_schematic.type_port_device := element (port_cursor);
			more_properties	: type_port_properties_access;
		begin
			-- get further properties of the current port
			more_properties := port_properties (
				module_cursor	=> module_cursor, 
				device_name		=> port_sch.device_name, 
				unit_name		=> port_sch.unit_name,
				port_name		=> port_sch.port_name);
			
			et_netlists.type_device_ports_extended.insert (
				container	=> ports_extended,
				new_item	=> 
					(
					direction		=> more_properties.direction, -- CS
					device			=> port_sch.device_name, -- IC1
					port			=> port_sch.port_name, -- CE
					terminal		=> more_properties.terminal,
					characteristics => more_properties.properties)
					);
			
		end query_ports;
		
	begin -- extend_ports
		iterate (ports, query_ports'access);
		return ports_extended;
	end extend_ports;

	function port_direction (
	-- Returns the direction (master/slave) of the given submodule port in module indicated by module_cursor.
	-- The submodule must exist in the module.
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		submod_instance	: in type_module_instance_name.bounded_string; -- OSC1
		port_name		: in type_net_name.bounded_string) -- clock_out
		return et_submodules.type_netchanger_port_name is

		use et_submodules;
		direction : type_netchanger_port_name; -- to be returned

		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use type_submodules;
			submod_cursor : type_submodules.cursor;

			procedure query_ports (
				submod_name	: in type_module_instance_name.bounded_string;
				submod		: in type_submodule) is
				use type_submodule_ports;
				port_cursor : type_submodule_ports.cursor;
			begin
				port_cursor := find (submod.ports, port_name);

				direction := element (port_cursor).direction;
			end query_ports;
			
		begin -- query_submodules
			-- locate submodule in schematic
			submod_cursor := find (module.submods, submod_instance);

			query_element (submod_cursor, query_ports'access);
		end query_submodules;

	begin -- port_properties 
		query_element (
			position	=> module_cursor,
			process		=> query_submodules'access);
		
		return direction;
	end port_direction;
	
	function extend_ports (
	-- Adds the port direction (master/slave) to the given submodule ports.
		module_cursor	: in pac_generic_modules.cursor;
		ports 			: in et_schematic.type_ports_submodule.set)
		return et_netlists.type_submodule_ports_extended.set is

		use et_netlists;
		ports_extended : type_submodule_ports_extended.set; -- to be returned

		use et_schematic.type_ports_submodule;

		procedure query_ports (port_cursor : in et_schematic.type_ports_submodule.cursor) is
			port : et_schematic.type_port_submodule := element (port_cursor);
			direction : et_submodules.type_netchanger_port_name; -- master/slave
		begin
 			-- get the direction of the current submodule port
			direction := port_direction (module_cursor, port.module_name, port.port_name);

			et_netlists.type_submodule_ports_extended.insert 
				(
				container	=> ports_extended,
				new_item	=> 
					(
					module		=> port.module_name, -- OSC1
					port		=> port.port_name, -- clock_out
					direction	=> direction -- master/slave
					)
				);
			
		end query_ports;
		
	begin -- extend_ports
		iterate (ports, query_ports'access);
		return ports_extended;
	end extend_ports;
	
	procedure make_netlists (
	-- Generates the netlists of all assembly variants from the given top module.
	-- If parameter "write_files" is true, then exports the netlists in files.
	-- The netlist files are named after the module name and the variant name.
		module_cursor 	: in pac_generic_modules.cursor;
		write_files		: in boolean := false;
		log_threshold	: in type_log_level) is

		use et_netlists;
		use et_general.type_net_name;
		use et_assembly_variants;
		use et_assembly_variants.pac_variants;
		use et_general.type_variant_name;

		procedure make_for_variant (variant_name : in et_general.type_variant_name.bounded_string) is

			-- Since we are dealing with hierarchic designs, a tree of modules (each of them having its
			-- own netlist) is required. In the course of this procedure the netlist_tree is built
			-- and finally passed to netlists.write_netlist for further processing.
			-- The netlist_tree does not provide information on dependencies between nets (such
			-- as primary or secondary nets. see netlist specs).
			netlist_tree : et_netlists.type_modules.tree := et_netlists.type_modules.empty_tree;
			netlist_cursor : et_netlists.type_modules.cursor := et_netlists.type_modules.root (netlist_tree);

			-- This stack keeps record of the netlist_cursor as we go trough the design structure.
			package stack_netlist is new et_general.stack_lifo (
				item	=> et_netlists.type_modules.cursor,
				max 	=> et_submodules.nesting_depth_max);
			
			procedure collect_nets (
			-- Collects net names of the given module and its variant in container netlist.
			-- Adds to the device index the given offset.
			-- If offset is zero, we are dealing with the top module.
				module_cursor	: in et_project.modules.pac_generic_modules.cursor;
				variant			: in et_general.type_variant_name.bounded_string;
				prefix			: in et_general.type_net_name.bounded_string; -- DRV3/OSC1/
				offset			: in type_name_index) is

				use et_assembly_variants.pac_variants;
				variant_cursor : et_assembly_variants.pac_variants.cursor;
				
				procedure query_nets (
					module_name	: in type_module_name.bounded_string;
					module		: in et_schematic.type_module) is

					use et_schematic.type_nets;
					net_cursor_sch : et_schematic.type_nets.cursor := module.nets.first;

					net_name : et_general.type_net_name.bounded_string;
					all_ports : et_schematic.type_ports;
					device_ports_extended : et_netlists.type_device_ports_extended.set;
					submodule_ports_extended : et_netlists.type_submodule_ports_extended.set;

					procedure apply_offsets is
					-- Applies the given offset to the devices in device_ports_extended.
						use et_netlists.type_device_ports_extended;
						-- temporarily the ports will be stored here. Once all ports of
						-- device_ports_extended have been offset, the list
						-- ports_with_offset overwrites device_ports_extended:
						ports_with_offset : et_netlists.type_device_ports_extended.set;
						
						procedure query_ports (cursor : in et_netlists.type_device_ports_extended.cursor) is 
							-- take a copy of the port as it is:
							port : et_netlists.type_device_port_extended := element (cursor);
						begin -- query_ports
							-- apply offset to device name of port
							apply_offset (port.device, offset, log_threshold + 2);

							-- insert the modified port in the container ports_with_offset
							et_netlists.type_device_ports_extended.insert (
								container	=> ports_with_offset,
								new_item	=> port);
						end; -- query_ports
						
					begin -- apply_offsets
						iterate (device_ports_extended, query_ports'access);

						-- overwrite by ports_with_offset
						device_ports_extended := ports_with_offset;
					end; -- apply_offsets

					procedure insert_net (module : in out et_netlists.type_module) is begin
						-- Prepend the given net prefix to the net name.
						-- Insert the net with its ports in the netlist of the submodule.
						et_netlists.type_nets.insert (
							container	=> module.nets,
							key			=> (prefix => prefix, base_name => net_name), -- CLK_GENERATOR/FLT1/ , clock_out
							new_item	=> (
									devices		=> device_ports_extended,
									submodules	=> submodule_ports_extended,
									netchangers	=> all_ports.netchangers,
									scope		=> element (net_cursor_sch).scope)
							--position	=> net_cursor_netlist,
							--inserted	=> inserted
							);
							-- CS: constraint_error arises here if net already in list. should never happen.
					end insert_net;
					
				begin -- query_nets
					if is_default (variant) then
						variant_cursor := et_assembly_variants.pac_variants.no_element;
					else
						variant_cursor := find (module.variants, variant);
						if variant_cursor = et_assembly_variants.pac_variants.no_element then
							assembly_variant_not_found (variant);
						end if;
					end if;
					-- Now variant_cursor points to the given assembly variant. If it points to
					-- no element then it is about the default variant.
					
					-- loop in nets of given module
					while net_cursor_sch /= et_schematic.type_nets.no_element loop

						net_name := et_schematic.type_nets.key (net_cursor_sch);
						
						log (text => "net " & et_general.to_string (prefix) & et_general.to_string (net_name),
							 level => log_threshold + 2);

						-- Get all device, netchanger and submodule ports of this net
						-- according to the given assembly variant:
						all_ports := et_schematic.ports (net_cursor_sch, variant_cursor);
					
						-- extend the submodule ports by their directions (master/slave):
						submodule_ports_extended := extend_ports (module_cursor, all_ports.submodules);
						
						-- extend the device ports by further properties (direction, terminal name, ...):
						device_ports_extended := extend_ports (module_cursor, all_ports.devices);
						
						-- The portlist device_ports_extended now requires the device indexes 
						-- to be changed according to the given offset:
						apply_offsets;
						
						-- insert the net with its ports in the list of nets
						et_netlists.type_modules.update_element (
							container	=> netlist_tree,
							position	=> netlist_cursor,
							process		=> insert_net'access);
						
						next (net_cursor_sch);
					end loop;
				end query_nets;
				
			begin -- collect_nets
					
				et_project.modules.pac_generic_modules.query_element (
					position	=> module_cursor,
					process		=> query_nets'access);

			end collect_nets;
			
			submod_tree : et_numbering.type_modules.tree := et_numbering.type_modules.empty_tree;
			tree_cursor : et_numbering.type_modules.cursor := et_numbering.type_modules.root (submod_tree);
			
			function make_prefix return et_general.type_net_name.bounded_string is
			-- Builds a string like CLK_GENERATOR/FLT1/ from the parent submodule instances.
			-- Starts at the position of the current tree_cursor and goes up to the first submodule level.
			-- NOTE: The nets in the top module do not have prefixes.
				prefix : et_general.type_net_name.bounded_string;
				use et_numbering.type_modules;
				cursor : et_numbering.type_modules.cursor := tree_cursor;
			begin
				-- The first prefix to PREPEND is the name of the current submodule instance:
				prefix := et_netlists.to_prefix (element (cursor).instance);

				-- look for the overlying parent submodule
				cursor := parent (cursor);

				-- travel upwards toward other overlying submodules. The search ends as
				-- soon as the top module has been reached.
				while not is_root (cursor) loop
					-- prepend instance name of parent submodule
					prefix := et_netlists.to_prefix (element (cursor).instance) & prefix;
					cursor := parent (cursor);
				end loop;
					
				return prefix;
			end make_prefix;
			
			-- A stack keeps record of the submodule level where tree_cursor is pointing at.
			package stack_level is new et_general.stack_lifo (
				item	=> et_numbering.type_modules.cursor,
				max 	=> et_submodules.nesting_depth_max);

			-- Another stack keeps record of the assembly variant at the submodule level.
			package stack_variant is new et_general.stack_lifo (
				item	=> et_general.type_variant_name.bounded_string,
				max 	=> et_submodules.nesting_depth_max);
			
			variant : et_general.type_variant_name.bounded_string; -- low_cost
			
			procedure query_submodules is 
			-- Reads the submodule tree submod_tree. It is recursive, means it calls itself
			-- until the deepest submodule (the bottom of the design structure) has been reached.
				use et_numbering.type_modules;
				module_name 	: type_module_name.bounded_string; -- motor_driver
				parent_name 	: type_module_name.bounded_string; -- water_pump
				module_instance	: et_general.type_module_instance_name.bounded_string; -- MOT_DRV_3
				offset			: type_name_index;

				use et_assembly_variants.type_submodules;
				alt_submod : et_assembly_variants.type_submodules.cursor;

				procedure insert_submodule is begin
				-- Insert a submodule in netlist_tree. Wherever procedure query_submodules is
				-- called, cursor netlist_cursor is pointing at the latest parent module. The
				-- submodules detected here must be inserted as children of that parent module.

					-- backup netlist_cursor
					stack_netlist.push (netlist_cursor);
					
					et_netlists.type_modules.insert_child (
						container	=> netlist_tree,
						parent		=> netlist_cursor,
						before		=> et_netlists.type_modules.no_element,
						position	=> netlist_cursor, -- points afterwards to the child that has just been inserted
						new_item	=> (
							generic_name	=> module_name,
							instance_name	=> module_instance,
							others			=> <>)
						);

					-- Collect nets from current module. inserts the nets in
					-- the submodule indicated by netlist_cursor:
					collect_nets (
						module_cursor	=> locate_module (module_name),
						variant			=> variant,
						prefix			=> make_prefix,
						offset			=> offset);
					
					-- restore netlist_cursor
					netlist_cursor := stack_netlist.pop;
				end insert_submodule;
				
			begin -- query_submodules
				log_indentation_up;

				-- start with the first submodule on the current hierarchy level
				tree_cursor := first_child (tree_cursor);

				-- iterate through the submodules on this level
				while tree_cursor /= et_numbering.type_modules.no_element loop
					module_name := element (tree_cursor).name;
					module_instance := element (tree_cursor).instance;

					log (text => "instance " & enclose_in_quotes (to_string (module_instance)) &
						" of generic module " & enclose_in_quotes (to_string (module_name)),
						level => log_threshold + 1);

					-- In case we are on the first level, the parent module is the given top module.
					-- In that case the parent variant is the given variant of the top module.
					-- If the top module has the default variant, all submodules in all levels
					-- assume default variant too.
					if parent (tree_cursor) = root (submod_tree) then
						parent_name := key (make_netlists.module_cursor);
						variant := variant_name; -- argument of make_for_variant
					else
						parent_name := element (parent (tree_cursor)).name;
					end if;

					-- Get the device name offset of the current submodule;
					offset := element (tree_cursor).device_names_offset;

					if not is_default (variant) then
						-- Query in parent module: Is there any assembly variant specified for this submodule ?

						alt_submod := alternative_submodule (
									module	=> locate_module (parent_name),
									variant	=> variant,
									submod	=> module_instance);

						if alt_submod = et_assembly_variants.type_submodules.no_element then
						-- no variant specified for this submodule -> collect devices of default variant

							variant := default;
						else
						-- alternative variant specified for this submodule
							variant := element (alt_submod).variant;
						end if;

					end if;

					-- Insert submodule in netlist_tree.
					insert_submodule;
					
					if first_child (tree_cursor) = et_numbering.type_modules.no_element then 
					-- No submodules on the current level. means we can't go deeper:
						
						log_indentation_up;
						log (text => "no submodules here -> bottom reached", level => log_threshold + 1);
						log_indentation_down;
					else
					-- There are submodules on the current level:
						
						-- backup the cursor to the current submodule on this level
						stack_level.push (tree_cursor);

						-- backup the parent assembly variant
						stack_variant.push (variant);

						-- iterate through submodules on the level below
						query_submodules; -- this is recursive !

						-- restore cursor to submodule (see stack_level.push above)
						tree_cursor := stack_level.pop;

						-- restore the parent assembly variant (see stack_variant.push above)
						variant := stack_variant.pop;
					end if;

					next_sibling (tree_cursor); -- next submodule on this level
				end loop;
				
				log_indentation_down;

				exception
					when event: others =>
						log_indentation_reset;
						log (text => ada.exceptions.exception_information (event), console => true);
						raise;
				
			end query_submodules;

			-- before updating the netlist of the module we keep the new netlist here temporarily:
			netlist : et_netlists.type_netlist.tree;

			procedure update_netlist (
			-- Updates the netlist of the module. The netlist is indicated by the variant_name.
				module_name		: in type_module_name.bounded_string;
				module			: in out et_schematic.type_module) is

				procedure assign_netlist (
					variant		: in et_general.type_variant_name.bounded_string;
					netlist		: in out et_netlists.type_netlist.tree) is
				begin
					-- overwrite the current netlist by the new netlist:
					netlist := make_for_variant.netlist;
				end assign_netlist;

				use et_schematic.type_netlists;
				netlist_cursor : et_schematic.type_netlists.cursor;
				
			begin -- update_netlist
				log (text => "updating netlist ...", level => log_threshold + 2);
				
				-- Locate the netlist within the module.
				-- If the netlist does not exist yet, insert it in module.netlists.
				-- If the netlist does exist, overwrite it by the new netlist.
				netlist_cursor := find (module.netlists, variant_name);

				if netlist_cursor = type_netlists.no_element then

					type_netlists.insert (
						container	=> module.netlists,
						key			=> variant_name,
						new_item	=> make_for_variant.netlist); -- the new netlist

				else
					type_netlists.update_element (
						container	=> module.netlists,
						position	=> netlist_cursor,
						process		=> assign_netlist'access);

				end if;
			end update_netlist;
			
		begin -- make_for_variant
			if is_default (variant_name) then
				log (text => "default assembly variant ", level => log_threshold + 1);
			else
				log (text => "assembly variant " &
					enclose_in_quotes (to_string (variant_name)), level => log_threshold + 1);
			end if;

			log_indentation_up;

			
			-- take a copy of the submodule tree of the given top module:
			submod_tree := element (module_cursor).submod_tree;

			-- set the cursor inside the tree at root position:
			tree_cursor := et_numbering.type_modules.root (submod_tree);
			
			stack_level.init;
			stack_variant.init;
			stack_netlist.init;

			-- Insert the top module in the netlist_tree. It is the only node on this level.
			-- Submodules will be inserted as children of the top module (where netlist_cursor 
			-- points at AFTER this statement):
			et_netlists.type_modules.insert_child (
				container	=> netlist_tree,
				parent		=> et_netlists.type_modules.root (netlist_tree),
				before		=> et_netlists.type_modules.no_element,
				position	=> netlist_cursor,
				new_item	=> (
					generic_name	=> key (make_netlists.module_cursor),
					instance_name	=> to_instance_name (""), -- the top module has no instance name
					others 			=> <>)
				);
			-- netlist_cursor now points at the top module in netlist_tree.

			-- Collect nets of the given top module. the top module has no device index offset.
			-- The nets will be inserted where netlist_cursor points at.
			collect_nets (
				module_cursor	=> module_cursor,
				variant			=> variant_name,
				prefix			=> to_net_name (""), -- no net prefix in top module
				offset			=> 0); 

			-- collect devices of the submodules
			query_submodules;
			
			-- Container netlist_tree is now ready for further processing.
			-- It contains the modules and their nets ordered in a tree structure.
			-- But the connections between nets are
			-- still unknown and will be analyzed now:
			netlist := et_netlists.make_netlist (
				modules			=> netlist_tree,	
				module_name		=> key (module_cursor), -- motor_driver (to be written in the netlist file header)
				variant_name	=> variant_name, 	-- low_cost, empty if default variant
				write_file		=> write_files,
				log_threshold	=> log_threshold);

			-- Now netlist provides information on primary nets and their subordinated secondary nets.
			
			-- Update the netlist (indicated by variant_name) in the module by variable "netlist".
			-- NOTE: This is about the internal netlist (module.netlists) and has nothing to do
			-- with netlist files:
			et_project.modules.pac_generic_modules.update_element (
				container		=> generic_modules,
				position		=> module_cursor,
				process			=> update_netlist'access);
			
			log_indentation_down;
		end make_for_variant;
		
		procedure query_variant (variant_cursor : in et_assembly_variants.pac_variants.cursor) is
			use et_general.type_variant_name;
		begin
			make_for_variant (key (variant_cursor));
		end query_variant;
			
	begin -- make_netlists
		log (text => "generating netlists ...", level => log_threshold);
		log_indentation_up;

		-- Build the submodule tree of the module according to the current design structure in
		-- et_schematic.type_module.submod_tree.
		-- All further operations rely on this tree:
		build_submodules_tree (
			module_name 	=> key (module_cursor),
			log_threshold	=> log_threshold + 1);

		-- make netlist of default variant
		make_for_variant (default);

		-- make netlists of other variants
		et_assembly_variants.pac_variants.iterate (element (module_cursor).variants, query_variant'access);
		
		log_indentation_down;
	end make_netlists;

	procedure make_netlists (
	-- Generates the netlist files of all assembly variants from the given top module.
	-- The netlist files are named after the module name and the variant name.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module
	begin
		-- locate the given top module
		module_cursor := locate_module (module_name);

		make_netlists (
			module_cursor	=> module_cursor,
			write_files		=> true,
			log_threshold	=> log_threshold);
	end;
	
	procedure check_integrity (
	-- Performs an in depth check on the schematic of the given module.
	-- Tests:
	-- 1. for device/submodule/netchanger ports that do not have a same named device/submodule/netchanger.
	-- 2. for device/submodule/netchanger ports that occur more than once.
	-- 3. CS: for net junctions sitting on top of each other
	-- 4. CS: for device/submodule/netchanger port that do not have a visual connection to the net
	-- 5. CS: for overlapping net segments
	-- 6. CS: unconnected ports of R, C, L (category depended)
	-- 6.1 CS: unconnected inputs
	-- 7. CS: devices with empty values
	-- 8. CS: interactive devices with empty purpose
	-- 9. CS: check partcode (et_conventions.validate_partcode)
	-- 10. units sitting on to of each other (same origin position)
	-- 11. CS: warning (or error ?) if any ports sit on top of each other. This would make the movable_tests obsolete.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module being checked

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
				net		: in type_net_name.bounded_string) is 
				use et_symbols;
			begin
			-- Collect device ports. exception will be raised of port occurs more than once.
				insert (device_port_collector, port);

				exception when event: others =>
					log (ERROR, "net " & to_string (net) &
						" device " & to_string (port.device_name) &
						" port " & to_string (port.port_name) &
						" already used !",
						console => true);
					-- CS: show the net, sheet, xy where the port is in use already

					log (text => ada.exceptions.exception_message (event), console => true);
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
					log (ERROR, "net " & to_string (net) &
						" submodule " & et_general.to_string (port.module_name) &
						" port " & et_general.to_string (port.port_name) &
						" already used !",
						console => true);
					-- CS: show the net, sheet, xy where the port is in use already

					log (text => ada.exceptions.exception_message (event), console => true);
			end collect_submodule_port;

			-- Here we collect all ports of netchangers (like netchanger port master/slave) across all the nets.
			-- Since netchanger_ports_collector is an ordered set, an exception will be raised if
			-- a port is to be inserted more than once. Something like "netchanger port master" must
			-- occur only ONCE throughout the module.
			use et_netlists.type_ports_netchanger;
			netchanger_ports_collector : et_netlists.type_ports_netchanger.set;

			procedure collect_netchanger_port (
				port	: in et_netlists.type_port_netchanger;
				net		: in type_net_name.bounded_string)
			is begin
			-- Collect netchanger ports. exception will be raised of port occurs more than once.
				insert (netchanger_ports_collector, port);

				exception when event: others =>
					log (ERROR, "net " & to_string (net) &
						" netchanger" & et_submodules.to_string (port.index) &
						" port" & et_submodules.to_string (port.port) &
						" already used !",
						console => true);
					-- CS: show the net, sheet, xy where the port is in use already

					log (text => ada.exceptions.exception_message (event), console => true);
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
									procedure query_port (port_cursor : in type_ports_device.cursor) is 
										use et_symbols;
									begin
										log (text => "device " & to_string (element (port_cursor).device_name) &
											 " port " & to_string (element (port_cursor).port_name), level => log_threshold + 4);

										if not exists_device_port (
											module_cursor	=> module_cursor,
											device_name		=> element (port_cursor).device_name,
											port_name		=> element (port_cursor).port_name) then

											error;
											
											log (ERROR, "device " & to_string (element (port_cursor).device_name) &
												 " port " & to_string (element (port_cursor).port_name) &
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
										log (text => "submodule " & et_general.to_string (element (port_cursor).module_name) &
											 " port " & et_general.to_string (element (port_cursor).port_name), level => log_threshold + 4);

										if not exists_submodule_port (
											module_cursor	=> module_cursor,
											submod_instance	=> element (port_cursor).module_name, -- MOT_DRV_3
											port_name		=> element (port_cursor).port_name) then -- RESET

											error;
											
											log (ERROR, "submodule " & et_general.to_string (element (port_cursor).module_name) &
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
									use et_netlists;
									
									procedure query_port (port_cursor : in type_ports_netchanger.cursor) is begin
										log (text => "netchanger " & et_submodules.to_string (element (port_cursor).index) &
											 " port " & et_submodules.to_string (element (port_cursor).port), level => log_threshold + 4);

										if not exists_netchanger (
											module_cursor	=> module_cursor,
											index			=> element (port_cursor).index) then -- 1, 2, 3, ...

											error;
											
											log (ERROR, "netchanger" & et_submodules.to_string (element (port_cursor).index) &
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
								log (text => to_string (segment_cursor), level => log_threshold + 3);

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
						log (text => "strand " & to_string (position => element (strand_cursor).position), level => log_threshold + 2);
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
				log (text => "net " & et_general.to_string (key (net_cursor)), level => log_threshold + 1);

				query_element (
					position	=> net_cursor,
					process		=> query_strands'access);
				
			end query_net;
			
		begin -- query_nets
			iterate (module.nets, query_net'access);
		end query_nets;

	begin -- check_integrity
		log (text => "module " & to_string (module_name) & " integrity check ...", level => log_threshold);
		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);
		
		-- check nets
		query_element (
			position	=> module_cursor,
			process		=> query_nets'access);

		-- check unit positions (units sitting on top of each other)
		if not unit_positions_valid (module_cursor, log_threshold + 1) then
			error;
		end if;

		-- netlist checks
		make_netlists (
			module_cursor	=> module_cursor,
			write_files		=> false,
			log_threshold	=> log_threshold + 1);

		
		if errors > 0 then
			log (WARNING, "integrity check found errors !");
			log (text => "errors   :" & natural'image (errors));
		end if;

		if warnings > 0 then
			log (WARNING, "integrity check issued warnings !");
			log (text => "warnings :" & natural'image (warnings));
		end if;

		log_indentation_down;
	end check_integrity;

	function to_string (
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- C
		query_result	: in type_unit_query)
		return string is 
	begin
		if query_result.exists then
			if pac_unit_name.length (unit_name) > 0 then
				return "Location of device " & to_string (device_name)
					& " unit " & to_string (unit_name)
					& " :" & to_string (query_result.position);
			else
				return "Location of device " & to_string (device_name)
					& " :" & to_string (query_result.position);
			end if;
		else
			return "device " & to_string (device_name)
				& " unit " & to_string (unit_name)
				& " does not exist !";
		end if;
	end to_string;
	
	function unit_position (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string) -- C
		return type_unit_query is

		exists : boolean := false;
		pos : et_coordinates.type_position; -- x/y, rotation, sheet
		
		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is

			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;

			procedure query_units (
				device_name	: in type_device_name; -- IC45
				device		: in et_schematic.type_device) is

				use et_schematic.type_units;
				unit_cursor : et_schematic.type_units.cursor;
				
			begin
				-- If the given unit_name contains something, locate the unit
				-- by its name. If unit_name is empty, locate the first unit.
				if pac_unit_name.length (unit_name) > 0 then -- locate by name
					
					unit_cursor := type_units.find (device.units, unit_name);

					if unit_cursor /= type_units.no_element then -- unit exists
						exists := true;
						pos := element (unit_cursor).position;
					else
						exists := false; -- unit does not exist
					end if;
					
				else -- locate the first unit:
					unit_cursor := type_units.first (device.units);
					-- There should be at least one unit. Otherwise raise constraint_error.

					if unit_cursor /= type_units.no_element then -- unit exists
						exists := true;
						pos := element (unit_cursor).position;
					else
						exists := false; -- unit does not exist
						raise constraint_error; -- CS do something
					end if;
					
				end if;
			end query_units;
			
		begin -- query_devices
			-- locate the device:
			device_cursor := et_schematic.type_devices.find (module.devices, device_name);

			if device_cursor /= et_schematic.type_devices.no_element then -- device exists
				et_schematic.type_devices.query_element (device_cursor, query_units'access);
			else
				exists := false; -- device does not exist
			end if;
			
		end query_devices;
		
	begin -- unit_position
		query_element (module_cursor, query_devices'access);

		if exists then return (exists => true, position => pos);
		else return (exists => false);
		end if;
		
	end unit_position;
	
end et_schematic_ops;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
