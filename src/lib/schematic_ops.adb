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
with submodules;
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
	-- If a port lands on either the start or end point of a segment, it will
	-- be regarded as "connected" with the segment.
	-- If a ports lands between start or end point of a segment, nothing happens
	-- as the docking to net segments is possible on segment ends/starts only.
	-- CS: Automatic splitting the segment into two and placing a junction is not supported
	-- jet and probably not a good idea.
		module			: in type_modules.cursor;		-- the module
		device			: in type_device_name;			-- the device
		ports			: in et_libraries.type_ports.map; -- the unit ports
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
					if element (port_cursor).name = port_name then
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
	
	procedure check_integrity (
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
