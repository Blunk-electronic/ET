------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         SCHEMATIC OPERATIONS                             --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                -- 
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
with ada.strings.unbounded;			use ada.strings.unbounded;
with ada.directories;
with ada.exceptions;				use ada.exceptions;

with et_exceptions;					use et_exceptions;

with et_modes;						use et_modes;
with et_conventions;
with et_pcb_coordinates_2;
with et_pcb;
with et_terminals;
with et_packages;
with et_device_rw;
with et_device_query_schematic;		use et_device_query_schematic;
with et_board_ops.ratsnest;			use et_board_ops.ratsnest;

with et_canvas_schematic_2;


package body et_schematic_ops is


	use et_submodules.pac_netchangers;
	use et_submodules.pac_submodules;
	use pac_strands;



	function get_basic_meta_information (
		module : in pac_generic_modules.cursor)
		return et_meta.type_basic
	is 
		use et_meta;
	begin
		return type_basic (element (module).meta.schematic);
	end get_basic_meta_information;
	

	
	function get_preferred_libraries (
		module : in pac_generic_modules.cursor)
		return et_meta.pac_preferred_libraries_schematic.list
	is begin
		return element (module).meta.schematic.preferred_libs;
	end get_preferred_libraries;


	
	
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

	

	
	procedure net_not_found (name : in pac_net_name.bounded_string) is begin
		raise semantic_error_1 with
			"ERROR ! Net " & enclose_in_quotes (to_string (name)) & " not found !";
	end;

	
	
	procedure assembly_variant_not_found (variant : in pac_assembly_variant_name.bounded_string) is 
	begin
		log (ERROR, "assembly variant " &
			 enclose_in_quotes (to_variant (variant)) & " not found !", console => true);
		raise constraint_error;
	end;

	

	
	procedure dragging_not_possible (
		port 		: in string;
		position	: in et_coordinates_2.type_position) is
	begin
		log (ERROR, "port " & enclose_in_quotes (port) &
			 " is directly connected with other ports at" &
			to_string (position => position) &
			 ". Dragging not possible !",
			 console => true);
		raise constraint_error;
	end;



	
	
	procedure log_unit_positions (
		positions 		: in pac_unit_positions.map;
		log_threshold	: in type_log_level) is
		
		procedure write (cursor : in pac_unit_positions.cursor) is begin
			log (text => 
				"unit " &
				to_string (pac_unit_positions.key (cursor)) & -- unit name
				et_coordinates_2.to_string (position => pac_unit_positions.element (cursor)), -- sheet x y
				level => log_threshold);
		end;
		
	begin
		log (text => "location(s) in schematic:", level => log_threshold);
		log_indentation_up;
		pac_unit_positions.iterate (positions, write'access);
		log_indentation_down;
	end;

	
	-- Writes the position of the package in the log file. If device is virtual, nothing happens.
	procedure log_package_position (
		device_cursor	: in pac_devices_sch.cursor;
		log_threshold	: in type_log_level) 
	is
		use et_pcb_coordinates_2;
		use et_pcb_coordinates_2.pac_geometry_2;
		use pac_devices_sch;
		use et_symbols;
	begin
		if element (device_cursor).appearance = PCB then
			log (text => "location in board:" & 
				to_string (element (device_cursor).position.place) &
				" face" & 
				to_string (get_face (element (device_cursor).position)),
				level => log_threshold);
		end if;
	end;

	
	function positions_of_units (
	-- Collects the positions of all units (in schematic) of the given device and returns
	-- them in a list.
		device_cursor : in pac_devices_sch.cursor) 
		return pac_unit_positions.map is

		-- temporarily storage of unit coordinates:
		positions : pac_unit_positions.map;
		
		procedure get_positions (
			device_name : in type_device_name;
			device		: in type_device_sch) is
		begin
			positions := unit_positions (device.units);
		end;

	begin -- positions_of_units
		pac_devices_sch.query_element (
			position	=> device_cursor,
			process		=> get_positions'access);

		return positions;
	end;

	
	procedure delete_ports (
		module			: in pac_generic_modules.cursor;		-- the module
		device			: in type_device_name;			-- the device
		ports			: in et_symbols.pac_ports.map := et_symbols.pac_ports.empty_map; -- the ports (if empty, all ports of the device will be deleted)
		sheets			: in pac_unit_positions.map;	-- the sheet numbers where the units can be found. CS implementation required
		log_threshold	: in type_log_level) 
	is
		dedicated_ports : boolean := false; -- goes true if "ports" contains something.
		
		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is

			procedure query_net (net_cursor : in pac_nets.cursor) is
				use pac_nets;

				procedure query_strands (
					net_name	: in pac_net_name.bounded_string;
					net			: in out type_net) 
				is
					procedure query_strand (strand_cursor : in pac_strands.cursor) is
						procedure query_segments (strand : in out type_strand) is
							use pac_net_segments;

							procedure query_segment (segment_cursor : in pac_net_segments.cursor) is 
								use pac_device_ports;

								procedure query_ports (segment : in out type_net_segment) is
								-- Tests device ports of given segment if their device name matches the given device name.
								-- On match the port is skipped. All other ports are collected in ports_new.	
									ports_new : pac_device_ports.set;
									
									procedure query_port (port_cursor : in pac_device_ports.cursor) is
										use et_symbols;
										port : type_device_port := element (port_cursor); -- take a copy of the port
									begin -- query_port
										if port.device_name = device then -- on match just report the port and skip it

											log_indentation_up;
											
											if dedicated_ports then
												if et_symbols.pac_ports.contains (ports, port.port_name) then
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
									iterate (segment.ports.devices, query_port'access); -- loop in portlist of given segment

									-- overwrite old portlist by new portlist
									segment.ports.devices := ports_new;
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
			pac_nets.iterate (module.nets, query_net'access);
		end query_nets;
		
		
	begin -- delete_ports
		log (text => "deleting device ports in nets ...", level => log_threshold);

		-- If ports are provided, we have to delete exactly those in list "ports".
		-- The flag dedicated_ports is later required in order to do this job:
		if et_symbols.pac_ports.length (ports) > 0 then
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
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified
		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_devices_sch;
			device_cursor : pac_devices_sch.cursor;

			-- temporarily storage of unit coordinates:
			position_of_units : pac_unit_positions.map;
			
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

				update_ratsnest (module_cursor, log_threshold + 1);
				
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
	
	
	function get_ports_of_unit (
		device_cursor	: in pac_devices_sch.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return et_symbols.pac_ports.map 
	is

		use et_symbols;
		ports : et_symbols.pac_ports.map; -- to be returned
		
		model : pac_device_model_file.bounded_string; -- ../libraries/devices/transistor/pnp.dev
		device_cursor_lib : pac_devices_lib.cursor;

		
		procedure query_internal_units (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_lib) 
		is
			use pac_units_internal;
			unit_cursor : pac_units_internal.cursor;
		begin
			-- locate the given unit among the internal units
			unit_cursor := find (device.units_internal, unit_name);

			-- Fetch the ports of the internal unit.
			-- Transfer the ports to the portlist to be returned:			
			-- CS: constraint_error arises here if unit can not be located.
			ports := element (unit_cursor).symbol.ports;
		end query_internal_units;

		
		procedure query_external_units (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_lib) 
		is
			use pac_units_external;
			unit_cursor : pac_units_external.cursor;
			sym_model : pac_symbol_model_file.bounded_string; -- like /libraries/symbols/NAND.sym

			procedure query_symbol (
			-- Appends the ports names of the external unit to the portlist to 
			-- be returned.
				symbol_name	: in pac_symbol_model_file.bounded_string;
				symbol		: in type_symbol ) 
			is begin
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
				pac_symbols.query_element (
					position	=> pac_symbols.find (symbols, sym_model),
					process		=> query_symbol'access);
			end if;
			
		end query_external_units;
		
		
	begin -- get_ports_of_unit

		-- Fetch the model name of the given device. 
		model := pac_devices_sch.element (device_cursor).model;

		-- Get cursor to device in device library (the model name is the key into the device library).
		-- CS: constraint_error will arise here if no associated device exists.
		device_cursor_lib := pac_devices_lib.find (devices, model);

		-- Query external units of device (in library). It is most likely that
		-- the unit is among the external units:
		pac_devices_lib.query_element (
			position	=> device_cursor_lib,
			process		=> query_external_units'access);

		-- If unit could not be found among external units then look up the internal units:
		if et_symbols.pac_ports.length (ports) = 0 then

			-- Query internal units of device (in library):
			pac_devices_lib.query_element (
				position	=> device_cursor_lib,
				process		=> query_internal_units'access);
		end if;

		-- If still no ports found, we have a problem:
		if et_symbols.pac_ports.length (ports) = 0 then
			raise constraint_error;
		end if;
		
		return ports;

		exception
			when event: others =>
				log_indentation_reset;
				log (text => ada.exceptions.exception_information (event), console => true);
				raise;
		
	end get_ports_of_unit;

	
	procedure move_ports (
		ports	: in out et_symbols.pac_ports.map; -- the portlist
		offset	: in et_coordinates_2.type_position) -- the offset (only x/y matters)
	is
		use et_symbols;
		use et_symbols.pac_ports;

		procedure move (
			name	: in pac_port_name.bounded_string;
			port	: in out type_port) 
		is begin
			move_by (port.position, to_distance_relative (offset.place));
		end;

		procedure query_port (cursor : in et_symbols.pac_ports.cursor) is begin
			update_element (
				container	=> ports,
				position	=> cursor,
				process		=> move'access);
		end;
			
	begin -- move_ports
		iterate (ports, query_port'access);
	end move_ports;

	
	function position (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC34
		port_name		: in et_symbols.pac_port_name.bounded_string; -- CE
		log_threshold	: in type_log_level)
		return et_coordinates_2.type_position 
	is
		port_position : et_coordinates_2.type_position; -- to be returned		
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being inquired

		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) 
		is
			use pac_devices_sch;
			device_cursor : pac_devices_sch.cursor;
			unit_position : et_coordinates_2.type_position;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in type_device_sch) 
			is				
				use pac_units;
				unit_cursor : pac_units.cursor := device.units.first;
				unit_name : pac_unit_name.bounded_string;
				
				use et_symbols.pac_ports;
				ports : et_symbols.pac_ports.map;
				port_cursor : et_symbols.pac_ports.cursor;
			begin
				-- Locate unit in schematic device:
				while unit_cursor /= pac_units.no_element loop

					-- Load the default xy-positions of ports relative to the center of the unit.
					unit_name := key (unit_cursor);
					ports := get_ports_of_unit (device_cursor, unit_name);

					-- If the unit has a port named port_name: 
					if contains (ports, port_name) then -- port found
						
						-- calculate the port position in the schematic
						unit_position := element (unit_cursor).position; -- unit pos. in schematic

						port_cursor := find (ports, port_name);
						port_position := et_coordinates_2.to_position (
							sheet	=> get_sheet (unit_position), -- the sheet where the unit is
							point	=> element (port_cursor).position -- default xy pos of port
							);														 

						-- Calculate the absolute port position in schematic by
						-- first rotating port_xy, and then moving port_xy:
						
						rotate_by (
							point		=> port_position.place,
							rotation	=> get_rotation (element (unit_cursor).position));
						
						-- CS mirror ?
						
						-- Calculate the absolute port position in the schematic:
						move_by (
							point 	=> port_position.place,
							offset	=> to_distance_relative (unit_position.place));
						
						exit; -- no need to look at other units
					end if;
					
					next (unit_cursor);
				end loop;
			end query_units;
			
		begin -- query_devices
			if contains (module.devices, device_name) then
				device_cursor := find (module.devices, device_name); -- the device should be there

				log_indentation_up;
				
				pac_devices_sch.query_element (
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

	

	


	
	procedure insert_ports (
		module			: in pac_generic_modules.cursor;		-- the module
		device			: in type_device_name;					-- the device
		unit			: in pac_unit_name.bounded_string;	-- the unit name like A, C, PWR
		ports			: in et_symbols.pac_ports.map; -- the ports to be inserted
		sheet			: in type_sheet;				-- the sheet to look at
		log_threshold	: in type_log_level)
	is

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is

			procedure query_net (net_cursor : in pac_nets.cursor) is
				use pac_nets;

				procedure query_strands (
					net_name	: in pac_net_name.bounded_string;
					net			: in out type_net) 
				is
					strand_cursor : pac_strands.cursor;

					use et_symbols;
					use et_symbols.pac_ports;
					port_cursor : et_symbols.pac_ports.cursor := ports.first;

					port_processed : boolean;
					
					procedure query_segments (strand : in out type_strand) is
						use pac_net_segments;
						segment_cursor : pac_net_segments.cursor := strand.segments.first;

						procedure change_segment (segment : in out type_net_segment) is
						begin -- change_segment
							-- If port sits on start OR end point of segment AND if it
							-- is not already in the segment then append it to segment.ports.devices.
							-- append it to the portlist of the segment.
							if 	segment.start_point = element (port_cursor).position or
								segment.end_point = element (port_cursor).position then

								-- If port not already in segment, append it.
								-- Otherwise it must not be appended again. constraint_error would arise.
								if pac_device_ports.contains (
									container	=> segment.ports.devices,
									item		=> (
											device_name	=> device,
											unit_name	=> unit, -- A, C, PWR
											port_name	=> key (port_cursor)) -- IC23, VCC_IO
									) then 

									log (text => " already there -> skipped", level => log_threshold + 3);
								else
									pac_device_ports.insert (
										container	=> segment.ports.devices,
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
						while not port_processed and segment_cursor /= pac_net_segments.no_element loop

							log_indentation_up;
							log (text => "probing " & to_string (segment_cursor), level => log_threshold + 2);
							
							pac_net_segments.update_element (
								container	=> strand.segments,
								position	=> segment_cursor,
								process		=> change_segment'access);

							log_indentation_down;
							next (segment_cursor);
						end loop;

					end query_segments;
				
				begin -- query_strands
					-- loop in portlist
					while port_cursor /= et_symbols.pac_ports.no_element loop
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
						while strand_cursor /= pac_strands.no_element loop

							-- We pick out only the strands on the targeted sheet:
							if get_sheet (element (strand_cursor).position) = sheet then
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
			pac_nets.iterate (module.nets, query_net'access);
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
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		meaning			: in type_placeholder_meaning; -- name, value, purpose
		log_threshold	: in type_log_level)
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use et_symbols;
		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_devices_sch;
			device_cursor : pac_devices_sch.cursor;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in out type_device_sch)
			is
				use pac_units;
				unit_cursor : pac_units.cursor;

				procedure move_placeholder (
					unit_name	: in pac_unit_name.bounded_string;
					unit		: in out type_unit)
				is
					-- In case absolute movement is required, calculate the
					-- new position of the placeholder relative to the unit origin:
					pos_abs : constant type_vector_model :=
						to_point (get_distance_relative (unit.position.place, point));
					
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
										offset	=> to_distance_relative (point));
							end case;
							
						when VALUE =>
							case coordinates is
								when ABSOLUTE =>
									unit.value.position := pos_abs;

								when RELATIVE =>
									move_by (
										point	=> unit.value.position,
										offset	=> to_distance_relative (point));
							end case;
							
						when PURPOSE =>
							case coordinates is
								when ABSOLUTE =>
									unit.purpose.position := pos_abs;

								when RELATIVE =>
									move_by (
										point	=> unit.purpose.position,
										offset	=> to_distance_relative (point));
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
		ports	: in out et_symbols.pac_ports.map; -- the portlist
		angle	: in et_coordinates_2.type_rotation_model)  -- 90
	is
		use et_symbols;
		use et_symbols.pac_ports;

		procedure rotate (
			name	: in pac_port_name.bounded_string;
			port	: in out type_port) 
		is begin
			rotate_by (port.position, angle);
		end;

		procedure query_port (cursor : in et_symbols.pac_ports.cursor) is begin
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
		device_cursor	: in pac_devices_sch.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return et_symbols.type_default_text_positions is
		
		use et_symbols;
		use pac_devices_sch;

		-- The positions to be returned depend on the appearance of the requested device:
		result : type_default_text_positions (element (device_cursor).appearance); -- to be returned
		
		model : pac_device_model_file.bounded_string; -- ../libraries/devices/transistor/pnp.dev
		device_cursor_lib : pac_devices_lib.cursor;
		
		use et_symbols.pac_texts;

		procedure query_text (c : in et_symbols.pac_texts.cursor) is 
		-- Appends a text position (x/y) the the result.
			use pac_text_positions;
		begin
			append (result.texts, element (c).position);
		end;

		-- Indicates whether the unit is internal or external:
		unit_status : type_unit_ext_int := EXT;
		
		procedure query_internal_units (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_lib) is
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
			device	: in type_device_lib) is
			use pac_units_external;
			unit_cursor : pac_units_external.cursor;
			sym_model : pac_symbol_model_file.bounded_string; -- like /libraries/symbols/NAND.sym

			procedure query_symbol (
				symbol_name	: in pac_symbol_model_file.bounded_string;
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
				pac_symbols.query_element (
					position	=> pac_symbols.find (symbols, sym_model),
					process		=> query_symbol'access);
			else
				unit_status := INT;
			end if;
			
		end query_external_units;
		
	begin -- default_text_positions

		-- Fetch the model name of the given device. 
		model := pac_devices_sch.element (device_cursor).model;

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

	

	

	
	procedure rotate_unit_placeholder (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		rotation		: in et_text.type_rotation_documentation; -- absolute ! -- 90
		meaning			: in type_placeholder_meaning; -- name, value, purpose		
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use et_symbols;
		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_devices_sch;
			device_cursor : pac_devices_sch.cursor;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in out type_device_sch) 
			is
				use pac_units;
				unit_cursor : pac_units.cursor;

				procedure rotate_placeholder (
					name	: in pac_unit_name.bounded_string; -- A
					unit	: in out type_unit) 
				is begin
					case meaning is
						when et_device_placeholders.NAME =>
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

					pac_units.update_element (
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
			to_string (rotation), level => log_threshold);
		
		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end rotate_unit_placeholder;

	
	function locate_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string)		
		return pac_nets.cursor 
	is	
		cursor : pac_nets.cursor;

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) 
		is begin
			cursor := pac_nets.find (module.nets, net_name);
		end query_nets;
		
	begin -- locate_net
		query_element (
			position	=> module_cursor,
			process		=> query_nets'access);
		
		return cursor;
	end locate_net;


	function get_active_assembly_variant (
		module_cursor	: in pac_generic_modules.cursor)
		return pac_assembly_variant_name.bounded_string
	is begin
		return element (module_cursor).active_variant;
	end get_active_assembly_variant;

	

	function get_active_assembly_variant (
		module_cursor	: in pac_generic_modules.cursor)
		return et_assembly_variants.pac_assembly_variants.cursor
	is
		variant : constant pac_assembly_variant_name.bounded_string := 
			get_active_assembly_variant (module_cursor);
			
		use et_assembly_variants;
		use pac_assembly_variants;
		
		av : pac_assembly_variants.cursor;

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;					   
			module 		: in type_module)
		is begin
			av := find (module.variants, variant);
		end query_module;
	
	begin
		if is_default (variant) then
			av := pac_assembly_variants.no_element;
		else
			pac_generic_modules.query_element (module_cursor, query_module'access);
		end if;
		
		return av;
	end get_active_assembly_variant;

	

	function get_net (
		module		: in pac_generic_modules.cursor;
		device		: in pac_devices_sch.cursor;
		terminal	: in et_terminals.pac_terminal_name.bounded_string)
		return pac_nets.cursor
	is
		result : pac_nets.cursor;

		use et_symbols;

		-- Map from the given terminal to the linked 
		-- unit and port of the device. If the terminal is not linked
		-- to any port then this function will just return a cursor that
		-- points to no net (no_element):
		linked_unit_and_port : constant type_get_port_result :=
			get_port (device, terminal);  -- CE, WE

		
		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) 
		is 
			proceed : aliased boolean := true;
			
			procedure query_net (n : in pac_nets.cursor) is 
				use et_assembly_variants;
				
				-- We search for ALL ports (of devices) in the net.
				-- We assume the default assembly variant.
				ports : constant et_nets.type_ports := 
					get_ports (n, pac_assembly_variants.no_element);

				-- ports.devices now contains all ports and units of devices in
				-- the net indicated by cursor n.

				procedure query_port (p : in pac_device_ports.cursor) is
					use pac_device_ports;
					use pac_port_name;
					use pac_unit_name;
					use pac_devices_sch;
				begin
					--log (text => to_string (element (p).device_name)
						--& " " & et_symbols.to_string (element (p).port_name));
					
					if element (p).device_name = key (device)
					and then element (p).unit_name = linked_unit_and_port.unit
					and then element (p).port_name = linked_unit_and_port.port then
						proceed := false; -- stops the iteration

						-- The result is now the cursor of the
						-- current net:
						result := n;
					end if;
				end query_port;

				
			begin
				-- Iterate through all device ports. Abort as soon
				-- as a unit and port have been found as given in 
				-- linked_unit_and_port:
				iterate (ports.devices, query_port'access, proceed'access);
			end query_net;

			
		begin
			-- Iterate through the nets of the module:
			iterate (module.nets, query_net'access, proceed'access);
		end query_nets;

		
	begin 
		if linked_unit_and_port.linked then
			--log (text => "given port " & et_symbols.to_string (port));
					
			query_element (module, query_nets'access);
		end if;
			
		return result;
	end get_net;


	

	function net_segment_at_place (
	-- Returns true if at given place a net segment starts or ends.
		module_cursor	: in pac_generic_modules.cursor;
		place			: in et_coordinates_2.type_position)
		return boolean is

		-- This flag goes true once a segment has been found.
		segment_found : boolean := false; -- to be returned
		
		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) is

			use pac_nets;			
			net_cursor : pac_nets.cursor := module.nets.first;

			procedure query_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net)
			is
				strand_cursor : pac_strands.cursor := net.strands.first;
				
				procedure query_segments (strand : in type_strand) is
					use pac_net_segments;

					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					procedure probe_segment (segment : in type_net_segment) is begin
						-- if place is a start point of a segment
						if segment.start_point = place.place then
							-- signal iterations in upper level to cancel
							segment_found := true;
						end if;

						-- if place is an end point of a segment
						if segment.end_point = place.place then
							-- signal iterations in upper level to cancel
							segment_found := true;
						end if;
					end probe_segment;
					
				begin -- query_segments
					while not segment_found and segment_cursor /= pac_net_segments.no_element loop
						query_element (
							position	=> segment_cursor,
							process		=> probe_segment'access);
						
						next (segment_cursor);
					end loop;
				end query_segments;
				
			begin -- query_strands
				while not segment_found and strand_cursor /= pac_strands.no_element loop
					
					-- We pick out only the strands on the targeted sheet:
					if get_sheet (element (strand_cursor).position) = get_sheet (place) then

						query_element (
							position	=> strand_cursor,
							process		=> query_segments'access);
					
					end if;
					
					next (strand_cursor);
				end loop;
			end query_strands;
			
		begin -- query_nets
			while not segment_found and net_cursor /= pac_nets.no_element loop

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
		module_cursor	: in pac_generic_modules.cursor;
		place			: in et_coordinates_2.type_position;
		log_threshold	: in type_log_level)
		return type_ports 
	is
		ports : type_ports; -- to be returned

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) 
		is
			use pac_devices_sch;
			use et_submodules.pac_netchangers;			
			
			procedure query_devices (device_cursor : in pac_devices_sch.cursor) is

				procedure query_units (unit_cursor : in pac_units.cursor) is
					use pac_units;
					use pac_unit_name;
					unit_position : et_coordinates_2.type_position;
					ports : et_symbols.pac_ports.map;

					procedure query_port (port_cursor : in et_symbols.pac_ports.cursor) is
						use et_symbols;
						use et_symbols.pac_ports;
					begin
						log (text => "unit " & et_devices.to_string (key (unit_cursor)) &
							 " port " & to_string (key (port_cursor)) &
							 " at" & to_string (element (port_cursor).position),
							 level => log_threshold + 2);

						-- If the port sits at x/y of place then we have a match:
						if element (port_cursor).position = place.place then
							log (text => " match", level => log_threshold + 2);
							
							-- Insert the port in the portlist to be returned:
							pac_device_ports.insert 
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
					if get_sheet (unit_position) = get_sheet (place) then
						log (text => "device " & to_string (key (device_cursor)) & " unit " &
							 pac_unit_name.to_string (key (unit_cursor)), level => log_threshold + 1);
						log_indentation_up;

						ports := get_ports_of_unit (
							device_cursor	=> device_cursor,
							unit_name		=> key (unit_cursor));

						-- CS mirror before rotate or after rotate ?
						--rotate_ports (ports, unit_rotation);
						rotate_ports (ports, get_rotation (unit_position));

						move_ports (ports, unit_position);

						et_symbols.pac_ports.iterate (ports, query_port'access);

						log_indentation_down;
					end if;

				end query_units;
				
			begin -- query_devices
				--log (text => "device " & to_string (key (device_cursor)), level => log_threshold + 1);
				--log_indentation_up;

				pac_units.iterate (
					container	=> element (device_cursor).units,
					process		=> query_units'access);
				
				--log_indentation_down;
			end query_devices;

			procedure query_submodules (submodule_cursor : in et_submodules.pac_submodules.cursor) is
				submodule_position : et_coordinates_2.type_position;
				ports : et_submodules.pac_submodule_ports.map;

				procedure query_port (port_cursor : in et_submodules.pac_submodule_ports.cursor) is
					use et_submodules.pac_submodule_ports;
					use pac_net_name;
				begin
					log (text => "port " & pac_net_name.to_string (key (port_cursor)) &
							" at" & to_string (element (port_cursor).position),
							level => log_threshold + 2);

					-- If the port sits at x/y of place then we have a match:
					if element (port_cursor).position = place.place then
						log (text => " match", level => log_threshold + 2);
						
						-- Insert the port in the portlist to be returned:
						pac_submodule_ports.insert 
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
				if get_sheet (submodule_position) = get_sheet (place) then
					log (text => "submodule " & to_string (key (submodule_cursor)), level => log_threshold + 1);
					log_indentation_up;

					ports := element (submodule_cursor).ports;
					
					et_submodules.move_ports (ports, submodule_position);

					et_submodules.pac_submodule_ports.iterate (ports, query_port'access);

					log_indentation_down;
				end if;
				
			end query_submodules;

			procedure query_netchangers (netchanger_cursor : in et_submodules.pac_netchangers.cursor) is
				netchanger_position : et_coordinates_2.type_position;
				ports : et_submodules.type_netchanger_ports;
				use et_netlists;
			begin -- query_netchangers
				netchanger_position := element (netchanger_cursor).position_sch;

				-- Look at netchangers on the given sheet of place:
				if get_sheet (netchanger_position) = get_sheet (place) then
					log (text => "netchanger " & et_submodules.to_string (key (netchanger_cursor)), level => log_threshold + 1);
					log_indentation_up;	
					
					-- get the absolute port positions of the netchanger
					ports := et_submodules.netchanger_ports (netchanger_cursor);

					-- If the port sits at x/y of place then we have a match.
					-- The match can either be at the msster or slave port.
					-- The match can/should NEVER be at both ports simultaneously.
					
					-- First test whether the master port sits here:
					if ports.master = place.place then

						log (text => "port " & et_submodules.to_string (et_submodules.MASTER) &
							" at" & to_string (ports.master),
							level => log_threshold + 2);
						
						-- Insert the port in the portlist to be returned:
						pac_netchanger_ports.insert 
							(
							container	=> ports_at_place.ports.netchangers,
							new_item	=> 
								(
								index	=> key (netchanger_cursor),
								port	=> et_submodules.MASTER
								)
							);

					-- Second, test wheter slave port sits here:
					elsif ports.slave = place.place then

						log (text => "port " & et_submodules.to_string (et_submodules.SLAVE) &
							" at" & to_string (ports.slave),
							level => log_threshold + 2);
						
						-- Insert the port in the portlist to be returned:
						pac_netchanger_ports.insert 
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
		log (text => "module " & enclose_in_quotes (to_string (key (module_cursor))) &
			 " locating ports at" & to_string (position => place),
			 level => log_threshold);

		log_indentation_up;
		

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
		sheets			: in pac_unit_positions.map;	-- the sheet numbers where the units can be found. CS implementation required
		log_threshold	: in type_log_level) 
	is
		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			procedure query_net (net_cursor : in pac_nets.cursor) is
				use pac_nets;

				procedure query_strands (
					net_name	: in pac_net_name.bounded_string;
					net			: in out type_net) 
				is
					procedure query_strand (strand_cursor : in pac_strands.cursor) is
						procedure query_segments (strand : in out type_strand) is
							use pac_net_segments;

							procedure query_segment (segment_cursor : in pac_net_segments.cursor) is 
								use pac_device_ports;

								procedure query_ports (segment : in out type_net_segment) is
								-- Tests device ports of given segment if their device name matches the given device name.
								-- On match replace the old device name by the new device name.

									port_cursor : pac_device_ports.cursor := segment.ports.devices.first;
									
								begin -- query_ports
									while port_cursor /= pac_device_ports.no_element loop

										if element (port_cursor).device_name = device_before then -- IC1

											replace_element (
												container	=> segment.ports.devices,
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
			pac_nets.iterate (module.nets, query_net'access);
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
		module_name			: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name_before	: in type_device_name; -- IC1
		device_name_after	: in type_device_name; -- IC23
		log_threshold		: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
			use pac_devices_sch;

			device_cursor_before : pac_devices_sch.cursor;
			device_cursor_after  : pac_devices_sch.cursor;
			inserted : boolean;

			-- temporarily storage of unit coordinates:
			position_of_units : pac_unit_positions.map;

		begin -- query_devices
			-- locate the device by the old name
			device_cursor_before := find (module.devices, device_name_before); -- IC1

			if device_cursor_before /= pac_devices_sch.no_element then -- the device should be there

				-- copy elements and properties of the old device to a new one:
				pac_devices_sch.insert (
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
				pac_devices_sch.delete (
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
		module_name			: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name			: in type_device_name; -- R2
		value				: in pac_device_value.bounded_string; -- 470R
		log_threshold		: in type_log_level) is
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
			use pac_devices_sch;

			device_cursor : pac_devices_sch.cursor;

			procedure set_value (
				device_name	: in type_device_name;
				device		: in out type_device_sch) is
			begin
				device.value := value;
			end;

			use et_symbols;
			
		begin -- query_devices
			-- locate the device
			device_cursor := find (module.devices, device_name); -- R1

			if device_cursor /= pac_devices_sch.no_element then -- the device should be there

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
		module_name			: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name			: in type_device_name; -- R2
		purpose				: in pac_device_purpose.bounded_string; -- brightness_control
		log_threshold		: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
			use pac_devices_sch;

			device_cursor : pac_devices_sch.cursor;

			procedure set_purpose (
				device_name	: in type_device_name;
				device		: in out type_device_sch) is
			begin
				device.purpose := purpose;
			end;

			use et_symbols;
			
		begin -- query_devices
			-- locate the device
			device_cursor := find (module.devices, device_name); -- R1

			if device_cursor /= pac_devices_sch.no_element then -- the device should be there

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
		module_name			: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name			: in type_device_name; -- R2
		partcode			: in pac_device_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
		log_threshold		: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
			use pac_devices_sch;

			device_cursor : pac_devices_sch.cursor;

			procedure set_partcode (
				device_name	: in type_device_name;
				device		: in out type_device_sch) is
			begin
				device.partcode := partcode;
			end;

			use et_symbols;
			
		begin -- query_devices
			-- locate the device
			device_cursor := find (module.devices, device_name); -- R1

			if device_cursor /= pac_devices_sch.no_element then -- the device should be there

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
			enclose_in_quotes (to_string (partcode)),
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
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) is
			use pac_devices_sch;
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
		return pac_devices_sch.cursor 
	is
		result : pac_devices_sch.cursor;
		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) is
			use pac_devices_sch;
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
		cursor_sch : pac_devices_sch.cursor;
	begin
		-- find the device in the module
		cursor_sch := locate_device (module, device);

		-- find the device in the library
		return get_device_model (cursor_sch);
	end locate_device;

	
	function locate_unit (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name; -- R2
		unit	: in pac_unit_name.bounded_string)
		return pac_units.cursor
	is
		use pac_devices_sch;
		use pac_units;
		
		device_cursor : pac_devices_sch.cursor;
		unit_cursor : pac_units.cursor; -- to be returned

		procedure query_units (
			device_name	: in et_devices.type_device_name; -- R2
			device		: in type_device_sch)
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
		use pac_units;
		unit_cursor : pac_units.cursor;
	begin
		unit_cursor := locate_unit (module, device, unit);

		if unit_cursor = pac_units.no_element then
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
		use pac_devices_sch;
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
		cursor_sch : pac_devices_sch.cursor;
	begin
		cursor_sch := locate_device (module, device);
		
		return pac_devices_sch.element (cursor_sch).variant;
	end get_variant;

	procedure set_variant (
		module	: in pac_generic_modules.cursor;
		device	: in pac_devices_sch.cursor;
		variant	: in pac_package_variant_name.bounded_string)
	is
		use pac_devices_sch;

		procedure query_device (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is

			procedure do_it (
				name	: in et_devices.type_device_name;
				dev		: in out type_device_sch)
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
		module			: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device			: in et_devices.type_device_name; -- R2
		variant			: in pac_package_variant_name.bounded_string; -- N, D
		log_threshold	: in type_log_level)
	is
		use pac_generic_modules;
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified
		device_cursor : pac_devices_sch.cursor;
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
		cursor_sch : pac_devices_sch.cursor;
		device_model : pac_device_model_file.bounded_string;
	begin
		cursor_sch := locate_device (module, device);
		device_model := pac_devices_sch.element (cursor_sch).model;
		return locate_device (device_model);
	end device_model_cursor;


	
	function exists_device_port (
	-- Returns true if given device with the given port exists in module indicated by module_cursor.
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		port_name		: in et_symbols.pac_port_name.bounded_string) -- CE
		return boolean is

		result : boolean := false; -- to be returned. goes true once the target has been found
		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) is
			use pac_unit_name;
			use pac_devices_sch;
			device_cursor : pac_devices_sch.cursor;

			use et_symbols;
			
			procedure query_units (
				device_name	: in type_device_name;
				device		: in type_device_sch) is
				use pac_units;
				unit_cursor : pac_units.cursor := device.units.first;
				use et_symbols.pac_ports;
				ports : et_symbols.pac_ports.map;
				use pac_port_name;
			begin
				while unit_cursor /= pac_units.no_element loop
					--log (text => "unit " & pac_unit_name.to_string (key (unit_cursor)));
					--log (text => "port " & pac_port_name.to_string (port_name));
					
					-- fetch the unit ports from the library model
					ports := get_ports_of_unit (device_cursor, key (unit_cursor));

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
		port_name		: in et_symbols.pac_port_name.bounded_string := et_symbols.to_port_name ("")) -- CE
		return boolean is

		result : boolean := false; -- to be returned, goes true once the target has been found
		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) is
			use pac_unit_name;
			use pac_devices_sch;
			device_cursor : pac_devices_sch.cursor;

			use et_symbols;
			
			procedure query_units (
				device_name	: in type_device_name;
				device		: in type_device_sch) is
				use pac_units;
				use et_symbols.pac_ports;
				ports : et_symbols.pac_ports.map;
				use pac_port_name;
			begin
				if contains (device.units, unit_name) then
					if length (port_name) > 0 then -- search for port in unit

						-- fetch the unit ports from the library model
						ports := get_ports_of_unit (device_cursor, unit_name);

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




	
	function next_device_name (
	-- Returns for the given device prefix the next available device name in the module.
	-- Example: prefix is C. If there are C1, C12, C1034 and C1035 the return will be C2.

	-- CS: look up non-electric devices

		module_cursor	: in pac_generic_modules.cursor;
		prefix			: in pac_device_prefix.bounded_string; -- R, L, C, IC, FD, H, ...
		category		: in type_device_category := ELECTRICAL)
		return type_device_name is -- C2
		
		next_name : type_device_name; -- to be returned

		use pac_device_prefix;
		
		procedure search_gap_electric (
		-- Searches for the lowest available device name. Looks at devices
		-- whose prefix equals the given prefix. Example: If given prefix is R, it looks
		-- for the lowest available resistor index.
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) is
			use pac_devices_sch;
			device_cursor : pac_devices_sch.cursor := module.devices.first;

			-- We start the search with index 1. Not 0 because this would result in a zero based
			-- numbering order. Index zero is allowed but not automatically choosen.
			index_expected : type_name_index := type_name_index'first + 1;

			gap_found : boolean := false; -- goes true once a gap has been found
		begin
			while device_cursor /= pac_devices_sch.no_element loop
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
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) 
		is
			use et_pcb;
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

	
	-- Returns the placeholders of the package of a device. The package is indirectly selected
	-- by the given variant name. The given device is accessed by the given device cursor.
	function placeholders_of_package (
		device	: in pac_devices_lib.cursor;
		variant	: in pac_package_variant_name.bounded_string) -- N, D, S_0805
		return et_device_placeholders.packages.type_text_placeholders
	is
		use et_packages;
		use et_device_placeholders.packages;
		use pac_devices_lib;
		use pac_variants;
		placeholders		: type_text_placeholders; -- to be returned

		-- fetch the package variants available for the given device:
		variants_available	: pac_variants.map := element (device).variants;
		
		variant_cursor		: pac_variants.cursor;
		package_model		: pac_package_model_file_name.bounded_string; -- ../lbr/smd/SO15.pac

		use et_packages;		
		use pac_package_models;
		package_cursor		: pac_package_models.cursor;

	begin -- placeholders_of_package
		
		-- locate the given variant in the device:
		variant_cursor := pac_variants.find (variants_available, variant);

		-- get the package model name:
		package_model := element (variant_cursor).package_model; -- ../lbr/smd/SO15.pac

		-- locate the package model in the package library:
		package_cursor := pac_package_models.find (package_models, package_model);

		-- fetch the placeholders of silk screen top and bottom
		placeholders.silkscreen.top := element (package_cursor).silkscreen.top.placeholders;
		placeholders.silkscreen.bottom := element (package_cursor).silkscreen.bottom.placeholders;

		-- fetch the placeholders of assembly documentation top and bottom
		placeholders.assy_doc.top := element (package_cursor).assy_doc.top.placeholders;
		placeholders.assy_doc.bottom := element (package_cursor).assy_doc.bottom.placeholders;
		
		return placeholders;
	end placeholders_of_package;

	
	procedure add_device (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_model	: in pac_device_model_file.bounded_string; -- ../libraries/devices/logic_ttl/7400.dev
		variant			: in pac_package_variant_name.bounded_string; -- N, D, S_0805
		destination		: in et_coordinates_2.type_position; -- sheet/x/y,rotation
		log_threshold	: in type_log_level) is separate;

	
	procedure copy_device (
	-- Copies the given device. Places the first unit of the device (according to add level)
	-- at the given destination in the schematic.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		destination		: in et_coordinates_2.type_position; -- sheet/x/y/rotation
		log_threshold	: in type_log_level) is separate;

	function available_units (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC1
		log_threshold	: in type_log_level)
		return et_devices.pac_unit_names.list
	is
		use pac_devices_sch;
		device_cursor_sch : pac_devices_sch.cursor;

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
				device		: in type_device_sch) 
			is
				use pac_units;
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
			module_name	: in pac_module_name.bounded_string;
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

		use pac_devices_sch;
		device_cursor_sch : pac_devices_sch.cursor;
		
		device_cursor_lib : pac_devices_lib.cursor;
		
		use pac_unit_names;
		all_unit_names : pac_unit_names.list;

		-- Clears the "available" flag if given unit is already in use:
		procedure query_in_use (
			device_name	: in type_device_name;
			device		: in type_device_sch) 
		is
			use pac_units;
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
		use pac_devices_sch;
		device_cursor_sch : pac_devices_sch.cursor;

		names_of_units : pac_unit_names.list;

		procedure query_units (
			device_name	: in type_device_name;
			device		: in type_device_sch)
		is 
			procedure query_unit (c : in pac_units.cursor) is 
				use et_devices.pac_unit_name;
				use pac_units;
				use pac_unit_names;
			begin
				-- If the unit is on the given sheet then append it to the result:
				if get_sheet (element (c).position) = sheet then
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
		return et_coordinates_2.type_position
	is
		use pac_devices_sch;
		device_cursor_sch : pac_devices_sch.cursor;

		unit_position : et_coordinates_2.type_position;
		
		procedure query_unit (
			device_name	: in type_device_name;
			device		: in type_device_sch)
		is 
			use pac_units;
			unit_cursor : pac_units.cursor;
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
		device	: in pac_devices_sch.cursor; -- R2
		unit	: in pac_units.cursor)
		return et_coordinates_2.type_position
	is
		use pac_devices_sch;
		unit_position : et_coordinates_2.type_position;
		
		procedure query_unit (
			device_name	: in type_device_name;
			device		: in type_device_sch)
		is 
			use pac_units;
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
		device		: in pac_devices_sch.cursor; -- R2
		unit		: in pac_units.cursor;
		category	: in type_placeholder_meaning)
		return type_vector_model
	is
		placeholder_position : type_vector_model; -- to be returned

		use pac_devices_sch;
		unit_position : et_coordinates_2.type_position;
		
		procedure query_unit (
			device_name	: in type_device_name;
			device		: in type_device_sch)
		is 
			use pac_units;
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

			move_by (placeholder_position, to_distance_relative (unit_position.place));
			
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
		return type_sheet
	is begin		
		return get_sheet (position (module, device, unit));
	end sheet;
		

	
	procedure fetch_unit (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC1
		unit_name		: in pac_unit_name.bounded_string; -- A, B, IO_BANK_2
		destination		: in et_coordinates_2.type_position; -- sheet/x/y/rotation
		log_threshold	: in type_log_level) is separate;





	procedure create_assembly_variant (
	-- Creates a new assembly variant.
		module_name		: in pac_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;

		procedure create (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
			inserted : boolean;
			cursor : et_assembly_variants.pac_assembly_variants.cursor;
		begin
			-- create the variant
			et_assembly_variants.pac_assembly_variants.insert (
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
		module_name		: in pac_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;

		procedure delete (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
			use et_assembly_variants.pac_assembly_variants;
			cursor : et_assembly_variants.pac_assembly_variants.cursor;
		begin
			-- before deleting, the variant must be located
			cursor := find (module.variants, variant_name);

			if cursor /= et_assembly_variants.pac_assembly_variants.no_element then
				
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
		module_name		: in pac_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost											
		description		: in et_assembly_variants.type_description; -- "this is the low budget variant"
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;

		procedure describe (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
			use et_assembly_variants.pac_assembly_variants;
			cursor : et_assembly_variants.pac_assembly_variants.cursor;

			procedure assign_description (
				name		: in pac_assembly_variant_name.bounded_string;
				variant		: in out et_assembly_variants.type_assembly_variant) is
			begin
				variant.description := description;
			end assign_description;
			
		begin -- describe
			-- before describing, the variant must be located
			cursor := et_assembly_variants.pac_assembly_variants.find (module.variants, variant_name);

			if cursor /= et_assembly_variants.pac_assembly_variants.no_element then

				et_assembly_variants.pac_assembly_variants.update_element (
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
		module_name		: in pac_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		device			: in type_device_name; -- R1
		value			: in pac_device_value.bounded_string; -- 220R
		partcode		: in pac_device_partcode.bounded_string; -- R_PAC_S_0805_VAL_220R
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
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
			use et_assembly_variants.pac_assembly_variants;
			cursor : et_assembly_variants.pac_assembly_variants.cursor;

			procedure insert_device (
				name		: in pac_assembly_variant_name.bounded_string;
				variant		: in out et_assembly_variants.type_assembly_variant) is
				use et_assembly_variants.pac_device_variants;
				cursor : et_assembly_variants.pac_device_variants.cursor;
				inserted : boolean;
			begin
				-- Locate the device in the variant.
				-- If already there, delete it and insert it anew
				-- as specified by the operator.
				cursor := find (variant.devices, device);

				if cursor /= et_assembly_variants.pac_device_variants.no_element then -- device already there
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
			cursor := et_assembly_variants.pac_assembly_variants.find (module.variants, variant_name);

			if cursor /= et_assembly_variants.pac_assembly_variants.no_element then

				et_assembly_variants.pac_assembly_variants.update_element (
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
			 " partcode " & to_string (partcode) &
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
		module_name		: in pac_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		device			: in type_device_name; -- R1
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;

		procedure unmount (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
			use et_assembly_variants.pac_assembly_variants;
			cursor : et_assembly_variants.pac_assembly_variants.cursor;

			procedure insert_device (
				name		: in pac_assembly_variant_name.bounded_string;
				variant		: in out et_assembly_variants.type_assembly_variant) is
				use et_assembly_variants.pac_device_variants;
				cursor : et_assembly_variants.pac_device_variants.cursor;
				inserted : boolean;
			begin
				-- Locate the device in the variant.
				-- If already there, delete it and insert it anew.
				cursor := find (variant.devices, device);

				if cursor /= et_assembly_variants.pac_device_variants.no_element then -- device already there
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
			cursor := et_assembly_variants.pac_assembly_variants.find (module.variants, variant_name);

			if cursor /= et_assembly_variants.pac_assembly_variants.no_element then

				et_assembly_variants.pac_assembly_variants.update_element (
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
		module_name		: in pac_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		device			: in type_device_name; -- R1
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;

		procedure remove (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
			use et_assembly_variants.pac_assembly_variants;
			cursor : et_assembly_variants.pac_assembly_variants.cursor;

			procedure delete_device (
				name		: in pac_assembly_variant_name.bounded_string;
				variant		: in out et_assembly_variants.type_assembly_variant) is
				use et_assembly_variants.pac_device_variants;
				cursor : et_assembly_variants.pac_device_variants.cursor;
			begin
				-- Locate the device in the variant. Issue error message
				-- if not found.
				cursor := find (variant.devices, device);

				if cursor /= et_assembly_variants.pac_device_variants.no_element then  -- device in assembly variant
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
			cursor := et_assembly_variants.pac_assembly_variants.find (module.variants, variant_name);

			if cursor /= et_assembly_variants.pac_assembly_variants.no_element then

				et_assembly_variants.pac_assembly_variants.update_element (
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
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
			use pac_devices_sch;

			device_cursor_before : pac_devices_sch.cursor;
			device_cursor_after  : pac_devices_sch.cursor;
			inserted : boolean;

			-- temporarily storage of unit coordinates:
			position_of_units : pac_unit_positions.map;

		begin -- query_devices
			-- locate the device by the old name
			device_cursor_before := find (module.devices, device_name_before); -- IC1

			-- copy elements and properties of the old device to a new one:
			pac_devices_sch.insert (
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
				pac_devices_sch.delete (
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

	function sort_by_coordinates_2 (
		module_cursor 	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level) 
		return et_numbering.pac_devices.map is
		use et_numbering;
		devices : et_numbering.pac_devices.map; -- to be returned

		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in et_schematic.type_module)
		is

			procedure query_units (device_cursor : in pac_devices_sch.cursor) is
				use pac_units;
				device_name : type_device_name := pac_devices_sch.key (device_cursor); -- R1

				procedure sort (unit_cursor : in pac_units.cursor) is 
					unit_name : pac_unit_name.bounded_string := key (unit_cursor);  -- 1, C, IO_BANK1
					unit_position : et_coordinates_2.type_position := element (unit_cursor).position;
					inserted : boolean := false;
					cursor_sort : et_numbering.pac_devices.cursor;
				begin -- sort
					log (text => "unit " & to_string (unit_name) &
						" at " & to_string (position => unit_position),
						 level => log_threshold + 2);
					
					et_numbering.pac_devices.insert 
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
				
				pac_units.iterate (
					container	=> pac_devices_sch.element (device_cursor).units,
					process		=> sort'access);

				log_indentation_down;
			end query_units;
			
		begin -- query_devices
			pac_devices_sch.iterate (
				container	=> module.devices,
				process		=> query_units'access);
		end query_devices;

	begin -- sort_by_coordinates_2
		log (text => "sorting devices/units by schematic coordinates ...", level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_devices'access);


		log_indentation_down;
		
		return devices;
	end sort_by_coordinates_2;

	function unit_positions_valid (
	-- Returns true if no unit sits on top of another.
		module_cursor 	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
		return boolean is
		use et_numbering;
		devices : et_numbering.pac_devices.map;
	begin
		devices := sort_by_coordinates_2 (module_cursor, log_threshold);
		-- If a unit sits on top of another unit, sort_by_coordinates_2 throws a
		-- constraint_error which will be catched here.

		return true;
		
		exception when event: others => 
			return false;
		
	end unit_positions_valid;
	
	procedure renumber_devices (
	-- Renumbers devices according to the sheet number.
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		step_width		: in type_name_index;
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_conventions;
		use et_numbering;

		-- The list of devices sorted by their coordinates.
		-- By their order in this list the devices will be renumbered.
		devices : et_numbering.pac_devices.map;

		function renumber (cat : in et_conventions.type_device_category) return boolean is
		-- Renumbers devices of given category. Returns true if all devices
		-- have been renamed.
		-- Marks every renamed unit in the device list so that the second
		-- run of this function does not try to renumber them again.
			result : boolean := true;
			
			use et_numbering.pac_devices;
			cursor : et_numbering.pac_devices.cursor := devices.first;
			name_before, name_after : type_device_name; -- R1
			sheet_before, sheet_now : type_sheet := type_sheet'first;

			index_on_sheet : type_name_index := type_name_index'first;
			device_index : type_name_index;

			procedure update_index is begin
			-- Detects when the sheet number changes. In this case
			-- resets the index_on_sheet so that the indexing starts anew.
				sheet_now := get_sheet (key (cursor));

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
				cursor_done : et_numbering.pac_devices.cursor := cursor;

				procedure set_done (
					coordinates : in et_coordinates_2.type_position;
					device		: in out et_numbering.type_device) is
				begin
					device.done := true;
				end;
				
			begin -- mark_units_done
				log (text => "marking all units done ...", level => log_threshold + 2);
				
				while cursor_done /= et_numbering.pac_devices.no_element loop
					
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
			while cursor /= et_numbering.pac_devices.no_element loop

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
		devices := sort_by_coordinates_2 (module_cursor, log_threshold + 2);

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
		


	

	
	procedure apply_offset (
	-- Adds the offset to the device index of the given device_name.
		device_name		: in out type_device_name; -- IC3
		offset			: in type_name_index; -- 100
		log_threshold	: in type_log_level) 
	is
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

	


	
	function get_port_properties (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A, B, IO_BANK_2
		port_name		: in et_symbols.pac_port_name.bounded_string) -- CE
		return type_port_properties_access
	is
		properties : type_port_properties_access; -- to be returned
		
		terminal_name : et_terminals.pac_terminal_name.bounded_string;

		use et_symbols;
		port_direction : type_port_direction := PASSIVE;
		port_properties_cursor : et_symbols.pac_ports.cursor;

		use et_devices;

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) 
		is
			use pac_devices_sch;
			device_cursor_sch	: pac_devices_sch.cursor;
			variant 			: pac_package_variant_name.bounded_string; -- D, N
			device_cursor_lib	: pac_devices_lib.cursor;

			
			procedure query_variants (
				model	: in pac_device_model_file.bounded_string;
				device	: in type_device_lib) 
			is
				variant_cursor : pac_variants.cursor;

				
				procedure query_ports (
					variant_name	: in pac_package_variant_name.bounded_string;
					variant			: in et_devices.type_variant) 
				is
					use pac_terminal_port_map;
					terminal_cursor : pac_terminal_port_map.cursor := variant.terminal_port_map.first;
					use pac_port_name;
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

			
			use et_symbols.pac_ports;
			
		begin -- query_devices
			-- locate the device in schematic (default assembly variant):
			device_cursor_sch := find (module.devices, device_name);

-- 			if device_cursor_sch /= pac_devices_sch.no_element then
			
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

	begin
		query_element (
			position	=> module_cursor,
			process		=> query_devices'access);
		
		return properties;
	end get_port_properties;

	

	

	

	
	
	
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
		pos : et_coordinates_2.type_position; -- x/y, rotation, sheet
		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) is

			use pac_devices_sch;
			device_cursor : pac_devices_sch.cursor;

			procedure query_units (
				device_name	: in type_device_name; -- IC45
				device		: in type_device_sch) is

				use pac_units;
				unit_cursor : pac_units.cursor;
				
			begin
				-- If the given unit_name contains something, locate the unit
				-- by its name. If unit_name is empty, locate the first unit.
				if pac_unit_name.length (unit_name) > 0 then -- locate by name
					
					unit_cursor := pac_units.find (device.units, unit_name);

					if unit_cursor /= pac_units.no_element then -- unit exists
						exists := true;
						pos := element (unit_cursor).position;
					else
						exists := false; -- unit does not exist
					end if;
					
				else -- locate the first unit:
					unit_cursor := pac_units.first (device.units);
					-- There should be at least one unit. Otherwise raise constraint_error.

					if unit_cursor /= pac_units.no_element then -- unit exists
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
			device_cursor := pac_devices_sch.find (module.devices, device_name);

			if device_cursor /= pac_devices_sch.no_element then -- device exists
				pac_devices_sch.query_element (device_cursor, query_units'access);
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
