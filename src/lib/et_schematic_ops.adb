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
with et_pcb_sides;
with et_board_coordinates;
with et_pcb;
with et_devices_electrical;
with et_devices_non_electrical;
with et_port_direction;
with et_terminals;
with et_packages;
with et_package_names;
with et_device_appearance;
with et_device_rw;
with et_device_model;				use et_device_model;
with et_board_ops.ratsnest;			use et_board_ops.ratsnest;

with et_canvas_schematic_2;


with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.packages;


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
		position	: in type_object_position) is
	begin
		log (ERROR, "port " & enclose_in_quotes (port) &
			 " is directly connected with other ports at" &
			to_string (position => position) &
			 ". Dragging not possible !",
			 console => true);
		raise constraint_error;
	end;



	

	
	procedure delete_ports (
		module			: in pac_generic_modules.cursor;		-- the module
		device			: in type_device_name;			-- the device
		ports			: in pac_ports.map := pac_ports.empty_map; -- the ports (if empty, all ports of the device will be deleted)
		sheets			: in pac_unit_positions.map;	-- the sheet numbers where the units can be found. CS implementation required
		log_threshold	: in type_log_level) 
	is
		dedicated_ports : boolean := false; -- goes true if "ports" contains something.
		
		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) is

			
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
												if pac_ports.contains (ports, port.port_name) then
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
		if pac_ports.length (ports) > 0 then
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
			module		: in out type_generic_module) 
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
				position_of_units := get_unit_positions (device_cursor);

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

	



	procedure insert_ports (
		module			: in pac_generic_modules.cursor;		-- the module
		device			: in type_device_name;					-- the device
		unit			: in pac_unit_name.bounded_string;	-- the unit name like A, C, PWR
		ports			: in pac_ports.map; -- the ports to be inserted
		sheet			: in type_sheet;				-- the sheet to look at
		log_threshold	: in type_log_level)
	is

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) is

			
			procedure query_net (net_cursor : in pac_nets.cursor) is
				use pac_nets;

				
				procedure query_strands (
					net_name	: in pac_net_name.bounded_string;
					net			: in out type_net) 
				is
					strand_cursor : pac_strands.cursor;

					use pac_ports;
					port_cursor : pac_ports.cursor := ports.first;

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
					while port_cursor /= pac_ports.no_element loop
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
			 to_string (sheet) & " ...", level => log_threshold);
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;
	end insert_ports;
	

	


	function locate_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string)		
		return pac_nets.cursor 
	is	
		cursor : pac_nets.cursor;

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
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
			module 		: in type_generic_module)
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
			module		: in type_generic_module) 
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
		place			: in type_object_position)
		return boolean is

		-- This flag goes true once a segment has been found.
		segment_found : boolean := false; -- to be returned
		
		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) is

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
		place			: in type_object_position;
		log_threshold	: in type_log_level)
		return type_ports 
	is
		ports : type_ports; -- to be returned

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_devices_sch;
			use et_submodules.pac_netchangers;			

			
			procedure query_devices (device_cursor : in pac_devices_sch.cursor) is

				procedure query_units (unit_cursor : in pac_units.cursor) is
					use pac_units;
					use pac_unit_name;
					unit_position : type_object_position;
					ports : pac_ports.map;

					
					procedure query_port (port_cursor : in pac_ports.cursor) is
						use et_symbols;
						use pac_ports;
					begin
						log (text => "unit " & to_string (key (unit_cursor)) &
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

						pac_ports.iterate (ports, query_port'access);

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
				submodule_position : type_object_position;
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
				netchanger_position : type_object_position;
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
			module		: in out type_generic_module) 
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
		log_threshold		: in type_log_level) 
	is

		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
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
				position_of_units := get_unit_positions (device_cursor_before);
				
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



	
	
	-- Sets the value of a device.
	procedure set_value (
		module_name			: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name			: in type_device_name; -- R2
		value				: in pac_device_value.bounded_string; -- 470R
		log_threshold		: in type_log_level) 
	is		
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_devices_sch;

			device_cursor : pac_devices_sch.cursor;

			procedure set_value (
				device_name	: in type_device_name;
				device		: in out type_device_sch) 
			is begin
				device.value := value;
			end;

			
			use et_symbols;
			use et_device_appearance;

			
		begin -- query_devices
			-- locate the device
			device_cursor := find (module.devices, device_name); -- R1

			if device_cursor /= pac_devices_sch.no_element then -- the device should be there

				-- Only real devices have a value.
				if element (device_cursor).appearance = APPEARANCE_PCB then

					-- Check value regarding the device category:
					if et_conventions.value_valid (value, get_prefix (device_name)) then 
					
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


	
	-- Sets the purpose of a device.
	procedure set_purpose (
		module_name			: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name			: in type_device_name; -- R2
		purpose				: in pac_device_purpose.bounded_string; -- brightness_control
		log_threshold		: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_devices_sch;

			device_cursor : pac_devices_sch.cursor;

			procedure set_purpose (
				device_name	: in type_device_name;
				device		: in out type_device_sch) 
			is begin
				device.purpose := purpose;
			end;

			
			use et_symbols;
			use et_device_appearance;

			
		begin -- query_devices
			-- locate the device
			device_cursor := find (module.devices, device_name); -- R1

			if device_cursor /= pac_devices_sch.no_element then -- the device should be there

				-- Only real devices have a purpose. Issue warning if targeted device is virtual.
				if element (device_cursor).appearance = APPEARANCE_PCB then

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
			module		: in out type_generic_module) is
			use pac_devices_sch;

			device_cursor : pac_devices_sch.cursor;

			procedure set_partcode (
				device_name	: in type_device_name;
				device		: in out type_device_sch) is
			begin
				device.partcode := partcode;
			end;

			use et_symbols;
			use et_device_appearance;
			
			
		begin -- query_devices
			-- locate the device
			device_cursor := find (module.devices, device_name); -- R1

			if device_cursor /= pac_devices_sch.no_element then -- the device should be there

				-- Only real devices have a purpose. Issue warning if targeted device is virtual.
				if element (device_cursor).appearance = APPEARANCE_PCB then

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
			module		: in type_generic_module) is
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
			module		: in type_generic_module) is
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
		return get_available_variants (cursor_lib);
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
			module		: in out type_generic_module) 
		is

			procedure do_it (
				name	: in type_device_name;
				dev		: in out type_device_sch)
			is 
				cursor_lib : pac_devices_lib.cursor;
				use pac_package_variant_name;
			begin
				cursor_lib := locate_device (dev.model);

				if is_variant_available (cursor_lib, variant) then
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
		device			: in type_device_name; -- R2
		variant			: in pac_package_variant_name.bounded_string; -- N, D
		log_threshold	: in type_log_level)
	is
		use pac_generic_modules;
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified
		device_cursor : pac_devices_sch.cursor;

		use pac_package_variant_name;
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


	



	
	function get_next_device_name (
		module_cursor	: in pac_generic_modules.cursor;
		prefix			: in pac_device_prefix.bounded_string; -- R, L, C, IC, FD, H, ...
		category		: in type_device_category := ELECTRICAL)
		return type_device_name
	is
		-- CS: look up non-electric devices
		
		next_name : type_device_name; -- to be returned

		use pac_device_prefix;

		
		-- Searches for the lowest available device name. Looks at devices
		-- whose prefix equals the given prefix. Example: If given prefix is R, it looks
		-- for the lowest available resistor index.
		procedure search_gap_electric (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_devices_sch;
			device_cursor : pac_devices_sch.cursor := module.devices.first;

			-- We start the search with index 1. Not 0 because this would result in a zero based
			-- numbering order. Index zero is allowed but not automatically choosen.
			index_expected : type_name_index := type_name_index'first + 1;

			gap_found : boolean := false; -- goes true once a gap has been found
		begin
			while device_cursor /= pac_devices_sch.no_element loop
				if get_prefix (key (device_cursor)) = prefix then -- prefix match
					
					if get_index (key (device_cursor)) /= index_expected then -- we have a gap

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

		
		-- Searches for the lowest available non-electrical device name. Looks at devices
		-- whose prefix equals the given prefix. Example: If given prefix is MH, it looks
		-- for the lowest available mounting hole index.
		procedure search_gap_non_electric (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use et_devices_non_electrical;
			use pac_devices_non_electric;
			device_cursor : pac_devices_non_electric.cursor := module.devices_non_electric.first;

			-- We start the search with index 1. Not 0 because this would result in a zero based
			-- numbering order. Index zero is allowed but not automatically choosen.
			index_expected : type_name_index := type_name_index'first + 1;

			gap_found : boolean := false; -- goes true once a gap has been found
		begin -- search_gap
			while device_cursor /= pac_devices_non_electric.no_element loop
				if get_prefix (key (device_cursor)) = prefix then -- prefix match
					
					if get_index (key (device_cursor)) /= index_expected then -- we have a gap

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

		
	begin

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
	end get_next_device_name;



		
	-- Returns the placeholders of the package of a device. The package is indirectly selected
	-- by the given variant name. The given device is accessed by the given device cursor.
	function placeholders_of_package (
		device	: in pac_devices_lib.cursor;
		variant	: in pac_package_variant_name.bounded_string) -- N, D, S_0805
		return et_device_placeholders.packages.type_text_placeholders
	is
		use et_package_names;
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
		destination		: in type_object_position; -- sheet/x/y,rotation
		log_threshold	: in type_log_level) is separate;


	
	
	-- Copies the given device. Places the first unit of the device (according to add level)
	-- at the given destination in the schematic.
	procedure copy_device (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		destination		: in type_object_position; -- sheet/x/y/rotation
		log_threshold	: in type_log_level) is separate;



	function device_exists (
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		variant	: in pac_assembly_variant_name.bounded_string; -- low_cost				
		device	: in type_device_name)
		return boolean 
	is
		result : boolean := false; -- to be returned

		
		procedure query_variants (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_assembly_variants;
			variant_cursor : pac_assembly_variants.cursor;

			
			procedure query_devices (
				variant_name	: in pac_assembly_variant_name.bounded_string;
				variant			: in type_assembly_variant) 
			is
				use et_assembly_variants;
				use pac_device_variants;
				device_cursor : pac_device_variants.cursor;
			begin
				device_cursor := find (variant.devices, device);

				-- The device may be listed in the assembly variant:
				if device_cursor /= pac_device_variants.no_element then
					case element (device_cursor).mounted is
						when YES => result := true; -- mounted with alternative value, partcode or purpose
						when NO  => result := false; -- not mounted
					end case;
				else
				-- The device may be NOT listed in the assembly variant. Means it is mounted always.
					result := true;
				end if;
					
			end query_devices;

			
		begin -- query_variants
			variant_cursor := find (module.variants, variant);

			query_element (
				position	=> variant_cursor,
				process		=> query_devices'access);
		end;

		
	begin
-- 		log (text => "module " & enclose_in_quotes (to_string (module_name)) &
-- 			" variant " & enclose_in_quotes (to_variant (variant)) &
-- 			" querying device " & to_string (device),
-- 			level => log_threshold);

		pac_generic_modules.query_element (
			position	=> module,
			process		=> query_variants'access);
		
		return result;
	end device_exists;




	function get_alternative_device (
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		variant	: in pac_assembly_variant_name.bounded_string; -- low_cost				
		device	: in type_device_name)
		return pac_device_variants.cursor 
	is

		cursor : pac_device_variants.cursor; -- to be returned;
		
		procedure query_variants (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) is
			use pac_assembly_variants;
			
			variant_cursor : pac_assembly_variants.cursor;

			procedure query_devices (
				variant_name	: in pac_assembly_variant_name.bounded_string;
				variant			: in type_assembly_variant) is
				use pac_device_variants;
			begin
				cursor := find (variant.devices, device);
			end query_devices;
				
		begin -- query_variants
			variant_cursor := find (module.variants, variant);

			query_element (
				position	=> variant_cursor,
				process		=> query_devices'access);
		end;
		
	begin
		pac_generic_modules.query_element (
			position	=> module,
			process		=> query_variants'access);
		
		return cursor;
	end get_alternative_device;

		

	




	procedure create_assembly_variant (
	-- Creates a new assembly variant.
		module_name		: in pac_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;

		procedure create (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) is
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
			module		: in out type_generic_module) is
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
			module		: in out type_generic_module) is
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
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;

		
		function write_purpose return string is
		begin
			if get_length (purpose) = 0 then
				return "";
			else
				return " purpose " & enclose_in_quotes (to_string (purpose));
			end if;
		end;

		
		procedure mount (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
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
			module		: in out type_generic_module) is
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
			module		: in out type_generic_module) is
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


	

	-- Renames the given device. Returns true if device has been renamed.
	-- Assumes that the device with name device_name_before exists.
	-- Does not perform any conformity checks on given device names.
	function rename_device (
		module_cursor		: in pac_generic_modules.cursor; -- the cursor to the module
		device_name_before	: in type_device_name; -- IC1
		device_name_after	: in type_device_name; -- IC101
		log_threshold		: in type_log_level) 
		return boolean 
	is

		result : boolean := false;

		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
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
				position_of_units := get_unit_positions (device_cursor_before);
				
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
		return et_numbering.pac_devices.map 
	is
		use et_numbering;
		devices : et_numbering.pac_devices.map; -- to be returned

		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is

			procedure query_units (device_cursor : in pac_devices_sch.cursor) is
				use pac_units;
				device_name : type_device_name := pac_devices_sch.key (device_cursor); -- R1

				
				procedure sort (
					unit_cursor : in pac_units.cursor) 
				is 
					unit_name : pac_unit_name.bounded_string := key (unit_cursor);  -- 1, C, IO_BANK1
					unit_position : type_object_position := element (unit_cursor).position;
					inserted : boolean := false;
					cursor_sort : et_numbering.pac_devices.cursor;

					use pac_unit_name;
				begin
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

				
			begin				
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



	




	
	-- Renumbers devices according to the sheet number.
	procedure renumber_devices (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		step_width		: in type_name_index;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_conventions;
		use et_numbering;
		use pac_unit_name;
		

		-- The list of devices sorted by their coordinates.
		-- By their order in this list the devices will be renumbered.
		devices : et_numbering.pac_devices.map;

		
		-- Renumbers devices of given category. Returns true if all devices
		-- have been renamed.
		-- Marks every renamed unit in the device list so that the second
		-- run of this function does not try to renumber them again.
		function renumber (
			cat : in et_conventions.type_device_category) 
			return boolean 
		is
			result : boolean := true;
			
			use et_numbering.pac_devices;
			cursor : et_numbering.pac_devices.cursor := devices.first;
			name_before, name_after : type_device_name; -- R1
			sheet_before, sheet_now : type_sheet := type_sheet'first;

			index_on_sheet : type_name_index := type_name_index'first;
			device_index : type_name_index;

			
			-- Detects when the sheet number changes. In this case
			-- resets the index_on_sheet so that the indexing starts anew.
			procedure update_index is begin
				sheet_now := get_sheet (key (cursor));

				if sheet_now = sheet_before then -- no change
					index_on_sheet := index_on_sheet + 1;
				else -- sheet has changed
					sheet_before := sheet_now;
					index_on_sheet := type_name_index'first + 1;
				end if;
			end update_index;


			
			-- Sets the "done" flag of all devices with name_before in the device list.
			procedure mark_units_done is

				-- Start with the unit indicated by cursor. All other
				-- units of the device come after this one.
				cursor_done : et_numbering.pac_devices.cursor := cursor;

				procedure set_done (
					coordinates : in type_object_position;
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
								prefix	=> get_prefix (name_before), -- R, C, IC
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

	
	
end et_schematic_ops;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
