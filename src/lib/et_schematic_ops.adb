------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         SCHEMATIC OPERATIONS                             --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                -- 
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

with et_symbol_model;
with et_units;							use et_units;
with et_unit_name;						use et_unit_name;
with et_device_category;
with et_devices_non_electrical;
with et_device_appearance;
with et_board_ops.ratsnest;				use et_board_ops.ratsnest;
with et_net_strands;					use et_net_strands;
with et_netchangers;
with et_assembly_variants;


package body et_schematic_ops is


	use et_netchangers.pac_netchangers;
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



	
	

	

	
	function get_net (
		module		: in pac_generic_modules.cursor;
		device		: in pac_devices_electrical.cursor;
		terminal	: in pac_terminal_name.bounded_string)
		return pac_nets.cursor
	is
		result : pac_nets.cursor;

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
				ports : constant type_ports := 
					get_ports (n, pac_assembly_variants.no_element);

				-- ports.devices now contains all ports and units of devices in
				-- the net indicated by cursor n.

				procedure query_port (p : in pac_device_ports.cursor) is
					use pac_device_ports;
					use pac_port_name;
					use pac_unit_name;
					use pac_devices_electrical;
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


	


	
	
	function get_ports (
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
			use pac_devices_electrical;
			use et_netchangers;
			use pac_netchangers;			

			
			procedure query_devices (device_cursor : in pac_devices_electrical.cursor) is

				procedure query_units (unit_cursor : in pac_units.cursor) is
					use pac_units;
					use pac_unit_name;
					unit_position : type_object_position;
					ports : pac_symbol_ports.map;

					
					procedure query_port (port_cursor : in pac_symbol_ports.cursor) is
						use et_symbol_model;
						use pac_symbol_ports;
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
								container	=> get_ports.ports.devices,
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

						pac_symbol_ports.iterate (ports, query_port'access);

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
							container	=> get_ports.ports.submodules,
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

			

			procedure query_netchangers (netchanger_cursor : in pac_netchangers.cursor) is
				netchanger_position : type_object_position;
				ports : type_netchanger_ports;
				use et_netlists;
			begin
				netchanger_position := element (netchanger_cursor).position_sch;

				-- Look at netchangers on the given sheet of place:
				if get_sheet (netchanger_position) = get_sheet (place) then
					log (text => "netchanger " & to_string (key (netchanger_cursor)), level => log_threshold + 1);
					log_indentation_up;	
					
					-- get the absolute port positions of the netchanger
					ports := netchanger_ports (netchanger_cursor);

					-- If the port sits at x/y of place then we have a match.
					-- The match can either be at the msster or slave port.
					-- The match can/should NEVER be at both ports simultaneously.
					
					-- First test whether the master port sits here:
					if ports.master = place.place then

						log (text => "port " & to_string (MASTER) &
							" at" & to_string (ports.master),
							level => log_threshold + 2);
						
						-- Insert the port in the portlist to be returned:
						pac_netchanger_ports.insert 
							(
							container	=> get_ports.ports.netchangers,
							new_item	=> 
								(
								index	=> key (netchanger_cursor),
								port	=> MASTER)
							);

						
					-- Second, test wheter slave port sits here:
					elsif ports.slave = place.place then

						log (text => "port " & to_string (SLAVE) &
							" at" & to_string (ports.slave),
							level => log_threshold + 2);
						
						-- Insert the port in the portlist to be returned:
						pac_netchanger_ports.insert 
							(
							container	=> get_ports.ports.netchangers,
							new_item	=> 
								(
								index	=> key (netchanger_cursor),
								port	=> SLAVE)
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

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " locating ports at " & to_string (position => place),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
		
		return ports;
	end get_ports;





	

	-- Renames the given device. Returns true if device has been renamed.
	-- Assumes that the device with name device_name_before exists.
	-- Does not perform any conformity checks on given device names.
-- 	function rename_device (
-- 		module_cursor		: in pac_generic_modules.cursor; -- the cursor to the module
-- 		device_name_before	: in type_device_name; -- IC1
-- 		device_name_after	: in type_device_name; -- IC101
-- 		log_threshold		: in type_log_level) 
-- 		return boolean 
-- 	is
-- 
-- 		result : boolean := false;
-- 
-- 		procedure query_devices (
-- 			module_name	: in pac_module_name.bounded_string;
-- 			module		: in out type_generic_module) 
-- 		is
-- 			use pac_devices_electrical;
-- 
-- 			device_cursor_before : pac_devices_electrical.cursor;
-- 			device_cursor_after  : pac_devices_electrical.cursor;
-- 			inserted : boolean;
-- 
-- 			-- temporarily storage of unit coordinates:
-- 			position_of_units : pac_unit_positions.map;
-- 
-- 			
-- 		begin -- query_devices
-- 			-- locate the device by the old name
-- 			device_cursor_before := find (module.devices, device_name_before); -- IC1
-- 
-- 			-- copy elements and properties of the old device to a new one:
-- 			pac_devices_electrical.insert (
-- 				container	=> module.devices,
-- 				key			=> device_name_after, -- IC23
-- 				new_item	=> element (device_cursor_before), -- all elements and properties of IC1
-- 				inserted	=> inserted,
-- 				position	=> device_cursor_after);
-- 
-- 			if inserted then
-- 
-- 				-- Before deleting the old device, the coordinates of the
-- 				-- units must be fetched. These coordinates will later assist
-- 				-- in renaming the port names in connected net segments.
-- 				position_of_units := get_unit_positions (device_cursor_before);
-- 				
-- 				-- delete the old device
-- 				pac_devices_electrical.delete (
-- 					container	=> module.devices,
-- 					position	=> device_cursor_before);
-- 
-- 				-- rename all ports in module.nets
-- 				rename_ports (
-- 					module			=> module_cursor,
-- 					device_before	=> device_name_before,
-- 					device_after	=> device_name_after,
-- 					sheets			=> position_of_units, -- the sheets to look at
-- 					log_threshold	=> log_threshold + 1);
-- 
-- 				result := true; -- successful renaming
-- 			else
-- 				log (text => "device name " & to_string (device_name_after) & 
-- 					" already used -> skipped", level => log_threshold + 1);
-- 			end if;
-- 		end query_devices;
-- 		
-- 
-- 	begin -- rename_device
-- 		log (text => "renaming " & to_string (device_name_before) & " to " & 
-- 			to_string (device_name_after),
-- 			level => log_threshold);
-- 
-- 		log_indentation_up;
-- 		
-- 		update_element (
-- 			container	=> generic_modules,
-- 			position	=> module_cursor,
-- 			process		=> query_devices'access);
-- 
-- 		log_indentation_down;
-- 
-- 		return result;
-- 
-- 		exception when event: others =>
-- 			return false;
-- 
-- 	end rename_device;


	

	
	
end et_schematic_ops;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
