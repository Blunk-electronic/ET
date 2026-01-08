------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD OPERATIONS / DEVICES                         --
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

with ada.strings.unbounded;

with et_conventions;
with et_assembly_variants;
with et_netlists;
with et_device_model;
with et_device_library;					use et_device_library;
with et_device_library.units;			use et_device_library.units;

with et_devices_electrical.packages;	use et_devices_electrical.packages;

with et_schematic_ops;					use et_schematic_ops;
with et_schematic_ops.units;			use et_schematic_ops.units;
with et_schematic_ops.groups;

with et_net_ports;
with et_net_segment;
with et_submodules;
with et_package_read;
with et_package_write;

with et_board_ops.groups;
with et_board_ops.ratsnest;			use et_board_ops.ratsnest;
with et_mirroring;					use et_mirroring;


package body et_board_ops.devices is

	use pac_devices_electrical;
	use pac_devices_non_electrical;
	use pac_nets;
	use pac_text_board;







	function get_device_name (
		object	: in type_object_electrical)
		return string
	is begin
		return to_string (key (object.cursor));
	end;




	function get_device_name (
		object	: in type_object_electrical)
		return type_device_name
	is begin
		return key (object.cursor);
	end;




	
	

	

	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		device			: in type_object_electrical;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_device (
				device_name	: in type_device_name;
				device		: in out type_device_electrical)
			is begin
				modify_status (device, operation);
			end query_device;


		begin
			if is_real (device.cursor) then -- ignore virtual devices (like GND symbols)
				module.devices.update_element (device.cursor, query_device'access);
			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of electrical device "
			& get_device_name (device.cursor)
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;



	
	
	
	
	procedure propose_electrical_devices (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural; -- the number of affected devices
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_device (
				device_name	: in type_device_name;
				device		: in out type_device_electrical)
			is begin
				log (text => to_string (device_name), level => log_threshold + 1);
				set_proposed (device.status);
				count := count + 1;
			end query_device;

			
			device_cursor : pac_devices_electrical.cursor := module.devices.first;
			
		begin
			while device_cursor /= pac_devices_electrical.no_element loop

				if is_real (device_cursor) then -- ignore virtual devices (like GND symbols)
					
					-- log (text => "probing device " & to_string (key (device_cursor)),
					-- 	 level => log_threshold + 1);
					-- log_indentation_up;
						
					if in_catch_zone (
						zone	=> catch_zone,
						point	=> get_place (device_cursor)) 
					then
						-- log_indentation_up;
						-- log (text => "in catch zone", level => log_threshold + 1);

						module.devices.update_element (device_cursor, query_device'access);
						-- log_indentation_down;
					end if;

					-- log_indentation_down;
				end if;
				
				next (device_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " proposing electrical devices in " & to_string (catch_zone),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end propose_electrical_devices;




	




	function get_first_device (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_electrical
	is
		result : type_object_electrical;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			device_cursor : pac_devices_electrical.cursor := module.devices.first;
		begin
			while device_cursor /= pac_devices_electrical.no_element loop
				case flag is
					when PROPOSED =>
						if is_proposed (device_cursor) then
							result.cursor := device_cursor;
							exit; -- no further probing required
						end if;

					when SELECTED =>
						if is_selected (device_cursor) then
							result.cursor := device_cursor;
							exit; -- no further probing required
						end if;

					when others => null; -- CS
				end case;
						
				next (device_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first device /" & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;

		return result;
	end get_first_device;


	
		

	

	
	procedure move_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level) 
	is

	
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_electrical		: pac_devices_electrical.cursor;
			device_non_electrical	: pac_devices_non_electrical.cursor;			

			
			procedure move_electrical (
				device_name	: in type_device_name;
				device		: in out type_device_electrical) 
			is begin
				case coordinates is
					when ABSOLUTE =>
						set_place (device, point); -- preserve angle and face

					when RELATIVE =>
						set_place_relative (device, point); -- preserve angle and face
						
				end case;
			end;

			
			procedure move_non_electrical (
				device_name	: in type_device_name;
				device		: in out type_device_non_electrical) 
			is begin
				case coordinates is
					when ABSOLUTE =>
						set_place (device, point); -- preserve angle and face

					when RELATIVE =>
						set_place_relative (device, point); -- preserve angle and face
						
				end case;
			end;


			
		begin
			-- Search the device first among the electrical devices.
			-- Most likely it will be among them. If not,
			-- search in non-electrical devices:
			
			device_electrical := get_electrical_device (module_cursor, device_name);
			
			if has_element (device_electrical) then

				update_element (
					container	=> module.devices,
					position	=> device_electrical,
					process		=> move_electrical'access);

			else
				-- Search among non-electrical devices:
				device_non_electrical := get_non_electrical_device (module_cursor, device_name);

				if has_element (device_non_electrical) then

					update_element (
						container	=> module.devices_non_electric,
						position	=> device_non_electrical,
						process		=> move_non_electrical'access);

				-- If the requested device has not been found,
				-- then log a warning:
				else
					log (WARNING, " Device " & to_string (device_name) & " not found !");
				end if;

			end if;
		end query_module;

		
	begin
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_cursor) 
					 & " move device " & to_string (device_name) 
					 & " to " & to_string (point), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_cursor) 
					 & " moving device " & to_string (device_name) 
					 & " by " & to_string (point), level => log_threshold);
		end case;


		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		update_ratsnest (module_cursor, log_threshold + 1);

		log_indentation_down;
	end move_device;





	
	
	procedure rotate_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		coordinates		: in type_coordinates; -- relative/absolute		
		rotation		: in et_board_geometry.type_rotation_model := 90.0;
		log_threshold	: in type_log_level) 
	is

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_electrical		: pac_devices_electrical.cursor;
			device_non_electrical	: pac_devices_non_electrical.cursor;			

			
			procedure rotate_electrical (
				device_name	: in type_device_name;
				device		: in out type_device_electrical) 
			is begin
				case coordinates is
					when ABSOLUTE =>
						set_rotation (device, rotation); -- preserve x/y and face

					when RELATIVE =>
						set_rotation_relative (device, rotation); -- preserve x/y and face
				end case;
			end;

			
			procedure rotate_non_electrical (
				device_name	: in type_device_name;
				device		: in out type_device_non_electrical) 
			is begin
				case coordinates is
					when ABSOLUTE =>
						set_rotation (device, rotation); -- preserve x/y and face

					when RELATIVE =>
						set_rotation_relative (device, rotation); -- preserve x/y and face
				end case;
			end;

			
		begin

			-- Search the device first among the electrical devices.
			-- Most likely it will be among them. If not,
			-- search in non-electrical devices:
			
			device_electrical := get_electrical_device (module_cursor, device_name);
			
			if has_element (device_electrical) then

				update_element (
					container	=> module.devices,
					position	=> device_electrical,
					process		=> rotate_electrical'access);

			else
				-- Search among non-electrical devices:
				device_non_electrical := get_non_electrical_device (module_cursor, device_name);

				if has_element (device_non_electrical) then

					update_element (
						container	=> module.devices_non_electric,
						position	=> device_non_electrical,
						process		=> rotate_non_electrical'access);

				-- If the requested device has not been found,
				-- then log a warning:
				else
					log (WARNING, " Device " & to_string (device_name) & " not found !");
				end if;

			end if;
		end query_module;


		
	begin
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_cursor) 
					 & " rotate device " & to_string (device_name) 
					 & " to " & to_string (rotation), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_cursor) 
					 & " rotating device " & to_string (device_name) 
					 & " by " & to_string (rotation), level => log_threshold);
		end case;

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		update_ratsnest (module_cursor, log_threshold + 1);

		log_indentation_down;
	end rotate_device;




	

	
	procedure flip_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		toggle			: in boolean := false;
		face			: in type_face := TOP; -- top/bottom
		log_threshold	: in type_log_level) 
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_electrical		: pac_devices_electrical.cursor;
			device_non_electrical	: pac_devices_non_electrical.cursor;			

			
			procedure flip_electrical (
				device_name	: in type_device_name;
				device		: in out type_device_electrical) 
			is begin
				if toggle then
					toggle_face (device);
				else
					set_face (device, face);
				end if;
				
				reset_placeholder_positions (device);
			end flip_electrical;
			

			
			procedure flip_non_electrical (
				device_name	: in type_device_name;
				device		: in out type_device_non_electrical) 
			is begin
				if toggle then
					toggle_face (device);
				else
					set_face (device, face);
				end if;
				
				reset_placeholder_positions (device);
			end flip_non_electrical;

			
		begin

			-- Search the device first among the electrical devices.
			-- Most likely it will be among them. If not,
			-- search in non-electrical devices:
			
			device_electrical := get_electrical_device (module_cursor, device_name);
			
			if has_element (device_electrical) then

				update_element (
					container	=> module.devices,
					position	=> device_electrical,
					process		=> flip_electrical'access);

			else
				-- Search among non-electrical devices:
				device_non_electrical := get_non_electrical_device (module_cursor, device_name);

				if has_element (device_non_electrical) then

					update_element (
						container	=> module.devices_non_electric,
						position	=> device_non_electrical,
						process		=> flip_non_electrical'access);

				-- If the requested device has not been found,
				-- then log a warning:
				else
					log (WARNING, " Device " & to_string (device_name) & " not found !");
				end if;

			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " flip device " & to_string (device_name),
			 -- & " to " & to_string (face), 
			 -- CS: toggle, face
			 level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		update_ratsnest (module_cursor, log_threshold + 1);		
	end flip_device;



	
	
--------------------------------------------------------------------------------------

-- NON-ELECTRICAL DEVICES:


	function non_electrical_device_exists (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name)
		return boolean
	is
		device_found : boolean := false; -- to be returned
		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is begin
			if contains (module.devices_non_electric, device) then
				device_found := true;
			end if;
		end query_devices;
		
	begin
		pac_generic_modules.query_element (
			position	=> module,
			process		=> query_devices'access);

		return device_found;
	end non_electrical_device_exists;


	



	
	

	function get_non_electrical_device (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- FD1
		return pac_devices_non_electrical.cursor
	is
		result : pac_devices_non_electrical.cursor;
		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is begin
			result := find (module.devices_non_electric, device);
		end;

	begin
		pac_generic_modules.query_element (
			position	=> module,
			process		=> query_devices'access);

		return result;
	end get_non_electrical_device;

	
	
	

	
	
-- SHOW DEVICE:

	procedure show_non_electrical_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- FD1, MH2
		error			: out boolean;
		log_warning		: in boolean := true;
		log_threshold	: in type_log_level)
	is
		device_cursor : pac_devices_non_electrical.cursor;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			procedure query_device (
				device_name	: in type_device_name;
				device		: in out type_device_non_electrical) 
			is begin
				set_selected (device);
			end query_device;
			
		begin
			module.devices_non_electric.update_element (device_cursor, query_device'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			 & " show non-electrical device " & to_string (device_name),
			level => log_threshold);

		error := false;
		
		log_indentation_up;
		
		-- Deselect all objects of previous show operations
		-- so that nothing is highlighted anymore:
		et_schematic_ops.groups.reset_objects (module_cursor, log_threshold + 1);
		et_board_ops.groups.reset_objects (module_cursor, log_threshold + 1);
		
		-- Locate the targeted device in the given module.
		-- If the device exists, then proceed with further actions.
		-- Otherwise abort this procedure with a warning:
		device_cursor := get_non_electrical_device (module_cursor, device_name);
			
		if has_element (device_cursor) then -- device exists in board
			generic_modules.update_element (module_cursor, query_module'access);
		else
			if log_warning then
				log (WARNING, " Device " & to_string (device_name) & " not found !");
			end if;

			error := true;
		end if;

		log_indentation_down;
	end show_non_electrical_device;






	function get_device_properties (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name;
		level			: in type_properties_level;
		linebreaks		: in boolean := false;
		error			: out boolean;
		log_threshold	: in type_log_level)
		return string
	is
		device_cursor : pac_devices_non_electrical.cursor;


		use ada.strings.unbounded;
		result : unbounded_string := to_unbounded_string ("");

		
	begin
		error := false;
		
		log (text => "module " & to_string (module_cursor) 
			 & " get properties of non-electrical device " & to_string (device_name)
			 & " linebreaks " & boolean'image (linebreaks)
			 & " inquiry level " & to_string (level),
			level => log_threshold);

		
		log_indentation_up;
		
		-- Locate the targeted device in the given module.
		-- If the device exists, then proceed with further actions.
		-- Otherwise abort this function, set the error flag and return
		-- an empty string:
		device_cursor := get_non_electrical_device (module_cursor, device_name);
			
		if has_element (device_cursor) then -- device exists in the board drawing

			result := to_unbounded_string (get_properties (
				device_cursor	=> device_cursor,
				linebreaks		=> linebreaks,											  
				level			=> level));
				
		else
			log (WARNING, " Device " & to_string (device_name) & " not found !");
			error := true;			
		end if;

		log_indentation_down;

		return to_string (result);
	end get_device_properties;
	


	



	
	function get_device_name (
		object	: in type_object_non_electrical)
		return string
	is begin
		return to_string (key (object.cursor));
	end;



	function get_device_name (
		object	: in type_object_non_electrical)
		return type_device_name
	is begin
		return key (object.cursor);
	end;

	
	

	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		device			: in type_object_non_electrical;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_device (
				device_name	: in type_device_name;
				device		: in out type_device_non_electrical)
			is begin
				modify_status (device, operation);
			end query_device;


		begin
			module.devices_non_electric.update_element (device.cursor, query_device'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of non-electrical device "
			& to_string (key (device.cursor))
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;




	
	
	procedure propose_non_electrical_devices (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural; -- the number of affected devices
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_device (
				device_name	: in type_device_name;
				device		: in out type_device_non_electrical)
			is begin
				log (text => to_string (device_name), level => log_threshold + 1);
				set_proposed (device.status);
				count := count + 1;
			end query_device;

			
			device_cursor : pac_devices_non_electrical.cursor := module.devices_non_electric.first;
			
		begin
			while device_cursor /= pac_devices_non_electrical.no_element loop

				-- log (text => "probing device " & to_string (key (device_cursor)),
				-- 	 level => log_threshold + 1);
				-- log_indentation_up;
					
				if in_catch_zone (
					zone	=> catch_zone, 
					point	=> get_place (device_cursor)) 
				then
					-- log_indentation_up;
					-- log (text => "in catch zone", level => log_threshold + 1);

					module.devices_non_electric.update_element (device_cursor, query_device'access);
					-- log_indentation_down;
				end if;
				
				next (device_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " proposing non-electrical devices in" & to_string (catch_zone),
			level => log_threshold);

		log_indentation_up;

				generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end propose_non_electrical_devices;





	
	
	procedure reset_proposed_non_electrical_devices (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_device (
				device_name	: in type_device_name;
				device		: in out type_device_non_electrical)
			is begin
				log (text => to_string (device_name), level => log_threshold + 1);
				reset_status (device);
			end query_device;

			
			device_cursor : pac_devices_non_electrical.cursor := module.devices_non_electric.first;
		begin
			while device_cursor /= pac_devices_non_electrical.no_element loop
					
				-- log (text => "probing device " & to_string (key (device_cursor)),
				-- 	 level => log_threshold + 1);
				-- log_indentation_up;
					
				module.devices_non_electric.update_element (device_cursor, query_device'access);

				-- log_indentation_down;
				
				next (device_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			& "resetting proposed non-electrical devices", 
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_proposed_non_electrical_devices;




	
	function get_first_non_electrical_device (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return pac_devices_non_electrical.cursor
	is
		result : pac_devices_non_electrical.cursor;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			device_cursor : pac_devices_non_electrical.cursor := module.devices_non_electric.first;
		begin
			while device_cursor /= pac_devices_non_electrical.no_element loop
				case flag is
					when PROPOSED =>
						if is_proposed (device_cursor) then
							result := device_cursor;
							exit; -- no further probing required
						end if;
						
					when SELECTED =>
						if is_selected (device_cursor) then
							result := device_cursor;
							exit; -- no further probing required
						end if;

					when others => null; -- CS
				end case;
						
				next (device_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first non-electrical device /" & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;

		return result;
	end get_first_non_electrical_device;
	



	

	function get_first_non_electrical_device (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_non_electrical
	is
		result : type_object_non_electrical;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			device_cursor : pac_devices_non_electrical.cursor := module.devices_non_electric.first;
		begin
			while device_cursor /= pac_devices_non_electrical.no_element loop
				case flag is
					when PROPOSED =>
						if is_proposed (device_cursor) then
							result.cursor := device_cursor;
							exit; -- no further probing required
						end if;
						
					when SELECTED =>
						if is_selected (device_cursor) then
							result.cursor := device_cursor;
							exit; -- no further probing required
						end if;

					when others => null; -- CS
				end case;
						
				next (device_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "looking up the first non-electrical device /" & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;

		return result;
	end get_first_non_electrical_device;


	
	
		
		


	
	function get_non_electrical_devices_by_prefix (
		module_cursor	: in pac_generic_modules.cursor;
		prefix			: in pac_device_prefix.bounded_string; -- FD
		log_threshold	: in type_log_level)
		return pac_devices_non_electrical.map
	is
		result : pac_devices_non_electrical.map;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			
			procedure query_device (c : in pac_devices_non_electrical.cursor) is
				use pac_device_prefix;
				device	: type_device_non_electrical renames element (c);
				name 	: constant type_device_name := key (c); -- FD3
			begin
				-- Select only those devices which have the given prefix
				-- and add them to the result:
				if get_prefix (name) = prefix then
					log (text => to_string (name), level => log_threshold + 1);
					result.insert (name, device);
				end if;
			end query_device;
			
		begin
			-- Iterate the electrical devices:
			module.devices_non_electric.iterate (query_device'access);
		end query_module;


		
	begin
		log (text => "module " & to_string (module_cursor) 
			& " get non-electrical devices with prefix " & to_string (prefix),
			level => log_threshold);

		log_indentation_up;
		
		query_element (module_cursor, query_module'access);

		log (text => "device count " & get_count (result), level => log_threshold);
		
		log_indentation_down;
		
		return result;
	end get_non_electrical_devices_by_prefix;




	
	

	
	procedure add_non_electrical_device (
		module_cursor	: in pac_generic_modules.cursor;
		package_model	: in pac_package_model_file.bounded_string; -- ../lbr/packages/fiducial.pac
		position		: in type_package_position; -- x,y,rotation,face
		prefix			: in pac_device_prefix.bounded_string; -- FD
		log_threshold	: in type_log_level) 
	is

		package_cursor_lib : pac_package_models.cursor;

		
		procedure query_module (						  
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_cursor : pac_devices_non_electrical.cursor;
			inserted : boolean;

			next_name : type_device_name;
		begin
			-- Build the next available device name:
			next_name := get_next_available_device_name (
				module_cursor, prefix, log_threshold + 1);
						
			log (text => "add device " & to_string (next_name), 
				 level => log_threshold + 1);
			
			log_indentation_up;

			-- Add the non-electrical device to the module:
			pac_devices_non_electrical.insert (
				container	=> module.devices_non_electric,
				inserted	=> inserted,
				position	=> device_cursor,
				key			=> next_name,
				new_item	=> (
					position		=> position,
					package_model	=> package_model,
					placeholders	=> get_default_placeholders (package_cursor_lib),
					others			=> <>)
				);

			-- check inserted flag
			if not inserted then
				raise constraint_error;
			end if;

			log_indentation_down;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			& " add non-electrical device " & to_string (package_model) 
			& " at " & to_string (position),
			level => log_threshold);

		log_indentation_up;
		
		-- Read the package model (like ../libraries/fiducials/crosshair.pac)
		-- and store it in the rig wide package library et_packages.packages.
		-- If it s already in the library, nothing happens:
		et_package_read.read_package (
			file_name		=> package_model,
-- CS						check_layers	=> YES,
			log_threshold	=> log_threshold + 1);

		-- locate the package in the library
		package_cursor_lib := get_package_model (package_model);

		-- add the device to the module
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end add_non_electrical_device;


	


	

	procedure copy_non_electrical_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- FD1
		destination		: in type_vector_model; -- x,y
		log_threshold	: in type_log_level)
	is
		device_cursor : pac_devices_non_electrical.cursor;
		
		-- The next available device name:
		next_name : type_device_name;


		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is 
			-- Take a copy of the original device:
			new_device : type_device_non_electrical := element (device_cursor);
		begin
			-- Set the position of the new device as
			-- given by the destination:
			set_place (new_device, destination);

			-- Insert the new device in the module:
			module.devices_non_electric.insert (next_name, new_device);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			 & " copy non-electrical device " & to_string (device_name)
			 & " to destination " & to_string (destination),
			 level => log_threshold);

		log_indentation_up;
		
		-- Locate the targeted device in the given module.
		-- If the device exists, then proceed with further actions.
		-- Otherwise abort this procedure with a warning:
		device_cursor := get_non_electrical_device (module_cursor, device_name);

		-- Build the next available device name:
		next_name := get_next_available_device_name (
			module_cursor, get_prefix (device_name), log_threshold + 1); -- FD2

		
		if has_element (device_cursor) then -- device exists in board
			generic_modules.update_element (module_cursor, query_module'access);
		else
			log (WARNING, " Device " & to_string (device_name) & " not found !");
		end if;

		log_indentation_down;
	end copy_non_electrical_device;



	

	
	procedure delete_non_electrical_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- FD1
		log_threshold	: in type_log_level) 
	is
		device_cursor : pac_devices_non_electrical.cursor;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			delete (module.devices_non_electric, device_cursor);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			& " delete non-electrical device " & to_string (device_name),
			 level => log_threshold);

		log_indentation_up;
		
		-- Locate the targeted device in the given module.
		-- If the device exists, then proceed with further actions.
		-- Otherwise abort this procedure with a warning:
		device_cursor := get_non_electrical_device (module_cursor, device_name);
			
		if has_element (device_cursor) then -- device exists in board
			generic_modules.update_element (module_cursor, query_module'access);
		else
			log (WARNING, " Device " & to_string (device_name) & " not found !");
		end if;

		log_indentation_down;
	end delete_non_electrical_device;



	
	
	procedure rename_non_electrical_device (
		module_cursor		: in pac_generic_modules.cursor;
		device_name_before	: in type_device_name; -- FD1
		device_name_after	: in type_device_name; -- FD3
		log_threshold		: in type_log_level) 
	is		
		device_cursor : pac_devices_non_electrical.cursor;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is 
			device_after : pac_devices_non_electrical.cursor;
			inserted : boolean;
		begin
			-- Copy elements and properties of the old device to a new one:
			pac_devices_non_electrical.insert (
				container	=> module.devices_non_electric,
				key			=> device_name_after, -- FD3
				new_item	=> element (device_cursor), -- all elements and properties of FD1
				inserted	=> inserted,
				position	=> device_after);

			if not inserted then
				log (WARNING, "Device " & to_string (device_name_after)
					 & " already exists !");
			end if;

			-- Delete the old device:
			delete (module.devices_non_electric, device_cursor);
		end query_module;



		procedure check_names is begin
			-- The old and new name must not be the same:
			if device_name_after /= device_name_before then

				-- The old and new prefix must be the same in order to
				-- prevent an inadvertently category change:
				if same_prefix (device_name_after, device_name_before) then

					-- A device having the new name must
					-- not exist yet:
					if not non_electrical_device_exists (module_cursor, device_name_after) then
						
						update_element (
							container	=> generic_modules,
							position	=> module_cursor,
							process		=> query_module'access);

					else
						log (WARNING, "Device " & to_string (device_name_after)
							 & " already exists !");
					end if;
				else
					log (WARNING, "Changing the prefix is not allowed !");
				end if;
			else
				log (WARNING, "Old and new device name are equal !");
			end if;
		end check_names;
		
		
		
	begin
		log (text => "module " & to_string (module_cursor) 
			 & " rename non-electrical device " & to_string (device_name_before) 
			 & " to " &  to_string (device_name_after),
			level => log_threshold);
				
		log_indentation_up;
		
		-- Locate the targeted device in the given module.
		-- If the device exists, then proceed with further actions.
		-- Otherwise abort this procedure with a warning:
		device_cursor := get_non_electrical_device (module_cursor, device_name_before);
			
		if has_element (device_cursor) then -- device exists in board
			check_names;			
		else
			log (WARNING, " Device " & to_string (device_name_before) & " not found !");
		end if;

		log_indentation_down;
	end rename_non_electrical_device;
	



	


-- PLACEHOLDERS:


	procedure reset_placeholder_positions (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_electrical		: pac_devices_electrical.cursor;
			device_non_electrical	: pac_devices_non_electrical.cursor;			

			
			procedure reset_electrical (
				device_name	: in type_device_name;
				device		: in out type_device_electrical) 
			is begin
				log (text => "reset_electrical", level => log_threshold + 1);
				reset_placeholder_positions (device);
			end;

			
			procedure reset_non_electrical (
				device_name	: in type_device_name;
				device		: in out type_device_non_electrical) 
			is begin
				log (text => "reset_non_electrical", level => log_threshold + 1);
				reset_placeholder_positions (device);
			end;

			
		begin

			-- Search the device first among the electrical devices.
			-- Most likely it will be among them. If not,
			-- search in non-electrical devices:
			
			device_electrical := get_electrical_device (module_cursor, device_name);
			
			if has_element (device_electrical) then

				update_element (
					container	=> module.devices,
					position	=> device_electrical,
					process		=> reset_electrical'access);

			else
				-- Search among non-electrical devices:
				device_non_electrical := get_non_electrical_device (module_cursor, device_name);

				if has_element (device_non_electrical) then

					update_element (
						container	=> module.devices_non_electric,
						position	=> device_non_electrical,
						process		=> reset_non_electrical'access);

				-- If the requested device has not been found,
				-- then log a warning:
				else
					log (WARNING, " Device " & to_string (device_name) & " not found !");
				end if;

			end if;
		end query_module;
	
	
	begin
		log (text => "module " & to_string (module_cursor)
			& " reset " & to_string (device_name) 
			& " placeholder positions",
			level => log_threshold);
	

		log_indentation_up;
			
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
	
		log_indentation_down;
	end reset_placeholder_positions;
	
	

	
	
	

	procedure move_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name;
		meaning			: in type_placeholder_meaning;
		layer			: in type_placeholder_layer;
		face			: in type_face;
		index			: in type_placeholder_index;
		coordinates		: in type_coordinates;
		point			: in type_vector_model;
		log_threshold	: in type_log_level)
	is
		

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_electrical		: pac_devices_electrical.cursor;
			device_non_electrical	: pac_devices_non_electrical.cursor;			

			
			procedure move_electrical (
				device_name	: in type_device_name;
				device		: in out type_device_electrical) 
			is begin
				log (text => "move_electrical", level => log_threshold + 1);
				move_placeholder (device, meaning, layer, face, index, coordinates, point);

				-- log (text => "new: " & to_string (device.placeholders),
					 -- level => log_threshold + 2);
			end;

			
			procedure move_non_electrical (
				device_name	: in type_device_name;
				device		: in out type_device_non_electrical) 
			is begin
				log (text => "move_non_electrical", level => log_threshold + 1);
				move_placeholder (device, meaning, layer, face, index, coordinates, point);

				-- log (text => "new: " & to_string (device.placeholders),
					 -- level => log_threshold + 2);
			end;

			
		begin

			-- Search the device first among the electrical devices.
			-- Most likely it will be among them. If not,
			-- search in non-electrical devices:
			
			device_electrical := get_electrical_device (module_cursor, device_name);
			
			if has_element (device_electrical) then

				update_element (
					container	=> module.devices,
					position	=> device_electrical,
					process		=> move_electrical'access);

			else
				-- Search among non-electrical devices:
				device_non_electrical := get_non_electrical_device (module_cursor, device_name);

				if has_element (device_non_electrical) then

					update_element (
						container	=> module.devices_non_electric,
						position	=> device_non_electrical,
						process		=> move_non_electrical'access);

				-- If the requested device has not been found,
				-- then log a warning:
				else
					log (WARNING, " Device " & to_string (device_name) & " not found !");
				end if;

			end if;
		end query_module;
	

		
	begin
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_cursor)
					& " move " & to_string (device_name) 
					& " placeholder " & enclose_in_quotes (to_string (meaning))
					& " layer " & to_string (layer)
					& " face " & to_string (face)
					& " index " & to_string (index)
					& " to " & to_string (point),
					level => log_threshold);


			when RELATIVE =>
				log (text => "module " & to_string (module_cursor)
					& " move " & to_string (device_name) 
					& " placeholder " & enclose_in_quotes (to_string (meaning))
					& " layer " & to_string (layer)
					& " face " & to_string (face)
					& " index " & to_string (index)
					& " by " & to_string (point),
					level => log_threshold);

		end case;

		
		log_indentation_up;
			
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
	
		log_indentation_down;		
	end move_placeholder;





	

	

	procedure rotate_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name;
		meaning			: in type_placeholder_meaning;
		layer			: in type_placeholder_layer;
		face			: in type_face;
		index			: in type_placeholder_index;
		coordinates		: in type_coordinates;
		rotation		: in type_rotation_model := 90.0;
		log_threshold	: in type_log_level) 
	is


		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_electrical		: pac_devices_electrical.cursor;
			device_non_electrical	: pac_devices_non_electrical.cursor;			

			
			procedure rotate_electrical (
				device_name	: in type_device_name;
				device		: in out type_device_electrical) 
			is begin
				log (text => "rotate_electrical", level => log_threshold + 1);
				
				rotate_placeholder (device, meaning, layer, face,
					index, coordinates, rotation);
			end;

			
			procedure rotate_non_electrical (
				device_name	: in type_device_name;
				device		: in out type_device_non_electrical) 
			is begin
				log (text => "rotate_non_electrical", level => log_threshold + 1);
				
				rotate_placeholder (device, meaning, layer, face,
					index, coordinates, rotation);
			end;

			
		begin

			-- Search the device first among the electrical devices.
			-- Most likely it will be among them. If not,
			-- search in non-electrical devices:
			
			device_electrical := get_electrical_device (module_cursor, device_name);
			
			if has_element (device_electrical) then

				update_element (
					container	=> module.devices,
					position	=> device_electrical,
					process		=> rotate_electrical'access);

			else
				-- Search among non-electrical devices:
				device_non_electrical := get_non_electrical_device (module_cursor, device_name);

				if has_element (device_non_electrical) then

					update_element (
						container	=> module.devices_non_electric,
						position	=> device_non_electrical,
						process		=> rotate_non_electrical'access);

				-- If the requested device has not been found,
				-- then log a warning:
				else
					log (WARNING, " Device " & to_string (device_name) & " not found !");
				end if;

			end if;
		end query_module;
		
		
	begin
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_cursor)
					& " rotate " & to_string (device_name) 
					& " placeholder " & enclose_in_quotes (to_string (meaning))
					& " layer " & to_string (layer)
					& " face " & to_string (face)
					& " index " & to_string (index)
					& " to " & to_string (rotation),
					level => log_threshold);


			when RELATIVE =>
				log (text => "module " & to_string (module_cursor)
					& " rotate " & to_string (device_name) 
					& " placeholder " & enclose_in_quotes (to_string (meaning))
					& " layer " & to_string (layer)
					& " face " & to_string (face)
					& " index " & to_string (index)
					& " by " & to_string (rotation),
					level => log_threshold);

		end case;

		
		log_indentation_up;
			
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end rotate_placeholder;




	
	function get_device_name (
		placeholder	: in type_object_placeholder)
		return type_device_name
	is begin
		if has_element (placeholder.device_electrical) then
			return get_device_name (placeholder.device_electrical);
		else
			return get_device_name (placeholder.device_non_electrical);
		end if;
	end;
	
	


	function get_device_name (
		placeholder	: in type_object_placeholder)
		return string
	is begin
		return to_string (get_device_name (placeholder));
	end;
	
	

	
	function get_place (
		placeholder	: in type_object_placeholder)
		return type_vector_model
	is 
		use pac_text_placeholders;
	begin
		return get_place (element (placeholder.placeholder));
	end;

	
	
	
	function get_layer (
		placeholder	: in type_object_placeholder)
		return string
	is begin
		return to_string (placeholder.layer);
	end;

	
	
	
	function get_meaning (
		placeholder	: in type_object_placeholder)
		return type_placeholder_meaning
	is begin
		return get_meaning (placeholder.placeholder);
	end;

	
	
	


	function to_string (
		placeholder	: in type_object_placeholder)
		return string
	is 
		use pac_text_placeholders;
	begin
		return "device " & get_device_name (placeholder)
			& " " & to_string (element (placeholder.placeholder))
			& " layer " & get_layer (placeholder)
			& " face " & to_string (placeholder.face)
			& " index " & to_string (placeholder.index);
	end to_string;

	
	

	
	
	
	
	procedure propose_placeholders (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is
	
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			
			-- This procedure queries the placeholders of
			-- all electrical devices:
			procedure query_electrical_devices is
				device_cursor : pac_devices_electrical.cursor := module.devices.first;
			
			
				procedure query_device (
					device_name	: in type_device_name;
					device		: in out type_device_electrical)
				is begin
					log (text => to_string (device_name), level => log_threshold + 2);
					
					propose_placeholders (device.placeholders, 
						et_devices_electrical.get_position (device), catch_zone, count, log_threshold + 3);
				end query_device;

				
			begin
				log (text => "query_electrical_devices", level => log_threshold + 1);
				log_indentation_up;
				
				while has_element (device_cursor) loop

					if is_real (device_cursor) then -- ignore virtual devices (like GND symbols)
						module.devices.update_element (device_cursor, query_device'access);
					end if;
					
					next (device_cursor);
				end loop;
				
				log_indentation_down;
			end query_electrical_devices;
			

			
			
			-- This procedure queries the placeholders of
			-- all non-electrical devices:			
			procedure query_non_electrical_devices is
				device_cursor : pac_devices_non_electrical.cursor := module.devices_non_electric.first;
					
					
				procedure query_device (
					device_name	: in type_device_name;
					device		: in out type_device_non_electrical)
				is begin
					log (text => to_string (device_name), level => log_threshold + 2);	
					
					propose_placeholders (device.placeholders,
						get_position (device), catch_zone, count, log_threshold + 3);					
				end query_device;

				
			begin
				log (text => "query_non_electrical_devices", level => log_threshold + 1);
				log_indentation_up;
				
				while has_element (device_cursor) loop
					module.devices_non_electric.update_element (device_cursor, query_device'access);					
					next (device_cursor);
				end loop;
				
				log_indentation_down;
			end query_non_electrical_devices;

			
			
		begin
			query_electrical_devices;
			query_non_electrical_devices;
		end query_module;

	
	
	begin
		log (text => "module " & to_string (module_cursor)
			& " propose placeholders in " & to_string (catch_zone),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end propose_placeholders;

	
	
	
	
	
	
	
	
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		placeholder		: in type_object_placeholder;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is 
	
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_electrical_device (
				device_name	: in type_device_name;
				device		: in out type_device_electrical)
			is begin
				modify_status (
					placeholders		=> device.placeholders, 
					layer				=> placeholder.layer, 
					face				=> placeholder.face,
					placeholder_cursor	=> placeholder.placeholder, 
					operation			=> operation);
			end query_electrical_device;


			procedure query_non_electrical_device (
				device_name	: in type_device_name;
				device		: in out type_device_non_electrical)
			is begin
				modify_status (
					placeholders		=> device.placeholders, 
					layer				=> placeholder.layer, 
					face				=> placeholder.face,
					placeholder_cursor	=> placeholder.placeholder, 
					operation			=> operation);
			end query_non_electrical_device;

			
			
		begin
			if has_element (placeholder.device_electrical) then
				module.devices.update_element (
					placeholder.device_electrical, query_electrical_device'access);
					
			else
				module.devices_non_electric.update_element (
					placeholder.device_non_electrical, query_non_electrical_device'access);
			
			end if;
		end query_module;
	
	
	
	begin
		log (text => "module " & to_string (module_cursor)
			& " modify status of placeholder "
			& get_device_name (placeholder)
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;

	
	
	
	
	
	
	
	function get_first_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_placeholder
	is
		result : type_object_placeholder;
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
		
			-- This procedure searches among the electrical devices
			-- for the one that has the first placeholder set as
			-- given by "flag". On the first match, the search is stopped
			-- and the result set accordingly:
			procedure query_electrical_devices is
				device_cursor : pac_devices_electrical.cursor := 
					module.devices.first;
					
					
				procedure query_device (
					device_name	: in type_device_name;
					device		: in type_device_electrical)
				is 
					use pac_text_placeholders;
				begin
					if is_real (device) then
						log (text => to_string (device_name), level => log_threshold + 2);
						log_indentation_up;

						get_first_placeholder (
							placeholders		=> device.placeholders,
							flag				=> flag,
							placeholder_cursor	=> result.placeholder,
							layer				=> result.layer,
							face				=> result.face,
							index				=> result.index,
							log_threshold		=> log_threshold + 3);

						-- If a matching placeholder has been found, then
						-- set the cursor of the device in the result:
						if has_element (result.placeholder) then
							result.device_electrical := device_cursor;
						end if;
						
						log_indentation_down;
					end if;
				end;
				 
			begin
				log (text => "query_electrical_devices", level => log_threshold + 1);
				log_indentation_up;
				
				while has_element (device_cursor) loop
					query_element (device_cursor, query_device'access);
					
					-- A device has been found -> abort the search:
					if has_element (result.device_electrical) then
						exit;
					end if;
					
					next (device_cursor);
				end loop;			
				
				log_indentation_down;
			end query_electrical_devices;
			
			


			-- This procedure searches among the non-electrical devices
			-- for the one that has the first placeholder set as
			-- given by "flag". On the first match, the search is stopped
			-- and the result set accordingly:			
			procedure query_non_electrical_devices is
				device_cursor : pac_devices_non_electrical.cursor := 
					module.devices_non_electric.first;

					
				procedure query_device (
					device_name	: in type_device_name;
					device		: in type_device_non_electrical)
				is 
					use pac_text_placeholders;
				begin
					log (text => to_string (device_name), level => log_threshold + 2);
					log_indentation_up;
					
					get_first_placeholder (
						placeholders		=> device.placeholders,
						flag				=> flag,
						placeholder_cursor	=> result.placeholder,
						layer				=> result.layer,
						face				=> result.face,
						index				=> result.index,
						log_threshold		=> log_threshold + 3);

					-- If a matching placeholder has been found, then
					-- set the cursor of the device in the result:
					if has_element (result.placeholder) then
						result.device_non_electrical := device_cursor;
					end if;
					
					log_indentation_down;
				end;
		
					
			begin
				log (text => "query_non_electrical_devices", level => log_threshold + 1);
				log_indentation_up;
			
				while has_element (device_cursor) loop
					query_element (device_cursor, query_device'access);
					
					-- A device has been found -> abort the search:
					if has_element (result.device_non_electrical) then
						exit;
					end if;
					
					next (device_cursor);
				end loop;
				
				log_indentation_down;
			end query_non_electrical_devices;

			
			
		begin
			-- First search among the electrical devices.
			-- If nothing found, search in non-electrical devices:
			query_electrical_devices;
			
			if not has_element (result.device_electrical) then
				query_non_electrical_devices;
			end if;
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " look up the first placeholder /" & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;

		return result;
	end get_first_placeholder;
	
	
	
	
	
	
	

	procedure reset_status_placeholders (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
	
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_electrical_device (
				device_name	: in type_device_name;
				device		: in out type_device_electrical)
			is begin
				if is_real (device) then
					reset_status (device.placeholders);
				end if;
			end query_electrical_device;


			procedure query_non_electrical_device (
				device_name	: in type_device_name;
				device		: in out type_device_non_electrical)
			is begin
				reset_status (device.placeholders);
			end query_non_electrical_device;

			
			device_electrical : pac_devices_electrical.cursor := 
				module.devices.first;
				
			device_non_electrical : pac_devices_non_electrical.cursor := 
				module.devices_non_electric.first;
				
		begin
			-- Iterate though all electrical devices:
			while has_element (device_electrical) loop
				module.devices.update_element (
					device_electrical, query_electrical_device'access);
					
				next (device_electrical);
			end loop;

			
			-- Iterate though all non-electrical devices:
			while has_element (device_non_electrical) loop
				module.devices_non_electric.update_element (
					device_non_electrical, query_non_electrical_device'access);
					
				next (device_non_electrical);
			end loop;
		end query_module;

	
	
	begin
		null;
		log (text => "module " & to_string (module_cursor) 
			& "reset placeholders of devices", 
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_status_placeholders;

	
	
	
------------------------------------------------------------------------------------------


	function get_count (
		objects : in pac_objects.list)
		return natural
	is begin
		return natural (objects.length);
	end get_count;
	
	

	

	function get_first_object (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object
	is
		result_category 		: type_object_category := CAT_VOID;
		result_electrical 	 	: type_object_electrical;
		result_non_electrical	: type_object_non_electrical;
		result_placeholder		: type_object_placeholder;

		use pac_devices_non_electrical;
		use pac_devices_electrical;
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first object / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;


		-- SEARCH FOR A PLACEHOLDER:
		
		-- If a placeholder has been found, then go to the end of this procedure:
		result_placeholder := get_first_placeholder (module_cursor, flag, log_threshold + 1);

		if has_element (result_placeholder.device_electrical) or
		   has_element (result_placeholder.device_non_electrical) then
			-- A placeholder has been found.
			log (text => to_string (result_placeholder),
				 level => log_threshold + 1);
			
			result_category := CAT_PLACEHOLDER;
		end if;

		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;

		
		

		
		-- SEARCH FOR AN ELECTRICAL DEVICE:
		
		-- If a device has been found, then go to the end of this procedure:
		result_electrical := get_first_device (module_cursor, flag, log_threshold + 1);

		if result_electrical.cursor /= pac_devices_electrical.no_element then
			-- A device has been found.
			log (text => get_device_name (result_electrical.cursor),
				 level => log_threshold + 1);
			
			result_category := CAT_ELECTRICAL_DEVICE;
		end if;

		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;



		
		-- SEARCH FOR A NON-ELECTRICAL DEVICE:
		
		-- If a device has been found, then go to the end of this procedure:
		result_non_electrical := 
			get_first_non_electrical_device (module_cursor, flag, log_threshold + 1);

		if result_non_electrical.cursor /= pac_devices_non_electrical.no_element then
			-- A device has been found.
			log (text => get_device_name (result_non_electrical.cursor),
				 level => log_threshold + 1);
			
			result_category := CAT_NON_ELECTRICAL_DEVICE;
		end if;

		
		-- If still nothing has been found then the category is CAT_VOID.
		

	<<end_of_search>>
		
		log_indentation_down;

		case result_category is
			when CAT_VOID =>
				return (cat => CAT_VOID);

			when CAT_ELECTRICAL_DEVICE =>
				return (CAT_ELECTRICAL_DEVICE, result_electrical);

			when CAT_NON_ELECTRICAL_DEVICE =>
				return (CAT_NON_ELECTRICAL_DEVICE, result_non_electrical);
				
			when CAT_PLACEHOLDER =>
				return (CAT_PLACEHOLDER, result_placeholder);
				
		end case;
	end get_first_object;


	
	

	

	function get_objects (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return pac_objects.list
	is
		use pac_objects;

		-- Here the objects are collected:
		result : pac_objects.list;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
		-- DEVICES ---------------------------------------------
	
			-- This procedure collects electrical devices
			-- according to the given flag:
			procedure query_electrical_devices is

				use pac_devices_electrical;
				cursor : pac_devices_electrical.cursor;

			
				procedure query_device (
					name	: in type_device_name;
					device	: in type_device_electrical) 
				is 

					procedure collect is begin
						result.append ((
							cat					=> CAT_ELECTRICAL_DEVICE,
							electrical_device	=> (cursor => cursor)));

						log (text => get_device_name (cursor), level => log_threshold + 2);
					end collect;
					
				begin
					case flag is
						when PROPOSED =>
							if is_proposed (device) then
								collect;
							end if;

						when SELECTED =>
							if is_selected (device) then
								collect;
							end if;

						when others => null; -- CS
					end case;
				end query_device;

			
			begin
				log (text => "electrical devices", level => log_threshold + 1);
				log_indentation_up;

				-- Iterate the electrical devices of the module:
				cursor := module.devices.first;
				while has_element (cursor) loop
					query_element (cursor, query_device'access);
					next (cursor);
				end loop;

				log_indentation_down;
			end query_electrical_devices;
			

			
			
			-- This procedure collects non-electrical devices
			-- according to the given flag:
			procedure query_non_electrical_devices is

				use pac_devices_non_electrical;
				cursor : pac_devices_non_electrical.cursor;

						
				procedure query_device (
					name	: in type_device_name;
					device	: in type_device_non_electrical) 
				is 

					procedure collect is begin
						result.append ((
							cat						=> CAT_NON_ELECTRICAL_DEVICE,
							non_electrical_device	=> (cursor => cursor)));

						log (text => get_device_name (cursor), level => log_threshold + 2);
					end collect;

					
				begin
					case flag is
						when PROPOSED =>
							if is_proposed (device) then
								collect;
							end if;

						when SELECTED =>
							if is_selected (device) then
								collect;
							end if;

						when others => null; -- CS
					end case;
				end query_device;

				
			begin
				log (text => "non-electrical devices", level => log_threshold + 1);
				log_indentation_up;

				-- Iterate the non-electrical devices of the module:
				cursor := module.devices_non_electric.first;
				while has_element (cursor) loop
					query_element (cursor, query_device'access);
					next (cursor);
				end loop;

				log_indentation_down;			
			end query_non_electrical_devices;
			
			
			
		-- PLACEHOLDERS ----------------------------------------
		
			-- This procedure collects placeholders of devices
			-- according to the given flag:
			procedure query_placeholders is
			
				procedure query_electrical_devices is

					use pac_devices_electrical;
					device_cursor : pac_devices_electrical.cursor;

				
					procedure query_device (
						name	: in type_device_name;
						device	: in type_device_electrical) 
					is 
						placeholders : type_placeholder_cursors;
						
						use pac_placeholder_cursors;

						layer : type_placeholder_layer;
						face : type_face;

					
						procedure query_placeholder (c : in pac_placeholder_cursors.cursor) is 
							pc : type_placeholder_cursor renames element (c);
							p : type_object_placeholder;
						begin
							p.device_electrical := device_cursor;
							p.device_non_electrical := pac_devices_non_electrical.no_element;
							p.placeholder := pc.cursor;
							p.layer := layer;
							p.face := face;
							p.index := pc.index;
										
							log (text => to_string (p), level => log_threshold + 4);
							result.append ((CAT_PLACEHOLDER, p));
						end;
						
					begin
						-- Get the placeholders of the candidate device according
						-- to the given flag. Then iterate through them and append
						-- them to the result:
						if is_real (device) then
							log (text => to_string (name), level => log_threshold + 2);
							log_indentation_up;
							
							placeholders := get_placeholder_cursors (
								device.placeholders, flag, log_threshold + 3);
						
							log_indentation_up;
							
							layer := SILKSCREEN;
							face := TOP;
							placeholders.silkscreen.top.iterate (query_placeholder'access);
							
							face := BOTTOM;
							placeholders.silkscreen.bottom.iterate (query_placeholder'access);
							
							layer := ASSY_DOC;							
							face := TOP;
							placeholders.assy_doc.top.iterate (query_placeholder'access);
							
							face := BOTTOM;
							placeholders.assy_doc.bottom.iterate (query_placeholder'access);
							
							log_indentation_down;
							log_indentation_down;
						end if;
					end query_device;

				
				begin
					log (text => "electrical devices", level => log_threshold + 1);
					log_indentation_up;

					-- Iterate the electrical devices of the module:
					device_cursor := module.devices.first;
					while has_element (device_cursor) loop
						query_element (device_cursor, query_device'access);
						next (device_cursor);
					end loop;

					log_indentation_down;
				end query_electrical_devices;

				

				
				
				procedure query_non_electrical_devices is

					use pac_devices_non_electrical;
					device_cursor : pac_devices_non_electrical.cursor;

				
					procedure query_device (
						name	: in type_device_name;
						device	: in type_device_non_electrical) 
					is 
						placeholders : type_placeholder_cursors;
						
						use pac_placeholder_cursors;

						layer : type_placeholder_layer;
						face : type_face;

					
						procedure query_placeholder (c : in pac_placeholder_cursors.cursor) is 
							pc : type_placeholder_cursor renames element (c);
							p : type_object_placeholder;
						begin
							p.device_non_electrical := device_cursor;
							p.device_electrical := pac_devices_electrical.no_element;
							p.placeholder := pc.cursor;
							p.layer := layer;
							p.face := face;
							p.index := pc.index;
									
							log (text => to_string (p), level => log_threshold + 4);
							result.append ((CAT_PLACEHOLDER, p));
						end;
						
					begin
						log (text => to_string (name), level => log_threshold + 2);
						log_indentation_up;
						
						-- Get the placeholders of the candidate device according
						-- to the given flag. Then iterate through them and append
						-- them to the result:
						placeholders := get_placeholder_cursors (
							device.placeholders, flag, log_threshold + 3);
					
						log_indentation_up;
						
						layer := SILKSCREEN;
						face := TOP;
						placeholders.silkscreen.top.iterate (query_placeholder'access);
						
						face := BOTTOM;
						placeholders.silkscreen.bottom.iterate (query_placeholder'access);
						
						layer := ASSY_DOC;							
						face := TOP;
						placeholders.assy_doc.top.iterate (query_placeholder'access);
						
						face := BOTTOM;
						placeholders.assy_doc.bottom.iterate (query_placeholder'access);
						
						log_indentation_down;
						log_indentation_down;
					end query_device;

				
				begin
					log (text => "non-electrical devices", level => log_threshold + 1);
					log_indentation_up;

					-- Iterate the non-electrical devices of the module:
					device_cursor := module.devices_non_electric.first;
					while has_element (device_cursor) loop
						query_element (device_cursor, query_device'access);
						next (device_cursor);
					end loop;

					log_indentation_down;
				end query_non_electrical_devices;

				
				
			begin
				log (text => "placeholders", level => log_threshold + 1);
				log_indentation_up;

				query_electrical_devices;
				query_non_electrical_devices;

				log_indentation_down;
			end query_placeholders;
			
			
		begin
			query_electrical_devices;			
			query_non_electrical_devices;
			query_placeholders;			
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up objects / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;

		-- log (text => "total " & natural'image (get_count (result)), 
		--	level => log_threshold);
		
		return result;
	end get_objects;


	
	

	

	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of object "
			& type_object_category'image (object.cat)
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		case object.cat is
			when CAT_ELECTRICAL_DEVICE =>
				modify_status (module_cursor, object.electrical_device, operation, log_threshold + 1);

			when CAT_NON_ELECTRICAL_DEVICE =>
				modify_status (module_cursor, object.non_electrical_device, operation, log_threshold + 1);

			when CAT_PLACEHOLDER =>
				modify_status (module_cursor, object.placeholder, operation, log_threshold + 1);
				
			when CAT_VOID =>
				null; -- CS
		end case;

		log_indentation_down;
	end modify_status;







	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object_cursor	: in pac_objects.cursor;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is 
		use pac_objects;
		object : constant type_object := element (object_cursor);
	begin
		log (text => "modify status", level => log_threshold);
		modify_status (module_cursor, object, operation, log_threshold);
	end modify_status;

	



	procedure reset_status_objects (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor) 
			& " reset objects",
			level => log_threshold);

		log_indentation_up;

		-- Reset electrical devices, units and packages:
		et_schematic_ops.units.reset_status_units (
			module_cursor, log_threshold + 1);

		-- Reset non-electrical devices:
		reset_proposed_non_electrical_devices (
			module_cursor, log_threshold + 1);

		-- Reset placeholders:
		reset_status_placeholders (
			module_cursor, log_threshold + 1);
		
		log_indentation_down;
	end reset_status_objects;




	


	procedure copy_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " copy object " 
			-- CS & to_string (object)
			& " to " & to_string (destination),
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_ELECTRICAL_DEVICE =>
				null;

				
			when CAT_NON_ELECTRICAL_DEVICE =>

				copy_non_electrical_device (
					module_cursor	=> module_cursor,
					device_name		=> get_device_name (object.non_electrical_device),
					destination		=> destination,
					log_threshold	=> log_threshold + 1);

				
			when CAT_PLACEHOLDER => null;
			
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end copy_object;
	



	
	
	


	procedure move_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " moving object " 
			-- CS & to_string (object)
			& " to " & to_string (destination),
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_ELECTRICAL_DEVICE =>

				move_device (
					module_cursor	=> module_cursor,
					device_name		=> get_device_name (object.electrical_device),
					coordinates		=> absolute,
					point			=> destination,
					log_threshold	=> log_threshold + 1);

				
			when CAT_NON_ELECTRICAL_DEVICE =>

				move_device (
					module_cursor	=> module_cursor,
					device_name		=> get_device_name (object.non_electrical_device),
					coordinates		=> absolute,
					point			=> destination,
					log_threshold	=> log_threshold + 1);

					
			when CAT_PLACEHOLDER =>

				move_placeholder (
					module_cursor	=> module_cursor,
					device_name		=> get_device_name (object.placeholder),
					meaning			=> get_meaning (object.placeholder),
					layer			=> object.placeholder.layer,
					face			=> object.placeholder.face,
					index			=> object.placeholder.index,
					coordinates		=> absolute,
					point			=> destination,
					log_threshold	=> log_threshold + 1);

				
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end move_object;
	


	


	procedure rotate_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " rotate object ",
			-- CS & to_string (object)
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_ELECTRICAL_DEVICE =>

				rotate_device (
					module_cursor	=> module_cursor,
					device_name		=> get_device_name (object.electrical_device),
					coordinates		=> relative,
					log_threshold	=> log_threshold + 1);

				
			when CAT_NON_ELECTRICAL_DEVICE =>

				rotate_device (
					module_cursor	=> module_cursor,
					device_name		=> get_device_name (object.non_electrical_device),
					coordinates		=> relative,
					log_threshold	=> log_threshold + 1);

					
			when CAT_PLACEHOLDER =>

				rotate_placeholder (
					module_cursor	=> module_cursor,
					device_name		=> get_device_name (object.placeholder),
					meaning			=> get_meaning (object.placeholder),
					layer			=> object.placeholder.layer,
					face			=> object.placeholder.face,
					index			=> object.placeholder.index,
					coordinates		=> relative,
					rotation		=> 90.0,
					log_threshold	=> log_threshold + 1);
					
				
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end rotate_object;
	



	



	procedure delete_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " delete object",
			-- CS & to_string (object)
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_ELECTRICAL_DEVICE =>
				null; -- CS
				
			when CAT_NON_ELECTRICAL_DEVICE =>

				delete_non_electrical_device (
					module_cursor	=> module_cursor,
					device_name		=> get_device_name (object.non_electrical_device),
					log_threshold	=> log_threshold + 1);
				
				
			when CAT_PLACEHOLDER => null; -- CS
			
				
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end delete_object;
	





	procedure flip_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " flip object",
			-- CS & to_string (object)
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_ELECTRICAL_DEVICE =>

				flip_device (
					module_cursor	=> module_cursor,
					device_name		=> get_device_name (object.electrical_device),
					toggle			=> true,
					log_threshold	=> log_threshold + 1);

				
			when CAT_NON_ELECTRICAL_DEVICE =>

				flip_device (
					module_cursor	=> module_cursor,
					device_name		=> get_device_name (object.non_electrical_device),
					toggle			=> true,
					log_threshold	=> log_threshold + 1);

				
			when CAT_PLACEHOLDER => null;
				
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end flip_object;

	


	

	procedure rename_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		new_name_device	: in type_device_name;
		-- CS add argument for new names of other kinds of objects
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " rename object",
			-- CS & to_string (object)
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_NON_ELECTRICAL_DEVICE =>
				
				rename_non_electrical_device (
					module_cursor		=> module_cursor,
					device_name_before	=> get_device_name (object.non_electrical_device),
					device_name_after	=> new_name_device,
					log_threshold		=> log_threshold + 1);
				
			when others =>
				null;
		end case;		
		
		log_indentation_down;
	end rename_object;


	


	

	

	procedure show_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level)
	is 
		error : boolean := false;
	begin
		log (text => "module " & to_string (module_cursor)
			& " show object",
			-- CS & to_string (object)
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_ELECTRICAL_DEVICE =>

				-- Show the device in the schematic drawing
				-- and in the board drawing:
				show_device (
					module_cursor	=> module_cursor,
					device_name		=> get_device_name (object.electrical_device.cursor),
					all_units		=> true,
					error			=> error,
					log_threshold	=> log_threshold + 1);


			when CAT_NON_ELECTRICAL_DEVICE =>

				-- Show the device in the board drawing:
				show_non_electrical_device (
					module_cursor	=> module_cursor,
					device_name		=> get_device_name (object.non_electrical_device.cursor),
					error			=> error,
					log_threshold	=> log_threshold + 1);

				
			when CAT_PLACEHOLDER =>
				null; -- CS clear content ? or do nothing ?

								
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end show_object;

	


	

	
	-- Returns the position (x/y/rotation) of a submodule instance.
	-- Assumptions:
	--  - The module to be searched in must be in the rig already.
	--  - The submodule instance must exist in the module.
	function get_position (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string) -- OSC1
		return type_position 
	is		
		position : type_position := origin_zero_rotation; -- to be returned

		module_cursor : pac_generic_modules.cursor; -- points to the module

		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use et_submodules.pac_submodules;
			submod_cursor : et_submodules.pac_submodules.cursor;
		begin
			submod_cursor := find (module.submods, instance);
			position := element (submod_cursor).position_in_board;
		end;
		
	begin -- get_position
		-- locate the given module
		module_cursor := locate_module (module_name);

		pac_generic_modules.query_element (
			position	=> module_cursor,
			process		=> query_submodules'access);

		return position;
	end get_position;

	
	--function get_terminal_position (
		--module_cursor	: in et_project.modules.pac_generic_modules.cursor;
		--device_cursor	: in pac_devices_electrical.cursor; -- IC45
		--terminal_name	: in pac_terminal_name.bounded_string) -- H7, 14
		--return type_terminal_position_fine
	--is
		---- This is the position of the package as it is in the layout:
		--package_position : et_board_coordinates.type_package_position; -- incl. angle and face

		--use pac_geometry_brd;
		--terminal_position : type_vector; -- x/y
		--terminal_rotation : type_angle;
		--terminal_position_face : type_face := TOP; -- top/bottom

		--model : pac_package_model_file.bounded_string; -- libraries/packages/smd/SOT23.pac
		--package_model_cursor : pac_package_models.cursor;

		--use pac_terminals;
		---- This cursor points to the terminal in the package model:
		--terminal_cursor : pac_terminals.cursor;
		
		--terminal_technology : type_assembly_technology;
		
	--begin
		---- Get the package model of the given device:
		--model := get_package_model (device_cursor);

		---- Get the position of the package as it is in the layout:
		--package_position := pac_devices_electrical.element (device_cursor).position;
		
		---- Set the cursor to package model:
		--package_model_cursor := get_package_model (model);

		---- Locate the desired terminal in the package model:
		--terminal_cursor := get_terminal (package_model_cursor, terminal_name);
		--if terminal_cursor = pac_terminals.no_element then
			--terminal_not_found (terminal_name);
		--end if;

		---- Get the assembly technology of the terminal (SMT or THT):
		--terminal_technology := element (terminal_cursor).technology;

		---- Get x/y of the terminal as given by the package model.
		---- This position is relative to the origin of the package model:
		--terminal_position := to_vector (pac_terminals.element (terminal_cursor).position.place);
		
		---- Get the rotation of the terminal (about its center) as given by the package model:
		--terminal_rotation := to_angle (pac_terminals.element (terminal_cursor).position.rotation);

		---- Add to the terminal rotation the rotation of the package:
		--terminal_rotation := terminal_rotation + to_angle (get_rotation (package_position));

		
		---- In the board: If the package has been flipped (to any side) by the operator
		---- then the terminal must be flipped also.
		---- If the package has not been flipped, then we assume the face of the terminal 
		---- is the same as the face of the package.
		--if element (device_cursor).flipped = YES then

			--case terminal_technology is
				--when SMT =>
					--if element (terminal_cursor).face = TOP then
						--terminal_position_face := BOTTOM;
					--else
						--terminal_position_face := TOP;
					--end if;

				--when THT => 
					---- If package flipped, then the face of the THT
					---- terminal is bottom. If package not flipped, then default TOP applies:
					--terminal_position_face := BOTTOM;
			--end case;

			
			---- mirror terminal position alog Y axis (swap right x with left x)
			--mirror (terminal_position, Y);

			---- Rotate the terminal position (x/y) by the rotation of the package:
			--rotate_by (terminal_position, - terminal_rotation);
			
		--else -- not flipped
			--terminal_position_face := get_face (package_position);

			---- Rotate the terminal position (x/y) by the rotation of the package:
			--rotate_by (terminal_position, terminal_rotation);
		--end if;


		---- Move the terminal position by the position of the package:
		--move_by (terminal_position, to_offset (package_position.place));

		--return (
			--technology	=> terminal_technology,
			--place		=> terminal_position,
			--rotation	=> terminal_rotation,	   
			--face		=> terminal_position_face);
		
	--end get_terminal_position;



	
	function get_terminal_positions (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in et_nets.pac_nets.cursor;
		observe_techno	: in boolean := false;
		technology		: in type_assembly_technology := assembly_technology_default;
		log_threshold	: in type_log_level)
		return pac_geometry_brd.pac_vectors.list
	is
		use pac_geometry_brd;
		use pac_vectors;
		result : pac_vectors.list;

		use et_nets;
		use et_net_ports;
		use pac_device_ports;
		ports : et_net_ports.type_ports;

		use et_device_model;
		port_properties : type_port_properties_access;

		
		procedure query_device (d : in pac_device_ports.cursor) is
			port : type_device_port renames element (d);
			
			-- CS use rename
			device_cursor : pac_devices_electrical.cursor;
			terminal_position : type_vector;
			terminal_cursor : et_terminals.pac_terminals.cursor;
		begin
			device_cursor := get_electrical_device (module_cursor, element (d).device_name);

			-- Only real devices have terminals. Virtual devices are ignored here:
			if is_real (device_cursor) then
				log (text => to_string (port), level => log_threshold + 2);
				
				port_properties := get_port_properties (
					module_cursor	=> module_cursor,
					device_name		=> element (d).device_name,
					unit_name		=> element (d).unit_name,
					port_name		=> element (d).port_name);

				-- port_properties.terminal -- 14, H6

				-- If technology is to be oberved then the
				-- technology of the candidate terminal must match the given technology:
				if observe_techno then
					terminal_cursor := get_terminal (device_cursor, element (d).unit_name, element (d).port_name);
					
					if get_technology (terminal_cursor) = technology then

						-- Get for the candidate port the position of the associated terminal:
						terminal_position := 
							get_terminal_position (module_cursor, device_cursor, port_properties.terminal).place;
						
						-- Add the terminal position to the result:
						append (result, terminal_position);
					end if;

				else
					-- Get for the candidate port the position of the associated terminal
					-- regardless of the technology:
					terminal_position := 
						get_terminal_position (module_cursor, device_cursor, port_properties.terminal).place;
					
					-- Add the terminal position to the result:
					append (result, terminal_position);
				end if;
			end if;
		end query_device;

		
		use pac_submodule_ports;
		procedure query_submodule (s : in pac_submodule_ports.cursor) is
		begin
			-- CS
			
			-- element (s).module_name
			-- element (s).port_name  -> position in brd
			null;
		end query_submodule;

		
		use et_netlists;
		use pac_netchanger_ports;
		procedure query_netchanger (n : in pac_netchanger_ports.cursor) is
		begin
			null;

			-- CS
			
			-- element (n).index
			-- element (n).port  -> position in brd
		end query_netchanger;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " get terminal positions",
			level => log_threshold);

		log_indentation_up;

		-- Get the ports of devices, netchangers and submodules that are connected
		-- with the given net. Assume default assembly variant:
		ports := get_ports (
				net		=> net_cursor,
				variant	=> et_assembly_variants.pac_assembly_variants.no_element);


		
		log (text => "devices " & natural'image (get_port_count_devices (ports)),
			 level => log_threshold + 1);
		log_indentation_up;
		iterate (ports.devices, query_device'access);
		log_indentation_down;
		
		
		log (text => "submodules " & natural'image (get_port_count_submodules (ports)),
			 level => log_threshold + 1);
		log_indentation_up;		
		iterate (ports.submodules, query_submodule'access);
		log_indentation_down;

		
		log (text => "netchangers " & natural'image (get_port_count_netchangers (ports)),
			 level => log_threshold + 1);
		log_indentation_up;
		iterate (ports.netchangers, query_netchanger'access);
		log_indentation_down;
		

		log_indentation_down;
		
		return result;

		-- exception
		-- 	when event: others =>
		-- 		log (text => ada.exceptions.exception_information (event), console => true);
				--log (text => ada.exceptions.exception_information (event));
		
	end get_terminal_positions;
	








	procedure terminal_not_found (
		terminal_name : in pac_terminal_name.bounded_string) 
	is 
		use et_terminals;
	begin
		log (ERROR,	"terminal " & enclose_in_quotes (to_string (terminal_name)) & " not found !",
			 console => true);
		raise constraint_error;
	end terminal_not_found;



	

	function get_terminal_position (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_electrical.cursor; -- IC45
		terminal_name	: in pac_terminal_name.bounded_string) -- H7, 14
		return type_terminal_position_fine
	is
		-- This is the position of the package as it is in the layout:
		package_position : et_board_coordinates.type_package_position; -- incl. angle and face

		use pac_geometry_brd;
		terminal_position : type_vector; -- x/y
		terminal_rotation : type_angle;
		terminal_position_face : type_face := TOP; -- top/bottom

		model : pac_package_model_file.bounded_string; -- libraries/packages/smd/SOT23.pac
		package_model_cursor : pac_package_models.cursor;

		use pac_terminals;
		-- This cursor points to the terminal in the package model:
		terminal_cursor : pac_terminals.cursor;
		
		terminal_technology : type_assembly_technology;
		
	begin
		-- Get the package model of the given device:
		model := get_package_model_name (device_cursor);

		-- Get the position of the package as it is in the layout:
		package_position := pac_devices_electrical.element (device_cursor).position;
		
		-- Set the cursor to package model:
		package_model_cursor := get_package_model (model);

		-- Locate the desired terminal in the package model:
		terminal_cursor := get_terminal (package_model_cursor, terminal_name);
		if terminal_cursor = pac_terminals.no_element then
			terminal_not_found (terminal_name);
		end if;

		-- Get the assembly technology of the terminal (SMT or THT):
		terminal_technology := element (terminal_cursor).technology;

		-- Get x/y of the terminal as given by the package model.
		-- This position is relative to the origin of the package model:
		terminal_position := to_vector (pac_terminals.element (terminal_cursor).position.place);
		
		-- Get the rotation of the terminal (about its center) as given by the package model:
		terminal_rotation := to_angle (pac_terminals.element (terminal_cursor).position.rotation);

		-- Add to the terminal rotation the rotation of the package:
		terminal_rotation := terminal_rotation + to_angle (get_rotation (package_position));

		
		-- In the board: If the package has been flipped by the operator
		-- then the terminal must be flipped also. In case of a THT terminal,
		-- flipping the terminal has no effect, since for THT there is no "face"-property:
		case get_face (device_cursor) is
			when BOTTOM =>

				case terminal_technology is
					when SMT =>
						if element (terminal_cursor).face = TOP then
							terminal_position_face := BOTTOM;
						else
							terminal_position_face := TOP;
						end if;

					when THT => 
						terminal_position_face := BOTTOM;
				end case;

				
				-- mirror terminal position alog Y axis (swap right x with left x)
				mirror (terminal_position, MIRROR_ALONG_Y_AXIS);

				-- Rotate the terminal position (x/y) by the rotation of the package:
				rotate_by (terminal_position, - terminal_rotation);

				
			when TOP =>

				case terminal_technology is
					when SMT =>
						terminal_position_face := element (terminal_cursor).face;

					when THT => 
						terminal_position_face := TOP;
				end case;

				
				-- Rotate the terminal position (x/y) by the rotation of the package:
				rotate_by (terminal_position, terminal_rotation);
		end case;


		-- Move the terminal position by the position of the package:
		move_by (terminal_position, to_offset (package_position.place));

		return (
			technology	=> terminal_technology,
			place		=> terminal_position,
			rotation	=> terminal_rotation,	   
			face		=> terminal_position_face);
		
	end get_terminal_position;


	



	



	function get_unconnected_terminals (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_electrical.cursor;
		log_threshold	: in type_log_level)
		return pac_terminals.map
	is
		use pac_terminals;

		-- To be returned:
		all_terminals : pac_terminals.map;

		-- Here we will store the terminals of the given 
		-- device which are connected with nets:
		connected_terminals : pac_terminal_names.list;

		

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			use et_nets;

			procedure query_net (net_cursor : in pac_nets.cursor) is
				-- Get the ports of all devices connected with the given net.
				-- Since this query is about the default assembly variant,
				-- we do not pass a specific assembly variant here.
				use et_net_ports;
				ports : constant type_ports := get_ports (net_cursor);

				use pac_device_ports;

				
				procedure query_device_port (c : in pac_device_ports.cursor) is
					port : type_device_port renames element (c);
					-- Now port contains the device name, unit name and port name.
					
					-- Get the cursor to the device in the schematic:
					device_cursor_candidate : constant pac_devices_electrical.cursor := 
						get_electrical_device (module_cursor, port.device_name);

						
					procedure query_terminal is
						-- Get the cursor to the physical terminal (in the package model)
						-- that is linked with the port:
						terminal_cursor : constant pac_terminals.cursor := 
							get_terminal (device_cursor_candidate, port.unit_name, port.port_name);

						-- Get the terminal name (like 3 or H5):
						terminal_name : constant pac_terminal_name.bounded_string := 
							key (terminal_cursor);
					begin
						-- Store the terminal name in list connected_terminals:
						connected_terminals.append (terminal_name);
					end query_terminal;
					
					
				begin
					if is_real (device_cursor_candidate) then
						log (text => "device " & get_device_name (device_cursor_candidate),
							level => log_threshold + 3);
							
						if key (device_cursor_candidate) = key (get_unconnected_terminals.device_cursor) then
						-- CS compare cursors directly ?
							log_indentation_up;
							query_terminal;
							log_indentation_down;
						end if;
					end if;
				end query_device_port;

				
			begin
				log (text => "net " & get_net_name (net_cursor),
					level => log_threshold + 2);
					
				log_indentation_up;
				
				-- In variable "ports" we are interested in 
				-- selector "devices" exclusively.
				-- Submodule ports and netchangers are just virtual devices
				-- that connect two conductor tracks. They can therefore be ignored:
				ports.devices.iterate (query_device_port'access);
				
				log_indentation_down;
			end query_net;
		
		
		begin
			log (text => "query nets", level => log_threshold + 1);
			log_indentation_up;
			module.nets.iterate (query_net'access);
			log_indentation_down;
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " get_unconnected_terminals of"
			& " device " & get_device_name (device_cursor),
			level => log_threshold);

		log_indentation_up;
	
		--put_line ("device " & to_string (key (device_cursor)));
		--put_line ("all " & count_type'image (all_terminals.length));

		-- If the given device is virtual, then there is
		-- nothing to do and an empty list will be returned:
		if is_real (device_cursor) then
			
			-- Get all terminals of the given device (according 
			-- to its package variant).
			-- Later the connected terminals will be removed 
			-- from this list:
			log (text => "get all terminals", level => log_threshold + 1);
			all_terminals := get_all_terminals (device_cursor);

			-- Iterate through the nets:
			query_element (module_cursor, query_module'access);

			--put_line ("connected " & count_type'image (connected_terminals.length));
			
			-- Remove the connected_terminals from all_terminals
			-- so that only the unconneced terminals are left:
			log (text => "remove connected terminals", level => log_threshold + 1);
			remove_terminals (all_terminals, connected_terminals);
		end if;

		
		log_indentation_down;
		
		return all_terminals;
	end get_unconnected_terminals;

	
	
	
end et_board_ops.devices;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
