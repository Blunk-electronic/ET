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
with et_device_library;				use et_device_library;
with et_schematic_ops;				use et_schematic_ops;
with et_schematic_ops.units;		use et_schematic_ops.units;
with et_schematic_ops.groups;

with et_net_ports;
with et_net_segment;
with et_submodules;
with et_package_read;
with et_package_write;

with et_contour_to_polygon;			use et_contour_to_polygon;

with et_board_ops.groups;
with et_board_ops.ratsnest;			use et_board_ops.ratsnest;
with et_mirroring;					use et_mirroring;


package body et_board_ops.devices is

	use pac_devices_sch;
	use pac_devices_non_electrical;
	use pac_nets;


	
	function get_devices (
		module			: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_devices_sch.map
	is
		use pac_devices_sch;
		result : pac_devices_sch.map;

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor := module.devices.first;			
		begin
			while device_cursor /= pac_devices_sch.no_element loop

				if is_real (device_cursor) then -- ignore virtual devices (like GND symbols)
					
					log (text => "probing device " & to_string (key (device_cursor)),
						level => log_threshold + 1);
					log_indentation_up;
						
					if in_catch_zone (
						zone	=> catch_zone,
						point	=> get_position (device_cursor))
					then
						log_indentation_up;

						log (text => "in catch zone", level => log_threshold + 1);
						result.insert (key (device_cursor), element (device_cursor));
								
						log_indentation_down;
					end if;
					
					log_indentation_down;
				end if;
				
				next (device_cursor);

			end loop;
		end query_devices;

		
	begin
		log (text => "looking up devices in" 
			 & to_string (catch_zone),
			 level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module,
			process		=> query_devices'access);

		log_indentation_down;
		
		return result;
	end get_devices;




	

	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_sch.cursor;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_device (
				device_name	: in type_device_name;
				device		: in out type_device_sch)
			is begin
				modify_status (device, operation);
			end query_device;


		begin
			if is_real (device_cursor) then -- ignore virtual devices (like GND symbols)
				module.devices.update_element (device_cursor, query_device'access);
			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of "
			& to_string (key (device_cursor))
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;





	

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
				device		: in out type_device_sch)
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
				device		: in out type_device_sch)
			is begin
				log (text => to_string (device_name), level => log_threshold + 1);
				set_proposed (device.status);
				count := count + 1;
			end query_device;

			
			device_cursor : pac_devices_sch.cursor := module.devices.first;
			
		begin
			while device_cursor /= pac_devices_sch.no_element loop

				if is_real (device_cursor) then -- ignore virtual devices (like GND symbols)
					
					-- log (text => "probing device " & to_string (key (device_cursor)),
					-- 	 level => log_threshold + 1);
					-- log_indentation_up;
						
					if in_catch_zone (
						zone	=> catch_zone,
						point	=> get_position (device_cursor)) 
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
		return pac_devices_sch.cursor
	is
		result : pac_devices_sch.cursor;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor := module.devices.first;
		begin
			while device_cursor /= pac_devices_sch.no_element loop
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
			& " looking up the first device /" & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;

		return result;
	end get_first_device;
	




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
			device_cursor : pac_devices_sch.cursor := module.devices.first;
		begin
			while device_cursor /= pac_devices_sch.no_element loop
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

		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_electric		: pac_devices_sch.cursor;
			device_non_electric	: pac_devices_non_electrical.cursor;			

			
			procedure set_position ( -- of an electric device
				device_name	: in type_device_name;
				device		: in out type_device_sch) 
			is begin
				case coordinates is
					when ABSOLUTE =>
						set (point => device.position.place, position => point); 
						-- preserve angle and face

					when RELATIVE =>
						move_by (point => device.position.place, offset => point);
						-- preserve angle and face
						
				end case;
			end set_position;

			
			procedure set_position ( -- of a non-electric device
				device_name	: in type_device_name;
				device		: in out type_device_non_electrical) 
			is begin
				case coordinates is
					when ABSOLUTE =>
						set (point => device.position.place, position => point); 
						-- preserve angle and face

					when RELATIVE =>
						move_by (point => device.position.place, offset => point); 
						-- preserve angle and face
						
				end case;
			end set_position;

			
		begin -- query_devices

			-- Search the device first among the electric devices.
			-- Most likely it will be among them. If not,
			-- search in non-electric devices.
			if contains (module.devices, device_name) then

				device_electric := find (module.devices, device_name);

				-- set new position
				update_element (
					container	=> module.devices,
					position	=> device_electric,
					process		=> set_position'access);

			else
				-- search among non-electric devices:
				if contains (module.devices_non_electric, device_name) then

					device_non_electric := find (module.devices_non_electric, device_name);

					-- set new position
					update_element (
						container	=> module.devices_non_electric,
						position	=> device_non_electric,
						process		=> set_position'access);

				else
					device_not_found (device_name);
				end if;
			end if;
		end query_devices;

		
	begin
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_cursor) &
					" moving device " & to_string (device_name) &
					" to " & to_string (point), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_cursor) &
					" moving device " & to_string (device_name) &
					" by " & to_string (point), level => log_threshold);
		end case;

		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

		update_ratsnest (module_cursor, log_threshold + 1);
	end move_device;



	
	
	procedure rotate_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		coordinates		: in type_coordinates; -- relative/absolute		
		rotation		: in et_board_geometry.type_rotation_model; -- 90
		log_threshold	: in type_log_level) 
	is

		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_electric		: pac_devices_sch.cursor;
			device_non_electric	: pac_devices_non_electrical.cursor;			

			procedure set_rotation ( -- of an electric device
				device_name	: in type_device_name;
				device		: in out type_device_sch) 
			is begin
				case coordinates is
					when ABSOLUTE =>
						set (device.position, rotation); -- preserve x/y and face

					when RELATIVE =>
						rotate_about_itself (position => device.position, offset => rotation); -- preserve x/y and face
				end case;
			end;

			
			procedure set_rotation ( -- of a non-electric device
				device_name	: in type_device_name;
				device		: in out type_device_non_electrical) 
			is begin
				case coordinates is
					when ABSOLUTE =>
						set (device.position, rotation); -- preserve x/y and face

					when RELATIVE =>
						rotate_about_itself (position => device.position, offset => rotation); -- preserve x/y and face
				end case;
			end;

			
		begin -- query_devices

			-- Search the device first among the electric devices.
			-- Most likely it will be among them. If not,
			-- search in non-electric devices.
			if contains (module.devices, device_name) then

				device_electric := find (module.devices, device_name);

				-- set new position
				update_element (
					container	=> module.devices,
					position	=> device_electric,
					process		=> set_rotation'access);

			else
				-- search among non-electric devices:
				if contains (module.devices_non_electric, device_name) then

					device_non_electric := find (module.devices_non_electric, device_name);

					-- set new position
					update_element (
						container	=> module.devices_non_electric,
						position	=> device_non_electric,
						process		=> set_rotation'access);

				else
					device_not_found (device_name);
				end if;
			end if;

		end query_devices;

		
	begin -- rotate_device
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_cursor) &
					" rotating device " & to_string (device_name) &
					" to " & to_string (rotation), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_cursor) &
					" rotating device " & to_string (device_name) &
					" by " & to_string (rotation), level => log_threshold);
		end case;

		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

		update_ratsnest (module_cursor, log_threshold + 1);
	end rotate_device;



	

	
	procedure flip_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		face			: in type_face; -- top/bottom
		log_threshold	: in type_log_level) 
	is

		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_electric		: pac_devices_sch.cursor;
			device_non_electric	: pac_devices_non_electrical.cursor;			

			
			procedure flip ( -- electric device
				device_name	: in type_device_name;
				device		: in out type_device_sch) 
			is				
				-- face_before : constant type_face := get_face (device.position);
			begin
				flip (device.position);
				
-- 				if face_before /= face then
-- 					set_face (position => device.position, face => face); -- preserve x/y and rotation
-- 
-- 					-- toggle the flipped flag
-- 					if device.flipped = NO then
-- 						device.flipped := YES;
-- 					else
-- 						device.flipped := NO;
-- 					end if;
-- 					
-- 				else
-- 					log (WARNING, "package already on " & to_string (face) & " !");
-- 				end if;
			end flip;

			
			procedure flip ( -- non-electric device
				device_name	: in type_device_name;
				device		: in out type_device_non_electrical) 
			is				
				-- face_before : constant type_face := get_face (device.position);
			begin
				flip (device.position);

-- 				if face_before /= face then
-- 					set_face (position => device.position, face => face); -- preserve x/y and rotation
-- 
-- 					-- toggle the flipped flag
-- 					if device.flipped = NO then
-- 						device.flipped := YES;
-- 					else
-- 						device.flipped := NO;
-- 					end if;
-- 	
-- 				else
-- 					log (WARNING, "package already on " & to_string (face) & " !");
-- 				end if;
			end flip;

			
		begin -- query_devices

			-- Search the device first among the electric devices.
			-- Most likely it will be among them. If not,
			-- search in non-electric devices.
			if contains (module.devices, device_name) then

				device_electric := find (module.devices, device_name);

				-- set new position
				update_element (
					container	=> module.devices,
					position	=> device_electric,
					process		=> flip'access);

			else
				-- search among non-electric devices:
				if contains (module.devices_non_electric, device_name) then

					device_non_electric := find (module.devices_non_electric, device_name);

					-- set new position
					update_element (
						container	=> module.devices_non_electric,
						position	=> device_non_electric,
						process		=> flip'access);

				else
					device_not_found (device_name);
				end if;

			end if;
		end query_devices;

		
	begin
		log (text => "module " & to_string (module_cursor) &
			" flipping device " & to_string (device_name) &
			" to " & to_string (face), level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

		update_ratsnest (module_cursor, log_threshold + 1);		
	end flip_device;



	
	
--------------------------------------------------------------------------------------

-- NON-ELECTRICAL DEVICES:

	
	
	function get_devices (
		module			: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_devices_non_electrical.map
	is
		result : pac_devices_non_electrical.map;

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			device_cursor : pac_devices_non_electrical.cursor := module.devices_non_electric.first;
		begin
			while device_cursor /= pac_devices_non_electrical.no_element loop

				log (text => "probing device " & to_string (key (device_cursor)),
					 level => log_threshold + 1);
				log_indentation_up;
					 
				if in_catch_zone (
					zone	=> catch_zone,
					point	=> element (device_cursor).position.place) 
				then
					log_indentation_up;

					log (text => "in zone", level => log_threshold + 1);
					result.insert (key (device_cursor), element (device_cursor));
							
					log_indentation_down;
				end if;
				
				next (device_cursor);

				log_indentation_down;
			end loop;
		end query_devices;

		
	begin
		log (text => "looking up devices in" & to_string (catch_zone),
			 level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module,
			process		=> query_devices'access);

		log_indentation_down;
		
		return result;
	end get_devices;




	

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
	


	
	
	
	
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_non_electrical.cursor;
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
				modify_status (device.status, operation);
			end query_device;


		begin
			module.devices_non_electric.update_element (device_cursor, query_device'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of "
			& to_string (key (device_cursor))
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;



	

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


	
	
	
	procedure next_proposed_non_electrical_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in out pac_devices_non_electrical.cursor;							
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			dc : pac_devices_non_electrical.cursor := device_cursor;

			subtype type_safety_counter is natural range 0 .. natural (module.devices_non_electric.length);
			safety_counter : type_safety_counter := 0;
			
		begin
			-- Advance to the next device after the given device:
			if dc = module.devices_non_electric.last then
				dc := module.devices_non_electric.first;
			else
				next (dc);
			end if;
			
			loop
				-- Exception is raised in case we get stuck here:
				safety_counter := safety_counter + 1;
				
				if is_proposed (dc) then
					device_cursor := dc;
					exit; -- no further probing required
				end if;
				

				if dc = module.devices_non_electric.last then
					dc := module.devices_non_electric.first;
				else
					next (dc);
				end if;
				
			end loop;
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " advancing to next proposed non-electrical device",
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end next_proposed_non_electrical_device;
		





	function get_placeholders (
		package_cursor : in pac_package_models.cursor)
		return et_device_placeholders.packages.type_text_placeholders 
	is
		use et_device_placeholders.packages;
		use pac_package_models;
	begin
		return p : type_text_placeholders do
		
			-- fetch the placeholders of silk screen top and bottom
			p.silkscreen.top := element (package_cursor).silkscreen.top.placeholders;
			p.silkscreen.bottom := element (package_cursor).silkscreen.bottom.placeholders;

			-- fetch the placeholders of assembly documentation top and bottom
			p.assy_doc.top := element (package_cursor).assy_doc.top.placeholders;
		p.assy_doc.bottom := element (package_cursor).assy_doc.bottom.placeholders;
		
		end return;
	end get_placeholders;
	


	


	
	procedure add_device ( -- non-electric !
		module_cursor	: in pac_generic_modules.cursor;
		package_model	: in pac_package_model_file_name.bounded_string; -- ../lbr/packages/fiducial.pac
		position		: in type_package_position; -- x,y,rotation,face
		prefix			: in pac_device_prefix.bounded_string; -- FD
		log_threshold	: in type_log_level) 
	is

		package_cursor_lib : pac_package_models.cursor;

		
		procedure add (						  
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_cursor : pac_devices_non_electrical.cursor;
			inserted : boolean;

			-- build the next available device name:
			next_name : type_device_name := get_next_available_device_name (module_cursor, prefix, CAT_NON_ELECTRICAL);
		begin
			log (text => "adding device " & to_string (next_name), level => log_threshold + 1);
			log_indentation_up;

			-- add the device to the collection of non-electic devices:
			pac_devices_non_electrical.insert (
				container	=> module.devices_non_electric,
				inserted	=> inserted,
				position	=> device_cursor,
				key			=> next_name,
				new_item	=> (
					position			=> position,
					package_model		=> package_model,
					text_placeholders	=> get_placeholders (package_cursor_lib),
					others				=> <>)
				);

			-- check inserted flag
			if not inserted then
				raise constraint_error;
			end if;

			log_indentation_down;
		end add;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			& " add non-electric device " & to_string (package_model) 
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
			process		=> add'access);
		
		log_indentation_down;
	end add_device;

	



	
	procedure delete_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- FD1
		log_threshold	: in type_log_level) 
	is

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			-- Search the device among the non-electric devices.
			if contains (module.devices_non_electric, device_name) then

				delete (module.devices_non_electric, device_name);
			else
				device_not_found (device_name);
			end if;
		end query_devices;

		
	begin
		log (text => "module " & to_string (module_cursor) &
			 " deleting device (non-electric) " & to_string (device_name),
			 level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end delete_device;



	
	
	procedure rename_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name_before	: in type_device_name; -- FD1
		device_name_after	: in type_device_name; -- FD3
		log_threshold		: in type_log_level) 
	is		
		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_before, device_after : pac_devices_non_electrical.cursor;

			inserted : boolean;
		begin
			-- Search the old device among the non-electric devices.
			if contains (module.devices_non_electric, device_name_before) then

				-- locate the device by the old name
				device_before := find (module.devices_non_electric, device_name_before); -- FD1
				
				-- copy elements and properties of the old device to a new one:
				pac_devices_non_electrical.insert (
					container	=> module.devices_non_electric,
					key			=> device_name_after, -- FD3
					new_item	=> element (device_before), -- all elements and properties of FD1
					inserted	=> inserted,
					position	=> device_after);

				if not inserted then
					device_already_exists (device_name_after);
				end if;

				-- check conformity of prefix
				if not et_conventions.prefix_valid (device_name_after) then
					null;
					--device_prefix_invalid (device_after);
				end if;

				-- delete the old device
				delete (module.devices_non_electric, device_before);
				
			else
				device_not_found (device_name_before);
			end if;
		end query_devices;

		
	begin
		log (text => "module " & to_string (module_cursor) &
			 " renaming device (non-electric) " & to_string (device_name_before) & " to " & 
			to_string (device_name_after),
			level => log_threshold);
		
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end rename_device;
	





	
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

		use pac_devices_non_electrical;
		use pac_devices_sch;
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first object / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;

		
		-- SEARCH FOR A ELECTRICAL DEVICE:
		
		-- If a device has been found, then go to the end of this procedure:
		result_electrical := get_first_device (module_cursor, flag, log_threshold + 1);

		if result_electrical.cursor /= pac_devices_sch.no_element then
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
				
		end case;
	end get_first_object;


	

	

	function get_objects (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return pac_objects.list
	is
		use pac_objects;

		-- Here the device objects are collected:
		result : pac_objects.list;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_devices_sch;
			cursor_electrical : pac_devices_sch.cursor;
			
			use pac_devices_non_electrical;
			cursor_non_electrical : pac_devices_non_electrical.cursor;
			

			-- This procedure collects electrical devices
			-- according to the given flag:
			procedure query_electrical_device (
				name	: in type_device_name;
				device	: in type_device_sch) 
			is 

				procedure collect is begin
					result.append ((
						cat					=> CAT_ELECTRICAL_DEVICE,
						electrical_device	=> (cursor => cursor_electrical)));

					log (text => get_device_name (cursor_electrical), level => log_threshold + 2);
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
			end query_electrical_device;

			

			-- This procedure collects non-electrical devices
			-- according to the given flag:
			procedure query_non_electrical_device (
				name	: in type_device_name;
				device	: in type_device_non_electrical) 
			is 

				procedure collect is begin
					result.append ((
						cat						=> CAT_NON_ELECTRICAL_DEVICE,
						non_electrical_device	=> (cursor => cursor_non_electrical)));

					log (text => get_device_name (cursor_non_electrical), level => log_threshold + 2);
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
			end query_non_electrical_device;

			
		begin
			log (text => "electrical devices", level => log_threshold + 1);
			log_indentation_up;

			-- Iterate the electrical devices of the module:
			cursor_electrical := module.devices.first;
			while cursor_electrical /= pac_devices_sch.no_element loop
				query_element (cursor_electrical, query_electrical_device'access);
				next (cursor_electrical);
			end loop;

			log_indentation_down;


			log (text => "non-electrical devices", level => log_threshold + 1);
			log_indentation_up;

			-- Iterate the non-electrical devices of the module:
			cursor_non_electrical := module.devices_non_electric.first;
			while cursor_non_electrical /= pac_devices_non_electrical.no_element loop
				query_element (cursor_non_electrical, query_non_electrical_device'access);
				next (cursor_non_electrical);
			end loop;

			log_indentation_down;			
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
		modify_status (module_cursor, object, operation, log_threshold);
	end modify_status;

	



	procedure reset_status_objects (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is 

	begin
		log (text => "module " & to_string (module_cursor) 
			& " reset objects",
			level => log_threshold);

		log_indentation_up;

		-- Reset electrial devices, units and packages:
		et_schematic_ops.units.reset_status_units (
			module_cursor, log_threshold + 1);

		-- Reset non-electrical devices:
		reset_proposed_non_electrical_devices (
			module_cursor, log_threshold + 1);

		-- CS placeholders of devices
		
		log_indentation_down;
	end reset_status_objects;



	
	


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
					device_name		=> key (object.electrical_device.cursor),
					coordinates		=> absolute,
					point			=> destination,
					log_threshold	=> log_threshold + 1);

				
			when CAT_NON_ELECTRICAL_DEVICE =>

				move_device (
					module_cursor	=> module_cursor,
					device_name		=> key (object.non_electrical_device.cursor),
					coordinates		=> absolute,
					point			=> destination,
					log_threshold	=> log_threshold + 1);

				
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end move_object;
	





	procedure delete_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " deleting object",
			-- CS & to_string (object)
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_ELECTRICAL_DEVICE =>
				null; -- CS
				
			when CAT_NON_ELECTRICAL_DEVICE =>

				delete_device (
					module_cursor	=> module_cursor,
					device_name		=> key (object.non_electrical_device.cursor),
					log_threshold	=> log_threshold + 1);
				
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end delete_object;
	





	procedure flip_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		face			: in type_face; -- top/bottom
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " flipping object",
			-- CS & to_string (object)
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_ELECTRICAL_DEVICE =>

				flip_device (
					module_cursor	=> module_cursor,
					device_name		=> key (object.electrical_device.cursor),
					face			=> face,
					log_threshold	=> log_threshold + 1);

				
			when CAT_NON_ELECTRICAL_DEVICE =>

				flip_device (
					module_cursor	=> module_cursor,
					device_name		=> key (object.non_electrical_device.cursor),
					face			=> face,
					log_threshold	=> log_threshold + 1);

				
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end flip_object;

	


	

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

				show_device (
					module_cursor	=> module_cursor,
					device_name		=> get_device_name (object.electrical_device.cursor),
					all_units		=> true,
					error			=> error,
					log_threshold	=> log_threshold + 1);


			when CAT_NON_ELECTRICAL_DEVICE =>

				show_non_electrical_device (
					module_cursor	=> module_cursor,
					device_name		=> get_device_name (object.non_electrical_device.cursor),
					error			=> error,
					log_threshold	=> log_threshold + 1);

				
			-- when CAT_PLACEHOLDER =>
				-- null; -- CS clear content ? or do nothing ?

								
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
		--device_cursor	: in pac_devices_sch.cursor; -- IC45
		--terminal_name	: in pac_terminal_name.bounded_string) -- H7, 14
		--return type_terminal_position_fine
	--is
		---- This is the position of the package as it is in the layout:
		--package_position : et_board_coordinates.type_package_position; -- incl. angle and face

		--use pac_geometry_brd;
		--terminal_position : type_vector; -- x/y
		--terminal_rotation : type_angle;
		--terminal_position_face : type_face := TOP; -- top/bottom

		--model : pac_package_model_file_name.bounded_string; -- libraries/packages/smd/SOT23.pac
		--package_model_cursor : pac_package_models.cursor;

		--use pac_terminals;
		---- This cursor points to the terminal in the package model:
		--terminal_cursor : pac_terminals.cursor;
		
		--terminal_technology : type_assembly_technology;
		
	--begin
		---- Get the package model of the given device:
		--model := get_package_model (device_cursor);

		---- Get the position of the package as it is in the layout:
		--package_position := pac_devices_sch.element (device_cursor).position;
		
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
			device_cursor : pac_devices_sch.cursor;
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
		terminal_name : in et_terminals.pac_terminal_name.bounded_string) 
	is 
		use et_terminals;
	begin
		log (ERROR,	"terminal " & enclose_in_quotes (to_string (terminal_name)) & " not found !",
			 console => true);
		raise constraint_error;
	end terminal_not_found;



	

	function get_terminal_position (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_sch.cursor; -- IC45
		terminal_name	: in pac_terminal_name.bounded_string) -- H7, 14
		return type_terminal_position_fine
	is
		-- This is the position of the package as it is in the layout:
		package_position : et_board_coordinates.type_package_position; -- incl. angle and face

		use pac_geometry_brd;
		terminal_position : type_vector; -- x/y
		terminal_rotation : type_angle;
		terminal_position_face : type_face := TOP; -- top/bottom

		model : pac_package_model_file_name.bounded_string; -- libraries/packages/smd/SOT23.pac
		package_model_cursor : pac_package_models.cursor;

		use pac_terminals;
		-- This cursor points to the terminal in the package model:
		terminal_cursor : pac_terminals.cursor;
		
		terminal_technology : type_assembly_technology;
		
	begin
		-- Get the package model of the given device:
		model := get_package_model_name (device_cursor);

		-- Get the position of the package as it is in the layout:
		package_position := pac_devices_sch.element (device_cursor).position;
		
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


	




	function to_polygon (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_sch.cursor;
		terminal_cursor	: in pac_terminals.cursor;
		layer_category	: in type_signal_layer_category;
		tolerance		: in type_distance_positive)
		return type_terminal_polygon
	is
		use pac_polygons;
		exists : boolean := false;
		result : type_polygon; -- to be returned

		use pac_contours;
		use pac_terminals;
		
		-- Get the actual terminal as described in the package model:
		terminal : constant et_terminals.type_terminal := element (terminal_cursor);
		
		-- Get the terminal name (like 3 or H5):
		terminal_name : constant pac_terminal_name.bounded_string := key (terminal_cursor);
		
		-- Get the terminal position (incl. rotation and face):
		terminal_position : constant type_terminal_position_fine := 
			get_terminal_position (module_cursor, device_cursor, terminal_name);

		-- The displacement required to move the contour to 
		-- its final position:
		terminal_displacement : constant type_vector_model := to_vector_model (terminal_position.place);

		
		-- intermediate storage place of a contour:
		contour : type_contour;


		-- Converts the contour to a polygon:
		procedure make_polygon is 
			use et_contour_to_polygon;
		begin
			exists := true;
			
			result := to_polygon (
				contour		=> contour,
				tolerance	=> tolerance,
				mode		=> EXPAND, -- CS ?
				debug		=> false);
		end make_polygon;


		-- Mirrors the contour (if terminal is flipped to bottom side) and
		-- rotates the contour:
		procedure mirror_and_rotate is begin
			if terminal_position.face = BOTTOM then
				mirror (contour, MIRROR_ALONG_Y_AXIS);

				-- if on bottom side: rotate CW
				rotate_by (contour, - to_rotation (terminal_position.rotation));
			else
				-- if on top side: rotate CCW
				rotate_by (contour, + to_rotation (terminal_position.rotation));
			end if;
		end mirror_and_rotate;


		-- Moves the contour to the final position and converts it to a polygon.
		-- Optionally, if required by the caller, offsets the polygon edges
		-- by the width of the inner signal layer:
		procedure finalize (do_offset : in boolean := false) is
			use et_board_geometry.pac_polygon_offsetting;
		begin
			move_by (contour, terminal_displacement);
			make_polygon;
			if do_offset then
				offset_polygon (result, type_float_model (terminal.width_inner_layers));
			end if;
		end finalize;
				
		
	begin -- to_polygon

		case terminal.technology is
			when THT => 
				case layer_category is
					when INNER =>								
						case terminal.tht_hole is
							when DRILLED =>
								contour := get_inner_contour (terminal, terminal_position.place);
								make_polygon;										
								
							when MILLED =>
								contour := terminal.millings;
								mirror_and_rotate;										
								finalize (do_offset => true);
								
						end case;
					
					when OUTER_TOP =>
						if terminal_position.face = TOP then
							contour := terminal.pad_shape_tht.top;
						else
							contour := terminal.pad_shape_tht.bottom;
						end if;
						mirror_and_rotate;
						finalize;

					when OUTER_BOTTOM =>
						if terminal_position.face = BOTTOM then
							contour := terminal.pad_shape_tht.top;
						else
							contour := terminal.pad_shape_tht.bottom;
						end if;
						mirror_and_rotate;
						finalize;
				end case;
				

			when SMT =>
				if layer_category = OUTER_TOP and terminal_position.face = TOP then
					contour := terminal.pad_shape_smt;
					rotate_by (contour, to_rotation (terminal_position.rotation));
					finalize;						
					
				elsif layer_category = OUTER_BOTTOM and terminal_position.face = BOTTOM then
					contour := terminal.pad_shape_smt;
					mirror (contour, MIRROR_ALONG_Y_AXIS);
					rotate_by (contour, - to_rotation (terminal_position.rotation));
					finalize;
				end if;
		end case;


		if exists then
			return (
				exists		=> TRUE, 
				polygon		=> result, 
				position	=> terminal_position);
		else
			return (exists => FALSE);
		end if;

	end to_polygon;



	



	function get_unconnected_terminals (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_sch.cursor) -- IC45
		return pac_terminals.map
	is
		use et_nets;
		use pac_terminals;
		use et_schematic_ops;

		-- To be returned:
		all_terminals : pac_terminals.map;

		-- Here we will store the terminals of the given 
		-- device which are connected with nets:
		connected_terminals : pac_terminal_names.list;

		
		procedure query_net (net_cursor : in pac_nets.cursor) is
			-- Get the ports of all devices connected with the given net.
			-- Since this query is about the default assembly variant,
			-- we do not pass a specific assembly variant here.
			use et_net_ports;
			ports : constant type_ports := get_ports (net_cursor);

			use pac_device_ports;

			
			procedure query_device_port (d : in pac_device_ports.cursor) is

				port : type_device_port renames element (d);
				-- Now port contains the device name, unit name and port name.
				
				-- Get the cursor to the device in the schematic:
				device_cursor : constant pac_devices_sch.cursor := 
					get_electrical_device (module_cursor, port.device_name);

				-- Get the cursor to the physical terminal (in the package model)
				-- that is linked with the port:
				terminal_cursor : constant pac_terminals.cursor := 
					get_terminal (device_cursor, port.unit_name, port.port_name);

				-- Get the terminal name (like 3 or H5):
				terminal_name : constant pac_terminal_name.bounded_string := 
					key (terminal_cursor);
				
			begin
				--put_line ("dev " & to_string (key (net_cursor)));
				if key (device_cursor) = key (get_unconnected_terminals.device_cursor) then
				
				-- Store the terminal name in list connected_terminals:
					connected_terminals.append (terminal_name);
				end if;
			end query_device_port;

			
		begin
			--put_line ("net " & to_string (key (net_cursor)));
			
			-- In variable "ports" we are interested in selector "devices" exclusively.
			-- Submodule ports and netchangers are just virtual devices
			-- that connect two conductor tracks. They can therefore be ignored:
			ports.devices.iterate (query_device_port'access);
		end query_net;

		
	begin
		--put_line ("device " & to_string (key (device_cursor)));
		--put_line ("all " & count_type'image (all_terminals.length));

		-- If the given device is virtual, then there is
		-- nothing to do and an empty list will be returned:
		if is_real (device_cursor) then
			
			-- Get all terminals of the given device (according 
			-- to its package variant).
			-- Later the connected terminals will be removed 
			-- from this list:
			all_terminals := get_all_terminals (device_cursor);

			-- Iterate through the nets:
			element (module_cursor).nets.iterate (query_net'access);

			--put_line ("connected " & count_type'image (connected_terminals.length));
			
			-- Remove the connected_terminals from all_terminals
			-- so that only the unconneced terminals are left:
			remove_terminals (all_terminals, connected_terminals);
		end if;

		
		return all_terminals;
	end get_unconnected_terminals;


	

	
end et_board_ops.devices;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
