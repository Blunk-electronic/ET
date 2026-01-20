------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     SCHEMATIC OPERATIONS / DEVICE                        --
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
-- To Do:
-- - clean up, rework
--

with ada.text_io;				use ada.text_io;
with ada.strings;				use ada.strings;
with ada.strings.unbounded;

with et_string_processing;		use et_string_processing;
with et_module;					use et_module;

with et_sheets;					use et_sheets;
with et_schematic_coordinates;	use et_schematic_coordinates;
with et_schematic_ops.units;	use et_schematic_ops.units;
with et_schematic_ops.groups;
with et_exceptions;				use et_exceptions;

with et_unit_name;				use et_unit_name;
with et_units;					use et_units;

with et_device_category;				use et_device_category;
with et_devices_electrical;				use et_devices_electrical;
with et_devices_electrical.packages;	use et_devices_electrical.packages;
with et_device_library.packages;
with et_devices_electrical.units;	use et_devices_electrical.units;
with et_devices_non_electrical;		use et_devices_non_electrical;

with et_board_ops.devices;			use et_board_ops.devices;
with et_conventions;				use et_conventions;


package body et_schematic_ops_device is


	use pac_devices_electrical;
	
	
-- 	
-- 	procedure device_not_found (name : in type_device_name) is begin
-- 		raise semantic_error_1 
-- 			with "ERROR: Device " & to_string (name) & " not found !";
-- 	end device_not_found;
-- 
-- 	
-- 	procedure device_already_exists (name : in type_device_name) is begin
-- 		raise semantic_error_1
-- 			with "ERROR: Device " & to_string (name) & " already exists !";
-- 	end device_already_exists;
-- 
-- 	


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

			procedure query_units (device_cursor : in pac_devices_electrical.cursor) is
				use pac_units;
				device_name : type_device_name := pac_devices_electrical.key (device_cursor); -- R1

				
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
					container	=> pac_devices_electrical.element (device_cursor).units,
					process		=> sort'access);

				log_indentation_down;
			end query_units;
			
			
		begin -- query_devices
			pac_devices_electrical.iterate (
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







	function electrical_device_exists (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name)
		return boolean 
	is
		device_found : boolean := false; -- to be returned
		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is begin
			if contains (module.devices, device) then
				device_found := true;
			end if;
		end query_devices;
		
	begin
		pac_generic_modules.query_element (
			position	=> module,
			process		=> query_devices'access);

		return device_found;
	end electrical_device_exists;



	

	
	function get_electrical_device (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_devices_electrical.cursor 
	is
		result : pac_devices_electrical.cursor;
		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is begin
			result := find (module.devices, device);
		end;

	begin
		pac_generic_modules.query_element (
			position	=> module,
			process		=> query_devices'access);

		return result;
	end get_electrical_device;




	
	
	function get_device_model (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_device_models.cursor
	is
		cursor_sch : pac_devices_electrical.cursor;
	begin
		-- Locate the device in the module:
		cursor_sch := get_electrical_device (module, device);

		-- If the device exists in the module, then
		-- locate the device model in the library.
		-- Otherwise return no_element:
		if has_element (cursor_sch) then
			return get_device_model (cursor_sch);
		else
			return pac_device_models.no_element;
		end if;
	end get_device_model;







	
	function get_device_model (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_device_model_file.bounded_string
	is 
		cursor : pac_devices_electrical.cursor;
	begin
		cursor := get_electrical_device (module, device);
		return get_device_model_file (cursor);
	end get_device_model;




	

-- ASSEMBLY VARIANT:

	



	

	















	

-- SHOW DEVICE:

	procedure show_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name;
		all_units		: in boolean;
		unit_name		: in pac_unit_name.bounded_string := unit_name_default;
		error			: out boolean;
		log_warning		: in boolean := true;
		log_threshold	: in type_log_level)
	is
		device_cursor_sch : pac_devices_electrical.cursor;
		

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is


			procedure query_device (
				device_name	: in type_device_name;
				device		: in out type_device_electrical) 
			is begin
				-- Independend on the search mode, set the whole device as selected.
				-- This is relevant for highlighting the package in the board editor. 
				-- If the device is virtual, then this has no meaning because virtual
				-- devices do not appear in the board drawing:
				set_selected (device);

				-- Now select the units:
				if all_units then
					-- Set all units as selected:

					select_unit (
						device		=> device, 
						all_units	=> true, 
						unit_name	=> unit_name_default); -- don't care

				else
					-- Set the given unit as selected:
					select_unit (
						device		=> device, 
						all_units	=> false, 
						unit_name	=> unit_name);

				end if;
			end query_device;
		
			
		begin
			module.devices.update_element (device_cursor_sch, query_device'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			 & " show electrical device " & to_string (device_name),
			level => log_threshold);

		error := false;
		
		log_indentation_up;
		
		-- Deselect all objects of previous show operations
		-- so that nothing is highlighted anymore:
		et_schematic_ops.groups.reset_objects (module_cursor, log_threshold + 1);
		
		-- Locate the targeted device in the given module.
		-- If the device exists, then proceed with further actions.
		-- Otherwise abort this procedure with a warning:
		device_cursor_sch := get_electrical_device (module_cursor, device_name);
			
		if has_element (device_cursor_sch) then -- device exists in schematic			
			generic_modules.update_element (module_cursor, query_module'access);
		else
			if log_warning then
				log (WARNING, " Device " & to_string (device_name) & " not found !");
			end if;
			
			error := true;
		end if;

		log_indentation_down;
	end show_device;

	
	


	
	function get_device_properties (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name;
		level			: in type_properties_level;
		all_units		: in boolean := true;
		unit_name		: in pac_unit_name.bounded_string := unit_name_default;
		linebreaks		: in boolean := false;
		error			: out boolean;
		log_threshold	: in type_log_level)
		return string
	is
		device_cursor_sch : pac_devices_electrical.cursor;

		use ada.strings.unbounded;
		result : unbounded_string := to_unbounded_string ("");


		use pac_units;
		use pac_unit_name;
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is


			procedure query_device (
				device_name	: in type_device_name;
				device		: in type_device_electrical) 
			is 
				unit_cursor : pac_units.cursor;
			begin
				-- Get the cursor to the targeted unit:
				unit_cursor := locate_unit (device, unit_name);

				-- If the targeted unit exists then get its properties.
				-- Otherwise set the error-flag and return an empty string:
				if has_element (unit_cursor) then
					-- Get the properties of the targeted device
					-- and the targeted unit:
					result := to_unbounded_string (get_properties (
						device_cursor	=> device_cursor_sch,
						level			=> level,
						linebreaks		=> linebreaks,
						all_units		=> false,
						unit_cursor		=> unit_cursor));

				else
					log (WARNING, " Unit " & to_string (unit_name) & " not found !");
					error := true;
				end if;
			end query_device;
		
			
		begin
			query_element (device_cursor_sch, query_device'access);
		end query_module;

		
	begin
		error := false;
		
		log (text => "module " & to_string (module_cursor) 
			 & " get properties of electrical device " & to_string (device_name)
			 & " linebreaks " & boolean'image (linebreaks)
			 & " inquiry level " & to_string (level),
			level => log_threshold);

		
		if all_units then
			log (text => "whole device -> all units",
				 level => log_threshold);

		else
			log (text => "unit " & to_string (unit_name),
				 level => log_threshold);
		end if;


				
		log_indentation_up;
		
		-- Locate the targeted device in the given module.
		-- If the device exists, then proceed with further actions.
		-- Otherwise abort this function, set the error flag and return
		-- an empty string:
		device_cursor_sch := get_electrical_device (module_cursor, device_name);
			
		if has_element (device_cursor_sch) then -- device exists in schematic

			-- If all units of the device are enquired for,
			-- then the cursor to the device is sufficient
			-- to query properties:
			if all_units then
				result := to_unbounded_string (get_properties (
					device_cursor	=> device_cursor_sch,
					linebreaks		=> linebreaks,											  
					level			=> level));
			else
				-- If a dedicated unit is enquired for, then
				-- the cursor to that unit must be set:
				query_element (module_cursor, query_module'access);
			end if;
				
		else
			log (WARNING, " Device " & to_string (device_name) & " not found !");
			error := true;			
		end if;

		log_indentation_down;

		return to_string (result);
	end get_device_properties;

	

	

	
-- VALUE, PURPOSE, PARTCODE:
	

	procedure set_value (
		module_cursor		: in pac_generic_modules.cursor;
		device_name			: in type_device_name; -- R2
		value				: in pac_device_value.bounded_string; -- 470R
		log_threshold		: in type_log_level) 
	is		
		device_cursor_sch : pac_devices_electrical.cursor;
		

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is


			procedure set_value (
				device_name	: in type_device_name;
				device		: in out type_device_electrical) 
			is begin
				-- CS log value old and value new
				device.value := value;
			end;

			
		begin
			-- Only real devices have a value. 
			-- For virtual devices is issue a warning:
			if is_real (device_cursor_sch) then

				-- Check value regarding the device category:
				if et_conventions.value_valid (value, get_prefix (device_name)) then 
				
					update_element (
						container	=> module.devices,
						position	=> device_cursor_sch,
						process		=> set_value'access);

				else
					log (WARNING, "Value " & enclose_in_quotes (to_string (value)) 
						 & " invalid for this kind of device !");
					-- CS: ERROR instead ?, exception ?
					-- CS more details ?
						
				end if;

			else -- virtual device
		
				log (WARNING, " Device " & to_string (device_name) 
					& " is virtual and has no value !");
			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			 & " set " & to_string (device_name) 
			 & " value to " & to_string (value),
			level => log_threshold);

		log_indentation_up;
		
		-- Locate the targeted device in the given module.
		-- If the device exists, then proceed with further actions.
		-- Otherwise abort this procedure with a warning:
		device_cursor_sch := get_electrical_device (module_cursor, device_name);
			
		if has_element (device_cursor_sch) then -- device exists in schematic
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> query_module'access);

		else
			log (WARNING, " Device " & to_string (device_name) & " not found !");
		end if;

		log_indentation_down;
	end set_value;



	
	

	procedure set_purpose (
		module_cursor		: in pac_generic_modules.cursor;
		device_name			: in type_device_name; -- R2
		purpose				: in pac_device_purpose.bounded_string; -- brightness_control
		log_threshold		: in type_log_level) 
	is
		device_cursor_sch : pac_devices_electrical.cursor;
		

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			procedure set_purpose (
				device_name	: in type_device_name;
				device		: in out type_device_electrical) 
			is begin
				device.purpose := purpose;
				-- CS log purpose before and after
			end;

			
		begin
			-- Only real devices have a purpose. 
			-- Issue a warning if targeted device is virtual.
			if is_real (device_cursor_sch) then

				update_element (
					container	=> module.devices,
					position	=> device_cursor_sch,
					process		=> set_purpose'access);

			else
				log (WARNING, "device " & to_string (device_name) 
					& " is virtual and has no practical purpose !");
			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			 & " set " & to_string (device_name) & " purpose to " 
			 & enclose_in_quotes (to_string (purpose)),
			level => log_threshold);

		log_indentation_up;
		
		-- Locate the targeted device in the given module.
		-- If the device exists, then proceed with further actions.
		-- Otherwise abort this procedure with a warning:
		device_cursor_sch := get_electrical_device (module_cursor, device_name);
			
		if has_element (device_cursor_sch) then -- device exists in schematic
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> query_module'access);

		else
			log (WARNING, " Device " & to_string (device_name) & " not found !");
		end if;

		log_indentation_down;		
	end set_purpose;




	
	
	
	procedure set_partcode (
		module_cursor		: in pac_generic_modules.cursor;
		device_name			: in type_device_name; -- R2
		partcode			: in pac_device_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
		log_threshold		: in type_log_level) 
	is
		device_cursor_sch : pac_devices_electrical.cursor;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			procedure set_partcode (
				device_name	: in type_device_name;
				device		: in out type_device_electrical) 
			is begin
				device.partcode := partcode;
				-- CS log old and new partcode
			end;
			
			
		begin
			-- Only real devices have a purpose. 
			-- Issue warning if targeted device is virtual.
			if is_real (device_cursor_sch) then

				update_element (
					container	=> module.devices,
					position	=> device_cursor_sch,
					process		=> set_partcode'access);

			else
				log (WARNING, "Device " & to_string (device_name) 
					& " is virtual and has no partcode !");
			end if;
		end query_module;

		
	begin -- set_partcode
		log (text => "module " & to_string (module_cursor) 
			 & " set " & to_string (device_name) 
			 & " partcode to " & enclose_in_quotes (to_string (partcode)),
			level => log_threshold);

		log_indentation_up;
		
		-- Locate the targeted device in the given module.
		-- If the device exists, then proceed with further actions.
		-- Otherwise abort this procedure with a warning:
		device_cursor_sch := get_electrical_device (module_cursor, device_name);
			
		if has_element (device_cursor_sch) then -- device exists in schematic
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> query_module'access);

		else
			log (WARNING, " Device " & to_string (device_name) & " not found !");
		end if;

		log_indentation_down;
	end set_partcode;



	



-- PACKAGE VARIANT:


	function get_available_package_variants (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_package_variants.map
	is
		use et_device_library.packages;
		cursor_lib : pac_device_models.cursor;	
	begin
		cursor_lib := get_device_model (module, device);
		return get_available_variants (cursor_lib);
	end get_available_package_variants;



	
	
	function get_package_variant (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_package_variant_name.bounded_string -- D, N
	is
		cursor_sch : pac_devices_electrical.cursor;
	begin
		cursor_sch := get_electrical_device (module, device);
		
		return get_package_variant (cursor_sch);
	end get_package_variant;




	
	
	procedure set_package_variant (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- R2
		variant			: in pac_package_variant_name.bounded_string; -- N, D
		log_threshold	: in type_log_level)
	is
		use pac_generic_modules;
		device_cursor_sch : pac_devices_electrical.cursor;

		use pac_package_variant_name;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			procedure query_device (
				device_name	: in type_device_name;
				device		: in out type_device_electrical)
			is 
				use et_device_library.packages;
				cursor_lib : pac_device_models.cursor;
			begin
				-- The device must be real:
				if is_real (device) then
					cursor_lib := get_device_model (device);

					-- If the requested package variant is available
					-- then assign it here. Otherwise issue a warning:
					if is_variant_available (cursor_lib, variant) then
						device.variant := variant;
					else
						log (WARNING, "Package variant " & to_string (variant) 
							& " is not defined in device model !"); 
							-- CS output file name ?
					end if;
				else
					log (WARNING, "The requested device is virtual and has no package !");
				end if;
			end query_device;

				
		begin
			update_element (
				container	=> module.devices,
				position	=> device_cursor_sch,
				process		=> query_device'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " set package variant of " & to_string (device_name)
			& " to " & to_string (variant),
			level => log_threshold);

		log_indentation_up;
		
		-- Locate the targeted device in the given module.
		-- If the device exists, then proceed with further actions.
		-- Otherwise abort this procedure with a warning:
		device_cursor_sch := get_electrical_device (module_cursor, device_name);
			
		if has_element (device_cursor_sch) then -- device exists in schematic
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> query_module'access);

		else
			log (WARNING, " Device " & to_string (device_name) & " not found !");
		end if;

		log_indentation_down;		
	end set_package_variant;






	function get_electrical_devices_by_prefix (
		module_cursor	: in pac_generic_modules.cursor;
		prefix			: in pac_device_prefix.bounded_string; -- C
		log_threshold	: in type_log_level)
		return pac_devices_electrical.map
	is
		result : pac_devices_electrical.map;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is

			procedure query_device (c : in pac_devices_electrical.cursor) is
				use pac_device_prefix;
				device	: type_device_electrical renames element (c);
				name 	: constant type_device_name := key (c); -- IC45
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
			module.devices.iterate (query_device'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			& " get electrical devices with prefix " & to_string (prefix),
			level => log_threshold);

		log_indentation_up;		
		query_element (module_cursor, query_module'access);

		log (text => "device count " & get_count (result), level => log_threshold);
		log_indentation_down;

		return result;
	end get_electrical_devices_by_prefix;






	
	function get_next_available_device_name (
		module_cursor	: in pac_generic_modules.cursor;
		prefix			: in pac_device_prefix.bounded_string;
		log_threshold	: in type_log_level)
		return type_device_name
	is
		next_name : type_device_name; -- to be returned


		procedure search_2 is
			use et_board_ops.devices;
			
			devices_electrical : pac_devices_electrical.map;
			devices_non_electrical : pac_devices_non_electrical.map;

			names_electrical : pac_device_names.set;
			names_non_electrical : pac_device_names.set;
			names_all : pac_device_names.set;
			
		begin
			-- Get all non-electrical devices having the given prefix:
			devices_electrical := get_electrical_devices_by_prefix (
				module_cursor, prefix, log_threshold + 1);

			names_electrical := get_device_names (devices_electrical);
			
			devices_non_electrical := get_non_electrical_devices_by_prefix (
				module_cursor, prefix, log_threshold + 1);

			names_non_electrical := get_device_names (devices_non_electrical);

			names_all := merge_device_names (names_electrical, names_non_electrical);

			next_name := get_first_available_name (names_all, prefix, log_threshold + 1);

			log (text => "proposed device name: " & to_string (next_name),
				level => log_threshold + 2);

		end search_2;
			
	
	begin
		log (text => "module " & to_string (module_cursor) 
			 & " search next available device name with prefix " 
			 & to_string (prefix),
			level => log_threshold);

		log_indentation_up;
		search_2;
		log_indentation_down;
		
		return next_name;
	end get_next_available_device_name;



		



	
	procedure add_electrical_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_model	: in pac_device_model_file.bounded_string;
		variant			: in pac_package_variant_name.bounded_string;
		destination		: in type_object_position;
		log_threshold	: in type_log_level) is separate;


	
	

	procedure copy_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		destination		: in type_object_position; -- sheet/x/y/rotation
		log_threshold	: in type_log_level) is separate;





	




	
	-- Renumbers devices according to the sheet number.
	procedure renumber_devices (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		step_width		: in type_name_index;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_device_category;
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
			cat : in type_device_category) 
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

							null;
							-- CS
							-- if rename_device (
							-- 	module_cursor		=> module_cursor,
							-- 	device_name_before	=> name_before, -- R1
							-- 	device_name_after	=> name_after, -- R407
							-- 	log_threshold		=> log_threshold + 2) then
       -- 
							-- 	-- Mark all units of the device as done:
							-- 	mark_units_done;
							-- else
							-- 	result := false;
							-- end if;
							
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
		for cat in type_device_category'pos (type_device_category'first) .. 
			type_device_category'pos (type_device_category'last) loop

			case type_device_category'val (cat) is
				when UNKNOWN => null;
				
				when others =>

					log (text => "category" & to_string (type_device_category'val (cat)),
						 level => log_threshold + 1);

					log_indentation_up;
					if renumber (type_device_category'val (cat)) = false then
						-- first iteration failed. start a second:
						
						log (text => "another iteration required", level => log_threshold + 2);
						log_indentation_up;
						
						if renumber (type_device_category'val (cat)) = false then
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

	
	

end et_schematic_ops_device;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
