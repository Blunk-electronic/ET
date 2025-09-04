------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    SCHEMATIC OPERATIONS ON UNITS                         --
--                                                                          --
--                               S p e c                                    --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 


with et_board_ops.ratsnest;					use et_board_ops.ratsnest;
with et_schematic_ops.nets;
with et_net_strands;						use et_net_strands;
with et_port_direction;
with et_device_model;						use et_device_model;
with et_devices_non_electrical;				use et_devices_non_electrical;
with et_numbering;
with et_device_appearance;


package body et_schematic_ops.units is

	use pac_devices_sch;
	use pac_units;
	
	use pac_unit_name;
	use pac_text_schematic;
	


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








	
	
	function device_exists (
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
	end device_exists;



	

	
	function locate_device (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name) -- R2
		return pac_devices_sch.cursor 
	is
		result : pac_devices_sch.cursor;
		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
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
				cursor_lib := get_device_model_cursor (dev.model);

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

		if device_exists (module_cursor, device) then
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
		return get_device_model_cursor (device_model);
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
		if device_exists (module_cursor, device) then
		
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> mount'access);

		else
			device_not_found (device);
		end if;
	end mount_device;




	

	
	procedure unmount_device (
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
		if device_exists (module_cursor, device) then
			
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
		if device_exists (module_cursor, device) then
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> remove'access);

		else
			device_not_found (device);
		end if;
		
	end remove_device;

	


	


	
	function get_position (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC34
		port_name		: in pac_port_name.bounded_string; -- CE
		log_threshold	: in type_log_level)
		return type_object_position 
	is
		port_position : type_object_position; -- to be returned		
		
		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor;
			unit_position : type_object_position;

			
			procedure query_units (
				device_name	: in type_device_name;
				device		: in type_device_sch) 
			is				
				unit_cursor : pac_units.cursor := device.units.first;
				unit_name : pac_unit_name.bounded_string;
				
				use pac_ports;
				ports : pac_ports.map;
				port_cursor : pac_ports.cursor;
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
						port_position := et_schematic_coordinates.to_position (
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
							offset	=> unit_position.place);
						
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

		
	begin
		log (text => "module " & to_string (module_cursor) &
			 " locating device " & to_string (device_name) & 
			 " port " & to_string (port_name) & " ...", level => log_threshold);

		query_element (
			position	=> module_cursor,
			process		=> query_devices'access);
		
		return port_position;
	end get_position;

	
	


	
	procedure move_unit_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		meaning			: in type_placeholder_meaning; -- name, value, purpose
		log_threshold	: in type_log_level)
	is
		use pac_unit_name;

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in out type_device_sch)
			is
				unit_cursor : pac_units.cursor;

				procedure move_placeholder (
					unit_name	: in pac_unit_name.bounded_string;
					unit		: in out type_unit)
				is
					-- In case absolute movement is required, calculate the
					-- new position of the placeholder relative to the unit origin:
					pos_abs : constant type_vector_model :=
						get_distance_relative (unit.position.place, point);
					
				begin -- move_placeholder
					
					-- The given meaning determines the placeholder to be moved:
					case meaning is
						when NAME =>
							case coordinates is
								when ABSOLUTE =>
									--log (text => "pos " & to_string (point));
									unit.placeholders.name.position := pos_abs;

								when RELATIVE =>
									move_by (
										point	=> unit.placeholders.name.position,
										offset	=> point);
							end case;
							
						when VALUE =>
							case coordinates is
								when ABSOLUTE =>
									unit.placeholders.value.position := pos_abs;

								when RELATIVE =>
									move_by (
										point	=> unit.placeholders.value.position,
										offset	=> point);
							end case;
							
						when PURPOSE =>
							case coordinates is
								when ABSOLUTE =>
									unit.placeholders.purpose.position := pos_abs;

								when RELATIVE =>
									move_by (
										point	=> unit.placeholders.purpose.position,
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

		
	begin
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_cursor)
					& " moving " & to_string (device_name) 
					& " unit " & to_string (unit_name) 
					& " placeholder " & enclose_in_quotes (to_string (meaning))
					& " to" & to_string (point),
					level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_cursor)
					& " moving " & to_string (device_name) 
					& " unit " & to_string (unit_name) 
					& " placeholder " & enclose_in_quotes (to_string (meaning))
					& " by" & to_string (point),
					level => log_threshold);
		end case;

		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);
		
	end move_unit_placeholder;





	

	procedure rotate_unit_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		rotation		: in et_text.type_rotation_documentation; -- absolute ! -- 90
		meaning			: in type_placeholder_meaning; -- name, value, purpose		
		log_threshold	: in type_log_level) 
	is

		use et_symbols;
		use pac_unit_name;

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in out type_device_sch) 
			is
				unit_cursor : pac_units.cursor;

				
				procedure rotate_placeholder (
					name	: in pac_unit_name.bounded_string; -- A
					unit	: in out type_unit) 
				is begin
					case meaning is
						when et_device_placeholders.NAME =>
							unit.placeholders.name.rotation := rotation;
							
						when VALUE =>
							unit.placeholders.value.rotation := rotation;
							
						when PURPOSE =>
							unit.placeholders.purpose.rotation := rotation;

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
		
		
	begin
		log (text => "module " & to_string (module_cursor) &
			" rotating " & to_string (device_name) & " unit " &
			to_string (unit_name) & " placeholder" & to_string (meaning) & " to" &
			to_string (rotation), level => log_threshold);
		
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end rotate_unit_placeholder;




	
	
	function locate_unit (
		module_cursor	: in pac_generic_modules.cursor;
		device			: in type_device_name; -- R2
		unit			: in pac_unit_name.bounded_string)
		return pac_units.cursor
	is
		device_cursor : pac_devices_sch.cursor;
		unit_cursor : pac_units.cursor; -- to be returned

		procedure query_units (
			device_name	: in type_device_name; -- R2
			device		: in type_device_sch)
		is begin
			unit_cursor := find (device.units, unit);
		end query_units;
	
	begin -- locate_unit
		device_cursor := locate_device (module_cursor, device);

		-- locate the unit
		query_element (device_cursor, query_units'access);

		return unit_cursor;
	end locate_unit;


	

	

	function is_deployed (
		module_cursor	: in pac_generic_modules.cursor;
		device			: in type_device_name; -- R2
		unit			: in pac_unit_name.bounded_string)
		return boolean
	is
		unit_cursor : pac_units.cursor;
	begin
		unit_cursor := locate_unit (module_cursor, device, unit);

		if unit_cursor = pac_units.no_element then
			return false;
		else
			return true;
		end if;
	end is_deployed;



	
	

	function device_port_exists (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		port_name		: in pac_port_name.bounded_string) -- CE
		return boolean 
	is
		result : boolean := false; -- to be returned. goes true once the target has been found

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			use pac_unit_name;
			device_cursor : pac_devices_sch.cursor;

			
			procedure query_units (
				device_name	: in type_device_name;
				device		: in type_device_sch) 
			is
				unit_cursor : pac_units.cursor := device.units.first;
				use pac_ports;
				ports : pac_ports.map;
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

			
		begin
			if contains (module.devices, device_name) then -- device found

				device_cursor := find (module.devices, device_name);
					
				query_element (
					position	=> device_cursor,
					process		=> query_units'access);

			end if;
		end query_devices;

		
	begin
		query_element (
			position	=> module_cursor,
			process		=> query_devices'access);

		return result;
	end device_port_exists;




	

	function device_unit_port_exists (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string := to_unit_name (""); -- A
		port_name		: in pac_port_name.bounded_string := to_port_name ("")) -- CE
		return boolean 
	is
		result : boolean := false; -- to be returned, goes true once the target has been found
		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) is
			use pac_unit_name;
			device_cursor : pac_devices_sch.cursor;

			use et_symbols;
			
			procedure query_units (
				device_name	: in type_device_name;
				device		: in type_device_sch) is
				use pac_ports;
				ports : pac_ports.map;
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

		
	begin
		query_element (
			position	=> module_cursor,
			process		=> query_devices'access);

		return result;
	end device_unit_port_exists;


	

	
	
	function get_available_units (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC1
		log_threshold	: in type_log_level)
		return pac_unit_names.list
	is
		device_cursor_sch : pac_devices_sch.cursor;

		device_model : pac_device_model_file.bounded_string;
		device_cursor_lib : pac_devices_lib.cursor;

		use pac_unit_names;
		all_unit_names : pac_unit_names.list;
		names_of_available_units : pac_unit_names.list;

		
		procedure query_in_use (c : in pac_unit_names.cursor) is 
			use pac_unit_name;
			in_use : boolean := false;

			-- Sets the in_use flag if given unit is already in use:
			procedure query_in_use (
				device_name	: in type_device_name;
				device		: in type_device_sch) 
			is
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
			module		: in type_generic_module)
		is begin
			-- locate the device in the schematic:
			device_cursor_sch := find (module.devices, device_name);

			device_model := element (device_cursor_sch).model;

			log (text => "device model " & to_string (device_model),
				level => log_threshold + 1);
		end get_device_model;
		
		
	begin
		log (text => "looking up available units of " 
			 & to_string (device_name) & " ...",
			 level => log_threshold);

		log_indentation_up;

		-- get the device model:
		query_element (module_cursor, get_device_model'access);
				
		-- locate the device in the library
		device_cursor_lib := get_device_model_cursor (device_model);

		log_indentation_up;
		
		-- get the names of all units of the device
		all_unit_names := get_all_units (device_cursor_lib);

		-- extract available units
		all_unit_names.iterate (query_in_use'access);

		log_indentation_down;
		log_indentation_down;
		
		return names_of_available_units;

		--exception when event: others =>
			--put_line (exception_information (event));

			--return names_of_available_units;
	end get_available_units;



	

	
	function unit_available (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC1
		unit_name		: in pac_unit_name.bounded_string)
		return boolean
	is
		available : boolean := true; -- to be returned

		device_cursor_sch : pac_devices_sch.cursor;
		
		device_cursor_lib : pac_devices_lib.cursor;
		
		use pac_unit_names;
		all_unit_names : pac_unit_names.list;

		
		-- Clears the "available" flag if given unit is already in use:
		procedure query_in_use (
			device_name	: in type_device_name;
			device		: in type_device_sch) 
		is
		begin
			if contains (device.units, unit_name) then
				available := false;
			end if;
		end query_in_use;
		
		
	begin -- unit_available
		device_cursor_lib := device_model_cursor (module_cursor, device_name);

		-- get the names of all units of the device
		all_unit_names := get_all_units (device_cursor_lib);

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




	

	function get_units_on_sheet (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC1
		sheet			: in type_sheet;
		log_threshold	: in type_log_level)
		return pac_unit_names.list
	is
		device_cursor_sch : pac_devices_sch.cursor;

		names_of_units : pac_unit_names.list;

		
		procedure query_units (
			device_name	: in type_device_name;
			device		: in type_device_sch)
		is 
			procedure query_unit (c : in pac_units.cursor) is 
				use pac_unit_name;
				use pac_unit_names;
			begin
				-- If the unit is on the given sheet then append it to the result:
				if get_sheet (element (c).position) = sheet then
					log (text => "unit " & to_string (key (c)) & " on sheet.",
						level => log_threshold + 2);
					
					names_of_units.append (key (c));
				end if;
			end query_unit;

			
		begin
			device.units.iterate (query_unit'access);
		end query_units;

		
	begin
		log (text => "looking up units of " 
			 & to_string (device_name) 
			 & " on sheet " & to_string (sheet) & " ...",
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
	end get_units_on_sheet;




	
	
	function get_position (
		module_cursor	: in pac_generic_modules.cursor;
		device			: in type_device_name; -- R2
		unit			: in pac_unit_name.bounded_string)
		return type_object_position
	is
		device_cursor_sch : pac_devices_sch.cursor;

		unit_position : type_object_position;

		
		procedure query_unit (
			device_name	: in type_device_name;
			device		: in type_device_sch)
		is 
			unit_cursor : pac_units.cursor;
		begin
			-- locate the given unit in the given device
			unit_cursor := find (device.units, unit);

			-- get the coordinates of the unit
			unit_position := element (unit_cursor).position;
		end query_unit;

		
	begin
		-- locate the device in the schematic:
		device_cursor_sch := locate_device (module_cursor, device);

		query_element (
			position	=> device_cursor_sch,
			process		=> query_unit'access);

		return unit_position;
	end get_position;




	
	

	

	
	function get_sheet (
		module_cursor	: in pac_generic_modules.cursor;
		device			: in type_device_name; -- R2
		unit			: in pac_unit_name.bounded_string)
		return type_sheet
	is begin		
		return get_sheet (get_position (module_cursor, device, unit));
	end get_sheet;

	





	
	procedure delete_ports (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name;
		unit_name		: in pac_unit_name.bounded_string;
		ports			: in pac_ports.map;
		sheet			: in type_sheet;
		log_threshold	: in type_log_level)
	is
		use pac_unit_name;
		
		-- In the course of this procedure the given list
		-- of ports is processed. Each processed port will be
		-- removed from the given ports. For this reason we make
		-- a copy of the given ports:
		ports_tmp : pac_ports.map := ports;

		-- CS: On the end of this procedure make sure ports_tmp is empty.
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_nets;
			net_cursor : pac_nets.cursor := module.nets.first;
			
			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is
				use pac_strands;
				strand_cursor : pac_strands.cursor := net.strands.first;

				
				procedure query_strand (
					strand	: in out type_strand)
				is
					use pac_net_segments;
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure query_segment (
						segment : in out type_net_segment)
					is
				
						-- Searches in list ports_tmp for a port that sits on
						-- the given A/B end of the segment. If a port exists there,
						-- then:
						-- 1. the port is removed from list port_tmp 
						-- 2. the unit name of the port is used to 
						--    build a device port.
						-- 3. The device port is then deleted on the A/B end of the segment:
						procedure delete_port (AB_end : in type_start_end_point) is 
							port_name : pac_port_name.bounded_string; -- IN1, IN2
							device_port : type_device_port; -- (IC1, AMP1, IN1)
							deleted : boolean;
						begin
							-- Get the name of the port that is connected
							-- with the given end of the segment candidate
							-- and delete it in list ports_tmp.
							-- NOTE: We need to do this only once, because only one port of
							-- the unit can be connected with the end of the segment.
							delete_port (ports_tmp, get_end_point (segment, AB_end), deleted, port_name);

							-- If a port has been found, then the delete-flag is set
							-- and port_name loaded with the affected port name.
							-- If nothing was found, then ports_tmp is unchanged and the
							-- flag "deleted" is cleared so that nothing else happens here:
							if deleted then
								
								-- Build the device port:
								device_port := to_device_port (device_name, unit_name, port_name);
								
								log (text => "Delete device port " & to_string (device_port),
									 level => log_threshold + 1);
								-- CS log segment and end point
								
								-- Remove the device port from the segment:
								delete_device_port (segment, AB_end, device_port);
							end if;
						end delete_port;

		
					begin
						delete_port (A);
						delete_port (B);
					end query_segment;

					
				begin
					-- Iterate through the segments:
					while has_element (segment_cursor) loop
						strand.segments.update_element (segment_cursor, query_segment'access);
						next (segment_cursor);
					end loop;
				end query_strand;

				
			begin
				-- Iterate through the strands on the given sheet:
				while has_element (strand_cursor) loop
					if get_sheet (strand_cursor) = sheet then
						net.strands.update_element (strand_cursor, query_strand'access);
					end if;
					
					next (strand_cursor);
				end loop;

			end query_net;				

			
		begin
			-- Iterate through the nets:
			while has_element (net_cursor) loop
				module.nets.update_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " delete ports of device " & to_string (device_name)
			& " unit " & to_string (unit_name)
			& " in nets on sheet " & to_string (sheet),
			level => log_threshold);

		
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end delete_ports;




	


	procedure insert_ports (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name;
		unit_name		: in pac_unit_name.bounded_string;
		ports			: in pac_ports.map;
		sheet			: in type_sheet;
		log_threshold	: in type_log_level)
	is
		use pac_unit_name;
		
		-- In the course of this procedure the given list
		-- of ports is processed. Each processed port will be
		-- removed from the given ports. For this reason we make
		-- a copy of the given ports:
		ports_tmp : pac_ports.map := ports;

		-- CS: On the end of this procedure make sure ports_tmp is empty.
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_nets;
			net_cursor : pac_nets.cursor := module.nets.first;
			
			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is
				use pac_strands;
				strand_cursor : pac_strands.cursor := net.strands.first;

				
				procedure query_strand (
					strand	: in out type_strand)
				is
					use pac_net_segments;
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure query_segment (
						segment : in out type_net_segment)
					is
				
						-- Searches in list ports_tmp for a port that sits on
						-- the given A/B end of the segment. If a port exists there,
						-- then:
						-- 1. the port is removed from list port_tmp 
						-- 2. the unit name of the port is used to 
						--    build a device port.
						-- 3. The device port is then added to the A/B end of the segment:
						procedure add_port (AB_end : in type_start_end_point) is 
							port_name : pac_port_name.bounded_string; -- IN1, IN2
							device_port : type_device_port; -- (IC1, AMP1, IN1)
							deleted : boolean;
						begin
							-- Get the name of the port that is connected
							-- with the given end of the segment candidate
							-- and delete it in list ports_tmp.
							-- NOTE: We need to do this only once, because only one port of
							-- the unit can be connected with the end of the segment.
							delete_port (ports_tmp, get_end_point (segment, AB_end), deleted, port_name);

							-- If a port has been found, then the delete-flag is set
							-- and port_name loaded with the affected port name.
							-- If nothing was found, then ports_tmp is unchanged and the
							-- flag "deleted" is cleared so that nothing else happens here:
							if deleted then
								
								-- Build the device port:
								device_port := to_device_port (device_name, unit_name, port_name);
								
								log (text => "Add device port " & to_string (device_port),
									 level => log_threshold + 1);
								-- CS log segment and end point
								
								-- Add the device port to the targeted end of the segment:
								insert_device_port (segment, AB_end, device_port);
							end if;
						end add_port;

		
					begin
						-- Add device ports on A and B end:
						add_port (A);
						add_port (B);
					end query_segment;

					
				begin
					-- Iterate through the segments:
					while has_element (segment_cursor) loop
						strand.segments.update_element (segment_cursor, query_segment'access);
						next (segment_cursor);
					end loop;
				end query_strand;

				
			begin
				-- Iterate through the strands on the given sheet:
				while has_element (strand_cursor) loop
					if get_sheet (strand_cursor) = sheet then
						net.strands.update_element (strand_cursor, query_strand'access);
					end if;
					
					next (strand_cursor);
				end loop;

			end query_net;				

			
		begin
			-- Iterate through the nets:
			while has_element (net_cursor) loop
				module.nets.update_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " insert ports of device " & to_string (device_name)
			& " unit " & to_string (unit_name) 
			& " in nets on sheet " & to_string (sheet),
			level => log_threshold);
		
		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end insert_ports;
	

	



	
	
	procedure fetch_unit (
		module_cursor 	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC1
		unit_name		: in pac_unit_name.bounded_string; -- A, B, IO_BANK_2
		destination		: in type_object_position; -- sheet/x/y/rotation
		log_threshold	: in type_log_level) is separate;




	
	function unit_positions_valid (
		module_cursor 	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
		return boolean 
	is
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
	


	

	function get_port_properties (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A, B, IO_BANK_2
		port_name		: in pac_port_name.bounded_string) -- CE
		return type_port_properties_access
	is
		properties : type_port_properties_access; -- to be returned
		
		terminal_name : et_terminals.pac_terminal_name.bounded_string;

		use et_port_direction;
		port_direction : type_port_direction := PASSIVE;
		port_properties_cursor : pac_ports.cursor;

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			device_cursor_sch	: pac_devices_sch.cursor;
			variant 			: pac_package_variant_name.bounded_string; -- D, N
			device_cursor_lib	: pac_devices_lib.cursor;

			
			procedure query_variants (
				model	: in pac_device_model_file.bounded_string;
				device	: in type_device_model) 
			is
				variant_cursor : pac_variants.cursor;

				
				procedure query_ports (
					variant_name	: in pac_package_variant_name.bounded_string;
					variant			: in type_variant) 
				is
					use pac_terminal_port_map;
					terminal_cursor : pac_terminal_port_map.cursor := variant.terminal_port_map.first;
					use pac_port_name;
					use pac_unit_name;
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

			
			use pac_ports;

			
		begin -- query_devices
			-- locate the device in schematic (default assembly variant):
			device_cursor_sch := find (module.devices, device_name);

-- 			if device_cursor_sch /= pac_devices_sch.no_element then
			
				variant := element (device_cursor_sch).variant;

				-- get the name of the device model (or the generic name)
				device_cursor_lib := get_device_model_cursor (element (device_cursor_sch).model);

				-- Get the name of the terminal (the pin or pad) according to the device variant.
				-- Store it in variable terminal_name:
				pac_devices_lib.query_element (
					position	=> device_cursor_lib,
					process		=> query_variants'access);

				-- Get the electrical properties of the port of the current device:
				port_properties_cursor := get_properties (device_cursor_lib, port_name);

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



	

	
	
	
	procedure delete_unit (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		log_threshold	: in type_log_level) 
	is
		device_cursor_sch : pac_devices_sch.cursor;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			-- Query whether the given unit is deployed in the schematic:
			unit_query : constant type_unit_query := 
				get_unit_position (device_cursor_sch, unit_name);

			-- The ports of the unit must be removed from the net segments.
			-- For this reason we need some temporarily storage place:
			sheet_old : type_sheet;
			ports_old : pac_ports.map;
			

			procedure query_device (
				device_name	: in type_device_name;
				device		: in out type_device_sch) 
			is
				-- Locate the targeted unit:
				unit_cursor : pac_units.cursor := locate_unit (device, unit_name);
			begin
				-- Get the sheet where the unit is:
				sheet_old := get_sheet (device_cursor_sch, unit_cursor);
				
				-- Get the ports of the unit:
				ports_old := get_ports_of_unit (device_cursor_sch, unit_cursor);

				-- Delete the unit:
				device.units.delete (unit_cursor);				
			end query_device;

			
			
		begin -- query_module

			-- Test whether the desired unit is deployed (in schematic).
			-- If the unit is deployed, then delete it:
			if unit_query.exists then
				
				update_element (
					container	=> module.devices,
					position	=> device_cursor_sch,
					process		=> query_device'access);
			
				-- Remove the old ports of the unit from the net segments:
				delete_ports (
					module_cursor	=> module_cursor,
					device_name		=> device_name,
					unit_name		=> unit_name,
					ports			=> ports_old,
					sheet			=> sheet_old,
					log_threshold	=> log_threshold + 1);

				-- If no more units are deployed, then the whole device
				-- must be removed from the module:
				if get_unit_count_deployed (device_cursor_sch) = 0 then
					log (text => "No more units deployed. Delete device entirely.",
						 level => log_threshold + 1);
					
					module.devices.delete (device_cursor_sch);
				end if;
			else
				log (WARNING, "Unit " & to_string (unit_name) & " is not deployed in the schematic");
			end if;
		end query_module;

		
		
	begin
		log (text => "module " & to_string (module_cursor) &
			 " delete " & to_string (device_name) & " unit " & 
			 to_string (unit_name),
			 level => log_threshold);

		log_indentation_up;
		
		-- Locate the targeted device in the given module.
		-- If the device exists, then proceed with further actions.
		-- Otherwise abort this procedure with a warning:
		device_cursor_sch := locate_device (module_cursor, device_name);
			
		if has_element (device_cursor_sch) then -- device exists in schematic
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> query_module'access);

		else
			log (WARNING, " Device " & to_string (device_name) & " not found !");
		end if;

		update_ratsnest (module_cursor, log_threshold + 1);
		
		log_indentation_down;
	end delete_unit;
	


	


	

	procedure delete_device (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		log_threshold	: in type_log_level) 
	is
		device_cursor_sch : pac_devices_sch.cursor;

		-- We use list of deployed units here. By iterating the names
		-- the procedure delete_unit (see above) is called for each unit:
		unit_names : pac_unit_names.list;


		-- Queries a unit of the list unit_names and deletes it:
		procedure query_unit (c : in pac_unit_names.cursor) is
			use pac_unit_names;
			name : pac_unit_name.bounded_string := element (c);
		begin
			log (text => "Delete unit " & to_string (name), level => log_threshold + 1);

			log_indentation_up;
			
			delete_unit (module_cursor, device_name, name, log_threshold + 2);
			
			log_indentation_down;
		end;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " delete " & to_string (device_name), 
			 level => log_threshold);

		log_indentation_up;
		
		-- Locate the targeted device in the given module.
		-- If the device exists, then proceed with further actions.
		-- Otherwise abort this procedure with a warning:
		device_cursor_sch := locate_device (module_cursor, device_name);
			
		if has_element (device_cursor_sch) then -- device exists in schematic

			-- Get the names of deployed units:
			unit_names := get_unit_names_deployed (device_cursor_sch);

			-- Iterate the name list and delete the units one by one:
			unit_names.iterate (query_unit'access);
			
		else
			log (WARNING, " Device " & to_string (device_name) & " not found !");
		end if;

		update_ratsnest (module_cursor, log_threshold + 1);
		
		log_indentation_down;
	end delete_device;




	


	procedure rename_device_ports (
		module_cursor	: in pac_generic_modules.cursor;
		device_before	: in type_device_name;	-- the device name before like IC1
		device_after	: in type_device_name;	-- the device name after like IC23
		sheets			: in pac_unit_positions.map; -- the sheet numbers where to look at
		log_threshold	: in type_log_level) 
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
-- 			procedure query_net (net_cursor : in pac_nets.cursor) is
-- 				use pac_nets;
-- 
-- 				procedure query_strands (
-- 					net_name	: in pac_net_name.bounded_string;
-- 					net			: in out type_net) 
-- 				is
-- 					procedure query_strand (strand_cursor : in pac_strands.cursor) is
-- 						procedure query_segments (strand : in out type_strand) is
-- 							use pac_net_segments;
-- 
-- 							procedure query_segment (segment_cursor : in pac_net_segments.cursor) is 
-- 								use pac_device_ports;
-- 
-- 								procedure query_ports (segment : in out type_net_segment) is
-- 								-- Tests device ports of given segment if their device name matches the given device name.
-- 								-- On match replace the old device name by the new device name.
-- 
-- 									port_cursor : pac_device_ports.cursor := segment.ports.devices.first;
-- 									
-- 								begin -- query_ports
-- 									while port_cursor /= pac_device_ports.no_element loop
-- 
-- 										if element (port_cursor).device_name = device_before then -- IC1
-- 
-- 											replace_element (
-- 												container	=> segment.ports.devices,
-- 												position	=> port_cursor,
-- 												new_item	=> (
-- 													device_name	=> device_after, -- IC23
-- 													unit_name	=> element (port_cursor).unit_name, -- unchanged
-- 													port_name 	=> element (port_cursor).port_name) -- unchanged
-- 												);
-- 										end if;
-- 										
-- 										next (port_cursor);
-- 									end loop;
-- 								end query_ports;
-- 
-- 								
-- 							begin -- query_segment
-- 								log_indentation_up;
-- 								log (text => to_string (segment_cursor), level => log_threshold + 2);
-- 
-- 								update_element (
-- 									container	=> strand.segments,
-- 									position	=> segment_cursor,
-- 									process		=> query_ports'access);
-- 												   
-- 								log_indentation_down;
-- 							end query_segment;
-- 
-- 							
-- 						begin -- query_segments
-- 							iterate (strand.segments, query_segment'access);
-- 						end query_segments;
-- 						
-- 					begin -- query_strand
-- 						log_indentation_up;
-- 						log (text => "strand " & to_string (position => element (strand_cursor).position),
-- 							 level => log_threshold + 2);
-- 
-- 						update_element (
-- 							container	=> net.strands,
-- 							position	=> strand_cursor,
-- 							process		=> query_segments'access);
-- 						
-- 						log_indentation_down;
-- 					end query_strand;
-- 					
-- 				begin -- query_strands
-- 					iterate (net.strands, query_strand'access);
-- 				end query_strands;
-- 				
-- 			begin -- query_net
-- 				log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);
-- 
-- 				update_element (
-- 					container	=> module.nets,
-- 					position	=> net_cursor,
-- 					process		=> query_strands'access);
-- 				
-- 			end query_net;				
-- 			
		begin
			null;
-- 			pac_nets.iterate (module.nets, query_net'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			& " rename ports of devices in nets. Device old: " & to_string (device_before) 
			& " to device new: " & to_string (device_after),
			level => log_threshold);


		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end rename_device_ports;





	
	

	procedure rename_device (
		module_cursor		: in pac_generic_modules.cursor;
		device_name_before	: in type_device_name; -- IC1
		device_name_after	: in type_device_name; -- IC23
		log_threshold		: in type_log_level) 
	is
		device_cursor_sch : pac_devices_sch.cursor;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_cursor_after  : pac_devices_sch.cursor;
			inserted : boolean;

			-- Temporarily storage of unit coordinates:
			unit_positions : pac_unit_positions.map;

		begin
			-- Before deleting the old device, the coordinates of the
			-- units must be fetched. Their sheet numbers will later assist
			-- renaming the port names connected with net segments.
			unit_positions := get_unit_positions (device_cursor_sch);
			
			-- Copy elements and properties of the old device to a new one:
			pac_devices_sch.insert (
				container	=> module.devices,
				key			=> device_name_after, -- IC23
				new_item	=> element (device_cursor_sch), -- all elements and properties of IC1
				inserted	=> inserted,
				position	=> device_cursor_after);

				
			-- Delete the old device:
			pac_devices_sch.delete (
				container	=> module.devices,
				position	=> device_cursor_sch);

			-- Rename all ports in module.nets
			rename_device_ports (
				module_cursor	=> module_cursor,
				device_before	=> device_name_before,
				device_after	=> device_name_after,
				sheets			=> unit_positions, -- the sheets to look at
				log_threshold	=> log_threshold + 1);
			
		end query_module;

		

		procedure check_names is begin
				
			-- The old and new name must not be the same:
			if device_name_after /= device_name_before then

				-- The old and new prefix must be the same in order to
				-- prevent an inadvertently category change:
				if same_prefix (device_name_after, device_name_before) then

					-- A device having the new name must
					-- not exist yet:
					if not device_exists (module_cursor, device_name_after) then
						
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
			& " rename device " & to_string (device_name_before) 
			& " to " & to_string (device_name_after),
			level => log_threshold);

		log_indentation_up;
		
		-- Locate the targeted device in the given module.
		-- If the device exists, then proceed with further actions.
		-- Otherwise abort this procedure with a warning:
		device_cursor_sch := locate_device (module_cursor, device_name_before);
			
		if has_element (device_cursor_sch) then -- device exists in schematic
			check_names;
		else
			log (WARNING, " Device " & to_string (device_name_before) & " not found !");
		end if;
		
		log_indentation_down;
	end rename_device;

	

	

	
	procedure move_unit (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		destination		: in type_vector_model; -- x/y
		log_threshold	: in type_log_level) 
	is

		device_cursor_sch : pac_devices_sch.cursor;
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			-- Query whether the given unit is deployed in the schematic:
			unit_query : constant type_unit_query := 
				get_unit_position (device_cursor_sch, unit_name);

			-- The old ports of the unit must be removed from the net segments,
			-- whereas new ports must be inserted in the net segments.
			-- For this reason we need some temporarily storage place:
			sheet_old, sheet_new : type_sheet;
			ports_old, ports_new : pac_ports.map;
			

			procedure query_device (
				device_name	: in type_device_name;
				device		: in out type_device_sch) 
			is
				
				-- Does the actual move of the unit:
				procedure move_unit (
					unit_name	: in pac_unit_name.bounded_string;
					unit		: in out type_unit) 
				is begin					
					case coordinates is
						when ABSOLUTE =>
							-- build the new position while preserving rotation:
							unit.position := to_position (
								point		=> destination, 
								sheet		=> type_sheet (sheet),
								rotation	=> get_rotation (unit.position));

						when RELATIVE =>
							move (
								position	=> unit.position,
								offset		=> to_position_relative (destination, sheet));
								-- rotation remains as it is
					end case;								
				end move_unit;

				-- Locate the targeted unit:
				unit_cursor : pac_units.cursor := locate_unit (device, unit_name);

				
			begin
				-- Get the sheet where the unit is BEFORE the move operation:
				sheet_old := get_sheet (device_cursor_sch, unit_cursor);
				
				-- Get the ports of the unit as they are
				-- BEFORE the move operation:
				ports_old := get_ports_of_unit (device_cursor_sch, unit_cursor);

				update_element (
					container	=> device.units,
					position	=> unit_cursor,
					process		=> move_unit'access);


				-- Get the sheet where the unit is AFTER the move operation:
				sheet_new := get_sheet (device_cursor_sch, unit_cursor);

				-- Get the ports of the unit as they are
				-- AFTER the move operation:
				ports_new := get_ports_of_unit (device_cursor_sch, unit_cursor);
				
			end query_device;

			
			
		begin -- query_module

			-- Test whether the desired unit is deployed (in schematic).
			-- If the unit is deployed, then move it:
			if unit_query.exists then
				
				update_element (
					container	=> module.devices,
					position	=> device_cursor_sch,
					process		=> query_device'access);
			
				-- Remove the old ports of the unit from the net segments:
				delete_ports (
					module_cursor	=> module_cursor,
					device_name		=> device_name,
					unit_name		=> unit_name,
					ports			=> ports_old,
					sheet			=> sheet_old,
					log_threshold	=> log_threshold + 1);

				-- Insert the new unit ports in the net segments:
				insert_ports (
					module_cursor	=> module_cursor,
					device_name		=> device_name,
					unit_name		=> unit_name,
					ports			=> ports_new,
					sheet			=> sheet_new,
					log_threshold	=> log_threshold + 1);

			else
				log (WARNING, "Unit " & to_string (unit_name) & " is not deployed in the schematic");
			end if;
		end query_module;

		
	begin -- move_unit
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_cursor)
					& " move " & to_string (device_name)
					& " unit " & to_string (unit_name)
					& " to sheet" & to_string (sheet) 
					& to_string (destination),
					level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_cursor)
					& " move " & to_string (device_name)
					& " unit " & to_string (unit_name)
					& " by " & relative_to_string (sheet) & " sheet(s)" 
					& to_string (destination),
					level => log_threshold);
		end case;

		
		log_indentation_up;
		
		-- Locate the targeted device in the given module.
		-- If the device exists, then proceed with further actions.
		-- Otherwise abort this procedure with a warning:
		device_cursor_sch := locate_device (module_cursor, device_name);
			
		if has_element (device_cursor_sch) then -- device exists in schematic
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> query_module'access);

		else
			log (WARNING, " Device " & to_string (device_name) & " not found !");
		end if;

		update_ratsnest (module_cursor, log_threshold + 1);
		
		log_indentation_down;
	end move_unit;



	

	
	-- Drags the net segments according to the given drag_list of a unit.
	-- Changes the position of start or end points of segments.
	-- Does NOT create new connections with segments if a port
	-- lands on the start or end point of another segment.
	-- Does NOT create a new connection with a segments if a port
	-- lands between start and end point.
	procedure drag_net_segments (
		module			: in pac_generic_modules.cursor;-- the module
		drag_list		: in type_drags_of_ports.map;	-- the old and new port positions
		sheet			: in type_sheet;				-- the sheet to look at
		log_threshold	: in type_log_level) 
	is

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			procedure query_net (net_cursor : in pac_nets.cursor) is
				use pac_nets;
				use et_symbols;

				procedure query_strands (
					net_name	: in pac_net_name.bounded_string;
					net			: in out type_net) is
					use et_schematic_coordinates;
					
					use pac_strands;
					strand_cursor : pac_strands.cursor;
					
					use type_drags_of_ports;
					drag_cursor : type_drags_of_ports.cursor := drag_list.first;

					drag_processed : boolean;

					-- We must keep record of segments that have been dragged already.
					-- Each time a segment has been dragged, it will be appended to
					-- this list:
					already_dragged_segments : pac_net_segments.list;

					
					procedure query_segments (strand : in out type_strand) is
						use pac_net_segments;

						
						procedure query_segment (segment_cursor : in pac_net_segments.cursor) is 

							-- Changes the position of start or end point of a segment 
							-- according to the drag point:
							procedure change_segment (
								segment : in out type_net_segment) 
							is begin								
								-- if port sits on a start point of a segment -> move start point
								if get_A (segment) = element (drag_cursor).before then
									log (text => "move segment start point from" & 
										to_string (get_A (segment)),
										level => log_threshold + 3);

									set_A (segment, element (drag_cursor).after);

									log (text => "to" & 
										to_string (get_A (segment)),
										level => log_threshold + 3);

									-- Now the segment has been dragged. Store it
									-- in list of already dragged segments:
									already_dragged_segments.append (segment);
									
									drag_processed := true;
								end if;

								-- if port sits on an end point of a segment -> move end point
								if get_B (segment) = element (drag_cursor).before then
									log (text => "move segment end point from" & 
										to_string (get_B (segment)),
										level => log_threshold + 3);

									set_B (segment, element (drag_cursor).after);

									log (text => "to" & 
										to_string (get_B (segment)),
										level => log_threshold + 3);

									-- Now the segment has been dragged. Store it
									-- in list of already dragged segments:
									already_dragged_segments.append (segment);
									
									drag_processed := true;
								end if;
							end change_segment;

							
						begin -- query_segment
							-- Probe only those segments which have not been dragged already:
							if not already_dragged_segments.contains (element (segment_cursor)) then
								
								log_indentation_up;
								log (text => "probing " & to_string (segment_cursor), level => log_threshold + 2);
								log_indentation_up;
								
								update_element (
									container	=> strand.segments,
									position	=> segment_cursor,
									process		=> change_segment'access);
												
								log_indentation_down;
								log_indentation_down;
								
							end if;
						end query_segment;

						
					begin -- query_segments
						-- Probe segments of this strand. Skip segments that have been
						-- dragged already:
						iterate (strand.segments, query_segment'access);

						-- Update strand position if any movement took place.
						if drag_processed then
							set_strand_position (strand); 
						end if;						
					end query_segments;

					
				begin -- query_strands
					-- loop in drag list
					while drag_cursor /= type_drags_of_ports.no_element loop
						log (text => "probing port " & to_string (key (drag_cursor)), level => log_threshold + 1);
						log_indentation_up;

						-- If the current drag point sits on a strand, this flag will go true. Other 
						-- strands will then not be looked at because the point can only sit on 
						-- one strand.
						drag_processed := false;
						
						strand_cursor := net.strands.first;
						while strand_cursor /= pac_strands.no_element loop
							
							-- We pick out only the strands on the targeted sheet:
							if get_sheet (element (strand_cursor).position) = sheet then
								log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);

								log_indentation_up;
								log (text => "strand " & to_string (position => element (strand_cursor).position),
									level => log_threshold + 1);
							
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
			pac_nets.iterate (module.nets, query_net'access);
		end query_nets;

		
	begin -- drag_net_segments
		log (text => "dragging net segments with units on sheet" & 
			 to_string (sheet) & " ...", level => log_threshold);
		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;
	end drag_net_segments;



	

	
	-- Tests whether the given unit ports at their individual location are movable. 
	-- The criteria for movement are: no netchanger port, no device port, no submodule ports there.
	-- The only port allowed at an individual drag point is the port-to-be-dragged itself.
	-- CS: Becomes obsolete once ports at the same x/y position are prevented.
	procedure movable_test (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name;
		unit_name		: in pac_unit_name.bounded_string;
		location 		: in type_object_position; -- only sheet number matters
		unit_ports		: in pac_ports.map;
		log_threshold	: in type_log_level)
	is
		use pac_ports;
		port_cursor : pac_ports.cursor := unit_ports.first;

		
		procedure test_point (port_cursor : in pac_ports.cursor) is
			point : type_object_position; -- the point
			ports : type_ports;
			port : type_device_port;
			use pac_submodule_ports;
			use pac_device_ports;
			use et_netlists.pac_netchanger_ports;
			use et_schematic_ops.nets;
		begin
			-- assemble the point to be probed
			point := to_position (
				point	=> element (port_cursor).position,
				sheet	=> get_sheet (location));
			
			-- If no net segments start or end at given point then this test won't
			-- complain. If segments are meeting this point, no other ports must be
			-- here (except the port-to-be-dragged):
			if net_segment_at_place (module_cursor, point) then

				-- There are net segments starting or ending at point.
				-- Make sure at point are no ports of devices, submodules or other 
				-- netchangers (except the unit port to be dragged):

				port := (device_name, unit_name, key (port_cursor)); -- IC12, CE
				
				-- Collect all ports of possible other devices, submodules and netchangers
				-- at given point:
				ports := get_ports (module_cursor, point, log_threshold + 1);

				-- If no netchanger and no submodule ports here:
				if is_empty (ports.netchangers) and is_empty (ports.submodules) then

					-- If the ONE and ONLY device/unit port is the 
					-- port-to-be-dragged then everything is fine.
					if length (ports.devices) = 1 then
						
						if contains (ports.devices, port) then
							null; -- fine -> movable test passed
						else
							-- there is another netchanger port
							dragging_not_possible (to_string (key (port_cursor)), point);
						end if;
					
					else
						-- there are more submodule ports
						dragging_not_possible (to_string (key (port_cursor)), point);
					end if;
					
				else -- device or netchanger ports here
					dragging_not_possible (to_string (key (port_cursor)), point);
				end if;
			end if;
		end test_point;

		
	begin -- movable_test
		log (text => "movable test ...", level => log_threshold);
		log_indentation_up;

		while port_cursor /= pac_ports.no_element loop
			test_point (port_cursor);
			next (port_cursor);
		end loop;
		
		log_indentation_down;
	end movable_test;






	function get_unit_position (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string) -- C
		return type_unit_query 
	is
		exists : boolean := false;
		pos : type_object_position; -- x/y, rotation, sheet

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor;

			
			procedure query_units (
				device_name	: in type_device_name; -- IC45
				device		: in type_device_sch) 
			is
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
		
	end get_unit_position;




	
	
	function is_movable (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name;
		unit_name		: in pac_unit_name.bounded_string;
		location 		: in type_object_position; -- only sheet number matters
		unit_ports		: in pac_ports.map;
		log_threshold	: in type_log_level)
		return boolean
	is
		result : boolean := false;
		
		use et_symbols;
		use pac_ports;
		port_cursor : pac_ports.cursor := unit_ports.first;

		
		procedure test_point (port_cursor : in pac_ports.cursor) is
			point : type_object_position; -- the point
			ports : type_ports;
			port : type_device_port;
			use pac_submodule_ports;
			use pac_device_ports;
			use et_netlists.pac_netchanger_ports;
			use et_schematic_ops.nets;
		begin
			-- assemble the point to be probed
			point := to_position (
				point	=> element (port_cursor).position,
				sheet	=> get_sheet (location));
			
			-- If no net segments start or end at given point then this test won't
			-- complain. If segments are meeting this point, no other ports must be
			-- here (except the port-to-be-dragged):
			if net_segment_at_place (module_cursor, point) then

				-- There are net segments starting or ending at point.
				-- Make sure at point are no ports of devices, submodules or other 
				-- netchangers (except the unit port to be dragged):

				port := (device_name, unit_name, key (port_cursor)); -- IC12, CE
				
				-- Collect all ports of possible other devices, submodules and netchangers
				-- at given point:
				ports := get_ports (module_cursor, point, log_threshold + 1);

				-- If no netchanger and no submodule ports here:
				if is_empty (ports.netchangers) and is_empty (ports.submodules) then

					-- If the ONE and ONLY device/unit port is the 
					-- port-to-be-dragged then everything is fine.
					if length (ports.devices) = 1 then
						
						if contains (ports.devices, port) then
							result := true; -- fine -> movable test passed
						else
							-- there is another netchanger port
							result := false;
						end if;
					
					else
						-- there are more submodule ports
						result := false;
					end if;
					
				else -- device or netchanger ports here
					result := false;
				end if;
			end if;
		end test_point;

		
	begin -- is_movable
		log (text => "movable test ...", level => log_threshold);
		log_indentation_up;

		while port_cursor /= pac_ports.no_element loop
			test_point (port_cursor);

			-- abort this loop as soon as a non-movable port has been detected:
			if result = false then
				exit;
			end if;
			
			next (port_cursor);
		end loop;
		
		log_indentation_down;

		return result;
	end is_movable;




	
	
	procedure drag_unit (
		module_cursor 	: in pac_generic_modules.cursor; -- points to the module being modified
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level) 
	is

		
		-- Merges the two maps ports_old and ports_new to a drag list.
		-- The resulting drag list tells which port is to be moved from old to new position.
		function make_drag_list ( 
			ports_old : in pac_ports.map;
			ports_new : in pac_ports.map) 
			return type_drags_of_ports.map 
		is
			use type_drags_of_ports;
			drag_list : type_drags_of_ports.map;

			-- ports_old and ports_new are both equally long and contain 
			-- equal keys (the port names). So we use two cursors and advance them
			-- simultaneously in a loop (see below).
			use pac_ports;
			cursor_old : pac_ports.cursor := ports_old.first;
			cursor_new : pac_ports.cursor := ports_new.first;
		begin
			-- Loop in ports_old, copy the key to the drag list.
			-- Take the old position from ports_old and the new position from ports_new:
			while cursor_old /= pac_ports.no_element loop
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
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor;

			-- temporarily storage of unit coordinates.
			-- There will be only one unit in this container.
			--position_of_unit_old : pac_unit_positions.map;
			position_of_unit_old : type_object_position;	
			position_of_unit_new : type_object_position;

			ports, ports_old, ports_new : pac_ports.map;

			procedure query_unit_location (
				device_name	: in type_device_name;
				device		: in type_device_sch) 
			is
				unit_cursor : pac_units.cursor;
			begin
				if contains (device.units, unit_name) then
					unit_cursor := find (device.units, unit_name); -- the unit should be there

					-- store old unit position
					position_of_unit_old := element (unit_cursor).position;
					log (text => "unit position old: " & to_string (position => position_of_unit_old), level => log_threshold + 1);
				else
					unit_not_found (unit_name);
				end if;
			end query_unit_location;

			
			procedure query_units (
				device_name	: in type_device_name;
				device		: in out type_device_sch) 
			is
				unit_cursor : pac_units.cursor;

				procedure move_unit (
					unit_name	: in pac_unit_name.bounded_string;
					unit		: in out type_unit) 
				is
					use et_schematic_coordinates;

					-- Load the current sheet number where the unit is.
					-- NOTE: The sheet number does not change in drag operations.
					sheet : type_sheet := get_sheet (unit.position);
				begin
					-- Set new x/y position. 
					-- Preserve sheet number and rotation.
					case coordinates is
						when ABSOLUTE =>

							unit.position := to_position (
								point		=> destination, 
								sheet		=> sheet,
								rotation	=> get_rotation (unit.position));
							
						when RELATIVE =>
							move_by (
								point	=> unit.position.place,
								offset	=> destination);
					end case;
					
					exception
						when event: others =>
							log (ERROR, "coordinates invalid !", console => true); -- CS required more details
							log (text => ada.exceptions.exception_information (event), console => true);
							raise;
					
				end move_unit;

				
			begin -- query_units
				unit_cursor := find (device.units, unit_name); -- the unit should be there

				-- move the unit
				update_element (
					container	=> device.units,
					position	=> unit_cursor,
					process		=> move_unit'access);

				-- store new unit position
				position_of_unit_new := element (unit_cursor).position;
				
				log (text => "unit position new: " & to_string (position => position_of_unit_new), level => log_threshold + 1);
			end query_units;

			
		begin -- query_devices
			if contains (module.devices, device_name) then
				device_cursor := find (module.devices, device_name); -- the device should be there

				log_indentation_up;

				-- Before the actual drag, the coordinates of the
				-- unit must be fetched. These coordinates will later assist
				-- in changing the positions of connected net segments.
				
				-- locate the unit, store old position in position_of_unit_old
				query_element (
					position	=> device_cursor,
					process		=> query_unit_location'access);
				
				-- Fetch the ports of the unit to be moved. These are the default port positions
				-- (relative to the symbol origin) as they are defined in the library model.
				ports := get_ports_of_unit (device_cursor, unit_name);
				
				-- Calculate the old and new positions of the unit ports:
				ports_old := ports;
				rotate_ports (ports_old, get_rotation (position_of_unit_old));
				move_ports (ports_old, position_of_unit_old); 
				-- ports_old now contains the absolute port positions in the schematic BEFORE the move.

				-- Test whether the ports of the unit can be dragged.
				-- CS: Might become obsolete once ports at the same x/y position are prevented.
				-- CS: Before the drag: If a port of the unit sits at the same place
				--     where a port of another unit is, then a net segment should be
				--     inserted between them ?
				movable_test (module_cursor, device_name, unit_name, 
					position_of_unit_old, ports_old, log_threshold + 1);

				-- locate the unit, move it, store new position in position_of_unit_new
				update_element (
					container	=> module.devices,
					position	=> device_cursor,
					process		=> query_units'access);
				
				ports_new := ports;
				rotate_ports (ports_new, get_rotation (position_of_unit_new));
				move_ports (ports_new, position_of_unit_new);
				-- ports_new now contains the absolute port positions in the schematic AFTER the move.
				
				-- Change net segments in the affected nets (type_generic_module.nets):
				drag_net_segments (
					module			=> module_cursor,
					drag_list		=> make_drag_list (ports_old, ports_new),
					sheet			=> get_sheet (position_of_unit_new), -- or position_of_unit_old
					log_threshold	=> log_threshold + 1);

				-- The drag operation might result in new port-to-net connections.
				-- So we must insert new ports in segments.
				-- Insert possible new unit ports in the nets (type_generic_module.nets):
				log_indentation_up;
				
				insert_ports (
					module_cursor	=> module_cursor,
					device_name		=> device_name,
					unit_name		=> unit_name,
					ports			=> ports_new,
					sheet			=> get_sheet (position_of_unit_new),
					log_threshold	=> log_threshold + 1);

				log_indentation_down;
				log_indentation_down;				
			else
				device_not_found (device_name);
			end if;
		end query_devices;

		
	begin
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_cursor)
					 & " dragging " & enclose_in_quotes (to_string (device_name)) 
					 & " unit " & enclose_in_quotes	(to_string (unit_name)) 
					 & " to" & to_string (destination), 
					 level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_cursor)
					 & " dragging " & enclose_in_quotes (to_string (device_name)) 
					 & " unit " & enclose_in_quotes	(to_string (unit_name)) 
					 & " by" & to_string (destination), 
					 level => log_threshold);
		end case;
		
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

		update_ratsnest (module_cursor, log_threshold + 1);
	end drag_unit;




	

	
	procedure rotate_unit (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		rotation		: in type_rotation_model; -- 90
		log_threshold	: in type_log_level) 
	is
		device_cursor_sch : pac_devices_sch.cursor;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			-- Query whether the given unit is deployed in the schematic:
			unit_query : constant type_unit_query := 
				get_unit_position (device_cursor_sch, unit_name);

			-- The old ports of the unit must be removed from the net segments,
			-- whereas new ports must be inserted in the net segments.
			-- For this reason we need some temporarily storage place:
			sheet_old, sheet_new : type_sheet;
			ports_old, ports_new : pac_ports.map;
			

			procedure query_device (
				device_name	: in type_device_name;
				device		: in out type_device_sch) 
			is
				
				-- Does the actual rotation of the unit:
				procedure rotate_unit (
					unit_name	: in pac_unit_name.bounded_string;
					unit		: in out type_unit) 
				is begin					
					case coordinates is
						when ABSOLUTE =>
							set_rotation (unit, rotation);
							
						when RELATIVE =>
							rotate_by (unit, rotation);
					end case;

					-- According to the new rotation of the unit,
					-- the placeholders must be rotated also. This is
					-- about the rotation of the about their own origin
					-- and the rotation about the origin of the unit:
					rotate_placeholders (unit.placeholders, rotation);
				end rotate_unit;
				

				-- Locate the targeted unit:
				unit_cursor : pac_units.cursor := locate_unit (device, unit_name);

				
			begin
				-- Get the sheet where the unit is BEFORE the rotate operation:
				sheet_old := get_sheet (device_cursor_sch, unit_cursor);
				
				-- Get the ports of the unit as they are
				-- BEFORE the rotate operation:
				ports_old := get_ports_of_unit (device_cursor_sch, unit_cursor);

				update_element (
					container	=> device.units,
					position	=> unit_cursor,
					process		=> rotate_unit'access);


				-- Get the sheet where the unit is AFTER the rotate operation:
				sheet_new := get_sheet (device_cursor_sch, unit_cursor);

				-- Get the ports of the unit as they are
				-- AFTER the rotate operation:
				ports_new := get_ports_of_unit (device_cursor_sch, unit_cursor);
			end query_device;
			
			
		begin -- query_module

			-- Test whether the desired unit is deployed (in schematic).
			-- If the unit is deployed, then rotate it:
			if unit_query.exists then
				
				update_element (
					container	=> module.devices,
					position	=> device_cursor_sch,
					process		=> query_device'access);
			
				-- Remove the old ports of the unit from the net segments:
				delete_ports (
					module_cursor	=> module_cursor,
					device_name		=> device_name,
					unit_name		=> unit_name,
					ports			=> ports_old,
					sheet			=> sheet_old,
					log_threshold	=> log_threshold + 1);

				-- Insert the new unit ports in the net segments:
				insert_ports (
					module_cursor	=> module_cursor,
					device_name		=> device_name,
					unit_name		=> unit_name,
					ports			=> ports_new,
					sheet			=> sheet_new,
					log_threshold	=> log_threshold + 1);

			else
				log (WARNING, "Unit " & to_string (unit_name) & " is not deployed in the schematic");
			end if;
		end query_module;

		
	begin
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_cursor)
					& " rotate device " & to_string (device_name)
					& " unit " & to_string (unit_name)
					& " to" & to_string (rotation),
					level => log_threshold);

			when RELATIVE =>
				if rotation in type_rotation_relative then
					log (text => "module " & to_string (module_cursor)
						& " rotating " & to_string (device_name)
						& " unit " & to_string (unit_name)
						& " by" & to_string (rotation), 
						level => log_threshold);
				else
					relative_rotation_invalid;
				end if;
		end case;
		
		
		log_indentation_up;
		
		-- Locate the targeted device in the given module.
		-- If the device exists, then proceed with further actions.
		-- Otherwise abort this procedure with a warning:
		device_cursor_sch := locate_device (module_cursor, device_name);
			
		if has_element (device_cursor_sch) then -- device exists in schematic
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> query_module'access);

		else
			log (WARNING, " Device " & to_string (device_name) & " not found !");
		end if;

		update_ratsnest (module_cursor, log_threshold + 1);
		
		log_indentation_down;		
	end rotate_unit;



	


	function get_device_name (
		object	: in type_object_unit)
		return string
	is begin
		return get_device_name (object.device_cursor);
	end;


	


	function get_object_name (
		object	: in type_object_unit)
		return string
	is begin
		return get_full_name (object.device_cursor, object.unit_cursor);
	end get_object_name;

	

	


	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		unit			: in type_object_unit;
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
			is 

				procedure query_unit (
					unit_name	: in pac_unit_name.bounded_string;
					unit		: in out type_unit)
				is begin
					modify_status (unit, operation);
					-- log (text => "done", level => log_threshold + 1);

					-- If the unit is set as moving, then
					-- backup the original position:
					if get_action (operation) = SET and
					   get_flag (operation) = MOVING then

					   object_original_position := get_place (get_position (unit));
					end if;
						
				end query_unit;

				
			begin
				device.units.update_element (unit.unit_cursor, query_unit'access);
			end query_device;

			
		begin
			module.devices.update_element (unit.device_cursor, query_device'access);
		end query_module;
		

	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of device unit "
			& get_object_name (unit)
			& " / " & to_string (operation),
			level => log_threshold);


		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;










	
	procedure propose_units (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor := module.devices.first;

			procedure query_device (
				device_name	: in type_device_name;
				device		: in out type_device_sch)
			is 
				unit_cursor : pac_units.cursor := device.units.first;


				procedure query_unit (
					unit_name	: in pac_unit_name.bounded_string;
					unit		: in out type_unit)
				is begin
					if in_catch_zone (unit, catch_zone, active_sheet) then
						log_indentation_up;
						log (text => to_string (unit_name), level => log_threshold + 2);
						log_indentation_down;
						
						set_proposed (unit);
						count := count + 1;
					end if;
				end query_unit;

				
			begin
				log (text => to_string (device_name), level => log_threshold + 1);
				log_indentation_up;

				-- Iterate through the units:
				while has_element (unit_cursor) loop
					device.units.update_element (unit_cursor, query_unit'access);
					next (unit_cursor);
				end loop;
				log_indentation_down;
			end query_device;

			
		begin
			-- Iterate through the devices:
			while has_element (device_cursor) loop
				module.devices.update_element (device_cursor, query_device'access);
				next (device_cursor);
			end loop;
		end query_module;
		

	begin
		log (text => "module " & to_string (module_cursor)
			& " proposing units in " & to_string (catch_zone),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end propose_units;







	procedure reset_proposed_units (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_device (
				device_name	: in type_device_name;
				device		: in out type_device_sch)
			is 
				unit_cursor : pac_units.cursor := device.units.first;


				procedure query_unit (
					unit_name	: in pac_unit_name.bounded_string;
					unit		: in out type_unit)
				is begin
					log (text => to_string (unit_name), level => log_threshold + 2);
					reset_status (unit);
				end query_unit;

										 
			begin
				log (text => to_string (device_name), level => log_threshold + 1);
				log_indentation_up;

				-- Iterate through the units:
				while has_element (unit_cursor) loop
					device.units.update_element (unit_cursor, query_unit'access);
					next (unit_cursor);
				end loop;
				
				log_indentation_down;
			end query_device;

			
			device_cursor : pac_devices_sch.cursor := module.devices.first;
			
		begin
			-- Iterate through the devices:
			while has_element (device_cursor) loop
				module.devices.update_element (device_cursor, query_device'access);
				next (device_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " resetting proposed units", 
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_proposed_units;







	function get_first_unit (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_unit
	is
		result : type_object_unit;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor := module.devices.first;

			proceed : aliased boolean := true;


			procedure query_device (
				device_name	: in type_device_name;
				device 		: in type_device_sch)
			is 

				procedure query_unit (unit_cursor : in pac_units.cursor) is 

					procedure set_result is begin
						log (text => " found " & get_unit_name (unit_cursor), level => log_threshold + 2);
						result.device_cursor := device_cursor;
						result.unit_cursor := unit_cursor;
						proceed := false; -- no further probing required
					end set_result;

				begin					
					log (text => "unit " & get_unit_name (unit_cursor), level => log_threshold + 2);
					case flag is
						when PROPOSED =>
							if is_proposed (unit_cursor) then
								set_result;
							end if;
		
						when SELECTED =>
							if is_selected (unit_cursor) then
								set_result;
							end if;
		
						when others => null; -- CS
					end case;
				end query_unit;

				
			begin
				log (text => "device " & to_string (device_name), level => log_threshold + 1);
				log_indentation_up;

				-- Iterate through the units:
				iterate (device.units, query_unit'access, proceed'access);

				log_indentation_down;
			end query_device;
			
			
		begin
			-- Iterate through the devices:
			while has_element (device_cursor) and proceed loop
				pac_devices_sch.query_element (device_cursor, query_device'access);
				next (device_cursor);
			end loop;

			if proceed then
				log (text => "nothing found", level => log_threshold);
			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first unit / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;

		return result;
	end get_first_unit;




------------------------------------------------------------------------------------------

-- OBJECTS:
	

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
		result_category : type_object_category := CAT_VOID;
		result_unit 	: type_object_unit;

	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first object / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;

		
		-- SEARCH FOR A UNIT:
		
		-- If a unit has been found, then go to the end of this procedure:
		result_unit := get_first_unit (module_cursor, flag, log_threshold + 1);

		if has_element (result_unit.unit_cursor) then
			-- A unit has been found.
			log (text => get_object_name (result_unit),
				 level => log_threshold + 1);
			
			result_category := CAT_UNIT;
		end if;
		
		-- If nothing has been found then the category is CAT_VOID.
		log_indentation_down;


		-- CS placeholders

		
		
		case result_category is
			when CAT_VOID =>
				return (cat => CAT_VOID);

			when CAT_UNIT =>
				return (CAT_UNIT, result_unit);

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
			device_cursor : pac_devices_sch.cursor := module.devices.first;
			

			procedure query_device (
				name	: in type_device_name;
				device	: in type_device_sch) 
			is 

				-- Queries a unit for its status flag
				-- and appends it to the result:
				procedure query_unit (c : in pac_units.cursor) is 

					-- This procedure appends the matching
					-- device and unit cursor to the result:
					procedure collect is begin
						log (text => get_unit_name (c), level => log_threshold + 4);
						
						result.append ((
							cat		=> CAT_UNIT,
							unit	=> (device_cursor, c)));

					end collect;

					
				begin
					case flag is
						when PROPOSED =>
							if is_proposed (c) then
								collect;
							end if;

						when SELECTED =>
							if is_selected (c) then
								collect;
							end if;

						when others => null; -- CS
					end case;					
				end query_unit;
		
				
			begin
				log (text => to_string (name), level => log_threshold + 2);
				log_indentation_up;
				log (text => "units", level => log_threshold + 3);
				log_indentation_up;
				device.units.iterate (query_unit'access);
				log_indentation_down;
				log_indentation_down;
			end query_device;

			
		begin
			log (text => "devices", level => log_threshold + 1);
			log_indentation_up;
			
			-- Iterate the units of the module:
			while has_element (device_cursor) loop
				query_element (device_cursor, query_device'access);
				next (device_cursor);
			end loop;

			log_indentation_down;

			
			log (text => "placeholders", level => log_threshold + 1);
			log_indentation_up;
			-- CS query placeholders
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
			when CAT_UNIT =>
				modify_status (module_cursor, object.unit, operation, log_threshold + 1);

			-- CS CAT_NANE, ...
				
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

	




	procedure reset_proposed_objects (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor) &
			" resetting proposed objects",
			level => log_threshold);

		log_indentation_up;
		reset_proposed_units (module_cursor, log_threshold + 1);
		-- CS reset_proposed_placeholders
		log_indentation_down;
	end reset_proposed_objects;





	

	procedure move_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " movie object " 
			-- CS & to_string (object)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_UNIT =>

				move_unit (
					module_cursor	=> module_cursor,
					device_name		=> key (object.unit.device_cursor),
					unit_name		=> key (object.unit.unit_cursor),
					coordinates		=> absolute,
					sheet			=> active_sheet,
					destination		=> destination,
					log_threshold	=> log_threshold + 1);


			-- CS CAT_NANE, ...
				
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
			& " rotate object " 
			-- CS & to_string (object)
			& " by 90 degrees",
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_UNIT =>

				rotate_unit (
					module_cursor	=> module_cursor,
					device_name		=> key (object.unit.device_cursor),
					unit_name		=> key (object.unit.unit_cursor),
					coordinates		=> relative, -- in order to rotate by 90 degrees
					rotation		=> 90.0,
					log_threshold	=> log_threshold + 1);


			-- CS CAT_NANE, ...
				
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end rotate_object;


	


	procedure set_segments_moving (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			device_cursor : pac_devices_sch.cursor := module.devices.first;

			
			procedure query_device (
				device_name	: in type_device_name;
				device		: in type_device_sch)
			is 

				procedure query_unit (unit_cursor : in pac_units.cursor) is
					-- Get the sheet where the candidate unit is:
					sheet : type_sheet := get_sheet (device_cursor, unit_cursor);

					-- This procedure takes a port position (indicated by cursor c)
					-- and sets start or end points of net segments which are
					-- at the port position as "moving":
					procedure query_position (c : in pac_points.cursor) is
						use pac_points;
						place : type_vector_model renames element (c);
						position : type_object_position;
					begin
						-- Compose the position of inquiry from port position and sheet number:
						position := to_position (place, sheet);

						-- Set the connected net segments as "moving":
						et_schematic_ops.nets.set_segments_moving (module_cursor, position, log_threshold + 3);
					end query_position;

					
					-- This list contains all points where a port of 
					-- the candidate unit is:
					port_positions : pac_points.list;
					
				begin
					if is_selected (unit_cursor) then
						log (text => "unit " & get_unit_name (unit_cursor),
							 level => log_threshold + 2);

						-- Get the port positions of the candidate unit:
						port_positions := get_port_positions (device_cursor, unit_cursor);

						-- Iterate the port positions:
						port_positions.iterate (query_position'access);
					end if;
				end query_unit;

										 
			begin
				log (text => "device " & to_string (device_name), level => log_threshold + 1);
				log_indentation_up;

				-- Iterate through the units:
				device.units.iterate (query_unit'access);
				
				log_indentation_down;
			end query_device;
			
			
		begin
			-- Iterate through the devices:
			while has_element (device_cursor) loop
				query_element (device_cursor, query_device'access);
				next (device_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " set net segments (connected with selected units) moving.",
			level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end set_segments_moving;




	

	procedure drag_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " dragging object " 
			-- CS & to_string (object)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_UNIT =>

				drag_unit (
					module_cursor	=> module_cursor,
					device_name		=> key (object.unit.device_cursor),
					unit_name		=> key (object.unit.unit_cursor),
					coordinates		=> absolute,
					destination		=> destination,
					log_threshold	=> log_threshold + 1);


			-- CS CAT_NANE, ...
				
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end drag_object;




	

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
			when CAT_UNIT =>

				-- If the unit cursor of the given object
				-- points to a unit, then s single unit
				-- is to be deleted.
				-- If the unit cursor is no_element then
				-- the whole device must be deleted:
				if has_element (object.unit.unit_cursor) then
					
					delete_unit (
						module_cursor	=> module_cursor,
						device_name		=> key (object.unit.device_cursor),
						unit_name		=> key (object.unit.unit_cursor),
						log_threshold	=> log_threshold + 1);

				else
					delete_device (
						module_cursor	=> module_cursor,
						device_name		=> key (object.unit.device_cursor),
						log_threshold	=> log_threshold + 1);

				end if;

			-- CS CAT_NANE, ...
				
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end delete_object;

	
	
end et_schematic_ops.units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
