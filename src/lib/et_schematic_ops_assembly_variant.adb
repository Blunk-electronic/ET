------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   SCHEMATIC OPERATIONS / ASSEMBLY VARIANT                --
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

with et_string_processing;		use et_string_processing;
with et_module;					use et_module;

with et_schematic_ops.units;	use et_schematic_ops.units;
with et_exceptions;				use et_exceptions;


package body et_schematic_ops_assembly_variant is


	
	
	procedure device_not_found (name : in type_device_name) is begin
		raise semantic_error_1 
			with "ERROR: Device " & to_string (name) & " not found !";
	end device_not_found;

	
	procedure device_already_exists (name : in type_device_name) is begin
		raise semantic_error_1
			with "ERROR: Device " & to_string (name) & " already exists !";
	end device_already_exists;

	

	

	
	procedure assembly_variant_not_found (variant : in pac_assembly_variant_name.bounded_string) is 
	begin
		log (ERROR, "assembly variant " &
			 enclose_in_quotes (to_variant (variant)) & " not found !", console => true);
		raise constraint_error;
	end;

	


	

	
	
	function get_active_assembly_variant (
		module_cursor	: in pac_generic_modules.cursor)
		return pac_assembly_variant_name.bounded_string
	is begin
		return element (module_cursor).assembly_variants.active;
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
			av := find (module.assembly_variants.variants, variant);
		end query_module;
	
	begin
		if is_default (variant) then
			av := pac_assembly_variants.no_element;
		else
			pac_generic_modules.query_element (module_cursor, query_module'access);
		end if;
		
		return av;
	end get_active_assembly_variant;

	








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
				container	=> module.assembly_variants.variants,
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
			cursor := find (module.assembly_variants.variants, variant_name);

			if cursor /= et_assembly_variants.pac_assembly_variants.no_element then
				
				delete (
					container	=> module.assembly_variants.variants,
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
		log_threshold	: in type_log_level) 
	is
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
			cursor := et_assembly_variants.pac_assembly_variants.find (
				module.assembly_variants.variants, variant_name);

			if cursor /= et_assembly_variants.pac_assembly_variants.no_element then

				et_assembly_variants.pac_assembly_variants.update_element (
					container	=> module.assembly_variants.variants,
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
			variant_cursor := find (module.assembly_variants.variants, variant);

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
			variant_cursor := find (module.assembly_variants.variants, variant);

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
			-- the variant must exist
			cursor := et_assembly_variants.pac_assembly_variants.find (
				module.assembly_variants.variants, variant_name);

			if cursor /= et_assembly_variants.pac_assembly_variants.no_element then

				et_assembly_variants.pac_assembly_variants.update_element (
					container	=> module.assembly_variants.variants,
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
		if electrical_device_exists (module_cursor, device) then
		
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
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;

		
		procedure unmount (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use et_assembly_variants.pac_assembly_variants;
			cursor : et_assembly_variants.pac_assembly_variants.cursor;

			procedure insert_device (
				name		: in pac_assembly_variant_name.bounded_string;
				variant		: in out et_assembly_variants.type_assembly_variant) 
			is
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
			cursor := et_assembly_variants.pac_assembly_variants.find (
				module.assembly_variants.variants, variant_name);

			if cursor /= et_assembly_variants.pac_assembly_variants.no_element then

				et_assembly_variants.pac_assembly_variants.update_element (
					container	=> module.assembly_variants.variants,
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
		if electrical_device_exists (module_cursor, device) then
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> unmount'access);

		else
			device_not_found (device);
		end if;
			
	end unmount_device;





	
	
	procedure remove_device (
		module_name		: in pac_module_name.bounded_string; -- the module like motor_driver (without extension *.mod)
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		device			: in type_device_name; -- R1
		log_threshold	: in type_log_level) 
	is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;

		
		procedure remove (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use et_assembly_variants.pac_assembly_variants;
			cursor : et_assembly_variants.pac_assembly_variants.cursor;

			
			procedure delete_device (
				name		: in pac_assembly_variant_name.bounded_string;
				variant		: in out et_assembly_variants.type_assembly_variant) 
			is
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
			cursor := et_assembly_variants.pac_assembly_variants.find (
				module.assembly_variants.variants, variant_name);

			if cursor /= et_assembly_variants.pac_assembly_variants.no_element then

				et_assembly_variants.pac_assembly_variants.update_element (
					container	=> module.assembly_variants.variants,
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
		if electrical_device_exists (module_cursor, device) then
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> remove'access);

		else
			device_not_found (device);
		end if;
		
	end remove_device;

	

end et_schematic_ops_assembly_variant;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
