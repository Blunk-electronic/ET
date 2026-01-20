------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE READ / ASSEMBLY VARIANT                        --
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
--                                                                          --
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
-- ToDo:
-- - clean up
-- - rename global variables
--
--

with ada.text_io;					use ada.text_io;

with et_module;						use et_module;
with et_module_names;				use et_module_names;
with et_file_sections;				use et_file_sections;
with et_keywords;					use et_keywords;

with et_module_instance;			use et_module_instance;
with et_device_name;				use et_device_name;
with et_device_model;
with et_device_purpose;
with et_device_value;
with et_device_partcode;
with et_assembly_variants;			use et_assembly_variants;
with et_assembly_variant_name;		use et_assembly_variant_name;

with et_schematic_ops;
with et_schematic_ops_device;		use et_schematic_ops_device;
with et_schematic_ops.submodules;



package body et_module_read_assembly_variant is

	use pac_generic_modules;



	active_assembly_variant : pac_assembly_variant_name.bounded_string; -- "low_cost"

	assembly_variant_name			: pac_assembly_variant_name.bounded_string; -- low_cost
	assembly_variant_description	: et_assembly_variants.type_description; -- "variant without temp. sensor"
	assembly_variant_devices		: et_assembly_variants.pac_device_variants.map;
	assembly_variant_submodules		: et_assembly_variants.pac_submodule_variants.map;


	

	procedure set_active_assembly_variant (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in type_fields_of_line;
		log_threshold	: in type_log_level)
	is
		kw : constant string := f (line, 1);

		
		procedure set_variant (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) is
		begin
			module.assembly_variants.active := active_assembly_variant;
		end;

		
	begin
		if kw = keyword_active then
			expect_field_count (line, 2);
			active_assembly_variant := to_variant (f (line, 2));
		else
			invalid_keyword (kw);
		end if;

		update_element (generic_modules, module_cursor, set_variant'access);
	end set_active_assembly_variant;






	procedure read_assembly_variant (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in type_fields_of_line)
	is
		use et_device_model;
		use et_device_purpose;
		use et_device_value;
		use et_device_partcode;
		use et_module_instance;
		
		kw : constant string := f (line, 1);
		device_name		: type_device_name; -- R1
		device			: access type_device_variant;
		device_cursor	: pac_device_variants.cursor;
		
		submod_name		: pac_module_instance_name.bounded_string; -- MOT_DRV_3
		submod_var		: pac_assembly_variant_name.bounded_string; -- low_cost
		submod_cursor	: pac_submodule_variants.cursor;
		inserted		: boolean;

		use et_schematic_ops.submodules;
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_name then -- name low_cost
			expect_field_count (line, 2);
			assembly_variant_name := to_variant (f (line, 2));

		elsif kw = keyword_description then -- description "variant without temperature sensor"
			expect_field_count (line, 2);

			assembly_variant_description := et_assembly_variants.to_unbounded_string (f (line, 2));
			
		-- A line like "device R1 not_mounted" or
		-- a line like "device R1 value 270R partcode 12345" or		
		-- a line like "device R1 value 270R partcode 12345 purpose "set temperature""
		-- tells whether a device is mounted or not.
		elsif kw = keyword_device then

			-- there must be at least 3 fields:
			expect_field_count (line, 3, warn => false);
			
			device_name := to_device_name (f (line, 2));

			-- test whether device exists
			if not electrical_device_exists (module_cursor, device_name) then
				log (ERROR, "device " &
						enclose_in_quotes (to_string (device_name)) &
						" does not exist !", console => true);
				raise constraint_error;
			end if;

			
			if f (line, 3) = keyword_not_mounted then
				-- line like "device R1 not_mounted"

				device := new type_device_variant'(mounted => et_assembly_variants.NO);
				
			elsif f (line, 3) = keyword_value then
				-- line like "device R1 value 270R partcode 12345"

				-- create a device with discriminant "mounted" where
				-- pointer assembly_variant_device is pointing at.
				device := new type_device_variant'(
					mounted	=> et_assembly_variants.YES,
					others	=> <>); -- to be assigned later
				
				-- there must be at least 6 fields:
				expect_field_count (line, 6, warn => false);

				-- read and validate value
				device.value := to_value_with_check (f (line, 4));

				-- read partcode
				if f (line, 5) = keyword_partcode then
					device.partcode := to_partcode (f (line, 6));
				else -- keyword partcode not found
					log (ERROR, "expect keyword " & enclose_in_quotes (keyword_partcode) &
							" after value !", console => true);
					raise constraint_error;
				end if;

				-- read optional purpose
				if get_field_count (line) > 6 then
					expect_field_count (line, 8);

					if f (line, 7) = keyword_purpose then

						-- validate purpose
						device.purpose := to_purpose (f (line, 8));

					else -- keyword purpose not found
						log (ERROR, "expect keyword " & enclose_in_quotes (keyword_purpose) &
							" after partcode !", console => true);
						raise constraint_error;
					end if;
				end if;
					
			else -- keyword value not found
				log (ERROR, "expect keyword " & enclose_in_quotes (keyword_value) &
						" or keyword " & enclose_in_quotes (keyword_not_mounted) &
						" after device name !", console => true);
				raise constraint_error;
			end if;											

			
			-- Insert the device in the current assembly variant:
			et_assembly_variants.pac_device_variants.insert (
				container	=> assembly_variant_devices,
				key			=> device_name, -- R1
				new_item	=> device.all,
				inserted	=> inserted,
				position	=> device_cursor);

			-- Raise error if device occurs more than once:
			if not inserted then
				log (ERROR, "device " &
						enclose_in_quotes (to_string (device_name)) &
						" already specified !", console => true);
				raise constraint_error;
			end if;

			
		-- a line like "submodule OSC1 variant low_cost
		-- tells which assembly variant of a submodule is used:
		elsif kw = keyword_submodule then

			-- there must be 4 fields:
			expect_field_count (line, 4);

			submod_name := to_instance_name (f (line, 2)); -- OSC1

			-- test whether submodule instance exists
			if not submodule_instance_exists (module_cursor, submod_name) then
				log (ERROR, "submodule instance " &
						enclose_in_quotes (to_string (submod_name)) &
						" does not exist !", console => true);
				raise constraint_error;
			end if;

			-- After the instance name (like OSC1) must come the keyword "variant"
			-- followed by the variant name:
			if f (line, 3) = keyword_variant then
				submod_var := to_variant (f (line, 4));
				
				-- NOTE: A test whether the submodule does provide the variant can
				-- not be executed at this stage because the submodules have not 
				-- been read yet. This will be done after procdure 
				-- read_submodule_files has been executed. See far below.

				-- Insert the submodule in the current assembly variant:
				pac_submodule_variants.insert (
					container	=> assembly_variant_submodules,
					key			=> submod_name, -- OSC1
					new_item	=> (variant => submod_var), -- type_submodule is a record with currently only one element
					inserted	=> inserted,
					position	=> submod_cursor);

				-- Raise error if submodule occurs more than once:
				if not inserted then
					log (ERROR, "submodule " &
						enclose_in_quotes (to_string (submod_name)) &
						" already specified !", console => true);
					raise constraint_error;
				end if;

			else
				log (ERROR, "expect keyword " & enclose_in_quotes (keyword_variant) &
						" after instance name !", console => true);
				raise constraint_error;
			end if;
			
		else
			invalid_keyword (kw);
		end if;
	end read_assembly_variant;


	




	procedure insert_assembly_variant (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
	
	
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			inserted : boolean;
			use et_assembly_variants.pac_assembly_variants;
			cursor : et_assembly_variants.pac_assembly_variants.cursor;
		begin
			log (text => "assembly variant " & 
					enclose_in_quotes (to_variant (assembly_variant_name)), level => log_threshold + 2);

			-- insert variant in container variants
			insert (
				container	=> module.assembly_variants.variants,
				key			=> assembly_variant_name,
				inserted	=> inserted,
				position	=> cursor,
				new_item	=> (
					description	=> assembly_variant_description,
					devices		=> assembly_variant_devices,
					submodules	=> assembly_variant_submodules));

			-- An assembly variant must be unique:
			if not inserted then
				log (ERROR, "assembly variant " & 
						enclose_in_quotes (to_variant (assembly_variant_name)) 
						& " already used !", console => true);
				raise constraint_error;
			end if;

			-- clean up for next assembly variant
			assembly_variant_name := to_variant ("");
			assembly_variant_description := to_unbounded_string ("");
			assembly_variant_devices := et_assembly_variants.pac_device_variants.empty_map;
			assembly_variant_submodules := pac_submodule_variants.empty_map;
			
		end query_module;
		

	
	begin
		log (text => "module " & to_string (module_cursor),
			level => log_threshold);
			
		log_indentation_up;
		
		generic_modules.update_element (module_cursor, query_module'access);
		
	end insert_assembly_variant;
	
	
	
	
	
	
	
	
	
	procedure test_assembly_variants_of_submodules (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

	
		procedure query_variants (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			use et_assembly_variants.pac_assembly_variants;
			
			variant_cursor : pac_assembly_variants.cursor := 
				module.assembly_variants.variants.first;
			
			variant_name : pac_assembly_variant_name.bounded_string; -- low_cost

			
			procedure query_submodules (
				variant_name	: in pac_assembly_variant_name.bounded_string;
				variant			: in type_assembly_variant)
			is
				use pac_submodule_variants;
				submod_cursor	: pac_submodule_variants.cursor := variant.submodules.first;
				submod_name		: pac_module_instance_name.bounded_string; -- CLK_GENERATOR
				submod_variant	: pac_assembly_variant_name.bounded_string; -- fixed_frequency
				use et_schematic_ops.submodules;
			begin
				if submod_cursor = pac_submodule_variants.no_element then
					log (text => "no submodule variants specified", level => log_threshold + 1);
				else
					-- iterate variants of submodules
					while submod_cursor /= pac_submodule_variants.no_element loop
						submod_name := key (submod_cursor); -- CLK_GENERATOR
						submod_variant := element (submod_cursor).variant;
						
						log (text => "submodule instance " & 
								enclose_in_quotes (to_string (submod_name)) &
								" variant " & 
								enclose_in_quotes (to_variant (submod_variant)),
								level => log_threshold + 2);

						if not assembly_variant_exists (module_cursor, submod_name, submod_variant) then
							log (ERROR, "submodule " &
								enclose_in_quotes (to_string (submod_name)) &
								" does not provide assembly variant " &
								enclose_in_quotes (to_variant (submod_variant)) & " !",
								console => true);

							log (text => "Look up section " & section_assembly_variants (2..section_assembly_variants'last) &
									" to fix the issue !");
							
							raise constraint_error;
						end if;

						next (submod_cursor);
					end loop;
				end if;
			end query_submodules;
			
			
		begin -- query_variants
			if variant_cursor = et_assembly_variants.pac_assembly_variants.no_element then
				log (text => "no variants specified", level => log_threshold);
			else
				-- iterate assembly variants of parent module
				while variant_cursor /= et_assembly_variants.pac_assembly_variants.no_element loop
					variant_name := key (variant_cursor);

					-- show assembly variant of parent module
					log (text => "variant " & enclose_in_quotes (to_variant (variant_name)), level => log_threshold + 1);
					log_indentation_up;

					-- look up the submodule variants
					query_element (
						position	=> variant_cursor,
						process		=> query_submodules'access);

					log_indentation_down;
					
					next (variant_cursor);
				end loop;
			end if;
		end query_variants;

			
	begin
		log (text => "verifying assembly variants of submodules ...", level => log_threshold);
		log_indentation_up;

		query_element (
			position	=> module_cursor,
			process		=> query_variants'access);

		log_indentation_down;
	end test_assembly_variants_of_submodules;

	
	
end et_module_read_assembly_variant;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
