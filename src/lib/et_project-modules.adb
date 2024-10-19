------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           PROJECT MODULES                                --
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

with ada.characters;			use ada.characters;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with ada.exceptions;
with ada.directories;
with gnat.directory_operations;

with et_export;
with et_text;					use et_text;
with et_general_rw;				use et_general_rw;
with et_meta;

with et_project.rigs;
with et_exceptions;				use et_exceptions;


package body et_project.modules is

	use pac_generic_modules;

	
	function to_string (project_name : in pac_project_name.bounded_string) return string is begin
		return pac_project_name.to_string (project_name);
	end to_string;

	
	function to_project_name (name : in string) return pac_project_name.bounded_string is begin
		return pac_project_name.to_bounded_string (name);
	end to_project_name;

	
	function to_string (path : in type_et_project_path.bounded_string) return string is begin
		return type_et_project_path.to_string (path);
	end to_string;

	
	function to_project_path (path : in string) return type_et_project_path.bounded_string is begin
		return type_et_project_path.to_bounded_string (path);
	end to_project_path;


	function get_active_module return string is
		use pac_module_name;
	begin
		return pac_module_name.to_string (key (active_module)); -- motor_driver (without extension)
	end get_active_module;


	
	function generic_module_exists (
		module : in pac_module_name.bounded_string) 
		return boolean
	is begin
		return pac_generic_modules.contains (generic_modules, module);
	end;


	
	function locate_module (name : in pac_module_name.bounded_string) -- motor_driver (without extension *.mod)
		return pac_generic_modules.cursor 
	is
		cursor : pac_generic_modules.cursor := find (generic_modules, name);
	begin
		if cursor = pac_generic_modules.no_element then
			raise semantic_error_1 with
				"ERROR: Module " & enclose_in_quotes (to_string (name)) 
				& " does not exist !";
		else
			return find (generic_modules, name);
		end if;
	end locate_module;



	function get_meta_information (
		module : in pac_generic_modules.cursor)
		return et_meta.type_meta
	is begin
		return element (module).meta;
	end get_meta_information;

	

	
	-- Returns the list of preferred schematic libraries:
	function get_preferred_libraries_schematic (module : in pac_generic_modules.cursor)
		return et_meta.pac_preferred_libraries_schematic.list
	is begin
		return element (module).meta.schematic.preferred_libs;
	end get_preferred_libraries_schematic;


	
	-- Returns the list of preferred board libraries (non-electrical packages):
	function get_preferred_libraries_board (module : in pac_generic_modules.cursor)
		return et_meta.pac_preferred_libraries_board.list
	is begin
		return element (module).meta.board.preferred_libs;
	end get_preferred_libraries_board;




	
	procedure save_module (
		module_cursor	: in pac_generic_modules.cursor;
		save_as_name	: in pac_module_name.bounded_string := to_module_name (""); -- motor_driver, templates/clock_generator							  
		log_threshold	: in type_log_level) 
	is separate;


	
	function to_string (section : in type_section) return string is
	-- Converts a section like SEC_NET to a string "net".
		len : positive := type_section'image (section)'length;
	begin
		return to_lower (type_section'image (section) (5..len));
	end to_string;


	
	procedure read_module (
		file_name 		: in string; -- motor_driver.mod, templates/clock_generator.mod
		log_threshold	: in type_log_level) 
		is separate;


		
	procedure create_module (
		module_name		: in pac_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor;
		inserted : boolean;
		use et_string_processing;
	begin
		log (
			text	=> "creating module " & enclose_in_quotes (to_string (module_name)) & " ...",
			level	=> log_threshold);

		-- We create the new module only if does not exist already:

		-- Create an empty module named after the given module name.
		-- So the module names are things like "motor_driver", "templates/clock_generator".
	
		-- CS: make sure the module is inside the current project directory.
		
		pac_generic_modules.insert (
			container	=> generic_modules,
			key			=> module_name,
			position	=> module_cursor,
			inserted	=> inserted);

		if to_string (module_name) /= untitled then
			if inserted then
				save_module (module_name, log_threshold + 1);
			else
				raise semantic_error_1 with
					"ERROR: Module " & enclose_in_quotes (to_string (module_name)) 
					& " already exists -> not created.";
			end if;
		end if;
		
	end create_module;


	
	procedure save_module (
		module_name		: in pac_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor := locate_module (module_name);

		use et_string_processing;
		use ada.directories;

		file_name : constant string := to_string (module_name) &
			latin_1.full_stop & module_file_name_extension;
		-- motor_driver.mod or templates/clock_generator.mod
	begin
		log (
			text	=> "saving module " & enclose_in_quotes (to_string (module_name)) & " ...",
			level	=> log_threshold);

		-- We save the module if it exists:
		if module_cursor /= pac_generic_modules.no_element then
	
			-- Make sure the module file is inside the current project directory.
			-- If the file is outside the project, issue a warning and do not save.
			if inside_project_directory (to_string (module_name)) then
			
				save_module (
					module_cursor	=> module_cursor,
					log_threshold 	=> log_threshold + 1);

			else
				log (WARNING, "module " & enclose_in_quotes (to_string (module_name)) &
					 " is outside the project and will not be saved !");
			end if;
			
		else
			log (WARNING, "module " & enclose_in_quotes (to_string (module_name)) &
					" does not exist !");
		end if;
		
	end save_module;



	
	procedure delete_module (
		module_name		: in pac_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor := locate_module (module_name);

		use et_string_processing;
		use ada.directories;

		file_name : constant string := append_extension (to_string (module_name));
		-- motor_driver.mod or templates/clock_generator.mod

		file_found : boolean := true;
		module_found : boolean := true;
	begin
		log (
			text	=> "deleting module " & enclose_in_quotes (to_string (module_name)) & " ...",
			level	=> log_threshold);

		-- Delete the module file in case it exists already:
		if exists (file_name) then
			delete_file (file_name);
		else
			file_found := false;
		end if;
		
		-- Remove the module from collection of generic modules:
		if module_cursor /= pac_generic_modules.no_element then
	
			---- Make sure the module file is inside the current project directory.
			---- If the file is outside the project, issue a warning and do not delete.
			--if inside_project_directory (to_string (module_name)) then
				
			pac_generic_modules.delete (
				container	=> generic_modules,
				position	=> module_cursor);

			--else
				--log (WARNING, "module " & enclose_in_quotes (to_string (module_name)) &
					 --" is outside the project and will not be deleted !");
			--end if;
		else
			module_found := false;
		end if;

		-- If neither the module nor the module file have been found, raise error:
		if not file_found and not module_found then
			raise semantic_error_1 with
				"ERROR: Neither module " & enclose_in_quotes (to_string (module_name)) 
				& " nor the file " & enclose_in_quotes (file_name) & " exist !";
		end if;
	end delete_module;



	
	function assembly_variant_exists (
		module		: in pac_generic_modules.cursor;
		variant		: in pac_assembly_variant_name.bounded_string) -- low_cost
		return boolean 
	is
		use pac_assembly_variants;

		result : boolean := false; -- to be returned

		procedure query_variants (
			module_name	: in pac_module_name.bounded_string;
			module		: in et_schematic.type_module) is
		begin
			result := contains (module.variants, variant);
		end;
		
	begin
		if pac_assembly_variant_name.length (variant) = 0 then
			result := true;
		else
			
			pac_generic_modules.query_element (
				position	=> module,
				process		=> query_variants'access);

		end if;
					
		return result;
	end assembly_variant_exists;



	

	function device_exists (
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		variant	: in pac_assembly_variant_name.bounded_string; -- low_cost				
		device	: in et_devices.type_device_name)
		return boolean 
	is
		result : boolean := false; -- to be returned

		
		procedure query_variants (
			module_name	: in pac_module_name.bounded_string;
			module		: in et_schematic.type_module) 
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
		device	: in et_devices.type_device_name)
		return pac_device_variants.cursor 
	is

		cursor : pac_device_variants.cursor; -- to be returned;
		
		procedure query_variants (
			module_name	: in pac_module_name.bounded_string;
			module		: in et_schematic.type_module) is
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

	

	

	function layout_rules_assigned (
		module	: in pac_generic_modules.cursor) -- the module like motor_driver
		return boolean is

		use et_design_rules.pac_file_name;
	begin
		if length (pac_generic_modules.element (module).rules.layout) > 0 then
			return true;
		else
			return false;
		end if;
	end layout_rules_assigned;

	

	function get_pcb_design_rules (
		module	: in pac_generic_modules.cursor) -- the module like motor_driver
		return et_design_rules.type_design_rules -- JLP_ML4_standard.dru
	is
		use et_design_rules;
	begin
		return get_rules (element (module).rules.layout); 
	end get_pcb_design_rules;

	

	function get_user_settings (
		module	: in pac_generic_modules.cursor) -- the module like motor_driver
		return et_pcb.type_user_settings
	is
		settings : et_pcb.type_user_settings; -- to be returned

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in et_schematic.type_module)
		is begin
			settings := module.board.user_settings;
		end;
	begin
		pac_generic_modules.query_element (module, query_module'access);
		
		return settings;
	end get_user_settings;

	
	function get_net_class (
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		class	: in et_pcb.pac_net_class_name.bounded_string) -- hi-voltage, si-critical
		return et_pcb.type_net_class
	is
		use et_pcb;
		
		result : type_net_class;

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in et_schematic.type_module)
		is
			use pac_net_classes;
			use pac_net_class_name;
		begin
			if class = net_class_name_default then
				null;
				-- CS load result with DRU settings (min track clearance, min track width, 
				-- min via drill size)
			else
				result := element (find (module.net_classes, class));
			end if;
		end query_module;
		
	begin
		query_element (module, query_module'access);
		
		return result;
	end get_net_class;


	function get_net_class (
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		net		: in et_nets.pac_nets.cursor) -- GND, RESET_N, ...
		return et_pcb.type_net_class
	is
		use et_pcb;
		result : type_net_class;

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in et_schematic.type_module)
		is
			use pac_net_classes;
			use pac_net_class_name;

			use et_schematic;
			use et_nets;
			use pac_nets;
		begin
			if net = et_nets.pac_nets.no_element then -- freetrack
				null;
				-- CS load result with DRU settings (min track clearance, min track width, 
				-- min via drill size)
			else
				if element (net).class = net_class_name_default then
					null;
					-- CS load result with DRU settings (min track clearance, min track width, 
					-- min via drill size)
				else
					result := element (find (module.net_classes, element (net).class));
				end if;
			end if;
		end query_module;
		
	begin
		query_element (module, query_module'access);
		return result;
	end get_net_class;
	
	
end et_project.modules;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
