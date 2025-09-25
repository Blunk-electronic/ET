------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           MODULE OPERATIONS                              --
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
with et_directory_and_file_ops;
with et_string_processing;		use et_string_processing;
with et_system_info;
with et_export;
with et_text;					use et_text;
with et_general_rw;				use et_general_rw;
with et_meta;
with et_exceptions;				use et_exceptions;
with et_project;
with et_module_write;			use et_module_write;


package body et_module_ops is

	use pac_generic_modules;



	procedure create_module (
		module_name		: in pac_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor;
		inserted : boolean;

		use et_directory_and_file_ops;
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
	


	procedure delete_module (
		module_name		: in pac_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor := locate_module (module_name);

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


	
	


	
	procedure save_module (
		module_name		: in pac_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor := locate_module (module_name);

		use ada.directories;
		use et_project;

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

	
	
end et_module_ops;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
