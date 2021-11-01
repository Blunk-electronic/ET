------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               PROJECT                                    --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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

with ada.strings.fixed; 		use ada.strings.fixed;
with ada.exceptions;
with ada.directories;
with gnat.directory_operations;

with et_export;
with et_general_rw;				use et_general_rw;

with et_project.modules;
with et_project.rigs;
with et_project.configuration;
	
package body et_project is
	
	function to_string (project_name : in pac_project_name.bounded_string) return string is
	begin
		return pac_project_name.to_string (project_name);
	end to_string;
	
	function to_project_name (name : in string) return pac_project_name.bounded_string is
	begin
		return pac_project_name.to_bounded_string (name);
	end to_project_name;

	function to_string (path : in type_et_project_path.bounded_string) return string is begin
		return type_et_project_path.to_string (path);
	end to_string;

	function to_project_path (path : in string) return type_et_project_path.bounded_string is begin
		return type_et_project_path.to_bounded_string (path);
	end to_project_path;

	procedure validate_project_name (
		project_name	: in pac_project_name.bounded_string;		-- blood_sample_analyzer
		log_threshold 	: in type_log_level)
	is
		use et_string_processing;
		use ada.directories;
		use gnat.directory_operations;
		expanded_name : constant string := expand (to_string (project_name));
	begin
		-- The project must be a directory inside the current directory.
		-- The project name must not be something like "ecad/et_projects/blood_sample_analyzer".
		-- The easiest way to check that is to detect directory separators ("/").
		if index (expanded_name, to_set (dir_separator)) = 0 then -- no separators
			if kind (expanded_name) = DIRECTORY then -- is a directory
				null;
			else
				log (ERROR, "The project must be a directory !", console => true);
				raise constraint_error;
			end if;
		else
			log (ERROR, "The project must be a child directory !", console => true);
			raise constraint_error;
		end if;
	end validate_project_name;
	
	procedure create_supplementary_directories (
		path			: in string;
		log_threshold	: in type_log_level) 
	is
		use et_string_processing;
		use ada.directories;
		use gnat.directory_operations;

		procedure create_library_subdirs (path : in string) is
		begin
			create_directory (compose (path, directory_libraries_devices));
			create_directory (compose (path, directory_libraries_symbols));
			create_directory (compose (path, directory_libraries_packages));			
			--log ("subdir " & compose (path, directory_libraries_devices));
		end create_library_subdirs;

		use et_export;
		
	begin -- create_supplementary_directories
		log (text => "creating subdirectories for supplementary stuff ...", level => log_threshold);
		create_directory (compose (path, directory_libraries));
		create_library_subdirs (compose (path, directory_libraries));
		
		create_directory (compose (path, directory_dru));
		--create_directory (compose (path, directory_cam));
		--create_directory (compose (path, directory_net_classes));
		create_directory (compose (path, directory_templates));
		create_directory (compose (path, directory_export));
		make_dir (path & dir_separator & directory_export & dir_separator & directory_cam);
		make_dir (path & dir_separator & directory_export & dir_separator & directory_cam & dir_separator & directory_bom);
		make_dir (path & dir_separator & directory_export & dir_separator & directory_cam & dir_separator & directory_netlists);
		make_dir (path & dir_separator & directory_export & dir_separator & directory_cam & dir_separator & directory_pick_and_place);
		
		--create_directory (compose (path, directory_settings));
		create_directory (compose (path, directory_reports));
		create_directory (compose (path, directory_documentation));
		create_directory (compose (path, directory_miscellaneous));
	end create_supplementary_directories;

	procedure create_project_directory (
		project_name	: in pac_project_name.bounded_string;		-- blood_sample_analyzer
		module_name		: in pac_module_name.bounded_string := to_module_name (""); -- motor_driver
		log_threshold	: in type_log_level) is
		use et_general;
		use ada.directories;
		use et_string_processing;
		use pac_project_name;
		use type_et_project_path;

-- 		use modules;
		use modules.pac_generic_modules;

		module_cursor : modules.pac_generic_modules.cursor;

		procedure create_project_configuration is
		-- create the project configuration file
			file_handle : ada.text_io.file_type;

			use et_project.configuration;
			prj_conf_file : pac_file_name.bounded_string; -- led_matrix.prj
		begin
			log (text => "creating project configuration file ...", level => log_threshold + 1);

			-- compose the full file name			
			prj_conf_file := pac_file_name.to_bounded_string (compose (
				containing_directory	=> to_string (project_name),
				name 					=> to_string (project_name),
				extension 				=> file_extension));

			-- create the file
			create (
				file => file_handle,
				mode => out_file, 
				name => pac_file_name.to_string (prj_conf_file));

			set_output (file_handle);

			write_configuration_header;
			
			-- section rules
			section_mark (section_rules, HEADER);
-- 			write (keyword => keyword_generic_name, parameters => to_string (project_name));
-- 			write (keyword => keyword_instance_name, parameters => to_string (project_name));
			section_mark (section_rules, FOOTER);

			-- CS other sections
			
			-- close the file
			write_configuration_footer;
			set_output (standard_output);
			close (file_handle);
			
		end create_project_configuration;
		
		procedure create_module_file is
			-- backup the current working directory
			previous_directory : constant string := current_directory;
			use pac_module_name;
		begin
			-- change into project directory
			set_directory (to_string (project_name));

			-- There are no modules yet. Create an empty generic module.
			-- If no module name given then the module will be named after the project.
			if length (module_name) > 0 then

				modules.create_module (
					module_name		=> module_name, -- as given module name
					log_threshold	=> log_threshold + 1);

			else

				modules.create_module (
					module_name		=> to_module_name (to_string (project_name)), -- name as project
					log_threshold	=> log_threshold + 1);

			end if;
				
			-- Save the single and first module:
			module_cursor := modules.generic_modules.first;

			modules.save_module (
				module_cursor	=> module_cursor,
				log_threshold	=> log_threshold + 1);
			
			-- restore working directory
			set_directory (previous_directory);
		end create_module_file;

		-- Creates an example rig configuration file.
		procedure create_rig_configuration is
			file_handle : ada.text_io.file_type;

			use et_project.rigs;
			rig_conf_file : pac_file_name.bounded_string; -- led_matrix.conf

			example_instance_name : constant string := "MOD1";
		begin
			log (text => "creating default rig configuration file ...", level => log_threshold + 1);

			-- compose the full file name			
			rig_conf_file := pac_file_name.to_bounded_string (compose (
				containing_directory	=> to_string (project_name),
				name 					=> to_string (project_name),
				extension 				=> file_extension));

			-- create the file
			create (
				file => file_handle,
				mode => out_file, 
				name => pac_file_name.to_string (rig_conf_file));

			set_output (file_handle);

			write_rig_configuration_header;
			
			-- section module instances
			section_mark (section_module_instances, HEADER);

			section_mark (section_module, HEADER);			
			write (keyword => keyword_generic_name, parameters => to_string (key (module_cursor)));
			write (keyword => keyword_instance_name, parameters => example_instance_name);
			section_mark (section_module, FOOTER);
			
			-- CS In the future, write other things here that characterize the instance.
			section_mark (section_module_instances, FOOTER);


			-- section connectors
			new_line;
			section_mark (section_module_connections, HEADER);

			section_mark (section_connector, HEADER);			
			write (keyword => comment_mark & " " & keyword_instance_A, parameters => example_instance_name);
			write (keyword => comment_mark & " " & keyword_purpose_A, wrap => true, parameters => "power_in");
			new_line;
			write (keyword => comment_mark & " " & keyword_instance_B, parameters => "power_supply");
			write (keyword => comment_mark & " " & keyword_purpose_B, wrap => true, parameters => "power_out");
			new_line;
			write (keyword => comment_mark & " " & keyword_net_comparator, parameters => "on"); -- CS image of enum type
			write (keyword => comment_mark & " " & keyword_net_comparator_warn_only, parameters => "on"); -- CS image of enum type
			section_mark (section_connector, FOOTER);			
			
			-- CS In the future, write other things here that characterize the board to board connection
			section_mark (section_module_connections, FOOTER);

			-- close the file
			write_rig_configuration_footer;
			set_output (standard_output);
			close (file_handle);
			
		end create_rig_configuration;
		
	begin -- create_project_directory
		log (text => "creating native project " & enclose_in_quotes (to_string (project_name)) &
			 " ...", level => log_threshold);

		-- CS validate_project_name
		
		log_indentation_up;
		
		-- delete previous project directory
		if exists (to_string (project_name)) then
			delete_tree (to_string (project_name));
		end if;
		
		-- create project root directory
		create_path (to_string (project_name));
		
		create_supplementary_directories (to_string (project_name), log_threshold + 1);

		create_project_configuration;
		
		create_module_file;
		
		create_rig_configuration; -- must come after create_module_file !
		
		log_indentation_down;
		
		exception when event:
			others => 
				log (text => ada.exceptions.exception_message (event), console => true);
				raise;
		
	end create_project_directory;
		
	procedure create_project_directory_bare (
		project_name	: in pac_project_name.bounded_string;		-- blood_sample_analyzer
		log_threshold	: in type_log_level) is
		
		use et_general;
		use ada.directories;
		use et_string_processing;
		use pac_project_name;

		path : constant string := to_string (project_name);
		
		procedure create_library_subdirs is
		begin
			create_directory (compose (path, directory_libraries_devices));
			create_directory (compose (path, directory_libraries_symbols));
			create_directory (compose (path, directory_libraries_packages));			
		end create_library_subdirs;

	begin -- create_project_directory_bare
		log (text => "creating bare native project " & enclose_in_quotes (path) & " ...",
			 level => log_threshold);
		log_indentation_up;

		-- CS validate_project_name
		
		-- delete previous project directory
		if exists (path) then
			delete_tree (path);
		end if;
		
		-- create project directory
		create_path (path);

		create_supplementary_directories (path, log_threshold + 1);

		log_indentation_down;
		
		exception when event:
			others => 
				log (text => ada.exceptions.exception_message (event), console => true);
				raise;
		
	end create_project_directory_bare;

	procedure validate_project (
		project_name	: in pac_project_name.bounded_string;
		log_threshold 	: in type_log_level)
	is
		use et_string_processing;
		use ada.directories;
	begin
		if exists (to_string (project_name)) then
			null;
			-- CS test if it is a directory
			-- CS test if it contains the project file
		else
			log (ERROR, "Native project " & to_string (project_name) 
					& " does not exist !", console => true);
			raise constraint_error;
		end if;
	end validate_project;
						
	procedure open_project (
		project_name	: in pac_project_name.bounded_string;		-- blood_sample_analyzer
		log_threshold 	: in type_log_level)
	is
		use et_string_processing;
		use ada.directories;
		
		-- We need a backup of the current working directory. When this procedure finishes,
		-- the working directory must be restored.
		current_working_directory : constant string := current_directory;
	begin
		validate_project (project_name, log_threshold + 1);
	
		-- set global project name
		current_active_project := project_name;

		-- change in project directory
		set_directory (to_string (project_name));
		
		-- read project configuration file
		configuration.read_configuration (project_name, log_threshold + 1);
		
		-- read the rig configurations and generic modules:
		rigs.read_rigs (log_threshold + 1);
		
		-- Restore working directory.
		set_directory (current_working_directory);
		
		exception when event:
			others => 
				-- Restore working directory.
				set_directory (current_working_directory);
				raise;
		
	end open_project;



	function inside_project_directory (file_name : in string) return boolean is
	-- Tests whether the given file name indicates whether the file is inside the project directory.
	-- CS: This works on Linux only. Implementation should be OS independent !

	-- 1. The expanded file_name may be a relative path to a directory outside the project.
	--    In this case the expanded path starts with ../ and the return will be false.
	-- 2. The expanded file_name may be a relative path to a subdirectory inside the project.
	--    In this case the return would be true.
	-- 3. The expanded file_name may be an absolute path pointing elsewhere in the filesystem.
	--    In this case the expanded path starts with / and the return will be false.
		use gnat.directory_operations;
		expanded_name : constant string := expand (file_name);
	begin
		if 	index (expanded_name, to_set (dir_separator)) = 1 or -- absolute path
			index (expanded_name, ".." & dir_separator) = 1 then -- relative path -> outside the project
			return false;
		else
			return true;
		end if;
	end inside_project_directory;

	
	procedure save_project (
		destination		: in pac_project_name.bounded_string; -- blood_sample_analyzer_experimental
		log_threshold 	: in type_log_level) 
	is
		use et_project.rigs;
		use rigs.pac_rigs;		

		use et_string_processing;
		use ada.directories;
		use et_project.modules;
		use et_project.modules.pac_generic_modules;

		-- We need a backup of the current working directory. When this procedure finishes,
		-- the working directory must be restored.
		current_working_directory : constant string := current_directory;

		-- break down destination into path and project name:
		path : type_et_project_path.bounded_string := to_project_path (containing_directory (to_string (destination)));
		name : pac_project_name.bounded_string := to_project_name (simple_name (to_string (destination)));
		
		procedure query_modules (module_cursor : in pac_generic_modules.cursor) is
		-- Saves a project internal module or a submodule (indicated by module_cursor).
			module_name : pac_module_name.bounded_string := key (module_cursor); -- motor_driver
		begin
			log_indentation_up;

			-- Only those modules inside the project will be saved in files:
			if inside_project_directory (to_string (module_name)) then
				log (text => "saving module " & enclose_in_quotes (to_string (module_name)),
					 level => log_threshold + 1);
				
				log_indentation_up;

				save_module (
					module_cursor	=> module_cursor,
					log_threshold 	=> log_threshold + 2);
				
				-- FOR TESTING ONLY
				-- save libraries (et_libraries.devices and et_pcb.packages)
	-- 			save_libraries (
	-- 				project_name	=> name, -- blood_sample_analyzer
	-- 				project_path	=> path, -- /home/user/ecad
	-- 				log_threshold 	=> log_threshold + 1);
				
				log_indentation_down;
			end if;
			
			log_indentation_down;			
		end query_modules;

		procedure query_rig_configuration (rig_cursor : in rigs.pac_rigs.cursor) is
			use pac_file_name;
			rig_name : pac_file_name.bounded_string := key (rig_cursor);
		begin
			log_indentation_up;
			log (text => "rig configuration " & to_string (rig_name), level => log_threshold + 1);
			log_indentation_up;
			
			save_rig_configuration (
				rig_cursor		=> rig_cursor,
				log_threshold 	=> log_threshold + 1);
			
			log_indentation_down;
			log_indentation_down;
		end query_rig_configuration;

		procedure copy_design_rules is
		begin
			log (text => "copying pcb design rules ...", level => log_threshold + 1);
			log_indentation_up;
			-- CS
			--copy_file ("test", "test2");
			log_indentation_down;
		end copy_design_rules;
		
	begin -- save_project
		log (text => row_separator_double, level => log_threshold);
		log (text => "saving project as " & to_string (destination) & " ...",
			 level => log_threshold, console => true);
		log_indentation_up;

		-- create project directory in current working directory:
		create_project_directory_bare (
			project_name	=> name, -- blood_sample_analyzer_experimental
			log_threshold 	=> log_threshold + 2);

		-- change into project directory:
		set_directory (to_string (name));
		
		-- save modules
		iterate (generic_modules, query_modules'access);

		-- save rig configuration files
		pac_rigs.iterate (et_project.rigs.rigs, query_rig_configuration'access);

		-- save project configuration
		configuration.save_configuration (
			project_name	=> name, -- blood_sample_analyzer_experimental
			log_threshold 	=> log_threshold + 1);

		copy_design_rules;
		
		-- CS copy scripts (use copy operations)

		set_directory (current_working_directory);
		
		log_indentation_down;
	end save_project;

end et_project;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
