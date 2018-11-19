------------------------------------------------------------------------------
--                                                                          --
--                         SYSTEM ET PROJECT                                --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with ada.text_io;				use ada.text_io;

with ada.exceptions;
with ada.directories;

with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;

with et_general;
with et_coordinates;
with et_string_processing;
with et_libraries;
with et_export;
with et_import;
with et_schematic;


package body et_project is

	function to_string (project_name : in type_project_name.bounded_string) return string is
	begin
		return type_project_name.to_string (project_name);
	end to_string;
	
	function to_project_name (name : in string) return type_project_name.bounded_string is
	-- Converts the given string to type_project_name.
	begin
		return type_project_name.to_bounded_string (name);
	end to_project_name;

	function to_string (path : in type_et_project_path.bounded_string) return string is
	begin
		return type_et_project_path.to_string (path);
	end to_string;
	
	function to_sheet_name_text_size (size : in string) return type_sheet_name_text_size is
	-- Converts a string to type_sheet_name_text_size.
	begin
		return type_sheet_name_text_size'value (size);
	end to_sheet_name_text_size;

	function to_file_name_text_size (size : in string) return type_file_name_text_size is
	-- Converts a string to type_file_name_text_size.
	begin
		return type_file_name_text_size'value (size);
	end to_file_name_text_size;
	
	
-- NATIVE PROJECT

-- 	procedure create_libraries_directory_components (
-- 	-- Creates a directory where component libraries will live.
-- 	-- An already existing directory will be overwritten.
-- 	-- Sets the global library directory name so that subsequent write and read operations
-- 	-- access the right directory.
-- 		project_path	: in type_et_project_path.bounded_string;
-- 		log_threshold	: in et_string_processing.type_log_level) is
-- 		use et_general;
-- 		use ada.directories;
-- 		use et_string_processing;
-- 		use type_project_name;
-- 		use type_et_project_path;
-- 
-- 		path_length : positive :=  project_path_max + directory_libraries'length + directory_libraries_components'length + 2; -- incl. directory separators
-- 		package type_path is new generic_bounded_length (path_length);
-- 		use type_path;
-- 		path : type_path.bounded_string;
-- 	begin -- create_libraries_directory_components
-- 		path := to_bounded_string (compose (to_string (project_path), directory_libraries));
-- 		path := to_bounded_string (compose (to_string (path), directory_libraries_components));
-- 		
-- -- 		log ("creating in " & current_directory & " a new " & et_general.system_name & " libraries directory " 
-- -- 			 & to_string (path) & " ...",
-- -- 			log_threshold);
-- 		log ("directory for project wide libraries '" & directory_libraries & "' ...", log_threshold);
-- 
-- 		log_indentation_up;
-- 		
-- 		-- delete previous libraries directory
-- 		if exists (to_string (path)) then
-- 			delete_tree (to_string (path));
-- 		end if;
-- 		
-- 		-- create the libraries directory
-- 		create_path (to_string (path));
-- 
-- 		-- set the global library directory name
-- 		log ("setting global library directory name ...", log_threshold + 1);
-- 		component_libraries_directory_name := type_libraries_directory.to_bounded_string (to_string (path));
-- 	
-- 		log (" global library directory name is now " 
-- 			 & type_libraries_directory.to_string (component_libraries_directory_name), log_threshold + 2);
-- 
-- 		log_indentation_down;
-- 		
-- 		exception when event:
-- 			others => 
-- 				log (ada.exceptions.exception_message (event), console => true);
-- 				raise;
-- 		
-- 	end create_libraries_directory_components;


	procedure create_project_directory (
	-- Creates given project directory in the given project_path.
	-- Already existing projects in given project_path are overwritten.
	-- Sets the global project file name so that subsequent write and read operations
	-- know the right project file.
	-- Leaves the project file (global project_file_handle) open (closes it on exception).
		project_name	: in type_project_name.bounded_string;
		project_path	: in type_et_project_path.bounded_string;
		log_threshold	: in et_string_processing.type_log_level) is
		use et_general;
		use ada.directories;
		use et_string_processing;
		use type_project_name;
		use type_et_project_path;

		package type_path is new generic_bounded_length (project_name_max + project_path_max + 1); -- incl. directory separator
		use type_path;
		path : type_path.bounded_string := to_bounded_string (compose (to_string (project_path), to_string (project_name)));

		procedure create_library_subdirs (path : in string) is
		begin
			create_directory (compose (path, directory_libraries_devices));
			create_directory (compose (path, directory_libraries_symbols));
			create_directory (compose (path, directory_libraries_packages));			
			--log ("subdir " & compose (path, directory_libraries_devices));
		end create_library_subdirs;
			
	begin
-- 		log ("creating in " & current_directory & " a new " & et_general.system_name & " project directory " 
-- 			 & to_string (path) & " ...",
-- 			log_threshold);
		log ("project name '" & to_string (project_name) & "' ...", log_threshold);

		log_indentation_up;
		
		-- delete previous project directory
		if exists (to_string (path)) then
			delete_tree (to_string (path));
		end if;
		
		-- create project root directory
		create_path (to_string (path));

		
		-- create sub-directories for supplementary stuff:
		log ("creating subdirectories for supplementary stuff ...", log_threshold + 1);
		create_directory (compose (to_string (path), directory_libraries));
		create_library_subdirs (compose (to_string (path), directory_libraries));
		
		create_directory (compose (to_string (path), directory_dru));
		create_directory (compose (to_string (path), directory_cam));
		create_directory (compose (to_string (path), directory_net_classes));
		create_directory (compose (to_string (path), directory_settings));
		create_directory (compose (to_string (path), directory_reports));
		create_directory (compose (to_string (path), directory_documentation));
		create_directory (compose (to_string (path), directory_miscellaneous));

		-- set the global project_file_name
		log ("setting global project file name ...", log_threshold + 1);
		project_file_name := type_project_file_name.to_bounded_string (compose (
			containing_directory	=> to_string (path),
			name 					=> to_string (project_name),
			extension 				=> project_file_name_extension));
		
		log (" global project file name is now " & type_project_file_name.to_string (project_file_name), log_threshold + 2);

		-- create project file and write in it a header
		create (
			file => project_file_handle,
			mode => out_file, 
			name => type_project_file_name.to_string (project_file_name));

		put_line (project_file_handle, comment_mark & " " & system_name & " project file");
		put_line (project_file_handle, comment_mark & " " & date);
		put_line (project_file_handle, comment_mark & " project " & to_string (project_name));
		put_line (project_file_handle, comment_mark & " " & row_separator_double);
		new_line (project_file_handle);

		log_indentation_down;
		
		exception when event:
			others => 
				log (ada.exceptions.exception_message (event), console => true);
				close (project_file_handle);
				raise;
		
	end create_project_directory;

	
	procedure save_project (log_threshold : in et_string_processing.type_log_level) is
	-- Saves the schematic and layout data in project file (project_file_handle).
		use et_string_processing;

		procedure write_project_footer is
		-- writes a nice footer in the project file and closes it.
		begin
			log ("closing project file ...", log_threshold + 1);
			put_line (comment_mark & " " & row_separator_double);
			put_line (comment_mark & " " & date);
			put_line (comment_mark & " project " & to_string (project_name) & " file end");
			new_line;
		end write_project_footer;

	begin
		log ("saving project ...", log_threshold);
		set_output (project_file_handle);


		-- CS write content

		
		-- close native project file
		write_project_footer;

		set_output (standard_output);		
		close (project_file_handle);

		exception when event:
			others => 
				log (ada.exceptions.exception_message (event), console => true);
				close (project_file_handle);
				raise;

	end save_project;


	procedure open_project (log_threshold : in et_string_processing.type_log_level) is
	-- Opens and reads the schematic and layout data present in project file (project_file_handle).
		use et_string_processing;
		use ada.directories;

		line : et_string_processing.type_fields_of_line;
	begin
		log ("opening project '" & to_string (project_name) & "' ...", log_threshold, console => true);
		log_indentation_up;

		--log ("directory " & base_name (to_string (project_name)));
		
		project_file_name := type_project_file_name.to_bounded_string (compose (
			containing_directory	=> to_string (project_name),
			name 					=> base_name (to_string (project_name)),
			extension 				=> project_file_name_extension));

		log ("project file is " & type_project_file_name.to_string (project_file_name), log_threshold + 1);
		
		if exists (type_project_file_name.to_string (project_file_name)) then

			-- open project file
			open (
				file => project_file_handle,
				mode => in_file, 
				name => type_project_file_name.to_string (project_file_name));

			set_input (project_file_handle);

			-- read the file line by line
			while not end_of_file loop
				line := et_string_processing.read_line (
					line 			=> get_line,
					number			=> ada.text_io.line (current_input),
					comment_mark 	=> et_string_processing.comment_mark,
					delimiter_wrap	=> true, -- if connector purpose is given in quotations
					ifs 			=> latin_1.space); -- fields are separated by space

				-- we are interested in lines that contain something. emtpy lines are skipped:
				if field_count (line) > 0 then
					put_line (standard_output, to_string (line));

					-- CS read content
				end if;
			end loop;

			
			set_input (standard_input);
			close (project_file_handle);

		else
			log_indentation_reset;
			log (message_error & "Native project file " & type_project_file_name.to_string (project_file_name) 
				 & " does not exist !", console => true);
			--log ("Example to open the native project by specifying the project directory:", console => true);			log ("Example to open the native project by specifying the project directory:", console => true);
			--log (system_name_cmd_line & "openetample to open the native project by specifying the project directory:", console => true);
			raise constraint_error;
		end if;
		log_indentation_down;
		
		exception when event:
			others => 
				log (ada.exceptions.exception_message (event), console => true);
				if is_open (project_file_handle) then
					close (project_file_handle);
				end if;
				raise;

	end open_project;
	
-- 	procedure write_component_libraries (log_threshold : in et_string_processing.type_log_level) is
-- 	-- Writes the ET native libraries in libraries_directory_name.
-- 	-- Creates sub-directories for library groups (like active, passive, misc, ...)
-- 	-- CS: currently only one group is supported. See et_libraries.library_group .
-- 	-- Each group is further-on composed of sub-directories for symbols, packages and devices.
-- 		use et_string_processing;
-- 		use ada.directories;
-- 		-- The group may be a path like "../../lbr" or "../passive". 
-- 		-- We are interested in the simple name like "lbr" or "passive".
-- 		lib_group_length : positive := simple_name (et_libraries.to_string (et_libraries.library_group))'length;
-- 
-- 		-- The path where the group is to be stored is composed of the libraries_directory_name and the group name.
-- 		path_length : positive := type_libraries_directory.length (component_libraries_directory_name) + lib_group_length + 1; -- incl. directory separator
-- 		package type_path is new generic_bounded_length (path_length);
-- 		use type_path;
-- 		path : type_path.bounded_string;
-- 
-- -- 		procedure w
-- -- 		device_file_handle : ada.text_io.file_type;
-- 	begin -- write_component_libraries
-- 		
-- 		-- set the path of the library group:
-- 		path := to_bounded_string (
-- 				  compose (
-- 					type_libraries_directory.to_string (component_libraries_directory_name), -- "components"
-- 					simple_name (et_libraries.to_string (et_libraries.library_group)) -- "passive"
-- 					)
-- 				);
-- 
-- 		-- create library group (CS or lots of groups in the future, see comments above)
-- 		create_path (to_string (path));
-- 
-- 		-- create sub-directories for symbols, packages and devices.
-- 		create_path (compose (to_string (path), directory_libraries_components_sym));
-- 		create_path (compose (to_string (path), directory_libraries_components_pac));
-- 		create_path (compose (to_string (path), directory_libraries_components_dev));
-- 		
-- 		log ("writing native libraries in " & to_string (path) & " ...", log_threshold);
-- 
-- 		
-- 		
-- 		exception when event:
-- 			others => 
-- 				log (ada.exceptions.exception_message (event), console => true);
-- 				raise;
-- 
-- 	end write_component_libraries;
	
end et_project;
	
-- Soli Deo Gloria
