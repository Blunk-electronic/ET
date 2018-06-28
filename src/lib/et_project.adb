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
with et_export;
with et_import;


package body et_project is

-- NATIVE PROJECT

	procedure create_libraries_directory (
	-- Creates a directory where libraries will live.
	-- An already existing directory will be overwritten.
	-- Sets the global library directory name so that subsequent write and read operations
	-- access the right directory.
		project_path	: in type_et_project_path.bounded_string;
		log_threshold	: in et_string_processing.type_log_level) is
		use et_general;
		use ada.directories;
		use et_string_processing;
		use type_et_project_name;
		use type_et_project_path;

		package type_path is new generic_bounded_length (project_name_max + project_path_max + 1); -- incl. directory separator
		use type_path;
		path : type_path.bounded_string := to_bounded_string (compose (to_string (project_path), directory_libraries));
	begin
		log ("creating in " & current_directory & " a new " & et_general.system_name & " libraries directory " 
			 & to_string (path) & " ...",
			log_threshold);

		-- create the libraries directory
		create_path (to_string (path));

		-- set the global library directory name
		log ("setting global library directory name ...", log_threshold + 1);
		libraries_directory_name := type_libraries_directory.to_bounded_string (to_string (path));
	
		log ("global library directory name is now " & type_libraries_directory.to_string (libraries_directory_name), log_threshold + 1);
		
		exception when event:
			others => 
				log (ada.exceptions.exception_message (event), console => true);
				raise;
		
	end create_libraries_directory;


	procedure create_project_directory (
	-- Creates given project directory in the given project_path.
	-- Already existing projects in given project_path are overwritten.
	-- Sets the global project file name so that subsequent write and read operations
	-- know the right project file.
		project_name	: in type_et_project_name.bounded_string;
		project_path	: in type_et_project_path.bounded_string;
		log_threshold	: in et_string_processing.type_log_level) is
		use et_general;
		use ada.directories;
		use et_string_processing;
		use type_et_project_name;
		use type_et_project_path;

		package type_path is new generic_bounded_length (project_name_max + project_path_max + 1); -- incl. directory separator
		use type_path;
		path : type_path.bounded_string := to_bounded_string (compose (to_string (project_path), to_string (project_name)));
	begin
		log ("creating in " & current_directory & " a new " & et_general.system_name & " project directory " 
			 & to_string (path) & " ...",
			log_threshold);

		-- delete previous project directory
		if exists (to_string (project_path)) then
			delete_tree (to_string (project_path));
		end if;
		
		-- create project root directory
		create_path (to_string (path));

		
		-- create sub-directories for supplementary stuff:
		log ("creating subdirectories for supplementary stuff ...", log_threshold + 1);
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
			containing_directory => to_string (path),
			name => to_string (project_name),
			extension => project_file_name_extension));
		
		log ("global project file name is now " & type_project_file_name.to_string (project_file_name), log_threshold + 1);

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

		close (project_file_handle);
		
		exception when event:
			others => 
				log (ada.exceptions.exception_message (event), console => true);
				raise;
		
	end create_project_directory;

	

	
end et_project;
	
-- Soli Deo Gloria
