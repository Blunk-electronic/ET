------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               PROJECT                                    --
--                                                                          --
--                               S p e c                                    --
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

with ada.text_io;				use ada.text_io;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;

with et_net_names;				use et_net_names;
with et_logging;				use et_logging;
with et_module_names;			use et_module_names;
with et_project_name;			use et_project_name;


package et_project is


-- DIRECTORY NAMES:
	
	-- after importing a foreign project, native ET projects are created here:
	directory_import		: constant string := "et_import";

	-- native ET projects live in a parent folder
	directory_projects		: constant string := "projects";

	-- native library directories
	directory_libraries 				: constant string := "libraries";	
	directory_libraries_devices			: constant string := "devices";
	directory_libraries_symbols			: constant string := "symbols";
	directory_libraries_packages		: constant string := "packages";	

	directory_libraries_schemlets		: constant string := "schemlets";
	
	-- supplementary stuff of a project
	directory_dru			: constant string := "pcb-design-rules";
-- 	directory_cam			: constant string := "CAM";
-- 	directory_net_classes	: constant string := "net_classes";
	directory_templates		: constant string := "templates";		
-- 	directory_settings		: constant string := "settings";
	directory_reports		: constant string := "reports";
	directory_documentation	: constant string := "documentation";
	directory_miscellaneous	: constant string := "miscellaneous";		




	
-- PROJECTS ROOT DIRECTORY:
	
	-- This is the root directory where all projects live:
	projects_root_dir_length : constant natural := 100;
	
	package pac_root_directory is new generic_bounded_length (projects_root_dir_length);
	
	projects_root_dir : pac_root_directory.bounded_string;
	
	


	
-- PROJECT PATH:
	
	project_path_max : constant natural := 200;
	
	package pac_project_path is new generic_bounded_length (project_path_max);
	
	function to_string (path : in pac_project_path.bounded_string) return string;
	
	function to_project_path (path : in string) return pac_project_path.bounded_string;



	
	
-- ACTIVE PROJECT:
	
	-- Here we store the name of the currently open project:
	active_project : pac_project_name.bounded_string;




	

	
	-- Creates given project directory.
	-- Creates a default rig configuration file.										   
	-- Already existing projects in given project_path are overwritten.
	procedure create_project_directory (
		project_name	: in pac_project_name.bounded_string;		-- blood_sample_analyzer
		module_name		: in pac_module_name.bounded_string := to_module_name (""); -- motor_driver
		log_threshold	: in type_log_level);


	
	-- Creates a bare project (without any configuration files).
	-- Already existing projects in given path are overwritten.
	procedure create_project_directory_bare (
		project_name	: in pac_project_name.bounded_string;		-- blood_sample_analyzer
		log_threshold	: in type_log_level);


	
	-- Opens the project with the given name. Assumes the project to exist
	-- in the current directory.
	-- Assigns to the global variable "current_project" the given project_name.
	procedure open_project (
		project_name	: in pac_project_name.bounded_string;		-- blood_sample_analyzer
		log_threshold 	: in type_log_level);


	
	-- Files outside the project directory MUST NOT be saved. To test a file for its location
	-- this function shall be used.
	function inside_project_directory (file_name : in string) return boolean;


	
	-- Saves the current project under the given destination like blood_sample_analyzer_experimental.
	-- Saves generic modules that are inside the project. Modules outside the project directory are
	-- untouched.
	-- Saves the project configuration.
	-- Saves the rigs (*.rig). CS: copy them as we do with scripts ?
	-- Scripts and design rules are regarded as source code and are therefore copied as they are.
	-- This way comments will be preserved.
	-- Copies the script and design rule files to the given destination.
	procedure save_project (
		destination		: in pac_project_name.bounded_string; -- blood_sample_analyzer_experimental
		log_threshold 	: in type_log_level);


	
end et_project;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
