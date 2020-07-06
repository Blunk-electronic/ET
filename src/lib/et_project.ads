------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               PROJECT                                    --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

with et_general;				use et_general;
with et_string_processing;


package et_project is
	
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
-- 	directory_dru			: constant string := "design_rules";
-- 	directory_cam			: constant string := "CAM";
-- 	directory_net_classes	: constant string := "net_classes";
	directory_templates		: constant string := "templates";		
-- 	directory_settings		: constant string := "settings";
	directory_reports		: constant string := "reports";
	directory_documentation	: constant string := "documentation";
	directory_miscellaneous	: constant string := "miscellaneous";		


-- KEYWORDS
	keyword_default			: constant string := "default";

	
	-- This is the root directory where all projects live:
	projects_root_dir_length : constant natural := 100;
	package type_projects_root_dir is new generic_bounded_length (projects_root_dir_length);
	projects_root_dir : type_projects_root_dir.bounded_string;
	
	
	project_name_max : constant natural := 100;
	package type_project_name is new generic_bounded_length (project_name_max);
	
	function to_string (project_name : in type_project_name.bounded_string) return string;
	function to_project_name (name : in string) return type_project_name.bounded_string;
	
	project_path_max : constant natural := 200;
	package type_et_project_path is new generic_bounded_length (project_path_max);
	function to_string (path : in type_et_project_path.bounded_string) return string;
	function to_project_path (path : in string) return type_et_project_path.bounded_string;
	
	-- Creates given project directory in the given project_path.
	-- Creates a default rig configuration file.										   
	-- Already existing projects in given project_path are overwritten.
	procedure create_project_directory (
		module_name		: in type_module_name.bounded_string;		-- motor_driver
		project_name	: in type_project_name.bounded_string;		-- blood_sample_analyzer
		project_path	: in type_et_project_path.bounded_string;	-- /home/user/et_projects
		log_threshold	: in et_string_processing.type_log_level);

	-- Creates a bare project (without any configuration files).
	-- Already existing projects in given path are overwritten.
	-- Sets the global project file name so that subsequent write and read operations
	-- know the right project file.
	procedure create_project_directory_bare (
		project_name	: in type_project_name.bounded_string;		-- blood_sample_analyzer
		project_path	: in type_et_project_path.bounded_string; 	-- /home/user/et_projects
		log_threshold	: in et_string_processing.type_log_level);

	-- Saves the current project under the given destination like /home/user/ecad/blood_sample_analyzer.
	-- Saves generic modules that are inside the project.
	-- Saves the project configuration.
	-- Saves the rig configurations.
	procedure save_project (
		destination		: in type_project_name.bounded_string;		-- blood_sample_analyzer
		log_threshold 	: in et_string_processing.type_log_level);
	
end et_project;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
