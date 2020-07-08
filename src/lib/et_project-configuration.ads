------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         PROJECT CONFIGURATION                            --
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

with et_conventions;

package et_project.configuration is

	file_length_max : constant positive := 100;
	package pac_file_name is new generic_bounded_length (file_length_max);
	use pac_file_name;
	
	file_extension : constant string := "prj";
	
	keyword_conventions : constant string := "conventions";
	
	section_rules					: constant string := "[RULES";
	section_environment_variables	: constant string := "[ENVIRONMENT_VARIABLES";
	
	type type_section_name is (
		SEC_INIT,
		SEC_RULES,
		SEC_ENVIRONMENT_VARIABLES -- CS not used currently
		-- CS SEC_LAST_OPENED
		);

	function to_string (section : in type_section_name) return string;

	type type_rules is record
		conventions	: et_conventions.pac_file_name.bounded_string; -- conventions.txt
	end record;
	
	type type_configuration is record
		rules	: type_rules;
		-- CS evironment_variables 
		-- CS last_opened
	end record;

	-- Here we store the configuration of the current project:
	project : type_configuration;

	-- Returns true if a conventions file has been specified for the project:
	function conventions_specified return boolean;
	
	-- Reads the project configuration file.
	-- The current working directory is assumed to be the project directory:
	procedure read_configuration (
		project_name 	: in pac_project_name.bounded_string; -- blood_sample_analyzer
		log_threshold 	: in et_string_processing.type_log_level);

	procedure write_configuration_header;
	procedure write_configuration_footer;

	-- Saves the project configuration in the project configuration file.
	-- The current working directory is assumed to be the parent directory
	-- of the current project. Call this procedure when saving a whole project.
	procedure save_configuration (
-- 		project_name 	: in pac_project_name.bounded_string; -- blood_sample_analyzer
-- 		project_path	: in type_et_project_path.bounded_string; 	-- /home/user/et_projects
		log_threshold 	: in et_string_processing.type_log_level);
	
end et_project.configuration;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
