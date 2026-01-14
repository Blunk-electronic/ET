------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         PROJECT CONFIGURATION                            --
--                                                                          --
--                               S p e c                                    --
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

--   For correct displaying set tab with in your editor to 4.

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
-- - separate package for conf. file name


with et_conventions;

package et_project.configuration is

	file_length_max : constant positive := 100;
	package pac_file_name is new generic_bounded_length (file_length_max);
	use pac_file_name;
	
	file_extension : constant string := "prj";
		



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
		log_threshold 	: in type_log_level);

	
	procedure write_configuration_header;
	procedure write_configuration_footer;


	
	-- Saves the project configuration in the project configuration file.
	-- The current working directory is assumed to be the parent directory
	-- of the current project. Call this procedure when saving a whole project.
	procedure save_configuration (
		project_name 	: in pac_project_name.bounded_string; -- blood_sample_analyzer
-- 		project_path	: in type_et_project_path.bounded_string; 	-- /home/user/et_projects
		log_threshold 	: in type_log_level);


	
end et_project.configuration;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
