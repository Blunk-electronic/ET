------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            PROJECT NAME                                  --
--                                                                          --
--                              B o d y                                     --
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
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.directories;
with gnat.directory_operations;

with et_directory_and_file_ops;
with et_string_processing;		use et_string_processing;


package body et_project_name is

	
	function get_length (
		project_name : in pac_project_name.bounded_string) 
		return natural
	is begin
		return natural (length (project_name));
	end get_length;



	
	function to_string (
		project_name : in pac_project_name.bounded_string) 
		return string 
	is begin
		return pac_project_name.to_string (project_name);
	end to_string;

	
	function to_project_name (
		name : in string) 
		return pac_project_name.bounded_string 
	is begin
		return pac_project_name.to_bounded_string (name);
	end to_project_name;




	procedure validate_project_name (
		project_name	: in pac_project_name.bounded_string;
		log_threshold 	: in type_log_level)
	is
		use ada.directories;
		use gnat.directory_operations;
		use et_directory_and_file_ops;
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

	
	

end et_project_name;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
