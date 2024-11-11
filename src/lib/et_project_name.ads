------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            PROJECT NAME                                  --
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


with ada.strings.bounded;       use ada.strings.bounded;

with et_logging;				use et_logging;

package et_project_name is


	project_name_max : constant natural := 100;

	package pac_project_name is new generic_bounded_length (project_name_max);

	use pac_project_name;

	

	function get_length (
		project_name : in pac_project_name.bounded_string) 
		return natural;	

	

	function to_string (
		project_name : in pac_project_name.bounded_string) 
		return string;


	
	function to_project_name (
		name : in string) 
		return pac_project_name.bounded_string;



	-- Tests whether the project name is a child directory 
	-- of the current working directory.
	-- Raises constraint error otherwise.
	procedure validate_project_name (
		project_name	: in pac_project_name.bounded_string; -- blood_sample_analyzer
		log_threshold 	: in type_log_level);

	

end et_project_name;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
