------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         PROJECT CONFIGURATION                            --
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

with ada.characters;				use ada.characters;
with ada.characters.handling;		use ada.characters.handling;
with ada.strings; 					use ada.strings;
with ada.strings.fixed; 			use ada.strings.fixed;

with ada.exceptions;
with ada.directories;
with gnat.directory_operations;

with ada.containers;            	use ada.containers;
with et_general_rw;					use et_general_rw;

with et_conventions;
with et_time;						use et_time;


package body et_project.configuration is

	function to_string (section : in type_section_name) return string is
		len : positive := type_section_name'image (section)'length;
	begin
		return to_lower (type_section_name'image (section) (5..len));
	end to_string;

	function conventions_specified return boolean is begin
		if et_conventions.pac_file_name.length (project.rules.conventions) > 0 then
			return true;
		else
			return false;
		end if;
	end conventions_specified;
	
	procedure read_configuration (
		project_name 	: in pac_project_name.bounded_string; -- blood_sample_analyzer
		log_threshold 	: in type_log_level) 
		is separate;

	procedure write_configuration_header is 
		use et_general;
		use et_string_processing;
	begin
		-- write a nice header
		put_line (comment_mark & " " & system_name & " project configuration file");
		put_line (comment_mark & " " & date);
		put_line (comment_mark & " " & row_separator_double);
		new_line;
	end;

	procedure write_configuration_footer is
		use et_string_processing;
	begin
		-- write a nice footer
		new_line;
		put_line (comment_mark & " " & row_separator_double);
		put_line (comment_mark & " " & date);
		put_line (comment_mark & " project configuration file end");
		new_line;
	end;

	procedure save_configuration (
		project_name 	: in pac_project_name.bounded_string; -- blood_sample_analyzer
		log_threshold 	: in type_log_level) 
		is separate;

		
end et_project.configuration;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
