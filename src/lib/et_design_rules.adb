------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           PCB DESIGN RULES                               --
--                                                                          --
--                               B o d y                                    --
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
--   ToDo: 

with ada.strings;					use ada.strings;
with ada.characters.handling;		use ada.characters.handling;
with gnat.directory_operations;
with et_general;					use et_general;
with general_rw;					use general_rw;

package body et_design_rules is

	function is_empty (rules : in pac_file_name.bounded_string) return boolean is begin
		if pac_file_name.length (rules) = 0 then
			return true;
		else
			return false;
		end if;
	end is_empty;
	
	function to_file_name (file : in string) return pac_file_name.bounded_string is begin
		return pac_file_name.to_bounded_string (file);
	end to_file_name;

	function to_string (file : in pac_file_name.bounded_string) return string is begin
		return pac_file_name.to_string (file);
	end to_string;

	function to_string (section : in type_section_name) return string is
		len : positive := type_section_name'image (section)'length;
	begin
		return to_lower (type_section_name'image (section) (5..len));
	end to_string;
	
	procedure read_rules (
		file_name		: in pac_file_name.bounded_string;
		log_threshold 	: in et_string_processing.type_log_level)
	is separate;
	
	function get_rules (rules : in pac_file_name.bounded_string) 
		return type_design_rules is

		use pac_design_rules;

		-- Locate the design rules:
		c : pac_design_rules.cursor := find (design_rules, rules);
	begin
		return element (c);
	end get_rules;
	
end et_design_rules;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
