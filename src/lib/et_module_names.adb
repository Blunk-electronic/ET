------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            MODULE NAMES                                  --
--                                                                          --
--                               B o d y                                    --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--


with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;



package body et_module_names is

	
	function to_module_file_name (name : in string) return pac_module_file_name.bounded_string is begin
		return pac_module_file_name.to_bounded_string (name);
	end;

	function to_string (name : in pac_module_file_name.bounded_string) return string is begin
		return pac_module_file_name.to_string (name);
	end;


	
	function remove_extension (file_name : in string) return string is
	-- Removes from a string like templates/clock_generator.mod the extension so that
	-- the return would be templates/clock_generator .
		name_length : positive := file_name'length;
		pos_last_character : positive;
	begin
		pos_last_character := name_length - module_file_name_extension'length - 1;
		return file_name (file_name'first .. pos_last_character);
	end remove_extension;


	
	function append_extension (file_name : in string) return string is
	-- Appends to a string like templates/clock_generator the extension "mod" so that
	-- the return would be templates/clock_generator.mod .
	begin
		return file_name & '.' & module_file_name_extension;
	end;
	

	

	
	function to_string (name : in pac_module_name.bounded_string) return string is begin
		return pac_module_name.to_string (name);
	end;

	
	function to_module_name (name : in string) return pac_module_name.bounded_string is begin
		return pac_module_name.to_bounded_string (name);
	end;
	

	
	function to_string (name : in pac_module_instance_name.bounded_string) return string is begin
		return pac_module_instance_name.to_string (name);
	end;


	function to_instance_name (name : in string) return pac_module_instance_name.bounded_string is begin
		return pac_module_instance_name.to_bounded_string (name);
	end;
	
	
	
end et_module_names;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
