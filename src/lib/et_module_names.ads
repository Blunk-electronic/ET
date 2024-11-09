------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             MODULE NAMES                                 --
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


with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;



package et_module_names is
	


	-- The module file name:
	module_file_name_length_max : constant positive := 100;
	package pac_module_file_name is new generic_bounded_length (module_file_name_length_max);

	function to_module_file_name (name : in string) return pac_module_file_name.bounded_string;
	function to_string (name : in pac_module_file_name.bounded_string) return string;
	
	

	module_file_name_extension : constant string := "mod";
	module_file_name_extension_asterisk : constant string := "*." & module_file_name_extension;
	
	function remove_extension (file_name : in string) return string;
	-- Removes from a string like templates/clock_generator.mod the extension so that
	-- the return would be templates/clock_generator .

	function append_extension (file_name : in string) return string;
	-- Appends to a string like templates/clock_generator the extension "mod" so that
	-- the return would be templates/clock_generator.mod .



	-- Generic modules are named with this type:
	-- (The actual file has the same name with extension *.mod.)
	module_name_length_max : constant := 100;
	package pac_module_name is new generic_bounded_length (module_name_length_max);

	function to_string (name : in pac_module_name.bounded_string) return string;
	function to_module_name (name : in string) return pac_module_name.bounded_string;

	
	
end et_module_names;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
