------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                  META / DEVICE LIBRARIES / SCHEMATIC                     --
--                                                                          --
--                               B o d y                                    --
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


-- with ada.text_io;			use ada.text_io;
with ada.directories;

with et_directory_and_file_ops;


package body et_meta_device_libraries_schematic is



	function library_path_exists (
		lib : in type_library_path_schematic)
		return boolean
	is
		use ada.directories;
		use et_directory_and_file_ops;
	begin
		if exists (expand (to_string (lib))) then
			return true;
		else
			return false;
		end if;
	end library_path_exists;











	function get_first (
		paths : in pac_library_paths_schematic.list)
		return type_library_path_schematic
	is (paths.first_element);




	function is_empty (
		paths : in pac_library_paths_schematic.list)
		return boolean
	is (pac_library_paths_schematic.is_empty (paths));



end et_meta_device_libraries_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
