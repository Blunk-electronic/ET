------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               M E T A                                    --
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
package body et_meta is




	procedure set_device_libraries (
		meta : in out type_meta_schematic;
		libs : in pac_library_paths_schematic.list)
	is begin
		meta.preferred_libs := libs;
	end set_device_libraries;


	procedure add_device_library (
		meta : in out type_meta_schematic;
		lib  : in type_library_path_schematic)
	is begin
		meta.preferred_libs.append (lib);
	end add_device_library;


	procedure remove_device_library (
		meta : in out type_meta_schematic;
		lib  : in type_library_path_schematic)
	is begin
		null;
		-- CS
		-- test whether lib exists, then remove it
	end remove_device_library;


	function get_device_libraries (
		meta : in type_meta_schematic)
		return pac_library_paths_schematic.list
	is (meta.preferred_libs);






	procedure set_device_libraries (
		meta : in out type_meta_board;
		libs : in pac_library_paths_board.list)
	is begin
		meta.preferred_libs := libs;
	end set_device_libraries;


	procedure add_device_library (
		meta : in out type_meta_board;
		lib  : in type_library_path_board)
	is begin
		meta.preferred_libs.append (lib);
	end add_device_library;


	procedure remove_device_library (
		meta : in out type_meta_board;
		lib  : in type_library_path_board)
	is begin
		null;
		-- CS
		-- test whether lib exists, then remove it
	end remove_device_library;




	function get_device_libraries (
		meta : in type_meta_board)
		return pac_library_paths_board.list
	is (meta.preferred_libs);



end et_meta;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
