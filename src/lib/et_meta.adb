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

	function to_company (company : in string) return type_company is begin
		return type_company (pac_company.to_bounded_string (company));
	end to_company;

	function to_string (company : in type_company) return string is begin
		return pac_company.to_string (pac_company.bounded_string (company));
	end to_string;


	function to_customer (customer : in string) return type_customer is begin
		return type_customer (pac_customer.to_bounded_string (customer));
	end to_customer;

	function to_string (customer : in type_customer) return string is begin
		return pac_customer.to_string (pac_customer.bounded_string (customer));
	end to_string;


	function to_partcode (partcode : in string) return type_partcode is begin
		return type_partcode (pac_partcode.to_bounded_string (partcode));
	end to_partcode;

	function to_string (partcode : in type_partcode) return string is begin
		return pac_partcode.to_string (pac_partcode.bounded_string (partcode));
	end to_string;


	function to_drawing_number (drawing_number : in string) return type_drawing_number is begin
		return type_drawing_number (pac_drawing_number.to_bounded_string (drawing_number));
	end to_drawing_number;

	function to_string (drawing_number : in type_drawing_number) return string is begin
		return pac_drawing_number.to_string (pac_drawing_number.bounded_string (drawing_number));
	end to_string;


	function to_revision (revision : in string) return type_revision is begin
		return type_revision (pac_revision.to_bounded_string (revision));
	end to_revision;

	function to_string (revision : in type_revision) return string is begin
		return pac_revision.to_string (pac_revision.bounded_string (revision));
	end to_string;


	function to_person (person : in string) return type_person is begin
		return type_person (pac_person.to_bounded_string (person));
	end to_person;

	function to_string (person : in type_person) return string is begin
		return pac_person.to_string (pac_person.bounded_string (person));
	end to_string;







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
	is begin
		return meta.preferred_libs;
	end get_device_libraries;






	procedure set_device_libraries (
		meta : in out type_meta_board;
		libs : in pac_library_paths_board.list)
	is begin
		meta.preferred_libs := libs;
	end set_device_libraries;


	procedure add_device_library (
		meta : in out type_meta_board;
		lib  : in pac_library_path_board.bounded_string)
	is begin
		meta.preferred_libs.append (lib);
	end add_device_library;


	procedure remove_device_library (
		meta : in out type_meta_board;
		lib  : in pac_library_path_board.bounded_string)
	is begin
		null;
		-- CS
		-- test whether lib exists, then remove it
	end remove_device_library;




	function get_device_libraries (
		meta : in type_meta_board)
		return pac_library_paths_board.list
	is begin
		return meta.preferred_libs;
	end get_device_libraries;



end et_meta;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
