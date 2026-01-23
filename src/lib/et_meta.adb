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

with ada.directories;
with et_string_processing;		use et_string_processing;
with et_directory_and_file_ops;


package body et_meta is

	function to_company (company : in string) return pac_company.bounded_string is begin
		return pac_company.to_bounded_string (company);
	end;

	function to_string (company : in pac_company.bounded_string) return string is begin
		return pac_company.to_string (company);
	end;


	function to_customer (customer : in string) return pac_customer.bounded_string is begin
		return pac_customer.to_bounded_string (customer);
	end;

	function to_string (customer : in pac_customer.bounded_string) return string is begin
		return pac_customer.to_string (customer);
	end;


	function to_partcode (partcode : in string) return pac_partcode.bounded_string is begin
		return pac_partcode.to_bounded_string (partcode);
	end;

	function to_string (partcode : in pac_partcode.bounded_string) return string is begin
		return pac_partcode.to_string (partcode);
	end;


	function to_drawing_number (drawing_number : in string) return pac_drawing_number.bounded_string is begin
		return pac_drawing_number.to_bounded_string (drawing_number);
	end;

	function to_string (drawing_number : in pac_drawing_number.bounded_string) return string is begin
		return pac_drawing_number.to_string (drawing_number);
	end;


	function to_revision (revision : in string) return pac_revision.bounded_string is begin
		return pac_revision.to_bounded_string (revision);
	end;

	function to_string (revision : in pac_revision.bounded_string) return string is begin
		return pac_revision.to_string (revision);
	end;


	function to_person (person : in string) return pac_person.bounded_string is begin
		return pac_person.to_bounded_string (person);
	end;

	function to_string (person : in pac_person.bounded_string) return string is begin
		return pac_person.to_string (person);
	end;




	


	procedure set_device_libraries (
		meta : in out type_meta_schematic;
		libs : in pac_preferred_libraries_schematic.list)
	is begin
		meta.preferred_libs := libs;
	end;


	procedure add_device_library (
		meta : in out type_meta_schematic;
		lib  : in pac_preferred_library_schematic.bounded_string)
	is begin
		meta.preferred_libs.append (lib);
	end;


	function get_device_libraries (
		meta : in type_meta_schematic)
		return pac_preferred_libraries_schematic.list
	is begin
		return meta.preferred_libs;
	end;

	


	
	
	procedure set_device_libraries (
		meta : in out type_meta_board;
		libs : in pac_preferred_libraries_board.list)
	is begin
		meta.preferred_libs := libs;
	end;


	procedure add_device_library (
		meta : in out type_meta_board;
		lib  : in pac_preferred_library_board.bounded_string)
	is begin
		meta.preferred_libs.append (lib);
	end;


	function get_device_libraries (
		meta : in type_meta_board)
		return pac_preferred_libraries_board.list
	is begin
		return meta.preferred_libs;
	end;


	
end et_meta;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
