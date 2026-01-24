------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    SCHEMATIC OPERATIONS / META                           --
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


with ada.text_io;					use ada.text_io;

with et_module;						use et_module;
with et_module_names;				use et_module_names;


package body et_schematic_ops_meta is



	function get_basic_meta_information (
		module : in pac_generic_modules.cursor)
		return type_meta_basic
	is begin
		return type_meta_basic (element (module).meta.schematic);
	end get_basic_meta_information;


	

	
	function get_preferred_libraries (
		module : in pac_generic_modules.cursor)
		return pac_library_paths_schematic.list
	is 
		m : type_generic_module renames element (module);
	begin
		return get_preferred_device_libraries_schematic (m);
	end get_preferred_libraries;



	

	procedure add_library_path (
		module_cursor	: in pac_generic_modules.cursor;
		path			: in pac_library_path_schematic.bounded_string;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			add_device_library (module.meta.schematic, path);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			 & " add library path to schematic: "
			 & to_string (path),
			 level => log_threshold);

		log_indentation_up;

		-- CS test whether the given path exists
		
		generic_modules.update_element (module_cursor, query_module'access);

		log_indentation_down;
	end add_library_path;


	
end et_schematic_ops_meta;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
