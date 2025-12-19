------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE READ / META DATA                               --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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
--                                                                          --
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
-- ToDo:
-- - clean up
--
--
--

with et_generic_module;			use et_generic_module;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;




package et_module_read_meta is


	procedure add_meta_schematic;

	procedure add_meta_board;

	
	
	-- Assigns the collected meta data to the module:
	procedure set_meta (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);

	
	
	function read_meta_basic (
		line : in type_fields_of_line)
		return boolean;


	procedure read_meta_schematic (
		line : in type_fields_of_line);



	procedure read_meta_board (
		line : in type_fields_of_line);
	


	procedure read_preferred_lib_schematic (
 		line : in type_fields_of_line);

	
	procedure read_preferred_lib_board (
		line : in type_fields_of_line);

	
	
end et_module_read_meta;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
