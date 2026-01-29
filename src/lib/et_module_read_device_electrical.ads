------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE READ / ELECTRICAL DEVICE                       --
--                                                                          --
--                               S p e c                                    --
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
--  ToDo: 
-- - clean up
-- - rename global subroutines


with et_generic_modules;		use et_generic_modules;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;


package et_module_read_device_electrical is
		

	procedure read_device_electrical (
		line : in type_fields_of_line);



	procedure insert_device_electrical (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);



	procedure read_package_position (
		line : in type_fields_of_line);

	procedure set_package_position;
	

	procedure read_unit (
		line : in type_fields_of_line);


	
	procedure insert_unit (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);


	procedure insert_units;


	procedure read_unit_placeholder (
		line : in type_fields_of_line);



	-- Builds a placeholder from unit_placeholder_meaning, unit_placeholder_position and unit_placeholder.
	-- Depending on the meaning of the placeholder it becomes a placeholder 
	-- for the reference (like R4), the value (like 100R) or the purpose (like "brightness control").
	procedure build_unit_placeholder;



	-- This procdure reads a property of a placeholder
	-- of a device package (board):
	procedure read_device_text_placeholder (
		line : in type_fields_of_line);

	
	procedure insert_package_placeholder;
	

	procedure insert_placeholders;

	
end et_module_read_device_electrical;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
