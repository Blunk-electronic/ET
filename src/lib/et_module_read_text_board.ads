------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         MODULE READ / TEXT IN BOARD                      --
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
-- - synchronize names of subprograms with those in et_module_write_text_board
--
--

with et_generic_modules;		use et_generic_modules;
with et_board_layer_category;	use et_board_layer_category;
with et_pcb_sides;				use et_pcb_sides;

with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;



package et_module_read_text_board is

	
	-- This procdure reads a property of a general 
	-- placeholder in the board drawing (like project name, material code, ...):
	procedure read_board_text_placeholder (
		line : in type_fields_of_line);


	procedure read_board_text_non_conductor (
		line : in type_fields_of_line);

	
	
	
	procedure read_board_text_conductor (
		line : in type_fields_of_line);
		
		
	procedure read_board_text_conductor_placeholder (
		line : in type_fields_of_line);
	
	
	procedure read_board_text_contours (
		line : in type_fields_of_line);

	
	
	procedure insert_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		layer_cat		: in type_layer_category;
		face			: in type_face;  -- TOP, BOTTOM
		log_threshold	: in type_log_level);
		
		
	procedure insert_board_text_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);

	
	
	procedure build_non_conductor_text (
		module_cursor	: in pac_generic_modules.cursor;
		layer_cat		: in type_layer_category;
		face 			: in et_pcb_sides.type_face;  -- TOP, BOTTOM
		log_threshold	: in type_log_level);
		
	
	
	procedure build_conductor_text (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);
		
	
end et_module_read_text_board;

	


	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
