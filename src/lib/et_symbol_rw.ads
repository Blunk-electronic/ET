------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        SYMBOL READ AND WRITE                             --
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

with ada.containers;					use ada.containers;

with et_string_processing;				use et_string_processing;
with et_logging;						use et_logging;
with et_coordinates_2;					use et_coordinates_2;
with et_symbols;						use et_symbols;
with et_device_appearance;				use et_device_appearance;
with et_schematic_shapes_and_text;		use et_schematic_shapes_and_text;


package et_symbol_rw is

	use pac_text_schematic;
	
	use pac_geometry_2;

	section_draw		: constant string := "[DRAW";
	section_port		: constant string := "[PORT";	
	section_ports		: constant string := "[PORTS";

	
	-- This function processes a line starting 
	-- from a given position and returns a grid spacing.
	-- Since both schematic and symbol read operations require
	-- this function, it is placed in this package:
	function to_grid_spacing (
		line : in type_fields_of_line; -- "spacing x 1.0 y 1.0"
		from : in count_type)
		return type_vector_model;


	
	function to_position (
		line : in type_fields_of_line; -- "keyword x 3 y 4" or "position x 44.5 y 53.5"
		from : in count_type)
		return type_vector_model;

	
	procedure write_text_properties (t : in type_text_basic'class);

	
	type type_section is (
		SEC_INIT,
		SEC_DRAW,
		SEC_LINE,
		SEC_ARC,
		SEC_CIRCLE,
		SEC_TEXTS,
		SEC_TEXT,
		SEC_PLACEHOLDER,		
		SEC_PLACEHOLDERS,
		SEC_PORTS,
		SEC_PORT
		);
	

	-- Creates a symbol and stores it in container et_symbols.symbols.
	procedure create_symbol (
		symbol_name		: in pac_symbol_model_file.bounded_string; -- libraries/symbols/nand.sym
		appearance		: in type_appearance;
		log_threshold	: in type_log_level);

	
	procedure write_symbol ( 
		symbol			: in type_symbol;
		log_threshold	: in type_log_level);

	
	-- Saves the given symbol model in a file specified by file_name.
	procedure save_symbol (
		file_name		: in pac_symbol_model_file.bounded_string; -- libraries/symbols/nand.sym
		symbol			: in type_symbol; -- the actual symbol model
		log_threshold	: in type_log_level);

	
	-- Opens the symbol file and stores the symbol in container et_symbols.symbols.
	procedure read_symbol (
		file_name 		: in pac_symbol_model_file.bounded_string; -- libraries/symbols/nand.sym
		log_threshold	: in type_log_level);

	
end et_symbol_rw;
