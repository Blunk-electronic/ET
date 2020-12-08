------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        SYMBOL READ AND WRITE                             --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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

with et_string_processing;
with et_coordinates;
with et_symbols;			use et_symbols;

package et_symbol_rw is

	use et_coordinates.pac_geometry_sch;
	
-- 	keyword_style		: constant string := "style";
	keyword_width 		: constant string := "width";
	
	section_draw		: constant string := "[DRAW";
	section_port		: constant string := "[PORT";	
	section_ports		: constant string := "[PORTS";
	
	
	function to_grid (
		line : in et_string_processing.type_fields_of_line; -- "default x 1 y 1"
		from : in positive)
		return type_grid;


	function position (pos : in type_point) return string;
	-- Returns something like "x 12.34 y 45.0".

	function to_position (
		line : in et_string_processing.type_fields_of_line; -- "keyword x 3 y 4" or "position x 44.5 y 53.5"
		from : in positive)
		return type_point;
	
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

	procedure create_symbol (
	-- Creates a symbol and stores it in container et_symbols.symbols.
		symbol_name		: in pac_symbol_model_file.bounded_string; -- libraries/symbols/nand.sym
		appearance		: in type_appearance;
		log_threshold	: in et_string_processing.type_log_level);
	
	procedure write_symbol ( 
		symbol			: in type_symbol;
		log_threshold	: in et_string_processing.type_log_level);
	
	procedure save_symbol (
	-- Saves the given symbol model in a file specified by file_name.
		file_name		: in pac_symbol_model_file.bounded_string; -- libraries/symbols/nand.sym
		symbol			: in type_symbol; -- the actual symbol model
		log_threshold	: in et_string_processing.type_log_level);
	
	procedure read_symbol (
	-- Opens the symbol file and stores the symbol in container et_symbols.symbols.
		file_name 		: in pac_symbol_model_file.bounded_string; -- libraries/symbols/nand.sym
		log_threshold	: in et_string_processing.type_log_level);

	
end et_symbol_rw;
