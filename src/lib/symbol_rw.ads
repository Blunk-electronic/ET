------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              SYMBOL_RW                                   --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

with ada.containers;            use ada.containers;

with et_schematic;
with et_string_processing;
with et_coordinates;
with et_symbols;

package symbol_rw is

	keyword_sheet				: constant string := "sheet";
	
	keyword_direction			: constant string := "direction";			
	keyword_style				: constant string := "style";

	section_draw				: constant string := "[DRAW";
	section_port				: constant string := "[PORT";	
	section_ports				: constant string := "[PORTS";
	
	
	function to_grid (
		line : in et_string_processing.type_fields_of_line; -- "default x 1 y 1"
		from : in positive)
		return et_coordinates.geometry.type_grid;


	function position (pos : in et_coordinates.geometry.type_point'class) return string;
	-- Returns something like "x 12.34 y 45.0" or "sheet 3 x 12.34 y 45.0".
	-- This kind of output depends on the tag of the given object.

	function to_position (
		line : in et_string_processing.type_fields_of_line; -- "keyword x 3 y 4" or "position x 44.5 y 53.5"
		from : in positive)
		return et_coordinates.geometry.type_point;
	
	procedure write_text_properties (t : in et_symbols.type_text_basic'class);

	type type_section_name_symbol is ( -- CS rename to type_section
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

	procedure write_symbol ( 
		symbol			: in et_symbols.type_symbol;
		log_threshold	: in et_string_processing.type_log_level);
	
	procedure save_symbol (
	-- Saves the given symbol model in a file specified by name.
		name			: in string; -- libraries/symbols/resistor.sym
		symbol			: in et_symbols.type_symbol; -- the actual symbol model
		log_threshold	: in et_string_processing.type_log_level);
	
	procedure read_symbol (
	-- Opens the symbol file and stores the symbol in container et_libraries.symbols.
		file_name 		: in et_symbols.type_symbol_model_file.bounded_string; -- libraries/symbols/nand.sym
		log_threshold	: in et_string_processing.type_log_level);

	
end symbol_rw;
