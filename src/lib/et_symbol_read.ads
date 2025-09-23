------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             SYMBOL READ                                  --
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

with et_string_processing;				use et_string_processing;
with et_logging;						use et_logging;
with et_schematic_geometry;				use et_schematic_geometry;
with et_schematic_coordinates;			use et_schematic_coordinates;
with et_symbol_name;					use et_symbol_name;
with et_symbol_model;					use et_symbol_model;
with et_device_appearance;				use et_device_appearance;
with et_schematic_text;					use et_schematic_text;


package et_symbol_read is

	use pac_text_schematic;
	
	use pac_geometry_2;


	
	-- Opens the symbol file and stores the symbol in container symbols.
	-- CS error : out boolean;
	-- CS device_curosr : out pac_symbols.cursor;
	procedure read_symbol (
		file_name 		: in pac_symbol_model_file.bounded_string; -- libraries/symbols/nand.sym
		log_threshold	: in type_log_level);

	
end et_symbol_read;
