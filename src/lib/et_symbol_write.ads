------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             SYMBOL WRITE                                 --
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
with et_symbol_name;					use et_symbol_name;
with et_symbol_model;					use et_symbol_model;


package et_symbol_write is


	-- This procedure writes the given symbol in
	-- the current output file.
	-- This procedure does not write a header or a footer,
	-- but just the symbol.
	-- It is called when:
	-- 1. a symbol is to be saved with 
	--    a given name and with a nice header and footer.
	--    Procedure save_symbol_2 calls save_symbol_1 for example.
	-- 2. a device is to be saved that has internal units.
	procedure save_symbol_1 (
		symbol			: in type_symbol_model;
		log_threshold	: in type_log_level);


	
	-- Saves the given symbol model in a 
	-- file specified by file_name:
	procedure save_symbol_2 (
		file_name		: in pac_symbol_model_name.bounded_string; -- libraries/symbols/nand.sym
		symbol			: in type_symbol_model; -- the actual symbol model
		log_threshold	: in type_log_level);
	-- CS rename to save_symbol_model
	
	
end et_symbol_write;
