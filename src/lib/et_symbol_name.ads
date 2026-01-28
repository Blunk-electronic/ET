------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       SCHEMATIC SYMBOL NAME                              --
--                                                                          --
--                              S p e c                                     --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
-- DESCRIPTION:
--
--
--   history of changes:
--
-- To Do:
-- - rename this package to et_symbol_model_name
--

with ada.strings.bounded; 		use ada.strings.bounded;


package et_symbol_name is

	
	
	symbol_file_name_length_max : constant natural := 500;


	-- A symbol model has a name like
	-- "../lbr/logic/nand.sym"
	-- The model name is equally to the file name that contains
	-- the model:
	package pac_symbol_model_file is new 
		generic_bounded_length (symbol_file_name_length_max);
	-- CS rename to pac_symbol_model_name

	symbol_library_file_extension : constant string := "sym";
		
		
	function to_string (
		name : in pac_symbol_model_file.bounded_string) 
		return string;

	
	function to_file_name (
		name : in string) 
		return pac_symbol_model_file.bounded_string;
	


end et_symbol_name;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
