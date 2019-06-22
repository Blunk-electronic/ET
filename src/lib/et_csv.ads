------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                 CSV                                      --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:

with ada.strings.unbounded; 		use ada.strings.unbounded;
with ada.strings.unbounded.text_io;	use ada.strings.unbounded.text_io;
with ada.text_io;					use ada.text_io;

package et_csv is

	file_extension		: constant string (1..3) := "csv";
	
	row_separator_1 	: constant string (1..10) := "----------";
	row_separator_2 	: constant string (1..20) := "--------------------";

	columns_max : constant natural := 1000; -- CS this is the limit of columns a csv file can have.
	type type_column is range 0..columns_max;
	column : type_column := type_column'first;

	function to_string (column : in type_column) return string;
	-- Returns the given column as string.

	procedure reset_column;

	procedure next_column;

	procedure put_field (
	-- Puts a field into a csv file.
	-- Each field (even if empty) is enclosed in delimiters.
	-- Counts columns in variable "column".
		file	: in ada.text_io.file_type := current_output; -- default to current output if not specified otherwise
		text	: in string := "";
		ifs		: in character := ascii.semicolon; -- field separator
		delim 	: in character := ascii.quotation);  -- text delimiter


	procedure put_lf (
	-- Puts a line feed into a csv file.
	-- Resets columns variable "column".
		file		: in ada.text_io.file_type := current_output; -- default to current output if not specified otherwise
		field_count	: in type_column); -- the number of empty fields to append before line feed

end et_csv;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
