------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                 CSV                                      --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 Mario Blunk, Blunk electronic                 --
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

-- 	function date_to_natural
-- 	-- Takes an iso date like 1974-07-04 and converts into a natural from 0 to 99999999.
-- 		(
-- 		date 	: string -- (1..10) := "0000-00-00"
-- 		)
-- 		return natural;
-- 
-- 	function get_field_count
-- 	-- Returns number of fields separated by ifs. if line is empty zero will be returned.
-- 		(
-- 		line	: unbounded_string;
-- 		ifs		: character := ';'	-- default separator is semicolon
-- 		)
-- 		return natural;
-- 
-- 	function get_field_position
-- 	-- Returns position of first field containing given string. Ff string not found, returns 0.
-- 		(
-- 		line	: unbounded_string;
-- 		text 	: string;
-- 		ifs		: character := ';'	-- default separator is semicolon
-- 		)
-- 		return natural;
-- 
-- 	function get_field
-- 	-- Returns field content indicated by field position and separated by ifs.
-- 	-- If line is empty, "" will be returned.		
-- 		(
-- 		line	: unbounded_string;
-- 		pos 	: natural;
-- 		ifs		: character := ';'	-- default separator is semicolon
-- 		)
-- 		return string;
-- 
-- 	function strip_text_delimiters 
-- 	-- Used to delete text delimiters such as quotes (" or ')
-- 		(
-- 		text_in		: string
-- 		)
-- 		return string;

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
