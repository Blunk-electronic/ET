------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                 CSV                                      --
--                                                                          --
--                               B o d y                                    --
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
--

with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with et_string_processing;

package body et_csv is

	function to_string (column : in type_column) return string is
	-- Returns the given column as string.
	begin
		return trim (type_column'image (column),left);
	end to_string;
	
	procedure reset_column is
	begin
		column := type_column'first;
	end reset_column;

	procedure next_column is
	begin
		column := column + 1;
	end next_column;

	
	procedure put_field (
	-- Puts a field into a csv file.
	-- Each field (even if empty) is enclosed in delimiters.
	-- Counts columns in variable "column".
		file	: in ada.text_io.file_type := current_output; -- default to current output if not specified otherwise
		text	: in string := "";
		ifs		: in character := ascii.semicolon; -- field separator
		delim 	: in character := ascii.quotation) is  -- text delimiter
	begin
		put (file, delim & trim (text, both) & delim & ifs);
		next_column;
	end put_field;


	procedure put_lf (
	-- Puts a line feed into a csv file.
	-- Resets columns variable "column".
		file		: in ada.text_io.file_type := current_output; -- default to current output if not specified otherwise
		field_count	: in type_column) is -- the number of empty fields to append before line feed

		use et_string_processing;
	
		fill_fields : type_column;
	begin
		if column < field_count then -- less columns than field_count -> fill pad fields
			fill_fields := field_count - column; -- number of fields to fill

			-- write empty pad fields
			for i in 1..fill_fields loop
				put_field (file);
			end loop;

		elsif column > field_count then -- column must not exceed field_count -> error
			
			log (ERROR, "record length "
				& to_string (field_count)
				& " less than current column "
				& to_string (column)
				& " !");
			
			raise constraint_error;
		end if;
		
		new_line (file);

		reset_column;
	end put_lf;
		
end et_csv;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

