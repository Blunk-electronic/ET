------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             GENERAL_RW                                   --
--                                                                          --
--                               B o d y                                    --
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

with et_string_processing;

package body general_rw is

	
	-- This function returns the string at position in given line:
	function f (line : in et_string_processing.type_fields_of_line; position : in positive) return string 
		renames et_string_processing.field;

	procedure expect_field_count (
		line			: in et_string_processing.type_fields_of_line;	-- the list of fields of the line
		count_expected	: in count_type;			-- the min. number of fields to expect
		warn			: in boolean := true) 		-- warn if too many fields
		is 
		use et_string_processing;
		count_found : constant count_type := field_count (line);

		function f (line : in type_fields_of_line; position : in positive) return string 
			renames et_string_processing.field;
		
		f1 : string := f (line, 1); -- CS: line must have at least one field otherwise exception occurs here
	begin
		if count_found = count_expected then null; -- fine, field count as expected
		
		elsif count_found < count_expected then -- less fields than expected
			log (ERROR, "missing parameter for '" & f1 & "' !", console => true);
			raise constraint_error;
			
		elsif count_found > count_expected then -- more fields than expeced
			if warn then
				log (WARNING, affected_line (line) & "excessive parameters after '" &
					f (line, positive (count_expected)) & "' ignored !");
			end if;
		end if;
		
	end expect_field_count;

	procedure invalid_keyword (word : in string) is 
		use et_string_processing;
	begin
		log (ERROR, "invalid keyword '" & word & "' !", console => true);
		raise constraint_error;
	end;

end general_rw;
