------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               M E T A                                    --
--                                                                          --
--                               B o d y                                    --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 

with ada.directories;
with et_string_processing;		use et_string_processing;

package body et_meta is

	function to_company (company : in string) return pac_company.bounded_string is begin
		return pac_company.to_bounded_string (company);
	end;

	function to_string (company : in pac_company.bounded_string) return string is begin
		return pac_company.to_string (company);
	end;


	function to_customer (customer : in string) return pac_customer.bounded_string is begin
		return pac_customer.to_bounded_string (customer);
	end;

	function to_string (customer : in pac_customer.bounded_string) return string is begin
		return pac_customer.to_string (customer);
	end;


	function to_partcode (partcode : in string) return pac_partcode.bounded_string is begin
		return pac_partcode.to_bounded_string (partcode);
	end;

	function to_string (partcode : in pac_partcode.bounded_string) return string is begin
		return pac_partcode.to_string (partcode);
	end;


	function to_drawing_number (drawing_number : in string) return pac_drawing_number.bounded_string is begin
		return pac_drawing_number.to_bounded_string (drawing_number);
	end;

	function to_string (drawing_number : in pac_drawing_number.bounded_string) return string is begin
		return pac_drawing_number.to_string (drawing_number);
	end;


	function to_revision (revision : in string) return pac_revision.bounded_string is begin
		return pac_revision.to_bounded_string (revision);
	end;

	function to_string (revision : in pac_revision.bounded_string) return string is begin
		return pac_revision.to_string (revision);
	end;


	function to_person (person : in string) return pac_person.bounded_string is begin
		return pac_person.to_bounded_string (person);
	end;

	function to_string (person : in pac_person.bounded_string) return string is begin
		return pac_person.to_string (person);
	end;
	
	function to_string (date : in time) return string is
		-- The function "image" returns something like "2017-08-17 14:17:25".
		-- We extract only year, month and day:
		t : constant string (1..10) := image (date) (1..10);
	begin
		return t; -- 2019-01-01
	end;

	function to_date (date : in string) return time is -- 2019-01-01 
	begin
		-- The function "value" requires something like "2017-08-17 14:17:25".
		-- Since date provides only year, month and day, we append hours, minutes and seconds.
		return value (date & " 00:00:00");

		exception when event:
			others => log (ERROR, text => "date invalid !", console => true);
				raise;
	end;


	function exists (lib : in pac_preferred_library_schematic.bounded_string)
		return boolean 
	is
		use ada.directories;
	begin
		if exists (expand (to_string (lib))) then
			return true;
		else
			return false;
		end if;
	end exists;
	
	function to_preferred_library_schematic (lib : in string)
		return pac_preferred_library_schematic.bounded_string
	is begin
		return to_bounded_string (lib);
	end to_preferred_library_schematic;
	
	function to_string (lib : in pac_preferred_library_schematic.bounded_string)
		return string
	is begin
		return pac_preferred_library_schematic.to_string (lib);
	end to_string;

	function exists (lib : in pac_preferred_library_board.bounded_string)
		return boolean 
	is
		use ada.directories;
	begin
		if exists (expand (to_string (lib))) then
			return true;
		else
			return false;
		end if;
	end exists;
	
	function to_preferred_library_board (lib : in string)
		return pac_preferred_library_board.bounded_string
	is begin
		return to_bounded_string (lib);
	end to_preferred_library_board;

	function to_string (lib : in pac_preferred_library_board.bounded_string)
		return string
	is begin
		return pac_preferred_library_board.to_string (lib);
	end to_string;

	
end et_meta;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
