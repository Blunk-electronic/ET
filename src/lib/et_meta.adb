------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               M E T A                                    --
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

with ada.text_io;				use ada.text_io;
with ada.characters.latin_1;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;				use et_general;
with et_coordinates;			use et_coordinates;
with et_string_processing;

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
	
-- KEYWORDS


-- 	keyword_active_assembly_variant	: constant string := "active_assembly_variant";	

	procedure dummy is begin null; end;
	
end et_meta;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
