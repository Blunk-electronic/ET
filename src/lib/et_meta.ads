------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               M E T A                                    --
--                                                                          --
--                               S p e c                                    --
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

with ada.calendar;				use ada.calendar;
with ada.calendar.formatting;	use ada.calendar.formatting;
with ada.calendar.time_zones;	use ada.calendar.time_zones;

with et_general;				use et_general;
with et_coordinates;			use et_coordinates;
with et_string_processing;

package et_meta is

	not_assigned : constant string := "N/A";

	company_length_max : constant positive := 100;
	package pac_company is new generic_bounded_length (company_length_max);

	function to_company (company : in string) return pac_company.bounded_string;
	function to_string (company : in pac_company.bounded_string) return string;

	company_default : constant pac_company.bounded_string := pac_company.to_bounded_string (not_assigned);
	
	
	customer_length_max : constant positive := 100;
	package pac_customer is new generic_bounded_length (customer_length_max);

	function to_customer (customer : in string) return pac_customer.bounded_string;
	function to_string (customer : in pac_customer.bounded_string) return string;

	customer_default : constant pac_customer.bounded_string := pac_customer.to_bounded_string (not_assigned);
	
		
	partcode_length_max : constant positive := 100;
	package pac_partcode is new generic_bounded_length (partcode_length_max);

	function to_partcode (partcode : in string) return pac_partcode.bounded_string;
	function to_string (partcode : in pac_partcode.bounded_string) return string;

	partcode_default : constant pac_partcode.bounded_string := pac_partcode.to_bounded_string (not_assigned);
	
	
	drawing_number_length_max : constant positive := 100;
	package pac_drawing_number is new generic_bounded_length (drawing_number_length_max);

	function to_drawing_number (drawing_number : in string) return pac_drawing_number.bounded_string;
	function to_string (drawing_number : in pac_drawing_number.bounded_string) return string;

	drawing_number_default : constant pac_drawing_number.bounded_string := pac_drawing_number.to_bounded_string (not_assigned);
	
	
	revision_length_max : constant positive := 5;
	package pac_revision is new generic_bounded_length (revision_length_max);

	function to_revision (revision : in string) return pac_revision.bounded_string;
	function to_string (revision : in pac_revision.bounded_string) return string;

	revision_default : constant pac_revision.bounded_string := pac_revision.to_bounded_string (not_assigned);
	
	
	function to_string (date : in time) return string;
	function to_date (date : in string) return time;
	
	person_length_max : constant positive := 20;
	package pac_person is new generic_bounded_length (person_length_max);

	function to_person (person : in string) return pac_person.bounded_string;
	function to_string (person : in pac_person.bounded_string) return string;

	person_default : constant pac_person.bounded_string := pac_person.to_bounded_string (not_assigned);
	

	-- PATHS FOR PREFERRED LIBRARIES:

	-- NOTE: System ET does not follow the classical approach of
	-- search paths for libraries. A library here is just a directory where
	-- device models (*.dev) or non-electrical packages (*.pac) live.
	-- Device models and non-electrical packages can be everywhere.
	-- In graphical mode, when adding a device in the schematic or a non-electrical package in
	-- the board/layout there is a window where the operator selects
	-- a model. This window just proposes the paths of preferred libraries.
	-- The operator is not restricted to those library paths and is
	-- free to store models wherever it suits her/him.
	
	-- A preferred directory that contains devices (*.dev)
	-- like "$HOME/git/BEL/ET_component_library/devices":
	prf_lib_sch_length_max : constant positive := 100;
	package pac_preferred_library_schematic is new generic_bounded_length (prf_lib_sch_length_max);
	use pac_preferred_library_schematic;

	-- Returns true if the given path exists:
	function exists (lib : in pac_preferred_library_schematic.bounded_string)
		return boolean;
	
	function to_preferred_library_schematic (lib : in string)
		return pac_preferred_library_schematic.bounded_string;

	function to_string (lib : in pac_preferred_library_schematic.bounded_string)
		return string;
	
	package pac_preferred_libraries_schematic is new 
		doubly_linked_lists (pac_preferred_library_schematic.bounded_string);


	-- A preferred directory that contains non-electrical packages (*.pac)
	-- like "$HOME/git/BEL/ET_component_library/packages":
	prf_lib_brd_length_max : constant positive := 100;
	package pac_preferred_library_board is new generic_bounded_length (prf_lib_brd_length_max);
	use pac_preferred_library_board;

	-- Returns true if the given path exists:
	function exists (lib : in pac_preferred_library_board.bounded_string)
		return boolean;
	
	function to_preferred_library_board (lib : in string)
		return pac_preferred_library_board.bounded_string;

	function to_string (lib : in pac_preferred_library_board.bounded_string)
		return string;
	
	package pac_preferred_libraries_board is new 
		doubly_linked_lists (pac_preferred_library_board.bounded_string);

	
	
	type type_basic is tagged record
		company			: pac_company.bounded_string := company_default;
		customer		: pac_customer.bounded_string := customer_default;
		partcode		: pac_partcode.bounded_string := partcode_default;
		drawing_number	: pac_drawing_number.bounded_string := drawing_number_default;
		revision		: pac_revision.bounded_string := revision_default;
		drawn_by		: pac_person.bounded_string := person_default;
		checked_by		: pac_person.bounded_string := person_default;
		approved_by		: pac_person.bounded_string := person_default;
		drawn_date		: time := clock;
		checked_date	: time := et_string_processing.date_first; -- default 1901-01-01
		approved_date	: time := et_string_processing.date_first; -- default 1901-01-01
	end record;

	type type_schematic is new type_basic with record
		preferred_libs	: pac_preferred_libraries_schematic.list;
	end record;
	
	type type_board is new type_basic with record
		preferred_libs	: pac_preferred_libraries_board.list;
	end record;

	
	type type_meta is record
		schematic	: type_schematic;
		board		: type_board;
	end record;


	
-- KEYWORDS

	keyword_company			: constant string := "company";
	keyword_customer		: constant string := "customer";
	keyword_partcode		: constant string := "partcode";
	keyword_drawing_number	: constant string := "drawing_number";
	keyword_revision		: constant string := "revision";
	keyword_drawn_by		: constant string := "drawn_by";
	keyword_drawn_date		: constant string := "drawn_date";	
	keyword_checked_by		: constant string := "checked_by";
	keyword_checked_date	: constant string := "checked_date";	
	keyword_approved_by		: constant string := "approved_by";
	keyword_approved_date	: constant string := "approved_date";	
	keyword_path			: constant string := "path";
	
end et_meta;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
