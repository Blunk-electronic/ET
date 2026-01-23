------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               M E T A                                    --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                -- 
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

with et_meta_device_libraries;	use et_meta_device_libraries;
with et_schematic_coordinates;	use et_schematic_coordinates;
with et_string_processing;
with et_time;					use et_time;
with et_logging;				use et_logging;


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
	
	
	
	person_length_max : constant positive := 20;
	package pac_person is new generic_bounded_length (person_length_max);

	function to_person (person : in string) return pac_person.bounded_string;
	function to_string (person : in pac_person.bounded_string) return string;

	person_default : constant pac_person.bounded_string := pac_person.to_bounded_string (not_assigned);
	




	
	
	type type_meta_basic is tagged record
		company			: pac_company.bounded_string := company_default;
		customer		: pac_customer.bounded_string := customer_default;
		partcode		: pac_partcode.bounded_string := partcode_default;
		drawing_number	: pac_drawing_number.bounded_string := drawing_number_default;
		revision		: pac_revision.bounded_string := revision_default;
		drawn_by		: pac_person.bounded_string := person_default;
		checked_by		: pac_person.bounded_string := person_default;
		approved_by		: pac_person.bounded_string := person_default;
		drawn_date		: time := clock;
		checked_date	: time := date_first; -- default 1901-01-01
		approved_date	: time := date_first; -- default 1901-01-01
	end record;

	-- CS accessor subprograms to set and get company, customer, ...
	
	

	

	
	type type_meta_schematic is new type_meta_basic with record
		preferred_libs	: pac_preferred_libraries_schematic.list;
	end record;


	procedure set_device_libraries (
		meta : in out type_meta_schematic;
		libs : in pac_preferred_libraries_schematic.list);
		

	procedure add_device_library (
		meta : in out type_meta_schematic;
		lib  : in pac_preferred_library_schematic.bounded_string);
	

	function get_device_libraries (
		meta : in type_meta_schematic)
		return pac_preferred_libraries_schematic.list;

		
	

	
	type type_meta_board is new type_meta_basic with record
		preferred_libs	: pac_preferred_libraries_board.list;
	end record;

	procedure set_device_libraries (
		meta : in out type_meta_board;
		libs : in pac_preferred_libraries_board.list);

		
	procedure add_device_library (
		meta : in out type_meta_board;
		lib  : in pac_preferred_library_board.bounded_string);

	
	function get_device_libraries (
		meta : in type_meta_board)
		return pac_preferred_libraries_board.list;

		
		
	
	
	type type_meta is record
		schematic	: type_meta_schematic;
		board		: type_meta_board;
	end record;


	
-- KEYWORDS

	keyword_company			: constant string := "company";
	keyword_customer		: constant string := "customer";
	keyword_drawing_number	: constant string := "drawing_number";
	keyword_drawn_by		: constant string := "drawn_by";
	keyword_drawn_date		: constant string := "drawn_date";	
	keyword_checked_by		: constant string := "checked_by";
	keyword_checked_date	: constant string := "checked_date";	
	keyword_approved_by		: constant string := "approved_by";
	keyword_approved_date	: constant string := "approved_date";	
	-- CS move to et_keywords
	
end et_meta;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
