------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               M E T A                                    --
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

with ada.strings.bounded;       use ada.strings.bounded;

with ada.calendar;				use ada.calendar;

with et_bounded_string_helpers;
with et_meta_device_libraries_schematic;	use et_meta_device_libraries_schematic;
with et_meta_device_libraries_board;		use et_meta_device_libraries_board;

with et_time;					use et_time;


package et_meta is

	not_assigned : constant string := "N/A";

	company_length_max : constant positive := 100;
	package pac_company is new generic_bounded_length (company_length_max);

	type type_company is new pac_company.bounded_string;

	function to_company is new et_bounded_string_helpers.from_string (pac_company, to_type   => type_company);
	function to_string  is new et_bounded_string_helpers.to_string   (pac_company, from_type => type_company);

	company_default : constant type_company := to_company (not_assigned);


	customer_length_max : constant positive := 100;
	package pac_customer is new generic_bounded_length (customer_length_max);

	type type_customer is new pac_customer.bounded_string;

	function to_customer is new et_bounded_string_helpers.from_string (pac_customer, to_type   => type_customer);
	function to_string   is new et_bounded_string_helpers.to_string   (pac_customer, from_type => type_customer);

	customer_default : constant type_customer := to_customer (not_assigned);


	partcode_length_max : constant positive := 100;
	package pac_partcode is new generic_bounded_length (partcode_length_max);

	type type_partcode is new pac_partcode.bounded_string;

	function to_partcode is new et_bounded_string_helpers.from_string (pac_partcode, to_type   => type_partcode);
	function to_string  is new et_bounded_string_helpers.to_string   (pac_partcode, from_type => type_partcode);

	partcode_default : constant type_partcode := to_partcode (not_assigned);


	drawing_number_length_max : constant positive := 100;
	package pac_drawing_number is new generic_bounded_length (drawing_number_length_max);

	type type_drawing_number is new pac_drawing_number.bounded_string;

	function to_drawing_number is new et_bounded_string_helpers.from_string (pac_drawing_number, to_type   => type_drawing_number);
	function to_string         is new et_bounded_string_helpers.to_string   (pac_drawing_number, from_type => type_drawing_number);

	drawing_number_default : constant type_drawing_number := to_drawing_number (not_assigned);


	revision_length_max : constant positive := 5;
	package pac_revision is new generic_bounded_length (revision_length_max);

	type type_revision is new pac_revision.bounded_string;

	function to_revision is new et_bounded_string_helpers.from_string (pac_revision, to_type   => type_revision);
	function to_string   is new et_bounded_string_helpers.to_string   (pac_revision, from_type => type_revision);

	revision_default : constant type_revision := to_revision (not_assigned);



	person_length_max : constant positive := 20;
	package pac_person is new generic_bounded_length (person_length_max);

	type type_person is new pac_person.bounded_string;

	function to_person is new et_bounded_string_helpers.from_string (pac_person, to_type   => type_person);
	function to_string is new et_bounded_string_helpers.to_string   (pac_person, from_type => type_person);

	person_default : constant type_person := to_person (not_assigned);







	type type_meta_basic is tagged record
		company			: type_company := company_default;
		customer		: type_customer := customer_default;
		partcode		: type_partcode := partcode_default;
		drawing_number	: type_drawing_number := drawing_number_default;
		revision		: type_revision := revision_default;
		drawn_by		: type_person := person_default;
		checked_by		: type_person := person_default;
		approved_by		: type_person := person_default;
		drawn_date		: time := clock;
		checked_date	: time := date_first; -- default 1901-01-01
		approved_date	: time := date_first; -- default 1901-01-01
	end record;

	-- CS accessor subprograms to set and get company, customer, ...






	type type_meta_schematic is new type_meta_basic with record
		preferred_libs	: pac_library_paths_schematic.list;
	end record;


	procedure set_device_libraries (
		meta : in out type_meta_schematic;
		libs : in pac_library_paths_schematic.list);


	procedure add_device_library (
		meta : in out type_meta_schematic;
		lib  : in type_library_path_schematic);


	procedure remove_device_library (
		meta : in out type_meta_schematic;
		lib  : in type_library_path_schematic);


	function get_device_libraries (
		meta : in type_meta_schematic)
		return pac_library_paths_schematic.list;





	type type_meta_board is new type_meta_basic with record
		preferred_libs	: pac_library_paths_board.list;
	end record;

	procedure set_device_libraries (
		meta : in out type_meta_board;
		libs : in pac_library_paths_board.list);


	procedure add_device_library (
		meta : in out type_meta_board;
		lib  : in type_library_path_board);


	procedure remove_device_library (
		meta : in out type_meta_board;
		lib  : in type_library_path_board);


	function get_device_libraries (
		meta : in type_meta_board)
		return pac_library_paths_board.list;





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
