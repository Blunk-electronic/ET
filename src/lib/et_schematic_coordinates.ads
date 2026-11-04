------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        SCHEMATIC COORDINATES                             --
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


with et_schematic_geometry;			use et_schematic_geometry;
with et_sheets;						use et_sheets;
with et_coordinates_formatting;		use et_coordinates_formatting;
with et_string_processing;			use et_string_processing;

-- with system.assertions;

package et_schematic_coordinates is

	use pac_geometry_2;
	use pac_basic_objects;

--	pragma assertion_policy (check);


	-- When handling hierachic structures we use a separator.
	-- Example: net name "HEATER_CONTROL/DRIVER/CLK"
	hierarchy_separator : constant string := "/";






-- POSITION OF AN OBJECT IN THE SCHEMATIC:


	type type_schematic_object
		is new type_object_with_rotation with private;


	type type_object_position_relative_2
		is new type_object_with_rotation with private;


	-- function to_schematic_object (
	-- 	object	: in type_object_with_rotation;
	-- 	sheet	: in type_sheet)
	-- 	return type_schematic_object'class;


	-- CS make a function that assigns a the elements of a type_schematic_object
	-- to a type_schematic_object and its derivatives.


	function to_position (
		point		: in type_vector_model;
		sheet		: in type_sheet;
		rotation	: in type_rotation_model := zero_rotation)
		return type_schematic_object'class;


	function get_sheet (
		object : in type_schematic_object)
		return type_sheet;


	procedure set_sheet (
		object	: in out type_schematic_object;
		sheet	: in type_sheet);


	-- Moves an object by the given number of sheets:
	procedure move_by_sheets (
		object	: in out type_schematic_object;
		offset	: in type_sheet_relative);


	function "<" (
		left, right : in type_schematic_object)
		return boolean;


	-- Resets the object. Sets its sheet to 1:
	overriding procedure reset_position (
		object	: in out type_schematic_object);



	-- Reads a line like "position sheet 3 x 44.5 y 53.5 rotation 90.0"
	-- and assigns the position to the object:
	overriding procedure set_position (
		object	: in out type_schematic_object;
		line	: in type_fields_of_line;
		from	: in type_field_count_positive);



	-- This function returns the given object position
	-- as string formatted as follows:
	-- FORMAT_1 : sheet/x/y/rotation 2 / 4.5 / 5.6 / 90.0
	-- FORMAT_2 : sheet 2 x 4.5 y 5.6 rotation 90.0
	-- FORMAT_3 : 2 4.5 5.6 90.0
	overriding function get_position (
		object	: in type_schematic_object;
		format	: in type_output_format := FORMAT_1)
		return string;



	-- Returns true if the given object is in
	-- the given catch zone:
	function in_catch_zone (
		object	: in type_schematic_object;
		zone	: in type_catch_zone;
		sheet	: in type_sheet)
		return boolean;


	-- Returns true if the given object is
	-- on the given sheet and in the given
	-- rectangular area:
	function on_sheet_and_in_area (
		object	: in type_schematic_object;
		sheet	: in type_sheet;
		area	: in type_area)
		return boolean;


	-- Returns true if the given object is
	-- on the given sheet and if it is selected:
	function on_sheet_and_selected (
		object	: in type_schematic_object;
		sheet	: in type_sheet)
		return boolean;




	function get_offset (
		left, right : in type_schematic_object'class)
		return type_object_position_relative_2;





-- SHEET:

	-- The current active sheet:
	active_sheet : type_sheet := type_sheet'first;


	function get_sheet (
		position : in type_object_position_relative_2)
		return type_sheet_relative;



private


	type type_schematic_object
		is new type_object_with_rotation with record
			sheet : type_sheet := type_sheet'first;
		end record;


	type type_object_position_relative_2
		is new type_object_with_rotation with record
			sheet : type_sheet_relative := 0;
		end record;


end et_schematic_coordinates;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
