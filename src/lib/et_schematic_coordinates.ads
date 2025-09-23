------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        SCHEMATIC COORDINATES                             --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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

with ada.characters;			use ada.characters;

with et_schematic_geometry;			use et_schematic_geometry;
with et_sheets;						use et_sheets;
with et_coordinates_formatting;		use et_coordinates_formatting;
with et_string_processing;			use et_string_processing;

-- with system.assertions;


package et_schematic_coordinates is

	use pac_geometry_2;

	
-- 	pragma assertion_policy (check);


	-- When handling hierachic structures we use a separator.
	-- Example: net name "HEATER_CONTROL/DRIVER/CLK"
	hierarchy_separator : constant string (1..1) := "/";
	
	

-- POSITION OF AN OBJECT:
	
	type type_object_position is new pac_geometry_2.type_position with private;
	
	type type_object_position_relative is new pac_geometry_2.type_position with private;


	
	greatest_position : constant type_object_position;

	
	function "<" (left, right : in type_object_position) 
		return boolean;

	
	procedure move (
		position	: in out type_object_position'class;
		offset		: in type_object_position_relative);

	
	
	function to_position (
		point 		: in type_vector_model;
		sheet		: in type_sheet;
		rotation	: in type_rotation_model := zero_rotation)
		return type_object_position;

	
	function to_position_relative (
		point 		: in type_vector_model;
		sheet		: in type_sheet_relative;
		rotation	: in type_rotation_model := zero_rotation)		
		return type_object_position_relative;
	
	zero_position : constant type_object_position;

	

	-- This function returns the given object position
	-- as string formatted as follows:
	-- FORMAT_1 : sheet/x/y/rotation 2 / 4.5 / 5.6 / 90.0
	-- FORMAT_2 : sheet 2 x 4.5 y 5.6 rotation 90.0
	-- FORMAT_3 : 2 4.5 5.6 90.0
	overriding function to_string (
		position	: in type_object_position;
		format		: in type_output_format := FORMAT_1)
		return string;


	-- Reads a line like "position sheet 3 x 44.5 y 53.5 rotation 90.0"
	-- starting at a field given by "from" and returns
	-- an object position:
	-- CS should be a procedure with an error flag output
	-- and the position output ?
	overriding function to_position (
		line : in type_fields_of_line;
		from : in type_field_count_positive)
		return type_object_position;

	
	overriding function to_position (
		line : in type_fields_of_line;
		from : in type_field_count_positive)
		return type_object_position_relative;


	
	
-- PLACE (X/Y):
	
	-- Returns x and y of a given object position:
	function get_place (
		position : in type_object_position)
		return type_vector_model;


	procedure set_place (
		position 	: in out type_object_position;
		place		: in type_vector_model);
	

	
-- ROTATION:
	
	function get_rotation (
		position : in type_object_position) 
		return type_rotation_model;


	procedure set_rotation (
		position 	: in out type_object_position;
		rotation	: in type_rotation_model);

	
	
-- SHEET:

	-- The current active sheet:
	active_sheet : type_sheet := type_sheet'first;

	
	-- Returns the sheet number of the given position:
	function get_sheet (
		position : in type_object_position) 
		return type_sheet;


	-- Sets the sheet number in given position:
	procedure set_sheet (
		position	: in out type_object_position;
		sheet		: in type_sheet);



	
	private 

		type type_object_position is new pac_geometry_2.type_position with record
			sheet : type_sheet := type_sheet'first;
		end record;

		type type_object_position_relative is new pac_geometry_2.type_position with record
			sheet : type_sheet_relative := 0;
		end record;

		
		zero_position : constant type_object_position := (
			origin_zero_rotation with sheet => type_sheet'first);

		
		-- A position in a schematic which is on the
		-- last possible sheet and the greatest distance in
		-- x and y from the origin:
		greatest_position : constant type_object_position := (
			far_upper_right_zero_rotation with sheet => type_sheet'last);

		
end et_schematic_coordinates;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
