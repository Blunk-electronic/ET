------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        SCHEMATIC COORDINATES                             --
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
with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;

with et_string_processing;
with et_general;				use et_general;
with et_geometry;

-- with system.assertions;

package et_coordinates is
-- 	pragma assertion_policy (check);
	
	
	-- The total distance between two objects:
	type type_distance is delta 0.01 range -100_000_000.00 .. 100_000_000.00; -- CS rename to type_distance_total
	for type_distance'small use 0.01; -- this is the accuracy required for schematic


	-- The x and y position of an object:
	subtype type_distance_xy is type_distance range -10_000_000.0 .. 10_000_000.0; -- unit is metric millimeter, CS rename to type_distance

	
	-- Angle or rotation is in mathematical sense, means:
	-- positive rotation -> counter clock wise
	-- negative rotation -> clock wise

	type type_rotation is delta 0.1 range -360.0 .. 360.0;
	for type_rotation'small use 0.1;
	
	-- instantiation of the geometry package:
	package pac_geometry_sch is new et_geometry.generic_pac_geometry (
		type_distance	=> type_distance,
		type_rotation	=> type_rotation);
	
	use pac_geometry_sch;
	
	subtype type_catch_zone is type_distance range 0.0 .. 10.0;
	catch_zone : type_catch_zone := 2.0; -- CS: should be a system setting in the future
	

	
-- 	rotation_delta : constant := 90;
-- 	rotation_min : constant := -270;
-- 	rotation_max : constant :=  270;
-- 	pragma assertion_policy (check);		
-- 	subtype type_rotation is integer range rotation_min .. rotation_max 
-- 		with dynamic_predicate => type_rotation mod rotation_delta = 0;
	
    
	rotation_relative_min : constant type_rotation := -90.0;
	rotation_relative_max : constant type_rotation := 180.0;	
	subtype type_rotation_relative is type_rotation range rotation_relative_min .. rotation_relative_max;

	-- When handling hierachic structures we use a separator.
	-- Example: net name "HEATER_CONTROL/DRIVER/CLK"
	hierarchy_separator : constant string (1..1) := "/";

	-- Sheets
	sheet_count_max : constant positive := 100;
	type type_sheet_relative is new integer range -(sheet_count_max) .. sheet_count_max;
	subtype type_sheet is type_sheet_relative range 1 .. type_sheet_relative'last;
	
	function to_sheet (sheet : in type_sheet) return string;
	function to_sheet (sheet : in string) return type_sheet;

	function to_sheet_relative (sheet : in type_sheet_relative) return string;
	function to_sheet_relative (sheet : in string) return type_sheet_relative;
	
	-- The whole schematic may have a total of x pages.
	schematic_page_count_max : constant positive := 100;
	type type_schematic_page_number is new positive range 1..schematic_page_count_max; -- CS: not used yet
	

	type type_position is new pac_geometry_sch.type_position with private;
	type type_position_relative is new pac_geometry_sch.type_position with private;

	function "<" (left, right : in type_position) return boolean;
	
	procedure move (
		position	: in out type_position'class;
		offset		: in type_position_relative);
	
	function to_position (
		point 		: in type_point'class;
		sheet		: in type_sheet;
		rotation	: in type_rotation := zero_rotation)
		return type_position;

	function to_position_relative (
		point 		: in type_point'class;
		sheet		: in type_sheet_relative;
		rotation	: in type_rotation := zero_rotation)		
		return type_position_relative;
	
	zero_position : constant type_position;


	function to_string (position : in type_position) return string;
	
	function sheet (position : in type_position) return type_sheet;

	procedure set_sheet (position : in out type_position; sheet : in type_sheet);
	-- Sets the sheet number in given position.


	
	private 

		type type_position is new pac_geometry_sch.type_position with record
			sheet : type_sheet := type_sheet'first;
		end record;

		type type_position_relative is new pac_geometry_sch.type_position with record
			sheet : type_sheet_relative := 0;
		end record;

		zero_position : constant type_position := (
			origin_zero_rotation with sheet => type_sheet'first);
		
end et_coordinates;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
