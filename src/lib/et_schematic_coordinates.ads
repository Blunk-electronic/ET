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

with et_geometry_1;
with et_geometry_1.et_polygons;
with et_geometry_1.et_polygons.offsetting;

with et_geometry_2a;
with et_geometry_2a.grid;
with et_geometry_2a.path;
with et_geometry_2a.contours;

with et_sheets;						use et_sheets;

-- with system.assertions;


package et_schematic_coordinates is
	
-- 	pragma assertion_policy (check);


	
	-- IMPORTANT: UNIT IS METRIC MILLIMETERS !!

	distance_digits_left  : constant := 5;
	distance_digits_right : constant := 2; -- 0.01mm
	
	distance_smallest : constant := 1.0 / (10 ** distance_digits_right);

	type type_distance_model is delta distance_smallest 
		digits distance_digits_left + distance_digits_right
		range - 0.1 * (10 ** distance_digits_left) .. 
			  + 0.1 * (10 ** distance_digits_left);


	
	-- Angle or rotation is in mathematical sense, means:
	-- positive rotation -> counter clock wise
	-- negative rotation -> clock wise

	rotation_digits_left  : constant := 3;
	rotation_digits_right : constant := 1;
 
	rotation_smallest : constant := 1.0 / (10 ** rotation_digits_right);
	type type_rotation_model is delta rotation_smallest
		digits rotation_digits_left + rotation_digits_right
		range -360.0 .. 360.0;
		-- CS range -360.0 + rotation_smallest .. +360.0 - rotation_smallest ?
 
 
	type type_float_model is digits 12;
	-- CS reduce digits. adapt accuracy
	-- when instantiating geometry package. See below.

	
	-- instantiation of the geometry 1 package:
	package pac_geometry_sch is new et_geometry_1 (
		type_float	=> type_float_model,

		-- For assumed greatest numbers of 999.999..
		-- we have 3 digits left and 9 digits right of comma.
		-- This leads to an accuracy of:											  
		accuracy	=> 1.0E-9
		-- CS: For numbers greater 999.9 this accuracy is useless.
		);
	
	use pac_geometry_sch;


	-- instantiation of the geometry 2 package:
	package pac_geometry_2 is new et_geometry_2a (
		pac_geometry_1			=> pac_geometry_sch,
		type_distance			=> type_distance_model,
		axis_max				=> 1_000.0,
		axis_min				=>  -100.0,
		type_rotation			=> type_rotation_model
		);
		
	use pac_geometry_2;


	package pac_grid is new pac_geometry_2.grid;
	package pac_path_and_bend is new pac_geometry_2.path;
	
	
	-- These packages are never used in schematic but are
	-- required for instantiation of some generic packages:
	package pac_contours is new pac_geometry_2.contours;
	package pac_polygons is new pac_geometry_1.et_polygons;
	package pac_polygon_offsetting is new pac_polygons.offsetting;


	-- In headless mode this accuracy should be used
	-- when locating objects inside a particual zone:
	accuracy_default : constant type_zone_radius := 2.0; 
	-- CS: should be a general setting for schematic and symbol editor in the future
	

	
-- 	rotation_delta : constant := 90;
-- 	rotation_min : constant := -270;
-- 	rotation_max : constant :=  270;
-- 	pragma assertion_policy (check);		
-- 	subtype type_rotation is integer range rotation_min .. rotation_max 
-- 		with dynamic_predicate => type_rotation mod rotation_delta = 0;
	
    
	rotation_relative_min : constant type_rotation_model := -90.0;
	rotation_relative_max : constant type_rotation_model := 180.0;	
	
	subtype type_rotation_relative is type_rotation_model range 
		rotation_relative_min .. rotation_relative_max;

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


	function to_string (
		position : in type_object_position) 
		return string;

	-- Returns something like "sheet 3 x 12.34 y 45.0".
	-- CS: merge this function with the to_string functin above
	-- using an argument for the desired output format.
	function get_position (
		pos : in type_object_position) 
		return string;


	-- Returns x and y of a given object position:
	function get_place (
		position	: in type_object_position)
		return type_vector_model;
	
	
	-- Returns the sheet number of the given position:
	function get_sheet (
		position	: in type_object_position) 
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
