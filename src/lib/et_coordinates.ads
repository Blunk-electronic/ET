------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        SCHEMATIC COORDINATES                             --
--                                                                          --
--                               S p e c                                    --
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
	pragma assertion_policy (check);
	
	-- There are drawings with the origin at the upper left corner (used by KiCad, ...).
	-- There are also drawings the the origin at the lower left corner (used by EAGLE, ...)
	type type_Y_axis_positive is (UPWARDS, DOWNWARDS);

	-- This flag is set on import or export of designs in accordance to the desired CAD format.
	-- Geometric operations like rotating require this flag.
	Y_axis_positive : type_Y_axis_positive := UPWARDS;
	
	-- The total distance between two objects:
	--	type type_distance is digits 9 range -100_000_000.0 .. 100_000_000.0; -- unit is metric millimeter
	type type_distance is delta 0.01 range -100_000_000.00 .. 100_000_000.00; -- CS rename to type_distance_total
	for type_distance'small use 0.01; -- this is the accuracy required for schematic


	-- The x and y position of an object:
	subtype type_distance_xy is type_distance range -10_000_000.0 .. 10_000_000.0; -- unit is metric millimeter, CS rename to type_distance
	zero_distance : constant type_distance := 0.0;

	-- instantiation of the 2d geometry package:
	package geometry is new et_geometry.geometry_operations_2d (type_distance);
	use geometry;
	
	subtype type_catch_zone is type_distance range 0.0 .. 10.0;
	catch_zone : type_catch_zone := 2.0; -- CS: should be a system setting in the future
	
	function to_distance (distance : in string) return type_distance_xy;	

	function to_string (distance : in type_distance) return string;
	-- Returns the given distance as a string.



	
	-- Angles/rotations are to be interpreted as: 
	-- positive rotation -> counter clock wise
	-- negative rotation -> clock wise
	rotation_delta : constant := 90;
	rotation_min : constant := -270;
	rotation_max : constant :=  270;
	pragma assertion_policy (check);		
	subtype type_rotation is integer range rotation_min .. rotation_max 
		with dynamic_predicate => type_rotation mod rotation_delta = 0;

    rotation_zero : type_rotation := 0;
    
	rotation_relative_min : constant type_rotation := -90;
	rotation_relative_max : constant type_rotation := 180;	
	subtype type_rotation_relative is type_rotation range rotation_relative_min .. rotation_relative_max;

	rotation_text_min : constant :=  0;
	rotation_text_max : constant := 90;
	subtype type_rotation_text is type_rotation range rotation_text_min .. rotation_text_max;
	-- CS: make use of this type by membership tests when required

	function to_string (angle : in type_rotation) return string;
	function to_angle (angle : in string) return type_rotation;

	function add (left, right : in type_rotation) return type_rotation;
	-- Adds two angles.
	-- If result greater or equal 360 degree then 360 degree is subtracted from result.
	-- If result less or equal 360 degree then 360 degree is added to the result.

	
	
	--type type_point is tagged private;
-- 	type type_point is new geometry.type_point with private;

-- 	zero : constant type_point;

	function "<" (left, right : in type_point) return boolean;
	
	axis_separator		: constant string (1..1) := "/";
	--position_preamble	: constant string (1..15) := "position (x" & axis_separator & "y) ";
	position_preamble	: constant string (1..11) := " pos (x" & axis_separator & "y) ";
	--position_preamble	: constant string (1..6) := "(x" & axis_separator & "y) ";
	
	function to_string (point : in type_point'class) return string;
	-- Returns the given point coordinates to a string.

-- 	function distance_x (point : in type_point) return type_distance_xy;
-- 	-- Returns the x distance of point from the drawing origin.
-- 	-- CS remove and use function distance instead.	
-- 	
-- 	function distance_y (point : in type_point) return type_distance_xy;
-- 	-- Returns the y distance of point from the drawing origin.
-- 	-- CS remove and use function distance instead.	

-- 	function distance (
-- 	-- Returns the distance of the point in x or y from the origin.
-- 		axis	: in type_axis_2d;
-- 		point	: in type_point'class)
-- 		return type_distance_xy;
	
-- 	procedure set_x (point : in out type_point; x : in type_distance_xy);
-- 	-- Assigns a point the given x position.
-- 	
-- 	procedure set_y (point : in out type_point'class; y : in type_distance_xy);
	
-- 	procedure set_xy (
-- 		point	 : in out type_point;
-- 		position : in type_point'class);

-- 	function set_point (x, y : in type_distance_xy) return type_point'class;
	
-- 	procedure mirror (
-- 		point	: in out type_point;
-- 		axis	: in type_axis_2d);	

-- 	procedure move (
-- 	-- moves x/y of given point by x/y of given offset
-- 		point	: in out type_point;
-- 		offset	: in type_point'class);

	procedure rotate (
	-- Rotates the given point by the given angle with the origin as center.
		point	: in out type_point'class;
		angle	: in type_rotation);

-- 	function distance (point_1, point_2 : in type_point) return type_distance;
-- 	-- Returns the total distance between the given points.

	function distance (
	-- Returns the absolute distance on the given axis between the given points.
		point_1	: in type_point;
		point_2	: in type_point;
		axis	: in type_axis_2d) 
		return type_distance;
	

	-- When handling hierachic structures we use a separator.
	-- Example: net name "HEATER_CONTROL/DRIVER/CLK"
	--hierarchy_separator : constant string (1..1) := ".";
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
	

	type type_coordinates is new type_point with private;
	type type_coordinates_relative is new type_point with private;

	function "<" (left, right : in type_coordinates) return boolean;
	-- Returns true if left comes before right.
	-- Returns false if left equals right.	
	
	procedure move (
		position	: in out type_coordinates'class;
		offset		: in type_coordinates_relative);
	
	function to_coordinates (
		point 	: in type_point'class;
		sheet	: in type_sheet)
		return type_coordinates;



	function to_coordinates_relative (
		point 	: in type_point'class;
		sheet	: in type_sheet_relative)
		return type_coordinates_relative;
	
	zero_position : constant type_coordinates;

	coordinates_preamble_xy : constant string (1..11) := " pos "
		& "(x"
		& axis_separator
		& "y) ";
	
	coordinates_preamble_sheet : constant string (1..17) := " pos "
		& "(sheet"
		& axis_separator
		& "x"
		& axis_separator
		& "y) ";

	coordinates_preamble_module : constant string (1..22) := " pos "
		& "(path"
		& axis_separator
		& "sheet"
		& axis_separator
		& "x"
		& axis_separator
		& "y) ";

	function to_string (position : in type_coordinates) return string;
	
	function sheet (position : in type_coordinates) return type_sheet;

	procedure set_sheet (position : in out type_coordinates; sheet : in type_sheet);
	-- Sets the sheet number in given position.

	-- PAPER SIZES
	-- As default we assume landscape format for all sheets.
	paper_size_A3_x : constant et_coordinates.type_distance := 420.0; -- CS use a common anchestor type and default value with sizes defined in et_pcb_coordinates.ads.
	paper_size_A3_y : constant et_coordinates.type_distance := 297.0;
	
	paper_size_A4_x : constant et_coordinates.type_distance := 297.0;
	paper_size_A4_y : constant et_coordinates.type_distance := 210.0;

	function paper_dimension (
	-- Returns for the given paper size, orientation and axis the correspoinding size in mm.
		paper_size	: in et_general.type_paper_size;
		orientation	: in et_general.type_paper_orientation := et_general.LANDSCAPE;
		axis		: in type_axis_2d)
		return type_distance_xy;

	
	private 
		-- In general every object has at least x,y coordinates.
-- 		type type_point is tagged record
-- 			x, y : type_distance_xy := zero_distance;
-- 		end record;

-- 		type type_point is new geometry.type_point with null record;
		
		--zero : constant type_point := (x => zero_distance, y => zero_distance);
-- 		zero : constant type_point := (geometry.origin with others => <>);

	
		type type_coordinates is new type_point with record
			sheet : type_sheet := type_sheet'first;
		end record;

		type type_coordinates_relative is new type_point with record
			sheet : type_sheet_relative := 0;
		end record;
		
-- 		zero_position : constant type_coordinates := (
-- 			sheet	=> type_sheet'first,
-- 			x		=> 0.0,
-- 			y		=> 0.0 );

		zero_position : constant type_coordinates := (
			origin with sheet => type_sheet'first);
		
end et_coordinates;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
