------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET COORDINATES                          --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 Mario Blunk, Blunk electronic                 --
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

-- with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;
-- 
with ada.containers.doubly_linked_lists;
-- with ada.numerics.generic_elementary_functions;
-- with ada.containers.indefinite_doubly_linked_lists;
-- with ada.containers.ordered_maps;
-- with ada.containers.indefinite_ordered_maps;
-- with ada.containers.ordered_sets;

--with et_string_processing;

package et_coordinates is

	

	type type_axis is (X, Y);

-- 	type type_unit_metric is (micrometer, millimeter, centimeter, meter);
-- 	type type_unit_imperial is (mil, inch);

	type type_distance is delta 0.01 range -100000000.0 .. 100000000.0; -- unit is metric millimeter
	zero_distance : constant type_distance := 0.0;


	-- CS: type_grid ?

	function mil_to_distance (mil : in string) return type_distance;
	-- Returns the given mils to type_distance.
	
	function to_string (distance : in type_distance) return string;
	-- Returns the given distance to a string.


	units_per_cycle : constant float := 360.0;
	
	-- Angles are to be interpreted as: 
	-- positive angle -> counter clock wise
	-- negative angle -> clock wise
	type type_angle is delta 0.1 digits 4 range -359.9 .. 359.9; -- unit is degrees
	-- CS: a type that allows angles of multiples of 45 degrees ? 
	-- or check angle via separate function when required ?
	
	subtype type_angle_90 is type_angle range 0.0 .. 90.0;
	-- CS: make use of this type by membership tests when required

	function to_string (angle : in type_angle) return string;
	-- Returns the given angle as string. 

	procedure warning_angle_greater_90_degrees;


	
	
	type type_2d_point is tagged private;

	zero : constant type_2d_point;

	axis_separator		: constant string (1..1) := "/";
 	position_preamble	: constant string (1..15) := "position (x" & axis_separator & "y) ";
	
	function to_string (point : in type_2d_point) return string;
	-- Returns the given point coordinates to a string.

	function distance_x (point : in type_2d_point) return type_distance;
	function distance_y (point : in type_2d_point) return type_distance;	

	procedure set_x (point : in out type_2d_point; x : in type_distance);
	procedure set_y (point : in out type_2d_point; y : in type_distance);

	procedure set (point : in out type_2d_point; position : in type_2d_point);
	
	procedure mirror (
		point	: in out type_2d_point;
		axis	: in type_axis);	

	procedure move (
		point	: in out type_2d_point;
		offset	: in type_2d_point);

	procedure rotate (
	-- Rotates the given point around the origin.
		point	: in out type_2d_point;
		angle	: in type_angle);
	
	-- The name of a submodule may have 100 characters which seems sufficient for now.
 	submodule_name_length : constant natural := 100;
	package type_submodule_name is new generic_bounded_length (submodule_name_length); use type_submodule_name;
	
    -- The location of a submodule within the design hierarchy is reflected by
    -- a list of submodule names like motor_driver.counter.supply
    -- The first item in this list is the name of the top level module.
    package type_path_to_submodule is new doubly_linked_lists (
        element_type => type_submodule_name.bounded_string);

	type type_coordinates is new type_2d_point with private;
-- 	overriding
-- 	procedure mirror (
-- 		coordinates : in out type_coordinates;
-- 		axis		: in type_axis);
	
	zero_position : constant type_coordinates;
	
	coordinates_preamble : constant string (1..21) := "position " 
		& "(sheet"
		& axis_separator
		& "x"
		& axis_separator
		& "y) ";
	
	function to_string (position : in type_coordinates) return string;
	-- Returns the given position as string.

	function path   (position : in type_coordinates) return type_path_to_submodule.list;
	function module (position : in type_coordinates) return type_submodule_name.bounded_string;
	function sheet  (position : in type_coordinates) return positive;

	procedure set_module (position : in out type_coordinates; name : in type_submodule_name.bounded_string);
	-- Sets the module name in given position.

	procedure set_path (position : in out type_coordinates; path : in type_path_to_submodule.list);
	-- Sets the path in given position.

	procedure set_sheet (position : in out type_coordinates; sheet : in positive);
	-- Sets the sheet number in given position.

	
	private 
		-- In general every object has at least x,y coordinates.
		type type_2d_point is tagged record
			x, y : type_distance := zero_distance;
		end record;
		
		zero : constant type_2d_point := (x => zero_distance, y => zero_distance);

	
		type type_coordinates is new type_2d_point with record
			path            : type_path_to_submodule.list;
			module_name		: type_submodule_name.bounded_string;
			sheet_number	: positive;
		end record;

		zero_position : constant type_coordinates := (
			module_name		=> type_submodule_name.to_bounded_string (""),
			path			=> type_path_to_submodule.empty_list,
			sheet_number	=> 1,
			x				=> 0.0,
			y				=> 0.0 );
		
end et_coordinates;

-- Soli Deo Gloria
