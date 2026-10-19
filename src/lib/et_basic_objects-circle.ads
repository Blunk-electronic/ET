------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                       BASIC OBJECTS / CIRCLE                             --
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
--
--  To Do:
--
--

-- with et_coordinates_formatting;		use et_coordinates_formatting;



generic


package et_basic_objects.circle is
	use pac_geometry;


	type type_circle is new type_object with record
		radius : type_distance_positive := 0.0;
	end record;


	-- Mirrors the center of a circle along the given axis.
	-- CS: no need. use inherited mirror_along_axis
	procedure mirror_circle (
		circle		: in out type_circle;
		axis		: in type_mirror)
		renames mirror_along_axis;


	-- Rotates the center of acirlce about the origin:
	-- CS: no need. use inherited rotate_about_origin
	procedure rotate_circle_by (
		circle		: in out type_circle;
		rotation	: in type_rotation)
		renames rotate_about_origin;



-- RADIUS:

	function to_radius (
		r : in string)
		return type_distance_positive;


	function to_radius (
		r : in type_distance_positive)
		return type_float_positive;


	function get_radius (
		circle : in type_circle)
		return type_distance_positive;


	procedure set_radius (
		circle	: in out type_circle;
		radius	: in type_distance_positive);


	function to_circle (
		center	: in type_vector_model; -- CS rename to center ?
		radius	: in type_distance_positive)
		return type_circle'class; -- CS class required ?


	-- Resets the center to origin
	-- and radius to zero:
	procedure reset_circle (
		c : in out type_circle);


	function to_circle_fine (
		circle : in type_circle)
		return type_circle_fine;



	-- Returns the center and radius of the given circle as string
	-- in the form "C: x/y R: r". C is the center, R is the radius:
	function to_string (
		circle : in type_circle)
		-- CS: parameter for format
		return string;


	function get_intersection (
		circle	: in type_circle;
		line	: in type_line_vector)
		return type_intersection_of_line_and_circle;


	function get_shortest_distance (
		circle	: in type_circle;
		point	: in type_vector_model)
		return type_distance_polar;


	function get_shortest_distance (
		circle	: in type_circle;
		point	: in type_vector)
		return type_float_positive;



	-- This function returns true if the given circle
	-- is in the given area.
	-- "In the area" means: The center of the
	-- circle is in the area:
	function in_area (
		circle	: in type_circle;
		area	: in type_area)
		return boolean;
	-- CS: an additional parameter could be used
	-- to test if the circle circumfence crosses the area



	-- Returns true if the given circle is in the given catch zone.
	-- Optionally the linewidth can be specified so that it
	-- is taken into account:
	function in_catch_zone (
		zone	: in type_catch_zone;
		circle	: in type_circle;
		width	: in type_distance_positive := 0.0)
		return boolean;


end et_basic_objects.circle;

