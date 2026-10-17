------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BASIC OBJECTS / ARC                             --
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
with et_directions;					use et_directions;


generic


package et_basic_objects.arc is
	use pac_geometry;


	type type_arc is new type_object with record
		A, B		: type_vector_model; -- start and end point
		direction	: type_direction_of_rotation := CW;
		-- CS status_AB	: type_AB_status;
	end record;




	-- Sets the start point of the given arc:
	procedure set_A (
		arc	: in out type_arc;
		A	: in type_vector_model);


	-- Sets the end point of the given arc:
	procedure set_B (
		arc	: in out type_arc;
		B	: in type_vector_model);



	-- Returns the start point of the given arc:
	function get_A (
		arc : in type_arc)
		return type_vector_model;


	-- Returns the end point of the given arc:
	function get_B (
		arc : in type_arc)
		return type_vector_model;





	-- Returns the direction of the given arc:
	function get_direction (
		arc : in type_arc)
		return type_direction_of_rotation;


	-- Sets the direction of the given arc:
	procedure set_direction (
		arc			: in out type_arc;
		direction	: in type_direction_of_rotation);




	-- Sets A, B and center to origin.
	-- Sets direction to CCW:
	procedure reset_arc (
		arc		: in out type_arc);



	function to_arc (
		center		: in type_vector_model;
		A			: in type_vector_model;
		B			: in type_vector_model;
		direction	: in type_direction_of_rotation)
		return type_arc'class;




	function to_arc_fine (
		arc : in type_arc)
		return type_arc_fine;


	function to_arc_coarse (
		arc : in type_arc_fine)
		return type_arc'class;



	-- Returns the start, end point and angle of the given arc as string
	-- in the form "A: x/y B: x/y" C: x/y D: x/y).
	-- C is the center, D is the direction:
	function to_string (
		arc : in type_arc)
		-- CS: parameter for format
		return string;



	function get_intersection (
		arc		: in type_arc;
		line	: in type_line_vector)
		return type_intersection_of_line_and_circle;


	-- Returns true if the given arc is selected.
	-- If either the A or B end or the whole arc
	-- is set as selected:
	-- CS: not complete. returns always false
	function is_selected_2 (
		arc : in type_arc)
		return boolean;



	-- Swaps start and end point of an arc.
	-- Reverses the direction of the arc:
	function reverse_arc (arc : in type_arc) return type_arc'class;

	procedure reverse_arc (arc : in out type_arc);


	-- Changes the direction of an arc to CCW (mathematical sense)
	-- by swapping start and end point. If direction is already CCW
	-- then nothing happens.
	function normalize_arc (arc : in type_arc) return type_arc'class;

	-- Returns true if start and end point of arc are equal:
	function zero_length (arc : in type_arc) return boolean;





	-- Mirrors an arc along the given axis.
	-- A and B are also mirrored.
	-- Direction is reversed.
	-- CS: override mirror_along_axis instead ?
	procedure mirror_arc (
		arc			: in out type_arc;
		axis		: in type_mirror);


	-- Rotates an arc about the origin by the given rotation.
	-- Rotates also A and B:
	-- CS: override rotate_about_origin instead ?
	procedure rotate_arc_by (
		arc			: in out type_arc;
		rotation	: in type_rotation);





	-- Returns the distance between the start point
	-- and the center of the arc.
	function get_radius_start (
		arc : in type_arc)
		return type_float_positive;


	-- Returns the distance between the end point
	-- and the center of the arc.
	function get_radius_end (
		arc : in type_arc)
		return type_float_positive;



	-- Test whether the given arc is valid. The arc is valid if:
	-- - start and end point have equal distance to center
	-- - radius is greater zero
	function is_valid (
		arc : in type_arc)
		return boolean;



	-- Computes the end point of an arc
	-- based on a given center and start point:
	function get_arc_B (
		center	: in type_vector_model;
		A		: in type_vector_model;	-- start point
		angle	: in type_angle) -- CS: type_angle_positive ?
		return type_vector_model;




	-- Returns the start and end angles of an arc.
	function to_arc_angles (
		arc : in type_arc)
		return type_arc_angles;





	function get_shortest_distance (
		arc		: in type_arc;
		point	: in type_vector_model)
		return type_distance_polar;


	function get_shortest_distance (
		arc		: in type_arc;
		point	: in type_vector)
		return type_float_positive;




	function on_arc (
		arc		: in type_arc;
		point	: in type_vector_model)
		return boolean;



	-- This function returns true if the given arc
	-- is in the given area.
	-- "In the area" means: Either start or end of the
	-- arc (or both) are in the area:
	function in_area (
		arc		: in type_arc;
		area	: in type_area)
		return boolean;
	-- CS: an additional parameter could be used
	-- to test if the arc crosses the area



	-- Returns true if the given arc is in the given catch zone.
	-- Optionally the linewidth can be specified so that it
	-- is taken into account. If a linewidth greater zero
	-- is given, then we assume, that the arc has round caps:
	function in_catch_zone (
		zone	: in type_catch_zone;
		arc		: in type_arc;
		width	: in type_distance_positive := 0.0)
		return boolean;


end et_basic_objects.arc;

