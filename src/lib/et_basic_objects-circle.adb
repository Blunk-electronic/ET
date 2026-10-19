------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                       BASIC OBJECTS / CIRCLE                             --
--                                                                          --
--                               B o d y                                    --
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

with ada.text_io;						use ada.text_io;



package body et_basic_objects.circle is

	function to_radius (
		r : in string)
		return type_distance_positive
	is (type_distance_positive'value (r));


	function to_radius (
		r : in type_distance_positive)
		return type_float_positive
	is (type_float_positive (r));


	function get_radius (
		circle : in type_circle)
		return type_distance_positive
	is (circle.radius);


	procedure set_radius (
		circle	: in out type_circle;
		radius	: in type_distance_positive)
	is begin
		circle.radius := radius;
	end set_radius;




	function to_circle (
		center	: in type_vector_model;
		radius	: in type_distance_positive)
		return type_circle'class
	is
		circle : type_circle;
	begin
		circle.place := center;
		circle.radius := radius;

		return circle;
	end to_circle;



	procedure reset_circle (
		c : in out type_circle)
	is begin
		c := (others => <>);
	end reset_circle;


	function to_circle_fine (
		circle : in type_circle)
		return type_circle_fine
	is
		c : type_circle_fine;
	begin
		set_center (c, to_vector (get_place (circle)));
		set_radius (c, type_float_positive (get_radius (circle)));

		return c;
	end to_circle_fine;



	function to_string (
		circle : in type_circle)
		return string
	is ("C: " & to_string (get_place (circle))
		& " R: " & to_string (get_radius (circle)));



	function get_intersection (
		circle	: in type_circle;
		line	: in type_line_vector)
		return type_intersection_of_line_and_circle
	is
		C : constant type_circle_fine := to_circle_fine (circle);
	begin
		return get_intersection (C, line);
	end get_intersection;



	function get_shortest_distance (
		circle	: in type_circle;
		point	: in type_vector_model)
		return type_distance_polar
	is
		C : constant type_circle_fine := to_circle_fine (circle);
		P : constant type_vector := to_vector (point);
	begin
		return get_shortest_distance (C, P);
	end get_shortest_distance;




	function get_shortest_distance (
		circle	: in type_circle;
		point	: in type_vector)
		return type_float_positive
	is
		C : constant type_circle_fine := to_circle_fine (circle);
		P : type_vector renames point;
	begin
		return get_absolute (get_shortest_distance (C, P));
	end get_shortest_distance;



	function in_area (
		circle	: in type_circle;
		area	: in type_area)
		return boolean
	is
		result : boolean := false;
	begin
		-- We regard the circle as "in the area" if
		-- its center lies in
		-- the given area:
		if in_area (get_place (circle), area) then
			result := true;
		else
			result := false;
		end if;

		return result;
	end in_area;



	function in_catch_zone (
		zone	: in type_catch_zone;
		circle	: in type_circle;
		width	: in type_distance_positive := 0.0)
		return boolean
	is
		distance : type_distance;
	begin
		distance := get_distance (circle.place, get_center (zone));
		distance := distance - circle.radius - width;

		if distance <= type_distance_positive (get_radius (zone)) then
			return true;
		else
			return false;
		end if;
	end in_catch_zone;





	function get_bounding_box (
		circle	: in type_circle;
		width	: in type_distance_positive)
		return type_area
	is
		-- CS: Optimization required. Compiler options ?
		-- CS: output warning if radius is zero ?

		result : type_area;
		w : type_distance;

		d : constant type_distance := width / 2.0;
	begin
		w := 2.0 * (circle.radius + d);

		result.width := w;
		result.height := w;

		result.position.x := circle.place.x - w / 2.0;
		result.position.y := circle.place.y - w / 2.0;

		return result;
	end get_bounding_box;


	



	function get_bounding_box (
		circle		: in type_circle;
		width		: in type_distance_positive;
		offset_1	: in type_vector_model;
		offset_2	: in type_vector_model := origin;
		rotation	: in type_rotation_2;
		mirror		: in type_mirror := MIRROR_NO)
		return type_area
	is
		-- Make a copy of the given circle:
		c : type_circle := circle;

		b : type_area;
		unused_result : type_area;
	begin
		-- Rotate the center of the circle:
		rotate_circle_by (c, rotation);

		-- Mirror the circle:
		case mirror is
			when MIRROR_NO => null;
			when MIRROR_ALONG_X_AXIS  => mirror_circle (c, MIRROR_ALONG_X_AXIS);
			when MIRROR_ALONG_Y_AXIS  => mirror_circle (c, MIRROR_ALONG_Y_AXIS);
		end case;

		-- Move the circle by offset_1:
		move_by (c, offset_1);

		-- Move the arc by offset_2:
		move_by (c, offset_2);

		-- Get the bounding-box of arc:
		b := get_bounding_box (c, width);
		-- put_line ("b " & to_string (b));

		return b;
	end get_bounding_box;

end et_basic_objects.circle;

