------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         BASIC OBJECTS / ARC                              --
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


package body et_basic_objects.arc is


	procedure set_A (
		arc	: in out type_arc;
		A	: in type_vector_model)
	is begin
		arc.A := A;
	end set_A;





	procedure set_B (
		arc	: in out type_arc;
		B	: in type_vector_model)
	is begin
		arc.B := B;
	end set_B;


	function get_A (
		arc : in type_arc)
		return type_vector_model
	is (arc.A);



	function get_B (
		arc : in type_arc)
		return type_vector_model
	is (arc.B);



	function get_direction (
		arc : in type_arc)
		return type_direction_of_rotation
	is (arc.direction);



	procedure set_direction (
		arc			: in out type_arc;
		direction	: in type_direction_of_rotation)
	is begin
		arc.direction := direction;
	end set_direction;





	procedure reset_arc (
		arc		: in out type_arc)
	is begin
		arc := (others => <>);
	end reset_arc;



	function to_arc (
		center		: in type_vector_model;
		A			: in type_vector_model;
		B			: in type_vector_model;
		direction	: in type_direction_of_rotation)
		return type_arc'class
	is
		arc : type_arc;
	begin
		set_place (arc, center);
		set_direction (arc, direction);
		set_A (arc, A);
		set_B (arc, B);

		-- arc status assumes default

		-- CS consistence check !!
		return arc;
	end to_arc;





	function to_arc_fine (
		arc : in type_arc)
		return type_arc_fine
	is (to_arc_fine (
		center		=> to_vector (arc.place),
		A			=> to_vector (arc.A),
		B			=> to_vector (arc.B),
		direction	=> arc.direction));



	function to_arc_coarse (
		arc : in type_arc_fine)
		return type_arc'class
	is
		result : type_arc;
	begin
		result := (
			place		=> to_vector_model (get_center (arc)),
			A			=> to_vector_model (get_A (arc)),
			B			=> to_vector_model (get_B (arc)),
			direction	=> get_direction (arc),
			others		=> <>);

		return result;
	end to_arc_coarse;




	function to_string (
		arc : in type_arc)
		return string
	is ("A: " & to_string (arc.A)
		& " B: " & to_string (arc.B)
		& " C: " & to_string (arc.place)
		& " D: " & to_string (arc.direction));




	function get_intersection (
		arc		: in type_arc;
		line	: in type_line_vector)
		return type_intersection_of_line_and_circle
	is
		A : constant type_arc_fine := to_arc_fine (arc);
	begin
		return get_intersection (A, line);
	end get_intersection;




	function is_selected_2 (
		arc : in type_arc)
		return boolean
	is
		result : constant boolean := false;
	begin
		-- CS
		return result;
	end is_selected_2;





	function reverse_arc (arc : in type_arc) return type_arc'class is
		result : type_arc := arc;
	begin
		result.A := arc.B;
		result.B := arc.A;

		case arc.direction is
			when CW  => result.direction := CCW;
			when CCW => result.direction := CW;
		end case;

		return result;
	end reverse_arc;



	procedure reverse_arc (arc : in out type_arc) is
		scratch : constant type_vector_model := arc.A;
	begin
		arc.A := arc.B;
		arc.B := scratch;

		case arc.direction is
			when CW	 => arc.direction := CCW;
			when CCW => arc.direction := CW;
		end case;
	end reverse_arc;



	function normalize_arc (arc : in type_arc) return type_arc'class is
	begin
		case arc.direction is
			when CW  => return reverse_arc (arc);
			when CCW => return arc;
		end case;
	end normalize_arc;



	function zero_length (arc : in type_arc)
		return boolean
	is (arc.A = arc.B);




	procedure mirror_arc (
		arc			: in out type_arc;
		axis		: in type_mirror)
	is begin
		mirror_point (arc.place, axis);
		mirror_point (arc.A, axis);
		mirror_point (arc.B, axis);
		arc.direction := reverse_direction (arc.direction);
	end mirror_arc;



	procedure rotate_arc_by (
		arc			: in out type_arc;
		rotation	: in type_rotation)
	is begin
		rotate_by (arc.place, rotation);
		rotate_by (arc.A, rotation);
		rotate_by (arc.B, rotation);
	end rotate_arc_by;



	function get_radius_start (
		arc : in type_arc)
		return type_float_positive
	is (get_distance_absolute (arc.place, arc.A));



	function get_radius_end (
		arc : in type_arc)
		return type_float_positive
	is (get_distance_absolute (arc.place, arc.B));



	function is_valid (
		arc : in type_arc)
		return boolean
	is
		rs : constant type_float_positive := get_radius_start (arc);
		re : constant type_float_positive := get_radius_end (arc);
	begin
		-- put_line (to_string (arc));
		-- put_line ("rs " & to_string (rs));
		-- put_line ("re " & to_string (re));

		if rs = re then

			if rs > 0.0 then
				return true;
			else
				return false;
			end if;
		else
			return false;
		end if;
	end is_valid;





	function get_arc_B (
		center	: in type_vector_model;
		A		: in type_vector_model;
		angle	: in type_angle) -- unit is degrees
		return type_vector_model
	is
		arc : type_arc;

		radius : type_float;
		angle_start, angle_end : type_angle; -- CS type_angle_positive ?
		end_x, end_y : type_float;

		use pac_float_numbers_functions;
	begin
		-- build an arc from the information available
		arc := (
			place		=> center,
			A			=> A,
			B			=> origin, -- not determined yet
			direction	=> get_direction (angle),
			others		=> <>);

		-- move arc so that its center is at 0/0
		move_to (arc, origin);

		-- calculate the radius of the arc
		radius := type_float (get_distance_absolute (arc.place, arc.A));

		-- calculate the angle where the arc begins:

		-- NOTE: If x and y are zero then the arctan operation is not possible.
		-- In this case we assume the resulting angle is zero.
		if get_x (arc.A) = zero and get_y (arc.A) = zero then
			angle_start := 0.0;
		else
			angle_start := arctan (
					y => type_float (get_y (arc.A)),
					x => type_float (get_x (arc.A)),
					cycle => units_per_cycle);
		end if;

		-- the angle where the arc ends:
		angle_end := angle_start + angle;

		-- The end point of the arc:
		end_y := sin (angle_end, units_per_cycle) * radius;
		end_x := cos (angle_end, units_per_cycle) * radius;

		return set (
			--x	=> type_distance (end_x), -- CS
			--y	=> type_distance (end_y)); -- CS
			x	=> to_distance (end_x),
			y	=> to_distance (end_y));

	end get_arc_B;







	function to_arc_angles (
		arc : in type_arc)
		return type_arc_angles
	is
	-- The angles may be negative. For example instead of 270 degree
	-- the angle can be -90 degree.
		result : type_arc_angles;

		-- Take a copy of the given arc in arc_tmp.
		arc_tmp : type_arc := arc;

		use pac_float_numbers_functions;
	begin
		-- move arc_tmp so that its center is at 0/0
		move_to (arc_tmp, origin);

		-- the center is not changed:
		set_center (result, to_vector (arc.place));

		-- calculate the radius of the arc
		set_radius (result, get_distance_absolute (
			arc_tmp.place, to_vector (arc_tmp.A)));

		-- calculate the angles where the arc begins and ends:

		-- NOTE: If x and y are zero then the arctan operation is not possible.
		-- In this case we assume the resulting angle is zero.

		if get_x (arc_tmp.A) = zero and get_y (arc_tmp.A) = zero then
			set_angle_start (result, 0.0);
		else
			set_angle_start (result, arctan (
					y => type_float (get_y (arc_tmp.A)),
					x => type_float (get_x (arc_tmp.A)),
					cycle => units_per_cycle));
		end if;

		if get_x (arc_tmp.B) = zero and get_y (arc_tmp.B) = zero then
			set_angle_end (result, 0.0);
		else
			set_angle_end (result, arctan (
					y => type_float (get_y (arc_tmp.B)),
					x => type_float (get_x (arc_tmp.B)),
					cycle => units_per_cycle));
		end if;

		-- make sure start and end angle are not equal
		if get_angle_start (result) = get_angle_end (result) then
			raise constraint_error; -- CS warning instead ?
		end if;

		-- direction is not changed:
		set_direction (result, arc.direction);

		return result;
	end to_arc_angles;






	function get_shortest_distance (
		arc		: in type_arc;
		point	: in type_vector_model)
		return type_distance_polar
	is
		A : constant type_arc_fine := to_arc_fine (arc);
		P : constant type_vector := to_vector (point);
	begin
		return get_shortest_distance (A, P);
	end get_shortest_distance;




	function get_shortest_distance (
		arc		: in type_arc;
		point	: in type_vector)
		return type_float_positive
	is
		A : constant type_arc_fine := to_arc_fine (arc);
		P : type_vector renames point;
	begin
		return get_absolute (get_shortest_distance (A, P));
	end get_shortest_distance;




	function on_arc (
		arc		: in type_arc;
		point	: in type_vector_model)
		return boolean
	is (on_arc (to_arc_fine (arc), to_vector (point)));






	function in_area (
		arc		: in type_arc;
		area	: in type_area)
		return boolean
	is
		result : boolean := false;
	begin
		-- We regard the arc as "in the area" if
		-- its start point or its end point lies in
		-- the given area:
		if in_area (get_A (arc), area)
		or in_area (get_B (arc), area) then
			result := true;
		else
			result := false;
		end if;

		return result;
	end in_area;




	function in_catch_zone (
		zone	: in type_catch_zone;
		arc		: in type_arc;
		width	: in type_distance_positive := 0.0)
		return boolean
	is
		-- debug : boolean := true;
		debug : constant boolean := false;

		distance : type_float;
		distance_polar : type_distance_polar;
	begin
		distance_polar := get_shortest_distance (
			to_arc_fine (arc), to_vector (get_center (zone)));

		if debug then
			put_line ("in_catch_zone arc");
			put_line ("dp " & to_string (distance_polar));
		end if;

		distance := get_absolute (distance_polar);

		distance := distance - type_float_positive (width);
		-- Due to the linewidth, the distance can also
		-- assume a negative value.

		if distance <= get_radius (zone) then
			return true;
		else
			return false;
		end if;
	end in_catch_zone;



end et_basic_objects.arc;

