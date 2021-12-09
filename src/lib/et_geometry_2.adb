------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              GEOMETRY 2                                  --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--


with ada.strings;				use ada.strings;
with ada.strings.fixed;			use ada.strings.fixed;
with ada.strings.unbounded;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with et_exceptions;				use et_exceptions;



package body et_geometry_2 is
	
	
	function to_string (status : in type_point_status) return string is begin
		return type_point_status'image (status);
	end to_string;

	
	procedure toggle_status (status : in out type_point_status) is begin
		case status is
			when OUTSIDE	=> status := INSIDE;
			when INSIDE		=> status := OUTSIDE;
		end case;
	end toggle_status;

	
	procedure union (
		boundaries	: in out type_boundaries;
		point		: in type_point) 
	is begin
		-- X axis
		if get_x (point) < boundaries.smallest_x then 
			boundaries.smallest_x := get_x (point); 
		end if;
		
		if get_x (point) > boundaries.greatest_x then
			boundaries.greatest_x := get_x (point); 
		end if;

		-- Y axis
		if get_y (point) < boundaries.smallest_y then
			boundaries.smallest_y := get_y (point);
		end if;
		
		if get_y (point) > boundaries.greatest_y then
			boundaries.greatest_y := get_y (point);
		end if;
	end;

	
	procedure union (
		left	: in out type_boundaries;
		right	: in type_boundaries) 
	is begin
		-- X axis
		-- smallest
		if right.smallest_x < left.smallest_x then
			left.smallest_x := right.smallest_x;
		end if;

		-- Y axis
		-- smallest
		if right.smallest_y < left.smallest_y then
			left.smallest_y := right.smallest_y;
		end if;

		-- X axis
		-- greatest
		if right.greatest_x > left.greatest_x then
			left.greatest_x := right.greatest_x;
		end if;

		-- Y axis
		-- greatest
		if right.greatest_y > left.greatest_y then
			left.greatest_y := right.greatest_y;
		end if;
	end union;


	
-- VECTOR OPERATIONS		

	function to_string (
		v	: in type_vector)
		return string
	is begin
		--return " x" & to_string (v.x) & " y" & to_string (v.y);
		return 
			" x" & type_float_internal'image (v.x) &
			" y" & type_float_internal'image (v.y);
		--& " z" & to_string (v.z);
	end to_string;

	
	function get_x (
		v	: in type_vector)
		return type_float_internal is
	begin
		return v.x;
	end get_x;
	
	function get_y (
		v	: in type_vector)
		return type_float_internal is
	begin
		return v.y;
	end get_y;

	function get_z (
		v	: in type_vector)
		return type_float_internal is
	begin
		return v.z;
	end get_z;


	function get_distance_total (
		point	: in type_point;
		vector	: in type_vector)
		return type_float_internal
	is 
		pv : constant type_vector := to_vector (point);
		
		dx : constant type_float_internal := abs (vector.x - pv.x);
		dy : constant type_float_internal := abs (vector.y - pv.y);
	begin
		return sqrt (dx ** 2.0 + dy ** 2.0);
	end get_distance_total;
	
	
	function get_distance (
		vector_one, vector_two : in type_vector)
		return type_distance_polar
	is
		result : type_distance_polar;

		dx : constant type_float_internal := type_float_internal (vector_two.x - vector_one.x);
		dy : constant type_float_internal := type_float_internal (vector_two.y - vector_one.y);

		abs_dx : constant type_float_internal := abs (dx);
		abs_dy : constant type_float_internal := abs (dy);
	begin
		--put_line ("dx " & float'image (abs_dx));
		--put_line ("dy " & float'image (abs_dy));
		
		set_absolute (result, to_distance (sqrt (abs_dx ** 2 + abs_dy ** 2)));
		
		-- NOTE: If the total distance between the location vectors is zero then
		-- the arctan operation is not possible. In this case we assume
		-- the resulting angle is zero.
		-- So we do the angle computation only if there is a distance between the vectors:
		if get_absolute (result) /= zero then
			
			set_angle (result, to_rotation (arctan (
					x 		=> dx,
					y		=> dy,
					cycle	=> units_per_cycle)));
		else
			-- distance is zero
			set_angle (result, zero_rotation);
		end if;
		
		return result;
	end get_distance;


	function get_distance (
		point	: in type_point;
		vector	: in type_vector)
		return type_distance_polar
	is
		v : constant type_vector := to_vector (point);
	begin
		return get_distance (v, vector);
	end get_distance;
	
	
	function move_by (
		v		: in type_vector;
		offset	: in type_distance_relative)
		return type_vector
	is
		result : type_vector := v;
	begin
		result.x := result.x + type_float_internal (offset.x); -- CS good idea ?
		result.y := result.y + type_float_internal (offset.y);
		return result;
	end move_by;


	procedure move_by (
		v			: in out type_vector;
		direction	: in type_rotation;
		distance	: in type_float_internal)
	is
		delta_x, delta_y : type_float_internal := 0.0;
	begin
		-- sin (direction) * distance = delta y
		-- cos (direction) * distance = delty x

		delta_y := sin (type_float_internal (direction), units_per_cycle) * distance;
		delta_x := cos (type_float_internal (direction), units_per_cycle) * distance;

		v.x := v.x + delta_x;
		v.y := v.y + delta_y;
	end move_by;

	
	function to_vector (
		point	: in type_point)
		return type_vector is
	begin
		return (
			x => type_float_internal (get_x (point)),
			y => type_float_internal (get_y (point)),
			z => 0.0
			);
	end to_vector;


	
	function to_point (
		v	: in type_vector)
		return type_point is
	begin
		--log (text => "to point: vector" & to_string (v));
		
		-- Since the return is a 2D point,
		-- the z component of v must be zero:
		if v.z /= 0.0 then
			raise constraint_error;
		end if;
					
		return type_point (set (
			x => to_distance (v.x),
			y => to_distance (v.y)));

		exception
			when constraint_error =>
				raise constraint_error 
					with "vector component too great:" & to_string (v);

	end to_point;

	
	function absolute (
		vector	: in type_vector)
		return type_float_internal
	is begin
		return
			sqrt (
				vector.x * vector.x + 
				vector.y * vector.y +
				vector.z * vector.z);

	end absolute;

	
	function scale (
		v	: in type_vector;
		s	: in type_float_internal)
		return type_vector
	is begin
		return (
			x => s * v.x,
			y => s * v.y,
			z => s * v.z
			);
	end scale;

	
	function add (
		a, b	: in type_vector)
		return type_vector
	is begin
		return (
			x => a.x + b.x,
			y => a.y + b.y,
			z => a.z + b.z);
	end add;

	
	function subtract (
		a, b	: in type_vector)
		return type_vector
	is begin
		return (
			x => a.x - b.x,
			y => a.y - b.y,
			z => a.z - b.z);
	end subtract;

	
	function cross_product (
		a, b	: in type_vector)
		return type_vector
	is begin
		return (
			x => a.y * b.z - a.z * b.y,
			y => a.z * b.x - a.x * b.z,
			z => a.x * b.y - a.y * b.x);
	end cross_product;

	
	function dot_product (
		a, b	: in type_vector)
		return type_float_internal
	is begin
		return (a.x * b.x  +  a.y * b.y  +  a.z * b.z);
	end dot_product;

	
	function mixed_product (
		a, b, c	: in type_vector)
		return type_float_internal
	is
		cp : type_vector;
	begin
		cp := cross_product (b, c);
		return dot_product (a, cp);
	end mixed_product;		

	
	function divide (
		a, b	: in type_vector)
		return type_float_internal
	is
		lambda : type_float_internal;
	begin
		-- It does not matter if we use
		-- the x,y or z component for this calculation.
		-- But we must skip the case when
		-- a division by zero is ahead.
		if b.x /= 0.0 then
			lambda := a.x / b.x;
		elsif b.y /= 0.0 then
			lambda := a.y / b.y;
		elsif b.z /= 0.0 then
			lambda := a.z / b.z;
		else
			put_line ("ERROR while vector division ");
		end if;
		
		return lambda;
	end divide;

	

	function start_vector (ray : in type_ray) 
		return type_vector
	is
		v : type_vector;
	begin
		v.x := ray.start_point.x;
		v.y := ray.start_point.y;
		v.z := 0.0;

		return v;
	end start_vector;

	
	function direction_vector (ray : in type_ray) 
		return type_vector
	is 
		v : type_vector;
	begin
		-- x = cos (direction) * 1
		v.x := cos (type_float_internal (ray.direction), units_per_cycle);

		-- y = sin (direction) * 1
		v.y := sin (type_float_internal (ray.direction), units_per_cycle);

		v.z := 0.0; -- we are in a 2D world
		
		return v;
	end direction_vector;


	function to_string (
		lv : in type_line_vector)
		return string
	is begin
		return "location vector start:" & to_string (lv.v_start) 
			& " direction vector" & to_string (lv.v_direction)
			& " angle" & to_string (get_angle (lv));
	end to_string;


	function move_by (
		lv		: in type_line_vector;
		offset	: in type_distance_relative)
		return type_line_vector
	is 
		result : type_line_vector := lv;
	begin
		result.v_start.x := result.v_start.x + type_float_internal (offset.x);
		result.v_start.y := result.v_start.y + type_float_internal (offset.y);

		return result;
	end move_by;
	
	
	function get_angle (
		line	: in type_line_vector)
		return type_rotation
	is 
		a : type_rotation;
	begin

		a := to_rotation (arctan (
				y		=> type_float_internal (line.v_direction.y), 
				x		=> type_float_internal (line.v_direction.x), 
				cycle	=> units_per_cycle));

		-- dz ignored. we are in a 2D world
		
		return a;
	end get_angle;
	
	
	function to_line_vector (
		ray : in type_ray)
		return type_line_vector
	is begin
		return (
			v_start		=> start_vector (ray),
			v_direction	=> direction_vector (ray));
	
	end to_line_vector;

	
	function to_perpendicular_line_vector (
		point	: in type_vector;
		angle	: in type_rotation)
		return type_line_vector 
	is
		ap : type_rotation; -- the angle of the resulting line
		r : type_ray;
	begin
		ap := add (angle, 90.0); -- perpendicular

		-- Build a ray that starts at point and travels
		-- in direction ap:
		r := (point, ap);

		-- Convert the ray to a line vector:
		return to_line_vector (r);
	end to_perpendicular_line_vector;

	
	function to_string (intersection : in type_intersection)
		return string
	is begin
		return to_string (intersection.vector) 
			& " angle" & to_string (intersection.angle);
	end to_string;



	
	function get_intersection (
		line_1, line_2	: in type_line_vector)
		return type_intersection_of_two_lines
	is 
		-- scratch variables:
		a, b, c, d, e, f, g : type_float_internal;
		lambda : type_float_internal;

		-- location vector and angle of intersection to be returned:			
		i : type_intersection;

		function exists_intersection return boolean is
			v1, v2 : type_vector;
		begin
			-- The first condition to be fulfilled is that the
			-- cross product of the direction vectors is not a null vector:
			v1 := cross_product (line_1.v_direction, line_2.v_direction); 

			if v1 /= null_vector then
				
				-- The second condition is:
				-- The mixed product of line_2.v_start, line_1.v_start and
				-- (line_2.v_start - line_1.v_start) must be zero.
				
				v2 := subtract (line_2.v_start, line_1.v_start);

				if mixed_product (line_1.v_direction, line_2.v_direction, v2) = 0.0 then
					return true; -- there is an intersection
				else
					return false;  -- no intersection exists
				end if;
				
			else					
				return false; -- no intersection exists
			end if;
		end exists_intersection;

		function lines_overlap return boolean is
			a, b, distance : type_float_internal;
			v1 : type_vector;

			th : constant type_float_internal := 1.0E-17;
		begin
			-- The first condition to be fulfilled is that the lines
			-- must run parallel to each other. In this case the cross
			-- product is zero.
			v1 := cross_product (line_1.v_direction, line_2.v_direction); 
			
			if v1 = null_vector then -- the lines run parallel to each other.

				-- Compute the distance between the lines.
				-- If the distance is zero then the lines overlap.
				
				a := absolute (cross_product (line_1.v_direction, subtract (line_2.v_start, line_1.v_start)));
				b := absolute (line_1.v_direction);

				distance := a / b;

				if abs (distance) <= th then						
					return true; -- lines overlap each other
				else
					return false; -- distance greater zero -> hence no overlap
				end if;
				
			else
				return false; -- not parallel -> hence no overlap
			end if;
		end lines_overlap;
			
	begin -- get_intersection
		--put_line ("");
		--put_line ("line_1 start" & to_string (to_point (line_1.v_start)) & " direction" & to_string (to_point (line_1.v_direction)));
		--put_line ("line_2 start" & to_string (to_point (line_2.v_start)) & " direction" & to_string (to_point (line_2.v_direction)));

		-- Test whether the lines overlap:
		if lines_overlap then
			return (status => OVERLAP);
		else
			-- Test whether there is an intersection:
			if exists_intersection then
				
				-- The direction vector of the first line can be zero in x.
				-- In order to avoid division by zero we must switch between
				-- two ways to find the intersection:
				if line_1.v_direction.x /= 0.0 then
					a := type_float_internal (line_1.v_start.y);
					b := type_float_internal (line_2.v_start.x * line_1.v_direction.y) / type_float_internal (line_1.v_direction.x);
					c := type_float_internal (line_1.v_start.x * line_1.v_direction.y) / type_float_internal (line_1.v_direction.x);
					d := type_float_internal (line_2.v_start.y);
					e := type_float_internal (line_2.v_direction.y);
					f := type_float_internal (line_2.v_direction.x * line_1.v_direction.y) / type_float_internal (line_1.v_direction.x);
					g := 1.0 / (e - f);

					lambda := (a + b - c - d) * g;

					i.vector := add (line_2.v_start, scale (line_2.v_direction, lambda));
				else
					a := type_float_internal (line_2.v_start.y);
					b := type_float_internal (line_1.v_start.x * line_2.v_direction.y) / type_float_internal (line_2.v_direction.x);
					c := type_float_internal (line_2.v_start.x * line_2.v_direction.y) / type_float_internal (line_2.v_direction.x);
					d := type_float_internal (line_1.v_start.y);
					e := type_float_internal (line_1.v_direction.y);
					f := type_float_internal (line_1.v_direction.x * line_2.v_direction.y) / type_float_internal (line_2.v_direction.x);
					g := 1.0 / (e - f);

					lambda := (a + b - c - d) * g;

					i.vector := add (line_1.v_start, scale (line_1.v_direction, lambda));
				end if;

				i.angle := get_angle_of_itersection (line_1, line_2);

				return (status => EXISTS, intersection => i);
			else

				return (status => NOT_EXISTENT);
			end if;
		end if;
		
	end get_intersection;

	
	function get_angle_of_itersection (
		line_1, line_2	: in type_line_vector)
		return type_rotation
	is 
		a, b, c : type_float_internal;
		r : type_rotation;
	begin
		a := type_float_internal (dot_product (line_1.v_direction, line_2.v_direction));
		b := type_float_internal (absolute (line_1.v_direction) * absolute (line_2.v_direction));
		c := a / b;

		-- c may be slightly greater than 1.0 or smaller than -1.0. In these cases
		-- the rotation can be set without any calculation:
		if c > 1.0 then
			r := 0.0;

		elsif c < -1.0 then
			r := 180.0;

		else
			r := to_rotation (arccos (X => a / b, cycle => units_per_cycle));				
		end if;

		return r;
	end get_angle_of_itersection;


	function make_line (
		start_x, start_y, end_x, end_y : in type_distance)
		return type_line'class
	is
		line : type_line;
	begin
		line := (
			start_point => type_point (set (start_x, start_y)),
			end_point	=> type_point (set (end_x, end_y)));

		return line;
	end make_line;


	function make_line (
		start_point, end_point : in type_point)
		return type_line'class
	is
		line : type_line;
	begin
		line := (start_point, end_point);

		return line;
	end make_line;

	

	function round (line : in type_line)
		return type_line'class
	is 
		r : type_line;
	begin
		r := (
			start_point	=> type_point (round (line.start_point)),
			end_point	=> type_point (round (line.end_point)));

		return r;
	end round;

	
	procedure round (line : in out type_line) 
	is begin
		line.start_point := type_point (round (line.start_point));
		line.end_point := type_point (round (line.end_point));
	end round;


	
	function get_length (line : in type_line)
		return type_distance_positive
	is begin
		return get_distance_total (line.start_point, line.end_point);
	end get_length;


	function get_greatest_length (l1, l2 : in type_line)
		return type_distance_positive
	is 
		length_1 : constant type_distance_positive := get_length (l1);
		length_2 : constant type_distance_positive := get_length (l2);
	begin
		if length_1 > length_2 then
			return length_1;
		else
			return length_2;
		end if;
	end get_greatest_length;
			

	function get_longest (l1, l2 : in type_line)
		return type_line'class
	is
		length_1 : constant type_distance_positive := get_length (l1);
		length_2 : constant type_distance_positive := get_length (l2);
	begin
		if length_1 > length_2 then
			return l1;
		else
			return l2;
		end if;
	end get_longest;
	
	
	function reverse_line (line : in type_line) return type_line'class is 
		result : type_line'class := line;
	begin
		result.start_point := line.end_point;
		result.end_point := line.start_point;

		return result;
	end reverse_line;

	
	procedure reverse_line (line : in out type_line) is 
		scratch : type_point := line.start_point;
	begin
		line.start_point := line.end_point;
		line.end_point := scratch;
	end reverse_line;
	
	
	function to_string (direction : in type_line_direction)
		return string
	is begin
		return " " & type_line_direction'image (direction);
	end to_string;
	
	
	function get_direction (line : in type_line)
		return type_line_direction
	is
		result : type_line_direction := HORIZONTAL;

		dx : constant type_distance := get_x (line.end_point) - get_x (line.start_point);
		dy : constant type_distance := get_y (line.end_point) - get_y (line.start_point);
	begin
		if dx = zero then
			-- no change in x -> line runs vertically
			result := VERTICAL;
			
		elsif dy = zero then
			-- no change in y -> line runs horizontally
			result := HORIZONTAL;
			
		elsif dx > zero then
			-- start point comes before end point
			
			if dy > zero then
				-- start point is lower than end point
				result := RISING;
				
			else -- dy < zero
				-- start point is higher than end point
				result := FALLING;
			end if;

		else -- dx < zero
			-- start point comes after end point
			
			if dy > zero then
				-- start point is higher than end point
				result := FALLING;
				
			else -- dy < zero
				-- start point is lower than end point
				result := RISING;
			end if;

		end if;
		
		return result;
	end get_direction;

	
	function get_tangent_direction (angle : in type_tangent_angle)
		return type_line_direction
	is
		result : type_line_direction := HORIZONTAL;
	begin
		if angle = 0.0 then
			result := HORIZONTAL;
			
		elsif angle < 90.0 then
			result := FALLING;
			
		elsif angle = 90.0 then
			result := VERTICAL;
			
		else -- angle > 90.0
			result := RISING;
		end if;

		return result;
	end get_tangent_direction;

	
	function crosses_threshold (
		line		: in type_line;	
		y_threshold	: in type_distance)
		return boolean
	is begin
		if	
			get_y (line.start_point) >= y_threshold and 
			get_y (line.end_point)   <  y_threshold then
			return true;
			
		elsif
			get_y (line.end_point)   >= y_threshold and 
			get_y (line.start_point) <  y_threshold then
			return true;
			
		else
			return false;
		end if;
	end crosses_threshold;

	
	function get_center (
		line	: in type_line)
		return type_point
	is
		dp : constant type_distance_polar := 
			get_distance (line.start_point, line.end_point);
	begin
		return type_point (move (
			point		=> line.start_point,
			direction	=> get_angle (dp),
			distance	=> get_absolute (dp) * 0.5));

	end get_center;

	
	function get_intersection (
		probe_line		: in type_line_vector;
		candidate_line	: in type_line)
		return type_intersection_of_two_lines
	is
		i : constant type_intersection_of_two_lines := get_intersection (
				line_1	=> probe_line,
				line_2	=> to_line_vector (candidate_line));
		
	begin
		case i.status is
			when EXISTS =>
				--put_line ("exists");
				--put_line (to_string (probe_line));
				--put_line (to_string (candidate_line));
				--put_line (to_string (i.intersection.point));
				
				-- The intersection must be between start and end point of
				-- the candidate line (start and end point itself included).
				-- If the intersection is between start and end point
				-- of candidate line, then return the intersection as it is.
				-- If the intersection is before start point or
				-- beyond end point, then return NOT_EXISTENT.
				if on_line (i.intersection.vector, candidate_line) then
					return i;
				else
					return (status => NOT_EXISTENT);
				end if;

			when others =>		
				return i;
		end case;

	end get_intersection;

	
	function start_vector (
		line	: in type_line)
		return type_vector is
	begin
		return (
			x => type_float_internal (get_x (line.start_point)),
			y => type_float_internal (get_y (line.start_point)),
			z => 0.0
			);
	end start_vector;

	
	function end_vector (
		line	: in type_line)
		return type_vector is
	begin
		return (
			x => type_float_internal (get_x (line.end_point)),
			y => type_float_internal (get_y (line.end_point)),
			z => 0.0
			);
	end end_vector;

	
	function direction_vector (
		line	: in type_line)
		return type_vector is
	begin
		return (
			x => type_float_internal (get_x (line.end_point) - get_x (line.start_point)),
			y => type_float_internal (get_y (line.end_point) - get_y (line.start_point)),
			z => 0.0
			);
	end direction_vector;

	
	function to_line_vector (
		line	: in type_line)
		return type_line_vector
	is begin
		return (
			v_start		=> start_vector (line),
			v_direction	=> direction_vector (line));
	end to_line_vector;

	
	--function get_distance (
		--line	: in type_line;
		--vector	: in type_vector)
		--return type_distance_positive
	--is
		--dv : constant type_vector := direction_vector (line);
		--sv : constant type_vector := start_vector (line);
		
		--d1 : constant type_vector := subtract (vector, sv);
		--m, n : type_float_internal;
	--begin
		--m := absolute (cross_product (dv, d1));
		--n := absolute (dv);
		
		--return to_distance (m / n);
	--end get_distance;

	
	function get_distance (
		line	: in type_line;
		vector	: in type_vector)
		return type_float_internal
	is
		dv : constant type_vector := direction_vector (line);
		sv : constant type_vector := start_vector (line);
		
		d1 : constant type_vector := subtract (vector, sv);
		m, n : type_float_internal;
	begin
		m := absolute (cross_product (dv, d1));
		n := absolute (dv);
		
		return (m / n);
	end get_distance;

	
	
	
	function get_direction (
		line	: in type_line)
		return type_rotation 
	is
		dx : constant type_float_internal := type_float_internal (get_x (line.end_point) - get_x (line.start_point));
		dy : constant type_float_internal := type_float_internal (get_y (line.end_point) - get_y (line.start_point));
	begin
		-- NOTE: If dx and dy are zero then the arctan operation is not possible. 
		-- In this case we assume the resulting angle is zero.
		if dx = 0.0 and dy = 0.0 then
			return zero_rotation;
		else
			return to_rotation (arctan (dy, dx, units_per_cycle));
		end if;
	end get_direction;

	
	procedure move_by (
		line		: in out type_line;
		direction	: in type_rotation;
		distance	: in type_distance_positive) 
	is begin
		-- Move start and and point of line into direction by distance.
		line.start_point	:= type_point (move (line.start_point, direction, distance));
		line.end_point		:= type_point (move (line.end_point,   direction, distance));
	end move_by;

	
	procedure move_by (
		line	: in out type_line;
		offset	: in type_distance_relative)
	is begin
		move_by (point	=> line.start_point,	offset => offset);
		move_by (point	=> line.end_point,		offset => offset);
	end move_by;

	
	procedure mirror (
		line		: in out type_line;
		axis		: in type_axis_2d)
	is begin
		mirror (line.start_point, axis);
		mirror (line.end_point, axis);
	end mirror;

	
	procedure rotate_by (
		line		: in out type_line;
		rotation	: in type_rotation) 
	is begin
		rotate_by (line.start_point, rotation);
		rotate_by (line.end_point, rotation);
	end rotate_by;

	
	function to_route (
		start_point, end_point	: in type_point;
		style					: in type_bend_style)
		return type_route
	is
		-- The area required for the route is a rectangle.
		-- We will need to figure out whether it is wider than tall:
		dx : constant type_distance := get_distance (start_point, end_point, X);
		dy : constant type_distance := get_distance (start_point, end_point, Y);

		sup_start, sup_end : type_point; -- support points near given start and end point

		-- distance of support points from given start or end point:
		ds : constant type_distance_positive := 1.0;

		bended : type_bended := YES;
		bend_point : type_point;

		-- CS this procedure should be made public as "intersection" or similar
		-- CS use function get_intersection with S1, R1, S2, R2 as input
		-- to compute intersection I.
		procedure compute_bend_point is 
			first_line	: constant type_line := (start_point, sup_start);
			second_line	: constant type_line := (end_point, sup_end);

			-- first line start vector:
			S1 : constant type_vector := start_vector (first_line);

			-- first line direction vector:
			R1 : constant type_vector := direction_vector (first_line);

			-- second line start vector:
			S2 : constant type_vector := start_vector (second_line);

			-- second line direction vector
			R2 : constant type_vector := direction_vector (second_line);

			-- scratch variables:
			a, b, c, d, e, f, g : type_float_internal;
			lambda_1, lambda_2 : type_float_internal;

			-- location vector of intersection
			I : type_vector;
		begin
			-- The direction vector of the first line can be zero in x (R1.x).
			-- In order to avoid division by zero we must switch between
			-- two ways to find the intersection:
			if R1.x /= 0.0 then
				a := S1.y;
				b := S2.x * R1.y / R1.x;
				c := S1.x * R1.y / R1.x;
				d := S2.y;
				e := R2.y;
				f := R2.x * R1.y / R1.x;
				g := 1.0 / (e - f);

				lambda_2 := (a + b - c - d) * g;

				I := add (S2, scale (R2, lambda_2));
			else
				a := S2.y;
				b := S1.x * R2.y / R2.x;
				c := S2.x * R2.y / R2.x;
				d := S1.y;
				e := R1.y;
				f := R1.x * R2.y / R2.x;
				g := 1.0 / (e - f);

				lambda_1 := (a + b - c - d) * g;

				I := add (S1, scale (R1, lambda_1));
			end if;
			
			bend_point := to_point (I);
		end compute_bend_point;
		
	begin -- to_route
		
		-- If start and end point are equally then do nothing
		-- and return given start and end point as they are:
		if start_point = end_point then
			bended := NO;
		else
		
			-- If start and end point have same x or y position, then we
			-- have a straight direct line between them.
			if dx = zero or dy = zero then
				bended := NO;
			else

				case style is
					when STRAIGTH_THEN_ANGLED =>
						if abs (dx) = abs (dy) then -- diagonal line from start to end
							bended := NO;
						else
							
							-- compute support point near start point:
							-- The first line must run straight from start point:
							--if wider_than_tall then
							if abs (dx) > abs (dy) then -- wider than tall
								sup_start := type_point (set (get_x (start_point) + ds, get_y (start_point)));
							else -- taller than wide
								sup_start := type_point (set (get_x (start_point), get_y (start_point) + ds));
							end if;

							-- compute support point near end point:
							-- The second line must run angled from end point:
							if dx > zero then -- to the right
								if dy > zero then -- upwards
									sup_end := type_point (set (get_x (end_point) + ds, get_y (end_point) + ds));
									--  45 degree
								else
									sup_end := type_point (set (get_x (end_point) + ds, get_y (end_point) - ds));
									-- -45 degree
								end if;
							else -- to the left
								if dy > zero then -- upwards
									sup_end := type_point (set (get_x (end_point) - ds, get_y (end_point) + ds));
									-- 135 degree
								else
									sup_end := type_point (set (get_x (end_point) - ds, get_y (end_point) - ds));
									-- 225 degree
								end if;
							end if;

							compute_bend_point;

						end if;
						
					when DIRECT => bended := NO;

					when ANGLED_THEN_STRAIGHT =>
						if abs (dx) = abs (dy) then -- diagonal line from start to end
							bended := NO;
						else
							
							-- Compute support point near start point:
							-- The first line must run angled from start point:
							if dx > zero then -- to the right
								if dy > zero then -- upwards
									sup_start := type_point (set (get_x (start_point) + ds, get_y (start_point) + ds));
									--  45 degree
								else -- downwards
									sup_start := type_point (set (get_x (start_point) + ds, get_y (start_point) - ds));
									-- -45 degree
								end if;
							else -- to the left
								if dy > zero then -- upwards
									sup_start := type_point (set (get_x (start_point) - ds, get_y (start_point) + ds));
									-- 135 degree
								else -- downwards
									sup_start := type_point (set (get_x (start_point) - ds, get_y (start_point) - ds));
									-- 225 degree
								end if;
							end if;

							-- compute support point near end point:
							-- The second line must run straight from end point:
							if abs (dx) > abs (dy) then -- wider than tall
								sup_end := type_point (set (get_x (end_point) + ds, get_y (end_point)));
								-- horizontally
							else -- taller than wide
								sup_end := type_point (set (get_x (end_point), get_y (end_point) + ds));
								-- vertically
							end if;

							compute_bend_point;
						end if;
						
					when VERTICAL_THEN_HORIZONTAL =>
						-- Compute support point near start point:
						-- The first line must run vertically from start point:
						sup_start := type_point (set (get_x (start_point), get_y (start_point) + ds));
						-- vertically

						-- The second line must run horizontally from end point:
						sup_end := type_point (set (get_x (end_point) + ds, get_y (end_point)));
						-- horizontally

						compute_bend_point;
						
					when HORIZONTAL_THEN_VERTICAL =>
						-- Compute support point near start point:
						-- The first line must run horizontal from start point:
						sup_start := type_point (set (get_x (start_point) + ds, get_y (start_point)));
						-- horizontally

						-- compute support point near end point:
						-- The second line must run vertically from end point:
						sup_end := type_point (set (get_x (end_point), get_y (end_point) + ds));
						-- vertically

						compute_bend_point;
						
				end case;
			end if;

		end if;
			
		if bended = NO then
			return (NO, start_point, end_point);
		else
			return (YES, start_point, end_point, bend_point);
		end if;

	end to_route;

	
	procedure next_bend_style (route : in out type_route_live) is
		i : constant natural := type_bend_style'pos (route.bend_style);
		-- i points now to the current bend style

		-- get the index of the last available bend style:
		max : constant natural := type_bend_style'pos (type_bend_style'last);
	begin
		if i < max then
			-- jump to next bend style
			route.bend_style := type_bend_style'succ (type_bend_style'val (i));
		else 
			-- After the last bend style, jump back to the first bend style:
			route.bend_style := type_bend_style'first;
		end if;
	end next_bend_style;

	
	function get_boundaries (
		line	: in type_line;	
		width	: in type_distance_positive)
		return type_boundaries 
	is begin
		return get_boundaries (line.start_point, line.end_point, width);
	end get_boundaries;

	
	function which_zone (
		point	: in type_point'class;
		line	: in type_line'class) 
		return type_line_zone 
	is
		zone : type_line_zone; -- to be returned
	
		line_length : type_distance;
		zone_border : type_distance;
		
	begin -- which_zone
		-- CS: The algorithm used here is not the best. Improve using vector algebra ?
		
		-- The greater distance from start to end point in X or Y determines 
		-- whether the line is handled like a horizontal or vertical drawn line.
		if get_distance_abs (line.start_point, line.end_point, X) > 
			get_distance_abs (line.start_point, line.end_point, Y) then

			-- distance in X greater -> decision will be made along the X axis.
			-- The line will be handled like a horizontal drawn line.
			
			-- calculate the zone border. This depends on the line length in X direction.
			line_length := get_distance_abs (line.start_point, line.end_point, X);
			zone_border := line_length / type_distance (line_zone_division_factor);
			-- CS ? should be: zone_border := line_length / to_distance (line_zone_division_factor);
			
			if get_x (line.start_point) < get_x (line.end_point) then 
			-- DRAWN FROM LEFT TO THE RIGHT
				if get_x (point) < get_x (line.start_point) + zone_border then
					zone := START_POINT; -- point is in the zone of line.start_point
				elsif get_x (point) > get_x (line.end_point) - zone_border then
					zone := END_POINT; -- point is in the zone of line.end_point
				else
					zone := CENTER;
				end if;

			else 
			-- DRAWN FROM RIGHT TO THE LEFT
				if get_x (point) > get_x (line.start_point) - zone_border then
					zone := START_POINT; -- point is in the zone of line.start_point
				elsif get_x (point) < get_x (line.end_point) + zone_border then
					zone := END_POINT; -- point is in the zone of line.end_point
				else
					zone := CENTER;
				end if;
			end if;

			
		else
			-- distance in Y greater or equal distance in X -> decision will be made along the Y axis.
			-- The line will be handled like a vertical drawn line.

			-- calculate the zone border. This depends on the line length in Y direction.
			line_length := get_distance_abs (line.start_point, line.end_point, Y);
			zone_border := line_length / type_distance (line_zone_division_factor);
			-- CS ? should be: zone_border := line_length / to_distance (line_zone_division_factor);
			
			if get_y (line.start_point) < get_y (line.end_point) then 
			-- DRAWN UPWARDS
				if get_y (point) < get_y (line.start_point) + zone_border then
					zone := START_POINT; -- point is in the zone of line.start_point
				elsif get_y (point) > get_y (line.end_point) - zone_border then
					zone := END_POINT; -- point is in the zone of line.end_point
				else
					zone := CENTER;
				end if;
					
			else 
			-- DRAWN DOWNWARDS
				if get_y (point) > get_y (line.start_point) - zone_border then
					zone := START_POINT; -- point is in the zone of line.start_point
				elsif get_y (point) < get_y (line.end_point) + zone_border then
					zone := END_POINT; -- point is in the zone of line.end_point
				else
					zone := CENTER;
				end if;
			end if;
			
			
		end if;
		
		return zone;
	end which_zone;

	function out_of_range (d : in type_distance_point_line) return boolean is begin
		return d.out_of_range;
	end out_of_range;

	function get_distance (d : in type_distance_point_line) 
		return type_float_internal
	is begin
		return d.distance;
	end get_distance;

	function get_intersection (d : in type_distance_point_line) 
		return type_vector 
	is begin
		return d.intersection;
	end get_intersection;

	function get_direction (d : in type_distance_point_line) return type_rotation is begin
		return d.direction;
	end get_direction;
	
	function on_start_point (d : in type_distance_point_line) return boolean is begin
		return d.sits_on_start;
	end on_start_point;

	function on_end_point (d : in type_distance_point_line) return boolean is begin
		return d.sits_on_end;
	end on_end_point;

	
	function get_distance (
		vector		: in type_vector;
		line		: in type_line;
		line_range	: in type_line_range)
		return type_distance_point_line 
	is
		result : type_distance_point_line;
	
		-- Imagine a line that starts on the given location vector,
		-- travels perpendicular towards
		-- the given line and finally intersects the given line somewhere.
		-- The intersection may be betweeen the start and end point of the given line.
		-- The intersection may be virtual, before start or after end point 
		-- of the given line.
		
		line_direction : constant type_rotation := get_direction (line);
		line_direction_vector : constant type_vector := direction_vector (line);
		line_start_vector, line_end_vector : type_vector;

		iv : type_vector;

		procedure compute_intersection is 
			distance : type_float_internal;
			th : constant type_float_internal := 1.0E-10; -- CS refine or set dynamically ?
		begin
			-- Compute the point of intersection: The intersection of a line that runs
			-- from the given location vector perpendicular to the given line.
			-- For the moment we do not know which direction to go. So we just try
			-- to go in 90 degree direction. If the distance from iv to the line
			-- is not zero, then we try in -90 degree direction.

			iv := vector;
			move_by (iv, line_direction + 90.0, result.distance);

			distance := get_distance (line, iv);
			--log (text => "delta  :" & type_float_internal'image (distance));
			
			if distance > th then
				--put_line ("wrong direction");
				
				-- we went the wrong direction
				iv := vector; -- restore iv

				-- try opposite direction:
				move_by (iv, line_direction - 90.0, result.distance);
			end if;

			--put_line ("iv" & to_string (iv));
			
			-- Assign the direction (from point to intersection) to the result:
			result.direction := get_angle (get_distance (vector, iv));
			--put_line ("direction" & to_string (result.direction));

			-- Assign the virtual point of intersection to the result:
			result.intersection := iv;
		end compute_intersection;
		
		lambda_forward, lambda_backward : type_float_internal;
	begin
		--put_line ("line direction vector: " & to_string (line_direction_vector));
		--put_line ("line direction angle : " & to_string (line_direction));
		
		-- The first and simplest test is to figure out whether
		-- the given point sits exactly on the start or end point of the line.
		-- Mind: result.distance has default zero.
		-- This test includes the start and end points of the line. 
		-- On match we exit this function prematurely and return the result
		-- with the appropiate flags set.
		case line_range is
			when WITH_END_POINTS | BEYOND_END_POINTS =>
				
				if vector = to_vector (line.start_point) then
					
					result.sits_on_start := true;
					result.out_of_range := false;
					return result;

				elsif vector = to_vector (line.end_point) then
					
					result.sits_on_end := true;
					result.out_of_range := false;
					return result;

				end if;
				
			when others => null;
		end case;

		---- If the ends of the line are included,
		---- test whether the point is in the vicinity of the line start or end point.
		---- Exit this function prematurely in that case.
		--if line_range = WITH_END_POINTS then

			---- compute distance of point to start of line:
			--result.distance := get_distance_total (point, line.start_point);
			
			--if result.distance <= accuracy then
				--result.out_of_range := false;
				--return result;
			--end if;

			--if point = line.start_point then
				--result.out_of_range := false;
				--return result;
			--end if;
				

			---- compute distance of point to end of line:
			--result.distance := get_distance_total (point, line.end_point);
			
			--if result.distance <= accuracy then
				--result.out_of_range := false;
				--return result;
			--end if;

		--end if;
		
		-- Compute the distance from the given point to the given line.
		-- This computation does not care about end or start point of the line.
		-- It assumes an indefinite long line without start or end point.
		result.distance := get_distance (line, vector);

		--put_line ("distance " & to_string (result.distance));

		-- Set iv so that it points to the intersection. The
		-- intersection can be anywhere on that indefinite long line.
		compute_intersection;

		
		-- Any point on a line can be computed by this formula (see textbook on vector algebra):
		-- iv = line.start_point + lambda_forward  * line_direction_vector
		-- iv = line.end_point   + lambda_backward * line_direction_vector

		-- Using these formula we can calculate whether iv points between 
		-- (or to) the start and/or end points of the line:
		
		line_start_vector := start_vector (line);
		lambda_forward := divide (subtract (iv, line_start_vector), line_direction_vector);

		--put_line ("lambda forward:" & to_string (lambda_forward));
		
		if lambda_forward < 0.0 then -- iv points BEFORE start of line
			--put_line ("before start point");
			case line_range is
				when BEYOND_END_POINTS => result.out_of_range := false;
				when others => result.out_of_range := true;
			end case;

			return result; -- no more computations required
		end if;
		
		if lambda_forward = 0.0 then -- iv points TO start point of line
			--put_line ("on start point");
			case line_range is
				when BETWEEN_END_POINTS => result.out_of_range := true;
				when others => result.out_of_range := false;
			end case;

			return result; -- no more computations required
		end if;

		--put_line ("after start point");

		
		line_end_vector := end_vector (line);
		lambda_backward := divide (subtract (iv, line_end_vector), line_direction_vector);

		--put_line ("lambda backward:" & to_string (lambda_backward));
		
		if lambda_backward > 0.0 then -- iv points AFTER end of line
			--put_line ("after end point");
			case line_range is
				when BEYOND_END_POINTS => result.out_of_range := false;
				when others => result.out_of_range := true;
			end case;

			return result; -- no more computations required
		end if;

		if lambda_backward = 0.0 then -- iv points TO end point of line
			--put_line ("on end point");
			case line_range is
				when BETWEEN_END_POINTS => result.out_of_range := true;
				when others => result.out_of_range := false;
			end case;

			return result; -- no more computations required
		end if;

		--put_line ("before end point");

		result.out_of_range := false;

		return result;
	end get_distance;


	function get_distance (
		point		: in type_point; 
		line		: in type_line;
		line_range	: in type_line_range)
		return type_distance_point_line
	is begin
		return get_distance (to_vector (point), line, line_range);
	end get_distance;
	
	
	function on_line (
		vector	: in type_vector;
		line	: in type_line)
		return boolean
	is
		distance : type_distance_point_line;
		th : constant type_float_internal := 1.0E-10; -- CS refine or set dynamically ?
	begin
		distance := get_distance (vector, line, WITH_END_POINTS);
		
		if not distance.out_of_range and distance.distance < th then
			return true;
		else
			return false;
		end if;
	end on_line;

	
	function on_line (
		point	: in type_point;
		line	: in type_line)
		return boolean
	is begin
		return on_line (to_vector (point), line);
	end on_line;


	
	function get_shortest_distance (
		point	: in type_point;
		line	: in type_line)
		return type_distance_polar
	is
		result : type_distance_polar;

		d : constant type_distance_point_line := get_distance (
			vector		=> to_vector (point),
			line		=> line,
			line_range	=> WITH_END_POINTS);

		d_to_start, d_to_end : type_distance_polar;
	begin
		--put_line ("point" & to_string (point) & " " & to_string (line));
		
		if on_start_point (d) or on_end_point (d) then
			-- Point is on top of start or end point of line.
			--log (text => "on start or end");
			null; -- result keeps its default (zero distance, zero angle)
		else

			--if on_line (get_intersection (d), line) then 
			if not out_of_range (d) then
				
				-- An imaginary line can be drawn perpendicular from
				-- point to line. Both intersect each other.
				set_absolute (result, to_distance (get_distance (d)));
				set_angle (result, get_direction (d));
			else
				
				-- No imaginary line can be drawn perpendicular from
				-- point to line.

				-- Compare the distances to the end points of the line:
				d_to_start := get_distance (point, line.start_point);
				d_to_end   := get_distance (point, line.end_point);

				if get_absolute (d_to_start) < get_absolute (d_to_end) then
					result := d_to_start;
				else
					result := d_to_end;
				end if;
				
			end if;

		end if;

		--put_line (to_string (result));
		
		return result;
	end get_shortest_distance;


	function round (arc : in type_arc)
		return type_arc'class
	is 
		r : type_arc;
	begin
		r := (
			center		=> type_point (round (arc.center)),
			start_point	=> type_point (round (arc.start_point)),
			end_point	=> type_point (round (arc.end_point)),
			direction	=> arc.direction);

		return r;
	end round;

	procedure round (arc : in out type_arc) 
	is begin
		arc.center		:= type_point (round (arc.center));
		arc.start_point	:= type_point (round (arc.start_point));
		arc.end_point	:= type_point (round (arc.end_point));
	end round;


	

	function reverse_arc (arc : in type_arc) return type_arc'class is
		result : type_arc := arc;
	begin
		result.start_point := arc.end_point;
		result.end_point := arc.start_point;

		case arc.direction is
			when CW  => result.direction := CCW;
			when CCW => result.direction := CW;
		end case;
		
		return result;
	end reverse_arc;

	
	procedure reverse_arc (arc : in out type_arc) is
		scratch : type_point := arc.start_point;
	begin
		arc.start_point := arc.end_point;
		arc.end_point := scratch;

		case arc.direction is
			when CW	 => arc.direction := CCW;
			when CCW => arc.direction := CW;
		end case;
	end reverse_arc;


	function normalize_arc (arc: in type_arc) return type_arc'class is
	begin
		case arc.direction is
			when CW  => return reverse_arc (arc);					
			when CCW => return arc;
		end case;
	end normalize_arc;


	function zero_length (arc : in type_arc) return boolean is
	begin
		if arc.start_point = arc.end_point then
			return true;
		else
			return false;
		end if;
	end zero_length;
	
	
	function get_shortest_distance (
		point	: in type_point;
		arc		: in type_arc)
		return type_distance_polar
	is
		result : type_distance_polar;

		procedure do_it is 
			-- Build a line that runs from the given point to the center of the arc:
			line : constant type_line_vector := to_line_vector (line => (point, arc.center));
			-- IMPORTANT NOTE: Function to_line_vector computes the direction vector of line as:
			--  arc.center.x - point.x and arc.center.y - point.y.
			--  Function after_center (see below) bases on this fact. Otherwise its result
			--  will be nonsense !!

			-- Get the intersection(s) of the line with the arc:
			ILC : constant type_intersection_of_line_and_circle := get_intersection (line, arc);

			DPC : constant type_distance_polar := get_distance (point, arc.center);
			radius : constant type_distance_positive := get_radius_start (arc);

			-- Assigns to the result either the start or the end point of
			-- the arc, depending on which one is closer.
			procedure compare_start_and_end_point is 
				d_to_start, d_to_end : type_distance_polar;
			begin
				d_to_start := get_distance (point, arc.start_point);
				d_to_end   := get_distance (point, arc.end_point);

				if get_absolute (d_to_start) < get_absolute (d_to_end) then
					result := d_to_start;
				else
					result := d_to_end;
				end if;
			end compare_start_and_end_point;

			-- Compute the distance of point to circle:
			procedure like_circle is begin
				-- The arc can be treated like a circle.
				result := DPC;
				set_absolute (result, get_absolute (DPC) - radius);
			end like_circle;

			-- Detects whether the given location vector i is after the
			-- center of the arc on "line".
			-- 1. It bases on the well known vector formula:
			--    i = start_vector + lambda * direction_vector
			--    This formula is solved by lambda.
			-- 2. It bases on the assumption that the direction_vector of line is
			--    already properly set (see comment above):
			function after_center (i : in type_vector) return boolean is
				lambda : type_float_internal;
			begin
				-- the start_vector is where "line" starts: the given point
				-- the direction vector is the direction of "line": towards the 
				-- center of the arc:
				lambda := divide ((subtract (i, line.v_start)), line.v_direction);
				--put_line ("lambda" & to_string (lambda));
				if lambda > 1.0 then
					return true; -- i is after center of arc
				else 
					return false; -- i is on or before center of arc
				end if;
			end after_center;
			
		begin -- do_it
			--log (text => "DPC" & to_string (get_absolute (DPC)));
			
			if get_absolute (DPC) >= radius then
				-- point outside or on virtual circle
				--put_line ("outside");
				--log (text => "outside");
				
				case ILC.status is
					when NONE_EXIST =>
						--put_line ("none");
						-- line travels past the arc. no intersections
						compare_start_and_end_point;

					when ONE_EXISTS =>
						--put_line ("one");
						
						if ILC.tangent_status = SECANT then
						-- line intersects the arc only once

							--log (text => "a: " & to_string (arc));
							--log (text => "l: " & to_string (line));
							--log (text => "i: " & to_string (ILC.intersection.point));
							
							if after_center (ILC.intersection.vector) then
								-- intersection after center of arc
								--log (text => "i after center");
								compare_start_and_end_point;
							else
								-- intersection on circumfence between point and center of arc
								--put_line ("i before center");
								--put_line ("p betweeen circumfence and center");
								like_circle;
							end if;
							
						else
							-- a tangent should never be the case
							raise constraint_error;
						end if;

					when TWO_EXIST =>
						--put_line ("two");
						
						-- line intersects the virtual circle twice on
						-- its circumfence. But the intersection nearest
						-- to point is relevant:
						like_circle;
						
				end case;

			else -- point is inside the virtual circle
				--put_line ("inside");
				--log (text => "inside");
				
				case ILC.status is
					when NONE_EXIST =>
						-- line travels past the arc
						--put_line ("none");
						compare_start_and_end_point;

					when ONE_EXISTS =>
						--put_line ("one");
						
						if ILC.tangent_status = SECANT then
						-- line intersects the arc only once

							--put_line ("i: " & to_string (ILC.intersection.point));
							
							if after_center (ILC.intersection.vector) then
								-- intersection after center of arc
								--put_line ("i after center");
								compare_start_and_end_point;
							else
								-- point is between circumfence and center of arc
								--put_line ("i before center");
								
								result := DPC;
								set_absolute (result, radius - get_absolute (DPC));
								--set_angle (result, add (get_angle (DPC), 180.0));
								reverse_angle (result);
							end if;

						else
							-- a tangent should never be the case
							raise constraint_error;
						end if;

					when TWO_EXIST =>
						-- treat the arc like a circle and compute distance point to circle:
						result := get_distance_to_circumfence (point, (arc.center, radius));
						
				end case;				
			end if;				
		end do_it;
		
	begin -- get_shortest_distance
		--put_line ("point" & to_string (point) & " " & to_string (arc));
		
		if point = arc.center then
			-- If the given point is right on the center of the arc,
			-- then return zero distance and zero angle:

			set_absolute (result, zero);
			set_angle (result, 0.0);
		else
			do_it;
		end if;

		--put_line (to_string (result));
		
		return result;
	end get_shortest_distance;

			
	
	function crosses_threshold (
		arc			: in type_arc;
		y_threshold	: in type_distance)
		return boolean
	is begin
		if	
			get_y (arc.start_point) >= y_threshold and 
			get_y (arc.end_point)   <  y_threshold then
			return true;
			
		elsif
			get_y (arc.end_point)   >= y_threshold and 
			get_y (arc.start_point) <  y_threshold then
			return true;
			
		else
			return false;
		end if;
	end crosses_threshold;

	
	
	
	function get_radius_start (
		arc : in type_arc) 
		return type_distance_positive 
	is begin
		return get_distance_total (arc.center, arc.start_point);
	end get_radius_start;

	
	function get_radius_end (
		arc : in type_arc)
		return type_distance_positive
	is begin
		return get_distance_total (arc.center, arc.end_point);
	end get_radius_end;

	
	function is_valid (
		arc : in type_arc)
		return boolean 
	is 
		rs : constant type_distance_positive := get_radius_start (arc);
		re : constant type_distance_positive := get_radius_end (arc);
	begin
		if rs = re then

			if rs > zero then				
				return true;
			else
				return false;
			end if;
		else
			return false;
		end if;
	end is_valid;

	
	function to_arc_angles (arc : in type_arc) return type_arc_angles is
	-- The angles may be negative. For example instead of 270 degree
	-- the angle can be -90 degree.
		result : type_arc_angles;
					
		-- Take a copy of the given arc in arc_tmp.
		arc_tmp : type_arc := arc;
	begin
		-- move arc_tmp so that its center is at 0/0
		move_to (arc_tmp, origin);

		-- the center is not changed:
		result.center := arc.center;
		
		-- calculate the radius of the arc
		result.radius := get_distance_total (arc_tmp.center, to_vector (arc_tmp.start_point));

		-- calculate the angles where the arc begins and ends:

		-- NOTE: If x and y are zero then the arctan operation is not possible. 
		-- In this case we assume the resulting angle is zero.
		
		if get_x (arc_tmp.start_point) = zero and get_y (arc_tmp.start_point) = zero then
			result.angle_start := zero_rotation;
		else
			result.angle_start := to_degrees (type_float_internal (arctan (
					y => type_float_internal (get_y (arc_tmp.start_point)),
					x => type_float_internal (get_x (arc_tmp.start_point)))));
		end if;

		if get_x (arc_tmp.end_point) = zero and get_y (arc_tmp.end_point) = zero then
			result.angle_end := zero_rotation;
		else
			result.angle_end := to_degrees (type_float_internal (arctan (
					y => type_float_internal (get_y (arc_tmp.end_point)),
					x => type_float_internal (get_x (arc_tmp.end_point)))));
		end if;

		-- make sure start and end angle are not equal
		if result.angle_start = result.angle_end then
			raise constraint_error; -- CS warning instead ?
		end if;
		
		-- direction is not changed:
		result.direction := arc.direction;
		
		return result;
	end to_arc_angles;


	function to_arc (arc : in type_arc_angles) return type_arc'class is
		result : type_arc;
		x, y : type_float_internal;
		offset : constant type_distance_relative := (get_x (arc.center), get_y (arc.center));
	begin
		result.center := arc.center;
		result.direction := arc.direction;

		-- start point:
		x := type_float_internal (arc.radius) * cos (type_float_internal (arc.angle_start), units_per_cycle);
		y := type_float_internal (arc.radius) * sin (type_float_internal (arc.angle_start), units_per_cycle);
		--result.start_point := type_point (set (type_distance (x), type_distance (y)));
		result.start_point := type_point (set (to_distance (x), to_distance (y)));
		move_by (result.start_point, offset);
		
		-- end point:
		x := type_float_internal (arc.radius) * cos (type_float_internal (arc.angle_end), units_per_cycle);
		y := type_float_internal (arc.radius) * sin (type_float_internal (arc.angle_end), units_per_cycle);
		--result.end_point := type_point (set (type_distance (x), type_distance (y)));
		result.end_point := type_point (set (to_distance (x), to_distance (y)));
		move_by (result.end_point, offset);
		
		return result;
	end to_arc;
	
	
	function get_boundaries (
		arc			: in type_arc;
		line_width	: in type_distance_positive) 
		return type_boundaries 
	is
		half_width : constant type_distance_positive := line_width * 0.5;
		
		result : type_boundaries; -- to be returned

		-- normalize the given arc
		arc_norm : type_arc := type_arc (normalize_arc (arc));

		-- Calculate the radius of the arc:
		radius : constant type_distance_positive := get_radius_start (arc_norm);

		-- The quadrant of start and end point:
		q_start : type_quadrant;
		q_end   : type_quadrant;
		
		procedure set_sx is begin result.smallest_x := - radius; end;
		procedure set_gx is begin result.greatest_x :=   radius; end;
		procedure set_sy is begin result.smallest_y := - radius; end;
		procedure set_gy is begin result.greatest_y :=   radius; end;

		procedure same_quadrant is 
			angles : type_arc_angles;
		begin
			-- get start and end angles of normalized arc:
			angles := to_arc_angles (arc_norm);

			if angles.angle_start <= angles.angle_end then
				null; -- arc is only in this quadrant
			else
				-- arc runs through all quadrants
				set_gy;
				set_sx;
				set_sy;
				set_gx;
			end if;
		end same_quadrant;
		
	begin -- get_boundaries

		-- move arc_norm so that its center is at 0/0
		move_to (arc_norm, origin);

		-- Calculate the quadrants of start and end point:
		q_start := get_quadrant (arc_norm.start_point);
		q_end   := get_quadrant (arc_norm.end_point);

		--put_line ("Q Start:" & type_quadrant'image (q_start));
		--put_line ("Q End:  " & type_quadrant'image (q_end));
		
		-- Calculate the boundaries of start and end point.
		-- For the moment we regard start and end point of the arc being
		-- connected with a straight line, ignoring the line width:
		result := get_boundaries (arc_norm.start_point, arc_norm.end_point, zero);

		--put_line ("result: " & to_string (result));
		
		-- Depending on the quadrants of start and end point, other quadrants may
		-- be crossed. The boundaries (held in result) must be pushed away into x
		-- or y direction if start and end point are not in the same quadrant.
		case q_start is
			when ONE =>
				case q_end is
					when ONE =>
						same_quadrant;
						
					when TWO => 
						set_gy;

					when THREE =>
						set_gy;
						set_sx;

					when FOUR =>
						set_gy;
						set_sx;
						set_sy;
				end case;

			when TWO =>
				case q_end is
					when ONE => 
						set_sx;
						set_sy;
						set_gx;

					when TWO =>
						same_quadrant;
							
					when THREE =>
						set_sx;

					when FOUR =>
						set_sx;
						set_sy;
				end case;
				
			when THREE =>
				case q_end is
					when ONE =>
						set_sy;
						set_gx;

					when TWO =>
						set_sy;
						set_gx;
						set_gy;

					when THREE =>
						same_quadrant;

					when FOUR =>
						set_sy;
				end case;

			when FOUR =>
				case q_end is
					when ONE =>
						set_gx;

					when TWO =>
						set_gx;
						set_gy;

					when THREE =>
						set_gx;
						set_gy;
						set_sx;

					when FOUR =>
						same_quadrant;
				end case;
				
		end case;

		-- The boundaries held in "result" are still relative to the origin (0/0).
		-- They must be moved back to where the given arc is positioned.
		move_by (result, to_distance_relative (arc.center));

		-- extend the boundaries by half the line width;
		result.smallest_x := result.smallest_x - half_width;
		result.smallest_y := result.smallest_y - half_width;

		result.greatest_x := result.greatest_x + half_width;
		result.greatest_y := result.greatest_y + half_width;
		
		return result;
	end get_boundaries;

	
	function on_arc (
		vector	: in type_vector;
		arc		: in type_arc)
		return boolean 
	is
		-- The angle of the given point relative to the
		-- center of the given arc:
		P : type_rotation_positive;

		-- A representation of the given arc in angles:
		arc_angles : constant type_arc_angles := to_arc_angles (arc);
		
		-- make the angles of the arc positive:
		S : type_rotation_positive := to_positive_rotation (arc_angles.angle_start);
		E : type_rotation_positive := to_positive_rotation (arc_angles.angle_end);

		procedure offset_ccw is 
			T : type_rotation_positive;
		begin
			T := 360.0 - S;
			--log (text => "offset" & to_string (T));
			
			S := 0.0;
			E := E + T;
			P := add (P, T);
		end offset_ccw;

		procedure offset_cw is 
			T : type_rotation_positive;
		begin
			T := 360.0 - E;
			E := 0.0;
			S := S + T;
			P := add (P, T);
		end offset_cw;

		distance_center_to_point : constant type_float_internal :=
			get_distance_total (arc.center, vector);

		th : constant type_float_internal := 1.0E-9; -- CS refine or set dynamically ?
	begin
		--put_line ("center" & to_string (arc.center) 
				--& " radius" & to_string (arc_angles.radius)
				--& " start" & to_string (arc.start_point)
				--& " end" & to_string (arc.end_point)
				--& " point" & to_string (point)
				--& " distance center to point" & to_string (distance_center_to_point));

		-- First test whether the given point is on the circumfence of
		-- a virtual circle. The circle has the same radius as the arc:
		--put_line ("delta:" & to_string (distance_center_to_point - arc_angles.radius));
		
		if abs (distance_center_to_point - arc_angles.radius) <= th then
		
			-- Point is on circumfence of virtual circle.
			--log (text => "on circumfence");

			--log (text => "S" & to_string (S));
			--log (text => "E" & to_string (E));
			
			-- Compute the angle of the point relative to the center
			-- of the given arc:
			P := to_positive_rotation (get_angle (get_distance (arc.center, vector)));
			--log (text => "P" & to_string (P));
			
			-- The angle of the point must be between start and end point
			-- of the arc to be considered as "on the arc".
			-- Special problem: The arc may run across the ZDG ("zero degree mark").
			--  In that case the start and end angle and the point angle must first be
			--  rotated so that the arc no longer crossed the ZDG.
			case arc.direction is
				when CW =>
					if S <= E then -- arc crosses the ZDG
						offset_cw;
					end if;
					
					if P <= S and P >= E then
						--log (text => "on cw arc");
						return true;
					else
						--log (text => "not on cw arc");
						return false;
					end if;

				when CCW =>
					if S >= E then -- arc crosses the ZDG
						offset_ccw;
					end if;

					--log (text => "start" & to_string (S));
					--log (text => "end  " & to_string (E));
					--log (text => "point" & to_string (P));

					
					if P >= S and P <= E then
						--log (text => "on ccw arc");
						return true;
					else
						--log (text => "not on ccw arc");
						return false;
					end if;
			end case;
			
		else
			return false; 
		end if;

	end on_arc;


	function on_arc (
		point	: in type_point;
		arc		: in type_arc)
		return boolean
	is begin
		return on_arc (to_vector (point), arc);
	end on_arc;


	
	function get_intersection (
		line	: in type_line_vector;
		arc		: in type_arc)
		-- CS optional argument for radius should improve performance
		-- default radius = zero ?
		return type_intersection_of_line_and_circle
	is
		-- We assume the arc is a virtual circle and compute the
		-- intersections of the line with the virtual circle.
		
		-- Build a virtual circle from the given arc:
		vc : constant type_circle := (
				center => arc.center, 
				radius => get_radius_start (arc));

		-- Compute the intersections of the line with the virtual circle:
		vi : constant type_intersection_of_line_and_circle := 
			get_intersection (line, vc);
		

	begin
		--new_line;
		--put_line ("---");
		--put_line (to_string (line));
		--put_line (to_string (vc));
		--put_line (to_string (arc));
		
		case vi.status is
			when NONE_EXIST => 
				--put_line ("none");
				
				-- line does not meet the virtual circle
				-- and does not meet the given arc either.
				return (status => NONE_EXIST);

			when ONE_EXISTS => 
				-- line is a tangent to the virtual circle
				
				--put_line ("one");					

				-- Test whether the point where the tangent meets the
				-- circle is on the given arc:
				if on_arc (vi.intersection.vector, arc) then
					return (ONE_EXISTS, vi.intersection, TANGENT);
				else
					return (status => NONE_EXIST);
				end if;
				
			when TWO_EXIST => 
				-- line is a secant to the virtual circle:
				
				--put_line ("two");

				-- Test whether the points where the secant meets the
				-- circle are on the given arc:

				--put_line ("p1" & to_string (to_point ((vi.intersection_1.point))));
				--put_line ("p2" & to_string (to_point ((vi.intersection_2.point))));

				declare
					oa_1 : constant boolean := on_arc (vi.intersection_1.vector, arc);
					oa_2 : constant boolean := on_arc (vi.intersection_2.vector, arc);
				begin					
					--put_line (boolean'image (oa_1));
					--put_line (boolean'image (oa_2));
					
					if oa_1 and oa_2 then
						-- both intersections are on the arc
						return (TWO_EXIST, vi.intersection_1, vi.intersection_2);
						
					elsif oa_1 then
						-- only intersection 1 is on the arc
						return (ONE_EXISTS, vi.intersection_1, SECANT);
						
					elsif oa_2 then
						-- only intersection 2 is on the arc
						return (ONE_EXISTS, vi.intersection_2, SECANT);
						
					else
						-- none intersection is on the arc
						return (status => NONE_EXIST);
					end if;
				end;
		end case;
		
	end get_intersection;

	
	function arc_end_point (
		center		: in type_point;
		start_point	: in type_point;	
		angle 		: in type_rotation) -- unit is degrees
		return type_point'class
	is						
		arc : type_arc;

		radius : type_float_internal;
		angle_start, angle_end : type_float_internal; -- unit is radians
		end_x, end_y : type_float_internal;
		
	begin -- arc_end_point
		
		-- build an arc from the information available
		arc := (
			center		=> center,
			start_point	=> start_point,
			end_point	=> origin, -- not determined yet
			direction	=> get_direction (angle)
			);
		
		-- move arc so that its center is at 0/0
		move_to (arc, origin);

		-- calculate the radius of the arc
		radius := type_float_internal (get_distance_total (arc.center, arc.start_point));

		-- calculate the angle where the arc begins:

		-- NOTE: If x and y are zero then the arctan operation is not possible. 
		-- In this case we assume the resulting angle is zero.
		if get_x (arc.start_point) = zero and get_y (arc.start_point) = zero then
			angle_start := 0.0;
		else
			angle_start := type_float_internal (arctan (
					y => type_float_internal (get_y (arc.start_point)),
					x => type_float_internal (get_x (arc.start_point))));
		end if;
		
		-- the angle where the arc ends:
		angle_end := angle_start + to_radians (angle);

		-- The end point of the arc:
		end_y := sin (type_float_internal (angle_end)) * radius;
		end_x := cos (type_float_internal (angle_end)) * radius;

		return set (
			--x	=> type_distance (end_x),
			--y	=> type_distance (end_y));
			x	=> to_distance (end_x),
			y	=> to_distance (end_y));
						
	end arc_end_point;

	
	procedure move_by (
		arc		: in out type_arc;
		offset	: in type_distance_relative)
	is begin
		move_by (point => arc.center,      offset => offset);
		move_by (point => arc.start_point, offset => offset);
		move_by (point => arc.end_point,   offset => offset);
	end move_by;

	
	procedure move_to (
		arc			: in out type_arc;
		position	: in type_point)
	is
		offset : constant type_distance_relative :=
			get_distance_relative (arc.center, position);
	begin
		-- move the center of the arc to the given position
		move_to (arc.center, position);

		-- move start and end point of the arc by the computed offset
		move_by (point => arc.start_point, offset => offset);
		move_by (point => arc.end_point,   offset => offset);
	end move_to;

	
	procedure mirror (
		arc			: in out type_arc;
		axis		: in type_axis_2d)
	is begin
		mirror (arc.center, axis);
		mirror (arc.start_point, axis);
		mirror (arc.end_point, axis);
		arc.direction := reverse_direction (arc.direction);
	end mirror;

	procedure rotate_by (
		arc			: in out type_arc;
		rotation	: in type_rotation) is
	begin
		rotate_by (arc.center, rotation);
		rotate_by (arc.start_point, rotation);
		rotate_by (arc.end_point, rotation);
	end;


	function split_arc (arc_in : in type_arc) 
		return type_arcs
	is
		-- normalize the given arc so that its direction is always CCW
		arc : type_arc := type_arc (normalize_arc (arc_in));
		
		-- the x and y-position of the center, start and end point of the given arc:
		CX : constant type_distance := get_x (arc.center);
		CY : constant type_distance := get_y (arc.center);			
		SX : type_distance;
		EX : type_distance;

		-- the radius of the given arc:
		R  : constant type_distance_positive := get_radius_start (arc);

		-- The connecting points between the resulting arcs are where an
		-- imaginary vertical line intersects an imaginary circle at its highest and 
		-- lowest point:
		PU : constant type_point := type_point (set (CX, CY + R));
		PL : constant type_point := type_point (set (CX, CY - R));
		
		-- the boundaries of the given arc:
		by : constant type_boundaries := get_boundaries (arc, zero);

		S_left, E_left : boolean := false;

		function left_of_center (x : in type_distance) return boolean is begin
			if x < CX then return true; -- x is left of center.x
			else return false; -- x is equal or right of center.x or 
			end if;
		end left_of_center;

		-- There can be up to 3 segments after splitting the arc.
		-- All arc segments have the same center and direction:
		result_2 : type_arcs (1..2);
		result_3 : type_arcs (1..3);

		-- If the arc breaks up into 3 arc fragments, then it may
		-- happen that the first or the last fragment has zero length.
		-- In this case this function removes such a fragment and 
		-- returns two fragments instead.
		-- Otherwise the three fragments are untouched.
		function remove_useless_fragments return type_arcs is begin
			if zero_length (result_3 (1)) then
				result_2 (1) := result_3 (2);
				result_2 (2) := result_3 (3);
				return result_2;
				
			elsif zero_length (result_3 (3)) then
				result_2 (1) := result_3 (1);
				result_2 (2) := result_3 (2);
				return result_2;
			else
				return result_3;
			end if;
		end remove_useless_fragments;
		
	begin -- split_arc

		-- test whether the arc can be split at all:
		if by.smallest_x < CX and by.greatest_x > CX then
			-- arc extends to the right and to the left of its center

			-- get x of start and end point:
			SX := get_x (arc.start_point);
			EX := get_x (arc.end_point);

			-- get position of start and end point (relative to center):
			S_left := left_of_center (SX);
			E_left := left_of_center (EX);
			
			if S_left then
				if E_left then
					-- start point is below end point

					-- the lower left segment:
					result_3 (1) := (
						center		=> arc.center, 
						start_point	=> arc.start_point,
						end_point	=> PL,
						direction 	=> CCW);

					-- the segment on the right
					result_3 (2) := (
						center		=> arc.center, 
						start_point	=> PL,
						end_point	=> PU,
						direction 	=> CCW);

					-- the upper left segment:
					result_3 (3) := (
						center		=> arc.center, 
						start_point	=> PU,
						end_point	=> arc.end_point,
						direction 	=> CCW);

					return remove_useless_fragments;
					
				else -- end point is on the right
					-- the segment on the left:
					result_2 (1) := (
						center		=> arc.center, 
						start_point	=> arc.start_point,
						end_point	=> PL,
						direction 	=> CCW);

					-- the segment on the right
					result_2 (2) := (
						center		=> arc.center, 
						start_point	=> PL,
						end_point	=> arc.end_point,
						direction 	=> CCW);

					-- return segment 1 and 2
					return result_2;
				end if;

				
			else -- start point is on the right
				if E_left then

					-- the segment on the right:
					result_2 (1) := (
						center		=> arc.center, 
						start_point	=> arc.start_point,
						end_point	=> PU,
						direction 	=> CCW);

					-- the segment on the left
					result_2 (2) := (
						center		=> arc.center, 
						start_point	=> PU,
						end_point	=> arc.end_point,
						direction 	=> CCW);

					-- return segment 1 and 2
					return result_2;

					
				else
					-- end point is on the right
					
					-- start point is above end point

					-- the upper right segment:
					result_3 (1) := (
						center		=> arc.center, 
						start_point	=> arc.start_point,
						end_point	=> PU,
						direction 	=> CCW);

					-- the segment on the left
					result_3 (2) := (
						center		=> arc.center, 
						start_point	=> PU,
						end_point	=> PL,
						direction 	=> CCW);

					result_3 (3) := (
						center		=> arc.center, 
						start_point	=> PL,
						end_point	=> arc.end_point,
						direction 	=> CCW);

					return remove_useless_fragments;
				end if;
			end if;
			
		else
			raise constraint_error with "can not split " & to_string (arc) & " !";
		end if;

	end split_arc;


	function split_circle (circle_in : in type_circle) 
		return type_arcs
	is
		-- the x and y-position of the center of the given circle:
		CX : constant type_distance := get_x (circle_in.center);
		CY : constant type_distance := get_y (circle_in.center);
		R  : constant type_distance_positive := circle_in.radius;

		-- The connecting points between the resulting arcs are where an
		-- imaginary vertical line intersects the circle at its highest and 
		-- lowest point:
		PU : constant type_point := type_point (set (CX, CY + R));
		PL : constant type_point := type_point (set (CX, CY - R));

		-- There will be only 2 arcs.
		result : type_arcs (1..2);
	begin
		-- the arc on the left:
		result (1) := (center => circle_in.center, start_point => PU, 
						end_point => PL, direction => CCW);

		-- the arc on the right:
		result (2) := (center => circle_in.center, start_point => PL,
						end_point => PU, direction => CCW);			

		return result;
	end split_circle;
	

	function get_distance_to_circumfence (
		point	: in type_point;
		circle	: in type_circle)
		return type_distance_polar
	is
		result : type_distance_polar;
	begin
		result := get_distance (circle.center, point);
		set_absolute (result, circle.radius - get_absolute (result));
		return result;
	end get_distance_to_circumfence;

	
	function get_shortest_distance (
		point	: in type_point;
		circle	: in type_circle)
		return type_distance_polar
	is
		result : type_distance_polar;

		-- Two cases can exist:
		-- 1. point is inside the circle
		-- 2. point is outside the circle
		
		-- the polar distance from center to point:
		d_cp : constant type_distance_polar := get_distance (circle.center, point);

		-- the polar distance from point to center:
		d_pc : constant type_distance_polar := get_distance (point, circle.center);
		
		dd : type_distance;
	begin
		--result := get_distance (point, circle.center);
		--set_absolute (result, get_absolute (result) - circle.radius);

		--dd := type_distance (round (get_absolute (d_pc) - circle.radius));
		dd := get_absolute (d_pc) - circle.radius;
		
		if dd > zero then -- point outside of circle

			-- Now the polar distance from point to center matters:
			result := d_pc;

			-- Since we are interested in the distance to the circumfence
			-- the radius must be subtracted from the total distance:
			set_absolute (result, get_absolute (d_pc) - circle.radius);
			
		else -- point inside circle or on circumfence

			-- Now the polar distance from center to point matters:
			result := d_cp;
			
			-- Since we are interested in the distance to the circumfence
			-- the total distance must be subtracted from the radius:
			set_absolute (result, circle.radius - get_absolute (d_pc));
		end if;
		
		return result;
	end get_shortest_distance;

	
	procedure move_by (
		circle	: in out type_circle;
		offset	: in type_distance_relative)
	is begin
		move_by (point	=> circle.center,	offset => offset);
	end move_by;
	
	procedure mirror (
		circle		: in out type_circle;
		axis		: in type_axis_2d) is
	begin
		mirror (circle.center, axis);
	end mirror;
	
	procedure rotate_by (
		circle		: in out type_circle;
		rotation	: in type_rotation) is
	begin
		rotate_by (circle.center, rotation);
	end;
	
	function get_boundaries (
		circle		: in type_circle;
		line_width	: in type_distance_positive)						
		return type_boundaries 
	is
		result : type_boundaries;

		half_width : constant type_distance_positive := line_width * 0.5;
	begin
		-- X axis
		result.smallest_x := get_x (circle.center) - circle.radius;
		result.greatest_x := get_x (circle.center) + circle.radius;

		-- Y axis
		result.smallest_y := get_y (circle.center) - circle.radius;
		result.greatest_y := get_y (circle.center) + circle.radius;

		
		-- extend the boundaries by half the line width;
		result.smallest_x := result.smallest_x - half_width;
		result.smallest_y := result.smallest_y - half_width;

		result.greatest_x := result.greatest_x + half_width;
		result.greatest_y := result.greatest_y + half_width;
		
		return result;
	end get_boundaries;

	
	function on_circle (
		point		: in type_point;
		circle		: in type_circle)
		return boolean 
	is
		-- the distance from center to point:
		DCP: constant type_distance_positive := 
			get_distance_total (point, circle.center);
	begin
		if abs (DCP - circle.radius) <= type_distance'small then

			-- Point is on circumfence of circle.
			return true;
		else
			return false; 
		end if;
	end on_circle;


	function get_point_to_circle_status (
		point		: in type_point;
		circle		: in type_circle)
		return type_point_status
	is begin
		if get_distance_total (point, circle.center) < circle.radius then
			return INSIDE;
		else
			return OUTSIDE; 
		end if;
	end get_point_to_circle_status;
	

	function intersect (
		circle	: in type_circle'class;
		line	: in type_line'class)
		return boolean
	is
		result : boolean := false;
		
		l : constant type_line_vector := to_line_vector (line);
		i : constant type_intersection_of_line_and_circle := get_intersection (l, circle);
	begin
		case i.status is
			when NONE_EXIST => null;
			
			when ONE_EXISTS => 
				if on_line (i.intersection.vector, line) then
					result := true;
				end if;		

			when TWO_EXIST =>
				if on_line (i.intersection_1.vector, line) then
					result := true;
				end if;
				
				if on_line (i.intersection_2.vector, line) then
					result := true;
				end if;
				
		end case;
		
		return result;			
	end intersect;
	
	
	function get_distance (
		circle	: in type_circle'class;
		line	: in type_line'class)
		return type_distance
	is
		result : type_distance := zero;

		--debug : constant boolean := false;
		--debug : constant boolean := true;
		
		-- the distance from circumfence to start of line:
		ds : type_distance_positive;
		
		-- the distance from circumfence to end of line:
		de : type_distance_positive;

		-- the distance from center perpendicular to line:
		dp : type_distance_point_line;
		
	begin
		--log (text => "");
		--log (text => "circle" & to_string (circle));
		--log (text => "line  " & to_string (line));

		--new_line;			
		--put_line ("circle: " & to_string (circle));
		
		dp := get_distance (to_vector (circle.center), line, WITH_END_POINTS);
		
		--if debug then
			----put_line ("circle: " & to_string (circle));
			--put_line ( "is" & to_string (get_intersection (dp)));
			--put_line ( "dl" & to_string (get_distance (dp)));
			--put_line ( "ds" & to_string (get_distance_total (circle.center, line.start_point)));
			--put_line ( "de" & to_string (get_distance_total (circle.center, line.end_point)));
		--end if;
		
		
		--if on_line (get_intersection (dp), line) then
		if not dp.out_of_range then -- line passes the circle
			result := to_distance (get_distance (dp));
			--log (text => "line passes the circle");

			--if debug then
				--put_line ("line passes the circle");
			--end if;
			
		else -- line does not pass the circle
			ds := get_distance_total (circle.center, line.start_point);
			de := get_distance_total (circle.center, line.end_point);
			
			-- select among ds and de the smallest:
			result := get_smallest (ds, de);
		end if;
		
		-- We are interested in the distance of the circumfence to
		-- the line. Therefore the radius must be subtracted:
		result := result - circle.radius;
		
		--log (text => "dp" & to_string (result));

		--if debug then
			--put_line ("d " & to_string (result));
		--end if;
		
		return result;
	end get_distance;


	function get_distance (
		circle	: in type_circle'class;
		arc		: in type_arc'class)
		return type_distance
	is
		result : type_distance := zero;

		dp : type_distance_polar;
	begin
		--new_line;
		--put_line (to_string (circle));
		--put_line ("-" & to_string (arc));

		--log (text => "circle" & to_string (circle));
		--log (text => "arc" & to_string (arc));
		
		dp := get_shortest_distance (circle.center, arc);
		
		result := get_absolute (dp) - circle.radius;
		return result;
	end get_distance;



	function get_distance (
		circle_1	: in type_circle'class;
		circle_2	: in type_circle'class)
		return type_distance
	is
		result : type_distance := zero;
		dp : type_distance_polar;
	begin
		dp := get_distance (circle_1.center, circle_2.center);
		result := get_absolute (dp) - circle_1.radius - circle_2.radius;
		return result;
	end get_distance;

	
	
	function get_tangent_angle (p : in type_point) 
		return type_tangent_angle_circle
	is
		a : type_rotation := get_angle (get_distance (origin, p));
	begin
		--put_line (to_string (a));

		-- The angle a ranges from -180 to 180 degrees.
		
		case get_quadrant (p) is
			when ONE	=> a := a - 90.0;
			when TWO	=> a := a - 90.0;
			when THREE	=> a := a + 90.0;
			when FOUR	=> a := a + 90.0;
		end case;
		
		return a;
	end get_tangent_angle;

	
	
	function get_intersection (
		line	: in type_line_vector;
		circle	: in type_circle)
		return type_intersection_of_line_and_circle
	is
		-- This function bases on the approach by
		-- Weisstein, Eric W. "Circle-Line Intersection." 
		-- From MathWorld--A Wolfram Web Resource. 
		-- <https://mathworld.wolfram.com/Circle-LineIntersection.html>.
		-- It has been further-on extended so that the angles
		-- of intersections are computed along with the actual points
		-- of intersection.

		-- The appoach assumes the circle center at 0/0.
		-- So we must first move the line by
		-- the given center of the circle. The intersections,
		-- if any exist, must in the end be moved back by this offset:
		offset : constant type_distance_relative := to_distance_relative (circle.center);

		-- The circle radius is:
		r : constant type_float_internal := type_float_internal (circle.radius);
		
		-- The line starts here:
		x1, y1 : type_float_internal;

		-- The line ends here:
		x2, y2 : type_float_internal;
		
		x, y, dx, dy, dr, DI : type_float_internal;

		-- scratch variables:
		line_moved : type_line_vector;
		v_end : type_vector;
		a, b, c, d : type_float_internal;

		-- Due to unavoidable errors this threshold is used
		-- instead of 0.0 when detecting the distance to the circle:
		--th : constant type_float_internal := 1.0E-17; -- CS refine
		th : constant type_float_internal := 1.0E-14; -- CS refine

		s : type_intersection_status_of_line_and_circle;
		intersection_1, intersection_2 : type_point;

		line_angle : constant type_rotation := get_angle (line);

		intersection_angle_1, intersection_angle_2 : type_rotation;

		-- Computes the angle of intersection of the given line with
		-- the circle at point p.
		-- NOTE: Since we assume a secant, the angle
		-- line_angle is travelling with, must not be a multiple of 90 degrees !
		function compute_intersection_angle (p : in type_point) 
			return type_rotation
		is
			result : type_rotation;

			-- Compute the tangent at the intersection:
			tangent_angle : type_rotation := get_tangent_angle (p);
		begin
			--log (text => "line angle:" & to_string (line_angle));
			--log (text => "tangent angle A:" & to_string (tangent_angle));
			
			if tangent_angle < 0.0 then
				tangent_angle := abs (tangent_angle);
			else
				tangent_angle := 180.0 - tangent_angle;
			end if;

			--log (text => "tangent angle B:" & to_string (tangent_angle));
			
			result := line_angle + tangent_angle;

			if result > 180.0 then
				result := result - 180.0;
			end if;
			
			--log (text => "intersection angle:" & to_string (result));
			
			return result;
		end compute_intersection_angle;
		
	begin -- get_intersection
		--new_line;
		--put_line ("GI LC");
		--put_line (to_string (line));
		--put_line (to_string (circle));
		
		-- Move the line by the offset (which is the center of the given circle):
		line_moved := move_by (line, invert (offset));
		v_end := add (line_moved.v_start, line_moved.v_direction);
		
		-- compute start and end point of line:
		x1 := type_float_internal (get_x (line_moved.v_start));
		y1 := type_float_internal (get_y (line_moved.v_start));
		
		x2 := type_float_internal (get_x (v_end));
		y2 := type_float_internal (get_y (v_end));
		
		dx := x2 - x1; -- the delta in x
		dy := y2 - y1; -- the delta in y

		dr := sqrt (dx ** 2 + dy ** 2);
		
		DI := x1 * y2 - x2 * y1;
		
		b := dr ** 2;
		a := r ** 2;
		c := DI ** 2;
		d := a * b - c; -- incidence of line and circle

		-- Theoretically the comparison should be against 0.0. 
		-- See comments on th above.
		--put_line (type_float_internal'image (d));
		
		if d < (-th) then
			
			--put_line ("none");
			
			s := NONE_EXIST;
			
			return (status => NONE_EXIST);
			
		elsif abs (d) < th then	
			--put_line ("one");
			
			s := ONE_EXISTS; -- tangent

			x := (DI * dy) / b;
			y := (-DI * dx) / b;

			intersection_1 := type_point (set (to_distance (x), to_distance (y)));
			-- NOTE: A constraint error is raised here if x or y is not in range 
			-- of type_position_axis !

			-- Move computed intersection back by offset
			-- (Which is the center of the given circle):
			move_by (intersection_1, offset);
			
			return (ONE_EXISTS, 
					(vector => to_vector (intersection_1), angle => line_angle),
					TANGENT);

			-- NOTE: The angle of the travel direction of the given line
			-- is now the angle of the tangent at this single intersection point.
			
		else
			--put_line ("two");
			
			s := TWO_EXIST; -- two intersections

			-- COMPUTE 1ST INTERSECTION:
			x := ( DI * dy + sgn (dy) * dx * sqrt (d)) / b;
			y := (-DI * dx + abs (dy) * sqrt (d))      / b;

			-- Compose the point of intersection 1:
			intersection_1 := type_point (set (to_distance (x), to_distance (y)));
			-- NOTE: A constraint error is raised here if x or y is not in range 
			-- of type_position_axis !
			
			intersection_angle_1 := compute_intersection_angle (intersection_1);
				
			-- Move computed intersection 1 back by offset
			-- (Which is the center of the given circle):
			move_by (intersection_1, offset);


			
			-- COMPUTE 2ND INTERSECTION:				
			x := ( DI * dy - sgn (dy) * dx * sqrt (d)) / b;
			y := (-DI * dx - abs (dy) * sqrt (d))      / b;
					
			-- Compose the point of intersection 2:
			intersection_2 := type_point (set (to_distance (x), to_distance (y)));
			-- NOTE: A constraint error is raised here if x or y is not in range 
			-- of type_position_axis !

			intersection_angle_2 := compute_intersection_angle (intersection_2);
			
			-- Move computed intersection 2 back by offset
			-- (Which is the center of the given circle):
			move_by (intersection_2, offset);				
			
			return (TWO_EXIST, 
					(vector => to_vector (intersection_1), angle => intersection_angle_1),
					(vector => to_vector (intersection_2), angle => intersection_angle_2)
					);

			
		end if;

	end get_intersection;


	function order_intersections (
		-- The start point of the line that intersects the circle.
		-- The start point must be outside the circle and will
		-- be passed through to the result unchanged.
		start_point		: in type_point;

		intersections	: in type_intersection_of_line_and_circle)
		return type_ordered_line_circle_intersections
	is
		result : type_ordered_line_circle_intersections := 
			(start_point => start_point, others => <>); -- pass start point through

		i : constant type_intersection_of_line_and_circle (TWO_EXIST) := intersections;

		d1, d2 : type_float_internal;
	begin
		-- the distance from start point to intersection point 1:
		d1 := get_distance_total (start_point, i.intersection_1.vector);

		-- the distance from start point to intersection point 2:
		d2 := get_distance_total (start_point, i.intersection_2.vector);

		if d1 < d2 then -- point ip1 is closer to start point that ip2
			result.entry_point := i.intersection_1;
			result.exit_point  := i.intersection_2;
			
		elsif d1 > d2 then -- point ip2 is closer to start point than ip1
			result.entry_point := i.intersection_2;
			result.exit_point  := i.intersection_1;

		else -- point ip1 has same distance to start point as ip2
			--put_line (to_string (d1)); put_line (to_string (d2));
			--put_line (to_string (ip1)); put_line (to_string (ip2));
			raise constraint_error;
		end if;
			
		return result;
	end order_intersections;


	
	
	function to_string (line : in type_line) return string is begin
		return 
			"line: S:" & to_string (line.start_point) 
			& " / E:" & to_string (line.end_point);
	end;

	function to_string (arc : in type_arc) return string is begin
		return "arc: "
			& "C:" & to_string (arc.center) 
			& " / S:" & to_string (arc.start_point) 
			& " / E:" & to_string (arc.end_point)
			& " / D: " & to_string (arc.direction);
	end to_string;

	function to_string (circle : in type_circle) return string is begin
		return
			"circle: C:" & to_string (circle.center) 
			& " / R:" & to_string (circle.radius);
	end to_string;




	
	function get_left_end (
		line		: in type_line;
		boundaries	: in type_boundaries := boundaries_default)
		return type_point
	is
		p : type_point; -- to be returned
		b : type_boundaries := boundaries;								   
	begin
		-- If no boundaries provided, compute them.
		-- Otherwise use provided boundaries as they are:
		if b = boundaries_default then
			b := get_boundaries (line, zero); -- a polygon line has zero width
		end if;
		
		if b.smallest_x = get_x (line.start_point) then
			p := line.start_point;
			
		elsif b.smallest_x = get_x (line.end_point) then
			p := line.end_point;

		else
			-- If boundaries where provided and neither start nor end point
			-- of line matches, issue exception:
			raise constraint_error with to_string (b) 
				& " invalid for line" & to_string (line) & " !";
		end if;
			
		return p;
	end get_left_end;

	
	function get_right_end (
		line		: in type_line;
		boundaries	: in type_boundaries := boundaries_default)
		return type_point
	is
		p : type_point; -- to be returned
		b : type_boundaries := boundaries;								   
	begin
		-- If no boundaries provided, compute them.
		-- Otherwise use provided boundaries as they are:
		if b = boundaries_default then
			b := get_boundaries (line, zero); -- a polygon line has zero width
		end if;
		
		if b.greatest_x = get_x (line.start_point) then
			p := line.start_point;
			
		elsif b.greatest_x = get_x (line.end_point) then
			p := line.end_point;

		else
			-- If boundaries where provided and neither start nor end point
			-- of line matches, issue exception:
			raise constraint_error with to_string (b) 
				& " invalid for line" & to_string (line) & " !";
		end if;
			
		return p;
	end get_right_end;

	
	function get_lower_end (
		line		: in type_line;
		boundaries	: in type_boundaries := boundaries_default)
		return type_point
	is
		p : type_point; -- to be returned
		b : type_boundaries := boundaries;								   
	begin
		-- If no boundaries provided, compute them.
		-- Otherwise use provided boundaries as they are:
		if b = boundaries_default then
			b := get_boundaries (line, zero); -- a polygon line has zero width
		end if;
		
		if b.smallest_y = get_x (line.start_point) then
			p := line.start_point;
			
		elsif b.smallest_y = get_x (line.end_point) then
			p := line.end_point;

		else
			-- If boundaries where provided and neither start nor end point
			-- of line matches, issue exception:
			raise constraint_error with to_string (b) 
				& " invalid for line" & to_string (line) & " !";
		end if;
			
		return p;
	end get_lower_end;

	
	function get_upper_end (
		line		: in type_line;
		boundaries	: in type_boundaries := boundaries_default)
		return type_point
	is
		p : type_point; -- to be returned
		b : type_boundaries := boundaries;								   
	begin
		-- If no boundaries provided, compute them.
		-- Otherwise use provided boundaries as they are:
		if b = boundaries_default then
			b := get_boundaries (line, zero); -- a polygon line has zero width
		end if;
		
		if b.greatest_y = get_x (line.start_point) then
			p := line.start_point;
			
		elsif b.greatest_y = get_x (line.end_point) then
			p := line.end_point;

		else
			-- If boundaries where provided and neither start nor end point
			-- of line matches, issue exception:
			raise constraint_error with to_string (b) 
				& " invalid for line" & to_string (line) & " !";
		end if;
			
		return p;
	end get_upper_end;

	
end et_geometry_2;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
