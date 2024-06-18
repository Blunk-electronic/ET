------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               GEOMETRY                                   --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2024                                                       --
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

with ada.text_io;				use ada.text_io;

with ada.strings;				use ada.strings;
with ada.strings.fixed;			use ada.strings.fixed;
with ada.strings.unbounded;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with et_exceptions;				use et_exceptions;



package body et_geometry_2a is
	
	function get_info (editor: in string)
		return string 
	is 
		use ada.characters.latin_1;
		distance_digits_total : constant positive := type_distance_model'digits;
		distance_digits_right : constant positive := type_distance_model'scale;

		--distance_coarse_digits_total : constant positive := type_distance_model_coarse'digits;
		--distance_coarse_digits_right : constant positive := type_distance_model_coarse'scale;

		rotation_digits_total : constant positive := type_rotation'digits;
		rotation_digits_right : constant positive := type_rotation'scale;
	begin
		--CS put_line ("rounding error:" & pac_geometry_1.to_string (type_float'small));
		-- CS write accuracy
		
		return to_upper (editor & " editor:")
		& lf & "distance fine [mm]"
		& lf & "min:        " & type_distance_model'image (type_distance_model'first)
		& lf & "max:        " & type_distance_model'image (type_distance_model'last)
		& lf & "axis min:   " & type_distance_model'image (axis_min)
		& lf & "axis max:   " & type_distance_model'image (axis_max)
		& lf & "resolution: " & type_distance_model'image (type_distance_model'small)
		& lf & "digits"
		& lf & "left:       " & positive'image (distance_digits_total - distance_digits_right)
		& lf & "right:      " & positive'image (distance_digits_right)
		& lf & "total:      " & positive'image (type_distance_model'digits)
		& lf
		--& lf & "distance coarse [mm]"
		--& lf & "min:        " & type_distance_model_coarse'image (type_distance_model_coarse'first)
		--& lf & "max:        " & type_distance_model_coarse'image (type_distance_model_coarse'last)
		--& lf & "resolution: " & type_distance_model_coarse'image (type_distance_model_coarse'small)
		--& lf & "digits"
		--& lf & "left:       " & positive'image (distance_coarse_digits_total - distance_coarse_digits_right)
		--& lf & "right:      " & positive'image (distance_coarse_digits_right)
		--& lf & "total:      " & positive'image (type_distance_model_coarse'digits)
		--& lf
		& lf & "rotation/angle [degrees (1/360)], mathematical sense, ccw"
		& lf & "min:        " & type_rotation'image (type_rotation'first)
		& lf & "max:        " & type_rotation'image (type_rotation'last)
		& lf & "resolution: " & type_rotation'image (type_rotation'small)
		& lf & "digits"
		& lf & "left:       " & positive'image (rotation_digits_total - rotation_digits_right)
		& lf & "right:      " & positive'image (rotation_digits_right)
		& lf & "total:      " & positive'image (type_rotation'digits)
		& lf
		& lf & "internal float"
		& lf & "min:        " & type_float'image (type_float'first)
		& lf & "max:        " & type_float'image (type_float'last)
		& lf & "digits:     " & positive'image (type_float'digits)
		& lf;
	end get_info;


	
	
	function to_string (
		distance : in type_distance_model)
		return string
	is begin
		return type_distance_model'image (distance);
	end to_string;


	function to_distance (f : in type_float)
		return type_distance_model 
	is
		use pac_float_numbers_io;
		
		d1 : type_distance_model;
		d2 : type_float;

		f1 : constant type_float := 5.0 * type_float (type_distance_model'small);
		-- CS should be a package wide constant ?
	begin
		d1 := type_distance_model (f);
		
		d2 := 10.0 * abs (f - type_float (d1));
		
		if f < 0.0 then
			if d2 > f1 then
				d1 := d1 - type_distance_model'small;
			end if;
		else
			if d2 > f1 then
				d1 := d1 + type_distance_model'small;
			end if;
		end if;

		return d1;
		
		--if f < 0.0 then
			--declare
				--r : string (1 .. type_distance'digits + 2); -- sign + point
			--begin
				---- CS: IMPROVEMENT REQUIRED !!!
				--put (to => r, item => f, aft => type_distance'scale, exp => 0);
				--return type_distance'value (r);
			--end;
		--else
			--declare
				--r : string (1 .. type_distance'digits + 1); -- point
			--begin
				----put_line (type_float'image (f) & " " & natural'image (r'length));
				---- CS: IMPROVEMENT REQUIRED !!!
				--put (to => r, item => f, aft => type_distance'scale, exp => 0);
				--return type_distance'value (r);
			--end;
		--end if;


		exception when event: others =>
			put_line ("f: " & to_string (f));
			raise;

	end to_distance;



	function to_distance (dd : in string) 
		return type_distance_model
	is begin
		return type_distance_model'value (dd);

		exception when event: others =>
			raise syntax_error_2 with 
				"ERROR: Expect a distance instead of " 
				& enclose_in_quotes (dd) & " !";
	end to_distance;


	
	
	function to_string (
		rotation : in type_rotation_model)
		return string
	is begin
		return type_rotation_model'image (rotation);
	end to_string;

	
	
	function to_string (
		v : in type_vector_model)
		return string
	is begin
		return "x/y: "
			& to_string (v.x) & "/" & to_string (v.y);
	end to_string;


	function invert (
		point	: in type_vector_model)
		return type_vector_model
	is begin
		return (- point.x, - point.y);
	end invert;

	

	procedure move_by (
		point	: in out type_vector_model;
		offset	: in type_vector_model)
	is begin
		point.x := point.x + offset.x;
		point.y := point.y + offset.y;
	end move_by;



	function get_distance (
		point_one, point_two : in type_vector_model) 
		return type_distance_polar 
	is begin
		return get_distance (to_vector (point_one), to_vector (point_two));
	end get_distance;
	

	
	function get_distance (
		point	: in type_vector_model;
		vector	: in type_vector)
		return type_distance_polar
	is
		v : constant type_vector := to_vector (point);
	begin
		return get_distance (v, vector);
	end get_distance;

	
	
	function get_distance (
		p1, p2 : in type_vector_model)
		return type_distance_model_positive
	is
		use pac_float_numbers_functions;

		dx : type_float := abs (type_float (p2.x - p1.x));
		dy : type_float := abs (type_float (p2.y - p1.y));
		d : type_float;
	begin
		d := sqrt (dx**2.0 + dy**2.0);
		return type_distance_model_positive (d);
	end get_distance;
	

	function get_angle (
		p1, p2 : in type_vector_model)
		return type_rotation_model
	is
		use pac_float_numbers_functions;

		dx : type_float := type_float (p2.x - p1.x);
		dy : type_float := type_float (p2.y - p1.y);
		a : type_float;
	begin
		-- For a tangens operations, dx must not
		-- be zero. If it is zero, then dy determines
		-- whether the result is 90 or -90 degree:
		if dx /= 0.0 then
			a := arctan (dy, dx, 360.0);
		else
			if dy > 0.0 then
				a := 90.0;
			else
				a := -90.0;
			end if;
		end if;
		
		return type_rotation_model (a);

		exception
			when ADA.NUMERICS.ARGUMENT_ERROR => 
				put_line ("tangens error");
				raise;
		
	end get_angle;



	function get_x (
		point : in type_vector_model) 
		return type_position_axis 
	is begin
		return point.x;
	end;


	
	function get_y (
		point : in type_vector_model)
		return type_position_axis 
	is begin
		return point.y;
	end;
	

	function set (
		x, y : in type_position_axis)
		return type_vector_model 
	is
		point : type_vector_model;
	begin
		point.x := x;
		point.y := y;
		return point;
	end;


	procedure set (
		point	: in out type_vector_model;
		axis 	: in type_axis_2d;
		value	: in type_position_axis)
	is begin
		case axis is
			when X => point.x := value;
			when Y => point.y := value;
		end case;
	end;

	
	
	function to_vector (
		point	: in type_vector_model)
		return type_vector 
	is 
		f : type_float := type_float (get_x (point));
	begin
		return set (
			x => type_float (get_x (point)),
			y => 0.0, --type_float (get_y (point)),
			z => 0.0
			);
	end to_vector;


	

	function to_point (
		v	: in type_vector)
		return type_vector_model
	is begin
		--log (text => "to point: vector" & to_string (v));
		
		-- Since the return is a 2D point,
		-- the z component of v must be zero:
		if get_z (v) /= 0.0 then
			raise constraint_error;
		end if;
					
		return set (
			x => to_distance (get_x (v)),
			y => to_distance (get_y (v)));

			-- Do not use type_distance (get_x (v)) !
			-- function to_distance conducts rounding
			-- by bankers rule.
		
		exception
			when constraint_error =>
				raise constraint_error 
					with "vector component too great:" & to_string (v);

	end to_point;



	function to_point (
		x,y : in string)
		return type_vector_model
	is 
		result : type_vector_model;					
	begin
		result.x := to_distance (dd => x);
		result.y := to_distance (dd => y);
		return result;

		-- CS exception handler
	end to_point;


	
	

	function to_offset (
		p : in type_vector_model)
		return type_offset
	is begin
		return (type_float (p.x), type_float (p.y));
	end to_offset;

	
	
	function get_distance_total (
		point	: in type_vector_model;
		vector	: in type_vector)
		return type_float_positive
	is begin
		return get_distance_total (to_vector (point), vector);
	end get_distance_total;
	


	function get_distance_relative (
		point_one, point_two : in type_vector_model)
		return type_distance_relative
	is
		d : type_distance_relative;
	begin
		d.x := point_two.x - point_one.x;
		d.y := point_two.y - point_one.y;
		return d;
	end get_distance_relative;



	
	function get_distance_total (
		point_one, point_two : in type_vector_model) 
		return type_float_positive 
	is begin
		return get_distance_total (to_vector (point_one), to_vector (point_two));
	end get_distance_total;



	procedure move_by (
		point	: in out type_vector_model;
		offset	: in type_distance_relative) 
	is begin
		point.x := point.x + offset.x;
		point.y := point.y + offset.y;
	end move_by;

	
	
	procedure move_to (
		point		: in out type_vector_model;
		destination	: in type_vector_model) 
	is begin
		point.x := destination.x;
		point.y := destination.y;
	end move_to;



	
	
	function to_string (
		box : in type_area)
		return string
	is begin
		return "(x/y/w/h): "
			& to_string (box.position.x) & "/"
			& to_string (box.position.y) & "/"
			& to_string (box.width) & "/"
			& to_string (box.height);
	end to_string;

	
	function get_corners (
		area	: in type_area)
		return type_area_corners
	is
		result : type_area_corners;
	begin
		result.BL := (area.position.x, area.position.y);
		result.BR := (area.position.x + area.width, area.position.y);

		result.TL := (area.position.x, area.position.y + area.height); 
		result.TR := (area.position.x + area.width, 
					  area.position.y + area.height); 
		return result;
	end get_corners;


	
	function get_center (
		area	: in type_area)
		return type_vector_model
	is
		result : type_vector_model;
	begin
		result.x := area.position.x + area.width  * 0.5;
		result.y := area.position.y + area.height * 0.5;
		return result;
	end get_center;


	
	function in_area (
		point	: type_vector_model;
		area	: type_area)
		return boolean
	is
		result : boolean := false;
	begin
		-- text x-axis:
		if point.x >= area.position.x then
			if point.x <= area.position.x + area.width then

				-- test y-axis:
				if point.y >= area.position.y then
					if point.y <= area.position.y + area.height then
						result := true;
					end if;
				end if;
				
			end if;
		end if;
		
		return result;
	end in_area;



	

	
	function areas_overlap (
		A, B : in type_area)
		return boolean
	is
		-- CS: Optimization required. Compiler options ?
		-- CS: rename lx, gx, ly, gy to x1, x2, y1, y2
		
		-- AREA A:
		-- This is the lowest x used by area A
		A_lx : type_distance_model renames A.position.x;

		-- This is the greatest x used by area A
		A_gx : constant type_distance_model := A_lx + A.width;

		
		-- This is the lowest y used by area A
		A_ly : type_distance_model renames A.position.y;

		-- This is the greatest y used by area A
		A_gy : constant type_distance_model := A_ly + A.height;


		-- AREA B:
		-- This is the lowest x used by area B
		B_lx : type_distance_model renames B.position.x;

		-- This is the greatest x used by area B
		B_gx : constant type_distance_model := B_lx + B.width;

		
		-- This is the lowest y used by area B
		B_ly : type_distance_model renames B.position.y;

		-- This is the greatest y used by area B
		B_gy : constant type_distance_model := B_ly + B.height;

	begin
		-- If all of the four criteria are true then the two 
		-- areas DO overlap:
		if	B_lx < A_gx 
		and	B_gx > A_lx
		and	B_ly < A_gy
		and	B_gy > A_ly then
			return true;
		else
			return false;
		end if;
	end areas_overlap;


	procedure merge_areas (
		A : in out type_area;
		B : in type_area)
	is
		-- CS: Optimization required. Compiler options ?
		
		-- AREA A:
		-- This is the lowest x used by area A
		A_lx : type_distance_model renames A.position.x;

		-- This is the greatest x used by area A
		A_gx : type_distance_model := A_lx + A.width;

		
		-- This is the lowest y used by area A
		A_ly : type_distance_model renames A.position.y;

		-- This is the greatest y used by area A
		A_gy : type_distance_model := A_ly + A.height;


		-- AREA B:
		-- This is the lowest x used by area B
		B_lx : type_distance_model renames B.position.x;

		-- This is the greatest x used by area B
		B_gx : type_distance_model := B_lx + B.width;

		
		-- This is the lowest y used by area B
		B_ly : type_distance_model renames B.position.y;

		-- This is the greatest y used by area B
		B_gy : type_distance_model := B_ly + B.height;

	begin
		-- x-axis:
		if B_lx < A_lx then
			A_lx := B_lx;
		end if;
		
		if B_gx > A_gx then
			A_gx := B_gx;
		end if;

		-- y-axis:
		if B_ly < A_ly then
			A_ly := B_ly;
		end if;
		
		if B_gy > A_gy then
			A_gy := B_gy;
		end if;

		A.width  := A_gx - A_lx;
		A.height := A_gy - A_ly;
	end merge_areas;



-- LINE:	

	function to_string (line : in type_line) return string is begin
		return 
			"line: S:" & to_string (line.start_point) 
			& " / E:" & to_string (line.end_point);
	end;


	function to_line_fine (
		line : in type_line)
		return type_line_fine
	is begin
		return (
			start_point	=> to_vector (line.start_point),
			end_point	=> to_vector (line.end_point));
	end to_line_fine;

	
	
	function on_line (
		line	: in type_line;
		vector	: in type_vector)
		return boolean
	is begin
		return on_line (vector, to_line_fine (line));
	end on_line;

	
	function on_line (
		line	: in type_line;
		point	: in type_vector_model)
		return boolean
	is begin
		return on_line (to_vector (point), to_line_fine (line));
	end on_line;
	


	function get_start_vector (
		line	: in type_line)
		return type_vector 
	is begin
		return set (
			x => type_float (line.start_point.x),
			y => type_float (line.start_point.y),
			z => 0.0);
	end get_start_vector;

	
	function get_end_vector (
		line	: in type_line)
		return type_vector 
	is begin
		return set (
			x => type_float (line.end_point.x),
			y => type_float (line.end_point.y),
			z => 0.0);
	end get_end_vector;


	function get_direction_vector (
		line	: in type_line)
		return type_vector 
	is begin
		return set (
			x => type_float (line.end_point.x - line.start_point.x),
			y => type_float (line.end_point.y - line.start_point.y),
			z => 0.0);
	end get_direction_vector;

	

	function to_line_vector (
		line	: in type_line)
		return type_line_vector
	is begin
		return (
			v_start		=> get_start_vector (line),
			v_direction	=> get_direction_vector (line));
	end to_line_vector;

	
	

	function get_shortest_distance (
		line	: in type_line;
		point	: in type_vector_model)
		return type_float_positive
	is begin
		return get_shortest_distance (to_vector (point), to_line_fine (line));
	end get_shortest_distance;


	
	function get_shortest_distance (
		line	: in type_line;
		point	: in type_vector)
		return type_float_positive
	is begin
		return get_shortest_distance (point, to_line_fine (line));
	end get_shortest_distance;

	

-- ARC:

	function to_string (arc : in type_arc) return string is begin
		return "arc: "
			& "C:" & to_string (arc.center) 
			& " / S:" & to_string (arc.start_point) 
			& " / E:" & to_string (arc.end_point)
			& " / D: " & to_string (arc.direction);
	end to_string;


	function get_radius_start (
		arc : in type_arc) 
		return type_float_positive 
	is begin
		return get_distance_total (arc.center, arc.start_point);
	end get_radius_start;


	
	function get_radius_end (
		arc : in type_arc)
		return type_float_positive
	is begin
		return get_distance_total (arc.center, arc.end_point);
	end get_radius_end;



	procedure move_to (
		arc			: in out type_arc;
		position	: in type_vector_model)
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
	

	function to_arc_angles (
		arc : in type_arc) 
		return type_arc_angles 
	is
	-- The angles may be negative. For example instead of 270 degree
	-- the angle can be -90 degree.
		result : type_arc_angles;
					
		-- Take a copy of the given arc in arc_tmp.
		arc_tmp : type_arc := arc;
	begin
		-- move arc_tmp so that its center is at 0/0
		move_to (arc_tmp, origin);

		-- the center is not changed:
		result.center := to_vector (arc.center);
		
		-- calculate the radius of the arc
		result.radius := get_distance_total (arc_tmp.center, to_vector (arc_tmp.start_point));

		-- calculate the angles where the arc begins and ends:

		-- NOTE: If x and y are zero then the arctan operation is not possible. 
		-- In this case we assume the resulting angle is zero.
		
		if get_x (arc_tmp.start_point) = zero and get_y (arc_tmp.start_point) = zero then
			result.angle_start := 0.0;
		else
			--result.angle_start := to_degrees (type_float (arctan (
					--y => type_float (get_y (arc_tmp.start_point)),
					--x => type_float (get_x (arc_tmp.start_point)))));
			result.angle_start := arctan (
					y => type_float (get_y (arc_tmp.start_point)),
					x => type_float (get_x (arc_tmp.start_point)), 
					cycle => units_per_cycle);
		end if;

		if get_x (arc_tmp.end_point) = zero and get_y (arc_tmp.end_point) = zero then
			result.angle_end := 0.0;
		else
			--result.angle_end := to_degrees (type_float (arctan (
					--y => type_float (get_y (arc_tmp.end_point)),
					--x => type_float (get_x (arc_tmp.end_point)))));
			result.angle_end := arctan (
					y => type_float (get_y (arc_tmp.end_point)),
					x => type_float (get_x (arc_tmp.end_point)),
					cycle => units_per_cycle);
		end if;

		-- make sure start and end angle are not equal
		if result.angle_start = result.angle_end then
			raise constraint_error; -- CS warning instead ?
		end if;
		
		-- direction is not changed:
		result.direction := arc.direction;
		
		return result;
	end to_arc_angles;


	
	
	function get_intersection (
		arc		: in type_arc;
		line	: in type_line_vector)
		-- CS optional argument for radius should improve performance
		-- default radius = zero ?
		return type_intersection_of_line_and_circle
	is
		-- We assume the arc is a virtual circle and compute the
		-- intersections of the line with the virtual circle.
		
		-- Build a virtual circle from the given arc:
		vc : constant type_circle := (
				center => arc.center, 
				radius => get_radius_start (arc),
				others => <>);

		-- Compute the intersections of the line with the virtual circle:
		vi : constant type_intersection_of_line_and_circle := 
			get_intersection (vc, line);
		

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
				if on_arc (arc, vi.intersection.vector) then
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
					oa_1 : constant boolean := on_arc (arc, vi.intersection_1.vector);
					oa_2 : constant boolean := on_arc (arc, vi.intersection_2.vector);
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

	

	function get_shortest_distance (
		arc		: in type_arc;
		point	: in type_vector_model)
		return type_distance_polar
	is
		result : type_distance_polar;

		procedure do_it is 
			-- Build a line that runs from the given point to the center of the arc:
			line : constant type_line_vector := 
				et_geometry_2a.to_line_vector (line => (point, arc.center, others => <>));
			-- IMPORTANT NOTE: Function to_line_vector computes the direction vector of line as:
			--  arc.center.x - point.x and arc.center.y - point.y.
			--  Function after_center (see below) bases on this fact. Otherwise its result
			--  will be nonsense !!

			-- Get the intersection(s) of the line with the arc:
			ILC : constant type_intersection_of_line_and_circle := get_intersection (arc, line);

			DPC : constant type_distance_polar := get_distance (point, arc.center);
			radius : constant type_float_positive := get_radius_start (arc);

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
				lambda : type_float;
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
						--result := get_distance_to_circumfence (point, (arc.center, radius));
						result := get_distance_to_circumfence ((arc.center, radius, others => <>), point);
						
				end case;				
			end if;				
		end do_it;

		
	begin -- get_shortest_distance
		--put_line ("point" & to_string (point) & " " & to_string (arc));
		
		if point = arc.center then
			-- If the given point is right on the center of the arc,
			-- then return zero distance and zero angle:

			set_absolute (result, 0.0);
			set_angle (result, 0.0);
		else
			do_it;
		end if;

		--put_line (to_string (result));
		
		return result;
	end get_shortest_distance;



	function get_shortest_distance (
		arc		: in type_arc;
		point	: in type_vector)
		return type_float
	is
		result : type_float := 0.0;

		--procedure do_it is 
			---- Build a line that runs from the given point to the center of the arc:
			--line : constant type_line_vector := to_line_vector (line => (point, arc.center));
			---- IMPORTANT NOTE: Function to_line_vector computes the direction vector of line as:
			----  arc.center.x - point.x and arc.center.y - point.y.
			----  Function after_center (see below) bases on this fact. Otherwise its result
			----  will be nonsense !!

			---- Get the intersection(s) of the line with the arc:
			--ILC : constant type_intersection_of_line_and_circle := get_intersection (line, arc);

			--DPC : constant type_distance_polar := get_distance (point, arc.center);
			--radius : constant type_distance_positive := get_radius_start (arc);

			---- Assigns to the result either the start or the end point of
			---- the arc, depending on which one is closer.
			--procedure compare_start_and_end_point is 
				--d_to_start, d_to_end : type_distance_polar;
			--begin
				--d_to_start := get_distance (point, arc.start_point);
				--d_to_end   := get_distance (point, arc.end_point);

				--if get_absolute (d_to_start) < get_absolute (d_to_end) then
					--result := d_to_start;
				--else
					--result := d_to_end;
				--end if;
			--end compare_start_and_end_point;

			---- Compute the distance of point to circle:
			--procedure like_circle is begin
				---- The arc can be treated like a circle.
				--result := DPC;
				--set_absolute (result, get_absolute (DPC) - radius);
			--end like_circle;

			---- Detects whether the given location vector i is after the
			---- center of the arc on "line".
			---- 1. It bases on the well known vector formula:
			----    i = start_vector + lambda * direction_vector
			----    This formula is solved by lambda.
			---- 2. It bases on the assumption that the direction_vector of line is
			----    already properly set (see comment above):
			--function after_center (i : in type_vector) return boolean is
				--lambda : type_float;
			--begin
				---- the start_vector is where "line" starts: the given point
				---- the direction vector is the direction of "line": towards the 
				---- center of the arc:
				--lambda := divide ((subtract (i, line.v_start)), line.v_direction);
				----put_line ("lambda" & to_string (lambda));
				--if lambda > 1.0 then
					--return true; -- i is after center of arc
				--else 
					--return false; -- i is on or before center of arc
				--end if;
			--end after_center;
			
		--begin -- do_it
			----log (text => "DPC" & to_string (get_absolute (DPC)));
			
			--if get_absolute (DPC) >= radius then
				---- point outside or on virtual circle
				----put_line ("outside");
				----log (text => "outside");
				
				--case ILC.status is
					--when NONE_EXIST =>
						----put_line ("none");
						---- line travels past the arc. no intersections
						--compare_start_and_end_point;

					--when ONE_EXISTS =>
						----put_line ("one");
						
						--if ILC.tangent_status = SECANT then
						---- line intersects the arc only once

							----log (text => "a: " & to_string (arc));
							----log (text => "l: " & to_string (line));
							----log (text => "i: " & to_string (ILC.intersection.point));
							
							--if after_center (ILC.intersection.vector) then
								---- intersection after center of arc
								----log (text => "i after center");
								--compare_start_and_end_point;
							--else
								---- intersection on circumfence between point and center of arc
								----put_line ("i before center");
								----put_line ("p betweeen circumfence and center");
								--like_circle;
							--end if;
							
						--else
							---- a tangent should never be the case
							--raise constraint_error;
						--end if;

					--when TWO_EXIST =>
						----put_line ("two");
						
						---- line intersects the virtual circle twice on
						---- its circumfence. But the intersection nearest
						---- to point is relevant:
						--like_circle;
						
				--end case;

			--else -- point is inside the virtual circle
				----put_line ("inside");
				----log (text => "inside");
				
				--case ILC.status is
					--when NONE_EXIST =>
						---- line travels past the arc
						----put_line ("none");
						--compare_start_and_end_point;

					--when ONE_EXISTS =>
						----put_line ("one");
						
						--if ILC.tangent_status = SECANT then
						---- line intersects the arc only once

							----put_line ("i: " & to_string (ILC.intersection.point));
							
							--if after_center (ILC.intersection.vector) then
								---- intersection after center of arc
								----put_line ("i after center");
								--compare_start_and_end_point;
							--else
								---- point is between circumfence and center of arc
								----put_line ("i before center");
								
								--result := DPC;
								--set_absolute (result, radius - get_absolute (DPC));
								----set_angle (result, add (get_angle (DPC), 180.0));
								--reverse_angle (result);
							--end if;

						--else
							---- a tangent should never be the case
							--raise constraint_error;
						--end if;

					--when TWO_EXIST =>
						---- treat the arc like a circle and compute distance point to circle:
						--result := get_distance_to_circumfence (point, (arc.center, radius));
						
				--end case;				
			--end if;				
		--end do_it;
		
	begin -- get_shortest_distance
		--put_line ("point" & to_string (point) & " " & to_string (arc));
		
		if to_point (point) = arc.center then
			-- If the given point is right on the center of the arc,
			-- then return zero distance and zero angle:

			result := 0.0;
		else
			--do_it; -- CS
			null;
		end if;

		--put_line (to_string (result));
		
		return result;
	end get_shortest_distance;




	function on_arc (
		arc		: in type_arc;
		vector	: in type_vector)
		return boolean 
	is
		-- The angle of the given point relative to the
		-- center of the given arc:
		P : type_angle_positive;

		-- A representation of the given arc in angles:
		arc_angles : constant type_arc_angles := to_arc_angles (arc);
		
		-- make the angles of the arc positive:
		S : type_angle_positive := arc_angles.angle_start;
		E : type_angle_positive := arc_angles.angle_end;

		procedure offset_ccw is 
			T : type_angle_positive;
		begin
			T := 360.0 - S;
			--log (text => "offset" & to_string (T));
			
			S := 0.0;
			E := E + T;
			P := add (P, T);
		end offset_ccw;

		procedure offset_cw is 
			T : type_angle_positive;
		begin
			T := 360.0 - E;
			E := 0.0;
			S := S + T;
			P := add (P, T);
		end offset_cw;

		distance_center_to_point : constant type_float :=
			get_distance_total (arc.center, vector);

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
		
		if abs (distance_center_to_point - arc_angles.radius) <= accuracy then
		
			-- Point is on circumfence of virtual circle.
			--log (text => "on circumfence");

			--log (text => "S" & to_string (S));
			--log (text => "E" & to_string (E));
			
			-- Compute the angle of the point relative to the center
			-- of the given arc:
			P := to_angle_positive (get_angle (get_distance (arc.center, vector)));
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

	

-- CIRCLE:
	
	function to_string (circle : in type_circle) return string is begin
		return
			"circle: C:" & to_string (circle.center) 
			& " / R:" & to_string (circle.radius);
	end to_string;



	function get_tangent_angle (p : in type_vector) 
		return type_tangent_angle_circle
	is
		a : type_angle := get_angle (get_distance (origin, p));
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
		circle	: in type_circle;
		line	: in type_line_vector)
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
		offset : constant type_offset := to_offset (circle.center);

		-- The circle radius is:
		r : constant type_float := type_float (circle.radius);
		
		-- The line starts here:
		x1, y1 : type_float;

		-- The line ends here:
		x2, y2 : type_float;
		
		x, y, dx, dy, dr, DI : type_float;

		-- scratch variables:
		line_moved : type_line_vector;
		v_end : type_vector;
		a, b, c, d : type_float;

		-- Due to unavoidable errors this threshold is used
		-- instead of 0.0 when detecting the distance to the circle:
		--th : constant type_float := 1.0E-17; -- CS refine
		th : constant type_float := 1.0E-14; -- CS refine

		s : type_intersection_status_of_line_and_circle;
		intersection_1, intersection_2 : type_vector;

		line_angle : constant type_angle := get_angle (line);

		intersection_angle_1, intersection_angle_2 : type_angle;

		-- Computes the angle of intersection of the given line with
		-- the circle at point p.
		-- NOTE: Since we assume a secant, the angle
		-- line_angle is travelling with, must not be a multiple of 90 degrees !
		function compute_intersection_angle (p : in type_vector) 
			return type_angle
		is
			result : type_angle;

			-- Compute the tangent at the intersection:
			tangent_angle : type_angle := get_tangent_angle (p);
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
		x1 := type_float (get_x (line_moved.v_start));
		y1 := type_float (get_y (line_moved.v_start));
		
		x2 := type_float (get_x (v_end));
		y2 := type_float (get_y (v_end));
		
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
		--put_line (type_float'image (d));
		
		if d < (-th) then
			
			--put_line ("none");
			
			s := NONE_EXIST;
			
			return (status => NONE_EXIST);
			
		elsif abs (d) < th then	
			--put_line ("one");
			
			s := ONE_EXISTS; -- tangent

			x := (DI * dy) / b;
			y := (-DI * dx) / b;

			--intersection_1 := type_vector_model (set (to_distance (x), to_distance (y)));
			intersection_1 := set (x, y, 0.0);
			-- NOTE: A constraint error is raised here if x or y is not in range 
			-- of type_position_axis !

			-- Move computed intersection back by offset
			-- (Which is the center of the given circle):
			move_by (intersection_1, offset);
			
			return (ONE_EXISTS, 
					(vector => intersection_1, angle => line_angle),
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
			--intersection_1 := type_vector_model (set (to_distance (x), to_distance (y)));
			intersection_1 := set (x, y, 0.0);
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
			--intersection_2 := type_vector_model (set (to_distance (x), to_distance (y)));
			intersection_2 := set (x, y, 0.0);
			-- NOTE: A constraint error is raised here if x or y is not in range 
			-- of type_position_axis !

			intersection_angle_2 := compute_intersection_angle (intersection_2);
			
			-- Move computed intersection 2 back by offset
			-- (Which is the center of the given circle):
			move_by (intersection_2, offset);				
			
			return (TWO_EXIST, 
					(vector => intersection_1, angle => intersection_angle_1),
					(vector => intersection_2, angle => intersection_angle_2)
					);

			
		end if;

	end get_intersection;



	function get_distance_to_circumfence (
		circle	: in type_circle;
		point	: in type_vector_model)
		return type_distance_polar
	is
		result : type_distance_polar;
	begin
		result := get_distance (circle.center, point);
		set_absolute (result, circle.radius - get_absolute (result));
		return result;
	end get_distance_to_circumfence;



	function get_shortest_distance (
		circle	: in type_circle;
		point	: in type_vector_model)
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
		
		dd : type_float;
	begin
		--result := get_distance (point, circle.center);
		--set_absolute (result, get_absolute (result) - circle.radius);

		--dd := type_distance (round (get_absolute (d_pc) - circle.radius));
		dd := get_absolute (d_pc) - circle.radius;
		
		if dd > 0.0 then -- point outside of circle

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


	function get_shortest_distance (
		point	: in type_vector;
		circle	: in type_circle)
		return type_float
	is
		result : type_float := 0.0;

		-- Two cases can exist:
		-- 1. point is inside the circle
		-- 2. point is outside the circle
		
		-- the polar distance from center to point:
		--d_cp : constant type_distance_polar := get_distance (circle.center, point);

		---- the polar distance from point to center:
		--d_pc : constant type_distance_polar := get_distance (point, circle.center);
		
		--dd : type_distance;
	begin
		--result := get_distance (point, circle.center);
		--set_absolute (result, get_absolute (result) - circle.radius);

		--dd := type_distance (round (get_absolute (d_pc) - circle.radius));
		--dd := get_absolute (d_pc) - circle.radius;
		
		--if dd > zero then -- point outside of circle

			---- Now the polar distance from point to center matters:
			--result := d_pc;

			---- Since we are interested in the distance to the circumfence
			---- the radius must be subtracted from the total distance:
			--set_absolute (result, get_absolute (d_pc) - circle.radius);
			
		--else -- point inside circle or on circumfence

			---- Now the polar distance from center to point matters:
			--result := d_cp;
			
			---- Since we are interested in the distance to the circumfence
			---- the total distance must be subtracted from the radius:
			--set_absolute (result, circle.radius - get_absolute (d_pc));
		--end if;
		
		return result;
	end get_shortest_distance;

	
end et_geometry_2a;

