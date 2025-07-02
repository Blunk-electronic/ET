------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               GEOMETRY                                   --
--                                                                          --
--                               B o d y                                    --
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

with ada.text_io;				use ada.text_io;

with ada.strings;				use ada.strings;
with ada.strings.fixed;			use ada.strings.fixed;
with ada.strings.unbounded;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with et_exceptions;						use et_exceptions;


package body et_geometry_2a is
	
	function get_info (editor: in string)
		return string 
	is 
		use ada.characters.latin_1;
		distance_digits_total : constant positive := type_distance'digits;
		distance_digits_right : constant positive := type_distance'scale;

		--distance_coarse_digits_total : constant positive := type_distance_coarse'digits;
		--distance_coarse_digits_right : constant positive := type_distance_coarse'scale;

		rotation_digits_total : constant positive := type_rotation'digits;
		rotation_digits_right : constant positive := type_rotation'scale;
	begin
		--CS put_line ("rounding error:" & pac_geometry_1.to_string (type_float'small));
		-- CS write accuracy
		
		return to_upper (editor & " editor:")
		& lf & "distance fine [mm]"
		& lf & "min:        " & type_distance'image (type_distance'first)
		& lf & "max:        " & type_distance'image (type_distance'last)
		& lf & "axis min:   " & type_distance'image (axis_min)
		& lf & "axis max:   " & type_distance'image (axis_max)
		& lf & "resolution: " & type_distance'image (type_distance'small)
		& lf & "digits"
		& lf & "left:       " & positive'image (distance_digits_total - distance_digits_right)
		& lf & "right:      " & positive'image (distance_digits_right)
		& lf & "total:      " & positive'image (type_distance'digits)
		& lf
		--& lf & "distance coarse [mm]"
		--& lf & "min:        " & type_distance_coarse'image (type_distance_coarse'first)
		--& lf & "max:        " & type_distance_coarse'image (type_distance_coarse'last)
		--& lf & "resolution: " & type_distance_coarse'image (type_distance_coarse'small)
		--& lf & "digits"
		--& lf & "left:       " & positive'image (distance_coarse_digits_total - distance_coarse_digits_right)
		--& lf & "right:      " & positive'image (distance_coarse_digits_right)
		--& lf & "total:      " & positive'image (type_distance_coarse'digits)
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




	function to_string (coordinates : in type_coordinates) return string is begin
		return space & to_lower (type_coordinates'image (coordinates));
	end;

	function to_coordinates (coordinates : in string) return type_coordinates is begin
		return type_coordinates'value (coordinates);

-- 			exception
-- 				when event: others =>
-- 					log (text => ada.exceptions.exception_information (event), console => true);
-- 					raise;
	end;



	
-- DISTANCE:


	function in_range (
		lower, upper	: in type_distance;
		value			: in type_distance)
		return boolean
	is begin
		if value >= lower and value <= upper then
			return true;
		else
			return false;
		end if;
	end in_range;


	

	function get_greatest (
		left, right : in type_distance)
		return type_distance
	is begin
		if left > right then
			return left;
		elsif left < right then
			return right;
		else
			return right;
		end if;
	end get_greatest;

	
	
	function get_smallest (
		left, right : in type_distance)
		return type_distance
	is begin
		if left < right then
			return left;
		elsif left > right then
			return right;
		else
			return right;
		end if;
	end get_smallest;



	
	
	procedure limit_to_maximum (
		distance	: in out type_distance;
		maximum		: in type_distance)
	is begin
		if distance > maximum then
			distance := maximum;
		end if;
	end limit_to_maximum;

	
	
	procedure limit_to_minimum (
		distance	: in out type_distance;
		minimum		: in type_distance)
	is begin
		if distance < minimum then
			distance := minimum;
		end if;
	end limit_to_minimum;

	
	
	
	function mil_to_distance (
		mil : in string) 
		return type_distance 
	is begin
		return type_distance (pac_geometry_1.mil_to_distance (mil));
		-- CS use to_distance instead of type_distance ?
	end mil_to_distance;
	

	function distance_to_mil (
		distance : in type_distance) 
		return string 
	is begin
		return pac_geometry_1.distance_to_mil (type_float (distance));
	end;



	function get_greatest (
		distances	: in pac_distances_positive.list)
		return type_distance_positive
	is
		ds : pac_distances_positive.list := distances;
		use pac_distances_positive_sorting;
	begin
		sort (ds);
		return ds.last_element;
	end get_greatest;



	
	function to_string (
		distance : in type_distance)
		return string
	is begin
		return type_distance'image (distance);
	end to_string;


	function to_distance (f : in type_float)
		return type_distance 
	is
		use pac_float_numbers_io;
		
		d1 : type_distance;
		d2 : type_float;

		f1 : constant type_float := 5.0 * type_float (type_distance'small);
		-- CS should be a package wide constant ?
	begin
		d1 := type_distance (f);
		
		d2 := 10.0 * abs (f - type_float (d1));
		
		if f < 0.0 then
			if d2 > f1 then
				d1 := d1 - type_distance'small;
			end if;
		else
			if d2 > f1 then
				d1 := d1 + type_distance'small;
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
		return type_distance
	is begin
		return type_distance'value (dd);

		exception when event: others =>
			raise syntax_error_2 with 
				"ERROR: Expect a distance instead of " 
				& enclose_in_quotes (dd) & " !";
	end to_distance;


	

	function clip_distance (d : in type_distance)
		return type_position_axis
	is begin
		if d > axis_max then return axis_max;
		elsif d < axis_min then return axis_min;
		else return d;
		end if;
	end clip_distance;

	
	procedure clip_distance (d : in out type_distance) is begin
		if d > axis_max then d := axis_max;
		elsif d < axis_min then d := axis_min;
		end if;
	end clip_distance;


	
	
-- ROTATION / ANGLE

	
	function to_string (
		rotation : in type_rotation) 
		return string 
	is begin
		if rotation < zero_rotation then
			return space & type_rotation'image (rotation);
		else
			return type_rotation'image (rotation);
		end if;
		-- CS suppress trailing zeros
	end;


	
	function to_rotation (
		rotation : in string)
		return type_rotation 
	is begin
		return type_rotation'value (rotation);
	end;



	
	function to_rotation (f : in type_float)
		return type_rotation 
	is
		use pac_float_numbers_io;

		d1 : type_rotation := type_rotation (f);
		d2 : type_float;

		f1 : constant type_float := 5.0 * type_float (type_rotation'small);

	begin
		d2 := 10.0 * abs (f - type_float (d1));
		
		if f < 0.0 then
			if d2 > f1 then
				d1 := d1 - type_rotation'small;
			end if;
		else
			if d2 > f1 then
				d1 := d1 + type_rotation'small;
			end if;
		end if;

		return d1;
		
		--if f < 0.0 then
			--declare
				--r : string (1 .. type_rotation'digits + 2); -- sign + point
			--begin
				---- CS: IMPROVEMENT REQUIRED !!!
				--put (to => r, item => f, aft => type_rotation'scale, exp => 0);
				--return type_rotation'value (r);
			--end;
		--else
			--declare
				--r : string (1 .. type_rotation'digits + 1); -- point
			--begin
				---- CS: IMPROVEMENT REQUIRED !!!
				--put (to => r, item => f, aft => type_rotation'scale, exp => 0);
				--return type_rotation'value (r);
			--end;
		--end if;
	end to_rotation;



	function to_angle (
		a : in type_rotation)
		return type_float
	is begin
		return type_float (a);
	end to_angle;

	

	function add (
		left, right : in type_rotation)
		return type_rotation 
	is
		subtype type_rotation_wide is type_float range -720.0 .. +720.0;
		scratch : type_rotation_wide;
		result : type_rotation; -- to be returned
	begin
		scratch := type_float (left) + type_float (right);
		
		if scratch >= 360.0 then
			scratch := scratch - 360.0;
			
		elsif scratch <= -360.0 then
			scratch := scratch + 360.0;
		end if;

		result := to_rotation (scratch);
		return result;
	end;




	procedure add (
		rotation	: in out type_rotation;
		offset		: in type_rotation)
	is begin
		rotation := add (rotation, offset);
	end add;



	
	


	
-- POINT / POSITION / LOCATION / LOCATION VECTOR / DISTANCE VECTOR:
	
	
	function to_string (
		v 		: in type_vector_model;
		format	: in type_output_format := FORMAT_1)
		return string
	is begin
		case format is
			when FORMAT_1 =>
				return "x/y: " & to_string (v.x) & "/" & to_string (v.y);

			when FORMAT_2 =>
				return "x " & to_string (v.x) & " y " & to_string (v.y);

			when FORMAT_3 =>
				return to_string (v.x) & " " & to_string (v.y);
				
			when others => -- do the same as with FORMAT_1
				return "x/y: " & to_string (v.x) & "/" & to_string (v.y);
		end case;
	end to_string;



	function to_vector_model (
		x, y : in type_distance)
		return type_vector_model
	is begin
		return (x, y);
	end to_vector_model;


	

	procedure reset (
		point : in out type_vector_model) 
	is begin
		point.x := zero;
		point.y := zero;
	end;
	
	

	function invert (
		point	: in type_vector_model)
		return type_vector_model
	is begin
		return (- point.x, - point.y);
	end invert;

	

	function invert (
		point	: in type_vector_model;
		axis	: in type_mirror)
		return type_vector_model
	is
		p : type_vector_model := point;
	begin
		case axis is
			when MIRROR_ALONG_X_AXIS => p.x := - p.x;
			when MIRROR_ALONG_Y_AXIS => p.y := - p.y;
			when MIRROR_NO => null;
		end case;

		return p;
	end invert;


	-- function add (
	-- 	v1, v2 : in type_vector_model)
	-- 	return type_vector_model
	-- is 
	-- 	r : type_vector_model;
	-- begin
	-- 	r.x := v1.x + v2.x;
	-- 	r.y := v1.y + v2.y;
	-- 	return r;
	-- end add;


	function "+" (
		left, right : in type_vector_model)
		return type_vector_model
	is
		r : type_vector_model;
	begin
		r.x := left.x + right.x;
		r.y := left.y + right.y;
		return r;
	end;


	

	procedure add (
		v1 : in out type_vector_model;
		v2 : in type_vector_model)
	is begin
		v1.x := v1.x + v2.x;
		v1.y := v1.y + v2.y;
	end add;



	

	function "-" (
		left, right : in type_vector_model)
		return type_vector_model
	is
		r : type_vector_model;
	begin
		r.x := left.x - right.x;
		r.y := left.y - right.y;
		return r;
	end;


	
	
	procedure move_by (
		point	: in out type_vector_model;
		offset	: in type_vector_model)
	is begin
		point.x := point.x + offset.x;
		point.y := point.y + offset.y;
	end move_by;



	procedure rotate_by (
		point		: in out type_vector_model;
		rotation	: in type_rotation) 
	is			
		v_tmp : type_vector := to_vector (point);
	begin
		rotate_by (
			vector		=> v_tmp,
			rotation	=> type_angle (rotation),
			debug		=> false);

		point := to_vector_model (v_tmp);
	end rotate_by;
	


	procedure rotate_to (
		point		: in out type_vector_model;
		rotation	: in type_rotation) -- degrees
	is
		distance_to_origin	: type_float; -- unit is mm
		scratch				: type_float;
	begin
		-- compute distance of given point to origin
		if get_x (point) = zero and get_y (point) = zero then
			distance_to_origin := 0.0;
			
		elsif get_x (point) = zero then
			distance_to_origin := type_float (abs (get_y (point)));
			
		elsif get_y (point) = zero then
			distance_to_origin := type_float (abs (get_x (point)));
			
		else
			distance_to_origin := sqrt (
				type_float (abs (get_x (point))) ** 2.0 
				+
				type_float (abs (get_y (point))) ** 2.0
				);
		end if;

		-- The new angle is the given rotation.

		-- compute new x   -- (cos rotation) * distance_to_origin
		scratch := cos (type_float (rotation), units_per_cycle);
		set (
			axis	=> AXIS_X,
			point	=> point,
			value	=> to_distance (scratch * distance_to_origin)
			);

		-- compute new y   -- (sin rotation) * distance_to_origin
		scratch := sin (type_float (rotation), units_per_cycle);
		set (
			axis 	=> AXIS_Y,
			point	=> point,
			value	=> to_distance (scratch * distance_to_origin)
			);
		
	end rotate_to;


	
	
	
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
		return type_distance_positive
	is
		use pac_float_numbers_functions;

		dx : type_float := abs (type_float (p2.x - p1.x));
		dy : type_float := abs (type_float (p2.y - p1.y));
		d : type_float;
	begin
		d := sqrt (dx**2.0 + dy**2.0);
		return type_distance_positive (d);
	end get_distance;
	



	function get_distance (
		point_1	: in type_vector_model;
		point_2	: in type_vector_model;
		axis	: in type_axis_2d) 
		return type_distance 
	is
		d : type_distance;
	begin
		case axis is
			when AXIS_X =>
				d := (point_2.x - point_1.x);

			when AXIS_Y =>
				d := (point_2.y - point_1.y);
		end case;

		return d;
	end get_distance;

	
	
	function get_angle (
		p1, p2 : in type_vector_model)
		return type_rotation
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
		
		return type_rotation (a);

		exception
			when ADA.NUMERICS.ARGUMENT_ERROR => 
				put_line ("tangens error");
				raise;
		
	end get_angle;



	function get_rotation (
		point : in type_vector_model) 
		return type_rotation 
	is begin
		return to_rotation (get_angle (get_distance (null_vector, to_vector (point))));
	end get_rotation;

	
	

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
			when AXIS_X => point.x := value;
			when AXIS_Y => point.y := value;
		end case;
	end;



	procedure set (
		point	: in out type_vector_model;
		position: in type_vector_model) 
	is begin
		point.x := position.x;
		point.y := position.y;
	end;
	

	
	function to_vector (
		point	: in type_vector_model)
		return type_vector 
	is 
		f : type_float := type_float (get_x (point));
	begin
		return set (
			x => type_float (get_x (point)),
			y => type_float (get_y (point)),
			z => 0.0
			);
	end to_vector;


	

	function to_vector_model (
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

	end to_vector_model;




	function to_vector_model (
		d 		: in type_vector_model;
		clip	: in boolean := false)
		return type_vector_model
	is 
		p : type_vector_model;
	begin
		if clip then
			p.x := clip_distance (d.x);
			p.y := clip_distance (d.y);				
		else
			p.x := d.x;
			p.y := d.y;
		end if;
		
		return p;

		exception
			when constraint_error =>
				log (text => "distance too great: x/y" 
					& to_string (d.x)
					& "/"
					& to_string (d.y));
				raise;
		
	end to_vector_model;
	
	

	function to_vector_model (
		x,y : in string)
		return type_vector_model
	is 
		result : type_vector_model;					
	begin
		result.x := to_distance (dd => x);
		result.y := to_distance (dd => y);
		return result;

		-- CS exception handler
	end to_vector_model;


	

	

	function to_offset (
		p : in type_vector_model)
		return type_offset
	is begin
		return (type_float (p.x), type_float (p.y));
	end to_offset;



	function to_offset (
		x, y : in type_distance)
		return type_offset
	is begin
		return (type_float (x), type_float (y));
	end to_offset;



	
	
	function get_distance_absolute (
		point	: in type_vector_model;
		vector	: in type_vector)
		return type_float_positive
	is begin
		return get_distance_total (to_vector (point), vector);
	end get_distance_absolute;
	


	function get_distance_relative (
		point_one, point_two : in type_vector_model)
		return type_vector_model
	is
		d : type_vector_model;
	begin
		d.x := point_two.x - point_one.x;
		d.y := point_two.y - point_one.y;
		return d;
	end get_distance_relative;



	
	function get_distance_absolute (
		point_one, point_two : in type_vector_model) 
		return type_float_positive 
	is begin
		return get_distance_total (to_vector (point_one), to_vector (point_two));
	end get_distance_absolute;



	function get_distance_absolute (
		point_1	: in type_vector_model;
		point_2	: in type_vector_model;
		axis	: in type_axis_2d) 
		return type_distance_positive
	is
		d : type_distance_positive;
	begin
		case axis is
			when AXIS_X =>
				d := abs (point_2.x - point_1.x);

			when AXIS_Y =>
				d := abs (point_2.y - point_1.y);
		end case;
				
		return d;
	end get_distance_absolute;


	

	
	
	procedure move_to (
		point		: in out type_vector_model;
		destination	: in type_vector_model) 
	is begin
		point.x := destination.x;
		point.y := destination.y;
	end move_to;



	function move (
		point		: in type_vector_model;
		direction	: in type_rotation;
		distance	: in type_distance_positive;
		clip		: in boolean := false)
		return type_vector_model 
	is 			
		v_tmp : type_vector;
		rx, ry : type_distance;
		result : type_vector_model;			
	begin
		v_tmp := move_by (
			v			=> to_vector (point),
			direction	=> type_angle (direction),
			distance	=> type_float_positive (distance));
		
		rx := to_distance (v_tmp.x);
		ry := to_distance (v_tmp.y);
		
		if clip then
			clip_distance (rx);
			clip_distance (ry);
		end if;

		result := (rx, ry);
		return result;
	end move;

	

	procedure mirror_point (
		point	: in out type_vector_model;
		axis	: in type_mirror) 
	is begin
		case axis is
			when MIRROR_ALONG_X_AXIS =>
				point.y := point.y * (-1.0);
				
			when MIRROR_ALONG_Y_AXIS =>
				point.x := point.x * (-1.0);
				
			when MIRROR_NO =>
				null;
		end case;
	end mirror_point;


	

	function "<" (left, right : in type_vector_model) return boolean is begin
		if left.x < right.x then
			return true;
		elsif left.x > right.x then
			return false;

		-- left.x = right.x -> compare y:
		elsif left.y < right.y then
			return true;
		else 
			-- if left.y greater or equal right.y
			return false;
		end if;
		
		-- CS compare absolute distance to origin instead
	end;





	procedure sort_by_distance (
		points		: in out pac_points.list;
		reference	: in type_vector_model;
		mode		: in type_sort_mode := SORT_ASCENDING)
	is
		-- Convert the given points to vectors:
		vectors : pac_vectors.list := to_vectors (points);				
	begin
		-- Sort the vectors by their distance to 
		-- the reference point (in ascending order):
		sort_by_distance (vectors, to_vector (reference));
		
		-- Convert the vectors back to model vectors:
		points := to_points (vectors);
	end sort_by_distance;
		
	


	function get_length (
		points : in pac_points.list)
		return natural
	is
		use pac_points;
	begin
		return natural (points.length);
	end get_length;




	procedure move_points (
		points 	: in out pac_points.list;
		offset	: in type_vector_model)
	is
		use pac_points;

		procedure move (p : in out type_vector_model) is begin
			move_by (p, offset);
		end;
		
		procedure query_point (c : in pac_points.cursor) is begin
			points.update_element (c, move'access);
		end;
	
	begin
		points.iterate (query_point'access); 
	end move_points;


	
	
	procedure rotate_points (
		points 		: in out pac_points.list;
		rotation	: in type_rotation)
	is
		use pac_points;

		procedure rotate (p : in out type_vector_model) is begin
			rotate_by (p, rotation);
		end;
		
		procedure query_point (c : in pac_points.cursor) is begin
			points.update_element (c, rotate'access);
		end;
	
	begin
		points.iterate (query_point'access); 
	end rotate_points;


	

	procedure mirror_points (
		points 	: in out pac_points.list;
		mirror	: in type_mirror)
	is
		use pac_points;

		procedure mirror_point (p : in out type_vector_model) is begin
			mirror_point (p, mirror);
		end;
		
		procedure query_point (c : in pac_points.cursor) is begin
			points.update_element (c, mirror_point'access);
		end;
	
	begin
		points.iterate (query_point'access); 
	end mirror_points;


	
	

	function get_nearest (
		points		: in pac_points.list;
		reference	: in type_vector_model := origin)
		return type_vector_model
	is
		use pac_points;
		
		result : type_vector_model;

		distance : type_float_positive := type_float_positive'last;
		
		procedure query_point (p : in pac_points.cursor) is
			d_scratch : constant type_float_positive := 
				get_absolute (get_distance (reference, element (p)));
		begin
			-- put_line (to_string (element (p)));
					  
			if d_scratch < distance then
				distance := d_scratch;
				result := element (p);
			end if;
		end query_point;
		
	begin
		-- put_line ("get_nearest");
		
		points.iterate (query_point'access);
		return result;
	end get_nearest;




	function to_vectors (
		points : in pac_points.list)
		return pac_vectors.list
	is
		result : pac_vectors.list;

		use pac_points;
		
		procedure query_point (c : in pac_points.cursor) is begin
			result.append (to_vector (element (c)));
		end query_point;
		
	begin
		points.iterate (query_point'access);
		return result;
	end to_vectors;




	
	function to_points (
		vectors : in pac_vectors.list)
		return pac_points.list
	is
		result : pac_points.list;

		use pac_vectors;
		
		procedure query_vector (c : in pac_vectors.cursor) is begin
			result.append (to_vector_model (element (c)));
		end;
		
	begin
		vectors.iterate (query_vector'access);
		return result;
	end to_points;



	

	procedure remove_redundant_points (
		points : in out pac_points.list)
	is
		use pac_points;
		target : pac_points.list;

		procedure query_point (p : in pac_points.cursor) is begin
			if not target.contains (element (p)) then
				target.append (element (p));
			end if;
		end query_point;
		
	begin
		points.iterate (query_point'access);
		points := target;
	end remove_redundant_points;


	
	
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


	procedure swap_edges (
		area : in out type_area)
	is
		w : type_distance_positive := area.width;
	begin
		area.width  := area.height;
		area.height := w;
	end swap_edges;
	
	
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


	function get_diagonal (
		area : type_area)
		return type_distance_positive
	is 
		d : type_distance_positive;
		w : type_float_positive := type_float_positive (area.width);
		h : type_float_positive := type_float_positive (area.height);
	begin
		d := type_distance_positive (sqrt (w ** 2.0 + h ** 2.0));		
		return d;
	end get_diagonal;


	
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
		A_lx : type_distance renames A.position.x;

		-- This is the greatest x used by area A
		A_gx : constant type_distance := A_lx + A.width;

		
		-- This is the lowest y used by area A
		A_ly : type_distance renames A.position.y;

		-- This is the greatest y used by area A
		A_gy : constant type_distance := A_ly + A.height;


		-- AREA B:
		-- This is the lowest x used by area B
		B_lx : type_distance renames B.position.x;

		-- This is the greatest x used by area B
		B_gx : constant type_distance := B_lx + B.width;

		
		-- This is the lowest y used by area B
		B_ly : type_distance renames B.position.y;

		-- This is the greatest y used by area B
		B_gy : constant type_distance := B_ly + B.height;

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
		A_lx : type_distance renames A.position.x;

		-- This is the greatest x used by area A
		A_gx : type_distance := A_lx + A.width;

		
		-- This is the lowest y used by area A
		A_ly : type_distance renames A.position.y;

		-- This is the greatest y used by area A
		A_gy : type_distance := A_ly + A.height;


		-- AREA B:
		-- This is the lowest x used by area B
		B_lx : type_distance renames B.position.x;

		-- This is the greatest x used by area B
		B_gx : type_distance := B_lx + B.width;

		
		-- This is the lowest y used by area B
		B_ly : type_distance renames B.position.y;

		-- This is the greatest y used by area B
		B_gy : type_distance := B_ly + B.height;

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



	
	function get_opposide_end (
		AB_end	: in type_start_end_point)
		return type_start_end_point
	is begin
		case AB_end is 
			when A => return B;
			when B => return A;
		end case;
	end;



	

	function to_string (
		point : in type_start_end_point)
		return string
	is begin
		return type_start_end_point'image (point);
	end to_string;


	
	function to_start_end_point (
		point : in string)
		return type_start_end_point
	is begin
		return type_start_end_point'value (point);
end;


	

-- LINE:	

	procedure reset_line (
		line : in out type_line)
	is begin
		line := (others => <>);
	end;
	



	function to_line (
		A, B : in type_vector_model)
		return type_line'class
	is 
		l : type_line;
	begin
		l.A := A;
		l.B := B;
		return l;
	end;
	
	

	
	function to_string (
		line : in type_line) 
		return string 
	is begin
		return "A: " & to_string (line.A) 
			& " B: " & to_string (line.B);
	end;



	function get_end_point (
		line 	: in type_line;
		AB_end	: in type_start_end_point)
		return type_vector_model
	is begin
		case AB_end is
			when A => return line.A;
			when B => return line.B;
		end case;
	end;



	

	function get_NSWE_end (
		line	: in type_line;
		side	: in type_direction_NSWE)
		return type_start_end_point
	is
		result : type_start_end_point := A;

		orientation : type_line_orientation;

		
		procedure test_x is begin
			case side is
				when DIR_WEST =>
					if line.A.x < line.B.x then -- A is west of B
						result := A;
					else
						result := B;
					end if;

				when DIR_EAST =>
					if line.A.x > line.B.x then -- A is east of B
						result := A;
					else
						result := B;
					end if;

				when others =>
					raise constraint_error;
			end case;
		end test_x;


		procedure test_y is begin
			case side is
				when DIR_NORTH =>
					if line.A.y > line.B.y then -- A is north of B
						result := A;
					else
						result := B;
					end if;

				when DIR_SOUTH =>
					if line.A.y < line.B.y then -- A is south of B
						result := A;
					else
						result := B;
					end if;

				when others =>
					raise constraint_error;
			end case;
		end test_y;


		procedure test_xy is begin
			case side is
				when DIR_WEST =>
					if line.A.x < line.B.x then -- A is west of B
						result := A;
					else
						result := B;
					end if;

				when DIR_EAST =>
					if line.A.x > line.B.x then -- A is east of B
						result := A;
					else
						result := B;
					end if;

				when DIR_NORTH =>
					if line.A.y > line.B.y then -- A is north of B
						result := A;
					else
						result := B;
					end if;

				when DIR_SOUTH =>
					if line.A.y < line.B.y then -- A is south of B
						result := A;
					else
						result := B;
					end if;

			end case;
		end test_xy;

		
		
	begin
		orientation := get_orientation (line);

		case orientation is
			when ORIENT_HORIZONTAL =>
				test_x;
				
			when ORIENT_VERTICAL =>
				test_y;

			when ORIENT_SLOPING =>
				test_xy;
		end case;
		
		return result;
	end get_NSWE_end;

	


	

	function get_A (
		line : in type_line)
		return type_vector_model
	is begin
		return line.A;
	end;

	

	function get_B (
		line : in type_line)
		return type_vector_model
	is begin
		return line.B;
	end;



	function get_center (
		line : in type_line)
		return type_vector_model
	is 
		c : type_vector;
		l : type_line_fine := to_line_fine (line);
	begin
		c := get_center (l);		
		return to_vector_model (c);
	end get_center;


	

	procedure set_A (
		line	: in out type_line;
		A		: in type_vector_model)
	is begin
		line.A := A;
	end;


	procedure set_B (
		line	: in out type_line;
		B		: in type_vector_model)
	is begin
		line.B := B;
	end;



	procedure move_A_by (
		line	: in out type_line;
		offset	: in type_vector_model)
	is begin
		move_by (line.A, offset);
	end;
	

	
	procedure move_B_by (
		line	: in out type_line;
		offset	: in type_vector_model)
	is begin
		move_by (line.B, offset);
	end;

	

	
	function is_selected (
		line : in type_line)
		return boolean
	is begin
		if is_selected (line.status) then
			return true;
		else
			return false;
		end if;
	end is_selected;

	
	procedure set_selected (
		line : in out type_line)
	is begin
		set_selected (line.status);
	end set_selected;

	
	procedure clear_selected (
		line : in out type_line)
	is begin
		clear_selected (line.status);
	end clear_selected;

	
	

	function is_proposed (
		line : in type_line)
		return boolean
	is begin
		if is_proposed (line.status) then
			return true;
		else 
			return false;
		end if;
	end is_proposed;

	
	procedure set_proposed (
		line : in out type_line)
	is begin
		set_proposed (line.status);
	end set_proposed;

	
	procedure clear_proposed (
		line : in out type_line)
	is begin
		clear_proposed (line.status);
	end clear_proposed;


	


	function is_moving (
		line : in type_line)
		return boolean
	is begin
		if is_moving (line.status) then
			return true;
		else 
			return false;
		end if;
	end is_moving;

	
	procedure set_moving (
		line : in out type_line)
	is begin
		set_moving (line.status);
	end set_moving;

	
	procedure clear_moving (
		line : in out type_line)
	is begin
		clear_moving (line.status);
	end clear_moving;



	procedure modify_status (
		line 		: in out type_line;
		operation	: in type_status_operation)						
	is begin
		modify_status (line.status, operation);
	end modify_status;




	function is_A_moving (
		line : in type_line)
		return boolean
	is begin
		return is_A_moving (line.status_AB);
	end;

	
	function is_B_moving (
		line : in type_line)
		return boolean
	is begin
		return is_B_moving (line.status_AB);
	end;



	

	procedure set_A_moving (
		line	: in out type_line)
	is begin
		set_A_moving (line.status_AB);
	end;
	

	procedure set_B_moving (
		line	: in out type_line)
	is begin
		set_B_moving (line.status_AB);
	end;


	
	
	procedure reset_status (
		line 	: in out type_line)
	is begin
		reset_status (line.status);

		clear_AB_moving (line.status_AB);
	end reset_status;

	
	
	procedure move_by (
		line	: in out type_line;
		offset	: in type_vector_model)
	is begin
		move_by (point	=> line.A,	offset => offset);
		move_by (point	=> line.B,	offset => offset);
	end move_by;



	procedure move_start_by (
		line	: in out type_line;
		offset	: in type_vector_model)
	is begin
		move_by (line.A, offset);
	end move_start_by;


	procedure move_end_by (
		line	: in out type_line;
		offset	: in type_vector_model)
	is begin
		move_by (line.B, offset);
	end move_end_by;




	

	procedure mirror_line (
		line : in out type_line;
		axis : in type_mirror)
	is begin
		mirror_point (line.A, axis);
		mirror_point (line.B, axis);
	end mirror_line;

	

	procedure rotate_line_by (
		line		: in out type_line;
		rotation	: in type_rotation) 
	is begin
		rotate_by (line.A, rotation);
		rotate_by (line.B, rotation);
	end;

	
	
	function to_line_fine (
		line : in type_line)
		return type_line_fine
	is begin
		return (
			A	=> to_vector (line.A),
			B	=> to_vector (line.B),
			status		=> line.status);
	end to_line_fine;



	function to_line_coarse (
		line : in type_line_fine)
		return type_line'class
	is
		l : type_line;
	begin
		l.A	:= to_vector_model (line.A);
		l.B := to_vector_model (line.B);
		l.status := line.status;
		return l;
	end to_line_coarse;
	


	function get_distance (
		line		: in type_line;
		vector		: in type_vector;
		line_range	: in type_line_range)
		return type_distance_point_line 
	is begin
		return get_distance (
			vector		=> vector,
			line		=> to_line_fine (line),
			line_range	=> line_range);
	end get_distance;


	function get_distance (
		line		: in type_line;
		point		: in type_vector_model; 
		line_range	: in type_line_range)
		return type_distance_point_line
	is begin
		return get_distance (to_vector (point), to_line_fine (line), line_range);
	end get_distance;





	
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
	


	function between_A_and_B (
		line	: in type_line;
		point	: in type_vector_model)
		return boolean
	is
		result : boolean := false;
	begin
		-- If the point sits on either A or B then
		-- nothing will be computed and the default false returned.
		if point /= get_A (line) and point /= get_B (line) then

			-- If the point lies on the the line
			-- then return true. Otherwise the default 
			-- false will be returned:
			if on_line (line, point) then
				result := true;
			end if;
		end if;

		return result;
	end between_A_and_B;

	
	

	function get_start_vector (
		line	: in type_line)
		return type_vector 
	is begin
		return set (
			x => type_float (line.A.x),
			y => type_float (line.A.y),
			z => 0.0);
	end get_start_vector;

	
	function get_end_vector (
		line	: in type_line)
		return type_vector 
	is begin
		return set (
			x => type_float (line.B.x),
			y => type_float (line.B.y),
			z => 0.0);
	end get_end_vector;


	function get_direction_vector (
		line	: in type_line)
		return type_vector 
	is begin
		return set (
			x => type_float (line.B.x - line.A.x),
			y => type_float (line.B.y - line.A.y),
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



	function get_nearest (
		line	: in type_line_fine;
		point	: in type_vector_model)
		return type_vector_model
	is
		result : type_vector_model;
		v : type_vector;
	begin
		v := get_nearest (line, to_vector (point));
		return to_vector_model (v);
	end get_nearest;



	

	function get_bounding_box (
		line	: in type_line;
		width	: in type_distance_positive)
		return type_area
	is
		-- CS: Optimization required. Compiler options ?
		
		result : type_area;
		w, h : type_distance;

		d : constant type_distance := width / 2.0;
	begin
		-- x-axis:
		w := line.B.x - line.A.x;
		
		if w > 0.0 then -- line runs from left to right
			result.position.x := line.A.x;
			result.width := w;
		else -- line runs from right to left or vertically
			result.position.x := line.B.x;
			result.width := -w;
		end if;
		
		-- y-axis:
		h := line.B.y - line.A.y;
		
		if h > 0.0 then -- line runs upwards
			result.position.y := line.A.y;
			result.height := h;
		else -- line runs downwards or horizontally
			result.position.y := line.B.y;
			result.height := -h;
		end if;

		-- extend the box by the linewidth:
		result.width  := result.width  + width;
		result.height := result.height + width;

		-- shift the box by half the linewidth down and left:
		result.position.x := result.position.x - d;
		result.position.y := result.position.y - d;
		return result;
	end get_bounding_box;



	

	function get_intersection (
		line		: in type_line;
		line_vector	: in type_line_vector)
		return type_line_vector_intersection
	is begin
		return get_intersection (
			line_vector	=> line_vector,
			line		=> to_line_fine (line));		
	end get_intersection;

	



	function get_orientation (
		line : in type_line) 
		return type_line_orientation 
	is		
		result : type_line_orientation;
		
		dx : constant type_distance := 
			get_x (get_A (line)) - get_x (get_B (line));
		
		dy : constant type_distance := 
			get_y (get_A (line)) - get_y (get_B (line));
		
	begin
		if dx = zero then 
			result := ORIENT_VERTICAL;
		
		elsif dy = zero then
			result := ORIENT_HORIZONTAL;
			
		else 
			result := ORIENT_SLOPING;
		end if;

		return result;
	end get_orientation;

	


	


	function lines_overlap (
		line_1, line_2 : in type_line)
		return boolean
	is
		result : boolean := false;

		O1 : constant type_line_orientation := get_orientation (line_1);
		O2 : constant type_line_orientation := get_orientation (line_2);

		-- x and y component of A and B end of line 1:
		L1_Ax : type_distance renames line_1.A.x;
		L1_Ay : type_distance renames line_1.A.y;
		
		L1_Bx : type_distance renames line_1.B.x;
		L1_By : type_distance renames line_1.B.y;
		
		-- x and y component of A and B end of line 2:
		L2_Ax : type_distance renames line_2.A.x;
		L2_Ay : type_distance renames line_2.A.y;
		
		L2_Bx : type_distance renames line_2.B.x;
		L2_By : type_distance renames line_2.B.y;

		
	begin
		if O1 = ORIENT_SLOPING or O2 = ORIENT_SLOPING then
			return false;
		end if;

		if O1 = O2 then
			case O1 is
				when ORIENT_HORIZONTAL =>
					-- The y values must be equal:
					if L1_Ay = L2_Ay then

						-- CASE H1: Line 1 runs from left to right:
						if in_range (lower => L1_Ax, upper => L1_Bx, value => L2_Ax)
						or in_range (lower => L1_Ax, upper => L1_Bx, value => L2_Bx)

						-- CASE H2: Line 1 runs from right to left:					
						or in_range (lower => L1_Bx, upper => L1_Ax, value => L2_Ax)
						or in_range (lower => L1_Bx, upper => L1_Ax, value => L2_Bx)

						then
							result := true;
						end if;
					end if;

					
				when ORIENT_VERTICAL =>
					-- The x values must be equal:
					if L1_Ax = L2_Ax then

						-- CASE V1: Line 1 runs from down to up:
						if in_range (lower => L1_Ay, upper => L1_By, value => L2_Ay)
						or in_range (lower => L1_Ay, upper => L1_By, value => L2_By)

						-- CASE V2: Line 1 runs from up to down:					
						or in_range (lower => L1_By, upper => L1_Ay, value => L2_Ay)
						or in_range (lower => L1_By, upper => L1_Ay, value => L2_By)

						then
							result := true;
						end if;
					end if;
					

				when ORIENT_SLOPING =>
					raise constraint_error; -- CS should never happen
					
			end case;
		end if;
		
		return result;
	end lines_overlap;


	


	function split_line (
		line 	: in type_line;
		point	: in type_vector_model)
		return type_split_line
	is begin
		-- If the given point eqals either the
		-- start or the end point of the given line,
		-- then there is nothing to split.
		-- The return will be a single line which is
		-- the given line without any modification:
		if line.A = point or line.B = point then
			declare
				result : type_split_line (count => 1);
			begin
				result.segments (1) := line;
				return result;
			end;
		else
			
		-- If the given point is neither A or B of the
		-- given line, then the given line is broken down
		-- in two lines:
			declare
				result : type_split_line (count => 2);
			begin
				result.segments (1).A := line.A;
				result.segments (1).B := point;

				result.segments (2).A := point;
				result.segments (2).B := line.B;

				return result;
			end;
		end if;
	end split_line;


	


	function split_line (
		line 	: in type_line;
		points	: in pac_points.list)
		return type_split_line
	is
		debug : boolean := false;
		
		fragment_count : natural;

		-- In case the given list of points is
		-- empty, then nothing is to do and this
		-- will be returned:
		result_no_split : type_split_line (count => 1);

		
		function do_it return type_split_line is
			result : type_split_line (count => fragment_count);
			points_sorted : pac_points.list := points;

			use pac_points;

			-- This index points to the fragment being built:
			i : natural := 1;
			
			procedure query_point (c : in pac_points.cursor) is
				p : type_vector_model renames element (c);
				l : type_line;
			begin
				if debug then
					put_line ("i:" & natural'image (i));
				end if;
				
				-- CS if p = get_A (line) or p = get_B (line) then
				-- 	null; -- skip

				if i = 1 then
					-- Build the first line. It starts where
					-- the given line starts. It ends at p:
					set_A (l, get_A (line));
					set_B (l, p);

				else
					-- Build an intermediate line. It starts where
					-- its predecessor ends. It ends at p:
					set_A (l, get_B (result.segments (i - 1)));
					set_B (l, p);
				end if;

				-- Assign the temporaily line to the result:
				result.segments (i) := l;

				i := i + 1; -- prepare next segment
			end query_point;

			
		begin
			-- Sort the given points by their increasing distance
			-- to the A end of the line:
			sort_by_distance (points => points_sorted, reference => get_A (line));

			-- The last point is "artificially" in order
			-- to compute the last segment:
			points_sorted.append (get_B (line));

			-- Iterate the points and build a line fragment:
			points_sorted.iterate (query_point'access);
			return result;
		end do_it;
		
			
	begin
		-- We will always have one more fragment than split points:
		fragment_count := 1 + natural (points.length);

		case fragment_count is
			when 0 => raise constraint_error;
			when 1 =>
				-- If the outcome will be just a single segment,
				-- then the given line will be returned as it is:
				result_no_split.segments (1) := line;
				return result_no_split;
				
			when others =>
				return do_it;
		end case;
		
	end split_line;
	



	

	procedure merge_lines (
		primary			: in out type_line;
		primary_end		: in type_start_end_point;
		secondary		: in type_line;
		secondary_end	: in type_start_end_point)
	is begin
		case primary_end is
			when A =>
				-- Modify the A end of the primary line:
				case secondary_end is
					when A => primary.A := secondary.B;
					when B => primary.A := secondary.A;
				end case;

			when B =>
				-- Modify the B end of the primary line:
				case secondary_end is
					when A => primary.B := secondary.B;
					when B => primary.B := secondary.A;
				end case;
		end case;
	end merge_lines;

	



	

	function merge_lines (
		primary			: in type_line;
		secondary		: in type_line)
		return type_line'class
	is
		result : type_line;

		-- Get the orientation of the two segments.
		OP : constant type_line_orientation := get_orientation (primary);
		OS : constant type_line_orientation := get_orientation (secondary);

		-- The two given lines may run into various 
		-- directions (upwards, downwards, right, left).
		-- We need their smallest and greatest extension along
		-- the x and y axis. So the function get_bounding_box seems
		-- appropiate for this purpose. Since we are dealing with lines
		-- that do not have linewidth, the width is set to zero:
		AP : type_area := get_bounding_box (primary,   0.0);
		AS : type_area := get_bounding_box (secondary, 0.0);

	begin
		-- If any of the given lines is a slope then
		-- raise an exception:
		if OP = ORIENT_SLOPING or OS = ORIENT_SLOPING then
			raise constraint_error;
		end if;

		-- If the orientation of the lines differs, then
		-- raise an exception:
		if OP /= OS then
			raise constraint_error;
		end if;

		
		-- Merge both bounding boxes. The outcome is stored in AP:
		merge_areas (AP, AS);

		-- Now, depending on the orientation we set the resulting line.
		-- Since both lines have the same orientation it is sufficient
		-- to just test the primary line:
		case OP is
			when ORIENT_HORIZONTAL =>
				-- X-values are relevant.
				-- The resulting line runs from left to right:
				result.A.x := AP.position.x;
				result.B.x := AP.position.x + AP.width;

				-- Y-value remains is equal on both ends:
				result.A.y := AP.position.y;
				result.B.y := AP.position.y;

				
			when ORIENT_VERTICAL =>
				-- Y-values are relevant.
				-- The resulting line runs from bottom to top:
				result.A.y := AP.position.y;
				result.B.y := AP.position.y + AP.height;

				-- X-value remains is equal on both ends:
				result.A.x := AP.position.x;
				result.B.x := AP.position.x;

  
			when ORIENT_SLOPING =>
				raise constraint_error; -- CS should never happen
		end case;
		

		return result;
	end merge_lines;
	

	
	
	
	
-- ARC:


	function to_arc (
		center		: in type_vector_model;
		A	: in type_vector_model;			
		B	: in type_vector_model;
		direction	: in type_direction_of_rotation)
		return type_arc'class
	is 
		arc : type_arc;
	begin
		arc := (center, A, B, direction, others => <>);

		-- CS consistence check !!
		return arc;
	end to_arc;
	

	
	procedure reset_arc (
		arc		: in out type_arc)
	is begin
		arc := (others => <>);
	end reset_arc;


	
	procedure set_center (
		arc		: in out type_arc;
		center	: in type_vector_model)
	is begin
		arc.center := center;
	end set_center;




	

	procedure set_A (
		arc			: in out type_arc;
		A	: in type_vector_model)
	is begin
		arc.A := A;
	end set_A;

	
	
	procedure set_B (
		arc			: in out type_arc;
		B	: in type_vector_model)
	is begin
		arc.B := B;
	end set_B;

	

	procedure set_direction (
		arc			: in out type_arc;
		direction	: in type_direction_of_rotation)
	is begin
		arc.direction := direction;
	end set_direction;


	

	function get_center (
		arc : in type_arc)
		return type_vector_model
	is begin
		return arc.center;
	end get_center;
	

	
	function get_A (
		arc : in type_arc)
		return type_vector_model
	is begin
		return arc.A;
	end get_A;

	

	function get_B (
		arc : in type_arc)
		return type_vector_model
	is begin
		return arc.B;
	end get_B;



	function get_direction (
		arc : in type_arc)
		return type_direction_of_rotation
	is begin
		return arc.direction;
	end get_direction;



	
	function to_arc_fine (
		arc : in type_arc)
		return pac_geometry_1.type_arc_fine
	is begin
		return to_arc_fine (
			center		=> to_vector (arc.center),
			A	=> to_vector (arc.A),
			B	=> to_vector (arc.B),
			direction	=> arc.direction);
	end to_arc_fine;

	

	function to_arc_coarse (
		arc : in pac_geometry_1.type_arc_fine)
		return type_arc'class
	is 
		result : type_arc;
	begin
		result := (
			center		=> to_vector_model (get_center (arc)),
			A			=> to_vector_model (get_A (arc)),
			B			=> to_vector_model (get_B (arc)),
			direction	=> get_direction (arc),
			others 		=> <>);

		return result;
	end to_arc_coarse;


	
	function to_string (
		arc : in type_arc) 
		return string
	is begin
		return "A: " & to_string (arc.A) 
			& " B: " & to_string (arc.B)
			& " C: " & to_string (arc.center) 
			& " D: " & to_string (arc.direction);
	end to_string;



	function get_intersection (
		arc		: in type_arc;
		line	: in type_line_vector)
		return type_intersection_of_line_and_circle
	is
		A : type_arc_fine := to_arc_fine (arc);
	begin
		return get_intersection (A, line);
	end get_intersection;


	

	function is_selected (
		arc : in type_arc)
		return boolean
	is begin
		if is_selected (arc.status) then
			return true;
		else
			return false;
		end if;
	end is_selected;

	
	
	procedure set_selected (
		arc : in out type_arc)
	is begin
		set_selected (arc.status);
	end set_selected;

	
	procedure clear_selected (
		arc : in out type_arc)
	is begin
		clear_selected (arc.status);
	end clear_selected;

	
	

	function is_proposed (
		arc : in type_arc)
		return boolean
	is begin
		if is_proposed (arc.status) then
			return true;
		else 
			return false;
		end if;
	end is_proposed;

	
	procedure set_proposed (
		arc : in out type_arc)
	is begin
		set_proposed (arc.status);
	end set_proposed;

	
	procedure clear_proposed (
		arc : in out type_arc)
	is begin
		clear_proposed (arc.status);
	end clear_proposed;


	


	function is_moving (
		arc : in type_arc)
		return boolean
	is begin
		if is_moving (arc.status) then
			return true;
		else 
			return false;
		end if;
	end is_moving;

	
	procedure set_moving (
		arc : in out type_arc)
	is begin
		set_moving (arc.status);
	end set_moving;

	
	procedure clear_moving (
		arc : in out type_arc)
	is begin
		clear_moving (arc.status);
	end clear_moving;



	procedure modify_status (
		arc 		: in out type_arc;
		operation	: in type_status_operation)						
	is begin
		modify_status (arc.status, operation);
	end modify_status;

	

	procedure reset_status (
		arc 		: in out type_arc)
	is begin
		reset_status (arc.status);
	end reset_status;





	

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
		scratch : type_vector_model := arc.A;
	begin
		arc.A := arc.B;
		arc.B := scratch;

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
		if arc.A = arc.B then
			return true;
		else
			return false;
		end if;
	end zero_length;



	
	procedure move_by (
		arc		: in out type_arc;
		offset	: in type_vector_model)
	is begin
		move_by (point => arc.center, offset => offset);
		move_by (point => arc.A, offset => offset);
		move_by (point => arc.B, offset => offset);
	end move_by;



	procedure move_to (
		arc			: in out type_arc;
		position	: in type_vector_model)
	is
		offset : constant type_vector_model :=
			get_distance_relative (arc.center, position);
	begin
		-- move the center of the arc to the given position
		move_to (arc.center, position);

		-- move start and end point of the arc by the computed offset
		move_by (point => arc.A, offset => offset);
		move_by (point => arc.B, offset => offset);
	end move_to;
	



	procedure mirror_arc (
		arc			: in out type_arc;
		axis		: in type_mirror)
	is begin
		mirror_point (arc.center, axis);
		mirror_point (arc.A, axis);
		mirror_point (arc.B, axis);
		arc.direction := reverse_direction (arc.direction);
	end mirror_arc;

	

	procedure rotate_arc_by (
		arc			: in out type_arc;
		rotation	: in type_rotation) 
	is begin
		rotate_by (arc.center, rotation);
		rotate_by (arc.A, rotation);
		rotate_by (arc.B, rotation);
	end;
	

	
	function get_radius_start (
		arc : in type_arc) 
		return type_float_positive 
	is begin
		return get_distance_absolute (arc.center, arc.A);
	end get_radius_start;


	
	function get_radius_end (
		arc : in type_arc)
		return type_float_positive
	is begin
		return get_distance_absolute (arc.center, arc.B);
	end get_radius_end;


	
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
		angle 	: in type_angle) -- unit is degrees
		return type_vector_model
	is						
		arc : type_arc;

		radius : type_float;
		angle_start, angle_end : type_angle; -- CS type_angle_positive ?
		end_x, end_y : type_float;
		
	begin		
		-- build an arc from the information available
		arc := (
			center		=> center,
			A	=> A,
			B	=> origin, -- not determined yet
			direction	=> get_direction (angle),
			others		=> <>);
		
		-- move arc so that its center is at 0/0
		move_to (arc, origin);

		-- calculate the radius of the arc
		radius := type_float (get_distance_absolute (arc.center, arc.A));

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
	begin
		-- move arc_tmp so that its center is at 0/0
		move_to (arc_tmp, origin);

		-- the center is not changed:
		set_center (result, to_vector (arc.center));
		
		-- calculate the radius of the arc
		set_radius (result, get_distance_absolute (
			arc_tmp.center, to_vector (arc_tmp.A)));
		
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
		A : type_arc_fine := to_arc_fine (arc);
		P : type_vector := to_vector (point);
	begin
		return get_shortest_distance (A, P);		
	end get_shortest_distance;




	function get_shortest_distance (
		arc		: in type_arc;
		point	: in type_vector)
		return type_float_positive
	is
		A : type_arc_fine := to_arc_fine (arc);
		P : type_vector renames point;
	begin
		return get_absolute (get_shortest_distance (A, P));
	end get_shortest_distance;

	



	function on_arc (
		arc		: in type_arc;
		point	: in type_vector_model)
		return boolean
	is begin
		return on_arc (to_arc_fine (arc), to_vector (point));
	end on_arc;



	
	function get_bounding_box (
		arc 	: in type_arc;
		width	: in type_distance_positive)
		return type_area
	is
		-- CS: Optimization required. Compiler options ?
		
		result : type_area;

		c : type_circle;
	begin
		c.center := arc.center;
		c.radius := type_distance_positive (get_radius_start (arc));

		result := get_bounding_box (c, width);
		
		return result;
	end get_bounding_box;


	

-- CIRCLE:


	function to_circle (
		center	: in type_vector_model;
		radius	: in type_distance_positive)
		return type_circle'class
	is 
		circle : type_circle;
	begin
		circle := (center, radius, others => <>);
		return circle;
	end to_circle;



	procedure reset_circle (
		c : in out type_circle)
	is begin
		c := (others => <>);
	end reset_circle;

	

	procedure set_center (
		c : in out type_circle;
		e : in type_vector_model)
	is begin
		c.center := e;
	end set_center;

	
	procedure set_radius (
		c : in out type_circle;
		r : in type_distance_positive)
	is begin
		c.radius := r;
	end set_radius;



	function get_center (
		c : in type_circle)
		return type_vector_model
	is begin
		return c.center;
	end get_center;
	

	
	function get_radius (
		c : in type_circle)
		return type_distance_positive
	is begin
		return c.radius;
	end get_radius;

	

	
	function to_circle_fine (
		circle : in type_circle)
		return type_circle_fine
	is
		C : type_circle_fine;
	begin
		set_center (C, to_vector (circle.center));
		set_radius (C, type_float_positive (circle.radius));

		return C;
	end to_circle_fine;


	
	
	function to_string (
		circle : in type_circle) 
		return string 
	is begin
		return "C: " & to_string (circle.center) 
			& " R: " & to_string (circle.radius);
	end to_string;


	

	function get_intersection (
		circle	: in type_circle;
		line	: in type_line_vector)
		return type_intersection_of_line_and_circle
	is
		C : type_circle_fine := to_circle_fine (circle);
	begin
		return get_intersection (C, line);
	end get_intersection;


	

	function is_selected (
		circle : in type_circle)
		return boolean
	is begin
		if is_selected (circle.status) then
			return true;
		else
			return false;
		end if;
	end is_selected;

	
	procedure set_selected (
		circle : in out type_circle)
	is begin
		set_selected (circle.status);
	end set_selected;

	
	procedure clear_selected (
		circle : in out type_circle)
	is begin
		clear_selected (circle.status);
	end clear_selected;

	
	

	function is_proposed (
		circle : in type_circle)
		return boolean
	is begin
		if is_proposed (circle.status) then
			return true;
		else 
			return false;
		end if;
	end is_proposed;

	
	procedure set_proposed (
		circle : in out type_circle)
	is begin
		set_proposed (circle.status);
	end set_proposed;

	
	procedure clear_proposed (
		circle : in out type_circle)
	is begin
		clear_proposed (circle.status);
	end clear_proposed;


	


	function is_moving (
		circle : in type_circle)
		return boolean
	is begin
		if is_moving (circle.status) then
			return true;
		else 
			return false;
		end if;
	end is_moving;

	
	procedure set_moving (
		circle : in out type_circle)
	is begin
		set_moving (circle.status);
	end set_moving;

	
	procedure clear_moving (
		circle : in out type_circle)
	is begin
		clear_moving (circle.status);
	end clear_moving;



	procedure modify_status (
		circle 		: in out type_circle;
		operation	: in type_status_operation)						
	is begin
		modify_status (circle.status, operation);
	end modify_status;

	

	procedure reset_status (
		circle 		: in out type_circle)
	is begin
		reset_status (circle.status);
	end reset_status;




	
	function to_radius (
		r : in string)
		return type_distance_positive
	is begin
		return type_distance_positive'value (r);
	end to_radius;


	function to_radius (
		r : in type_distance_positive)
		return type_float_positive
	is begin
		return type_float_positive (r);
	end to_radius;



	
	
	procedure move_by (
		circle	: in out type_circle;
		offset	: in type_vector_model)
	is begin
		move_by (point	=> circle.center, offset => offset);
	end move_by;



	procedure mirror_circle (
		circle		: in out type_circle;
		axis		: in type_mirror) 
	is begin
		mirror_point (circle.center, axis);
	end mirror_circle;



	procedure rotate_circle_by (
		circle		: in out type_circle;
		rotation	: in type_rotation) 
	is begin
		rotate_by (circle.center, rotation);
	end;





	


	

	function get_shortest_distance (
		circle	: in type_circle;
		point	: in type_vector_model)
		return type_distance_polar
	is
		C : type_circle_fine := to_circle_fine (circle);
		P : type_vector := to_vector (point);
	begin
		return get_shortest_distance (C, P);
	end get_shortest_distance;




	function get_shortest_distance (
		circle	: in type_circle;
		point	: in type_vector)
		return type_float_positive
	is
		C : type_circle_fine := to_circle_fine (circle);
		P : type_vector renames point;
	begin
		return get_absolute (get_shortest_distance (C, P));
	end get_shortest_distance;


	

	
	
	function get_bounding_box (
		circle 	: in type_circle;
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

		result.position.x := circle.center.x - w / 2.0;
		result.position.y := circle.center.y - w / 2.0;
		
		return result;
	end get_bounding_box;





-- POSITION:

	function to_string (
		position : in type_position)
		return string 
	is begin
		return point_preamble_with_rotation
			& to_string (position.place.x)
			& axis_separator
			& to_string (position.place.y)
			& axis_separator
			& to_string (position.rotation);
	end to_string;



	function to_position (
		point		: in type_vector_model;
		rotation	: in type_rotation)
		return type_position'class
	is 
		result : type_position;
	begin
		result.place := point;
		result.rotation := rotation;

		return result;
	end to_position;



	procedure set (
		position	: in out type_position;
		axis 		: in type_axis_2d;
		value		: in type_position_axis)
	is begin
		case axis is
			when AXIS_X => position.place.x := value;
			when AXIS_Y => position.place.y := value;
		end case;
	end set;


	procedure set (
		position	: in out type_position;
		place		: in type_vector_model)
	is begin
		position.place := place;
	end set;
	
	
	procedure set (
		position	: in out type_position;
		rotation	: in type_rotation) 
	is begin
		position.rotation := rotation;
	end;


	

	function get_x (
		position : in type_position)
		return type_distance
	is begin
		return position.place.x;
	end get_x;
	

	function get_y (
		position : in type_position)
		return type_distance
	is begin
		return position.place.y;
	end get_y;


	

	
	function get_rotation (
		position : in type_position)
		return type_rotation 
	is begin
		return position.rotation;
	end;




	procedure rotate_about_itself (
		position	: in out type_position;
		offset		: in type_rotation)
	is begin
		position.rotation := add (position.rotation, offset);
	end;


	

	procedure add (
		position	: in out type_position;
		offset		: in type_position;
		mirror		: in type_mirror := MIRROR_NO)
	is
	begin
		case mirror is
			when MIRROR_NO =>
				rotate_by (position.place, get_rotation (offset));
				
				add (position.place, offset.place);				
				add (position.rotation, offset.rotation);

			when MIRROR_ALONG_Y_AXIS =>
				mirror_point (position.place, MIRROR_ALONG_Y_AXIS);

				rotate_by (position.place, - get_rotation (offset));
				
				add (position.place, offset.place);				
				add (position.rotation, offset.rotation);

			when others =>
				null; -- CS
		end case;
	end add;

	
	

	
-- CATCH ZONE:

	function set_catch_zone (
		center	: in type_vector_model;
		radius	: in type_zone_radius)
		return type_catch_zone
	is begin
		return (center, radius);
	end set_catch_zone;



	function get_center (
		zone	: in type_catch_zone)
		return type_vector_model
	is begin
		return zone.center;
	end get_center;

	

	function get_radius (
		zone	: in type_catch_zone)
		return type_zone_radius
	is begin
		return zone.radius;
	end get_radius;

	
	
	function to_string (
		zone : in type_catch_zone)
		return string
	is begin
		return " catch zone: " & to_string (zone.center)
			& " radius " & pac_geometry_1.to_string (zone.radius);
	end to_string;
	


	function to_zone_radius (
		c : in string)
		return type_zone_radius
	is begin
		return pac_geometry_1.to_distance (c);
	end to_zone_radius;


	
	function in_radius (
		distance : in type_float_positive;
		radius	 : in type_zone_radius)
		return boolean
	is begin
		if distance <= radius then
			return true;
		else
			return false;
		end if;
	end in_radius;
	
	



	function in_catch_zone (
		zone	: in type_catch_zone;
		point	: in type_vector_model)
		return boolean
	is
		d : type_float_positive := get_distance_absolute (zone.center, point);
	begin
		if d <= zone.radius then
			return true;
		else
			return false;
		end if;
	end in_catch_zone;
	

	


	

	function in_catch_zone (
		zone	: in type_catch_zone;
		line	: in type_line;
		width	: in type_distance_positive := 0.0)
		return boolean
	is
		width_float : constant type_float_positive := type_float_positive (width);
		distance_to_center : type_float_positive;
	begin
		-- put_line ("point " & to_string (point));
		-- put_line ("line  " & to_string (line));

		-- Compute the distance from line to center of catch zone:
		distance_to_center := get_shortest_distance (line, zone.center);

		-- If no linewidth, then we simply compare the distance_to_center
		-- with the radius of the zone:
		if width = 0.0 then
			-- put_line ("distance to center " & to_string (distance_to_center));
			if distance_to_center <= zone.radius then
				return true;
			else
				return false;
			end if;

		-- If a linewidth is given then there are two possible cases.
		-- Case 1: The given point is inside the strip around the line.
		-- Case 2: The point is outside the strip around the line.
		else
			if distance_to_center <= width_float then
				-- case 1
				return true;
			else
				-- case 2
				if distance_to_center - width_float <= zone.radius then 
					return true;
				else
					return false;
				end if;				
			end if;
		end if;
	end in_catch_zone;
		


	

	function in_catch_zone (
		zone	: in type_catch_zone;
		arc		: in type_arc;
		width	: in type_distance_positive := 0.0)
		return boolean
	is
		-- debug : boolean := true;
		debug : boolean := false;
		
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




	function in_catch_zone (
		zone	: in type_catch_zone;
		circle	: in type_circle;
		width	: in type_distance_positive := 0.0)
		return boolean
	is
		distance : type_distance;
	begin
		distance := get_distance (circle.center, get_center (zone));
		distance := distance - circle.radius - width;

		if distance <= type_distance_positive (get_radius (zone)) then
			return true;
		else
			return false;
		end if;
	end in_catch_zone;


	

	procedure nothing_found (
		zone	: in type_catch_zone)
	is begin
		log (importance => WARNING, 
			 text => "nothing found in" & to_string (zone));
	end nothing_found;




	
	
	

-- ZONES OF A LINE


	function to_string (
		zone : in type_line_zone)
		return string
	is begin
		return type_line_zone'image (zone);
	end;

	

	
	function get_zone (
		line	: in type_line;
		point	: in type_vector_model)
		return type_line_zone 
	is
		zone : type_line_zone; -- to be returned
	
		line_length : type_distance;
		zone_border : type_distance;
		
	begin -- get_zone
		-- CS: The algorithm used here is not the best. Improve using vector algebra ?
		
		-- The greater distance from start to end point in X or Y determines 
		-- whether the line is handled like a horizontal or vertical drawn line.
		if get_distance_absolute (line.A, line.B, AXIS_X) > 
			get_distance_absolute (line.A, line.B, AXIS_Y) then

			-- distance in X greater -> decision will be made along the X axis.
			-- The line will be handled like a horizontal drawn line.
			
			-- calculate the zone border. This depends on the line length in X direction.
			line_length := get_distance_absolute (line.A, line.B, AXIS_X);
			zone_border := line_length / type_distance (line_zone_division_factor);
			-- CS ? should be: zone_border := line_length / to_distance (line_zone_division_factor);
			
			if get_x (line.A) < get_x (line.B) then 
			-- DRAWN FROM LEFT TO THE RIGHT
				if get_x (point) < get_x (line.A) + zone_border then
					zone := START_POINT; -- point is in the zone of line.A
				elsif get_x (point) > get_x (line.B) - zone_border then
					zone := END_POINT; -- point is in the zone of line.B
				else
					zone := CENTER;
				end if;

			else 
			-- DRAWN FROM RIGHT TO THE LEFT
				if get_x (point) > get_x (line.A) - zone_border then
					zone := START_POINT; -- point is in the zone of line.A
				elsif get_x (point) < get_x (line.B) + zone_border then
					zone := END_POINT; -- point is in the zone of line.B
				else
					zone := CENTER;
				end if;
			end if;

			
		else
			-- distance in Y greater or equal distance in X -> decision will be made along the Y axis.
			-- The line will be handled like a vertical drawn line.

			-- calculate the zone border. This depends on the line length in Y direction.
			line_length := get_distance_absolute (line.A, line.B, AXIS_Y);
			zone_border := line_length / type_distance (line_zone_division_factor);
			-- CS ? should be: zone_border := line_length / to_distance (line_zone_division_factor);
			
			if get_y (line.A) < get_y (line.B) then 
			-- DRAWN UPWARDS
				if get_y (point) < get_y (line.A) + zone_border then
					zone := START_POINT; -- point is in the zone of line.A
				elsif get_y (point) > get_y (line.B) - zone_border then
					zone := END_POINT; -- point is in the zone of line.B
				else
					zone := CENTER;
				end if;
					
			else 
			-- DRAWN DOWNWARDS
				if get_y (point) > get_y (line.A) - zone_border then
					zone := START_POINT; -- point is in the zone of line.A
				elsif get_y (point) < get_y (line.B) + zone_border then
					zone := END_POINT; -- point is in the zone of line.B
				else
					zone := CENTER;
				end if;
			end if;
			
			
		end if;
		
		return zone;
	end get_zone;



	procedure attack (
		line			: in out type_line;
		point_of_attack	: in type_vector_model;
		destination		: in type_vector_model)
	is
		zone : constant type_line_zone := get_zone (line, point_of_attack);
		offset : type_vector_model;
	begin
		case zone is
			when START_POINT =>
				offset := get_distance_relative (line.A, destination);
				move_start_by (line, offset);
				
			when END_POINT =>
				offset := get_distance_relative (line.B, destination);
				move_end_by (line, offset);
				
			when CENTER =>
				offset := get_distance_relative (point_of_attack, destination);
				move_start_by (line, offset);
				move_end_by (line, offset);

		end case;
	end attack;



	

	function get_zone (
		arc		: in type_arc;
		point	: in type_vector_model)
		return type_line_zone 
	is
		zone : type_line_zone := CENTER; -- to be returned

		subtype type_arcs is type_arc_segments (1 .. 3);
		segments : type_arcs;

		idx : positive;		

	begin
		-- Split the given arc into three segments
		-- which are the start, center and end portion of the arc:
		segments := split_arc (to_arc_fine (arc), 3);

		-- Find the index of the segment that is
		-- nearest to the given point:
		idx := get_nearest (segments, to_vector (point));

		-- put_line ("idx" & positive'image (idx));
		
		case idx is
			when 1 => zone := START_POINT;
			when 3 => zone := END_POINT;
			when others => null;
		end case;
		
		return zone;
	end get_zone;


	

	procedure attack (
		arc				: in out type_arc;
		point_of_attack	: in type_vector_model;
		destination		: in type_vector_model)
	is
		-- debug : boolean := true;
		debug : boolean := false;
		
		zone : type_line_zone;
		offset : type_vector_model;
	begin
		zone := get_zone (arc, point_of_attack);

		if debug then
			put_line (to_string (arc));
			put_line ("point of attack " & to_string (point_of_attack));
			put_line ("destination     " & to_string (destination));
			put_line ("zone            " & to_string (zone));
		end if;
		

		case zone is
			when START_POINT =>
				null;

			when END_POINT =>
				null;

			when CENTER =>
				-- Move the arc without changing
				-- start, end or radius:
				offset := destination - point_of_attack;
				move_by (arc, offset);
		end case;
	end attack;


	

	function get_bounding_box (
		line		: in type_line'class;
		width		: in type_distance_positive;
		offset_1	: in type_vector_model;
		offset_2	: in type_vector_model := origin;
		rotation	: in type_rotation;
		mirror		: in type_mirror := MIRROR_NO)	
		return type_area
	is
		-- Make a copy of the given line:
		l : type_line'class := line;

		b : type_area;
	begin
		rotate_line_by (l, rotation);

		case mirror is
			when MIRROR_NO => null;
			when MIRROR_ALONG_X_AXIS => mirror_line (l, MIRROR_ALONG_X_AXIS);
			when MIRROR_ALONG_Y_AXIS => mirror_line (l, MIRROR_ALONG_Y_AXIS);
		end case;
		
		-- Move the line by offset_1:
		move_by (l, offset_1);
		
		-- Move the line by offset_2:
		move_by (l, offset_2);

		-- Get the bounding-box of line:
		b := get_bounding_box (l, width);
		-- put_line ("b " & to_string (b));
		
		return b;
	end get_bounding_box;



	function get_bounding_box (
		arc			: in type_arc'class;
		width		: in type_distance_positive;
		offset_1	: in type_vector_model;
		offset_2	: in type_vector_model := origin;
		rotation	: in type_rotation;
		mirror		: in type_mirror := MIRROR_NO)	
		return type_area
	is
		-- Make a copy of the given arc:
		c : type_arc'class := arc;

		b : type_area;
	begin
		rotate_arc_by (c, rotation);

		-- Mirror the arc:
		case mirror is
			when MIRROR_NO => null;
			when MIRROR_ALONG_X_AXIS  => mirror_arc (c, MIRROR_ALONG_X_AXIS);
			when MIRROR_ALONG_Y_AXIS  => mirror_arc (c, MIRROR_ALONG_Y_AXIS);
		end case;

		-- Move the arc by offset_1:
		move_by (c, offset_1);
				 
		-- Move the arc by offset_2:
		move_by (c, offset_2);

		-- Get the bounding-box of arc:
		b := get_bounding_box (c, width);
		-- put_line ("b " & to_string (b));
		
		return b;
	end get_bounding_box;



	function get_bounding_box (
		circle		: in type_circle'class;
		width		: in type_distance_positive;
		offset_1	: in type_vector_model;
		offset_2	: in type_vector_model := origin;
		rotation	: in type_rotation;
		mirror		: in type_mirror := MIRROR_NO)	
		return type_area
	is
		-- Make a copy of the given circle:
		c : type_circle'class := circle;

		b, result : type_area;
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

	
	
end et_geometry_2a;

