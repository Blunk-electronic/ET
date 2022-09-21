------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              GEOMETRY 1                                  --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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
with ada.strings.unbounded;		use ada.strings.unbounded;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with et_exceptions;				use et_exceptions;



package body et_geometry_1 is

	
	function "=" (left, right : in type_float_internal) return boolean is begin
		if abs (left - right) <= accuracy then
			return true;
		else
			return false;
		end if;
	end "=";

	
	--function ">=" (left, right : in type_float_internal) return boolean is begin
		--if left = right then
			--return true;
		--elsif left > right then
			--return true;
		--else
			--return false;
		--end if;
	--end ">=";


	--function "<=" (left, right : in type_float_internal) return boolean is begin
		--if left = right then
			--return true;
		--elsif left < right then
			--return true;
		--else
			--return false;
		--end if;
	--end "<=";

	
	
	function get_average (
		f1, f2 : in type_float_internal)
		return type_float_internal
	is begin
		return (f1 + f2) * 0.5;
	end get_average;

	
	--procedure round (
		--f : in out type_float_internal;
		--a : in type_rounding_accuracy)
	--is 
		--base : constant type_float_internal := 10.0;
	--begin
		----put_line ("given   :" & type_float_internal'image (f));
		----put_line ("accuracy:" & positive'image (a));

		--f := type_float_internal'rounding (f * base**a) * base**(-a); 
		
		----put_line ("rounded :" & type_float_internal'image (f));
		----new_line;
	--end round;


	--function round (
		--f : in type_float_internal;
		--a : in type_rounding_accuracy)
		--return type_float_internal
	--is 
		--base : constant type_float_internal := 10.0;
	--begin
		--return type_float_internal'rounding (f * base**a) * base**(-a); 
	--end round;


	function mil_to_distance (mil : in string) return type_float_internal is
		distance_mil : type_float_internal := type_float_internal'value (mil);
	begin
		return distance_mil * (25.4 * 0.001);
	end mil_to_distance;
	

	function distance_to_mil (d : in type_float_internal) return string is
		scratch : type_float_internal;
	begin
		scratch := d * 1000.00 / 25.4;
		return to_string (scratch);
	end;

	
	
	function to_string (f : in type_float_internal) return string is begin
		return type_float_internal'image (f);
	end;

	
	function to_float (s : in string) return type_float_internal is begin
		return type_float_internal'value (s);
	end to_float;
	

	function to_angle (a : in string) return type_angle is begin
		return type_angle'value (a);
	end to_angle;

	
	function to_radians (degrees : in type_angle) return type_float_internal is
		use ada.numerics;
	begin
		return (pi * type_float_internal (degrees)) / (units_per_cycle * 0.5);
	end to_radians;

	
	function to_degrees (
		radians : in type_float_internal)
		return type_angle 
	is
		use ada.numerics;
	begin
		--return to_rotation ((units_per_cycle * 0.5 * radians) / pi); -- CS
		return (units_per_cycle * 0.5 * radians) / pi;
	end to_degrees;


	
	function get_direction (
		rotation : in type_angle) 
		return type_direction_of_rotation
	is begin
		if rotation < 0.0 then
			return CW;
		else
			return CCW;
		end if;
	end get_direction;

	

	function to_string (
		numbers : in pac_float_numbers.list)
		return string
	is 
		use pac_float_numbers;
		
		scratch : unbounded_string;

		procedure query_number (c : in pac_float_numbers.cursor) is 
			use ada.characters.latin_1;
		begin
			scratch := scratch & to_string (element (c)) & LF;
		end query_number;
					
	begin
		numbers.iterate (query_number'access);
		return to_string (scratch);
	end to_string;
	

	
	procedure clean_up (
		numbers	: in out pac_float_numbers.list;
		mode	: in type_clean_up_mode)
	is
		use pac_float_numbers;
		
		result : pac_float_numbers.list;
		
		c : pac_float_numbers.cursor := numbers.first;

		
		procedure do_reduce is

			procedure query_number (c : in pac_float_numbers.cursor) is 
				n_candidate : type_float_internal renames element (c);
			begin
				if c = numbers.first then
					result.append (n_candidate);
				else
					if n_candidate /= result.last_element then
						result.append (n_candidate);
					end if;
				end if;
			end query_number;
				
		begin
			numbers.iterate (query_number'access);
		end do_reduce;


		
		procedure do_remove is 

			procedure query_number (c : in pac_float_numbers.cursor) is
				n_candidate : type_float_internal renames element (c);
			begin
				if c = numbers.first then
					-- If next number equals candidate then skip candidate,
					-- else append candidate to the return:
					if element (next (c)) /= n_candidate then
						result.append (n_candidate);
					end if;
					
				elsif c = numbers.last then
					-- If previous number equals candidate then skip candidate,
					-- else append candidate to the return:
					if element (previous (c)) /= n_candidate then
						result.append (n_candidate);
					end if;

				else
					-- If previous OR next number equals candidate then skip candidate,
					-- else append candidate to the return:
					if element (previous (c)) = n_candidate
					or element (next (c)) = n_candidate
					then
						null;
					else
						result.append (n_candidate);
					end if;
				end if;
			end query_number;

		begin
			numbers.iterate (query_number'access);
		end do_remove;
		
		
	begin
		case numbers.length is
			when 0 | 1 => null; -- leave given list as it is

			when others =>
				case mode is 
					when REDUCE_TO_ONE =>
						do_reduce;
						
					when REMOVE_REDUNDANT =>
						do_remove;
				end case;

				numbers := result; -- overwrite given list
		end case;
	end clean_up;

	


	function sgn (x : type_float_internal) return type_float_internal is begin
		if x >= 0.0 then
			return 1.0;
		else
			return -1.0;
		end if;
	end sgn;


	function get_greatest (
		left, right : in type_float_internal)
		return type_float_internal
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
		left, right : in type_float_internal)
		return type_float_internal
	is begin
		if left < right then
			return left;
		elsif left > right then
			return right;
		else
			return right;
		end if;
	end get_smallest;

	

	
	--function to_string (distance : in type_distance) 
		--return string
	--is begin
		--if distance < zero then
			--return space & type_distance'image (distance);
		--else
			--return type_distance'image (distance);
		--end if;
		---- CS suppress trailing zeros
	--end to_string;


	--function to_string (d_coarse : in type_distance_coarse) 
		--return string
	--is begin
		--if d_coarse < 0.0 then
			--return space & type_distance_coarse'image (d_coarse);
		--else
			--return type_distance_coarse'image (d_coarse);
		--end if;
		---- CS suppress trailing zeros
	--end to_string;


	function to_distance (df : in string)
		return type_float_internal
	is begin
		return type_float_internal'value (df);
	end to_distance;

	
	--function to_distance (f : in type_float_internal)
		--return type_distance 
	--is
		--use pac_distance_io;
		
		--d1 : type_distance;
		--d2 : type_float_internal;

		--f1 : constant type_float_internal := 5.0 * type_float_internal (type_distance'small);
	--begin
		--d1 := type_distance (f);
		
		--d2 := 10.0 * abs (f - type_float_internal (d1));
		
		--if f < 0.0 then
			--if d2 > f1 then
				--d1 := d1 - type_distance'small;
			--end if;
		--else
			--if d2 > f1 then
				--d1 := d1 + type_distance'small;
			--end if;
		--end if;

		--return d1;
		
		----if f < 0.0 then
			----declare
				----r : string (1 .. type_distance'digits + 2); -- sign + point
			----begin
				------ CS: IMPROVEMENT REQUIRED !!!
				----put (to => r, item => f, aft => type_distance'scale, exp => 0);
				----return type_distance'value (r);
			----end;
		----else
			----declare
				----r : string (1 .. type_distance'digits + 1); -- point
			----begin
				------put_line (type_float_internal'image (f) & " " & natural'image (r'length));
				------ CS: IMPROVEMENT REQUIRED !!!
				----put (to => r, item => f, aft => type_distance'scale, exp => 0);
				----return type_distance'value (r);
			----end;
		----end if;


		--exception when event: others =>
			--put_line ("f: " & to_string (f));
			--raise;

	--end to_distance;
	

	--function to_rotation (f : in type_float_internal)
		--return type_rotation 
	--is
		--use pac_distance_io;

		--d1 : type_rotation := type_rotation (f);
		--d2 : type_float_internal;

		--f1 : constant type_float_internal := 5.0 * type_float_internal (type_rotation'small);

	--begin
		--d2 := 10.0 * abs (f - type_float_internal (d1));
		
		--if f < 0.0 then
			--if d2 > f1 then
				--d1 := d1 - type_rotation'small;
			--end if;
		--else
			--if d2 > f1 then
				--d1 := d1 + type_rotation'small;
			--end if;
		--end if;

		--return d1;
		
		----if f < 0.0 then
			----declare
				----r : string (1 .. type_rotation'digits + 2); -- sign + point
			----begin
				------ CS: IMPROVEMENT REQUIRED !!!
				----put (to => r, item => f, aft => type_rotation'scale, exp => 0);
				----return type_rotation'value (r);
			----end;
		----else
			----declare
				----r : string (1 .. type_rotation'digits + 1); -- point
			----begin
				------ CS: IMPROVEMENT REQUIRED !!!
				----put (to => r, item => f, aft => type_rotation'scale, exp => 0);
				----return type_rotation'value (r);
			----end;
		----end if;
	--end to_rotation;

	
	--function round (
		--d_fine	: in type_distance;
		--mode	: in type_rounding_mode := rounding_mode_default) 
		--return type_distance_coarse
	--is
		--d_coarse : type_distance_coarse := type_distance_coarse (d_fine);
		--d_delta : type_distance_positive;

		--procedure do_it is begin
			--if d_fine > 0.0 then
				--d_coarse := d_coarse + type_distance_coarse'small;
			--else
				--d_coarse := d_coarse - type_distance_coarse'small;
			--end if;
		--end do_it;
		
	--begin
		--d_delta := abs (d_fine) - abs (type_distance (d_coarse));

		--case mode is
			--when UP =>
				--if d_delta > zero then
					--do_it;
				--end if;
					
			--when DOWN =>
				--null;
				
			--when BANKERS_RULE =>
				----if d_delta >= 500_000.0 * type_distance'small then
				--if d_delta >= 0.5 * type_distance (type_distance_coarse'small) then
					--do_it;
				--end if;
		--end case;
		
		--return d_coarse;
	--end round;



	function to_offset (
		x, y : in type_float_internal)
		return type_offset
	is begin
		return (x, y);
	end to_offset;


	
	
	function invert (
		d : in type_offset)
		return type_offset
	is begin
		return (-1.0 * d.x, -1.0 * d.y);
	end invert;


	
	function add (
		left, right : in type_angle) 
		return type_angle 
	is
		scratch : type_angle;
	begin
		scratch := left + right;
		
		if scratch >= 360.0 then
			scratch := scratch - 360.0;
			
		elsif scratch <= -360.0 then
			scratch := scratch + 360.0;
		end if;

		--result := to_rotation (scratch); -- CS
		return scratch;
	end;


	function to_angle_positive (
		rotation : in type_angle)
		return type_angle_positive
	is begin
		if rotation < 0.0 then
			return 360.0 + rotation;
		else
			return rotation;
		end if;
	end to_angle_positive;


	
	function to_string (
		distance : in type_distance_polar)
		return string
	is begin
		return ("abs:" & to_string (distance.absolute) 
			& " / angle:" & to_string (distance.angle));
	end to_string;
	
	
	function to_polar (
		absolute	: in type_float_internal_positive;
		angle		: in type_angle)
		return type_distance_polar
	is begin
		return (absolute, angle);
	end to_polar;

	
	procedure set_absolute (
		distance : in out type_distance_polar;
		absolute : in type_float_internal_positive)
	is begin
		distance.absolute := absolute;
	end set_absolute;

	
	procedure set_angle (
		distance : in out type_distance_polar;
		angle    : in type_angle)
	is begin
		distance.angle := angle;
	end set_angle;


	procedure reverse_angle (
		distance : in out type_distance_polar)
	is begin
		--distance.angle := add (distance.angle, 180.0);
		distance.angle := distance.angle + 180.0; -- CS range check may fail
	end reverse_angle;

	
	
	function get_angle (
		distance : in type_distance_polar) 
		return type_angle 
	is begin
		return distance.angle;
	end get_angle;

	
	function get_absolute (
		distance : in type_distance_polar) 
		return type_float_internal_positive
	is begin
		return distance.absolute;
	end get_absolute;


-- VECTORS	

	function "=" (
		left, right : in type_vector)
		return boolean 
	is begin
		--new_line;
		--put_line ("left: " & to_string (left));
		--put_line ("right:" & to_string (right));

		if  left.x = right.x 
		and left.y = right.y
		and left.z = right.z
		then
			--put_line ("equal");
			return true;
		else
			--put_line ("not equal");
			return false;
		end if;
	end "=";


	function get_average (
		v1, v2 : in type_vector)
		return type_vector
	is 
		result : type_vector;
	begin
		result.x := (v1.x + v2.x) * 0.5;
		result.y := (v1.y + v2.y) * 0.5;
		result.z := (v1.z + v2.z) * 0.5;
		return result;
	end get_average;

	
	function get_offset (
		v1, v2 : in type_vector)
		return type_offset
	is begin
		return (v2.x - v1.x, v2.y - v1.y);
	end get_offset;

	
	function to_offset (
		v : in type_vector)
		return type_offset
	is begin
		return (v.x, v.y);
	end to_offset;

	
	function to_string (
		v		: in type_vector;
		show_z	: in boolean := false)
		return string
	is begin
		if show_z then
			return vector_preamble_3d 
				& to_string (v.x) & axis_separator
				& to_string (v.y) & axis_separator
				& to_string (v.z);
		else
			return vector_preamble_2d 
				& to_string (v.x) & axis_separator
				& to_string (v.y);
		end if;
	end to_string;
	
	
	function get_quadrant (
		point : in type_vector) 
		return type_quadrant
	is begin
		if point.x >= 0.0 then -- we are right of the y-axis or on top of it
			if point.y >= 0.0 then -- we are above the x-axis or on top of it
				return ONE; 
			else -- we are below the x-axis
				return FOUR;
			end if;
			
		else -- we are left of the y-axis
			if point.y >= 0.0 then -- we are above the x-axis or on top of it
				return TWO;
			else -- we are below the x-axis
				return THREE;
			end if;
		end if;
	end get_quadrant;

	
	function set (
		x : in type_float_internal;
		y : in type_float_internal;
		z : in type_float_internal := 0.0)
		return type_vector
	is begin
		return (x, y, z);
	end set;


	
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


	function get_absolute (
		vector	: in type_vector)
		return type_float_internal
	is begin
		return
			sqrt (
				vector.x * vector.x + 
				vector.y * vector.y +
				vector.z * vector.z);

	end get_absolute;


	function get_sum_of_squared_components (
		vector	: in type_vector)
		return type_float_internal
	is begin
		return (
			vector.x * vector.x + 
			vector.y * vector.y +
			vector.z * vector.z);

	end get_sum_of_squared_components;

	

	function "<" (
		left, right : in type_vector) 
		return boolean
	is begin
		if get_absolute (left) < get_absolute (right) then
			return true;
		else
			return false;
		end if;
	end;

	
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
		--if is_not_zero (b.x) then
			lambda := a.x / b.x;
			
		elsif b.y /= 0.0 then
		--elsif is_not_zero (b.y) then
			lambda := a.y / b.y;
			
		elsif b.z /= 0.0 then
		--elsif is_not_zero (b.z) then
			lambda := a.z / b.z;
			
		else
			put_line ("ERROR while vector division ");
		end if;

		-- CS divide all components and take the average of all lambdas ?
		return lambda;
	end divide;



	procedure mirror (
		v		: in out type_vector;
		axis	: in type_axis_2d)
	is begin
		case axis is
			when X =>
				v.y := v.y * (-1.0);
			when Y =>
				v.x := v.x * (-1.0);
		end case;
	end mirror;
	



	function get_displacement (
		v1, v2 : in type_vector)
		return type_vector
	is begin
		return (
			v2.x - v1.x,
			v2.y - v1.y,
			v2.z - v1.z);
	end get_displacement;


	procedure move_by (
		vectors	: in out pac_vectors.list;
		offset	: in type_offset)
	is 
		use pac_vectors;
		
		procedure move (vector : in out type_vector) is begin
			move_by (vector, offset);
		end move;
		
		v : pac_vectors.cursor := vectors.first;
	begin
		while v /= pac_vectors.no_element loop
			vectors.update_element (v, move'access);
			next (v);
		end loop;
	end move_by;



	procedure rotate_by (
		vectors	: in out pac_vectors.list;
		angle	: in type_angle)
	is
		use pac_vectors;
		c : pac_vectors.cursor := vectors.first;

		procedure rotate (v : in out type_vector) is begin
			rotate_by (v, angle);
		end rotate;
		
	begin
		while c /= pac_vectors.no_element loop
			vectors.update_element (c, rotate'access);
			next (c);
		end loop;
	end rotate_by;
	
	
	
	procedure splice_vectors (
		v_target : in out pac_vectors.list;
		v_source : in pac_vectors.list)
	is
		scratch : pac_vectors.list := v_source;
	begin
		pac_vectors.splice (
			target	=> v_target,
			before	=> pac_vectors.no_element,
			source	=> scratch);

	end splice_vectors;



	procedure remove_redundant_vectors (
		vectors : in out pac_vectors.list)
	is
		use pac_vectors;
		target : pac_vectors.list;

		procedure query_vector (c : in pac_vectors.cursor) is begin
			if not target.contains (element (c)) then
				target.append (element (c));
			end if;
		end query_vector;
		
	begin
		vectors.iterate (query_vector'access);
		vectors := target;
	end remove_redundant_vectors;

	
	
	function get_distance_total (
		v1 : in type_vector;
		v2 : in type_vector)
		return type_float_internal_positive
	is 
		dx : constant type_float_internal := abs (v2.x - v1.x);
		dy : constant type_float_internal := abs (v2.y - v1.y);
	begin
		return sqrt (dx ** 2.0 + dy ** 2.0);
	end get_distance_total;


	function get_distance (
		v1, v2 : in type_vector)
		return type_distance_polar
	is
		result : type_distance_polar;

		dx : constant type_float_internal := v2.x - v1.x;
		dy : constant type_float_internal := v2.y - v1.y;

		abs_dx : constant type_float_internal := abs (dx);
		abs_dy : constant type_float_internal := abs (dy);
	begin
		--put_line ("dx " & type_float_internal'image (abs_dx));
		--put_line ("dy " & type_float_internal'image (abs_dy));
		
		set_absolute (result, sqrt (abs_dx ** 2 + abs_dy ** 2));
		
		-- NOTE: If the total distance between the location vectors is zero then
		-- the arctan operation is not possible. In this case we assume
		-- the resulting angle is zero.
		-- So we do the angle computation only if there is a distance between the vectors:
		if get_absolute (result) /= 0.0 then
			
			--set_angle (result, to_rotation (arctan (
			set_angle (result, arctan (
					x 		=> dx,
					y		=> dy,
					cycle	=> units_per_cycle));
		else
			-- distance is zero
			set_angle (result, 0.0);
		end if;
		
		return result;

		exception when event: others =>
			put_line ("v1:" & to_string (v1));
			put_line ("v2:" & to_string (v2));
			raise;
		
	end get_distance;


	procedure rotate_by (
		vector		: in out type_vector;
		rotation	: in type_angle;
		debug		: in boolean := false)
	is
		angle_out			: type_float_internal; -- degrees
		distance_to_origin	: type_float_internal; -- unit is mm
		scratch				: type_float_internal;
	begin
		-- Do nothing if the given rotation is zero.
		if rotation /= 0.0 then

			-- compute distance of given vector to origin
			if vector.x = 0.0 and vector.y = 0.0 then
				distance_to_origin := 0.0;
				
			elsif vector.x = 0.0 then
				distance_to_origin := abs (vector.y);
				
			elsif vector.y = 0.0 then
				distance_to_origin := abs (vector.x);
				
			else
				--put_line ("A");

				distance_to_origin := sqrt (
					(abs (vector.x)) ** 2.0
					+
					(abs (vector.y)) ** 2.0
					);

				--put_line ("B");
			end if;
			
			-- compute the current angle of the given vector (in degrees)

			if vector.x = 0.0 then
				if vector.y > 0.0 then
					angle_out := 90.0;
					
				elsif vector.y < 0.0 then
					angle_out := -90.0;
					
				else
					angle_out := 0.0;
				end if;

			elsif vector.y = 0.0 then
				if vector.x > 0.0 then
					angle_out := 0.0;
					
				elsif vector.x < 0.0 then
					angle_out := 180.0;
					
				else
					angle_out := 0.0;
				end if;

			else
				--put_line ("C");
				
				-- neither x nor y of vector is 0.0
				angle_out := arctan (
					x		=> vector.x,
					y		=> vector.y,
					cycle	=> units_per_cycle);

				--put_line ("D");
			end if;

			-- Compute new angle by adding current angle and given angle.
			angle_out := angle_out + rotation;

			-- compute new x   -- (cos angle_out) * distance_to_origin
			scratch := cos (angle_out, units_per_cycle);
			vector.x := scratch * distance_to_origin;
			
			-- compute new y   -- (sin angle_out) * distance_to_origin
			scratch := sin (angle_out, units_per_cycle);
			vector.y := scratch * distance_to_origin;
			
		end if; -- if angle not zero			
	end rotate_by;


	function move_by (
		v		: in type_vector;
		offset	: in type_offset)
		return type_vector
	is
		result : type_vector := v;
	begin
		result.x := result.x + offset.x; -- CS good idea ?
		result.y := result.y + offset.y;
		return result;
	end move_by;


	procedure move_by (
		v		: in out type_vector;
		offset	: in type_offset)
	is begin
		v.x := v.x + offset.x; -- CS good idea ?
		v.y := v.y + offset.y;
	end move_by;

	
	procedure move_by (
		v			: in out type_vector;
		direction	: in type_angle;
		distance	: in type_float_internal)
	is
		delta_x, delta_y : type_float_internal := 0.0;
	begin
		-- sin (direction) * distance = delta y
		-- cos (direction) * distance = delty x

		delta_y := sin (direction, units_per_cycle) * distance;
		delta_x := cos (direction, units_per_cycle) * distance;

		v.x := v.x + delta_x;
		v.y := v.y + delta_y;
	end move_by;


	function move_by (
		v			: in type_vector;
		direction	: in type_angle;
		distance	: in type_float_internal) -- CS type_float_internal_positive ?
		return type_vector
	is
		result : type_vector := v;
	begin
		move_by (result, direction, distance);
		return result;
	end move_by;


	

-- RAY

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


	

-- LINE VECTOR

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
		offset	: in type_offset)
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
		return type_angle
	is 
		a : type_angle;
	begin

		--a := to_rotation (arctan (
		a := arctan (
				y		=> type_float_internal (line.v_direction.y), 
				x		=> type_float_internal (line.v_direction.x), 
				cycle	=> units_per_cycle);

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

	
	function get_normal_vector (
		line	: in type_line_vector;
		point	: in type_vector)
		return type_line_vector
	is
		result : type_line_vector := (v_start => point, others => <>);

		dir_A : type_vector renames line.v_direction;
		dir_B : type_vector := (0.0, 0.0, 1.0);
		N : type_vector;
	begin
		N := cross_product (dir_A, dir_B);
		
		result.v_direction := N;
		
		return result;
	end get_normal_vector;


	
-- INTERSECTIONS

	function to_string (intersection : in type_intersection)
		return string
	is begin
		return to_string (intersection.vector) 
			& " angle" & to_string (intersection.angle);
	end to_string;


	function get_angle_of_itersection (
		line_1, line_2	: in type_line_vector)
		return type_angle
	is 
		a, b, c : type_float_internal;
		r : type_angle;
	begin
		a := dot_product (line_1.v_direction, line_2.v_direction);
		b := get_absolute (line_1.v_direction) * get_absolute (line_2.v_direction);
		c := a / b;

		-- c may be slightly greater than 1.0 or smaller than -1.0. In these cases
		-- the rotation can be set without any calculation:
		if c > 1.0 then
			r := 0.0;

		elsif c < -1.0 then
			r := 180.0;

		else
			--r := to_rotation (arccos (X => a / b, cycle => units_per_cycle));
			-- CS
			r := arccos (X => a / b, cycle => units_per_cycle);
		end if;

		return r;
	end get_angle_of_itersection;

	
	
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
		begin
			-- The first condition to be fulfilled is that the lines
			-- must run parallel to each other. In this case the cross
			-- product is zero.
			v1 := cross_product (line_1.v_direction, line_2.v_direction); 
			
			if v1 = null_vector then -- the lines run parallel to each other.

				-- Compute the distance between the lines.
				-- If the distance is zero then the lines overlap.
				
				a := get_absolute (cross_product (line_1.v_direction, subtract (line_2.v_start, line_1.v_start)));
				b := get_absolute (line_1.v_direction);

				distance := a / b;

				if distance = 0.0 then
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
					a := line_1.v_start.y;
					b := line_2.v_start.x * line_1.v_direction.y / line_1.v_direction.x;
					c := line_1.v_start.x * line_1.v_direction.y / line_1.v_direction.x;
					d := line_2.v_start.y;
					e := line_2.v_direction.y;
					f := line_2.v_direction.x * line_1.v_direction.y / line_1.v_direction.x;
					g := 1.0 / (e - f);

					lambda := (a + b - c - d) * g;

					i.vector := add (line_2.v_start, scale (line_2.v_direction, lambda));
				else
					a := line_2.v_start.y;
					b := line_1.v_start.x * line_2.v_direction.y / line_2.v_direction.x;
					c := line_2.v_start.x * line_2.v_direction.y / line_2.v_direction.x;
					d := line_1.v_start.y;
					e := line_1.v_direction.y;
					f := line_1.v_direction.x * line_2.v_direction.y / line_2.v_direction.x;
					g := 1.0 / (e - f);

					lambda := (a + b - c - d) * g;

					i.vector := add (line_1.v_start, scale (line_1.v_direction, lambda));
				end if;

				i.angle := get_angle_of_itersection (line_1, line_2);

				--put_line ("get_intersection: " & to_string (i.vector));
				return (status => EXISTS, intersection => i);
			else

				return (status => NOT_EXISTENT);
			end if;
		end if;
		
	end get_intersection;



	
-- BOUNDARIES

	function to_string (boundaries : in type_boundaries) return string is begin
		return "boundaries: SX:" & to_string (boundaries.smallest_x) 
			& " / GX:" & to_string (boundaries.greatest_x)
			& " / SY:" & to_string (boundaries.smallest_y)
			& " / GY:" & to_string (boundaries.greatest_y);
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


	
	function get_height (boundaries : in type_boundaries)
		return type_float_internal_positive
	is begin
		return boundaries.greatest_y - boundaries.smallest_y;
	end get_height;

	
	function get_width (boundaries : in type_boundaries)
		return type_float_internal_positive
	is begin
		return boundaries.greatest_x - boundaries.smallest_x;
	end get_width;


	function get_left (boundaries : in type_boundaries)
		return type_float_internal
	is begin
		return boundaries.smallest_x;
	end get_left;

	
	function get_right (boundaries : in type_boundaries)
		return type_float_internal
	is begin
		return boundaries.greatest_x;
	end get_right;

	
	function get_bottom (boundaries : in type_boundaries)
		return type_float_internal
	is begin
		return boundaries.smallest_y;
	end get_bottom;


	function get_top (boundaries : in type_boundaries)
		return type_float_internal
	is begin
		return boundaries.greatest_y;
	end get_top;

	
	
	function overlap (
		b1 : in type_boundaries;
		b2 : in type_boundaries)
		return boolean
	is
		-- The the smallest and greatest x-value occupied by the two boundaries:
		left   : constant type_float_internal := get_smallest (b1.smallest_x, b2.smallest_x);
		right  : constant type_float_internal := get_greatest (b1.greatest_x, b2.greatest_x);

		-- The the smallest and greatest y-value occupied by the two boundaries:
		bottom : constant type_float_internal := get_smallest (b1.smallest_y, b2.smallest_y);
		top    : constant type_float_internal := get_greatest (b1.greatest_y, b2.greatest_y);
		
		span_h, span_v : type_float_internal_positive;

		sum_h, sum_v : type_float_internal_positive;

		result : boolean := false;
	begin
		-- The total horizontal and vertical span occupied by the boundaries:
		span_h := right - left;
		span_v := top - bottom;

		-- The sum of their widths and heights:
		sum_h := get_width (b1) + get_width (b2);
		sum_v := get_height (b1) + get_height (b2);

		if sum_h >= span_h  -- overlap horizontally
		and sum_v >= span_v -- overlap vertically
		then
			result := true;
		end if;

		return result;

		exception when constraint_error =>
			put_line ("b1: " & to_string (b1));
			put_line ("b2: " & to_string (b2));
			raise;
		
	end overlap;
	

	--function get_intersection (
		--b1 : in type_boundaries;
		--b2 : in type_boundaries)
		--return type_boundaries_intersection
	--is
		--i : type_boundaries;
	--begin
		--if overlap (b1, b2) then

			----log (text => "b1" & to_string (b1));
			----log (text => "b2" & to_string (b2));
			
			--i.smallest_x := get_greatest (b1.smallest_x, b2.smallest_x);
			--i.greatest_x := get_smallest (b1.greatest_x, b2.greatest_x);

			--i.smallest_y := get_greatest (b1.smallest_y, b2.smallest_y);
			--i.greatest_y := get_smallest (b1.greatest_y, b2.greatest_y);

			----log (text => "b " & to_string (i));
			
			--return (exists => true, intersection => i);
		--else
			--return (exists => false);
		--end if;
	--end get_intersection;


	
	-- Adds two boundaries.
	procedure add (
		b1 : in out type_boundaries;
		b2 : in type_boundaries) 
	is begin
-- 			if boundaries_two.smallest_x < boundaries_one.smallest_x , smallest_y : type_distance := type_distance'last;
-- 			greatest_x, greatest_y : type_distance := type_distance'first;
		null; -- CS
	end; 

	

	
	function to_line_vector (
		line : in type_line)
		return type_line_vector
	is
		result : type_line_vector;
		dp : type_vector;
	begin
		dp := get_displacement (line.start_point, line.end_point);
		
		result.v_start := line.start_point;
		result.v_direction := dp;
		return result;
	end to_line_vector;


	function get_length (
		line : in type_line)
		return type_float_internal_positive
	is begin
		return get_distance_total (line.start_point, line.end_point);
	end get_length;
	

	function to_string (
		line	: in type_line)
		return string
	is begin
		return 
			"line: S:" & to_string (line.start_point) 
			& " / E:" & to_string (line.end_point);
	end to_string;
	

	function make_line (
		start_point, end_point : in type_vector)
		return type_line
	is begin
		return (start_point, end_point);
	end make_line;

	
	procedure move_by (
		line	: in out type_line;
		offset	: in type_offset)
	is begin
		move_by (line.start_point, offset);
		move_by (line.end_point, offset);
	end move_by;


	procedure rotate_by (
		line	: in out type_line;
		offset	: in type_angle)
	is begin
		rotate_by (line.start_point, offset);
		rotate_by (line.end_point, offset);
	end rotate_by;
	

	procedure mirror (
		line	: in out type_line;
		axis	: in type_axis_2d)
	is begin
		mirror (line.start_point, axis);
		mirror (line.start_point, axis);
	end mirror;

	
	procedure mirror_line (
		line	: in out type_line;
		axis	: in type_axis_2d)
	is begin
		mirror (line.start_point, axis);
		mirror (line.start_point, axis);
	end mirror_line;

	
	function reverse_line (
		line	: in type_line)
		return type_line
	is begin
		return (line.end_point, line.start_point);
	end reverse_line;


	procedure reverse_line (
		line	: in out type_line)
	is begin
		line := reverse_line (line);
	end reverse_line;
		

	function get_center (
		line : in type_line)
		return type_vector
	is
		result : type_vector;
		dp : type_vector; -- displacement vector
	begin
		-- get the displacement:
		dp := get_displacement (line.start_point, line.end_point);

		-- halve the displacement
		dp := scale (dp, 0.5);

		-- move start point by displacement
		result := add (line.start_point, dp);
		return result;
	end get_center;


	function get_direction (
		line : in type_line)
		return type_angle
	is
		dp : type_vector;
	begin
		-- get the displacement:
		dp := get_displacement (line.start_point, line.end_point);

		-- NOTE: If dx and dy are zero then the arctan operation is not possible. 
		-- In this case we assume the resulting angle is zero.
		if dp.x = 0.0 and dp.y = 0.0 then
			return 0.0;
		else
			--return to_rotation (arctan (dp.y, dp.x, units_per_cycle));
			return arctan (dp.y, dp.x, units_per_cycle);
		end if;
	end get_direction;


	function opposide_direction (
		right, left : in type_line)
		return boolean
	is
		d1, d2 : type_angle;
	begin
		d1 := get_direction (right);
		d2 := get_direction (left);

		if abs (d1 - d2) = 180.0 then
			return true;
		else
			return false;
		end if;
	end opposide_direction;

	
	function get_boundaries (
		line : in type_line)
		return type_boundaries
	is 
		result : type_boundaries;
	begin
		-- X axis
		if line.start_point.x = line.end_point.x then -- both ends on a vertical line

			result.smallest_x := line.start_point.x;
			result.greatest_x := line.start_point.x;
			
		elsif line.start_point.x < line.end_point.x then
			
			result.smallest_x := line.start_point.x;
			result.greatest_x := line.end_point.x;
		else
			result.smallest_x := line.end_point.x;
			result.greatest_x := line.start_point.x;
		end if;

		-- Y axis
		if line.start_point.y = line.end_point.y then -- both ends on a horizontal line

			result.smallest_y := line.start_point.y;
			result.greatest_y := line.start_point.y;
			
		elsif line.start_point.y < line.end_point.y then
			
			result.smallest_y := line.start_point.y;
			result.greatest_y := line.end_point.y;
		else
			result.smallest_y := line.end_point.y;
			result.greatest_y := line.start_point.y;
		end if;
		
		return result;
	end get_boundaries;


	
	procedure move_by (
		line		: in out type_line;
		direction	: in type_angle;
		distance	: in type_float_internal_positive)
	is begin
		move_by (line.start_point, direction, distance);
		move_by (line.end_point, direction, distance);
	end move_by;


	function move_by (
		line		: in type_line;
		direction	: in type_angle;
		distance	: in type_float_internal_positive)
		return type_line
	is
		result : type_line;
	begin
		result.start_point := move_by (line.start_point, direction, distance);
		result.end_point := move_by (line.end_point, direction, distance);		
		return result;
	end move_by;

	
	
	function get_intersection (
		line_vector : in type_line_vector;
		line		: in type_line;
		debug		: in boolean := false)
		return type_intersection_of_two_lines
	is
		i : constant type_intersection_of_two_lines := get_intersection (
				line_1	=> line_vector,
				line_2	=> to_line_vector (line));
		
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
				if on_line (i.intersection.vector, line, debug) then
					return i;
				else
					return (status => NOT_EXISTENT);
				end if;

			when others =>		
				return i;
		end case;
		
	end get_intersection;
	

	function get_intersection (
		line_1 : in type_line;
		line_2 : in type_line)
		return type_intersection_of_two_lines
	is
		lv_1 : constant type_line_vector := to_line_vector (line_1);
		lv_2 : constant type_line_vector := to_line_vector (line_2);

		int_A : constant type_intersection_of_two_lines := get_intersection (lv_1, line_2);
		int_B : constant type_intersection_of_two_lines := get_intersection (lv_2, line_1);

		status : type_intersection_status_of_two_lines;
		intersection : type_intersection;
	
	begin
		--put_line ("get intersection");
		--put_line ("line 1: " & to_string (line_1));
		--put_line ("line 2: " & to_string (line_2));

		
		--if int_A.status = NOT_EXISTENT or int_B.status = NOT_EXISTENT then
			--status := NOT_EXISTENT;

		-- CS: Safety measure:
		if int_A.status = OVERLAP xor int_B.status = OVERLAP then
			raise constraint_error;
		end if;

		
		if int_A.status = OVERLAP and int_B.status = OVERLAP then -- CS ? correct ?
		--if int_A.status = OVERLAP or int_B.status = OVERLAP then
			status := OVERLAP;

			
		elsif int_A.status = EXISTS and int_B.status = EXISTS then

			-- double check: location vectors must match !
			--if int_A.intersection.vector = int_B.intersection.vector then
				status := EXISTS;
				--intersection.vector := int_A.intersection.vector;
				--intersection.angle := int_A.intersection.angle;

				-- CS: return the average of intersection A and B ?
				intersection.vector := get_average (int_A.intersection.vector, int_B.intersection.vector);
				intersection.angle := get_average (int_A.intersection.angle, int_B.intersection.angle);
				
				--put_line ("intersections match:");
				--put_line ("int  A: " & to_string (int_A.intersection.vector));
				--put_line ("int  B: " & to_string (int_B.intersection.vector));

			--else
				--put_line ("intersection mismatch:");
				----put_line ("line 1: " & to_string (line_1));
				----put_line ("line 2: " & to_string (line_2));
				--put_line ("int  A: " & to_string (int_A.intersection.vector));
				--put_line ("int  B: " & to_string (int_B.intersection.vector));

				--put_line ("delta : " & to_string (
					--subtract (int_A.intersection.vector, int_B.intersection.vector)));
													 
				--raise constraint_error with 
					--"Intersection mismatch: " & to_string (int_A.intersection.vector)
					--& to_string (int_B.intersection.vector);

				
			--end if;

		else
			status := NOT_EXISTENT;
		end if;


		case status is
			when NOT_EXISTENT =>
				return (status => NOT_EXISTENT);

			when OVERLAP =>
				return (status => OVERLAP);

			when EXISTS =>
				return (
					status			=> EXISTS,
					intersection	=> intersection);	   
		end case;

	end get_intersection;

	
	function lines_overlap (
		line_1, line_2 : in type_line)
		return boolean
	is
		I2L : constant type_intersection_of_two_lines :=
			get_intersection (line_1, line_2);
	begin
		if I2L.status = OVERLAP then
			return true;
		else
			return false;
		end if;
	end lines_overlap;

	

-- ARCS

	function to_string (
		arc : in type_arc)
		return string 
	is begin
		return "arc: "
			& "C:" & to_string (arc.center) 
			& " / S:" & to_string (arc.start_point) 
			& " / E:" & to_string (arc.end_point)
			& " / D: " & to_string (arc.direction);
	end to_string;


	function get_radius_start (
		arc : in type_arc) 
		return type_float_internal_positive 
	is begin
		return get_distance_total (arc.center, arc.start_point);
	end get_radius_start;

	
	function get_radius_end (
		arc : in type_arc)
		return type_float_internal_positive
	is begin
		return get_distance_total (arc.center, arc.end_point);
	end get_radius_end;


	function reverse_arc (
		arc : in type_arc) 
		return type_arc
	is
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

	
	procedure reverse_arc (
		arc : in out type_arc) 
	is
		scratch : type_vector := arc.start_point;
	begin
		arc.start_point := arc.end_point;
		arc.end_point := scratch;

		case arc.direction is
			when CW	 => arc.direction := CCW;
			when CCW => arc.direction := CW;
		end case;
	end reverse_arc;


	function normalize_arc (
		arc: in type_arc) 
		return type_arc
	is begin
		case arc.direction is
			when CW  => return reverse_arc (arc);					
			when CCW => return arc;
		end case;
	end normalize_arc;


	function zero_length (
		arc : in type_arc) 
		return boolean 
	is begin
		if arc.start_point = arc.end_point then
			return true;
		else
			return false;
		end if;
	end zero_length;


	function get_span (
		arc	: type_arc)
		return type_angle
	is
		result : type_angle;
		arc_angles : constant type_arc_angles := to_arc_angles (arc);
	begin
		case arc.direction is
			when CCW =>
				result := abs (arc_angles.angle_end - arc_angles.angle_start);
				
			when CW =>
				-- CS use function normalize_arc ?
				result := abs (arc_angles.angle_start - arc_angles.angle_end);
		end case;

		return result;
	end get_span;
	

	function to_string (
		arc : in type_arc_angles)
		return string
	is begin
		return "C:" & to_string (arc.center)
			& " R:" & to_string (arc.radius)
			& " S:" & to_string (arc.angle_start)
			& " E:" & to_string (arc.angle_end)
			& " D: " & to_string (arc.direction);
	end to_string;


	
	function get_span (
		arc	: type_arc_angles)
		return type_angle
	is
		result : type_angle;
	begin
		case arc.direction is
			when CCW =>
				result := abs (arc.angle_end - arc.angle_start);
				
			when CW =>
				-- CS use function normalize_arc ?
				result := abs (arc.angle_start - arc.angle_end);
		end case;

		return result;
	end get_span;

	
	procedure move_to (
		arc			: in out type_arc;
		position	: in type_vector)
	is
		offset : constant type_offset := get_offset (arc.center, position);
	begin
		-- move the center of the arc to the given position
		arc.center := position;

		-- move start and end point of the arc by the offset
		move_by (arc.start_point, offset);
		move_by (arc.end_point,   offset);
	end move_to;


	function move_to (
		arc			: in type_arc;
		position	: in type_vector)
		return type_arc
	is
		result : type_arc := arc;
	begin
		move_to (result, position);
		return result;
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
		move_to (arc_tmp, null_vector);

		
		-- the center is not changed:
		result.center := arc.center;
		
		-- calculate the radius of the arc
		result.radius := get_distance_total (arc_tmp.center, arc_tmp.start_point);

		-- calculate the angles where the arc begins and ends:

		-- NOTE: If x and y are zero then the arctan operation is not possible. 
		-- In this case we assume the resulting angle is zero.
		
		if get_x (arc_tmp.start_point) = 0.0 and get_y (arc_tmp.start_point) = 0.0 then
			result.angle_start := 0.0;
		else
			result.angle_start := arctan (
					y => get_y (arc_tmp.start_point),
					x => get_x (arc_tmp.start_point), 
					cycle => units_per_cycle);
		end if;

		if get_x (arc_tmp.end_point) = 0.0 and get_y (arc_tmp.end_point) = 0.0 then
			result.angle_end := 0.0;
		else
			result.angle_end := arctan (
					y => get_y (arc_tmp.end_point),
					x => get_x (arc_tmp.end_point),
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


	function to_arc (
		arc : in type_arc_angles) 
		return type_arc 
	is
		result : type_arc;
		x, y : type_float_internal;
		offset : constant type_offset := to_offset (arc.center);
	begin
		result.center := arc.center;
		result.direction := arc.direction;

		-- start point:
		x := arc.radius * cos (arc.angle_start, units_per_cycle);
		y := arc.radius * sin (arc.angle_start, units_per_cycle);
		result.start_point := set (x, y);
		move_by (result.start_point, offset);
		
		-- end point:
		x := arc.radius * cos (arc.angle_end, units_per_cycle);
		y := arc.radius * sin (arc.angle_end, units_per_cycle);
		result.end_point := set (x, y);
		move_by (result.end_point, offset);
		
		return result;
	end to_arc;



-- DISTANCE POINT TO LINE

	function get_distance (
		line	: in type_line;
		vector	: in type_vector)
		return type_float_internal
	is
		dv : constant type_vector := to_line_vector (line).v_direction;
		sv : constant type_vector := line.start_point;
		
		d1 : constant type_vector := subtract (vector, sv);
		m, n : type_float_internal;
	begin
		m := get_absolute (cross_product (dv, d1));
		n := get_absolute (dv);
		
		return (m / n);
	end get_distance;



	function get_shortest_distance (
		vector	: in type_vector;
		line	: in type_line)
		return type_float_internal
	is
		result : type_float_internal := 0.0;
		
		d : constant type_distance_point_line := get_distance (
			vector		=> vector,
			line		=> line,
			line_range	=> WITH_END_POINTS);

		d_to_start, d_to_end : type_float_internal;
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
				result := get_distance (d);
			else
				
				-- No imaginary line can be drawn perpendicular from
				-- point to line.

				-- Compare the distances to the end points of the line:
				d_to_start := get_distance_total (line.start_point, vector);
				d_to_end   := get_distance_total (line.end_point, vector);

				if d_to_start < d_to_end then
					result := d_to_start;
				else
					result := d_to_end;
				end if;
				
			end if;

		end if;

		--put_line (to_string (result));
		
		return result;
	end get_shortest_distance;
	

	function on_line (
		vector	: in type_vector;
		line	: in type_line;
		debug	: in boolean := false)
		return boolean
	is
		distance : type_distance_point_line;
	begin
		distance := get_distance (vector, line, WITH_END_POINTS, debug);

		if debug then
			put_line ("on_line distance: " & to_string (distance.distance) 
				& "/ out of range: " & boolean'image (distance.out_of_range));
		end if;
		
		if not distance.out_of_range and distance.distance = 0.0 then
			return true;
		else
			return false;
		end if;
	end on_line;

	
	function get_distance (
		vector		: in type_vector;
		line		: in type_line;
		line_range	: in type_line_range;
		debug		: in boolean := false)
		return type_distance_point_line 
	is
		result : type_distance_point_line;
	
		-- Imagine a line that starts on the given location vector,
		-- travels perpendicular towards
		-- the given line and finally intersects the given line somewhere.
		-- The intersection may be betweeen the start and end point of the given line.
		-- The intersection may be virtual, before start or after end point 
		-- of the given line.
	
		line_direction : constant type_vector := to_line_vector (line).v_direction;

		iv : type_vector renames result.intersection;


		-- Computes the point of intersection: The intersection of a line that runs
		-- from the given location vector perpendicular to the given line:
		procedure compute_intersection is
			SE : constant type_vector := get_displacement (line.start_point, line.end_point);
			SV : constant type_vector := get_displacement (line.start_point, vector);
			SI : type_vector;
			dp : type_float_internal;
			sum : type_float_internal_positive;
		begin
			dp := dot_product (SE, SV);
			sum := get_sum_of_squared_components (SE);
			result.intersection := add (line.start_point, scale (SE, dp / sum));
		end compute_intersection;

			
		lambda_forward, lambda_backward : type_float_internal;
	begin
		--put_line ("line direction vector: " & to_string (line_direction));
		--put_line ("line direction angle : " & to_string (line_direction));
		
		-- The first and simplest test is to figure out whether
		-- the given point sits exactly on the start or end point of the line.
		-- Mind: result.distance has default zero.
		-- This test includes the start and end points of the line. 
		-- On match we exit this function prematurely and return the result
		-- with the appropiate flags set.
		--case line_range is
			--when WITH_END_POINTS | BEYOND_END_POINTS =>
				
				--if vector = line.start_point then
					
					--result.sits_on_start := true;
					--result.out_of_range := false;
					--return result;

				--elsif vector = line.end_point then
					
					--result.sits_on_end := true;
					--result.out_of_range := false;
					--return result;

				--end if;
				
			--when others => null;
		--end case;

		
		-- Compute the distance from the given point to the given line.
		-- This computation does not care about end or start point of the line.
		-- It assumes an indefinite long line without start or end point.
		result.distance := get_distance (line, vector);

		--put_line ("distance " & to_string (result.distance));

		-- Set iv so that it points to the intersection. The
		-- intersection can be anywhere on that indefinite long line.
		compute_intersection;

		
		-- Any point on a line can be computed by this formula (see textbook on vector algebra):
		-- iv = line.start_point + lambda_forward  * line_direction
		-- iv = line.end_point   + lambda_backward * line_direction

		-- Using these formula we can calculate whether iv points between 
		-- (or to) the start and/or end points of the line:
		
		lambda_forward := divide (subtract (iv, line.start_point), line_direction);

		--put_line ("lambda forward:" & to_string (lambda_forward));

		
		if lambda_forward = 0.0 then -- iv points TO start point of line
			--put_line ("on start point");
			case line_range is
				when BETWEEN_END_POINTS =>
					result.out_of_range := true;
					
				when others => 
					result.out_of_range := false;
					result.sits_on_start := true;
			end case;

			return result; -- no more computations required
		end if;
		
		
		if lambda_forward < 0.0 then -- iv points BEFORE start of line
			--put_line ("before start point");
			case line_range is
				when BEYOND_END_POINTS => 
					result.out_of_range := false;
					
				when others => 
					result.out_of_range := true;
			end case;

			return result; -- no more computations required
		end if;
				


		--put_line ("after start point");

		
		lambda_backward := divide (subtract (iv, line.end_point), line_direction);

		--put_line ("lambda backward:" & to_string (lambda_backward));

		
		if lambda_backward = 0.0 then -- iv points TO end point of line
			--put_line ("on end point");
			case line_range is
				when BETWEEN_END_POINTS =>
					result.out_of_range := true;
					
				when others => 
					result.out_of_range := false;
					result.sits_on_end := true;
			end case;

			return result; -- no more computations required
		end if;


		if lambda_backward > 0.0 then -- iv points AFTER end of line
			--put_line ("after end point");
			case line_range is
				when BEYOND_END_POINTS => 
					result.out_of_range := false;
					
				when others => 
					result.out_of_range := true;
			end case;

			return result; -- no more computations required
		end if;


		
		--put_line ("before end point");

		result.out_of_range := false;

		return result;
	end get_distance;

	
	

	
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


	
	function on_start_point (d : in type_distance_point_line) return boolean is begin
		return d.sits_on_start;
	end on_start_point;

	
	function on_end_point (d : in type_distance_point_line) return boolean is begin
		return d.sits_on_end;
	end on_end_point;

	
end et_geometry_1;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
