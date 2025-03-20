------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              GEOMETRY 1                                  --
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


with ada.strings;					use ada.strings;
with ada.strings.fixed;				use ada.strings.fixed;
with ada.strings.unbounded;			use ada.strings.unbounded;
with ada.characters.latin_1;
with ada.characters.handling;		use ada.characters.handling;

with et_exceptions;					use et_exceptions;

with et_coordinates_formatting;		use et_coordinates_formatting;



package body et_geometry_1 is

	
	function "=" (
		left, right : in type_float) 
		return boolean 
	is begin
		-- put_line ("test equality");
		
		if abs (left - right) <= accuracy then
			return true;
		else
			return false;
		end if;
	end "=";

	
	--function ">=" (left, right : in type_float) return boolean is begin
		--if left = right then
			--return true;
		--elsif left > right then
			--return true;
		--else
			--return false;
		--end if;
	--end ">=";


	--function "<=" (left, right : in type_float) return boolean is begin
		--if left = right then
			--return true;
		--elsif left < right then
			--return true;
		--else
			--return false;
		--end if;
	--end "<=";

	
	
	function get_average (
		f1, f2 : in type_float)
		return type_float
	is begin
		return (f1 + f2) * 0.5;
	end get_average;

	
	--procedure round (
		--f : in out type_float;
		--a : in type_rounding_accuracy)
	--is 
		--base : constant type_float := 10.0;
	--begin
		----put_line ("given   :" & type_float'image (f));
		----put_line ("accuracy:" & positive'image (a));

		--f := type_float'rounding (f * base**a) * base**(-a); 
		
		----put_line ("rounded :" & type_float'image (f));
		----new_line;
	--end round;


	--function round (
		--f : in type_float;
		--a : in type_rounding_accuracy)
		--return type_float
	--is 
		--base : constant type_float := 10.0;
	--begin
		--return type_float'rounding (f * base**a) * base**(-a); 
	--end round;


	function mil_to_distance (mil : in string) return type_float is
		distance_mil : type_float := type_float'value (mil);
	begin
		return distance_mil * (25.4 * 0.001);
	end mil_to_distance;
	

	function distance_to_mil (d : in type_float) return string is
		scratch : type_float;
	begin
		scratch := d * 1000.00 / 25.4;
		return to_string (scratch);
	end;

	
	
	function to_string (f : in type_float) return string is begin
		return type_float'image (f);
	end;

	
	function to_float (s : in string) return type_float is begin
		return type_float'value (s);
	end to_float;
	

	function to_angle (a : in string) return type_angle is begin
		return type_angle'value (a);
	end to_angle;

	
	function to_radians (degrees : in type_angle) return type_float is
		use ada.numerics;
	begin
		return (pi * type_float (degrees)) / (units_per_cycle * 0.5);
	end to_radians;

	
	function to_degrees (
		radians : in type_float)
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
				n_candidate : type_float renames element (c);
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
				n_candidate : type_float renames element (c);
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

	


	function sgn (x : type_float) return type_float is begin
		if x >= 0.0 then
			return 1.0;
		else
			return -1.0;
		end if;
	end sgn;


	function get_greatest (
		left, right : in type_float)
		return type_float
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
		left, right : in type_float)
		return type_float
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
		return type_float
	is begin
		return type_float'value (df);
	end to_distance;

	
	--function to_distance (f : in type_float)
		--return type_distance 
	--is
		--use pac_distance_io;
		
		--d1 : type_distance;
		--d2 : type_float;

		--f1 : constant type_float := 5.0 * type_float (type_distance'small);
	--begin
		--d1 := type_distance (f);
		
		--d2 := 10.0 * abs (f - type_float (d1));
		
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
				------put_line (type_float'image (f) & " " & natural'image (r'length));
				------ CS: IMPROVEMENT REQUIRED !!!
				----put (to => r, item => f, aft => type_distance'scale, exp => 0);
				----return type_distance'value (r);
			----end;
		----end if;


		--exception when event: others =>
			--put_line ("f: " & to_string (f));
			--raise;

	--end to_distance;
	

	--function to_rotation (f : in type_float)
		--return type_rotation 
	--is
		--use pac_distance_io;

		--d1 : type_rotation := type_rotation (f);
		--d2 : type_float;

		--f1 : constant type_float := 5.0 * type_float (type_rotation'small);

	--begin
		--d2 := 10.0 * abs (f - type_float (d1));
		
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
		x, y : in type_float)
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
	end add;




	procedure add (
		left	: in out type_angle;
		right	: in type_angle)
	is begin
		left := add (left, right);
	end add;
	



	

	function to_angle_positive (
		rotation : in type_angle)
		return type_angle_positive
	is 
		result : type_angle_positive;
	begin
		-- IMPORTANT: The redefined equality test
		-- must come first:
		if rotation = 0.0 then
			result := 0.0;

		elsif rotation = 360.0 then
			result := 0.0;
			
		elsif rotation < 0.0 then
			result := 360.0 + rotation;
			-- example: 360 + (-90) = 270
		else
			-- rotation > 0.0
			result := rotation;
		end if;

		return result;

		exception 
			when others =>
				put_line ("ERROR: rotation " & to_string (rotation));
				raise;
			
	end to_angle_positive;


	
	function to_string (
		distance : in type_distance_polar)
		return string
	is begin
		return ("abs:" & to_string (distance.absolute) 
			& " / angle:" & to_string (distance.angle));
	end to_string;
	
	
	function to_polar (
		absolute	: in type_float_positive;
		angle		: in type_angle)
		return type_distance_polar
	is begin
		return (absolute, angle);
	end to_polar;

	
	procedure set_absolute (
		distance : in out type_distance_polar;
		absolute : in type_float_positive)
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
		return type_float_positive
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


	

	function get_lower_left (
		left, right : in type_vector)
		return boolean 
	is begin
		--new_line;
		--put_line ("left: " & to_string (left));
		--put_line ("right:" & to_string (right));

		if left.x < right.x then
			if left.y < right.y then
				return true;
			else
				return false;
			end if;
		else
			return false;			
		end if;
	end get_lower_left;



	
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
		x : in type_float;
		y : in type_float;
		z : in type_float := 0.0)
		return type_vector
	is begin
		return (x, y, z);
	end set;


	
	function get_x (
		v	: in type_vector)
		return type_float is
	begin
		return v.x;
	end get_x;
	

	function get_y (
		v	: in type_vector)
		return type_float is
	begin
		return v.y;
	end get_y;

	
	function get_z (
		v	: in type_vector)
		return type_float is
	begin
		return v.z;
	end get_z;


	function get_absolute (
		vector	: in type_vector)
		return type_float
	is begin
		return
			sqrt (
				vector.x * vector.x + 
				vector.y * vector.y +
				vector.z * vector.z);

	end get_absolute;


	function get_sum_of_squared_components (
		vector	: in type_vector)
		return type_float
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
		s	: in type_float)
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
		return type_float
	is begin
		return (a.x * b.x  +  a.y * b.y  +  a.z * b.z);
	end dot_product;

	
	function mixed_product (
		a, b, c	: in type_vector)
		return type_float
	is
		cp : type_vector;
	begin
		cp := cross_product (b, c);
		return dot_product (a, cp);
	end mixed_product;		

	
	function divide (
		a, b	: in type_vector)
		return type_float
	is
		lambda : type_float;
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
		axis	: in type_mirror)
	is begin
		case axis is
			when MIRROR_ALONG_X_AXIS =>
				v.y := v.y * (-1.0);
				
			when MIRROR_ALONG_y_AXIS =>
				v.x := v.x * (-1.0);

			when MIRROR_NO =>
				null;
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


	procedure iterate (
		vectors	: in pac_vectors.list;
		process	: not null access procedure (position : in pac_vectors.cursor);
		proceed	: not null access boolean)
	is
		c : pac_vectors.cursor := vectors.first;
	begin
		while c /= pac_vectors.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;


		

	
	procedure put_vectors (
		vectors	: in pac_vectors.list)
	is

		procedure query_vector (c : in pac_vectors.cursor) is begin
			put_line (to_string (element (c)));
		end;
		
	begin
		vectors.iterate (query_vector'access);
	end put_vectors;


	
	function to_list (
		vectors : in type_vector_array)
		return pac_vectors.list
	is
		result : pac_vectors.list;
	begin
		for i in vectors'first .. vectors'last loop
			result.append (vectors (i));			
		end loop;
		
		return result;
	end to_list;


	procedure scale (
		vectors	: in out pac_vectors.list;
		factor	: in type_float_positive)
	is
		c : pac_vectors.cursor := vectors.first;
		
		procedure do_it (v : in out type_vector) is begin
			v := scale (v, factor);
		end do_it;
		
	begin
		while c /= pac_vectors.no_element loop
			vectors.update_element (c, do_it'access);
			next (c);
		end loop;
	end scale;

	


	
	
	procedure move_by (
		vectors	: in out pac_vectors.list;
		offset	: in type_offset)
	is 
	
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
	

	procedure mirror_vectors (
		vectors	: in out pac_vectors.list;
		axis	: in type_mirror)
	is
		c : pac_vectors.cursor := vectors.first;

		procedure do_it (v : in out type_vector) is begin
			mirror (v, axis);
		end do_it;
		
	begin
		while c /= pac_vectors.no_element loop
			vectors.update_element (c, do_it'access);
			next (c);
		end loop;
	end mirror_vectors;

	
	
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


	procedure remove (
		vectors_1 : in out pac_vectors.list; -- primary list
		vectors_2 : in pac_vectors.list) -- to be removed
	is

		procedure query_vector_to_be_removed (c2 : in pac_vectors.cursor) is
			-- locate vector-to-be-removed in primary list:
			c1 : pac_vectors.cursor := vectors_1.find (element (c2));
		begin
			-- If vector-to-be-removed exists in primary list,
			-- then delete it in the primary list:
			if c1 /= pac_vectors.no_element then
				vectors_1.delete (c1);
			end if;
		end query_vector_to_be_removed;
		
	begin
		-- iterate list of vectors-to-be-removed:
		vectors_2.iterate (query_vector_to_be_removed'access);
	end remove;

	
	procedure sort_by_distance (
		vectors		: in out pac_vectors.list;
		reference	: in type_vector)
	is
		type type_item is record
			-- We will be sorting vectors by their distance to
			-- the reference point:
			vector		: type_vector;
			distance	: type_float_positive;
		end record;

		
		function "<" (left, right : in type_item) return boolean is begin
			if left.distance < right.distance then
				return true;
			else
				return false;
			end if;
		end;
	
		package pac_items is new doubly_linked_lists (type_item);
		use pac_items;
		
		package pac_sorting is new pac_items.generic_sorting;
		use pac_sorting;

		items : pac_items.list;
		
		
		procedure query_vector (v : in pac_vectors.cursor) is
			d : constant type_float_positive := 
				get_distance_total (reference, element (v));
		begin
			items.append (new_item => (
				vector		=> element (v),
				distance	=> d));

		end query_vector;


		procedure query_item (i : in pac_items.cursor) is begin
			vectors.append (element (i).vector);
		end query_item;
		
	begin
		-- Collect vectors and their distance to the reference
		-- in list "items":
		vectors.iterate (query_vector'access);

		-- Sort items by the distance to the reference point:
		sort (items);

		-- The old vectors are no longer required:
		vectors.clear;
		-- New vectors will be appended here.
		
		-- Traverse items and append them one by one to the
		-- list of vectors:
		items.iterate (query_item'access);
	end sort_by_distance;


	function get_lowest_left (
		vectors		: in pac_vectors.list)
		return type_vector
	is
		result : type_vector := top_right;		

		procedure query_vector (v : in pac_vectors.cursor) is
			vector : type_vector renames element (v);
		begin
			if get_lower_left (vector, result) then
				result := vector;
			end if;
		end query_vector;
		
	begin
		vectors.iterate (query_vector'access);
		return result;
	end get_lowest_left;
	
	
	function get_distance_total (
		v1 : in type_vector;
		v2 : in type_vector)
		return type_float_positive
	is 
		dx : constant type_float := abs (v2.x - v1.x);
		dy : constant type_float := abs (v2.y - v1.y);
	begin
		return sqrt (dx ** 2.0 + dy ** 2.0);
	end get_distance_total;


	function get_distance (
		v1, v2 : in type_vector)
		return type_distance_polar
	is
		result : type_distance_polar;

		dx : constant type_float := v2.x - v1.x;
		dy : constant type_float := v2.y - v1.y;

		abs_dx : constant type_float := abs (dx);
		abs_dy : constant type_float := abs (dy);
	begin
		--put_line ("dx " & type_float'image (abs_dx));
		--put_line ("dy " & type_float'image (abs_dy));
		
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
		angle_out			: type_float; -- degrees
		distance_to_origin	: type_float; -- unit is mm
		scratch				: type_float;
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
		distance	: in type_float)
	is
		delta_x, delta_y : type_float := 0.0;
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
		distance	: in type_float) -- CS type_float_positive ?
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
		v.x := cos (type_float (ray.direction), units_per_cycle);

		-- y = sin (direction) * 1
		v.y := sin (type_float (ray.direction), units_per_cycle);

		v.z := 0.0; -- we are in a 2D world
		
		return v;
	end direction_vector;


	

-- LINE VECTOR

	function to_string (
		lv : in type_line_vector)
		return string
	is begin
		return "line vector start: " & to_string (lv.v_start) 
			& " direction: " & to_string (lv.v_direction)
			& " angle: " & to_string (get_angle (lv));
	end to_string;


	function move_by (
		lv		: in type_line_vector;
		offset	: in type_offset)
		return type_line_vector
	is 
		result : type_line_vector := lv;
	begin
		result.v_start.x := result.v_start.x + type_float (offset.x);
		result.v_start.y := result.v_start.y + type_float (offset.y);

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
				y		=> type_float (line.v_direction.y), 
				x		=> type_float (line.v_direction.x), 
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



	function get_angle_of_itersection (
		line_1, line_2	: in type_line_vector)
		return type_angle
	is 
		a, b, c : type_float;
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
		return type_line_vector_intersection
	is 
		-- scratch variables:
		a, b, c, d, e, f, g : type_float;
		lambda : type_float;

		-- location vector and angle of intersection to be returned:			
		i : type_vector;

		
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
			a, b, distance : type_float;
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

					i := add (line_2.v_start, scale (line_2.v_direction, lambda));
				else
					a := line_2.v_start.y;
					b := line_1.v_start.x * line_2.v_direction.y / line_2.v_direction.x;
					c := line_2.v_start.x * line_2.v_direction.y / line_2.v_direction.x;
					d := line_1.v_start.y;
					e := line_1.v_direction.y;
					f := line_1.v_direction.x * line_2.v_direction.y / line_2.v_direction.x;
					g := 1.0 / (e - f);

					lambda := (a + b - c - d) * g;

					i := add (line_1.v_start, scale (line_1.v_direction, lambda));
				end if;

				--i.angle := get_angle_of_itersection (line_1, line_2);

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
		return type_float_positive
	is begin
		return boundaries.greatest_y - boundaries.smallest_y;
	end get_height;

	
	function get_width (boundaries : in type_boundaries)
		return type_float_positive
	is begin
		return boundaries.greatest_x - boundaries.smallest_x;
	end get_width;


	function get_left (boundaries : in type_boundaries)
		return type_float
	is begin
		return boundaries.smallest_x;
	end get_left;

	
	function get_right (boundaries : in type_boundaries)
		return type_float
	is begin
		return boundaries.greatest_x;
	end get_right;

	
	function get_bottom (boundaries : in type_boundaries)
		return type_float
	is begin
		return boundaries.smallest_y;
	end get_bottom;


	function get_top (boundaries : in type_boundaries)
		return type_float
	is begin
		return boundaries.greatest_y;
	end get_top;

	
	
	function overlap (
		b1 : in type_boundaries;
		b2 : in type_boundaries)
		return boolean
	is
		-- The the smallest and greatest x-value occupied by the two boundaries:
		left   : constant type_float := get_smallest (b1.smallest_x, b2.smallest_x);
		right  : constant type_float := get_greatest (b1.greatest_x, b2.greatest_x);

		-- The the smallest and greatest y-value occupied by the two boundaries:
		bottom : constant type_float := get_smallest (b1.smallest_y, b2.smallest_y);
		top    : constant type_float := get_greatest (b1.greatest_y, b2.greatest_y);
		
		span_h, span_v : type_float_positive;

		sum_h, sum_v : type_float_positive;

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
		line : in type_line_fine)
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


	
	function get_nearest (
		line	: in type_line_fine;
		point	: in type_vector)
		return type_vector
	is
		distance_to_start, distance_to_end : type_float_positive;
	begin
		distance_to_start := get_distance_total (point, line.start_point);
		distance_to_end   := get_distance_total (point, line.end_point);

		if distance_to_start < distance_to_end then
			return line.start_point;
		else
			return line.end_point;
		end if;
	end get_nearest;
	


	
	function get_length (
		line : in type_line_fine)
		return type_float_positive
	is begin
		return get_distance_total (line.start_point, line.end_point);
	end get_length;
	

	function to_string (
		line	: in type_line_fine)
		return string
	is begin
		return 
			"line: S:" & to_string (line.start_point) 
			& " / E:" & to_string (line.end_point);
	end to_string;
	

	function make_line (
		start_point, end_point : in type_vector)
		return type_line_fine
	is begin
		return (start_point, end_point);
	end make_line;


	procedure scale (
		line	: in out type_line_fine;
		factor	: in type_float_positive)
	is begin
		line.start_point := scale (line.start_point, factor);
		line.end_point   := scale (line.end_point,   factor);
	end scale;

	
	
	procedure move_by (
		line	: in out type_line_fine;
		offset	: in type_offset)
	is begin
		move_by (line.start_point, offset);
		move_by (line.end_point, offset);
	end move_by;


	procedure rotate_by (
		line	: in out type_line_fine;
		offset	: in type_angle)
	is begin
		rotate_by (line.start_point, offset);
		rotate_by (line.end_point, offset);
	end rotate_by;
	
	
	procedure mirror_line (
		line	: in out type_line_fine;
		axis	: in type_mirror)
	is begin
		mirror (line.start_point, axis);
		mirror (line.end_point, axis);
	end mirror_line;

	
	function reverse_line (
		line	: in type_line_fine)
		return type_line_fine
	is begin
		return (line.end_point, line.start_point);
	end reverse_line;


	procedure reverse_line (
		line	: in out type_line_fine)
	is begin
		line := reverse_line (line);
	end reverse_line;
		

	function get_center (
		line : in type_line_fine)
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
		line : in type_line_fine)
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
		right, left : in type_line_fine)
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
		line : in type_line_fine)
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
		line		: in out type_line_fine;
		direction	: in type_angle;
		distance	: in type_float_positive)
	is begin
		move_by (line.start_point, direction, distance);
		move_by (line.end_point, direction, distance);
	end move_by;



	
	function move_by (
		line		: in type_line_fine;
		direction	: in type_angle;
		distance	: in type_float_positive)
		return type_line_fine
	is
		result : type_line_fine;
	begin
		result.start_point := move_by (line.start_point, direction, distance);
		result.end_point := move_by (line.end_point, direction, distance);		
		return result;
	end move_by;


	
	
	function get_intersection (
		line_vector : in type_line_vector;
		line		: in type_line_fine;
		debug		: in boolean := false)
		return type_line_vector_intersection
	is
		i : constant type_line_vector_intersection := get_intersection (
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
				if on_line (i.intersection, line, debug) then
					return i;
				else
					return (status => NOT_EXISTENT);
				end if;

			when others =>		
				return i;
		end case;
		
	end get_intersection;
	


	
	function get_intersection (
		ray			: in type_ray;
		line		: in type_line_fine;
		debug		: in boolean := false)
		return type_line_vector_intersection
	is
		-- In this function we must test angles for equality.
		-- The direction of the ray must be positive in any case:
		ray_direction_positive : constant type_angle_positive := 
			to_angle_positive (ray.direction);
		
		line_vector : constant type_line_vector := to_line_vector (ray);

		I : constant type_line_vector_intersection := 
			get_intersection (line_vector, line);
		
		dp : type_distance_polar;
	begin
		case I.status is
			-- If line_vector and line intersect:
			when EXISTS =>
				dp := get_distance (ray.start_point, I.intersection);

				-- The direction from ray start to intersection must be positive:
				dp.angle := to_angle_positive (dp.angle);

				--put_line ("dp " & to_string (dp.angle) & " " & to_string (ray_direction_positive));
				-- Compare directions. The intersection must be in
				-- the direction of travel of the ray. If it lies in opposide
				-- direction then it must be ignored:
				if dp.angle = ray_direction_positive then -- CS consider rounding errors ?
					--put_line ("intersection valid");					
					return I;
				else
					--put_line ("intersection ignored");
					return (status => NOT_EXISTENT);
				end if;

			-- line_vector and line do no intersect:
			when others => 
				return (status => NOT_EXISTENT);
		end case;

	end get_intersection;


	

	
	function get_intersection (
		line_1 : in type_line_fine;
		line_2 : in type_line_fine)
		return type_line_vector_intersection
	is		
		lv_1 : constant type_line_vector := to_line_vector (line_1);
		lv_2 : constant type_line_vector := to_line_vector (line_2);

		-- Get the intersection of the two line vectors:
		I : constant type_line_vector_intersection := get_intersection (lv_1, lv_2);
	begin
		--put_line ("get intersection");
		--put_line ("line 1: " & to_string (line_1));
		--put_line ("line 2: " & to_string (line_2));

		case I.status is
			when OVERLAP =>
				return (status => OVERLAP);
			
			when EXISTS =>
				if on_line (I.intersection, line_1) and
				   on_line (I.intersection, line_2) 
				then
					return (EXISTS, I.intersection);
				else
					return (status => NOT_EXISTENT);
				end if;

			when NOT_EXISTENT =>
				return (status => NOT_EXISTENT);
		end case;
	end get_intersection;

	


	
	function lines_overlap (
		line_1, line_2 : in type_line_fine)
		return boolean
	is
		I2L : constant type_line_vector_intersection :=
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
		arc : in type_arc_fine)
		return string 
	is begin
		return "arc: "
			& "C:" & to_string (arc.center) 
			& " / S:" & to_string (arc.start_point) 
			& " / E:" & to_string (arc.end_point)
			& " / D: " & to_string (arc.direction);
	end to_string;


	
	function get_radius_start (
		arc : in type_arc_fine) 
		return type_float_positive 
	is begin
		return get_distance_total (arc.center, arc.start_point);
	end get_radius_start;

	
	
	function get_radius_end (
		arc : in type_arc_fine)
		return type_float_positive
	is begin
		return get_distance_total (arc.center, arc.end_point);
	end get_radius_end;

	

	function reverse_arc (
		arc : in type_arc_fine) 
		return type_arc_fine
	is
		result : type_arc_fine := arc;
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
		arc : in out type_arc_fine) 
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
		arc: in type_arc_fine) 
		return type_arc_fine
	is begin
		case arc.direction is
			when CW  => return reverse_arc (arc);					
			when CCW => return arc;
		end case;
	end normalize_arc;



	
	function zero_length (
		arc : in type_arc_fine) 
		return boolean 
	is begin
		if arc.start_point = arc.end_point then
			return true;
		else
			return false;
		end if;
	end zero_length;


	
	function get_span (
		arc	: type_arc_fine)
		return type_angle
	is
		arc_angles : constant type_arc_angles := to_arc_angles (arc);
	begin
		return get_span (arc_angles);
	end get_span;




	function on_arc (
		arc		: in type_arc_fine;
		vector	: in type_vector)
		return boolean 
	is
		-- debug : boolean := true;
		debug : boolean := false;
		
		-- The angle of the given point relative to the
		-- center of the given arc:
		P : type_angle_positive;

		-- A representation of the given arc in angles:
		arc_angles : constant type_arc_angles := normalize_arc (to_arc_angles (arc));

		
		-- make the angles of the arc positive:
		S : type_angle_positive := to_angle_positive (arc_angles.angle_start);
		E : type_angle_positive := to_angle_positive (arc_angles.angle_end);

		T : type_angle_positive;
		
		distance_center_to_point : constant type_float :=
			get_distance_total (arc.center, vector);

		d1 : type_float;
		
	begin
		if debug then
			put_line ("on_arc");
			put_line (" " & to_string (arc));
			put_line (" " & to_string (arc_angles));
			put_line (" P " & to_string (vector));
			put_line (" distance center to point " & to_string (distance_center_to_point));
		end if;
		
		-- First test whether the given point is on the circumfence of
		-- a virtual circle. The circle has the same radius as the arc:

		d1 := distance_center_to_point - arc_angles.radius;

		if debug then
			put_line (" d1 " & to_string (d1));
		end if;

		
		if d1 = 0.0 then

			-- Point is on circumfence of virtual circle.
			if debug then
				put_line (" on circumfence of virtual circle");
				put_line (" S " & to_string (S));
				put_line (" E " & to_string (E));
			end if;

			
			-- Compute the angle of the point relative to the center
			-- of the given arc:
			P := to_angle_positive (get_angle (get_distance (arc.center, vector)));

			-- put_line ("DP " & to_string (get_distance (arc.center, vector)));
			
			if debug then
				put_line (" P " & to_string (P));
			end if;
			
			-- The angle P of the point must be between start S and end point E
			-- of the arc to be considered as "on the arc".
			-- Special problem: The arc may run across the ZDM ("zero degree mark").
			--  In that case the start and end angle and the point angle must first be
			--  rotated so that the arc no longer crossed the ZDM.

			if S >= E then -- arc crosses the ZDM
				if debug then
					put_line (" arc crosses the ZDM -> offset requred");
				end if;

				T := 360.0 - S;
				
				if debug then
					put_line (" T:" & to_string (T));
				end if;
				
				S := 0.0;
				E := E + T;
				P := add (P, T);

				if debug then
					put_line (" S " & to_string (S));
					put_line (" E " & to_string (E));
					put_line (" P " & to_string (P));
				end if;
			end if;


			
			if P >= S and P <= E then
				if debug then
					put_line (" P is on arc");
				end if;
				return true;
			else
				if debug then
					put_line (" P is NOT on arc");
				end if;
				return false;
			end if;

			
		else
			if debug then
				put_line (" P is NOT on arc (distance differs from radius)");
			end if;

			return false; 
		end if;
	end on_arc;

	

	

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



	function normalize_arc (
		arc: in type_arc_angles) 
		return type_arc_angles
	is
		result : type_arc_angles;
	begin
		case arc.direction is
			when CW =>
				result.center 		:= arc.center;
				result.radius 		:= arc.radius;
				result.angle_start	:= to_angle_positive (arc.angle_end);
				result.angle_end	:= to_angle_positive (arc.angle_start);
				result.direction	:= CCW;

			when CCW =>
				result.center 		:= arc.center;
				result.radius 		:= arc.radius;
				result.angle_start	:= to_angle_positive (arc.angle_start);
				result.angle_end	:= to_angle_positive (arc.angle_end);
				result.direction	:= arc.direction;

		end case;

		return result;
	end normalize_arc;




	function rotate (
		arc		: in type_arc_angles;
		angle	: in type_angle)
		return type_arc_angles
	is
		A : type_arc_angles := arc;
	begin
		add (A.angle_start, angle);
		add (A.angle_end,   angle);

		return A;
	end rotate;
	
	
	
	
	function get_span (
		arc		: in type_arc_angles;
		full	: in boolean := true)
		return type_angle
	is
		A : type_angle; -- span to be returned

		N : type_arc_angles := normalize_arc (arc);
		
		S : type_angle_positive renames N.angle_start;
		E : type_angle_positive renames N.angle_end;
	begin
		-- put_line ("get_span");
		-- put_line ("S: " & to_string (S));
		-- put_line ("E: " & to_string (E));

		if E > S then
			A := E - S;
			
		elsif E < S then
			A := (360.0 - S) + E;
			-- test 1: S=90, E=0 => S=270 

		else -- E=S
			if full then
				A := 360.0;
			else
				A := 0.0;
			end if;
		end if;

		return A;
	end get_span;




	
	procedure move_to (
		arc			: in out type_arc_fine;
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
		arc			: in type_arc_fine;
		position	: in type_vector)
		return type_arc_fine
	is
		result : type_arc_fine := arc;
	begin
		move_to (result, position);
		return result;
	end move_to;



	
	
	function to_arc_angles (
		arc					: in type_arc_fine;
		allow_full_circle	: in boolean := true) 
		return type_arc_angles 
	is
		-- The angles may be negative. For example instead of 270 degree
		-- the angle can be -90 degree.
		result : type_arc_angles;
					
		-- Take a copy of the given arc in arc_tmp.
		arc_tmp : type_arc_fine := arc;

		procedure compute_end_point is begin
			if get_x (arc_tmp.end_point) = 0.0 and get_y (arc_tmp.end_point) = 0.0 then
				result.angle_end := 0.0;
			else
				result.angle_end := arctan (
						y => get_y (arc_tmp.end_point),
						x => get_x (arc_tmp.end_point),
						cycle => units_per_cycle);
			end if;
		end compute_end_point;
	
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

		-- compute start point:
		if get_x (arc_tmp.start_point) = 0.0 and get_y (arc_tmp.start_point) = 0.0 then
			result.angle_start := 0.0;
		else
			result.angle_start := arctan (
					y => get_y (arc_tmp.start_point),
					x => get_x (arc_tmp.start_point), 
					cycle => units_per_cycle);
		end if;


		-- compute end point depending on argument allow_full_circle:
		if allow_full_circle then -- default
			if arc_tmp.start_point = arc_tmp.end_point then
				result.angle_end := result.angle_start + 360.0; -- span will be 360 degrees
			else		
				compute_end_point;
			end if;
		else
			if arc_tmp.start_point = arc_tmp.end_point then
				result.angle_end := result.angle_start; -- span will be zero
			else		
				compute_end_point;
			end if;
		end if;
		
		
		-- direction is not changed:
		result.direction := arc.direction;
		
		return result;
	end to_arc_angles;



	

	function to_arc (
		arc : in type_arc_angles) 
		return type_arc_fine 
	is
		result : type_arc_fine;
		x, y : type_float;
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





	procedure reverse_arc_segments (
		segments : in out type_arc_segments)
	is
		scratch : type_arc_segments := segments;

		b : positive := segments'last + 1;
	begin
		-- Loop forward through the elements of the input:
		for a in segments'first .. segments'last loop

			-- Reverse the candidate arc:
			reverse_arc (segments (a));

			-- Store the reversed arc in the output
			-- array in reverse order:
			scratch (b - a) := segments (a);
		end loop;

		-- Overwrite the input array with the reversed arcs:
		segments := scratch;
	end reverse_arc_segments;






	function get_nearest (
		segments	: in type_arc_segments;
		point		: in type_vector)
		return positive
	is
		debug : boolean := false;
		
		result : positive := 1;

		-- A temporarly storage place for the
		-- distance between a candidate segment and the point
		-- in polar form:
		p : type_distance_polar;
		
		-- A temporarly storage place for the absolute
		-- distance between a candidate segment and the point:
		d2 : type_float_positive;

		-- The storage place for the smallest
		-- distance found. Will be updated if a smaller
		-- distance has been detected:
		d1 : type_float_positive := type_float_positive'last;
	begin
		if debug then
			put_line ("get_nearest");
		end if;

		
		for i in segments'first .. segments'last loop
			if debug then
				put_line (" segment " & to_string (segments (i)));	
			end if;
			
			p := get_shortest_distance (segments (i), point);

			-- We are interested in the total distance:
			d2 := get_absolute (p);

			if debug then
				put_line (" distance " & to_string (d2));	
			end if;
			
			-- If the total distance is less than the latest
			-- shortest distance then update the shortest distance
			-- by the current total distance:
			if d2 < d1 then
				-- update:
				d1 := d2;

				-- store index of arc segment:
				result := i;
			end if;
		end loop;
		
		return result;
	end get_nearest;


	
	
	

	function split_arc (
		arc		: in type_arc_fine;
		count	: in positive)
		return type_arc_segments
	is
		-- debug : boolean := true;
		debug : boolean := false;
		
		subtype type_arcs is type_arc_segments (1 .. count);
		result : type_arcs;

		radius : type_float_positive := get_radius_start (arc);
		center : type_vector := arc.center;
		
		norm : type_arc_fine;
		angles : type_arc_angles;
		span : type_angle;
		fragment_angle : type_angle;

		S, E : type_angle;
		O : type_angle_positive;
	begin
		norm := normalize_arc (arc);
		if debug then
			put_line ("norm   : " & to_string (norm));
		end if;
		
		angles := to_arc_angles (norm);
		if debug then
			put_line ("angles : " & to_string (angles));
		end if;
		
		span := get_span (arc);
		if debug then
			put_line ("span   : " & to_string (span));
		end if;
		
		fragment_angle := span / type_float_positive (count);
		if debug then
			put_line ("frag   : " & to_string (fragment_angle));
		end if;
		
		for i in 1 .. count loop
			if debug then
				put_line ("i      : " & positive'image (i));
			end if;
			
			O := type_float_positive (i - 1) * fragment_angle;			

			if debug then
				put_line ("O      : " & to_string (O));
			end if;
			
			S := add (angles.angle_start, O);
			E := add (S, fragment_angle);

			if debug then
				put_line ("S      : " & to_string (S));
				put_line ("E      : " & to_string (E));
			end if;

			
			result (i) := to_arc ((
				center		=> center,
				radius		=> radius,
				angle_start	=> S,
				angle_end	=> E,
				direction	=> CCW));
					 
		end loop;


		-- If the given arc direction was CW then
		-- the arc segments must be reversed:
		if arc.direction = CW then
			reverse_arc_segments (result);
		end if;
		
		return result;
	end split_arc;
	




	function get_tangent_angle (p : in type_vector) 
		return type_tangent_angle_circle
	is
		a : type_angle := get_angle (get_distance (set (0.0, 0.0), p));
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




	
	

	function to_string (
		circle	: in type_circle_fine)
		return string
	is begin
		return "circle C: " & to_string (circle.center)
			& " R: " & to_string (circle.radius);
	end to_string;




	function on_circle (
		circle		: in type_circle_fine;
		point		: in type_vector)
		return boolean 
	is
		-- the distance from center to point:
		DCP: constant type_float_positive := 
			get_distance_total (point, circle.center);
	begin
		if DCP = circle.radius then 
			return true;
		else
			return false; 
		end if;
	end on_circle;






	function order_intersections (
		-- The start point of the line that intersects the circle.
		-- The start point must be outside the circle and will
		-- be passed through to the result unchanged.
		start_point		: in type_vector;

		intersections	: in type_intersection_of_line_and_circle)
		return type_ordered_line_circle_intersections
	is
		result : type_ordered_line_circle_intersections := 
			(start_point => start_point, others => <>); -- pass start point through

		i : constant type_intersection_of_line_and_circle (TWO_EXIST) := intersections;

		d1, d2 : type_float;
	begin
		-- the distance from start point to intersection point 1:
		d1 := get_distance_total (start_point, i.intersection_1);

		-- the distance from start point to intersection point 2:
		d2 := get_distance_total (start_point, i.intersection_2);

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






	


	function get_intersection (
		circle	: in type_circle_fine;
		line	: in type_line_vector)
		return type_intersection_of_line_and_circle
	is
		--debug : boolean := true;
		debug : boolean := false;
		
		-- This function bases on the approach by
		-- Weisstein, Eric W. "Circle-Line Intersection." 
		-- From MathWorld--A Wolfram Web Resource. 
		-- <https://mathworld.wolfram.com/Circle-LineIntersection.html>.

		-- The appoach assumes the circle center at 0/0.
		-- So we must first move the line by
		-- the given center of the circle. The intersections,
		-- if any exist, must in the end be moved back by this offset:
		offset : constant type_offset := to_offset (circle.center);

		-- The circle radius is:
		r : type_float_positive renames circle.radius;
		
		-- The line starts here:
		x1, y1 : type_float;

		-- The line ends here:
		x2, y2 : type_float;
		
		x, y, dx, dy, dr, DI : type_float;

		-- scratch variables:
		a, b, c, d	: type_float;

		s : type_intersection_status_of_line_and_circle;
		intersection_1, intersection_2 : type_vector;

		
		procedure compute_incidence is 
			line_moved	: type_line_vector;
			v_end		: type_vector;
		begin
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

			-- Compute the discriminant
			DI := x1 * y2 - x2 * y1;
			
			b := dr ** 2;
			a := r ** 2;
			c := DI ** 2;

			-- the incidence finally:
			d := a * b - c;
		end compute_incidence;
		
		
	begin
		if debug then
			put_line ("get_intersection circle/line");
			put_line (to_string (line));
			put_line (to_string (circle));
		end if;

		compute_incidence;

		if debug then
			put_line (" incidence d: " & to_string (d));
		end if;

					
		-- CASE 1: 
		-- If the incidence is zero, then
		-- there is one intersection -> we have a tangent.
		-- NOTE: The follwing equality test is redefined
		-- and MUST be executed before "<" and ">":
		if d = 0.0 then
			if debug then
				put_line (" one intersection -> a tangent");
			end if;
			
			s := ONE_EXISTS; -- tangent

			x := (DI * dy) / b;
			y := (-DI * dx) / b;

			intersection_1 := set (x, y);

			-- Move computed intersection back by offset
			-- (Which is the center of the given circle):
			move_by (intersection_1, offset);

			if debug then
				put_line (" at " & to_string (intersection_1));
			end if;
			
			return (ONE_EXISTS, intersection_1, TANGENT);


		-- CASE 2: 
		-- If the incidence is less than zero, then
		-- there is no intersection at all:
		elsif d < 0.0 then
			if debug then
				put_line (" no intersection");
			end if;
			
			s := NONE_EXIST;
			
			return (status => NONE_EXIST);

			
		-- CASE 3: 
		-- If the incidence is greater than zero, then
		-- there are two intersections -> we have a secant:			
		else
			if debug then
				put_line (" two intersections -> a secant");
			end if;
			
			s := TWO_EXIST; -- secant

			-- COMPUTE 1ST INTERSECTION:
			x := ( DI * dy + sgn (dy) * dx * sqrt (d)) / b;
			y := (-DI * dx + abs (dy) * sqrt (d))      / b;

			-- Compose the point of intersection 1:
			intersection_1 := set (x, y);
		
			-- Move computed intersection 1 back by offset
			-- (Which is the center of the given circle):
			move_by (intersection_1, offset);
			
			-- COMPUTE 2ND INTERSECTION:				
			x := ( DI * dy - sgn (dy) * dx * sqrt (d)) / b;
			y := (-DI * dx - abs (dy) * sqrt (d))      / b;
					
			-- Compose the point of intersection 2:
			intersection_2 := set (x, y, 0.0);
			
			-- Move computed intersection 2 back by offset
			-- (Which is the center of the given circle):
			move_by (intersection_2, offset);				

			if debug then
				put_line (" at " & to_string (intersection_1));
				put_line (" at " & to_string (intersection_2));
			end if;
			
			return (TWO_EXIST, intersection_1, intersection_2);			
		end if;
	end get_intersection;





	function get_intersection (
		arc		: in type_arc_fine;
		line	: in type_line_vector)
		return type_intersection_of_line_and_circle
	is
		--debug : boolean := true;
		debug : boolean := false;
		

		function do_it return type_intersection_of_line_and_circle is
			-- We assume the arc is a virtual circle and compute the
			-- intersections of the line with the virtual circle.
			-- Build a virtual circle from the given arc:
			vc : constant type_circle_fine := (
					center => arc.center, 
					radius => get_radius_start (arc));
			
			-- Compute the intersections of the line with the virtual circle:
			vi : constant type_intersection_of_line_and_circle := 
				get_intersection (vc, line);
		begin
			if debug then
				put_line ("virtual " & to_string (vc));
			end if;

			case vi.status is
				when NONE_EXIST => 
					-- line does not meet the virtual circle
					-- and does not meet the given arc either.
					
					if debug then
						put_line (" no intersection");
					end if;
					
					return (status => NONE_EXIST);

					
				when ONE_EXISTS => 
					-- line is a tangent to the virtual circle			

					-- Test whether the tangent touches the arc:
					if on_arc (arc, vi.intersection) then
						if debug then
							put_line (" one intersection");
							put_line (" at " & to_string (vi.intersection));
						end if;

						return (ONE_EXISTS, vi.intersection, TANGENT);
						
					else
						if debug then
							put_line (" no intersection");
						end if;

						return (status => NONE_EXIST);
					end if;

					
				when TWO_EXIST => 
					-- line is a secant to the virtual circle.				
					-- Test whether the secant intersects the arc:

					if debug then
						put_line ("two intersections with virtual circle");
						put_line (" at " & to_string (vi.intersection_1));
						put_line (" at " & to_string (vi.intersection_2));
					end if;

					
					declare
						oa_1 : constant boolean := on_arc (arc, vi.intersection_1);
						oa_2 : constant boolean := on_arc (arc, vi.intersection_2);
					begin					
						-- if debug then
						-- 	put_line (boolean'image (oa_1));
						-- 	put_line (boolean'image (oa_2));
						-- end if;
						
						if oa_1 and oa_2 then
							-- both intersections are on the arc

							if debug then
								put_line ("two intersections");
								put_line (" at " & to_string (vi.intersection_1));
								put_line (" at " & to_string (vi.intersection_2));
							end if;
							
							return (TWO_EXIST, vi.intersection_1, vi.intersection_2);

							
						elsif oa_1 then
							-- only intersection 1 is on the arc

							if debug then
								put_line ("one intersection (1)");
								put_line (" at " & to_string (vi.intersection_1));
							end if;

							return (ONE_EXISTS, vi.intersection_1, SECANT);


							
						elsif oa_2 then
							-- only intersection 2 is on the arc

							if debug then
								put_line ("one intersection (2)");
								put_line (" at " & to_string (vi.intersection_2));
							end if;

							return (ONE_EXISTS, vi.intersection_2, SECANT);

							
						else
							-- none intersection is on the arc

							if debug then
								put_line ("no intersections");
							end if;
							
							return (status => NONE_EXIST);
						end if;
					end;
			end case;
		end do_it;	
			
		
	begin
		if debug then
			put_line ("get_intersection arc/line");
			put_line (to_string (line));
			put_line (to_string (arc));
		end if;

		return do_it;
	end get_intersection;





	function get_distance_to_circumfence (
		circle	: in type_circle_fine;
		point	: in type_vector)
		return type_distance_polar
	is
		result : type_distance_polar;
	begin
		result := get_distance (circle.center, point);
		set_absolute (result, circle.radius - get_absolute (result));
		return result;
	end get_distance_to_circumfence;



	


	function get_shortest_distance (
		circle	: in type_circle_fine;
		point	: in type_vector)
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
		
		dd : type_float_positive;
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
		arc		: in type_arc_fine;
		point	: in type_vector)
		return type_distance_polar
	is
		debug : boolean := false;
		-- debug : boolean := true;
		
		result : type_distance_polar;

		radius : constant type_float_positive := get_radius_start (arc);
		
		
		procedure do_it is 
			-- Build a line that runs from the given point to the center of the arc:
			line : constant type_line_vector := 
				to_line_vector (line => (point, arc.center, others => <>));
			-- IMPORTANT NOTE: Function to_line_vector computes the direction vector 
			-- of line as:
			--  arc.center.x - point.x and arc.center.y - point.y.
			--  Function after_center (see below) bases on this fact. Otherwise its result
			--  will be nonsense !!

			-- Get the intersection(s) of the line with the arc:
			ILC : constant type_intersection_of_line_and_circle := 
				get_intersection (arc, line);

			DPC : constant type_distance_polar := 
				get_distance (point, arc.center);

			
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
							
							if after_center (ILC.intersection) then
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
							
							if after_center (ILC.intersection) then
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
						result := get_distance_to_circumfence (
							circle	=> (arc.center, radius),
							point	=> point);
						
				end case;				
			end if;				
		end do_it;

		
	begin
		if debug then
			put_line ("get_shortest_distance");
			put_line (" P " & to_string (point)); 
			put_line ("   " & to_string (arc));
			put_line ("   " & to_string (to_arc_angles (arc)));
		end if;
		
		if point = arc.center then
			-- If the given point is right on the center of the arc,
			-- then return zero distance and zero angle:

			set_absolute (result, radius);
			set_angle (result, to_arc_angles (arc).angle_start);
		else
			do_it;
		end if;

		--put_line (to_string (result));
		
		return result;
	end get_shortest_distance;



	
	

-- DISTANCE POINT TO LINE

	function get_distance (
		line	: in type_line_fine;
		vector	: in type_vector)
		return type_float_positive
	is
		dv : constant type_vector := to_line_vector (line).v_direction;
		sv : constant type_vector := line.start_point;
		
		d1 : constant type_vector := subtract (vector, sv);
		m, n : type_float;
	begin
		m := get_absolute (cross_product (dv, d1));
		n := get_absolute (dv);
		
		return (m / n);
	end get_distance;



	function get_shortest_distance (
		vector	: in type_vector;
		line	: in type_line_fine)
		return type_float_positive
	is
		result : type_float_positive := 0.0;
		
		d : constant type_distance_point_line := get_distance (
			vector		=> vector,
			line		=> line,
			line_range	=> WITH_END_POINTS);

		d_to_start, d_to_end : type_float;
	begin
		-- put_line ("vector" & to_string (vector) & " " & to_string (line));
		
		if not out_of_range (d) then
			-- put_line ("in range");
			
			-- An imaginary line can be drawn perpendicular from
			-- point to line. Both intersect each other.
			result := get_distance (d);
		else
			-- put_line ("out of range");
			
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

		-- put_line ("distance" & to_string (result));
		
		return result;
	end get_shortest_distance;
	


	
	function on_line (
		vector	: in type_vector;
		line	: in type_line_fine;
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
		line		: in type_line_fine;
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
			dp : type_float;
			sum : type_float_positive;
		begin
			dp := dot_product (SE, SV);
			sum := get_sum_of_squared_components (SE);
			result.intersection := add (line.start_point, scale (SE, dp / sum));
		end compute_intersection;

			
		lambda_forward, lambda_backward : type_float;
	begin
		-- new_line;
		-- put_line ("get distance");
		-- put_line ("v: " & to_string (vector) & " " & to_string (line));
		-- put_line ("line direction vector: " & to_string (line_direction));

		
		-- Compute the distance from the given point to the given line.
		-- This computation does not care about end or start point of the line.
		-- It assumes an indefinite long line without start or end point.
		result.distance := get_distance (line, vector);

		-- put_line ("distance " & to_string (result.distance));

		-- Set iv so that it points to the intersection. The
		-- intersection can be anywhere on that indefinite long line.
		compute_intersection;

		-- put_line ("iv " & to_string (iv));
		
		-- Any point on a line can be computed by this formula (see textbook on vector algebra):
		-- iv = line.start_point + lambda_forward  * line_direction
		-- iv = line.end_point   + lambda_backward * line_direction

		-- Using these formula we can calculate whether iv points between 
		-- (or to) the start and/or end points of the line:
		
		lambda_forward := divide (subtract (iv, line.start_point), line_direction);

		--put_line ("lambda forward:" & to_string (lambda_forward));

		
		if lambda_forward = 0.0 then -- iv points TO start point of line
			-- put_line ("on start point");
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
			-- put_line ("before start point");
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
			-- put_line ("on end point");
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
			-- put_line ("after end point");
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

		-- put_line ("distance " & to_string (result.distance));
		return result;
	end get_distance;

	
	

	
	function out_of_range (d : in type_distance_point_line) return boolean is begin
		return d.out_of_range;
	end out_of_range;

	
	function get_distance (d : in type_distance_point_line) 
		return type_float
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
