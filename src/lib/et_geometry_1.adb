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
with ada.strings.unbounded;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with et_exceptions;				use et_exceptions;



package body et_geometry_1 is


	function "=" (left, right : in type_float_internal) return boolean is begin
		--put_line ("=" & to_string (abs (left - right)));

		if abs (left - right) <= accuracy then
			return true;
		else
			return false;
		end if;
	end "=";
	
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

	


	
	--function get_greatest (
		--left, right : in type_distance)
		--return type_distance
	--is begin
		--if left > right then
			--return left;
		--elsif left < right then
			--return right;
		--else
			--return right;
		--end if;
	--end get_greatest;

	
	--function get_smallest (
		--left, right : in type_distance)
		--return type_distance
	--is begin
		--if left < right then
			--return left;
		--elsif left > right then
			--return right;
		--else
			--return right;
		--end if;
	--end get_smallest;


	--function get_greatest (
		--distances	: in pac_distances_positive.list)
		--return type_distance_positive
	--is
		--ds : pac_distances_positive.list := distances;
		--use pac_distances_positive_sorting;
	--begin
		--sort (ds);
		--return ds.last_element;
	--end get_greatest;

	
	--function to_positive_rotation (
		--rotation	: in type_rotation)
		--return type_rotation_positive
	--is begin
		--if rotation < 0.0 then
			--return 360.0 + rotation;
		--else
			--return rotation;
		--end if;
	--end to_positive_rotation;


	--function round (
		--distance	: in type_position_axis;
		--grid		: in type_distance_grid)
		--return type_position_axis 
	--is 
		--i : integer;
		--f : type_float_internal;
	--begin
		----put_line (type_distance'image (distance) & " " & type_distance_grid'image (grid));
		--i := integer (distance / grid);
		----put_line (integer'image (i));
		----put_line ("dl " & type_distance'image (type_distance'last));
		--f := type_float_internal (i) * type_float_internal (grid);
		--return to_distance (f);
	--end round;


	
	
	--procedure scale_grid (
		--grid	: in out type_grid;
		--scale	: in type_distance_positive)
	--is begin
		--grid.x := grid.x * scale;
		--grid.y := grid.y * scale;
	--end scale_grid;

	
	--function to_string (grid : in type_grid) return string is begin
		--return point_preamble & to_string (grid.x) & axis_separator & to_string (grid.y);
	--end;

	
	--function "=" (left, right : in type_point) return boolean is begin
		--if abs (left.x - right.x) <= type_distance'small 
		--and abs (left.y - right.y) <= type_distance'small
		--then
			--return true;
		--else
			--return false;
		--end if;
	--end;
	
	--function "<" (left, right : in type_point) return boolean is begin
		--if left.x < right.x then
			--return true;
		--elsif left.x > right.x then
			--return false;

		---- left.x = right.x -> compare y:
		--elsif left.y < right.y then
			--return true;
		--else 
			---- if left.y greater or equal right.y
			--return false;
		--end if;
	--end;


	
	--function to_string (point : in type_point) return string is begin
		--return point_preamble
			--& to_string (point.x)
			--& axis_separator
			--& to_string (point.y);
	--end to_string;


	--function round_to_string (
		--point 	: in type_point;
		--grid	: in type_grid) 
		--return string 
	--is begin
		--return point_preamble
			--& to_string (round (point.x, grid.x))
			--& axis_separator
			--& to_string (round (point.y, grid.y));
	--end round_to_string;



	--function round (
		--point 	: in type_point;
		--grid	: in type_grid) 
		--return type_point'class 
	--is		
		--p : type_point := (
			--x => (round (point.x, grid.x)),
			--y => (round (point.y, grid.y)));
	--begin
		--return p;
	--end;

	
	
	--function round (point : in type_point)
		--return type_point'class
	--is
		--r : type_point := (
			--x => type_distance (round (point.x)),
			--y => type_distance (round (point.y)));
	--begin
		--return r;
	--end round;


	--procedure round (
		--point : in out type_point)
	--is begin
		--point.x := type_distance (round (point.x));
		--point.y := type_distance (round (point.y));
	--end round;

	
	
	--function to_string (
		--distance : in type_distance_relative)
		--return string
	--is begin
		--return "distance relative: x/y" 
			--& to_string (distance.x)
			--& "/"
			--& to_string (distance.y);
	--end to_string;



	--function to_distance_relative (
		--x,y : in type_float_internal)
		--return type_distance_relative
	--is begin
		--return (x, y);
	--end to_distance_relative;
	

	
	--function to_distance_relative (p : in type_point)
		--return type_distance_relative
	--is begin
		--return (p.x, p.y);
	--end to_distance_relative;



	function to_offset (
		x, y : in type_float_internal)
		return type_offset
	is begin
		return (x, y);
	end to_offset;

	

	
	
	--function get_rotation (
		--point : in type_point) 
		--return type_rotation 
	--is
		--x : constant type_float_internal := type_float_internal (point.x);
		--y : constant type_float_internal := type_float_internal (point.y);
	--begin
		---- NOTE: If x and y are zero then the arctan operation is not possible. 
		---- In this case we assume the resulting angle is zero.
		--if x = 0.0 and y = 0.0 then
			--return zero_rotation;
		--else
			--return to_rotation (arctan (y, x, units_per_cycle));
		--end if;
	--end get_rotation;



	--procedure rotate_by (
		--point		: in out type_point;
		--rotation	: in type_rotation) 
	--is			
		--angle_out			: type_rotation; -- degrees
		--distance_to_origin	: type_float_internal; -- unit is mm
		--scratch				: type_float_internal;
	--begin
		---- Do nothing if the given rotation is zero.
		--if rotation /= 0.0 then

			---- compute distance of given point to origin
			--if get_x (point) = zero and get_y (point) = zero then
				--distance_to_origin := 0.0;
				
			--elsif get_x (point) = zero then
				--distance_to_origin := type_float_internal (abs (get_y (point)));
				
			--elsif get_y (point) = zero then
				--distance_to_origin := type_float_internal (abs (get_x (point)));
				
			--else
				--distance_to_origin := sqrt (
					--type_float_internal (abs (get_x (point))) ** 2.0
					--+
					--type_float_internal (abs (get_y (point))) ** 2.0
					--);
			--end if;
			
			---- compute the current angle of the given point (in degrees)

			--if get_x (point) = zero then
				--if get_y (point) > zero then
					--angle_out := 90.0;
					
				--elsif get_y (point) < zero then
					--angle_out := -90.0;
					
				--else
					--angle_out := 0.0;
				--end if;

			--elsif get_y (point) = zero then
				--if get_x (point) > zero then
					--angle_out := 0.0;
					
				--elsif get_x (point) < zero then
					--angle_out := 180.0;
					
				--else
					--angle_out := 0.0;
				--end if;

			--else
				---- neither x nor y of point is zero
				----angle_out := arctan (
				--angle_out := to_rotation (arctan (
					--x		=> type_float_internal (get_x (point)),
					--y		=> type_float_internal (get_y (point)),
					--cycle	=> units_per_cycle));
				
			--end if;

			---- Compute new angle by adding current angle and given angle.
			--angle_out := add (angle_out, rotation);

			---- compute new x   -- (cos angle_out) * distance_to_origin
			--scratch := cos (type_float_internal (angle_out), units_per_cycle);
			
			--set (
				--axis	=> X, 
				--point	=> point, 
				--value	=> to_distance (scratch * distance_to_origin)
				--);

			---- compute new y   -- (sin angle_out) * distance_to_origin
			--scratch := sin (type_float_internal (angle_out), units_per_cycle);
			
			--set (
				--axis	=> Y,
				--point	=> point,
				--value	=> to_distance (scratch * distance_to_origin)
				--);
	
		--end if; -- if angle not zero			
	--end rotate_by;



	--procedure rotate_to (
		--point		: in out type_point;
		--rotation	: in type_rotation) -- degrees
	--is
		--distance_to_origin	: type_float_internal; -- unit is mm
		--scratch				: type_float_internal;
	--begin
		---- compute distance of given point to origin
		--if get_x (point) = zero and get_y (point) = zero then
			--distance_to_origin := 0.0;
			
		--elsif get_x (point) = zero then
			--distance_to_origin := type_float_internal (abs (get_y (point)));
			
		--elsif get_y (point) = zero then
			--distance_to_origin := type_float_internal (abs (get_x (point)));
			
		--else
			--distance_to_origin := sqrt (
				--type_float_internal (abs (get_x (point))) ** 2.0 
				--+
				--type_float_internal (abs (get_y (point))) ** 2.0
				--);
		--end if;

		---- The new angle is the given rotation.

		---- compute new x   -- (cos rotation) * distance_to_origin
		--scratch := cos (type_float_internal (rotation), units_per_cycle);
		--set (
			--axis	=> X,
			--point	=> point,
			--value	=> to_distance (scratch * distance_to_origin)
			--);

		---- compute new y   -- (sin rotation) * distance_to_origin
		--scratch := sin (type_float_internal (rotation), units_per_cycle);
		--set (
			--axis 	=> Y,
			--point	=> point,
			--value	=> to_distance (scratch * distance_to_origin)
			--);
		
	--end rotate_to;
	
	

	--function to_point (
		--d 		: in type_distance_relative;
		--clip	: in boolean := false)
		--return type_point'class
	--is 
		--p : type_point;
	--begin
		--if clip then
			--p.x := clip_distance (d.x);
			--p.y := clip_distance (d.y);				
		--else
			--p.x := d.x;
			--p.y := d.y;
		--end if;
		
		--return p;

		--exception
			--when constraint_error =>
				--log (text => "distance too great: x/y" 
					--& to_string (d.x)
					--& "/"
					--& to_string (d.y));
				--raise;
		
	--end to_point;


	--function to_point (
		--x,y : in string)
		--return type_point'class
	--is 
		--result : type_point;					
	--begin
		--result.x := to_distance (dd => x);
		--result.y := to_distance (dd => y);
		--return result;

		---- CS exception handler
	--end to_point;

	
	
	function invert (
		d : in type_offset)
		return type_offset
	is begin
		return (-1.0 * d.x, -1.0 * d.y);
	end invert;

	
	

	
	--function get_x (
		--point : in type_point) 
		--return type_position_axis 
	--is begin
		--return point.x;
	--end;

	
	--function get_y (
		--point : in type_point)
		--return type_position_axis 
	--is begin
		--return point.y;
	--end;



	
	--function get_height (boundaries : in type_boundaries)
		--return type_float_internal_positive
	--is begin
		--return boundaries.greatest_y - boundaries.smallest_y;
	--end get_height;

	
	--function get_width (boundaries : in type_boundaries)
		--return type_float_internal_positive
	--is begin
		--return boundaries.greatest_x - boundaries.smallest_x;
	--end get_width;

	
	--function to_string (boundaries : in type_boundaries) return string is begin
		--return "boundaries: SX:" & to_string (boundaries.smallest_x) 
			--& " / GX:" & to_string (boundaries.greatest_x)
			--& " / SY:" & to_string (boundaries.smallest_y)
			--& " / GY:" & to_string (boundaries.greatest_y);
	--end;

	
	--function intersect (
		--boundaries_one : in type_boundaries;
		--boundaries_two : in type_boundaries)
		--return boolean
	--is begin
		--if	boundaries_two.greatest_x > boundaries_one.smallest_x
		--and boundaries_two.smallest_x < boundaries_one.greatest_x
		--then -- boundaries intersect in x-direction

			--if	boundaries_two.greatest_y > boundaries_one.smallest_y
			--and boundaries_two.smallest_y < boundaries_one.greatest_y
			--then -- boundaries intersect in y-direction
				--return true;
			--else
				--return false;
			--end if;
			
		--else
			--return false;
		--end if;
		
	--end intersect;


	--function get_intersection (
		--boundaries_one : in type_boundaries;
		--boundaries_two : in type_boundaries)
		--return type_boundaries_intersection
	--is
		--i : type_boundaries;
	--begin
		--if intersect (boundaries_one, boundaries_two) then

			----log (text => "b1" & to_string (boundaries_one));
			----log (text => "b2" & to_string (boundaries_two));
			
			--i.smallest_x := get_greatest (boundaries_one.smallest_x, boundaries_two.smallest_x);
			--i.greatest_x := get_smallest (boundaries_one.greatest_x, boundaries_two.greatest_x);

			--i.smallest_y := get_greatest (boundaries_one.smallest_y, boundaries_two.smallest_y);
			--i.greatest_y := get_smallest (boundaries_one.greatest_y, boundaries_two.greatest_y);

			----log (text => "b " & to_string (i));
			
			--return (exists => true, intersection => i);
		--else
			--return (exists => false);
		--end if;
	--end get_intersection;


	
	---- Adds two boundaries.
	--procedure add (
		--boundaries_one : in out type_boundaries;
		--boundaries_two : in type_boundaries) is
	--begin
---- 			if boundaries_two.smallest_x < boundaries_one.smallest_x , smallest_y : type_distance := type_distance'last;
---- 			greatest_x, greatest_y : type_distance := type_distance'first;
		--null; -- CS
	--end; 

	
	--function get_boundaries (
		--point_one	: in type_point;
		--point_two	: in type_point;
		--width		: in type_distance_positive) 
		--return type_boundaries
	--is
		--result : type_boundaries;

		--half_width : constant type_float_internal_positive := type_float_internal (width) * 0.5;

		----p1x : type_float_internal renames type_float_internal (point_one.x);
		----p1y : type_float_internal renames point_one.y;

		----p2x : type_float_internal renames point_two.x;
		----p2y : type_float_internal renames point_two.y;
	--begin
		---- X axis
		--if point_one.x = point_two.x then -- both points on a vertical line

			--result.smallest_x := type_float_internal (point_one.x);
			--result.greatest_x := type_float_internal (point_one.x);
			
		--elsif point_one.x < point_two.x then
			
			--result.smallest_x := type_float_internal (point_one.x);
			--result.greatest_x := type_float_internal (point_two.x);
		--else
			--result.smallest_x := type_float_internal (point_two.x);
			--result.greatest_x := type_float_internal (point_one.x);
		--end if;

		---- Y axis
		--if point_one.y = point_two.y then -- both points on a horizontal line

			--result.smallest_y := type_float_internal (point_one.y);
			--result.greatest_y := type_float_internal (point_one.y);
			
		--elsif point_one.y < point_two.y then
			
			--result.smallest_y := type_float_internal (point_one.y);
			--result.greatest_y := type_float_internal (point_two.y);
		--else
			--result.smallest_y := type_float_internal (point_two.y);
			--result.greatest_y := type_float_internal (point_one.y);
		--end if;

		
		---- extend the boundaries by half the line width;
		--result.smallest_x := result.smallest_x - half_width;
		--result.smallest_y := result.smallest_y - half_width;

		--result.greatest_x := result.greatest_x + half_width;
		--result.greatest_y := result.greatest_y + half_width;
		
		--return result;
	--end get_boundaries;

	
	--procedure move_by (
		--boundaries	: in out type_boundaries;
		--offset		: in type_distance_relative;
		--clip		: in boolean := false)
	--is 
		--sx : constant type_float_internal := boundaries.smallest_x + type_float_internal (offset.x);
		--gx : constant type_float_internal := boundaries.greatest_x + type_float_internal (offset.x);
		--sy : constant type_float_internal := boundaries.smallest_y + type_float_internal (offset.y);
		--gy : constant type_float_internal := boundaries.greatest_y + type_float_internal (offset.y);

		--axf : constant type_float_internal := type_float_internal (type_position_axis'first);
		--axl : constant type_float_internal := type_float_internal (type_position_axis'last);
	--begin
		--if clip then
			--if sx < axf then
				--boundaries.smallest_x := axf;
			--else
				--boundaries.smallest_x := sx;
			--end if;

			--if sy < axf then
				--boundaries.smallest_y := axf;
			--else
				--boundaries.smallest_y := sy;
			--end if;

			--if gx > axl then
				--boundaries.greatest_x := axl;
			--else
				--boundaries.greatest_x := gx;
			--end if;

			--if gy > axl then
				--boundaries.greatest_y := axl;
			--else
				--boundaries.greatest_y := gy;
			--end if;
			
		--else
			--boundaries.smallest_x := sx;
			--boundaries.greatest_x := gx;
			
			--boundaries.smallest_y := sy;
			--boundaries.greatest_y := gy;
		--end if;
	--end move_by;

	

	
	--function mil_to_distance (mil : in string) return type_distance is
		--distance_mil : type_float_internal := type_float_internal'value (mil);
	--begin
		--return to_distance (distance_mil * (25.4 * 0.001));
	--end mil_to_distance;
	

	--function distance_to_mil (distance : in type_distance) return string is
		--scratch : type_float_internal;
	--begin
		--scratch := type_float_internal (distance) * 1000.00 / 25.4;
		--return to_string (to_distance (scratch));
	--end;



	

	
	--function get_quadrant (
		--point : in type_point) 
		--return type_quadrant
	--is begin
		--if point.x >= zero then -- we are right of the y-axis or on top of it
			--if point.y >= zero then -- we are above the x-axis or on top of it
				--return ONE; 
			--else -- we are below the x-axis
				--return FOUR;
			--end if;
			
		--else -- we are left of the y-axis
			--if point.y >= zero then -- we are above the x-axis or on top of it
				--return TWO;
			--else -- we are below the x-axis
				--return THREE;
			--end if;
		--end if;
	--end get_quadrant;

	
	--function invert (
		--point : in type_point'class)
		--return type_point'class 
	--is
		--pi : type_point'class := point;
	--begin
		--pi.x := - pi.x;
		--pi.y := - pi.y;
		--return pi;
	--end invert;

	
	--function invert (
		--point	: in type_point;
		--axis	: in type_axis_2d)
		--return type_point'class
	--is
		--p : type_point := point;
	--begin
		--case axis is
			--when X => p.x := - p.x;
			--when Y => p.y := - p.y;
		--end case;

		--return p;
	--end invert;

	
	--procedure reset (
		--point : in out type_point) 
	--is begin
		--point.x := zero;
		--point.y := zero;
	--end;

	
	--procedure move_by (
		--point	: in out type_point;
		--offset	: in type_distance_relative) 
	--is begin
		--point.x := point.x + offset.x;
		--point.y := point.y + offset.y;
	--end move_by;

	
	--procedure move_to (
		--point		: in out type_point;
		--destination	: in type_point'class) 
	--is begin
		--point.x := destination.x;
		--point.y := destination.y;
	--end move_to;

	
	--function move (
		--point		: in type_point;
		--direction	: in type_rotation;
		--distance	: in type_distance_positive;
		--clip		: in boolean := false)
		--return type_point'class 
	--is 			
		--delta_x, delta_y : type_float_internal := 0.0;

		--rx, ry : type_distance;			
		--result : type_point;			
	--begin
		---- sin (direction) * distance = delta y
		---- cos (direction) * distance = delty x

		--delta_y := sin (type_float_internal (direction), units_per_cycle) * type_float_internal (distance);
		--delta_x := cos (type_float_internal (direction), units_per_cycle) * type_float_internal (distance);

		--rx := point.x + to_distance (delta_x);
		--ry := point.y + to_distance (delta_y);

		--if clip then
			--clip_distance (rx);
			--clip_distance (ry);
		--end if;

		--result := (rx, ry);
		--return result;
	--end move;

	
	--procedure mirror (
		--point	: in out type_point;
		--axis	: in type_axis_2d) 
	--is begin
		--case axis is
			--when X =>
				--point.y := point.y * (-1.0);
			--when Y =>
				--point.x := point.x * (-1.0);
		--end case;
	--end mirror;

	
	--function get_distance (
		--point_1	: in type_point;
		--point_2	: in type_point;
		--axis	: in type_axis_2d) 
		--return type_distance 
	--is
		--d : type_distance;
	--begin
		--case axis is
			--when X =>
				--d := (point_2.x - point_1.x);

			--when Y =>
				--d := (point_2.y - point_1.y);
		--end case;

		--return d;
	--end get_distance;

	
	--function get_distance_abs (
		--point_1	: in type_point;
		--point_2	: in type_point;
		--axis	: in type_axis_2d) 
		--return type_distance_positive
	--is
		--d : type_distance_positive;
	--begin
		--case axis is
			--when X =>
				--d := abs (point_2.x - point_1.x);

			--when Y =>
				--d := abs (point_2.y - point_1.y);
		--end case;
				
		--return d;
	--end get_distance_abs;

	
	--function "+" (point_one, point_two : in type_point) return type_point'class is
		--d : type_point;
	--begin
		--d.x := point_one.x + point_two.x;
		--d.y := point_one.y + point_two.y;
		--return d;
	--end;
	
	--function "-" (point_one, point_two : in type_point) return type_point'class is
		--d : type_point;
	--begin
		--d.x := point_one.x - point_two.x;
		--d.y := point_one.y - point_two.y;
		--return d;
	--end;



	
	--function get_distance_relative (
		--point_one, point_two : in type_point)
		--return type_distance_relative
	--is
		--d : type_distance_relative;
	--begin
		--d.x := point_two.x - point_one.x;
		--d.y := point_two.y - point_one.y;
		--return d;
	--end get_distance_relative;

	
	--function get_distance_total (
		--point_one, point_two : in type_point) 
		--return type_float_internal_positive 
	--is
		--distance : type_float_internal_positive; -- to be returned
		--delta_x, delta_y : type_float_internal := 0.0;
	--begin
		--if point_one = point_two then
			--distance := 0.0;
			
		--elsif get_x (point_one) = get_x (point_two) then -- points are in a vertical line
			--distance := type_float_internal_positive (abs (get_y (point_two) - get_y (point_one)));
			
		--elsif get_y (point_one) = get_y (point_two) then -- points are in a horizontal line
			--distance := type_float_internal_positive (abs (get_x (point_two) - get_x (point_one)));
			
		--else
			--delta_x := type_float_internal (get_x (point_one) - get_x (point_two));
			--delta_y := type_float_internal (get_y (point_one) - get_y (point_two));

			--distance := sqrt ((delta_x ** 2) + (delta_y ** 2));
		--end if;
			
		--return distance;
	--end get_distance_total;
	

	--function in_catch_zone (
		--point_1		: in type_point; -- the reference point
		--catch_zone	: in type_catch_zone; -- zone around reference point
		--point_2 	: in type_point) -- the point being tested
		--return boolean 
	--is
		--d : type_float_internal_positive := get_distance_total (point_1, point_2);
	--begin
		--if d <= catch_zone then
			--return true;
		--else
			--return false;
		--end if;
	--end in_catch_zone;

	
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

	
	
	--function get_distance (
		--point_one, point_two : in type_point) 
		--return type_distance_polar 
	--is
		--result : type_distance_polar;

		--delta_x, delta_y : type_float_internal := 0.0;
	--begin
		--result.absolute := get_distance_total (point_one, point_two);

		---- NOTE: If the total distance between the points is zero then
		---- the arctan operation is not possible. In this case we assume
		---- the resulting angle is zero.
		---- So we do the angle computation only if there is a distance between the points:
		--if result.absolute /= 0.0 then
			
			--delta_x := type_float_internal (get_x (point_two) - get_x (point_one));
			--delta_y := type_float_internal (get_y (point_two) - get_y (point_one));

			--result.angle := to_rotation (arctan (
					--x 		=> delta_x,
					--y		=> delta_y,
					--cycle	=> units_per_cycle));
		--else
			---- distance is zero
			--result.angle := zero_rotation;
		--end if;
		
		--return result;
	--end get_distance;

	
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


	
	--function to_string (point : in type_position) return string is begin
		--return point_preamble_with_rotation
			--& to_string (point.x)
			--& axis_separator
			--& to_string (point.y)
			--& axis_separator
			--& to_string (get_rotation (point));
	--end;
	

	--function to_rotation (rotation : in string) return type_rotation is begin
		--return type_rotation'value (rotation);
	--end;

	
	--function to_string (rotation : in type_rotation) return string is begin
		--if rotation < zero_rotation then
			--return space & type_rotation'image (rotation);
		--else
			--return type_rotation'image (rotation);
		--end if;
		---- CS suppress trailing zeros
	--end;

	
	--function to_string (direction : in type_direction_of_rotation) return string is begin
		--return to_lower (type_direction_of_rotation'image (direction));
	--end to_string;
		
	--function to_direction (direction : in string) return type_direction_of_rotation is begin
		--return type_direction_of_rotation'value (direction);
	--end to_direction;

	
	--function get_direction (rotation : in type_rotation) 
		--return type_direction_of_rotation
	--is begin
		--if rotation < zero_rotation then
			--return CW;
		--else
			--return CCW;
		--end if;
	--end get_direction;
	

	--function reverse_direction (direction : in type_direction_of_rotation)
		--return type_direction_of_rotation is
	--begin
		--case direction is 
			--when CW => return CCW;
			--when CCW => return CW;
		--end case;
	--end reverse_direction;

	

	
	--function to_position (
		--point		: in type_point;
		--rotation	: in type_rotation)
		--return type_position'class
	--is 
		--result : type_position;
	--begin
		--result.x := point.x;
		--result.y := point.y;
		--result.rotation := rotation;

		--return result;
	--end to_position;

	
	--procedure set (
		--position	: in out type_position;
		--rotation	: in type_rotation) 
	--is begin
		--position.rotation := rotation;
	--end;

	
	--function get_rotation (
		--position : in type_position)
		--return type_rotation 
	--is begin
		--return position.rotation;
	--end;

	
	--procedure rotate_about_itself (
		--position	: in out type_position;
		--offset		: in type_rotation)
	--is begin
		--position.rotation := add (position.rotation, offset);
	--end;
		

-- VECTORS	

	function "=" (
		left, right : in type_vector)
		return boolean 
	is
		--lx : type_float_internal := get_x (left);
		--ly : type_float_internal := get_y (left);
		--lz : type_float_internal := get_z (left);

		--rx : type_float_internal := get_x (right);
		--ry : type_float_internal := get_y (right);
		--rz : type_float_internal := get_z (right);
	begin
		--new_line;
		--put_line ("left: " & to_string (left));
		--put_line ("right:" & to_string (right));
		
		--if  abs (lx - rx) <= rounding_threshold
		--and abs (ly - ry) <= rounding_threshold
		--and abs (lz - rz) <= rounding_threshold
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
		v	: in type_vector)
		return string
	is begin
		return 
			  " x: " & to_string (v.x) 
			& " y: " & to_string (v.y)
			& " z: " & to_string (v.z);
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
		v_1	: in type_vector;
		v_2	: in type_vector)
		return type_float_internal_positive
	is 
		dx : constant type_float_internal := abs (v_2.x - v_1.x);
		dy : constant type_float_internal := abs (v_2.y - v_1.y);
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

	
	function to_perpendicular_line_vector (
		point	: in type_vector;
		angle	: in type_angle)
		return type_line_vector 
	is
		ap : type_angle; -- the angle of the resulting line
		r : type_ray;
	begin
		--ap := add (angle, 90.0); -- perpendicular
		ap := angle + 90.0; -- CS ? perpendicular

		-- Build a ray that starts at point and travels
		-- in direction ap:
		r := (point, ap);

		-- Convert the ray to a line vector:
		return to_line_vector (r);
	end to_perpendicular_line_vector;


	
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

			--th : constant type_float_internal := 1.0E-17;
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

				if abs (distance) <= rounding_threshold then						
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

				--put_line ("get_intersection: " & to_string (i.vector));
				return (status => EXISTS, intersection => i);
			else

				return (status => NOT_EXISTENT);
			end if;
		end if;
		
	end get_intersection;



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


	procedure move_by (
		line		: in out type_line;
		direction	: in type_angle;
		distance	: in type_float_internal_positive)
	is begin
		move_by (line.start_point, direction, distance);
		move_by (line.end_point, direction, distance);
	end move_by;


	

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

	
end et_geometry_1;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
