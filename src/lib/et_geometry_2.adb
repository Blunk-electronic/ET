------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              GEOMETRY 2                                  --
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



package body et_geometry_2 is

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
		--CS put_line ("rounding error:" & pac_geometry_1.to_string (type_float_internal'small));
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
		& lf & "min:        " & type_float_internal'image (type_float_internal'first)
		& lf & "max:        " & type_float_internal'image (type_float_internal'last)
		& lf & "digits:     " & positive'image (type_float_internal'digits)
		& lf;
	end get_info;


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

	
	
	function mil_to_distance (mil : in string) return type_distance is begin
		return type_distance (pac_geometry_1.mil_to_distance (mil));
		-- CS use to_distance instead of type_distance ?
	end mil_to_distance;
	

	function distance_to_mil (distance : in type_distance) return string is begin
		return pac_geometry_1.distance_to_mil (type_float_internal (distance));
	end;



	function to_distance (dd : in string) 
		return type_distance 
	is begin
		return type_distance'value (dd);

		exception when event: others =>
			raise syntax_error_2 with 
				"ERROR: Expect a distance instead of " 
				& enclose_in_quotes (dd) & " !";
	end to_distance;
	
	
	function to_distance (f : in type_float_internal)
		return type_distance 
	is
		use pac_distance_io;
		
		d1 : type_distance;
		d2 : type_float_internal;

		f1 : constant type_float_internal := 5.0 * type_float_internal (type_distance'small);
		-- CS should be a package wide constant ?
	begin
		d1 := type_distance (f);
		
		d2 := 10.0 * abs (f - type_float_internal (d1));
		
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
				----put_line (type_float_internal'image (f) & " " & natural'image (r'length));
				---- CS: IMPROVEMENT REQUIRED !!!
				--put (to => r, item => f, aft => type_distance'scale, exp => 0);
				--return type_distance'value (r);
			--end;
		--end if;


		exception when event: others =>
			put_line ("f: " & to_string (f));
			raise;

	end to_distance;
	
	
	function to_string (distance : in type_distance) 
		return string
	is begin
		if distance < zero then
			return space & type_distance'image (distance);
		else
			return type_distance'image (distance);
		end if;
		-- CS suppress trailing zeros
	end to_string;


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




	function round (
		distance	: in type_position_axis;
		grid		: in type_distance_grid)
		return type_position_axis 
	is 
		i : integer;
		f : type_float_internal;
	begin
		--put_line (type_distance'image (distance) & " " & type_distance_grid'image (grid));
		i := integer (distance / grid);
		--put_line (integer'image (i));
		--put_line ("dl " & type_distance'image (type_distance'last));
		f := type_float_internal (i) * type_float_internal (grid);
		return to_distance (f);
	end round;


	procedure scale_grid (
		grid	: in out type_grid;
		scale	: in type_distance_positive)
	is begin
		grid.x := grid.x * scale;
		grid.y := grid.y * scale;
	end scale_grid;

	
	function to_string (grid : in type_grid) return string is begin
		return point_preamble & to_string (grid.x) & axis_separator & to_string (grid.y);
	end;

	
	
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


	


	procedure move_by (
		boundaries	: in out type_boundaries;
		offset		: in type_offset;
		clip		: in boolean := false)
	is 
		sx : constant type_float_internal := boundaries.smallest_x + offset.x;
		gx : constant type_float_internal := boundaries.greatest_x + offset.x;
		sy : constant type_float_internal := boundaries.smallest_y + offset.y;
		gy : constant type_float_internal := boundaries.greatest_y + offset.y;

		axf : constant type_float_internal := type_float_internal (type_position_axis'first);
		axl : constant type_float_internal := type_float_internal (type_position_axis'last);
	begin
		if clip then
			if sx < axf then
				boundaries.smallest_x := axf;
			else
				boundaries.smallest_x := sx;
			end if;

			if sy < axf then
				boundaries.smallest_y := axf;
			else
				boundaries.smallest_y := sy;
			end if;

			if gx > axl then
				boundaries.greatest_x := axl;
			else
				boundaries.greatest_x := gx;
			end if;

			if gy > axl then
				boundaries.greatest_y := axl;
			else
				boundaries.greatest_y := gy;
			end if;
			
		else
			boundaries.smallest_x := sx;
			boundaries.greatest_x := gx;
			
			boundaries.smallest_y := sy;
			boundaries.greatest_y := gy;
		end if;
	end move_by;

	
	

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

	

	
	
	function to_string (status : in type_point_status) return string is begin
		return type_point_status'image (status);
	end to_string;

	
	procedure toggle_status (status : in out type_point_status) is begin
		case status is
			when OUTSIDE	=> status := INSIDE;
			when INSIDE		=> status := OUTSIDE;
		end case;
	end toggle_status;

	


	
	function get_distance (
		point	: in type_point;
		vector	: in type_vector)
		return type_distance_polar
	is
		v : constant type_vector := to_vector (point);
	begin
		return get_distance (v, vector);
	end get_distance;
	






-------------------


-- ROTATION / ANGLE

	
	function to_rotation (rotation : in string) return type_rotation is begin
		return type_rotation'value (rotation);
	end;

	
	function to_string (rotation : in type_rotation) return string is begin
		if rotation < zero_rotation then
			return space & type_rotation'image (rotation);
		else
			return type_rotation'image (rotation);
		end if;
		-- CS suppress trailing zeros
	end;

	
	
	function to_rotation (f : in type_float_internal)
		return type_rotation 
	is
		use pac_distance_io;

		d1 : type_rotation := type_rotation (f);
		d2 : type_float_internal;

		f1 : constant type_float_internal := 5.0 * type_float_internal (type_rotation'small);

	begin
		d2 := 10.0 * abs (f - type_float_internal (d1));
		
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
		return type_float_internal
	is begin
		return type_float_internal (a);
	end to_angle;

	
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

	
	
	function add (
		left, right : in type_rotation)
		return type_rotation 
	is
		subtype type_rotation_wide is type_float_internal range -720.0 .. +720.0;
		scratch : type_rotation_wide;
		result : type_rotation; -- to be returned
	begin
		scratch := type_float_internal (left) + type_float_internal (right);
		
		if scratch >= 360.0 then
			scratch := scratch - 360.0;
			
		elsif scratch <= -360.0 then
			scratch := scratch + 360.0;
		end if;

		result := to_rotation (scratch);
		return result;
	end;

	

-- RELATIVE DISTANCE:


	
	function invert (
		d : in type_distance_relative)
		return type_distance_relative
	is begin
		return (-1.0 * d.x, -1.0 * d.y);
	end invert;

	
	function to_string (
		distance : in type_distance_relative)
		return string
	is begin
		return "distance relative: x/y" 
			& to_string (distance.x)
			& "/"
			& to_string (distance.y);
	end to_string;


	function to_distance_relative (
		x,y : in type_distance)
		return type_distance_relative
	is begin
		return (x, y);
	end to_distance_relative;

	
	function to_distance_relative (
		v : in type_vector)
		return type_distance_relative
	is begin
		return (type_distance (v.x), type_distance (v.y));
	end to_distance_relative;

	

	
	
-- POINT

	function to_vector (
		point	: in type_point)
		return type_vector 
	is begin
		return set (
			x => type_float_internal (get_x (point)),
			y => type_float_internal (get_y (point)),
			z => 0.0
			);
	end to_vector;


	
	function to_point (
		v	: in type_vector)
		return type_point
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


	function to_offset (
		p : in type_point)
		return type_offset
	is begin
		return (type_float_internal (p.x), type_float_internal (p.y));
	end to_offset;


	
	function to_offset (
		x, y : in type_distance)
		return type_offset
	is begin
		return (type_float_internal (x), type_float_internal (y));
	end to_offset;
	
	
	
	function to_distance_relative (
		p : in type_point)
		return type_distance_relative
	is begin
		return (p.x, p.y);
	end to_distance_relative;


	function get_distance (
		point_one, point_two : in type_point) 
		return type_distance_polar 
	is begin
		return get_distance (to_vector (point_one), to_vector (point_two));
	end get_distance;


	
	function get_x (
		point : in type_point) 
		return type_position_axis 
	is begin
		return point.x;
	end;

	
	function get_y (
		point : in type_point)
		return type_position_axis 
	is begin
		return point.y;
	end;



	function set (
		x, y : in type_position_axis)
		return type_point 
	is
		point : type_point;
	begin
		point.x := x;
		point.y := y;
		return point;
	end;

	
	procedure set (
		point	: in out type_point;
		axis 	: in type_axis_2d;
		value	: in type_position_axis)
	is begin
		case axis is
			when X => point.x := value;
			when Y => point.y := value;
		end case;
	end;

	
	procedure set (
		point	: in out type_point;
		position: in type_point) 
	is begin
		point.x := position.x;
		point.y := position.y;
	end;



	function invert (
		point : in type_point)
		return type_point 
	is
		pi : type_point := point;
	begin
		pi.x := - pi.x;
		pi.y := - pi.y;
		return pi;
	end invert;

	
	function invert (
		point	: in type_point;
		axis	: in type_axis_2d)
		return type_point
	is
		p : type_point := point;
	begin
		case axis is
			when X => p.x := - p.x;
			when Y => p.y := - p.y;
		end case;

		return p;
	end invert;

	
	procedure reset (
		point : in out type_point) 
	is begin
		point.x := zero;
		point.y := zero;
	end;


	
	procedure move_by (
		point	: in out type_point;
		offset	: in type_distance_relative) 
	is begin
		point.x := point.x + offset.x;
		point.y := point.y + offset.y;
	end move_by;

	
	procedure move_to (
		point		: in out type_point;
		destination	: in type_point) 
	is begin
		point.x := destination.x;
		point.y := destination.y;
	end move_to;


	function move (
		point		: in type_point;
		direction	: in type_rotation;
		distance	: in type_distance_positive;
		clip		: in boolean := false)
		return type_point 
	is 			
		v_tmp : type_vector;
		rx, ry : type_distance;			
		result : type_point;			
	begin
		v_tmp := move_by (
			v			=> to_vector (point),
			direction	=> type_angle (direction),
			distance	=> type_float_internal_positive (distance));
		
		rx := to_distance (v_tmp.x);
		ry := to_distance (v_tmp.y);
		
		if clip then
			clip_distance (rx);
			clip_distance (ry);
		end if;

		result := (rx, ry);
		return result;
	end move;

	
	procedure mirror (
		point	: in out type_point;
		axis	: in type_axis_2d) 
	is begin
		case axis is
			when X =>
				point.y := point.y * (-1.0);
			when Y =>
				point.x := point.x * (-1.0);
		end case;
	end mirror;


	function get_distance_total (
		point	: in type_point;
		vector	: in type_vector)
		return type_float_internal_positive
	is begin
		return get_distance_total (to_vector (point), vector);
	end get_distance_total;

	

	function get_distance (
		point_1	: in type_point;
		point_2	: in type_point;
		axis	: in type_axis_2d) 
		return type_distance 
	is
		d : type_distance;
	begin
		case axis is
			when X =>
				d := (point_2.x - point_1.x);

			when Y =>
				d := (point_2.y - point_1.y);
		end case;

		return d;
	end get_distance;

	
	function get_distance_abs (
		point_1	: in type_point;
		point_2	: in type_point;
		axis	: in type_axis_2d) 
		return type_distance_positive
	is
		d : type_distance_positive;
	begin
		case axis is
			when X =>
				d := abs (point_2.x - point_1.x);

			when Y =>
				d := abs (point_2.y - point_1.y);
		end case;
				
		return d;
	end get_distance_abs;



	function "+" (point_one, point_two : in type_point) return type_point is
		d : type_point;
	begin
		d.x := point_one.x + point_two.x;
		d.y := point_one.y + point_two.y;
		return d;
	end;

	
	function "-" (point_one, point_two : in type_point) return type_point is
		d : type_point;
	begin
		d.x := point_one.x - point_two.x;
		d.y := point_one.y - point_two.y;
		return d;
	end;


	function get_distance_relative (
		point_one, point_two : in type_point)
		return type_distance_relative
	is
		d : type_distance_relative;
	begin
		d.x := point_two.x - point_one.x;
		d.y := point_two.y - point_one.y;
		return d;
	end get_distance_relative;


	function get_distance_total (
		point_one, point_two : in type_point) 
		return type_float_internal_positive 
	is begin
		return get_distance_total (to_vector (point_one), to_vector (point_two));
	end get_distance_total;

	
	function catch_zone_to_string (
		c : in type_catch_zone)
		return string
	is begin
		return pac_geometry_1.to_string (c);
	end catch_zone_to_string;


	function to_catch_zone (
		c : in string)
		return type_catch_zone
	is begin
		return pac_geometry_1.to_distance (c);
	end to_catch_zone;

	
	function in_catch_zone (
		point_1		: in type_point; -- the reference point
		catch_zone	: in type_catch_zone; -- zone around reference point
		point_2 	: in type_point) -- the point being tested
		return boolean 
	is
		d : type_float_internal_positive := get_distance_total (point_1, point_2);
	begin
		if d <= catch_zone then
			return true;
		else
			return false;
		end if;
	end in_catch_zone;


	
	
	function to_string (point : in type_point) 
		return string 
	is begin
		return point_preamble
			& to_string (point.x)
			& axis_separator
			& to_string (point.y);
	end to_string;

	
	function round_to_string (
		point 	: in type_point;
		grid	: in type_grid) 
		return string 
	is begin
		return point_preamble
			& to_string (round (point.x, grid.x))
			& axis_separator
			& to_string (round (point.y, grid.y));
	end round_to_string;



	function round (
		point 	: in type_point;
		grid	: in type_grid) 
		return type_point 
	is		
		p : type_point := (
			x => (round (point.x, grid.x)),
			y => (round (point.y, grid.y)));
	begin
		return p;
	end;

	
	
	--function round (
		--point : in type_point)
		--return type_point
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



	

	function get_rotation (
		point : in type_point) 
		return type_rotation 
	is begin
		return to_rotation (get_angle (get_distance (null_vector, to_vector (point))));
	end get_rotation;



	procedure rotate_by (
		point		: in out type_point;
		rotation	: in type_rotation) 
	is			
		v_tmp : type_vector := to_vector (point);
	begin
		rotate_by (
			vector		=> v_tmp,
			rotation	=> type_angle (rotation),
			debug		=> false);

		point := to_point (v_tmp);
	end rotate_by;



	procedure rotate_to (
		point		: in out type_point;
		rotation	: in type_rotation) -- degrees
	is
		distance_to_origin	: type_float_internal; -- unit is mm
		scratch				: type_float_internal;
	begin
		-- compute distance of given point to origin
		if get_x (point) = zero and get_y (point) = zero then
			distance_to_origin := 0.0;
			
		elsif get_x (point) = zero then
			distance_to_origin := type_float_internal (abs (get_y (point)));
			
		elsif get_y (point) = zero then
			distance_to_origin := type_float_internal (abs (get_x (point)));
			
		else
			distance_to_origin := sqrt (
				type_float_internal (abs (get_x (point))) ** 2.0 
				+
				type_float_internal (abs (get_y (point))) ** 2.0
				);
		end if;

		-- The new angle is the given rotation.

		-- compute new x   -- (cos rotation) * distance_to_origin
		scratch := cos (type_float_internal (rotation), units_per_cycle);
		set (
			axis	=> X,
			point	=> point,
			value	=> to_distance (scratch * distance_to_origin)
			);

		-- compute new y   -- (sin rotation) * distance_to_origin
		scratch := sin (type_float_internal (rotation), units_per_cycle);
		set (
			axis 	=> Y,
			point	=> point,
			value	=> to_distance (scratch * distance_to_origin)
			);
		
	end rotate_to;
	
	

	function to_point (
		d 		: in type_distance_relative;
		clip	: in boolean := false)
		return type_point
	is 
		p : type_point;
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
		
	end to_point;


	function to_point (
		x,y : in string)
		return type_point
	is 
		result : type_point;					
	begin
		result.x := to_distance (dd => x);
		result.y := to_distance (dd => y);
		return result;

		-- CS exception handler
	end to_point;

	
	procedure union (
		boundaries	: in out type_boundaries;
		point		: in type_point) 
	is 
		x : constant type_float_internal := type_float_internal (get_x (point));
		y : constant type_float_internal := type_float_internal (get_y (point));
	begin
		-- X axis
		if x < boundaries.smallest_x then 
			boundaries.smallest_x := x; 
		end if;
		
		if x > boundaries.greatest_x then
			boundaries.greatest_x := x; 
		end if;

		-- Y axis
		if y < boundaries.smallest_y then
			boundaries.smallest_y := y;
		end if;
		
		if y > boundaries.greatest_y then
			boundaries.greatest_y := y;
		end if;
	end union;



	-- Calculates the boundaries of the given points
	-- connected with a line that has the
	-- given width. The boundaries are extended
	-- by half the given width.
	function get_boundaries (
		point_one	: in type_point;
		point_two	: in type_point;
		width		: in type_distance_positive) 
		return type_boundaries
	is
		result : type_boundaries;

		half_width : constant type_float_internal_positive := type_float_internal (width) * 0.5;
	begin
		-- X axis
		if point_one.x = point_two.x then -- both points on a vertical line

			result.smallest_x := type_float_internal (point_one.x);
			result.greatest_x := type_float_internal (point_one.x);
			
		elsif point_one.x < point_two.x then
			
			result.smallest_x := type_float_internal (point_one.x);
			result.greatest_x := type_float_internal (point_two.x);
		else
			result.smallest_x := type_float_internal (point_two.x);
			result.greatest_x := type_float_internal (point_one.x);
		end if;

		-- Y axis
		if point_one.y = point_two.y then -- both points on a horizontal line

			result.smallest_y := type_float_internal (point_one.y);
			result.greatest_y := type_float_internal (point_one.y);
			
		elsif point_one.y < point_two.y then
			
			result.smallest_y := type_float_internal (point_one.y);
			result.greatest_y := type_float_internal (point_two.y);
		else
			result.smallest_y := type_float_internal (point_two.y);
			result.greatest_y := type_float_internal (point_one.y);
		end if;

		
		-- extend the boundaries by half the line width;
		result.smallest_x := result.smallest_x - half_width;
		result.smallest_y := result.smallest_y - half_width;

		result.greatest_x := result.greatest_x + half_width;
		result.greatest_y := result.greatest_y + half_width;
		
		return result;
	end get_boundaries;


	
	
	function "<" (left, right : in type_point) return boolean is begin
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
		


	function get_nearest (
		points		: in pac_points.list;
		reference	: in type_point := origin)
		return type_point
	is
		use pac_points;
		
		result : type_point;

		distance : type_float_internal_positive := type_float_internal_positive'last;
		
		procedure query_point (p : in pac_points.cursor) is
			d_scratch : constant type_float_internal_positive := 
				get_absolute (get_distance (reference, element (p)));
		begin
			if d_scratch < distance then
				distance := d_scratch;
				result := element (p);
			end if;
		end query_point;
		
	begin
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

	
	--function to_string (points : in pac_points.list) return string is
		--use pac_points;
		--use ada.strings.unbounded;
		
		--result : unbounded_string;
		
		--procedure query_point (p : in pac_points.cursor) is begin
			--result := result & " " 
				--& trim (to_string (get_x (element (p))), left)
				--& "/"
				--& trim (to_string (get_y (element (p))), left);
		--end query_point;
			
	--begin
		--points.iterate (query_point'access);
		--return to_string (result);
	--end to_string;

	
	
	--procedure splice_points (
		--points_target : in out pac_points.list;
		--points_source : in pac_points.list)
	--is 
		--scratch : pac_points.list := points_source;
	--begin
		--pac_points.splice (
			--target	=> points_target,
			--before	=> pac_points.no_element,
			--source	=> scratch);
	--end splice_points;

	

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

	
	
	--procedure sort_by_distance (
		--points 		: in out pac_points.list;
		--reference	: in type_point)
	--is
		--type type_item is record
			--point		: type_point;
			--distance	: type_float_internal_positive;
		--end record;

		
		--function "<" (left, right : in type_item) return boolean is begin
			--if left.distance < right.distance then
				--return true;
			--else
				--return false;
			--end if;
		--end;
	
			
		--package pac_items is new doubly_linked_lists (type_item);
		--use pac_items;
		
		--items : pac_items.list;

		
		--procedure query_point (p : in pac_points.cursor) is 
			--use pac_points;
			--d : type_distance_polar;
		--begin
			--d := get_distance (type_point (reference), type_point (element (p)));
			
			--items.append (new_item => (
				--point		=> element (p),
				--distance	=> get_absolute (d)));
		--end query_point;

		

		--package pac_sorting is new pac_items.generic_sorting;
		--use pac_sorting;
		

		--procedure query_item (i : in pac_items.cursor) is begin
			--points.append (element (i).point);
		--end query_item;
		
		
	--begin
		---- Collect points and their distance to the reference
		---- in list "items":
		--points.iterate (query_point'access);

		---- Sort items by distance to reference:
		--sort (items);

		---- The old points are no longer required:
		--points.clear;
		---- New points will be appended here.
		

		---- Traverse items and append them one by one to the
		---- list of points:
		--items.iterate (query_item'access);
	--end sort_by_distance;




	
-- LINE

	function to_string (line : in type_line) return string is begin
		return 
			"line: S:" & to_string (line.start_point) 
			& " / E:" & to_string (line.end_point);
	end;


	function to_line_fine (
		line : in type_line)
		return pac_geometry_1.type_line
	is begin
		return (
			start_point	=> to_vector (line.start_point),
			end_point	=> to_vector (line.end_point));
	end to_line_fine;

	
	function to_line_coarse (
		line : in pac_geometry_1.type_line)
		return type_line'class
	is 
		l : type_line;
	begin
		l.start_point := to_point (line.start_point);
		l.end_point := to_point (line.end_point);		
		return l;
	end to_line_coarse;

	
	
	function get_start_vector (
		line	: in type_line)
		return type_vector 
	is begin
		return set (
			x => type_float_internal (line.start_point.x),
			y => type_float_internal (line.start_point.y),
			z => 0.0);
	end get_start_vector;

	
	function get_end_vector (
		line	: in type_line)
		return type_vector 
	is begin
		return set (
			x => type_float_internal (line.end_point.x),
			y => type_float_internal (line.end_point.y),
			z => 0.0);
	end get_end_vector;

	
	function get_direction_vector (
		line	: in type_line)
		return type_vector 
	is begin
		return set (
			x => type_float_internal (line.end_point.x - line.start_point.x),
			y => type_float_internal (line.end_point.y - line.start_point.y),
			z => 0.0);
	end get_direction_vector;


	function get_direction (
		line	: in type_line)
		return type_angle 
	is
		dx : constant type_float_internal := 
			type_float_internal (line.end_point.x - line.start_point.x);
		
		dy : constant type_float_internal := 
			type_float_internal (line.end_point.y - line.start_point.y);
		
	begin
		-- NOTE: If dx and dy are zero then the arctan operation is not possible. 
		-- In this case we assume the resulting angle is zero.
		if dx = 0.0 and dy = 0.0 then
			return 0.0;
		else
			return arctan (dy, dx, units_per_cycle);
		end if;
	end get_direction;

	
	function to_line_vector (
		line	: in type_line)
		return type_line_vector
	is begin
		return (
			v_start		=> get_start_vector (line),
			v_direction	=> get_direction_vector (line));
	end to_line_vector;
	
	
	function make_line (
		start_x, start_y, end_x, end_y : in type_distance)
		return type_line'class
	is
		line : type_line;
	begin
		line := (
			start_point => set (start_x, start_y),
			end_point	=> set (end_x, end_y));

		if get_length (line) > type_distance'small then
			return line;
		else
			raise constraint_error with "Line has zero length !";
		end if;
	end make_line;


	function make_line (
		start_point, end_point : in type_point)
		return type_line'class
	is
		line : type_line;
	begin
		line := (start_point, end_point);

		if get_length (line) > type_distance'small then
			return line;
		else
			raise constraint_error with "Line has zero length !";
		end if;
	end make_line;




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
		
		if b.smallest_x = type_float_internal (get_x (line.start_point)) then
			p := line.start_point;
			
		elsif b.smallest_x = type_float_internal (get_x (line.end_point)) then
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
		
		if b.greatest_x = type_float_internal (get_x (line.start_point)) then
			p := line.start_point;
			
		elsif b.greatest_x = type_float_internal (get_x (line.end_point)) then
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
		
		if b.smallest_y = type_float_internal (get_x (line.start_point)) then
			p := line.start_point;
			
		elsif b.smallest_y = type_float_internal (get_x (line.end_point)) then
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
		
		if b.greatest_y = type_float_internal (get_x (line.start_point)) then
			p := line.start_point;
			
		elsif b.greatest_y = type_float_internal (get_x (line.end_point)) then
			p := line.end_point;

		else
			-- If boundaries where provided and neither start nor end point
			-- of line matches, issue exception:
			raise constraint_error with to_string (b) 
				& " invalid for line" & to_string (line) & " !";
		end if;
			
		return p;
	end get_upper_end;



	
	

	--function round (line : in type_line)
		--return type_line'class
	--is 
		--r : type_line;
	--begin
		--r := (
			--start_point	=> round (line.start_point),
			--end_point	=> round (line.end_point));

		--return r;

		---- CS length check as in function make_line ?
	--end round;

	
	--procedure round (line : in out type_line) 
	--is begin
		--line.start_point := round (line.start_point);
		--line.end_point := round (line.end_point);

		---- CS length check as in function make_line ?
	--end round;


	
	function get_length (line : in type_line)
		return type_float_internal_positive
	is begin
		return get_distance_total (line.start_point, line.end_point);
	end get_length;


	function get_greatest_length (l1, l2 : in type_line)
		return type_float_internal_positive
	is 
		length_1 : constant type_float_internal_positive := get_length (l1);
		length_2 : constant type_float_internal_positive := get_length (l2);
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
		length_1 : constant type_float_internal_positive := get_length (l1);
		length_2 : constant type_float_internal_positive := get_length (l2);
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


	function get_tangent_direction (
		angle : in type_tangent_angle)
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
	

	function get_distance (
		line	: in type_line;
		vector	: in type_vector)
		return type_float_internal_positive
	is begin
		return get_distance (to_line_fine (line), vector);
	end get_distance;
	


	
	function get_distance (
		vector		: in type_vector;
		line		: in type_line;
		line_range	: in type_line_range)
		return type_distance_point_line 
	is begin
		return get_distance (
			vector		=> vector,
			line		=> to_line_fine (line),
			line_range	=> line_range);
	end get_distance;


	function get_distance (
		point		: in type_point; 
		line		: in type_line;
		line_range	: in type_line_range)
		return type_distance_point_line
	is begin
		return get_distance (to_vector (point), to_line_fine (line), line_range);
	end get_distance;



	function on_line (
		vector	: in type_vector;
		line	: in type_line)
		return boolean
	is begin
		return on_line (vector, to_line_fine (line));
	end on_line;

	
	function on_line (
		point	: in type_point;
		line	: in type_line)
		return boolean
	is begin
		return on_line (to_vector (point), to_line_fine (line));
	end on_line;


	
	function get_shortest_distance (
		line	: in type_line;
		point	: in type_point)
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
				set_absolute (result, get_distance (d));
				-- CS set_angle (result, get_direction (d));
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


	function get_shortest_distance (
		line	: in type_line;
		point	: in type_vector)
		return type_float_internal
	is
		result : type_float_internal;

		d : constant type_distance_point_line := get_distance (
			vector		=> point,
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
				d_to_start := get_distance_total (line.start_point, point);
				d_to_end   := get_distance_total (line.end_point, point);

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



	function get_intersection (
		line_1, line_2 : in type_line)
		return type_intersection_of_two_lines
	is
		lv_1 : constant type_line_vector := to_line_vector (line_1);
		lv_2 : constant type_line_vector := to_line_vector (line_2);

		int_A : constant type_intersection_of_two_lines := get_intersection (lv_1, line_2);
		int_B : constant type_intersection_of_two_lines := get_intersection (lv_2, line_1);

		status : type_intersection_status_of_two_lines;
		intersection : type_intersection;
		
	begin
		--if int_A.status = NOT_EXISTENT or int_B.status = NOT_EXISTENT then
			--status := NOT_EXISTENT;

		if int_A.status = OVERLAP and int_B.status = OVERLAP then -- CS ? correct ?
			status := OVERLAP;
			
		elsif int_A.status = EXISTS and int_B.status = EXISTS then

			-- double check: location vectors must match !
			if get_absolute (get_distance (int_A.intersection.vector, int_B.intersection.vector)) = 0.0 then
				status := EXISTS;
				intersection.vector := int_A.intersection.vector;
				intersection.angle := int_A.intersection.angle;
			else
				raise constraint_error with 
					"Intersection point mismatch: " & to_string (int_A.intersection.vector)
					& to_string (int_B.intersection.vector);
			end if;

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
	



	procedure move_by (
		line		: in out type_line;
		direction	: in type_rotation;
		distance	: in type_distance_positive) 
	is begin
		-- Move start and and point of line into direction by distance.
		line.start_point	:= move (line.start_point, direction, distance);
		line.end_point		:= move (line.end_point,   direction, distance);
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


	
	function get_boundaries (
		line	: in type_line;	
		width	: in type_distance_positive)
		return type_boundaries 
	is begin
		return get_boundaries (line.start_point, line.end_point, width);
	end get_boundaries;


	

-- ZONES OF A LINE
	
	function which_zone (
		point	: in type_point;
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


	

-- ARC

	function to_arc_fine (
		arc : in type_arc)
		return pac_geometry_1.type_arc
	is begin
		return (
			center		=> to_vector (arc.center),
			start_point	=> to_vector (arc.start_point),
			end_point	=> to_vector (arc.end_point),
			direction	=> arc.direction);
	end to_arc_fine;


	function to_arc_coarse (
		arc : in pac_geometry_1.type_arc)
		return type_arc'class
	is 
		result : type_arc;
	begin
		result := (
			center		=> to_point (arc.center),
			start_point	=> to_point (arc.start_point),
			end_point	=> to_point (arc.end_point),
			direction	=> arc.direction);

		return result;
	end to_arc_coarse;

	
	function to_string (arc : in type_arc) return string is begin
		return "arc: "
			& "C:" & to_string (arc.center) 
			& " / S:" & to_string (arc.start_point) 
			& " / E:" & to_string (arc.end_point)
			& " / D: " & to_string (arc.direction);
	end to_string;

	
	
	--function round (arc : in type_arc)
		--return type_arc'class
	--is 
		--r : type_arc;
	--begin
		--r := (
			--center		=> round (arc.center),
			--start_point	=> round (arc.start_point),
			--end_point	=> round (arc.end_point),
			--direction	=> arc.direction);

		--return r;
	--end round;

	--procedure round (arc : in out type_arc) 
	--is begin
		--arc.center		:= round (arc.center);
		--arc.start_point	:= round (arc.start_point);
		--arc.end_point	:= round (arc.end_point);
	--end round;


	

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
		arc		: in type_arc;
		point	: in type_point)
		return type_distance_polar
	is
		result : type_distance_polar;

		procedure do_it is 
			-- Build a line that runs from the given point to the center of the arc:
			line : constant type_line_vector := et_geometry_2.to_line_vector (line => (point, arc.center));
			-- IMPORTANT NOTE: Function to_line_vector computes the direction vector of line as:
			--  arc.center.x - point.x and arc.center.y - point.y.
			--  Function after_center (see below) bases on this fact. Otherwise its result
			--  will be nonsense !!

			-- Get the intersection(s) of the line with the arc:
			ILC : constant type_intersection_of_line_and_circle := get_intersection (arc, line);

			DPC : constant type_distance_polar := get_distance (point, arc.center);
			radius : constant type_float_internal_positive := get_radius_start (arc);

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
						--result := get_distance_to_circumfence (point, (arc.center, radius));
						result := get_distance_to_circumfence ((arc.center, radius), point);
						
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
		return type_float_internal
	is
		result : type_float_internal := 0.0;

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
				--lambda : type_float_internal;
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

	
	
	--function crosses_threshold ( -- CS remove ?
		--arc			: in type_arc;
		--y_threshold	: in type_distance)
		--return boolean
	--is begin
		--if	
			--get_y (arc.start_point) >= y_threshold and 
			--get_y (arc.end_point)   <  y_threshold then
			--return true;
			
		--elsif
			--get_y (arc.end_point)   >= y_threshold and 
			--get_y (arc.start_point) <  y_threshold then
			--return true;
			
		--else
			--return false;
		--end if;
	--end crosses_threshold;

	
	
	
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

	
	function is_valid (
		arc : in type_arc)
		return boolean 
	is 
		rs : constant type_float_internal_positive := get_radius_start (arc);
		re : constant type_float_internal_positive := get_radius_end (arc);
	begin
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
			--result.angle_start := to_degrees (type_float_internal (arctan (
					--y => type_float_internal (get_y (arc_tmp.start_point)),
					--x => type_float_internal (get_x (arc_tmp.start_point)))));
			result.angle_start := arctan (
					y => type_float_internal (get_y (arc_tmp.start_point)),
					x => type_float_internal (get_x (arc_tmp.start_point)), 
					cycle => units_per_cycle);
		end if;

		if get_x (arc_tmp.end_point) = zero and get_y (arc_tmp.end_point) = zero then
			result.angle_end := 0.0;
		else
			--result.angle_end := to_degrees (type_float_internal (arctan (
					--y => type_float_internal (get_y (arc_tmp.end_point)),
					--x => type_float_internal (get_x (arc_tmp.end_point)))));
			result.angle_end := arctan (
					y => type_float_internal (get_y (arc_tmp.end_point)),
					x => type_float_internal (get_x (arc_tmp.end_point)),
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
		return type_arc'class 
	is
		result : type_arc;
		x, y : type_float_internal;
		--offset : constant type_distance_relative := (get_x (arc.center), get_y (arc.center));
		offset : constant type_distance_relative := to_distance_relative (arc.center);
	begin
		result.center := to_point (arc.center);
		result.direction := arc.direction;

		-- start point:
		x := type_float_internal (arc.radius) * cos (type_float_internal (arc.angle_start), units_per_cycle);
		y := type_float_internal (arc.radius) * sin (type_float_internal (arc.angle_start), units_per_cycle);
		--result.start_point := type_point (set (type_distance (x), type_distance (y)));
		result.start_point := set (to_distance (x), to_distance (y));
		move_by (result.start_point, offset);
		
		-- end point:
		x := type_float_internal (arc.radius) * cos (type_float_internal (arc.angle_end), units_per_cycle);
		y := type_float_internal (arc.radius) * sin (type_float_internal (arc.angle_end), units_per_cycle);
		--result.end_point := type_point (set (type_distance (x), type_distance (y)));
		result.end_point := set (to_distance (x), to_distance (y));
		move_by (result.end_point, offset);
		
		return result;
	end to_arc;
	



	
	
	function get_boundaries (
		arc			: in type_arc;
		line_width	: in type_distance_positive) 
		return type_boundaries 
	is
		half_width : constant type_float_internal_positive := type_float_internal (line_width) * 0.5;
		
		result : type_boundaries; -- to be returned

		-- normalize the given arc
		arc_norm : type_arc := type_arc (normalize_arc (arc));

		-- Calculate the radius of the arc:
		radius : constant type_float_internal_positive := get_radius_start (arc_norm);

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
		q_start := get_quadrant (to_vector (arc_norm.start_point));
		q_end   := get_quadrant (to_vector (arc_norm.end_point));

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
		move_by (result, to_offset (arc.center));

		-- extend the boundaries by half the line width;
		result.smallest_x := result.smallest_x - half_width;
		result.smallest_y := result.smallest_y - half_width;

		result.greatest_x := result.greatest_x + half_width;
		result.greatest_y := result.greatest_y + half_width;
		
		return result;
	end get_boundaries;

	
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

		distance_center_to_point : constant type_float_internal :=
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


	function on_arc (
		arc		: in type_arc;
		point	: in type_point)
		return boolean
	is begin
		return on_arc (arc, to_vector (point));
	end on_arc;


	
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
				radius => get_radius_start (arc));

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

	
	function arc_end_point (
		center		: in type_point;
		start_point	: in type_point;	
		angle 		: in type_angle) -- unit is degrees
		return type_point
	is						
		arc : type_arc;

		radius : type_float_internal;
		angle_start, angle_end : type_angle; -- CS type_angle_positive ?
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
			angle_start := arctan (
					y => type_float_internal (get_y (arc.start_point)),
					x => type_float_internal (get_x (arc.start_point)),
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


	function move_to (
		arc			: in type_arc;
		position	: in type_point)
		return type_arc'class
	is
		a : type_arc := arc;
	begin
		a.move_to (position);
		return a;
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
		rotation	: in type_rotation) 
	is begin
		rotate_by (arc.center, rotation);
		rotate_by (arc.start_point, rotation);
		rotate_by (arc.end_point, rotation);
	end;


	--function split_arc (arc_in : in type_arc) 
		--return type_arcs
	--is
		---- normalize the given arc so that its direction is always CCW
		--arc : type_arc := type_arc (normalize_arc (arc_in));
		
		---- the x and y-position of the center, start and end point of the given arc:
		--CX : constant type_distance := get_x (arc.center);
		--CY : constant type_distance := get_y (arc.center);			
		--SX : type_distance;
		--EX : type_distance;

		---- the radius of the given arc:
		--R  : constant type_float_internal_positive := get_radius_start (arc);

		---- The connecting points between the resulting arcs are where an
		---- imaginary vertical line intersects an imaginary circle at its highest and 
		---- lowest point:
		--PU : constant type_point := type_point (set (CX, CY + R));
		--PL : constant type_point := type_point (set (CX, CY - R));
		
		---- the boundaries of the given arc:
		--by : constant type_boundaries := get_boundaries (arc, zero);

		--S_left, E_left : boolean := false;

		--function left_of_center (x : in type_distance) return boolean is begin
			--if x < CX then return true; -- x is left of center.x
			--else return false; -- x is equal or right of center.x or 
			--end if;
		--end left_of_center;

		---- There can be up to 3 segments after splitting the arc.
		---- All arc segments have the same center and direction:
		--result_2 : type_arcs (1..2);
		--result_3 : type_arcs (1..3);

		---- If the arc breaks up into 3 arc fragments, then it may
		---- happen that the first or the last fragment has zero length.
		---- In this case this function removes such a fragment and 
		---- returns two fragments instead.
		---- Otherwise the three fragments are untouched.
		--function remove_useless_fragments return type_arcs is begin
			--if zero_length (result_3 (1)) then
				--result_2 (1) := result_3 (2);
				--result_2 (2) := result_3 (3);
				--return result_2;
				
			--elsif zero_length (result_3 (3)) then
				--result_2 (1) := result_3 (1);
				--result_2 (2) := result_3 (2);
				--return result_2;
			--else
				--return result_3;
			--end if;
		--end remove_useless_fragments;
		
	--begin -- split_arc

		---- test whether the arc can be split at all:
		--if by.smallest_x < CX and by.greatest_x > CX then
			---- arc extends to the right and to the left of its center

			---- get x of start and end point:
			--SX := get_x (arc.start_point);
			--EX := get_x (arc.end_point);

			---- get position of start and end point (relative to center):
			--S_left := left_of_center (SX);
			--E_left := left_of_center (EX);
			
			--if S_left then
				--if E_left then
					---- start point is below end point

					---- the lower left segment:
					--result_3 (1) := (
						--center		=> arc.center, 
						--start_point	=> arc.start_point,
						--end_point	=> PL,
						--direction 	=> CCW);

					---- the segment on the right
					--result_3 (2) := (
						--center		=> arc.center, 
						--start_point	=> PL,
						--end_point	=> PU,
						--direction 	=> CCW);

					---- the upper left segment:
					--result_3 (3) := (
						--center		=> arc.center, 
						--start_point	=> PU,
						--end_point	=> arc.end_point,
						--direction 	=> CCW);

					--return remove_useless_fragments;
					
				--else -- end point is on the right
					---- the segment on the left:
					--result_2 (1) := (
						--center		=> arc.center, 
						--start_point	=> arc.start_point,
						--end_point	=> PL,
						--direction 	=> CCW);

					---- the segment on the right
					--result_2 (2) := (
						--center		=> arc.center, 
						--start_point	=> PL,
						--end_point	=> arc.end_point,
						--direction 	=> CCW);

					---- return segment 1 and 2
					--return result_2;
				--end if;

				
			--else -- start point is on the right
				--if E_left then

					---- the segment on the right:
					--result_2 (1) := (
						--center		=> arc.center, 
						--start_point	=> arc.start_point,
						--end_point	=> PU,
						--direction 	=> CCW);

					---- the segment on the left
					--result_2 (2) := (
						--center		=> arc.center, 
						--start_point	=> PU,
						--end_point	=> arc.end_point,
						--direction 	=> CCW);

					---- return segment 1 and 2
					--return result_2;

					
				--else
					---- end point is on the right
					
					---- start point is above end point

					---- the upper right segment:
					--result_3 (1) := (
						--center		=> arc.center, 
						--start_point	=> arc.start_point,
						--end_point	=> PU,
						--direction 	=> CCW);

					---- the segment on the left
					--result_3 (2) := (
						--center		=> arc.center, 
						--start_point	=> PU,
						--end_point	=> PL,
						--direction 	=> CCW);

					--result_3 (3) := (
						--center		=> arc.center, 
						--start_point	=> PL,
						--end_point	=> arc.end_point,
						--direction 	=> CCW);

					--return remove_useless_fragments;
				--end if;
			--end if;
			
		--else
			--raise constraint_error with "can not split " & to_string (arc) & " !";
		--end if;

	--end split_arc;



	
-- CIRCLE


	function to_string (circle : in type_circle) return string is begin
		return
			"circle: C:" & to_string (circle.center) 
			& " / R:" & to_string (circle.radius);
	end to_string;

	
	function to_radius (
		r : in string)
		return type_float_internal_positive
	is begin
		return pac_geometry_1.to_distance (r);
	end to_radius;


	function to_diameter (
		d : in string)
		return type_float_internal_positive
	is begin
		return pac_geometry_1.to_distance (d);
	end to_diameter;

	
	function split_circle (
		circle_in : in type_circle) 
		return type_arcs
	is
		center : constant type_vector := to_vector (circle_in.center);
		
		-- the x and y-position of the center of the given circle:
		CX : constant type_float_internal := center.x;
		CY : constant type_float_internal := center.y;
		R  : constant type_float_internal_positive := circle_in.radius;

		-- The connecting points between the resulting arcs are where an
		-- imaginary vertical line intersects the circle at its highest and 
		-- lowest point:
		PU : constant type_point := to_point (set (CX, CY + R));
		PL : constant type_point := to_point (set (CX, CY - R));

		-- There will be only 2 arcs.
		result : type_arcs (1 .. 2);
	begin
		-- the arc on the left:
		result (1) := (center => circle_in.center, start_point => PU, 
						end_point => PL, direction => CCW);

		-- the arc on the right:
		result (2) := (center => circle_in.center, start_point => PL,
						end_point => PU, direction => CCW);			

		return result;
	end split_circle;


	--function to_arc (
		--c : in type_circle)
		--return type_arc'class
	--is
		--a1 : type_arc_angles;
	--begin
		--a1.center := to_vector (c.center);
		--a1.radius := c.radius;
		--a1.angle_start := 0.0;
		--a1.angle_end := 360.0;
		--a1.direction := CCW;
		
		--return to_arc (a1);
	--end to_arc;

	
	function get_distance_to_circumfence (
		circle	: in type_circle;
		point	: in type_point)
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
		point	: in type_point)
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
		
		dd : type_float_internal;
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
		return type_float_internal
	is
		result : type_float_internal := 0.0;

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

	
	procedure move_by (
		circle	: in out type_circle;
		offset	: in type_distance_relative)
	is begin
		move_by (point	=> circle.center,	offset => offset);
	end move_by;


	procedure move_to (
		circle		: in out type_circle;
		position	: in type_point)
	is begin
		move_to (circle.center,	position);
	end move_to;


	function move_to (
		circle		: in type_circle;
		position	: in type_point)
		return type_circle'class
	is 
		result : type_circle := circle;
	begin
		move_to (result, position);
		return result;
	end move_to;

	
	procedure mirror (
		circle		: in out type_circle;
		axis		: in type_axis_2d) 
	is begin
		mirror (circle.center, axis);
	end mirror;

	
	procedure rotate_by (
		circle		: in out type_circle;
		rotation	: in type_rotation) 
	is begin
		rotate_by (circle.center, rotation);
	end;

	
	function get_boundaries (
		circle		: in type_circle;
		line_width	: in type_distance_positive)						
		return type_boundaries 
	is
		result : type_boundaries;

		half_width : constant type_float_internal_positive := type_float_internal (line_width) * 0.5;
	begin
		-- X axis
		result.smallest_x := type_float_internal (get_x (circle.center)) - circle.radius;
		result.greatest_x := type_float_internal (get_x (circle.center)) + circle.radius;

		-- Y axis
		result.smallest_y := type_float_internal (get_y (circle.center)) - circle.radius;
		result.greatest_y := type_float_internal (get_y (circle.center)) + circle.radius;

		
		-- extend the boundaries by half the line width;
		result.smallest_x := result.smallest_x - half_width;
		result.smallest_y := result.smallest_y - half_width;

		result.greatest_x := result.greatest_x + half_width;
		result.greatest_y := result.greatest_y + half_width;

		--put_line (to_string (result));
		return result;
	end get_boundaries;

	
	function on_circle (
		circle		: in type_circle;
		point		: in type_point)
		return boolean 
	is
		-- the distance from center to point:
		DCP: constant type_float_internal_positive := 
			get_distance_total (point, circle.center);
	begin
		if abs (DCP - circle.radius) <= type_float_internal (type_distance'small) then

			-- Point is on circumfence of circle.
			return true;
		else
			return false; 
		end if;
	end on_circle;


	function get_point_to_circle_status (
		circle		: in type_circle;
		point		: in type_point)
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
		i : constant type_intersection_of_line_and_circle := get_intersection (circle, l);
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
		return type_float_internal_positive
	is
		result : type_float_internal_positive := 0.0;

		--debug : constant boolean := false;
		--debug : constant boolean := true;
		
		-- the distance from circumfence to start of line:
		ds : type_float_internal_positive;
		
		-- the distance from circumfence to end of line:
		de : type_float_internal_positive;

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
			result := get_distance (dp);
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
		return type_float_internal_positive
	is
		result : type_float_internal_positive := 0.0;

		dp : type_distance_polar;
	begin
		--new_line;
		--put_line (to_string (circle));
		--put_line ("-" & to_string (arc));

		--log (text => "circle" & to_string (circle));
		--log (text => "arc" & to_string (arc));
		
		dp := get_shortest_distance (arc, circle.center);
		
		result := get_absolute (dp) - circle.radius;
		return result;
	end get_distance;



	function get_distance (
		circle_1	: in type_circle'class;
		circle_2	: in type_circle'class)
		return type_float_internal_positive
	is
		result : type_float_internal_positive := 0.0;
		dp : type_distance_polar;
	begin
		dp := get_distance (circle_1.center, circle_2.center);
		result := get_absolute (dp) - circle_1.radius - circle_2.radius;
		return result;
	end get_distance;

	
	
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

			--intersection_1 := type_point (set (to_distance (x), to_distance (y)));
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
			--intersection_1 := type_point (set (to_distance (x), to_distance (y)));
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
			--intersection_2 := type_point (set (to_distance (x), to_distance (y)));
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



	
	


	
-- PATH FROM POINT TO POINT
	
	function to_path (
		start_point, end_point	: in type_point;
		style					: in type_bend_style)
		return type_path
	is
		-- The area required for the path is a rectangle.
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
			S1 : constant type_vector := get_start_vector (first_line);

			-- first line direction vector:
			R1 : constant type_vector := get_direction_vector (first_line);

			-- second line start vector:
			S2 : constant type_vector := get_start_vector (second_line);

			-- second line direction vector
			R2 : constant type_vector := get_direction_vector (second_line);

			-- scratch variables:
			a, b, c, d, e, f, g : type_float_internal;
			lambda_1, lambda_2 : type_float_internal;

			-- location vector of intersection
			I : type_vector;
		begin
			-- The direction vector of the first line can be zero in x (R1.x).
			-- In order to avoid division by zero we must switch between
			-- two ways to find the intersection:
			if get_x (R1) /= 0.0 then
				a := get_y (S1);
				b := get_x (S2) * get_y (R1) / get_x (R1);
				c := get_x (S1) * get_y (R1) / get_x (R1);
				d := get_y (S2);
				e := get_y (R2);
				f := get_x (R2) * get_y (R1) / get_x (R1);
				g := 1.0 / (e - f);

				lambda_2 := (a + b - c - d) * g;

				I := add (S2, scale (R2, lambda_2));
			else
				a := get_y (S2);
				b := get_x (S1) * get_y (R2) / get_x (R2);
				c := get_x (S2) * get_y (R2) / get_x (R2);
				d := get_y (S1);
				e := get_y (R1);
				f := get_x (R1) * get_y (R2) / get_x (R2);
				g := 1.0 / (e - f);

				lambda_1 := (a + b - c - d) * g;

				I := add (S1, scale (R1, lambda_1));
			end if;
			
			bend_point := to_point (I);
		end compute_bend_point;
		
	begin -- to_path
		
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
								sup_start := set (get_x (start_point) + ds, get_y (start_point));
							else -- taller than wide
								sup_start := set (get_x (start_point), get_y (start_point) + ds);
							end if;

							-- compute support point near end point:
							-- The second line must run angled from end point:
							if dx > zero then -- to the right
								if dy > zero then -- upwards
									sup_end := set (get_x (end_point) + ds, get_y (end_point) + ds);
									--  45 degree
								else
									sup_end := set (get_x (end_point) + ds, get_y (end_point) - ds);
									-- -45 degree
								end if;
							else -- to the left
								if dy > zero then -- upwards
									sup_end := set (get_x (end_point) - ds, get_y (end_point) + ds);
									-- 135 degree
								else
									sup_end := set (get_x (end_point) - ds, get_y (end_point) - ds);
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
									sup_start := set (get_x (start_point) + ds, get_y (start_point) + ds);
									--  45 degree
								else -- downwards
									sup_start := set (get_x (start_point) + ds, get_y (start_point) - ds);
									-- -45 degree
								end if;
							else -- to the left
								if dy > zero then -- upwards
									sup_start := set (get_x (start_point) - ds, get_y (start_point) + ds);
									-- 135 degree
								else -- downwards
									sup_start := set (get_x (start_point) - ds, get_y (start_point) - ds);
									-- 225 degree
								end if;
							end if;

							-- compute support point near end point:
							-- The second line must run straight from end point:
							if abs (dx) > abs (dy) then -- wider than tall
								sup_end := set (get_x (end_point) + ds, get_y (end_point));
								-- horizontally
							else -- taller than wide
								sup_end := set (get_x (end_point), get_y (end_point) + ds);
								-- vertically
							end if;

							compute_bend_point;
						end if;
						
					when VERTICAL_THEN_HORIZONTAL =>
						-- Compute support point near start point:
						-- The first line must run vertically from start point:
						sup_start := set (get_x (start_point), get_y (start_point) + ds);
						-- vertically

						-- The second line must run horizontally from end point:
						sup_end := set (get_x (end_point) + ds, get_y (end_point));
						-- horizontally

						compute_bend_point;
						
					when HORIZONTAL_THEN_VERTICAL =>
						-- Compute support point near start point:
						-- The first line must run horizontal from start point:
						sup_start := set (get_x (start_point) + ds, get_y (start_point));
						-- horizontally

						-- compute support point near end point:
						-- The second line must run vertically from end point:
						sup_end := set (get_x (end_point), get_y (end_point) + ds);
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

	end to_path;

	
	procedure next_bend_style (path : in out type_path_live) is
		i : constant natural := type_bend_style'pos (path.bend_style);
		-- i points now to the current bend style

		-- get the index of the last available bend style:
		max : constant natural := type_bend_style'pos (type_bend_style'last);
	begin
		if i < max then
			-- jump to next bend style
			path.bend_style := type_bend_style'succ (type_bend_style'val (i));
		else 
			-- After the last bend style, jump back to the first bend style:
			path.bend_style := type_bend_style'first;
		end if;
	end next_bend_style;

	
	
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
		point		: in type_point;
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
			when X => position.place.x := value;
			when Y => position.place.y := value;
		end case;
	end set;


	procedure set (
		position	: in out type_position;
		place		: in type_point)
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

	
end et_geometry_2;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
