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


	procedure round (
		f : in out type_float_internal;
		a : in type_rounding_accuracy)
	is 
		base : constant type_float_internal := 10.0;
	begin
		--put_line ("given   :" & type_float_internal'image (f));
		--put_line ("accuracy:" & positive'image (a));

		f := type_float_internal'rounding (f * base**a) * base**(-a); 
		
		--put_line ("rounded :" & type_float_internal'image (f));
		--new_line;
	end round;


	function round (
		f : in type_float_internal;
		a : in type_rounding_accuracy)
		return type_float_internal
	is 
		base : constant type_float_internal := 10.0;
	begin
		return type_float_internal'rounding (f * base**a) * base**(-a); 
	end round;


	
	function to_string (f : in type_float_internal) return string is begin
		return type_float_internal'image (f);
	end;
	

	function get_info (editor: in string)
		return string 
	is 
		use ada.characters.latin_1;
		distance_digits_total : constant positive := type_distance'digits;
		distance_digits_right : constant positive := type_distance'scale;

		distance_coarse_digits_total : constant positive := type_distance_coarse'digits;
		distance_coarse_digits_right : constant positive := type_distance_coarse'scale;

		rotation_digits_total : constant positive := type_rotation'digits;
		rotation_digits_right : constant positive := type_rotation'scale;
	begin
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
		& lf & "distance coarse [mm]"
		& lf & "min:        " & type_distance_coarse'image (type_distance_coarse'first)
		& lf & "max:        " & type_distance_coarse'image (type_distance_coarse'last)
		& lf & "resolution: " & type_distance_coarse'image (type_distance_coarse'small)
		& lf & "digits"
		& lf & "left:       " & positive'image (distance_coarse_digits_total - distance_coarse_digits_right)
		& lf & "right:      " & positive'image (distance_coarse_digits_right)
		& lf & "total:      " & positive'image (type_distance_coarse'digits)
		& lf
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

	

	function to_distance (dd : in string) 
		return type_distance 
	is begin
		return type_distance'value (dd);

		exception when event: others =>
			raise syntax_error_2 with 
				"ERROR: Expect a distance instead of " 
				& enclose_in_quotes (dd) & " !";
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


	function to_string (d_coarse : in type_distance_coarse) 
		return string
	is begin
		if d_coarse < 0.0 then
			return space & type_distance_coarse'image (d_coarse);
		else
			return type_distance_coarse'image (d_coarse);
		end if;
		-- CS suppress trailing zeros
	end to_string;


	function to_distance (df : in string)
		return type_float_internal
	is begin
		return type_float_internal'value (df);
	end to_distance;

	
	function to_distance (f : in type_float_internal)
		return type_distance 
	is
		use pac_distance_io;
		
		d1 : type_distance;
		d2 : type_float_internal;

		f1 : constant type_float_internal := 5.0 * type_float_internal (type_distance'small);
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

	
	function round (
		d_fine	: in type_distance;
		mode	: in type_rounding_mode := rounding_mode_default) 
		return type_distance_coarse
	is
		d_coarse : type_distance_coarse := type_distance_coarse (d_fine);
		d_delta : type_distance_positive;

		procedure do_it is begin
			if d_fine > 0.0 then
				d_coarse := d_coarse + type_distance_coarse'small;
			else
				d_coarse := d_coarse - type_distance_coarse'small;
			end if;
		end do_it;
		
	begin
		d_delta := abs (d_fine) - abs (type_distance (d_coarse));

		case mode is
			when UP =>
				if d_delta > zero then
					do_it;
				end if;
					
			when DOWN =>
				null;
				
			when BANKERS_RULE =>
				--if d_delta >= 500_000.0 * type_distance'small then
				if d_delta >= 0.5 * type_distance (type_distance_coarse'small) then
					do_it;
				end if;
		end case;
		
		return d_coarse;
	end round;

	
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

	
	function to_positive_rotation (
		rotation	: in type_rotation)
		return type_rotation_positive
	is begin
		if rotation < 0.0 then
			return 360.0 + rotation;
		else
			return rotation;
		end if;
	end to_positive_rotation;


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

	
	--function "=" (left, right : in type_point) return boolean is begin
		--if abs (left.x - right.x) <= type_distance'small 
		--and abs (left.y - right.y) <= type_distance'small
		--then
			--return true;
		--else
			--return false;
		--end if;
	--end;
	
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
	end;


	--procedure append_point (
		--points	: in out type_points;
		--point	: in type_point)
	--is begin
		--pac_points.append (points.points, point);
	--end append_point;


	function to_string (points : in pac_points.list) return string is
		use pac_points;
		use ada.strings.unbounded;
		
		result : unbounded_string;
		
		procedure query_point (p : in pac_points.cursor) is begin
			result := result & " " 
				& trim (to_string (get_x (element (p))), left)
				& "/"
				& trim (to_string (get_y (element (p))), left);
		end query_point;
			
	begin
		points.iterate (query_point'access);

		return to_string (result);
	end to_string;
	

	procedure splice_points (
		points_target : in out pac_points.list;
		points_source : in pac_points.list)
	is 
		scratch : pac_points.list := points_source;
	begin
		pac_points.splice (
			target	=> points_target,
			before	=> pac_points.no_element,
			source	=> scratch);
	end splice_points;
	

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


	procedure sort_by_distance (
		points 		: in out pac_points.list;
		reference	: in type_point'class)
	is
		type type_item is record
			point		: type_point;
			distance	: type_float_internal_positive;
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
		
		items : pac_items.list;

		
		procedure query_point (p : in pac_points.cursor) is 
			use pac_points;
			d : type_distance_polar;
		begin
			d := get_distance (type_point (reference), type_point (element (p)));
			
			items.append (new_item => (
				point		=> element (p),
				distance	=> get_absolute (d)));
		end query_point;

		

		package pac_sorting is new pac_items.generic_sorting;
		use pac_sorting;
		

		procedure query_item (i : in pac_items.cursor) is begin
			points.append (element (i).point);
		end query_item;
		
		
	begin
		-- Collect points and their distance to the reference
		-- in list "items":
		points.iterate (query_point'access);

		-- Sort items by distance to reference:
		sort (items);

		-- The old points are no longer required:
		points.clear;
		-- New points will be appended here.
		

		-- Traverse items and append them one by one to the
		-- list of points:
		items.iterate (query_item'access);
	end sort_by_distance;
	
	
	function to_string (point : in type_point) return string is begin
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
		return type_point'class 
	is		
		p : type_point := (
			x => (round (point.x, grid.x)),
			y => (round (point.y, grid.y)));
	begin
		return p;
	end;

	
	
	function round (point : in type_point)
		return type_point'class
	is
		r : type_point := (
			x => type_distance (round (point.x)),
			y => type_distance (round (point.y)));
	begin
		return r;
	end round;


	procedure round (
		point : in out type_point)
	is begin
		point.x := type_distance (round (point.x));
		point.y := type_distance (round (point.y));
	end round;

	
	
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
		x,y : in type_float_internal)
		return type_distance_relative
	is begin
		return (to_distance (x), to_distance (y));
	end to_distance_relative;
	

	
	function to_distance_relative (p : in type_point)
		return type_distance_relative
	is begin
		return (p.x, p.y);
	end to_distance_relative;


	function get_rotation (
		point : in type_point) 
		return type_rotation 
	is
		x : constant type_float_internal := type_float_internal (point.x);
		y : constant type_float_internal := type_float_internal (point.y);
	begin
		-- NOTE: If x and y are zero then the arctan operation is not possible. 
		-- In this case we assume the resulting angle is zero.
		if x = 0.0 and y = 0.0 then
			return zero_rotation;
		else
			return to_rotation (arctan (y, x, units_per_cycle));
		end if;
	end get_rotation;



	procedure rotate_by (
		point		: in out type_point;
		rotation	: in type_rotation) 
	is			
		angle_out			: type_rotation; -- degrees
		distance_to_origin	: type_float_internal; -- unit is mm
		scratch				: type_float_internal;
	begin
		-- Do nothing if the given rotation is zero.
		if rotation /= 0.0 then

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
			
			-- compute the current angle of the given point (in degrees)

			if get_x (point) = zero then
				if get_y (point) > zero then
					angle_out := 90.0;
					
				elsif get_y (point) < zero then
					angle_out := -90.0;
					
				else
					angle_out := 0.0;
				end if;

			elsif get_y (point) = zero then
				if get_x (point) > zero then
					angle_out := 0.0;
					
				elsif get_x (point) < zero then
					angle_out := 180.0;
					
				else
					angle_out := 0.0;
				end if;

			else
				-- neither x nor y of point is zero
				--angle_out := arctan (
				angle_out := to_rotation (arctan (
					x		=> type_float_internal (get_x (point)),
					y		=> type_float_internal (get_y (point)),
					cycle	=> units_per_cycle));
				
			end if;

			-- Compute new angle by adding current angle and given angle.
			angle_out := add (angle_out, rotation);

			-- compute new x   -- (cos angle_out) * distance_to_origin
			scratch := cos (type_float_internal (angle_out), units_per_cycle);
			
			set (
				axis	=> X, 
				point	=> point, 
				value	=> to_distance (scratch * distance_to_origin)
				);

			-- compute new y   -- (sin angle_out) * distance_to_origin
			scratch := sin (type_float_internal (angle_out), units_per_cycle);
			
			set (
				axis	=> Y,
				point	=> point,
				value	=> to_distance (scratch * distance_to_origin)
				);
	
		end if; -- if angle not zero			
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
		return type_point'class
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
		return type_point'class
	is 
		result : type_point;					
	begin
		result.x := to_distance (dd => x);
		result.y := to_distance (dd => y);
		return result;

		-- CS exception handler
	end to_point;

	
	
	function invert (
		d : in type_distance_relative)
		return type_distance_relative
	is begin
		return (-1.0 * d.x, -1.0 * d.y);
	end invert;

	
	

	
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

	
	function to_string (boundaries : in type_boundaries) return string is begin
		return "boundaries: SX:" & to_string (boundaries.smallest_x) 
			& " / GX:" & to_string (boundaries.greatest_x)
			& " / SY:" & to_string (boundaries.smallest_y)
			& " / GY:" & to_string (boundaries.greatest_y);
	end;

	
	function intersect (
		boundaries_one : in type_boundaries;
		boundaries_two : in type_boundaries)
		return boolean
	is begin
		if	boundaries_two.greatest_x > boundaries_one.smallest_x
		and boundaries_two.smallest_x < boundaries_one.greatest_x
		then -- boundaries intersect in x-direction

			if	boundaries_two.greatest_y > boundaries_one.smallest_y
			and boundaries_two.smallest_y < boundaries_one.greatest_y
			then -- boundaries intersect in y-direction
				return true;
			else
				return false;
			end if;
			
		else
			return false;
		end if;
		
	end intersect;


	function get_intersection (
		boundaries_one : in type_boundaries;
		boundaries_two : in type_boundaries)
		return type_boundaries_intersection
	is
		i : type_boundaries;
	begin
		if intersect (boundaries_one, boundaries_two) then

			--log (text => "b1" & to_string (boundaries_one));
			--log (text => "b2" & to_string (boundaries_two));
			
			i.smallest_x := get_greatest (boundaries_one.smallest_x, boundaries_two.smallest_x);
			i.greatest_x := get_smallest (boundaries_one.greatest_x, boundaries_two.greatest_x);

			i.smallest_y := get_greatest (boundaries_one.smallest_y, boundaries_two.smallest_y);
			i.greatest_y := get_smallest (boundaries_one.greatest_y, boundaries_two.greatest_y);

			--log (text => "b " & to_string (i));
			
			return (exists => true, intersection => i);
		else
			return (exists => false);
		end if;
	end get_intersection;


	
	-- Adds two boundaries.
	procedure add (
		boundaries_one : in out type_boundaries;
		boundaries_two : in type_boundaries) is
	begin
-- 			if boundaries_two.smallest_x < boundaries_one.smallest_x , smallest_y : type_distance := type_distance'last;
-- 			greatest_x, greatest_y : type_distance := type_distance'first;
		null; -- CS
	end; 

	
	function get_boundaries (
		point_one	: in type_point;
		point_two	: in type_point;
		width		: in type_distance_positive) 
		return type_boundaries
	is
		result : type_boundaries;

		half_width : constant type_float_internal_positive := type_float_internal (width) * 0.5;

		--p1x : type_float_internal renames type_float_internal (point_one.x);
		--p1y : type_float_internal renames point_one.y;

		--p2x : type_float_internal renames point_two.x;
		--p2y : type_float_internal renames point_two.y;
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

	
	procedure move_by (
		boundaries	: in out type_boundaries;
		offset		: in type_distance_relative;
		clip		: in boolean := false)
	is 
		sx : constant type_float_internal := boundaries.smallest_x + type_float_internal (offset.x);
		gx : constant type_float_internal := boundaries.greatest_x + type_float_internal (offset.x);
		sy : constant type_float_internal := boundaries.smallest_y + type_float_internal (offset.y);
		gy : constant type_float_internal := boundaries.greatest_y + type_float_internal (offset.y);

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

	
	--procedure rotate (
		--boundaries	: in out type_boundaries;
		--rotation	: in type_rotation) 
	--is

		---- The boundaries are basically a rectangle with those four corners:
		--corners : array (positive range 1 .. 4) of type_point;

		---- backup the position of the topleft corner of the boundaries:
		--topleft_before_rotation : constant type_point := (
				--x	=> boundaries.smallest_x,
				--y	=> boundaries.greatest_y);

		--topleft_after_rotation : type_point;
		
	--begin -- rotate
		---- Set the corner points according to the given boundaries:
		--corners (1)	:= (boundaries.smallest_x, boundaries.greatest_y);
		--corners (2) := (boundaries.greatest_x, boundaries.greatest_y);
		--corners (3) := (boundaries.smallest_x, boundaries.smallest_y);
		--corners (4) := (boundaries.greatest_x, boundaries.smallest_y);

		---- After the rotation the boundaries may become wider than actually
		---- required.
		
		---- The boundaries are always relative to a certain origin that
		---- sits somewhere inside the rectangle. The four corners are now rotated
		---- around the origin by the given angle:
		--for c in corners'first .. corners'last loop
			--rotate_by (corners (c), rotation);
		--end loop;

		---- reset boundaries
		--boundaries := boundaries_default;
		
		--for c in corners'first .. corners'last loop
			
			---- find the smallest x
			--if corners (c).x < boundaries.smallest_x then
				--boundaries.smallest_x := corners (c).x;
			--end if;

			---- find the greatest x
			--if corners (c).x > boundaries.greatest_x then
				--boundaries.greatest_x := corners (c).x;
			--end if;

			---- find the smallest y
			--if corners (c).y < boundaries.smallest_y then
				--boundaries.smallest_y := corners (c).y;
			--end if;

			---- find the greatest y
			--if corners (c).y > boundaries.greatest_y then
				--boundaries.greatest_y := corners (c).y;
			--end if;
			
		--end loop;

		---- After the rotation we get a new topleft position:
		--topleft_after_rotation := (
			--x	=> boundaries.smallest_x,
			--y	=> boundaries.greatest_y);

		---- The difference in x and y between topleft_before_rotation
		---- and topleft_after_rotation:
		--boundaries.distance_of_topleft_to_default := type_point 
			--(topleft_before_rotation - topleft_after_rotation);
		
	--end rotate;

	
	function to_string (rectangle : in type_rectangle) return string is begin
		return "rectangle " --to_string (set (rectangle.x, rectangle.y))
			& "x/y " & to_string (rectangle.x) & "/" & to_string (rectangle.y)
			& " width" & to_string (rectangle.width)
			& " height" & to_string (rectangle.height);
	end;

	
	procedure move_by (
		rectangle	: in out type_rectangle;
		offset		: in type_distance_relative)
	is begin
		rectangle.x := rectangle.x + type_float_internal (offset.x);
		rectangle.y := rectangle.y + type_float_internal (offset.y);
	end move_by;

	
	function intersects (rect1, rect2 : type_rectangle) return boolean is begin
		return not (
			rect1.x > rect2.x + rect2.width            --  r1 on the right of r2
			or else rect2.x > rect1.x + rect1.width    --  r2 on the right of r1
			or else rect1.y > rect2.y + rect2.height   --  r1 below r2
			or else rect2.y > rect1.y + rect1.height); --  r1 above r2
	end intersects;



	
	function mil_to_distance (mil : in string) return type_distance is
		distance_mil : type_float_internal := type_float_internal'value (mil);
	begin
		return to_distance (distance_mil * (25.4 * 0.001));
	end mil_to_distance;

	
	

	function distance_to_mil (distance : in type_distance) return string is
		scratch : type_float_internal;
	begin
		scratch := type_float_internal (distance) * 1000.00 / 25.4;
		return to_string (to_distance (scratch));
	end;

	
	function set (
		x, y : in type_position_axis)
		return type_point'class 
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
		point	: in out type_point'class;
		position: in type_point) 
	is begin
		point.x := position.x;
		point.y := position.y;
	end;

	
	function get_quadrant (
		point : in type_point) 
		return type_quadrant
	is begin
		if point.x >= zero then -- we are right of the y-axis or on top of it
			if point.y >= zero then -- we are above the x-axis or on top of it
				return ONE; 
			else -- we are below the x-axis
				return FOUR;
			end if;
			
		else -- we are left of the y-axis
			if point.y >= zero then -- we are above the x-axis or on top of it
				return TWO;
			else -- we are below the x-axis
				return THREE;
			end if;
		end if;
	end get_quadrant;

	
	function invert (
		point : in type_point'class)
		return type_point'class 
	is
		pi : type_point'class := point;
	begin
		pi.x := - pi.x;
		pi.y := - pi.y;
		return pi;
	end invert;

	
	function invert (
		point	: in type_point;
		axis	: in type_axis_2d)
		return type_point'class
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
		destination	: in type_point'class) 
	is begin
		point.x := destination.x;
		point.y := destination.y;
	end move_to;

	
	function move (
		point		: in type_point;
		direction	: in type_rotation;
		distance	: in type_distance_positive;
		clip		: in boolean := false)
		return type_point'class 
	is 			
		delta_x, delta_y : type_float_internal := 0.0;

		rx, ry : type_distance;			
		result : type_point;			
	begin
		-- sin (direction) * distance = delta y
		-- cos (direction) * distance = delty x

		delta_y := sin (type_float_internal (direction), units_per_cycle) * type_float_internal (distance);
		delta_x := cos (type_float_internal (direction), units_per_cycle) * type_float_internal (distance);

		rx := point.x + to_distance (delta_x);
		ry := point.y + to_distance (delta_y);

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

	
	function "+" (point_one, point_two : in type_point) return type_point'class is
		d : type_point;
	begin
		d.x := point_one.x + point_two.x;
		d.y := point_one.y + point_two.y;
		return d;
	end;
	
	function "-" (point_one, point_two : in type_point) return type_point'class is
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
	is
		distance : type_float_internal_positive; -- to be returned
		delta_x, delta_y : type_float_internal := 0.0;
	begin
		if point_one = point_two then
			distance := 0.0;
			
		elsif get_x (point_one) = get_x (point_two) then -- points are in a vertical line
			distance := type_float_internal_positive (abs (get_y (point_two) - get_y (point_one)));
			
		elsif get_y (point_one) = get_y (point_two) then -- points are in a horizontal line
			distance := type_float_internal_positive (abs (get_x (point_two) - get_x (point_one)));
			
		else
			delta_x := type_float_internal (get_x (point_one) - get_x (point_two));
			delta_y := type_float_internal (get_y (point_one) - get_y (point_two));

			distance := sqrt ((delta_x ** 2) + (delta_y ** 2));
		end if;
			
		return distance;
	end get_distance_total;
	

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

	
	function add (left, right : in type_rotation) return type_rotation is
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


	function to_string (
		distance : in type_distance_polar)
		return string
	is begin
		return ("abs:" & to_string (distance.absolute) 
			& " / angle:" & to_string (distance.angle));
	end to_string;
	
	
	function to_polar (
		absolute	: in type_float_internal_positive;
		angle		: in type_rotation)
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
		angle    : in type_rotation)
	is begin
		distance.angle := angle;
	end set_angle;


	procedure reverse_angle (
		distance : in out type_distance_polar)
	is begin
		distance.angle := add (distance.angle, 180.0);
	end reverse_angle;

	
	
	function get_distance (
		point_one, point_two : in type_point) 
		return type_distance_polar 
	is
		result : type_distance_polar;

		delta_x, delta_y : type_float_internal := 0.0;
	begin
		result.absolute := get_distance_total (point_one, point_two);

		-- NOTE: If the total distance between the points is zero then
		-- the arctan operation is not possible. In this case we assume
		-- the resulting angle is zero.
		-- So we do the angle computation only if there is a distance between the points:
		if result.absolute /= 0.0 then
			
			delta_x := type_float_internal (get_x (point_two) - get_x (point_one));
			delta_y := type_float_internal (get_y (point_two) - get_y (point_one));

			result.angle := to_rotation (arctan (
					x 		=> delta_x,
					y		=> delta_y,
					cycle	=> units_per_cycle));
		else
			-- distance is zero
			result.angle := zero_rotation;
		end if;
		
		return result;
	end get_distance;

	
	function get_angle (
		distance : in type_distance_polar) 
		return type_rotation 
	is begin
		return distance.angle;
	end get_angle;

	
	function get_absolute (
		distance : in type_distance_polar) 
		return type_float_internal_positive
	is begin
		return distance.absolute;
	end get_absolute;


	
	function to_string (point : in type_position) return string is begin
		return point_preamble_with_rotation
			& to_string (point.x)
			& axis_separator
			& to_string (point.y)
			& axis_separator
			& to_string (get_rotation (point));
	end;
	

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

	
	function to_string (direction : in type_direction_of_rotation) return string is begin
		return to_lower (type_direction_of_rotation'image (direction));
	end to_string;
		
	function to_direction (direction : in string) return type_direction_of_rotation is begin
		return type_direction_of_rotation'value (direction);
	end to_direction;

	
	function get_direction (rotation : in type_rotation) 
		return type_direction_of_rotation
	is begin
		if rotation < zero_rotation then
			return CW;
		else
			return CCW;
		end if;
	end get_direction;
	

	function reverse_direction (direction : in type_direction_of_rotation)
		return type_direction_of_rotation is
	begin
		case direction is 
			when CW => return CCW;
			when CCW => return CW;
		end case;
	end reverse_direction;

	
	function to_radians (degrees : in type_rotation) return type_float_internal is
		use ada.numerics;
	begin
		return (pi * type_float_internal (degrees)) / (units_per_cycle * 0.5);
	end to_radians;

	
	function to_degrees (radians : in type_float_internal) return type_rotation is
		use ada.numerics;
	begin
		return to_rotation ((units_per_cycle * 0.5 * radians) / pi);
	end to_degrees;

	
	function to_position (
		point		: in type_point;
		rotation	: in type_rotation)
		return type_position'class
	is 
		result : type_position;
	begin
		result.x := point.x;
		result.y := point.y;
		result.rotation := rotation;

		return result;
	end to_position;

	
	procedure set (
		position	: in out type_position;
		rotation	: in type_rotation) 
	is begin
		position.rotation := rotation;
	end;

	
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
		

end et_geometry_1;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
