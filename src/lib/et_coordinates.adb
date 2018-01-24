------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET COORDINATES                          --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 Mario Blunk, Blunk electronic                 --
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

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings;				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.unbounded;

with ada.numerics.generic_elementary_functions;
with et_string_processing;
with et_math;

package body et_coordinates is

	function mil_to_distance (mil : in string; warn_on_negative : boolean := true) 
		return type_distance_xy is
	-- Returns the given mils to type_distance_xy.
		use et_string_processing;
		type type_distance_intermediate is digits 13 range -10000000.0 .. 1000000.0;
		-- unit is mil
		-- CS: refine range and delta if required

		d_in : type_distance_intermediate;
		distance : type_distance_xy; -- the distance to be returned
	begin
		d_in := type_distance_intermediate'value (mil);

		log_indentation_up;
		log ("mil to mm: mil in " & mil, level => 6);
		log_indentation_down;

		distance := type_distance (d_in * (25.4 * 0.001));

		log ("mm out " & type_distance'image (distance), level => 6);
		--log ("mil as distance " & type_distance'image (type_distance (d_in * (25.4 * 0.001))));

		if warn_on_negative then
			if distance < zero_distance then
				log (text => message_warning & "negative coordinates found !");
			end if;
		end if;
		
		return distance;
		
		-- CS: exception handler
	end mil_to_distance;

	function to_string (distance : in type_distance) return string is
	-- Returns the given distance to a string.
	begin
		return trim (type_distance'image (distance), left);
		--return format_distance (distance);
	end to_string;


	function to_string (angle : in type_angle) return string is
	-- Returns the the given angle as string.
		preamble	: constant string (1..5) := "angle";
		suffix		: constant string (1..4) := " deg";
	begin
		return (preamble & type_angle'image (angle) & suffix);
	end to_string;

	procedure warning_angle_greater_90_degrees is
		use et_string_processing;
	begin
		log (et_string_processing.message_warning 
			& "text placed in an angle greater than" 
			& type_angle_90'image (type_angle_90'last));
	end warning_angle_greater_90_degrees;
	
	
	
	function to_string (point : in type_2d_point) return string is
	-- Returns the given point coordinates to a string.
		use et_string_processing;
	begin
		return position_preamble
		-- 	& trim (type_distance'image (point.x), left)
			& to_string (point.x)
			& latin_1.space & axis_separator & latin_1.space
		--	& trim (type_distance'image (point.y), left);
			& to_string (point.y);
	
	end to_string;

	function distance_x (point : in type_2d_point) return type_distance_xy is
	-- Returns the x distance of point from the drawing origin.		
	begin
		--return et_math.round (point.x, accuracy_schematic);
		return point.x;
	end distance_x;

	function distance_y (point : in type_2d_point) return type_distance_xy is
	-- Returns the y distance of point from the drawing origin.
	begin
		--return et_math.round (point.y, accuracy_schematic);
		return point.y;
	end distance_y;

	procedure set_x (point : in out type_2d_point; x : in type_distance_xy) is
	-- Assigns a point the given x position.
		use et_string_processing;
	begin
		point.x := x;
	end set_x;
	
	procedure set_y (point : in out type_2d_point; y : in type_distance_xy) is
	-- Assigns a point the given y position.
	begin
		point.y := y;
	end set_y;

	procedure set (
		point : in out type_2d_point;
		position : in type_2d_point) is
	begin
		point := position;
	end set;
	
	procedure mirror (
		point	: in out type_2d_point;
		axis	: in type_axis)
		is
	begin
		case axis is
			when X =>
				point.y := point.y * (-1.0);
			when Y =>
				point.x := point.x * (-1.0);
		end case;
	end mirror;

	procedure move (
		point	: in out type_2d_point;
		offset	: in type_2d_point)
		is
	begin
		point.x := point.x + offset.x;
		point.y := point.y + offset.y;
	end move;

	procedure rotate (
	-- Rotates the given point by the given angle with the origin as center.
		point	: in out type_2d_point;
		angle	: in type_angle;
		log_threshold : in et_string_processing.type_log_level)
		is

		type type_float_distance is digits 7 range -1000.0 .. 1000.0; -- CS: refine
		package functions_distance is new ada.numerics.generic_elementary_functions (type_float_distance);
		use functions_distance;
		
		type type_float_angle is digits 4 range -719.9 .. 719.9; -- CS: refine			
		package functions_angle is new ada.numerics.generic_elementary_functions (type_float_angle);
		use functions_angle;

		angle_out			: type_float_angle;		-- unit is degrees
		distance_to_origin	: type_float_distance;	-- unit is mm
		scratch				: type_float_distance;

		use et_string_processing;
	begin
		-- Do nothing if the given rotation is zero.
		if angle /= 0.0 then

			-- CS: in order to improve performance some ops can be skipped or simplified
			-- if point.x or point.y are zero
			
			-- compute distance of given point to origin
			if point.x = zero_distance and point.y = zero_distance then
				distance_to_origin := type_float_distance (zero_distance);
			elsif point.x = zero_distance then
				distance_to_origin := type_float_distance (abs (point.y));
			elsif point.y = zero_distance then
				distance_to_origin := type_float_distance (abs (point.x));
			else
				distance_to_origin := sqrt (
					type_float_distance (abs (point.x)) ** type_float_distance (2) 
					+
					type_float_distance (abs (point.y)) ** type_float_distance (2)
					);
			end if;
			
			-- compute the current angle of the given point (in degrees)

			if point.x = zero_distance then
				if point.y > zero_distance then
					angle_out := 90.0;
				elsif point.y < zero_distance then
					angle_out := -90.0;
				else
					angle_out := 0.0;
				end if;

			elsif point.y = zero_distance then
				if point.x > zero_distance then
					angle_out := 0.0;
				elsif point.x < zero_distance then
					angle_out := 180.0;
				else
					angle_out := 0.0;
				end if;

			else
				angle_out := type_float_angle (arctan (
					x => type_float_distance (point.x),
					y => type_float_distance (point.y),
					cycle => type_float_distance (units_per_cycle))
					);
			end if;

			-- Compute new angle by adding current angle and given angle.
			-- This computation depends on the Y axis style. The in the conventional style (Y going upwards positive)
			-- we add the given angle to the current angle. In the old fashioned stlyle (Y going downwards positive)
			-- we subtract the given angle from the current angle.
			log_indentation_up;
			log ("angle in  " & to_string (type_angle (angle_out)), log_threshold);
			if Y_axis_positive = upwards then
				angle_out := angle_out + type_float_angle (angle);
			else
				angle_out := angle_out - type_float_angle (angle);
			end if;
			log ("angle out " & type_float_angle'image (angle_out), log_threshold);
			
	-- 		-- Remove multiturns in angle_out. 
	-- 		CS: no need because angle_out is invisible to the outside world.
	-- 		-- example 1: angle_out =  370 degrees.  370 - 360 =  10. so angle_out equals  10 degrees.
	-- 		-- example 2: angle_out = -370 degrees. -370 + 360 = -10. so angle_out equals -10 degrees.
	-- 		if angle_out > type_float_angle (type_angle'last) then
	-- 			angle_out := angle_out - type_float_angle (units_per_cycle);
	-- 		elsif angle_out < type_float_angle (type_angle'first) then
	-- 			angle_out := angle_out + type_float_angle (units_per_cycle);
	-- 		else
	-- 			null;
	-- 		end if;

			-- compute new x   -- (cos angle_out) * distance_to_origin
			scratch := cos (type_float_distance (angle_out), type_float_distance (units_per_cycle));
			--point.x := type_distance (scratch * distance_to_origin);
			--point.x := et_math.round (float_in => type_distance (scratch * distance_to_origin), accuracy => accuracy_schematic);
			point.x := type_distance (scratch * distance_to_origin);
			--log ("x in sch. " & to_string (point.x), log_threshold);

			-- compute new y   -- (sin angle_out) * distance_to_origin
			scratch := sin (type_float_distance (angle_out), type_float_distance (units_per_cycle));
			--point.y := et_math.round (float_in => type_distance (scratch * distance_to_origin), accuracy => accuracy_schematic);
			point.y := type_distance (scratch * distance_to_origin);
			--log ("y in sch. " & to_string (point.y), log_threshold);
			log ("point out " & to_string (point), log_threshold);
			log_indentation_down;
	
		end if; -- if angle not zero
	end rotate;

	function distance (point_1, point_2 : in type_2d_point) return type_distance is
	-- Returns the distance between the given points.
-- 		dis : type_distance;
		dis : float;
		package functions_distance is new ada.numerics.generic_elementary_functions (float);
		use functions_distance;

		use et_string_processing;
	begin
		-- To save computing time a few simple checks:
		
		if point_1 = point_2 then -- points have same x/y position -> zero difference
-- 			log (text => "equal points", level => 4);
			dis := float (zero_distance);
			
		elsif point_1.x = point_2.x then -- points are in a vertical line
-- 			log (text => "vertical", level => 4);
			dis := float (abs (point_2.y - point_1.y));
			
		elsif point_1.y = point_2.y then -- points are in a horizontal line
-- 			log (text => "horizontal", level => 4);
			dis := float (abs (point_2.x - point_1.x));

		else -- distance = sqrt (delta_x^2 + delta_y^2)
-- 			log (text => "diagonal", level => 4);
			
			dis := sqrt (
					(float (abs (point_2.x - point_1.x))) ** 2
					+
					(float (abs (point_2.y - point_1.y))) ** 2 
					);

		end if;

		log (text => "distance " & float'image (dis), level => 4);

		return type_distance (dis);
	end distance;

	procedure check_submodule_name_length (name : in string) is
	-- Checks if the given submodule name is not longer than allowed.
		use et_string_processing;
	begin
		if name'length > submodule_name_length then
			log_indentation_reset;
			log (message_error & "max. number of characters for module name is" 
				 & positive'image (submodule_name_length) & " !",
				console => true);
			raise constraint_error;
		end if;
	end check_submodule_name_length;

	procedure check_submodule_name_characters (
		name : in type_submodule_name.bounded_string;
		characters : in character_set := submodule_name_characters) is
	-- Checks for forbiddedn characters in submodule name.
	begin
		null;
	end check_submodule_name_characters;
	
	function to_string (submodule : in type_submodule_name.bounded_string) return string is
	-- Returns the given submodule name as string.
	begin
		return type_submodule_name.to_string (submodule);
	end to_string;

	procedure check_number_of_instances (instances : in string) is
	-- Checks if given instances is a digit and if it is within allowed range.
		use et_string_processing;
	begin
		-- CS: check charactes. all must be digits

		-- Test if within range:
		if positive'value (instances) not in type_submodule_instance then
			log_indentation_reset;
			log (message_error & "max. number of instances per module is" 
				 & type_submodule_instance'image (type_submodule_instance'last) & " !",
				console => true);
			raise constraint_error;
		end if;
	end check_number_of_instances;

	function to_number_of_instances (instances : in string) return type_submodule_instance is
	begin
		return type_submodule_instance'value (instances);
	end to_number_of_instances;
	
	function to_string (
		path : in type_path_to_submodule.list;
		top_module : in boolean := true) return string is -- CS: probably no longer required
	-- Returns the given path as string with hierarchy_separator.
	-- If top_module = false, the name of the top module is omitted.
	
		use type_path_to_submodule;
		use ada.strings.unbounded;
	
		submodule : type_path_to_submodule.cursor := path.first;
		result : unbounded_string;
	begin
		-- If top_module is false, advance cursor right to next module.
		if not top_module then
			next (submodule);
		end if;

		if is_empty (path) then
			result := to_unbounded_string (hierarchy_separator);
		else
			-- Loop through list of submodules and collect their names in "result".
			while submodule /= type_path_to_submodule.no_element loop
				result := result & hierarchy_separator 
					& to_unbounded_string (to_string (element (submodule)));
-- 					& hierarchy_separator;
				next (submodule);
			end loop;
		end if;
-- 		if result = hierarchy_separator then
-- 			result := result & " (top module)";
-- 		end if;
		
		--return to_string ("location " & result);
		return to_string (result);
	end to_string;
	
	function to_coordinates (point : in type_2d_point'class)
	-- Converts a type_2d_point to type_coordinates.
		return type_coordinates is
	begin
		return (
			x		=> point.x,
			y		=> point.y,
			path	=> type_path_to_submodule.empty_list,
			sheet_number	=> 1
			);
	end to_coordinates;

	
	function to_string (
	-- Returns the given position as string. Scope specifies how much position is to
	-- be displayed. See specification of type_scope.
		position	: in type_coordinates;
		scope		: in type_scope := sheet)
		return string is

		use et_string_processing;
	begin
		case scope is
			when module =>
				return coordinates_preamble_module
					& to_string (position.path) & latin_1.space & hierarchy_separator & latin_1.space
-- 					& to_string (position.module)
-- 					& latin_1.space & axis_separator & latin_1.space
					& trim (positive'image (position.sheet_number),left) 
					& latin_1.space & axis_separator & latin_1.space
					& to_string (distance_x (position))
					& latin_1.space & axis_separator & latin_1.space
					& to_string (distance_y (position));
				
			when sheet =>
				return coordinates_preamble_sheet
					& trim (positive'image (position.sheet_number),left) 
					& latin_1.space & axis_separator & latin_1.space
					& to_string (distance_x (position))
					& latin_1.space & axis_separator & latin_1.space
					& to_string (distance_y (position));

			when xy =>
				return coordinates_preamble_xy
					& to_string (distance_x (position))
					& latin_1.space & axis_separator & latin_1.space
					& to_string (distance_y (position));

		end case;
		-- CS: output in both mil and mm
		
		-- CS: exception handler
	end to_string;

	function path (position : in type_coordinates) return type_path_to_submodule.list is
	begin
		return position.path;
	end path;

	function sheet (position : in type_coordinates) return positive is
	begin
		return position.sheet_number;
	end sheet;

	function same_path_and_sheet (left, right : in type_coordinates) return boolean is
	-- Returns true if the given coordinates have same path and sheet.
		same : boolean := false;
		use type_path_to_submodule;
	begin 
		-- We compare path and sheet. x/y are ignored
		if path (left) = path (right) then
			if sheet (left) = sheet (right) then
				same := true;
			end if;
		end if;

		return same;
	end same_path_and_sheet;
	
	procedure set_path (position : in out type_coordinates; path : in type_path_to_submodule.list) is
	-- Sets the path in given position.
	begin
		position.path := path;
	end set_path;

	procedure set_sheet (position : in out type_coordinates; sheet : in positive) is
	-- Sets the sheet number in given position.
	begin
		position.sheet_number := sheet;
	end set_sheet;
	
end et_coordinates;

-- Soli Deo Gloria
