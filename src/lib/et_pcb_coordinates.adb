------------------------------------------------------------------------------
--                                                                          --
--                      SYSTEM ET PCB COORDINATES                           --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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
with ada.strings.fixed;			use ada.strings.fixed;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_string_processing;		use et_string_processing;

package body et_pcb_coordinates is

	function to_string (face : in type_face) return string is
	begin
		return latin_1.space & type_face'image (face);
	end to_string;
	
	function to_distance (distance : in string) return type_distance is
	begin
		return type_distance'value (distance);
	end to_distance;

	function to_string (distance : in type_distance_total) return string is
	begin
		return latin_1.space & trim (type_distance_total'image (distance), left);
	end to_string;

	function to_string (
		angle 		: in type_angle;
		preamble 	: in boolean := false)
		return string is
	begin
		if preamble then
			return " angle " 
				& trim (type_angle'image (angle), left); 
		else
			return latin_1.space & trim (type_angle'image (angle), left);
		end if;
	end to_string;

	function to_angle (angle : in string) return type_angle is
	begin
		return type_angle'value (angle);
	end to_angle;
	
	function to_string (point : in type_point_3d) return string is
	begin
		return position_preamble_3d
			& to_string (point.x)
			& latin_1.space
			& et_coordinates.axis_separator
			& to_string (point.y)
			& latin_1.space			
			& et_coordinates.axis_separator
			& to_string (point.z);
	end to_string;

-- 	function point_zero return type_point_3d is
-- 	begin
-- 		return zero;
-- 	end point_zero;
	
	function terminal_position_default return type_terminal_position is
	begin
		return (zero with zero_angle);
	end terminal_position_default;
	
	function package_position_default return type_package_position is
	begin
		return (zero with face => TOP, angle => zero_angle);
	end package_position_default;

	procedure set_point (
		axis 	: in type_axis;
		value	: in type_distance;
		point	: in out type_point_3d) is
	begin
		case axis is
			when X => point.x := value;
			when Y => point.y := value;
			when Z => point.z := value;
		end case;
	end set_point;

	procedure rotate (
	-- Rotates the given point by the given angle with the origin as center.
		point	: in out type_point_3d; -- z axis ignored
		angle	: in type_angle) is

		type type_float_distance is digits 7 range -1000.0 .. 1000.0; -- CS: refine
		package functions_distance is new ada.numerics.generic_elementary_functions (type_float_distance);
		use functions_distance;
		
		type type_float_angle is digits 4 range -719.9 .. 719.9; -- CS: refine			
		package functions_angle is new ada.numerics.generic_elementary_functions (type_float_angle);
		use functions_angle;

		angle_out			: type_float_angle;		-- unit is degrees
		distance_to_origin	: type_float_distance;	-- unit is mm
		scratch				: type_float_distance;

	begin
		-- Do nothing if the given rotation is zero.
		if angle /= 0.0 then

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
	
	function get_axis (
		axis	: in type_axis;
		point	: in type_point_3d)
		return type_distance_total is
	begin
		case axis is
			when X => return point.x;
			when Y => return point.y;
			when Z => return point.z;
		end case;
	end get_axis;
	
	procedure set_angle (
		value	: in type_angle;
		point	: in out type_terminal_position) is
	begin
		point.angle := value;
	end set_angle;

	function get_angle (point : in type_terminal_position) return type_angle is
	begin
		return point.angle;
	end get_angle;
		
	
	function to_terminal_position (
	-- Composes from a given point and angle the terminal position.
		point	: in type_point_3d;
		angle	: in type_angle)
		return type_terminal_position'class is
		pos : type_terminal_position;
	begin
		pos := (point with angle);
		return pos;
	end to_terminal_position;
	
end et_pcb_coordinates;

-- Soli Deo Gloria
