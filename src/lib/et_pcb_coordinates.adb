------------------------------------------------------------------------------
--                                                                          --
--                      SYSTEM ET PCB COORDINATES                           --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

--   For correct displaying set tab width in your edtior to 4.

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

with ada.tags;

with ada.exceptions;
with ada.numerics;
with ada.numerics.generic_elementary_functions;

with et_string_processing;		use et_string_processing;

package body et_pcb_coordinates is

	function to_string (face : in type_face) return string is begin
		return latin_1.space & to_lower (type_face'image (face));
	end to_string;

	function to_face (face : in string) return type_face is begin
		return type_face'value (face);
	end to_face;
	
	function to_distance (distance : in string) return type_distance is
	begin
		return type_distance'value (distance);
	end to_distance;

	function right_point_before_left_2d (right, left : in type_point_2d) return boolean is
	-- Returns true if right point comes before left point.
	-- Compares axis in this order: x, y
	-- If right point equals left point, returns false.
		result : boolean := false;
	begin
		if right.x < left.x then result := true;
		elsif right.x = left.x then
			
			if right.y < left.y then result := true;
-- 			elsif right.y = left.y then
-- 
-- 				if right.z < left.z then result := true;
-- 				else result := false;
-- 				end if;
				
			else result := false;
			end if;

		else result := false;
		end if;

		return result;
	end right_point_before_left_2d;
	
	function mil_to_distance (mil : in string; warn_on_negative : boolean := true) 
		return type_distance is
	-- Converts a mil number (given as a string) to millimeters.
		use et_string_processing;
		
		type type_distance_intermediate is digits 13 range mil_min .. mil_max; -- unit is mil
		-- CS: refine range and delta if required

		d_in : type_distance_intermediate;
		distance : type_distance; -- the distance to be returned
	begin
 		d_in := type_distance_intermediate'value (mil);
		distance := type_distance (d_in * (25.4 * 0.001));

		if warn_on_negative then
			if distance < zero_distance then
				log (WARNING, "negative coordinates found !");
			end if;
		end if;
		
		return distance;
		
		exception
			when event:
				others =>
					log (ERROR, "mil value " & mil & " invalid !", console => true);
					log ("Allowed range for mil numbers is" 
						& float'image (mil_min) & " .." & float'image (mil_max) & ".", console => true);

					-- log ("mm out " & type_distance'image (distance));
					log (ada.exceptions.exception_message (event));
					raise;

	end mil_to_distance;
	
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

	function paper_dimension (
	-- Returns for the given paper size, orientation and axis the correspoinding size in mm.
		paper_size	: in et_general.type_paper_size;
		orientation	: in et_general.type_paper_orientation := et_general.LANDSCAPE;
		axis		: in type_axis_2d)
		return type_distance is

		dimension : type_distance;
		use et_general;
	
	begin
		case orientation is
			when LANDSCAPE =>
				case paper_size is 
					when A3 =>
						case axis is
							when X => dimension := paper_size_A3_x;
							when Y => dimension := paper_size_A3_y;
						end case;

					when A4 =>
						case axis is
							when X => dimension := paper_size_A4_x;
							when Y => dimension := paper_size_A4_y;
						end case;
				end case;

			when PORTRAIT =>
				case paper_size is 
					when A3 =>
						case axis is
							when X => dimension := paper_size_A3_y;
							when Y => dimension := paper_size_A3_x;
						end case;

					when A4 =>
						case axis is
							when X => dimension := paper_size_A4_y;
							when Y => dimension := paper_size_A4_x;
						end case;
				end case;

		end case;

		return dimension;
	end paper_dimension;

	
	function to_angle (angle : in string) return type_angle is
	begin
		return type_angle'value (angle);
	end to_angle;

	function set_point (x, y : in type_distance) 
		return type_point_2d'class is
		point : type_point_2d;
	begin
		point.x := x;
		point.y := y;
		return point;
	end set_point;

	function set_point (x, y, z : in type_distance) 
		return type_point_3d'class is
		point : type_point_3d;
	begin
		point.x := x;
		point.y := y;
		point.z := z;
		return point;
	end set_point;

	
	function to_string (point : in type_point_2d) return string is
	begin
		return position_preamble_2d
			& to_string (point.x)
			& latin_1.space
			& et_coordinates.axis_separator
			& to_string (point.y);
	end to_string;

	function to_string (point : in type_point_2d_with_angle) return string is
	begin
		return position_preamble_2d
			& to_string (point.x)
			& latin_1.space
			& et_coordinates.axis_separator
			& to_string (point.y)
			& et_coordinates.axis_separator
			& to_string (point.angle);
	end to_string;
	
	procedure reset_point (
	-- Moves the given point to the origin (0/0).
		point	: in out type_point_2d'class)
	is begin
		point.x := zero_distance;
		point.y := zero_distance;
	end reset_point;
	
	procedure move_point (
		point	: in out type_point_2d'class;
		offset	: in type_point_2d) 
	is begin
		point.x := point.x + offset.x;
		point.y := point.y + offset.y;
	end move_point;

	procedure set_point (
		axis 	: in type_axis_2d;
		value	: in type_distance;
		point	: in out type_point_2d'class) is
	begin
		case axis is
			when X => point.x := value;
			when Y => point.y := value;
		end case;
	end set_point;
	
	procedure rotate (
	-- Rotates the given point by the given angle with the origin as center.
		point	: in out type_point_2d;
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

		use et_coordinates;
	begin
		-- Do nothing if the given rotation is zero.
		if angle /= 0.0 then

			-- compute distance of given point to origin
			if get_axis (X, point) = zero_distance and get_axis (Y, point) = zero_distance then
				distance_to_origin := type_float_distance (zero_distance);
			elsif get_axis (X, point) = zero_distance then
				distance_to_origin := type_float_distance (abs (get_axis (Y, point)));
			elsif get_axis (Y, point) = zero_distance then
				distance_to_origin := type_float_distance (abs (get_axis (X, point)));
			else
				distance_to_origin := sqrt (
					type_float_distance (abs (get_axis (X, point))) ** type_float_distance (2) 
					+
					type_float_distance (abs (get_axis (Y, point))) ** type_float_distance (2)
					);
			end if;
			
			-- compute the current angle of the given point (in degrees)

			if get_axis (X, point) = zero_distance then
				if get_axis (Y, point) > zero_distance then
					angle_out := 90.0;
				elsif get_axis (Y, point) < zero_distance then
					angle_out := -90.0;
				else
					angle_out := 0.0;
				end if;

			elsif get_axis (Y, point) = zero_distance then
				if get_axis (X, point) > zero_distance then
					angle_out := 0.0;
				elsif get_axis (X, point) < zero_distance then
					angle_out := 180.0;
				else
					angle_out := 0.0;
				end if;

			else
				angle_out := type_float_angle (arctan (
					x => type_float_distance (get_axis (X, point)),
					y => type_float_distance (get_axis (Y, point)),
					cycle => type_float_distance (units_per_cycle))
					);
			end if;

			-- Compute new angle by adding current angle and given angle.
			-- This computation depends on the Y axis style. The in the conventional style (Y going upwards positive)
			-- we add the given angle to the current angle. In the old fashioned stlyle (Y going downwards positive)
			-- we subtract the given angle from the current angle.
			if Y_axis_positive = upwards then
				angle_out := angle_out + type_float_angle (angle);
			else
				angle_out := angle_out - type_float_angle (angle);
			end if;

			-- compute new x   -- (cos angle_out) * distance_to_origin
			scratch := cos (type_float_distance (angle_out), type_float_distance (units_per_cycle));
			set_point (axis => X, point => point, value => type_distance (scratch * distance_to_origin));

			-- compute new y   -- (sin angle_out) * distance_to_origin
			scratch := sin (type_float_distance (angle_out), type_float_distance (units_per_cycle));
			set_point (axis => Y, point => point, value => type_distance (scratch * distance_to_origin));
	
		end if; -- if angle not zero
		
	end rotate;
	
	function get_axis (
		axis	: in type_axis_2d;
		point	: in type_point_2d)
		return type_distance_total is
	begin
		case axis is
			when X => return point.x;
			when Y => return point.y;
		end case;
	end get_axis;
	
	procedure set_angle (
		value	: in type_angle;
		point	: in out type_point_2d_with_angle'class) is
	begin
		point.angle := value;
	end set_angle;

	function get_angle (point : in type_point_2d_with_angle'class) return type_angle is
	begin
		return point.angle;
	end get_angle;
		
	procedure set_face (
		face	: in type_face;
		position: in out type_package_position) is
	begin
		position.face := face;
	end set_face;

	function get_face (packge : in type_package_position)
		return type_face is
	begin
		return packge.face;
	end get_face;
	
	
	function to_terminal_position (
	-- Composes from a given point and angle the terminal position.
		point	: in type_point_2d;
		angle	: in type_angle)
		return type_point_2d_with_angle'class is
		pos : type_point_2d_with_angle;
	begin
		pos := (point with angle);
		return pos;
	end to_terminal_position;
	
end et_pcb_coordinates;

-- Soli Deo Gloria
