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
		return type_face'image (face);
	end to_string;
	
	function to_distance (distance : in string) return type_distance is
	begin
		return type_distance'value (distance);
	end to_distance;

	function to_string (distance : in type_distance_total) return string is
	begin
		return trim (type_distance_total'image (distance), left);
	end to_string;

	function to_string (
		angle 		: in type_angle;
		preamble 	: in boolean := false)
		return string is
	begin
		if preamble then
			return " angle " 
				& trim (type_angle'image (angle), left) 
				& latin_1.space & "degrees";
		else
			return trim (type_angle'image (angle), left);
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
			& latin_1.space			
			& to_string (point.y)
			& latin_1.space			
			& et_coordinates.axis_separator
			& latin_1.space
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
