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

	function to_distance (distance : in string) return type_distance is
	begin
		return type_distance'value (distance);
	end to_distance;
	
	function terminal_position_default return type_terminal_position is
	begin
		return (
			x => zero_distance,
			y => zero_distance,
			z => zero_distance,
			angle => zero_angle);
	end terminal_position_default;
	
	function package_position_default return type_package_position is
	begin
		return (
			x => zero_distance,
			y => zero_distance,
			z => zero_distance,
			face => TOP,
			angle => zero_angle);
	end package_position_default;

	procedure set (
		axis 	: in type_axis;
		value	: in type_distance;
		point	: in out type_point_3d) is
	begin
		case axis is
			when X => point.x := value;
			when Y => point.y := value;
			when Z => point.z := value;
		end case;
	end set;
	
end et_pcb_coordinates;

-- Soli Deo Gloria
