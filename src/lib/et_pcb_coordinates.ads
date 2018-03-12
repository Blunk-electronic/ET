------------------------------------------------------------------------------
--                                                                          --
--                      SYSTEM ET PCB COORDINATES                           --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
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

with et_string_processing;

package et_pcb_coordinates is

	
	type type_axis is (X, Y, Z);
	type type_face is (TOP, BOTTOM);
	
	-- The total distance between two objects:
	type type_distance_total is delta 0.001 range -100_000_000.00 .. 100_000_000.00; -- unit is metric millimeter
	for type_distance_total'small use 0.001; -- this is the accuracy required for layout

	-- The x and y position of an object:
	subtype type_distance is type_distance_total range -10_000_000.0 .. 10_000_000.0; -- unit is metric millimeter
	zero_distance : constant type_distance := 0.0;

	function to_distance (distance : in string) return type_distance;
	
	type type_angle is delta 0.01 range -359.9 .. 359.9;
	for type_angle'small use 0.01;
	zero_angle : constant type_angle := 0.0;

	type type_point_3d is private;
	type type_terminal_position is private;
	type type_package_position is private;


	function terminal_position_default return type_terminal_position;

	function package_position_default return type_package_position;

	procedure set (
		axis 	: in type_axis;
		value	: in type_distance;					 
		point	: in out type_point_3d);

	private

		type type_point_3d is tagged record
			x, y, z : type_distance := zero_distance;
		end record;

		zero : constant type_point_3d := (others => zero_distance);

		
		type type_terminal_position is new type_point_3d with record
			angle	: type_angle := zero_angle;
		end record;
		
		type type_package_position is new type_point_3d with record
			angle	: type_angle := zero_angle;			
			face	: type_face := TOP;
		end record;

		
end et_pcb_coordinates;

-- Soli Deo Gloria
