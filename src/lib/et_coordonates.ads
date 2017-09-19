------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET COORDINATES                          --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
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

--with et_string_processing;

package et_coordinates is

	procedure dummy;
	
-- GRID AND COORDINATES

	type type_distance is delta 0.0001 digits 11 range -10000.0000 .. 10000.0000;
-- 	subtype type_grid is type_grid_extended range -100000.00 .. 100000.00; -- CS: unit assumed is MIL !!!
-- 	-- CS: negative schematic coordinates should be forbidden
-- 	-- type type_grid is digits 7 range 0.00 .. 100000.00; -- CS: unit assumed is MIL !!!	
-- 
-- 	coordinate_zero : constant type_grid := 0.0;
-- 
--     -- In general every object has at least x,y coordinates.
-- 	type type_coordinates is tagged record
-- 		x,y				: type_grid := coordinate_zero;
-- 	end record;
-- 
-- 	coordinates_dimension_separator : constant string (1..1) := "/";
-- 	coordinates_preamble : constant string (1..15) := "position (x" & coordinates_dimension_separator & "y) ";
-- 
-- 	function to_grid (grid : in string) return type_grid;
-- 	-- Returns the given grid as type_grid.
-- 
-- 	function to_string (grid : in type_grid) return string;
-- 	-- Returns the given grid as string.
-- 	
-- 	function to_string (position : in type_coordinates) return string;
-- 	-- Returns the given position as string.
-- 
-- 
-- -- ORIENTATION, ANGLE AND ROTATION
-- 
-- 	-- Objects may be placed at a certain angle:
-- 	type type_angle is delta 0.1 digits 4 range -359.9 .. 359.9; -- unit is degrees
-- 	-- CS: a type that allows angles of multiples of 45 degrees ? 
-- 	-- or check angle via separate function when required ?
-- 	
-- 	subtype type_angle_90 is type_angle range 0.0 .. 90.0;
-- 	-- CS: make use of this type by membership tests when required
-- 
-- 	function to_string (angle : in type_angle) return string;
-- 	-- Returns the given angle as string. 
-- 
-- 	procedure warning_angle_greater_90_degrees;



		
end et_coordinates;

-- Soli Deo Gloria
