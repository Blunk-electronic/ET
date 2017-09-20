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
--with et_string_processing;

package body et_coordinates is

	function mil_to_distance (mil : in string) return type_distance is
	-- Returns the given mils to type_distance.		

		type type_distance_intermediate is digits 13 range -10000000.0 .. 1000000.0;
		-- unit is mil
		-- CS: refine range and delta if required

		d_in : type_distance_intermediate;
	begin
		d_in := type_distance_intermediate'value (mil);

		return type_distance ( d_in * (25.4 * 0.001) );
		
		-- CS: exception handler
	end mil_to_distance;

	
	function to_string (distance : in type_distance) return string is
	-- Returns the given distance to a string.
	begin
		return trim (type_distance'image (distance), left);
	end to_string;

	
	function to_string (point : in type_2d_point) return string is
	-- Returns the given point coordinates to a string.
	begin
		return trim (type_distance'image (point.x), left)
			& axis_separator 
			& trim (type_distance'image (point.y), left);
	end to_string;

	
end et_coordinates;

-- Soli Deo Gloria
