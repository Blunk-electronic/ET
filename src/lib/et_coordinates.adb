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
		return position_preamble
			& trim (type_distance'image (point.x), left)
			& latin_1.space & axis_separator & latin_1.space
			& trim (type_distance'image (point.y), left);
	end to_string;

	function distance_x (point : in type_2d_point) return type_distance is
	begin
		return point.x;
	end distance_x;

	function distance_y (point : in type_2d_point) return type_distance is
	begin
		return point.y;
	end distance_y;

	procedure set_x (point : in out type_2d_point; x : in type_distance) is
	begin
		point.x := x;
	end set_x;
	
	procedure set_y (point : in out type_2d_point; y : in type_distance) is
	begin
		point.y := y;
	end set_y;


	
	
	function to_string (position : in type_coordinates) return string is
	-- Returns the given position as string.
	begin
		return coordinates_preamble
			& trim (positive'image (position.sheet_number),left) 
			& latin_1.space & axis_separator & latin_1.space
			--& to_string ( type_2d_point (position));
			& to_string (distance_x (position))
			& latin_1.space & axis_separator & latin_1.space
			& to_string (distance_y (position));

		-- CS: output in both mil and mm
		
		-- CS: exception handler
	end to_string;

	function path (position : in type_coordinates) return type_path_to_submodule.list is
	begin
		return position.path;
	end path;

	function module (position : in type_coordinates) return type_submodule_name.bounded_string is
	begin
		return position.module_name;
	end module;

	function sheet (position : in type_coordinates) return positive is
	begin
		return position.sheet_number;
	end sheet;

	procedure set_module (position : in out type_coordinates; name : in type_submodule_name.bounded_string) is
	-- Sets the module name in given position.
	begin
		position.module_name := name;
	end set_module;

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
