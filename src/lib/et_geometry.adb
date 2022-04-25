------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              GEOMETRY                                    --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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
with ada.characters;			use ada.characters;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with et_exceptions;				use et_exceptions;

package body et_geometry is


	function to_string (direction : in type_direction_of_rotation) return string is begin
		return to_lower (type_direction_of_rotation'image (direction));
	end to_string;

	
	function to_direction (direction : in string) return type_direction_of_rotation is begin
		return type_direction_of_rotation'value (direction);
	end to_direction;

	
	function reverse_direction (direction : in type_direction_of_rotation)
		return type_direction_of_rotation is
	begin
		case direction is 
			when CW => return CCW;
			when CCW => return CW;
		end case;
	end reverse_direction;


	
	
	function to_string (axis : in type_axis) return string is begin
		return to_lower (type_axis'image (axis));
	end;

	function to_axis (axis : in string) return type_axis is begin
		return type_axis'value (axis);
	end;



	function to_string (tool : in type_tool) return string is begin
		return type_tool'image (tool);
	end to_string;

	function to_tool (tool : in string) return type_tool is begin
		return type_tool'value (tool);
	end to_tool;


	

	function to_notches (notches : in string) return type_grid_notches is begin
		return type_grid_notches'value (notches);
	end;

	function to_string (notches : in type_grid_notches) return string is begin
		return type_grid_notches'image (notches);
	end;



	function to_shape (shape : in string) return type_shape is begin
		return type_shape'value (shape);
	end;

	function to_string (shape : in type_shape) return string is begin
		return to_lower (type_shape'image (shape));
	end;

	
	
	
	function to_string (coordinates : in type_coordinates) return string is begin
		return space & to_lower (type_coordinates'image (coordinates));
	end;

	function to_coordinates (coordinates : in string) return type_coordinates is begin
		return type_coordinates'value (coordinates);

-- 			exception
-- 				when event: others =>
-- 					log (text => ada.exceptions.exception_information (event), console => true);
-- 					raise;
	end;


	function to_string (filled : in type_filled) return string is begin
		return to_lower (type_filled'image (filled));
	end to_string;

	function to_filled (filled : in string) return type_filled is begin
		return type_filled'value (filled);
	end to_filled;


	-- FILL STYLE
	function to_string (fill_style : in type_fill_style) return string is begin
		return to_lower (type_fill_style'image (fill_style));
	end;

	function to_fill_style (fill_style : in string) return type_fill_style is begin
		return type_fill_style'value (fill_style);
	end;

	
	
end et_geometry;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
