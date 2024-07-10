------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          COLORS GENERAL                                  --
--                                                                          --
--                             B o d y                                      --
--                                                                          --
--         Copyright (C) 2017 - 2024 Mario Blunk, Blunk electronic          --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 

with ada.text_io;				use ada.text_io;

package body et_colors is

	function dim (
		color		: in type_color;
		brightness	: in type_dim_factor)
		return type_color 
	is
		use type color_range;
		
		b : constant color_range := color_range (brightness);
		result : type_color;
	begin
		result.red		:= color.red * b;
		result.green	:= color.green * b;
		result.blue		:= color.blue * b;
		
		return result;
	end dim;


	function dim (
		color		: in type_color;
		brightness	: in type_brightness)
		return type_color
	is begin
		case brightness is
			when DARK	=> return dim (color, 0.25);
			when NORMAL	=> return dim (color, dim_factor_default);
			when BRIGHT	=> return dim (color, 1.0);
		end case;
	end dim;
	
end et_colors;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
