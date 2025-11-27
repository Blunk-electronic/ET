------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          COLORS GENERAL                                  --
--                                                                          --
--                             B o d y                                      --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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

	function to_string (
		col_range : in color_range)
		return string
	is begin
		return color_range'image (col_range);
	end;


	
	function to_string (
		color	: in type_color)
		return string
	is begin
		return "R: " & to_string (color.red)
		& " G: " & to_string (color.green)
		& " B: " & to_string (color.blue);
	end;


	
	
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
