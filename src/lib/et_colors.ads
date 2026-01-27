------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            COLORS GENERAL                                --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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

with cairo;						use cairo;

package et_colors is

	function to_string (
		col_range : in color_range)
		return string;


	type type_color is record -- CS make private ?
		red, green, blue : color_range := 0.0;
	end record;


	function to_string (
		color	: in type_color)
		return string;
	

		
	-- CS function that takes separate values for R, G, B
	-- and returns a type_color.
	
	
-- BASIC COLORS:

	white		: constant type_color := (1.0, 1.0, 1.0);
	black		: constant type_color := (0.0, 0.0, 0.0);
	red			: constant type_color := (1.0, 0.0, 0.0);
	green		: constant type_color := (0.0, 1.0, 0.0);	
	blue		: constant type_color := (0.0, 0.0, 1.0);

	gray		: constant type_color := (0.5, 0.5, 0.5);
	yellow		: constant type_color := (1.0, 1.0, 0.0);
	orange		: constant type_color := (1.0, 0.5, 0.0);
	pink		: constant type_color := (1.0, 0.4, 0.4);	
	mangenta	: constant type_color := (1.0, 0.0, 1.0);
	turquise	: constant type_color := (0.0, 1.0, 1.0);
	-- CS: others ?

	
	
	-- This function takes a string like "white" or "black"
	-- (see table above) and return the associated color type:
	function to_color (
		color : in string)
		return type_color;
	
	
	
	
-- OPACITY:
	
	type type_opacity is new color_range; 
	-- 0.0 -> max. opacity, 1.0 -> no transparency

	-- default_opacity : constant type_opacity := 0.5;
	
	-- CS: experimental:
	default_opacity : constant type_opacity := 0.7;

	
	no_opacity : constant type_opacity := 1.0;


	
	
	
	
-- DIM FACTOR AND BRIGHTNESS:
	
	type type_dim_factor is new color_range; -- 0.0 -> dark, 1.0 -> bright


	dim_factor_dark		: constant type_dim_factor := 0.25;

	
	-- dim_factor_default	: constant type_dim_factor := 0.6;
	-- CS: experimental:
	dim_factor_default	: constant type_dim_factor := 0.7;
	
	
	-- dim_factor_bright	: constant type_dim_factor := 0.8; 
	-- CS: experimental:
	dim_factor_bright	: constant type_dim_factor := 1.0;

	
	
	-- Changes the brightness of a given color to the value
	-- given by brightness:
	function dim (
		color		: in type_color;
		brightness	: in type_dim_factor)
		return type_color;

	
	
	type type_brightness is (DARK, NORMAL, BRIGHT);
	brightness_default : constant type_brightness := NORMAL;


	
	-- Modifies the given color by the given brightness:
	function dim (
		color		: in type_color;
		brightness	: in type_brightness)
		return type_color;
	
	
end et_colors;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
