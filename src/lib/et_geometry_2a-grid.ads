------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                GRID                                      --
--                                                                          --
--                               S p e c                                    --
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

with et_logical_pixels;			use et_logical_pixels;

generic
	
package et_geometry_2a.grid is

	-- The grid helps the operator to align or place objects.


-- ON / OFF;
	
	type type_grid_on_off is (GRID_ON, GRID_OFF);

	
	function to_string (
		on_off : in type_grid_on_off)
		return string;

	function to_on_off (
		on_off : in string)
		return type_grid_on_off;

	

-- STYLE (lines or dots):
	
	type type_grid_style is (STYLE_DOTS, STYLE_LINES);

	function to_string (
		style : in type_grid_style)
		return string;

	function to_style (
		style : in string)
		return type_grid_style;





	
	-- The linewidth of the grid lines:
	grid_width_lines : constant type_logical_pixels_positive := 0.5;

	-- The linewidth of the circles which form the grid dots:
	grid_width_dots : constant type_logical_pixels_positive := 1.0;
	grid_radius_dots : constant type_logical_pixels_positive := 0.5;

	
	-- The arm length of a grid point if drawn as a cross:
	grid_cross_arm_length : constant type_logical_pixels_positive := 1.0;



	-- The default grid size in in the model domain:
	grid_spacing_default : constant type_distance_positive := 10.0; 

	grid_spacing_max : constant type_distance_positive := 100.0;
	grid_spacing_min : constant type_distance_positive := 0.01; 
	
		
	-- If the displayed grid is too dense, then it makes no
	-- sense to draw a grid. For this reason we define a minimum
	-- distance between grid rows and columns. If the spacing becomes
	-- greater than this threshold then the grid will be drawn:
	grid_spacing_min_lp : constant type_logical_pixels_positive := 10.0;

	
	type type_grid is record
		on		: type_grid_on_off := GRID_ON;
		-- on		: type_grid_on_off := GRID_OFF;
		spacing : type_vector_model := (others => grid_spacing_default);
		style	: type_grid_style := STYLE_DOTS;
		--style	: type_grid_style := STYLE_LINES;
	end record;




	-- The grid spacing can be changed via certain keystrokes.
	-- It can be scaled up or down:
	type type_grid_direction is (GRID_UP, GRID_DOWN);

	-- When scaling the grid spacing up or down we use
	-- a simple multiplier:
	density_multiplier : constant type_distance_positive := 2.0;

	
	-- This procedure changes the given grid:
	procedure next_grid_density (
		grid 		: in out type_grid;
		direction	: in type_grid_direction);


	-- This procedure resets the grid spacing to the default value:
	procedure reset_grid_density (
		grid		: in out type_grid);
	

	keyword_on_off	: constant string := "on_off";
	keyword_spacing	: constant string := "spacing";
	keyword_style	: constant string := "style";
	
end et_geometry_2a.grid;

