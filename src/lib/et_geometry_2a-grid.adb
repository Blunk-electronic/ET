------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                GRID                                      --
--                                                                          --
--                               B o d y                                    --
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

with ada.text_io;					use ada.text_io;



package body et_geometry_2a.grid is


	function to_string (
		on_off : in type_grid_on_off)
		return string
	is begin
		return type_grid_on_off'image (on_off);
	end;
	

	function to_on_off (
		on_off : in string)
		return type_grid_on_off
	is begin
		return type_grid_on_off'value (on_off);
	end to_on_off;
	
	

	function to_string (
		style : in type_grid_style)
		return string
	is begin
		return type_grid_style'image (style);
	end;


	function to_style (
		style : in string)
		return type_grid_style
	is begin
		return type_grid_style'value (style);
	end;



	
	procedure next_grid_density (
		grid 		: in out type_grid;
		direction	: in type_grid_direction)
	is begin
		put_line ("next_grid_density " & type_grid_direction'image (direction));

		case direction is
			when GRID_UP =>
				grid.spacing.x := grid.spacing.x * density_multiplier;
				grid.spacing.y := grid.spacing.y * density_multiplier;

				limit_to_maximum (grid.spacing.x, grid_spacing_max);
				limit_to_maximum (grid.spacing.y, grid_spacing_max);
		
			when GRID_DOWN =>
				grid.spacing.x := grid.spacing.x / density_multiplier;
				grid.spacing.y := grid.spacing.y / density_multiplier;

				limit_to_minimum (grid.spacing.x, grid_spacing_min);
				limit_to_minimum (grid.spacing.y, grid_spacing_min);

		end case;
	end next_grid_density;

	
	
	procedure reset_grid_density (
		grid : in out type_grid)
	is begin
		put_line ("reset_grid_density");
		
		grid.spacing.x := grid_spacing_default;
		grid.spacing.y := grid_spacing_default;
	end reset_grid_density;

	
end et_geometry_2a.grid;

