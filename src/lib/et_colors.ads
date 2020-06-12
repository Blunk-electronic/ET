------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            LAYER DISPLAY                                 --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

with cairo;						use cairo;
with et_geometry;				use et_geometry;

package et_colors is

	type type_color is record
		red, green, blue : color_range := 0.0;
	end record;


	white		: constant type_color := (1.0, 1.0, 1.0);
	black		: constant type_color := (0.0, 0.0, 0.0);
	red			: constant type_color := (1.0, 0.0, 0.0);
	green		: constant type_color := (0.0, 1.0, 0.0);	
	blue		: constant type_color := (0.0, 0.0, 1.0);

	gray		: constant type_color := (0.5, 0.5, 0.5);
	yellow		: constant type_color := (1.0, 1.0, 0.0);
	orange		: constant type_color := (1.0, 0.5, 0.0);	
	mangenta	: constant type_color := (1.0, 0.0, 1.0);
	turquise	: constant type_color := (0.0, 1.0, 1.0);

	type type_opacity is new color_range; -- 0.0 -> max. opacity, 1.0 -> no transparency
	default_opacity : constant type_opacity := 0.5;
	no_opacity : constant type_opacity := 1.0;

	type type_brightness is new color_range; -- 0.0 -> dark, 1.0 -> bright

	-- Changes the brightness of a given color to the value
	-- given by brightness:
	function dim_to (
		color		: in type_color;
		brightness	: in type_brightness)
		return type_color;
	
	type type_fill_style is (
		SOLID, 
		STRIPED_0,
		STRIPED_45,
		STRIPED_90,
		STRIPED_135
-- CS DOTTED_SPARSE,
-- CS DOTTED_MEDIUM,
-- CS DOTTED_DENSE,
-- CS HATCHED_0,
-- CS HATCHED_45
		);

	fill_pattern_gap_brightness_default : constant type_brightness := 0.5;
	
	-- Creates a fill pattern in the given context:
	procedure create_fill_pattern (
		context			: in cairo_context;
		color			: in type_color;		-- the color of the pattern
		opacity			: in type_opacity;		-- the opacity of the pattern
		-- background	: in type_color; ?

		-- the brightness of the gaps betweeen lines and dots:
		gap_brightness	: in type_brightness := fill_pattern_gap_brightness_default;
		
		style			: in type_fill_style;	-- the style (solid, striped, dotted)
		scale			: in type_scale);		-- the scale of the canvas

	
end et_colors;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
