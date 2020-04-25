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
-- with cairo.pattern;				use cairo.pattern;
-- with gtkada.style;

package et_colors is

	procedure dummy;
	
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
	mangenta	: constant type_color := (1.0, 0.0, 1.0);
	turquise	: constant type_color := (0.0, 1.0, 1.0);
	
end et_colors;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
