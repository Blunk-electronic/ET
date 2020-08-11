------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          SCHEMATIC COLORS                                --
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

package et_colors.schematic is

	cursor			: type_color := white;
	background 		: type_color := black;
	frame			: type_color := white;
	grid 			: type_color := gray;	
	nets 			: type_color := green;
	origin			: type_color := gray;	
	placeholders	: type_color := white;
	ports			: type_color := green;	
	submodules		: type_color := mangenta;
	symbols			: type_color := red;
	texts			: type_color := turquise;
	
	procedure set_color_cursor (context : in cairo_context);	
	procedure set_color_background (context : in cairo_context);
	procedure set_color_frame (context : in cairo_context);	
	
	procedure set_color_nets (
		context		: in cairo_context;
		brightness	: in type_brightness := brightness_default);
	
	procedure set_color_origin (context : in cairo_context);	
	
	procedure set_color_placeholders (
		context		: in cairo_context;
		brightness	: in type_brightness := brightness_default);
	
	procedure set_color_ports (context : in cairo_context);	
	procedure set_color_submodules (context : in cairo_context);	
	
	procedure set_color_symbols (
		context		: in cairo_context;
		brightness	: in type_brightness := brightness_default);
									
	procedure set_color_texts (
		context		: in cairo_context;
		brightness	: in type_brightness := brightness_default);

end et_colors.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
