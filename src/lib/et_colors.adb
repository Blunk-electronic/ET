------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              COLORS                                      --
--                                                                          --
--                             B o d y                                      --
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

with ada.text_io;				use ada.text_io;
with glib;
with cairo.pattern;
with cairo.matrix;

package body et_colors is

	procedure dummy is begin null; end;

	function dim (
		color		: in type_color;
		brightness	: in type_brightness)
		return type_color is
	begin
		return color;
	end dim;

	
	procedure create_fill_pattern (
		context		: in cairo_context;
		color		: in type_color;
		opacity		: in type_opacity;
-- 		background	: in type_color;
		style		: in type_fill_style;
		scale		: in type_scale)
	is
		use glib;
		use cairo.pattern;
		use cairo.matrix;
		
		s : constant gdouble := 20.0 / scale;
		p : cairo_pattern := pattern_create_linear (0.0, 0.0, s, s);

		b : gdouble := 0.5;
		c : type_color := (color.red * b, color.green * b, color.blue * b);
	begin
		--pattern_add_color_stop_rgba (p, 0.50, background.red, background.green, background.blue, 0.5);
		pattern_add_color_stop_rgba (p, 0.50, c.red, c.green, c.blue, 0.5);
		pattern_add_color_stop_rgba (p, 0.51, color.red, color.green, color.blue, 0.5);  
		pattern_add_color_stop_rgba (p, 0.55, color.red, color.green, color.blue, 0.5);  
		pattern_add_color_stop_rgba (p, 0.56, c.red, c.green, c.blue, 0.5);
		
		set_source (context, p);
		set_extend (get_source (context), CAIRO_EXTEND_REPEAT);
		
	end create_fill_pattern;

	
end et_colors;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
