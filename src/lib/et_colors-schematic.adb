------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           SCHEMATIC COLORS                               --
--                                                                          --
--                               B o d y                                    --
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

package body et_colors.schematic is

	procedure set_color_cursor (context : in cairo_context) is begin		
		set_source_rgb (
			context, 
			cursor.red,
			cursor.green,
			cursor.blue);
	end set_color_cursor;
	
	procedure set_color_background (context : in cairo_context) is begin
		set_source_rgb (
			context, 
			background.red,
			background.green,
			background.blue);
	end set_color_background;



	procedure set_color (
		color		: in type_color;
		brightness	: in type_brightness;
		opacity		: in type_opacity := default_opacity)
	is 
		c : type_color;
	begin
		case brightness is
			when DARK 	=>	c := dim (color, 0.25);
			when NORMAL	=>	c := dim (color, dim_factor_default);
			when BRIGHT	=>	c := dim (color, 1.0);
		end case;

		set_source_rgb (context, c.red, c.green, c.blue);
	end set_color;


	
	procedure set_color_frame (
		brightness	: in type_brightness := brightness_default)
	is begin
		-- CS query color schema defined by user
		-- and overwrite value of variable frame
		
		set_color (frame, brightness);
	end set_color_frame;



	
	procedure set_color_nets (
		brightness	: in type_brightness := brightness_default)
	is begin
		-- CS query color schema defined by user
		-- and overwrite value of variable nets
		
		set_color (nets, brightness);
	end set_color_nets;

	
	procedure set_color_origin (
		brightness	: in type_brightness := brightness_default)
	is begin
		-- CS query color schema defined by user
		-- and overwrite value of variable origin
		
		set_color (origin, brightness);
	end set_color_origin;

	
	procedure set_color_placeholders (
		brightness	: in type_brightness := brightness_default)
	is begin										
		-- CS query color schema defined by user
		-- and overwrite value of variable placeholders
		
		set_color (placeholders, brightness);
	end set_color_placeholders;

	
	procedure set_color_ports (
		brightness	: in type_brightness := brightness_default)
	is begin
		-- CS query color schema defined by user
		-- and overwrite value of variable ports
		
		set_color (ports, brightness);
	end set_color_ports;

	
	procedure set_color_submodules (
		brightness	: in type_brightness := brightness_default)
	is begin
		-- CS query color schema defined by user
		-- and overwrite value of variable submodules
		
		set_color (submodules, brightness);
	end set_color_submodules;

	
	procedure set_color_symbols (
		brightness	: in type_brightness := brightness_default)
	is begin
		-- CS query color schema defined by user
		-- and overwrite value of variable symbols
		
		set_color (symbols, brightness);
	end set_color_symbols;

	
	procedure set_color_texts (
		brightness	: in type_brightness := brightness_default)
	is begin
		-- CS query color schema defined by user
		-- and overwrite value of variable nets
		
		set_color (texts, brightness);
	end set_color_texts;
	
end et_colors.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
