------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          SCHEMATIC COLORS                                --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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

with et_canvas_schematic;

package et_colors.schematic is

	use et_canvas_schematic.pac_canvas;
	-- The global context is now visible
	-- for all procedures that set the color.
	-- CS remove the useless argument "context".
	
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


	-- Sets the given color, brightness and opacity in the given context:
	procedure set_color (
		color		: in type_color;
		brightness	: in type_brightness;
		opacity		: in type_opacity := default_opacity);


	
	procedure set_color_frame (
		brightness	: in type_brightness := brightness_default);

	
	procedure set_color_nets (

		brightness	: in type_brightness := brightness_default);

	
	procedure set_color_origin (
		brightness	: in type_brightness := brightness_default);

	
	procedure set_color_placeholders (
		brightness	: in type_brightness := brightness_default);

	
	procedure set_color_ports (
		brightness	: in type_brightness := brightness_default);

	
	procedure set_color_submodules (
		brightness	: in type_brightness := brightness_default);

	
	procedure set_color_symbols (
		brightness	: in type_brightness := brightness_default);

	
	procedure set_color_texts (
		brightness	: in type_brightness := brightness_default);

end et_colors.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
