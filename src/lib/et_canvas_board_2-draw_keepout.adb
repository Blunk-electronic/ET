------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW KEEPOUT                              --
--                                                                          --
--                               B o d y                                    --
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

with ada.text_io;					use ada.text_io;

with et_primitive_objects;			use et_primitive_objects;
with et_keepout;					use et_keepout;
with et_colors;						use et_colors;
with et_modes.board;
with et_canvas_tool;


separate (et_canvas_board_2)

procedure draw_keepout (
	face	: in type_face) 
is
	use et_colors.board;
	use et_board_text;

	use pac_keepout_zones;
	use pac_keepout_cutouts;


	procedure set_default_brightness is begin
		set_color_keepout (face, NORMAL);
	end set_default_brightness;

	
	procedure set_highlight_brightness is begin
		set_color_keepout (face, BRIGHT);
	end set_highlight_brightness;
		
	

	
	procedure query_zone (c : in pac_keepout_zones.cursor) is
		use pac_draw_contours;
	begin
		draw_contour (
			contour	=> element (c),
			filled	=> NO, -- CS YES ?
			width	=> zero);
	end query_zone;

	
	procedure query_cutout (c : in pac_keepout_cutouts.cursor) is 
		-- CS use rename
		use pac_draw_contours;
	begin
		set_color_background;
		
		draw_contour (
			contour	=> element (c),
			filled	=> YES,
			width	=> zero);
	end query_cutout;

	

	
	procedure query_items (
		module_name	: in pac_module_name.bounded_string;
		module		: in type_generic_module) 
	is begin
		-- All keepout segments will be drawn with the same color:
		set_color_keepout (face, NORMAL);
		
		case face is
			when TOP =>
				iterate (module.board.keepout.top.zones, query_zone'access);
				iterate (module.board.keepout.top.cutouts, query_cutout'access);

			when BOTTOM =>
				iterate (module.board.keepout.bottom.zones, query_zone'access);
				iterate (module.board.keepout.bottom.cutouts, query_cutout'access);
		end case;
	end query_items;
	
	
begin -- draw_keepout
-- 	put_line ("draw keepout ...");
	
	pac_generic_modules.query_element (
		position	=> active_module,
		process		=> query_items'access);


	-- Draw the zone begin drawn:
	draw_live_zone (LAYER_CAT_KEEPOUT);

	
end draw_keepout;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
