------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      BOARD DRAW STENCIL / SOLDER PASTE MASK              --
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

with ada.text_io;				use ada.text_io;

with et_stencil;				use et_stencil;
with et_colors;					use et_colors;
with et_modes.board;
with et_canvas_tool;


separate (et_canvas_board_2)


procedure draw_stencil (
	face : in type_face) 
is
	use et_colors.board;
	use et_board_text;

	use pac_stencil_lines;
	use pac_stencil_arcs;
	use pac_stencil_circles;
	use pac_stencil_zones;
	

	procedure set_default_brightness is begin
		set_color_stencil (face, NORMAL);
	end set_default_brightness;

	
	procedure set_highlight_brightness is begin
		set_color_stencil (face, BRIGHT);
	end set_highlight_brightness;


	
	
	procedure query_line (c : in pac_stencil_lines.cursor) is 
		line : type_stencil_line renames element (c);

		procedure draw is begin
			draw_line (line => line, width => line.width, do_stroke => true);
		end draw;

	begin
		if is_selected (line) then
			set_highlight_brightness;
			draw;
			set_default_brightness;
		else
			draw;
		end if;
	end query_line;


	
	procedure query_arc (c : in pac_stencil_arcs.cursor) is 
		arc : type_stencil_arc renames element (c);

		procedure draw is begin
			draw_arc (
				arc		=> element (c),
				width	=> element (c).width,
				do_stroke => true);
		end draw;

	begin
		if is_selected (arc) then
			set_highlight_brightness;
			draw;
			set_default_brightness;
		else
			draw;
		end if;
	end query_arc;

	
	procedure query_circle (c : in pac_stencil_circles.cursor) is 
		-- CS use renames
	begin
		draw_circle (
			circle		=> element (c),
			filled		=> NO,
			width		=> element (c).width,
			do_stroke => true);
	end query_circle;

	
	procedure query_zone (c : in pac_stencil_zones.cursor) is
		-- CS use renames
		use pac_draw_contours;
	begin
		draw_contour (
			contour	=> element (c),
			filled	=> YES,
			width	=> zero);
	end query_zone;

	
	procedure query_items (
		module_name	: in pac_module_name.bounded_string;
		module		: in type_generic_module) 
	is begin
		-- All stencil segments will be drawn with the same color:
		set_color_stencil (face, NORMAL);

		case face is
			when TOP =>
				iterate (module.board.stencil.top.lines, query_line'access);
				iterate (module.board.stencil.top.arcs, query_arc'access);
				iterate (module.board.stencil.top.circles, query_circle'access);
				iterate (module.board.stencil.top.zones, query_zone'access);
				
			when BOTTOM =>
				iterate (module.board.stencil.bottom.lines, query_line'access);
				iterate (module.board.stencil.bottom.arcs, query_arc'access);
				iterate (module.board.stencil.bottom.circles, query_circle'access);
				iterate (module.board.stencil.bottom.zones, query_zone'access);
		end case;
	end query_items;

	
begin
-- 	put_line ("draw stencil / solder paste mask ...");
	
	pac_generic_modules.query_element (
		position	=> active_module,
		process		=> query_items'access);


	-- Draw the lines of a path that is being drawn:
	draw_path (LAYER_CAT_STENCIL);

	-- Draw the zone begin drawn:
	draw_live_zone (LAYER_CAT_STENCIL);
	
	
end draw_stencil;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
