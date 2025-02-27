------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      BOARD DRAW SOLDER STOP MASK                         --
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

with ada.text_io;				use ada.text_io;

with et_stopmask;				use et_stopmask;
with et_colors;					use et_colors;
with et_board_ops.text;			use et_board_ops.text;
with et_canvas_tool;
with et_schematic;
with et_pcb_placeholders;		use et_pcb_placeholders;


separate (et_canvas_board_2)


procedure draw_stop (
	face : in type_face)
is
	use et_colors.board;
	use et_board_shapes_and_text;
	
	use pac_stop_lines;
	use pac_stop_arcs;
	use pac_stop_circles;
	use pac_stop_zones;
	use pac_text_placeholders;
	use pac_stop_texts;


	procedure set_default_brightness is begin
		set_color_stop_mask (face, NORMAL);
	end set_default_brightness;
		
	procedure set_highlight_brightness is begin
		set_color_stop_mask (face, BRIGHT);
	end set_highlight_brightness;

	
	
	procedure query_line (c : in pac_stop_lines.cursor) is 
		-- CS use renames
	begin
		draw_line (
			line	=> element (c),
			width	=> element (c).width,
			do_stroke => true);

	end query_line;

	
	procedure query_arc (c : in pac_stop_arcs.cursor) is 
		-- CS use renames
	begin		
		draw_arc (
			arc		=> element (c),
			width	=> element (c).width,
			do_stroke => true);
	end query_arc;

	
	procedure query_circle (c : in pac_stop_circles.cursor) is 
		-- CS use renames
	begin
		draw_circle (
			circle	=> element (c),
			filled	=> NO,
			width	=> element (c).width,
			do_stroke => true);

	end query_circle;

	
	
	procedure query_zone (c : in pac_stop_zones.cursor) is
		use pac_draw_contours;
	begin
		draw_contour (
			contour	=> element (c),
			filled	=> YES,
			width	=> zero);
	end query_zone;

	
	
	procedure query_placeholder (
		c : in pac_text_placeholders.cursor)
	is 
		use pac_text;
		use pac_draw_text;

		use et_text;
		content : pac_text_content.bounded_string;

		t : type_text_fab_with_content;
	begin
		-- Build the final content to be drawn:
		content := to_placeholder_content (active_module, element (c).meaning);
		-- put_line ("content " & to_string (content));

		-- Build the text to be drawn:
		t := (type_text_fab (element (c)) with content);

		-- Draw the placeholder highlighted if it is selected:
		if is_selected (c) then
			set_highlight_brightness;
			draw_vector_text (t);
			set_default_brightness;
		else
			-- not selected
			draw_vector_text (t);
		end if;
	end query_placeholder;



	
	
	procedure query_text (c : in pac_stop_texts.cursor) is 
		use pac_draw_text;
	begin
		if is_selected (c) then
			set_highlight_brightness;
			draw_vector_text (element (c));
			set_default_brightness;
		else
			draw_vector_text (element (c));
		end if;
	end query_text;


	
	procedure query_items (
		module_name	: in pac_module_name.bounded_string;
		module		: in type_generic_module) 
	is begin
		-- All stop mask segments will be drawn with the same color:
		set_color_stop_mask (face, NORMAL);

		case face is
			when TOP =>
				iterate (module.board.stopmask.top.lines, query_line'access);
				iterate (module.board.stopmask.top.arcs, query_arc'access);
				iterate (module.board.stopmask.top.circles, query_circle'access);
				iterate (module.board.stopmask.top.contours, query_zone'access);
				iterate (module.board.stopmask.top.placeholders, query_placeholder'access);
				iterate (module.board.stopmask.top.texts, query_text'access);

			when BOTTOM =>
				iterate (module.board.stopmask.bottom.lines, query_line'access);
				iterate (module.board.stopmask.bottom.arcs, query_arc'access);
				iterate (module.board.stopmask.bottom.circles, query_circle'access);
				iterate (module.board.stopmask.bottom.contours, query_zone'access);
				iterate (module.board.stopmask.bottom.placeholders, query_placeholder'access);
				iterate (module.board.stopmask.bottom.texts, query_text'access);

		end case;

	end query_items;

	
begin -- draw_stop
	
	-- 	put_line ("draw solder stop mask ...");
	
	pac_generic_modules.query_element (
		position	=> active_module,
		process		=> query_items'access);

	draw_text_being_placed (face, LAYER_CAT_STOP);

	-- Draw the lines of a path that is being drawn:
	draw_path (LAYER_CAT_STOP);

	
	-- Draw the zone begin drawn:
	draw_live_zone (LAYER_CAT_STOP);
	
end draw_stop;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
