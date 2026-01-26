------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      BOARD DRAW ASSEMBLY DOCUMENTATION                   --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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

with et_mirroring;					use et_mirroring;
with et_primitive_objects;			use et_primitive_objects;
with et_assy_doc;					use et_assy_doc;
with et_colors;						use et_colors;
with et_text_content;				use et_text_content;
with et_board_ops_text;				use et_board_ops_text;

with et_pcb_placeholders.non_conductor;	use et_pcb_placeholders.non_conductor;

separate (et_canvas_board)


procedure draw_assy (
	face : in type_face)
is
	use et_colors.board;
	use et_board_text;

	use et_canvas_board_assy_doc;
	use pac_doc_lines;
	use pac_doc_arcs;
	use pac_doc_circles;
	use pac_doc_zones;
	use pac_doc_texts;
	use pac_placeholders_non_conductor;

	use et_canvas_board_preliminary_object;



	procedure set_default_brightness is begin
		set_color_assy_doc (face, NORMAL);
	end set_default_brightness;
		
	procedure set_highlight_brightness is begin
		set_color_assy_doc (face, BRIGHT);
	end set_highlight_brightness;

	
	
	procedure query_line (c : in pac_doc_lines.cursor) is 
		line : type_doc_line renames element (c);

		procedure draw is begin
			draw_line (line => line, width => line.width, do_stroke => true);
		end draw;
		
	begin
		if is_selected (c) then
			set_highlight_brightness;
			draw;			
			set_default_brightness;
		else
			draw;
		end if;
	end query_line;

	
	
	procedure query_arc (c : in pac_doc_arcs.cursor) is
		arc : type_doc_arc renames element (c);

		procedure draw is begin
			draw_arc (
				arc			=> arc,
				width		=> arc.width,
				do_stroke	=> true);
		end draw;
		
	begin
		if is_selected (c) then
			set_highlight_brightness;
			draw;			
			set_default_brightness;
		else
			draw;
		end if;
	end query_arc;

	
	
	procedure query_circle (c : in pac_doc_circles.cursor) is 
		circle : type_doc_circle renames element (c);
	begin
		-- CS is_selected
		
		draw_circle (
			circle		=> circle,
			filled		=> NO,
			width		=> circle.width,
			do_stroke	=> true);				
	end query_circle;

	
	procedure query_zone (c : in pac_doc_zones.cursor) is
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
	is 

		
		procedure query_text (c : in pac_doc_texts.cursor) is 
			text : type_doc_text renames element (c);

			
			procedure draw is 
				use et_mirroring;
				use pac_draw_text;
			begin
				if face = BOTTOM then
					draw_vector_text (
						text			=> text,
						mirror			=> MIRROR_ALONG_Y_AXIS,
						place_absolute	=> true);

				else
					draw_vector_text (text);
				end if;
			end draw;

			
		begin
			if is_selected (text) then
				set_highlight_brightness;
				draw;
				set_default_brightness;
			else
				draw;
			end if;
		end query_text;


		
		
		procedure query_placeholder (
			c : in pac_placeholders_non_conductor.cursor) 
		is
			placeholder : type_placeholder_non_conductor renames element (c);
			
			use pac_text_vectorized;
			content : pac_text_content.bounded_string;
			text : type_text_fab_with_content;


			procedure draw is
				use pac_draw_text;
				use et_mirroring;
			begin
				if face = BOTTOM then
					draw_vector_text (
						text			=> text,
						mirror			=> MIRROR_ALONG_Y_AXIS,
						place_absolute	=> true);

				else
					draw_vector_text (text);
				end if;
			end draw;

			
		begin
			-- Build the final content to be drawn:
			content := to_placeholder_content (
				active_module, placeholder.meaning);
			-- put_line ("content " & to_string (content));

			-- Build the text to be drawn:
			text := (type_text_fab (placeholder) with content);

			-- Draw the placeholder highlighted if it is selected:
			if is_selected (placeholder) then
				set_highlight_brightness;
				draw;
				set_default_brightness;
			else
				-- not selected
				draw;
			end if;
		end query_placeholder;


	begin
		-- All assy_doc segments will be drawn with the same color:
		set_color_assy_doc (face, NORMAL);

		case face is
			when TOP =>
				iterate (module.board.assy_doc.top.lines, query_line'access);
				iterate (module.board.assy_doc.top.arcs, query_arc'access);
				iterate (module.board.assy_doc.top.circles, query_circle'access);
				iterate (module.board.assy_doc.top.zones, query_zone'access);
				iterate (module.board.assy_doc.top.placeholders, query_placeholder'access);
				iterate (module.board.assy_doc.top.texts, query_text'access);

			when BOTTOM =>
				iterate (module.board.assy_doc.bottom.lines, query_line'access);
				iterate (module.board.assy_doc.bottom.arcs, query_arc'access);
				iterate (module.board.assy_doc.bottom.circles, query_circle'access);
				iterate (module.board.assy_doc.bottom.zones, query_zone'access);
				iterate (module.board.assy_doc.bottom.placeholders, query_placeholder'access);
				iterate (module.board.assy_doc.bottom.texts, query_text'access);
		end case;
	end query_items;

	
	
begin
	
-- 	put_line ("draw board assembly documentation ...");
	
	pac_generic_modules.query_element (
		position	=> active_module,
		process		=> query_items'access);

	draw_text_being_placed (face, LAYER_CAT_ASSY);

	-- Draw the lines of a path that is being drawn:
	draw_path (LAYER_CAT_ASSY);

	-- Draw the zone begin drawn:
	draw_live_zone (LAYER_CAT_ASSY);
	
end draw_assy;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
