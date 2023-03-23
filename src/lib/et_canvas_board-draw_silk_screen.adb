------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW SILK SCREEN                          --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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
--                                                                          --
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
with et_silkscreen;				use et_silkscreen;
with et_silkscreen.boards;		use et_silkscreen.boards;
with et_colors;					use et_colors;
with et_board_ops.text;			use et_board_ops.text;


separate (et_canvas_board)


procedure draw_silk_screen (
	face : in type_face)
is
	use et_board_shapes_and_text;
	use pac_geometry_2;	

	use et_canvas_board_silkscreen;
	use pac_silk_lines;
	use pac_silk_arcs;
	use pac_silk_circles;
	use pac_silk_contours;
	use pac_silk_texts;
	use et_pcb.pac_text_placeholders;



	procedure set_default_brightness is begin
		set_color_silkscreen (context.cr, face, NORMAL);
	end set_default_brightness;
		
	procedure set_highlight_brightness is begin
		set_color_silkscreen (context.cr, face, BRIGHT);
	end set_highlight_brightness;

	
	
	procedure query_line (c : in pac_silk_lines.cursor) is 
		line : type_silk_line renames element (c);

		procedure draw_unchanged is begin
			draw_line (to_line_fine (line), line.width);
		end draw_unchanged;

	begin
		set_line_width (context.cr, type_view_coordinate (line.width));
		
		if is_selected (c, face) then
			set_highlight_brightness;

			case verb is
				when VERB_MOVE =>
					if preliminary_object.ready then
						declare
							line_tmp : type_silk_line := line;
							POA : type_point renames preliminary_object.point_of_attack;
						begin
							case preliminary_object.tool is
								when MOUSE =>
									move_line_to (line_tmp, POA, snap_to_grid (get_mouse_position));

								when KEYBOARD =>
									move_line_to (line_tmp, POA, cursor_main.position);
							end case;

							draw_line (to_line_fine (line_tmp), line.width);
						end;
					end if;

				when others =>
					draw_unchanged;
					
			end case;
			
			set_default_brightness;
		else
			-- draw the line as it is:
			draw_unchanged;
		end if;
	end query_line;

	
	procedure query_arc (c : in pac_silk_arcs.cursor) is begin
		set_line_width (context.cr, type_view_coordinate (element (c).width));
		
		draw_arc (
			arc			=> to_arc_fine (element (c)),
			width		=> element (c).width);

	end query_arc;

	
	procedure query_circle (c : in pac_silk_circles.cursor) is begin
		set_line_width (context.cr, type_view_coordinate (element (c).width));

		draw_circle (
			circle		=> element (c),
			filled		=> NO,
			width		=> element (c).width);
				
	end query_circle;

	
	procedure query_contour (c : in pac_silk_contours.cursor) is 
		drawn : boolean := false;
	begin
		draw_contour (
			contour	=> element (c),
			filled	=> YES,
			width	=> zero,
			drawn	=> drawn);
	end query_contour;

	
	
	procedure query_placeholder (c : in et_pcb.pac_text_placeholders.cursor) is 
		v_text : type_vector_text;
	begin
		draw_text_origin (element (c).position);

		-- Set the line width of the vector text:
		set_line_width (context.cr, type_view_coordinate (element (c).line_width));

		-- Vectorize the text:
		v_text := vectorize_text (
			content		=> to_placeholder_content (current_active_module, element (c).meaning),
			size		=> element (c).size,
			rotation	=> get_rotation (element (c).position),
			position	=> element (c).position.place,
			mirror		=> face_to_mirror (face),
			line_width	=> element (c).line_width,
			alignment	=> element (c).alignment -- right, bottom
			);

		-- Draw the text:
		draw_vector_text (v_text, element (c).line_width);

	end query_placeholder;

	
	procedure query_text (c : in pac_silk_texts.cursor) is 
		text : type_silk_text renames element (c);

		-- Draws the given text as it is given:
		procedure draw_unchanged is begin
			draw_text_origin (text.position);

			-- Set the line width of the vector text:
			set_line_width (context.cr, type_view_coordinate (text.line_width));
			draw_vector_text (text.vectors, text.line_width);
		end draw_unchanged;

	begin
		if is_selected (c, face) then
			set_highlight_brightness;

			case verb is
				when VERB_MOVE =>
					if preliminary_text.ready then
						-- Draw a temporarily copy of the original text at
						-- the place where the tool is pointing at:
						declare
							text_tmp	: type_silk_text := text;
							destination	: type_point;
							offset		: type_distance_relative;
						begin
							case preliminary_text.tool is
								when MOUSE =>
									destination := snap_to_grid (get_mouse_position);
													  
								when KEYBOARD =>
									destination := cursor_main.position;
							end case;

							-- Get the relative distance of the destination to the original
							-- text position:
							offset := get_distance_relative (get_place (text_tmp), destination);

							-- Move the text:
							move_text (text_tmp, offset);
							move_vector_text (text_tmp.vectors, offset);

							draw_text_origin (text_tmp.position);

							-- Set the line width of the vector text:
							set_line_width (context.cr, type_view_coordinate (text_tmp.line_width));

							-- Draw the text:
							draw_vector_text (text_tmp.vectors, text_tmp.line_width);
						end;
					else
						draw_unchanged;
					end if;

				when others =>
					draw_unchanged;
					
			end case;

			-- After drawing a selected (highlighted) text, the brightness
			-- must be set to normal:
			set_default_brightness;

		else -- not selected
			draw_unchanged;
		end if;
	end query_text;


	
	procedure query_items (
		module_name	: in pac_module_name.bounded_string;
		module		: in et_schematic.type_module) is
	begin
		-- All silkscreen segments will be drawn with the same color:
		set_color_silkscreen (context.cr, face, NORMAL);

		case face is
			when TOP =>
				iterate (module.board.silk_screen.top.lines, query_line'access);
				iterate (module.board.silk_screen.top.arcs, query_arc'access);
				iterate (module.board.silk_screen.top.circles, query_circle'access);
				iterate (module.board.silk_screen.top.contours, query_contour'access);
				iterate (module.board.silk_screen.top.placeholders, query_placeholder'access);
				iterate (module.board.silk_screen.top.texts, query_text'access);

			when BOTTOM =>
				iterate (module.board.silk_screen.bottom.lines, query_line'access);
				iterate (module.board.silk_screen.bottom.arcs, query_arc'access);
				iterate (module.board.silk_screen.bottom.circles, query_circle'access);
				iterate (module.board.silk_screen.bottom.contours, query_contour'access);
				iterate (module.board.silk_screen.bottom.placeholders, query_placeholder'access);
				iterate (module.board.silk_screen.bottom.texts, query_text'access);

		end case;

	end query_items;


	
begin -- draw_silk_screen

	-- 	put_line ("draw board silk screen ...");
	
	pac_generic_modules.query_element (
		position	=> current_active_module,
		process		=> query_items'access);

	draw_text_being_placed (face, LAYER_CAT_SILKSCREEN);

	-- Draw the lines of a path that is being drawn:
	draw_path (LAYER_CAT_SILKSCREEN);
	
end draw_silk_screen;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
