------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      BOARD DRAW ASSEMBLY DOCUMENTATION                   --
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

with et_geometry;
with et_assy_doc;				use et_assy_doc;
with et_assy_doc.boards;		use et_assy_doc.boards;
with ada.text_io;				use ada.text_io;
with et_colors;					use et_colors;
with et_modes.board;
with et_board_ops.text;			use et_board_ops.text;
with et_canvas_tool;
with et_schematic;
with et_pcb;


separate (et_canvas_board_2)


procedure draw_assy (
	face : in type_face)
is
	use et_colors.board;
	use et_board_shapes_and_text;

	use et_canvas_board_assy_doc;
	use pac_doc_lines;
	use pac_doc_arcs;
	use pac_doc_circles;
	use pac_doc_contours;
	use pac_doc_texts;
	use et_pcb.pac_text_placeholders;



	procedure set_default_brightness is begin
		set_color_assy_doc (face, NORMAL);
	end set_default_brightness;
		
	procedure set_highlight_brightness is begin
		set_color_assy_doc (face, BRIGHT);
	end set_highlight_brightness;

	
	
	procedure query_line (c : in pac_doc_lines.cursor) is 
		line : type_doc_line renames element (c);

		procedure draw_unchanged is begin
			draw_line (line => line, width => line.width);
		end draw_unchanged;

		use et_modes.board;
		use et_canvas_tool;
		
	begin
		--if is_selected (c, face) then
		if is_selected (c) then
			set_highlight_brightness;

			case verb is
				when VERB_MOVE =>
					if preliminary_object.ready then
						declare
							line_tmp : type_doc_line := line;
							POA : type_vector_model renames preliminary_object.point_of_attack;
						begin
							case preliminary_object.tool is
								when MOUSE =>
									move_line_to (line_tmp, POA, snap_to_grid (get_mouse_position));

								when KEYBOARD =>
									move_line_to (line_tmp, POA, get_cursor_position);
							end case;

							draw_line (line => line_tmp, width => line.width);
						end;
					else
						draw_unchanged;
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

	
	
	procedure query_arc (c : in pac_doc_arcs.cursor) is
		arc : type_doc_arc renames element (c);
	begin
		-- CS is_selected
		draw_arc (
			arc			=> arc,
			width		=> arc.width,
			do_stroke	=> true);
	end query_arc;

	
	procedure query_circle (c : in pac_doc_circles.cursor) is 
		circle : type_doc_circle renames element (c);
		use et_geometry;
	begin
		-- CS is_selected
		
		draw_circle (
			circle		=> circle,
			filled		=> et_geometry.NO,
			width		=> circle.width,
			do_stroke	=> true);				
	end query_circle;

	
	procedure query_polygon (c : in pac_doc_contours.cursor) is -- CS rename to query_contour
		contour : type_doc_contour renames element (c);
		use et_geometry;
		use pac_draw_contours;
	begin
		draw_contour (
			contour	=> element (c),
			filled	=> YES,
			width	=> zero);
	end query_polygon;


	
	procedure query_placeholder (c : in et_pcb.pac_text_placeholders.cursor) is 
		-- CS use rename
		use pac_text;
		v_text : type_vector_text;

		use pac_draw_text;
	begin
		draw_origin (element (c).position);

		-- Set the line width of the vector text:
		set_linewidth (element (c).line_width);

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


	
	procedure query_text (c : in pac_doc_texts.cursor) is 
		text : type_doc_text renames element (c);

		use pac_draw_text;
		
		
		-- Draws the given text as it is given:
		procedure draw_unchanged is begin
			draw_origin (text.position);

			draw_vector_text (text.vectors, text.line_width);
		end draw_unchanged;


		use et_canvas_board_texts;
		use et_modes.board;
		use et_canvas_tool;

		use pac_text;
		
	begin
		if is_selected (c, face) then
			set_highlight_brightness;

			case verb is
				when VERB_MOVE =>
					if preliminary_text.ready then
						-- Draw a temporarily copy of the original text at
						-- the place where the tool is pointing at:
						declare
							text_tmp	: type_doc_text := text;
							destination	: type_vector_model;
							offset		: type_distance_relative;
						begin
							case preliminary_text.tool is
								when MOUSE =>
									destination := snap_to_grid (get_mouse_position);
													  
								when KEYBOARD =>
									destination := get_cursor_position;
							end case;

							-- Get the relative distance of the destination to the original
							-- text position:
							offset := get_distance_relative (get_place (text_tmp), destination);

							-- Move the text:
							move_text (text_tmp, offset);
							move_vector_text (text_tmp.vectors, offset);

							draw_origin (text_tmp.position);

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
		module		: in et_schematic.type_module) 
	is begin
		-- All assy_doc segments will be drawn with the same color:
		set_color_assy_doc (face, NORMAL);

		case face is
			when TOP =>
				iterate (module.board.assy_doc.top.lines, query_line'access);
				iterate (module.board.assy_doc.top.arcs, query_arc'access);
				iterate (module.board.assy_doc.top.circles, query_circle'access);
				iterate (module.board.assy_doc.top.contours, query_polygon'access);
				iterate (module.board.assy_doc.top.placeholders, query_placeholder'access);
				iterate (module.board.assy_doc.top.texts, query_text'access);

			when BOTTOM =>
				iterate (module.board.assy_doc.bottom.lines, query_line'access);
				iterate (module.board.assy_doc.bottom.arcs, query_arc'access);
				iterate (module.board.assy_doc.bottom.circles, query_circle'access);
				iterate (module.board.assy_doc.bottom.contours, query_polygon'access);
				iterate (module.board.assy_doc.bottom.placeholders, query_placeholder'access);
				iterate (module.board.assy_doc.bottom.texts, query_text'access);

		end case;
	end query_items;

	
	
begin -- draw_assy
	
-- 	put_line ("draw board assembly documentation ...");
	
	pac_generic_modules.query_element (
		position	=> current_active_module,
		process		=> query_items'access);

	draw_text_being_placed (face, LAYER_CAT_ASSY);

	-- CS
	-- Draw the lines of a path that is being drawn:
	-- draw_path (LAYER_CAT_ASSY);
	
end draw_assy;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
