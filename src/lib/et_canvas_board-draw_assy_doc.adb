------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      BOARD DRAW ASSEMBLY DOCUMENTATION                   --
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

with et_assy_doc;				use et_assy_doc;
with et_assy_doc.boards;		use et_assy_doc.boards;
with ada.text_io;				use ada.text_io;
with et_colors;					use et_colors;

separate (et_canvas_board)


procedure draw_assy_doc (
	self    : not null access type_view;
	in_area	: in type_bounding_box := no_area;
	face	: in type_face)
is
	use et_board_shapes_and_text;
	use pac_geometry_2;	

	use pac_doc_lines;
	use pac_doc_arcs;
	use pac_doc_circles;
	use pac_doc_contours;
	use et_pcb.pac_text_placeholders;
	use pac_doc_texts;


	-- CS must be overwritten according to select status:
	brightness : type_brightness := NORMAL;

	
	procedure query_line (c : in pac_doc_lines.cursor) is begin
		set_line_width (context.cr, type_view_coordinate (element (c).width));
		
		draw_line (
			line	=> to_line_fine (element (c)),
			width	=> element (c).width);

	end query_line;

	
	procedure query_arc (c : in pac_doc_arcs.cursor) is begin
		set_line_width (context.cr, type_view_coordinate (element (c).width));
		
		draw_arc (
			arc		=> to_arc_fine (element (c)),
			width	=> element (c).width);

	end query_arc;

	
	procedure query_circle (c : in pac_doc_circles.cursor) is 
		circle : type_doc_circle renames element (c);
	begin
		set_line_width (context.cr, type_view_coordinate (circle.width));

		draw_circle (
			circle		=> circle,
			filled		=> NO,
			width		=> circle.width);

	end query_circle;

	
	procedure query_polygon (c : in pac_doc_contours.cursor) is 
		contour : type_doc_contour renames element (c);
		drawn : boolean := false;
	begin
		draw_contour (
			area	=> in_area,
			contour	=> contour,
			filled	=> YES,
			width	=> zero,
			drawn	=> drawn);

	end query_polygon;


	
	procedure query_placeholder (c : in et_pcb.pac_text_placeholders.cursor) is 
		v_text : type_vector_text;
	begin
		draw_text_origin (self, element (c).position, in_area);

		-- Set the line width of the vector text:
		set_line_width (context.cr, type_view_coordinate (element (c).line_width));

		-- Vectorize the text:
		v_text := vectorize_text (
			content		=> to_placeholder_content (element (c).meaning),
			size		=> element (c).size,
			rotation	=> get_rotation (element (c).position),
			position	=> element (c).position.place,
			mirror		=> face_to_mirror (face),
			line_width	=> element (c).line_width,
			alignment	=> element (c).alignment -- right, bottom
			);

		-- Draw the text:
		draw_vector_text (in_area, v_text, element (c).line_width);

	end query_placeholder;

	
	procedure query_text (c : in pac_doc_texts.cursor) is begin
		draw_text_origin (self, element (c).position, in_area);

		-- Set the line width of the vector text:
		set_line_width (context.cr, type_view_coordinate (element (c).line_width));

		-- Draw the text:
		draw_vector_text (in_area, element (c).vectors, element (c).line_width);
		
	end query_text;

	
	procedure query_items (
		module_name	: in pac_module_name.bounded_string;
		module		: in et_schematic.type_module) 
	is begin
		-- All assy_doc segments will be drawn with the same color:
		set_color_assy_doc (context.cr, face, brightness);

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

	
begin -- draw_assy_doc
-- 	put_line ("draw board assembly documentation ...");
	
	pac_generic_modules.query_element (
		position	=> current_active_module,
		process		=> query_items'access);

	draw_text_being_placed (self, in_area, face, LAYER_CAT_ASSY);
	
end draw_assy_doc;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
