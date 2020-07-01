------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      BOARD DRAW ASSEMBLY DOCUMENTATION                   --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

separate (et_canvas_board)

procedure draw_assy_doc (
	self    : not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context;
	face	: in type_face) is

	use et_general;
	use et_terminals.pac_shapes;	
	use et_packages;
	use type_doc_lines;
	use type_doc_arcs;
	use type_doc_circles;
	use pac_doc_polygons;
	use pac_doc_cutouts;
	use et_pcb.pac_text_placeholders;
	use type_texts_with_content;
	
	procedure query_line (c : in type_doc_lines.cursor) is begin
		cairo.set_line_width (context.cr, type_view_coordinate (element (c).width));
		
		pac_draw_package.draw_line (
			area		=> in_area,
			context		=> context,
			line		=> element (c),
			height		=> self.frame_height);

	end query_line;

	procedure query_arc (c : in type_doc_arcs.cursor) is begin
		cairo.set_line_width (context.cr, type_view_coordinate (element (c).width));
		
		pac_draw_package.draw_arc (
			area		=> in_area,
			context		=> context,
			arc			=> element (c),
			height		=> self.frame_height);

	end query_arc;

	procedure query_circle (c : in type_doc_circles.cursor) is begin
		case element (c).filled is
			when NO =>
				-- We draw a normal non-filled circle:
				cairo.set_line_width (context.cr, type_view_coordinate (element (c).border_width));

				pac_draw_package.draw_circle (
					area		=> in_area,
					context		=> context,
					circle		=> element (c),
					filled		=> NO,
					height		=> self.frame_height);
				
			when YES =>
				-- We draw a filled circle with a certain fill style:
				case element (c).fill_style is
					when SOLID =>
						pac_draw_package.draw_circle (
							area		=> in_area,
							context		=> context,
							circle		=> element (c),
							filled		=> YES,
							height		=> self.frame_height);

					when HATCHED 	=> null; -- CS
				end case;
		end case;

	end query_circle;

	procedure query_polygon (c : in pac_doc_polygons.cursor) is begin
		case element (c).fill_style is
			when SOLID =>
				pac_draw_package.draw_polygon (
					area	=> in_area,
					context	=> context,
					polygon	=> element (c),
					filled	=> YES,
					height	=> self.frame_height);

			when HATCHED =>
				set_line_width (context.cr, type_view_coordinate (element (c).hatching.border_width));

				pac_draw_package.draw_polygon (
					area	=> in_area,
					context	=> context,
					polygon	=> element (c),
					filled	=> NO,
					height	=> self.frame_height);

				-- CS hatching ?
		end case;

	end query_polygon;

	procedure query_cutout (c : in pac_doc_cutouts.cursor) is begin
		save (context.cr);
		set_color_background (context.cr);
		
		pac_draw_package.draw_polygon (
			area	=> in_area,
			context	=> context,
			polygon	=> element (c),
			filled	=> YES,
			height	=> self.frame_height);

		restore (context.cr);
	end query_cutout;

	
	procedure query_placeholder (c : in et_pcb.pac_text_placeholders.cursor) is 
		use pac_text.pac_vector_text_lines;
		vector_text : pac_text.pac_vector_text_lines.list;
	begin
		draw_text_origin (self, element (c).position, in_area, context);

		-- Set the line width of the vector text:
		set_line_width (context.cr, type_view_coordinate (element (c).line_width));

		-- Vectorize the text:
		vector_text := pac_text.vectorize (
			content		=> to_placeholder_content (element (c).meaning),
			size		=> element (c).size,
			rotation	=> rot (element (c).position),
			position	=> type_point (element (c).position),
			mirror		=> face_to_mirror (face),
			line_width	=> element (c).line_width,
			alignment	=> element (c).alignment -- right, bottom
			);

		-- Draw the text:
		pac_draw_package.draw_vector_text (in_area, context, vector_text, self.frame_height);

	end query_placeholder;

	procedure query_text (c : in type_texts_with_content.cursor) is 
		use pac_text.pac_vector_text_lines;
		vector_text : pac_text.pac_vector_text_lines.list;
	begin
		draw_text_origin (self, element (c).position, in_area, context);

		-- Set the line width of the vector text:
		set_line_width (context.cr, type_view_coordinate (element (c).line_width));

		-- Vectorize the text:
		vector_text := pac_text.vectorize (
			content		=> element (c).content,
			size		=> element (c).size,
			rotation	=> rot (element (c).position),
			position	=> type_point (element (c).position),
			mirror		=> face_to_mirror (face),
			line_width	=> element (c).line_width,
			alignment	=> element (c).alignment -- right, bottom
			);

		-- Draw the text:
		pac_draw_package.draw_vector_text (in_area, context, vector_text, self.frame_height);
		
	end query_text;

	
	procedure query_items (
		module_name	: in type_module_name.bounded_string;
		module		: in et_schematic.type_module) is
	begin
		-- All assy_doc segments will be drawn with the same color:
		set_color_assy_doc (context.cr, face);

		case face is
			when TOP =>
				iterate (module.board.assy_doc.top.lines, query_line'access);
				iterate (module.board.assy_doc.top.arcs, query_arc'access);
				iterate (module.board.assy_doc.top.circles, query_circle'access);
				iterate (module.board.assy_doc.top.polygons, query_polygon'access);
				iterate (module.board.assy_doc.top.cutouts, query_cutout'access);
				iterate (module.board.assy_doc.top.placeholders, query_placeholder'access);
				iterate (module.board.assy_doc.top.texts, query_text'access);

			when BOTTOM =>
				iterate (module.board.assy_doc.bottom.lines, query_line'access);
				iterate (module.board.assy_doc.bottom.arcs, query_arc'access);
				iterate (module.board.assy_doc.bottom.circles, query_circle'access);
				iterate (module.board.assy_doc.bottom.polygons, query_polygon'access);
				iterate (module.board.assy_doc.bottom.cutouts, query_cutout'access);
				iterate (module.board.assy_doc.bottom.placeholders, query_placeholder'access);
				iterate (module.board.assy_doc.bottom.texts, query_text'access);

		end case;

	end query_items;
	
begin -- draw_assy_doc
-- 	put_line ("draw board assembly documentation ...");
	
	pac_generic_modules.query_element (
		position	=> et_canvas_schematic.current_active_module,
		process		=> query_items'access);
	
end draw_assy_doc;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
