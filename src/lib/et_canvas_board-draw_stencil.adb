------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      BOARD DRAW STENCIL / SOLDER PASTE MASK              --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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
with et_canvas_primitive_draw_ops;

separate (et_canvas_board)

procedure draw_stencil (
	self    : not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context;
	face	: in type_face) 
is
	use pac_draw_fab;
	use et_board_shapes_and_text;
	use et_board_shapes_and_text.pac_text_fab;
	use et_board_shapes_and_text.pac_shapes;	

	use et_packages;
	use pac_stencil_lines;
	use pac_stencil_arcs;
	use pac_stencil_circles;
	use pac_stencil_polygons;
	use pac_stencil_cutouts;

	use et_packages.pac_texts_with_content;
	
	procedure query_line (c : in pac_stencil_lines.cursor) is begin
		set_line_width (context.cr, type_view_coordinate (element (c).width));
		
		draw_line (
			area		=> in_area,
			context		=> context,
			line		=> element (c),
			width		=> element (c).width,
			height		=> self.frame_height);

	end query_line;

	procedure query_arc (c : in pac_stencil_arcs.cursor) is begin
		set_line_width (context.cr, type_view_coordinate (element (c).width));
		
		draw_arc (
			area		=> in_area,
			context		=> context,
			arc			=> element (c),
			width		=> element (c).width,
			height		=> self.frame_height);

	end query_arc;

	procedure query_circle (c : in pac_stencil_circles.cursor) is begin
		case element (c).filled is
			when NO =>
				-- We draw a normal non-filled circle:
				set_line_width (context.cr, type_view_coordinate (element (c).border_width));

				draw_circle (
					area		=> in_area,
					context		=> context,
					circle		=> element (c),
					filled		=> NO,
					width		=> element (c).border_width,
					height		=> self.frame_height);
				
			when YES =>
				-- We draw a solid filled circle:
				draw_circle (
					area		=> in_area,
					context		=> context,
					circle		=> element (c),
					filled		=> YES,
					width		=> zero,
					height		=> self.frame_height);

		end case;

	end query_circle;

	procedure query_polygon (c : in pac_stencil_polygons.cursor) is begin
		case element (c).fill_style is
			when SOLID =>
				draw_polygon (
					area	=> in_area,
					context	=> context,
					polygon	=> element (c),
					filled	=> YES,
					width	=> zero,
					height	=> self.frame_height);

			when HATCHED =>
				set_line_width (context.cr, type_view_coordinate (element (c).hatching.border_width));

				draw_polygon (
					area	=> in_area,
					context	=> context,
					polygon	=> element (c),
					filled	=> NO,
					width	=> element (c).hatching.border_width,
					height	=> self.frame_height);

				-- CS hatching ?
		end case;

	end query_polygon;

	procedure query_cutout (c : in pac_stencil_cutouts.cursor) is begin
		set_color_background (context.cr);
		
		draw_polygon (
			area	=> in_area,
			context	=> context,
			polygon	=> element (c),
			filled	=> YES,
			width	=> zero,
			height	=> self.frame_height);

	end query_cutout;

	procedure query_text (c : in et_packages.pac_texts_with_content.cursor) is 
		use pac_vector_text_lines;
		vector_text : pac_vector_text_lines.list;
	begin
		draw_text_origin (self, element (c).position, in_area, context);

		-- Set the line width of the vector text:
		set_line_width (context.cr, type_view_coordinate (element (c).line_width));

		-- Vectorize the text:
		vector_text := vectorize_text (
			content		=> element (c).content,
			size		=> element (c).size,
			rotation	=> rot (element (c).position),
			position	=> type_point (element (c).position),
			mirror		=> face_to_mirror (face),
			line_width	=> element (c).line_width,
			alignment	=> element (c).alignment -- right, bottom
			);

		-- Draw the text:
		draw_vector_text (in_area, context, vector_text,
			element (c).line_width, self.frame_height);
		
	end query_text;

	
	procedure query_items (
		module_name	: in pac_module_name.bounded_string;
		module		: in et_schematic.type_module) is
	begin
		-- All stencil segments will be drawn with the same color:
		set_color_stencil (context.cr, face, self.scale);

		case face is
			when TOP =>
				iterate (module.board.stencil.top.lines, query_line'access);
				iterate (module.board.stencil.top.arcs, query_arc'access);
				iterate (module.board.stencil.top.circles, query_circle'access);
				iterate (module.board.stencil.top.polygons, query_polygon'access);
				iterate (module.board.stencil.top.cutouts, query_cutout'access);
				iterate (module.board.stencil.top.texts, query_text'access);
				
			when BOTTOM =>
				iterate (module.board.stencil.bottom.lines, query_line'access);
				iterate (module.board.stencil.bottom.arcs, query_arc'access);
				iterate (module.board.stencil.bottom.circles, query_circle'access);
				iterate (module.board.stencil.bottom.polygons, query_polygon'access);
				iterate (module.board.stencil.bottom.cutouts, query_cutout'access);
				iterate (module.board.stencil.bottom.texts, query_text'access);				
		end case;

	end query_items;
	
begin -- draw_stencil
-- 	put_line ("draw stencil / solder paste mask ...");
	
	pac_generic_modules.query_element (
		position	=> et_canvas_schematic.current_active_module,
		process		=> query_items'access);

	draw_text_being_placed (self, in_area, context, face, LAYER_CAT_STENCIL);
	
end draw_stencil;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
