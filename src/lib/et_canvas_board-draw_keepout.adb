------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW KEEPOUT                              --
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

separate (et_canvas_board)

procedure draw_keepout (
	self    : not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context;
	face	: in type_face) 
is
	use et_board_shapes_and_text;
	use pac_text_fab;
	use pac_shapes;	

	use et_packages;
	use pac_keepout_lines;
	use pac_keepout_arcs;
	use pac_keepout_circles;
	use pac_keepout_polygons;
	use pac_keepout_cutouts;

	use pac_texts_fab_with_content;
	
	procedure query_line (c : in pac_keepout_lines.cursor) is begin
		
		pac_draw_fab.draw_line (
			area		=> in_area,
			context		=> context,
			line		=> element (c),
			width		=> keepout_line_width,
			height		=> self.frame_height);

	end query_line;

	
	procedure query_arc (c : in pac_keepout_arcs.cursor) is begin
		
		pac_draw_fab.draw_arc (
			area		=> in_area,
			context		=> context,
			arc			=> element (c),
			width		=> keepout_line_width,
			height		=> self.frame_height);

	end query_arc;

	
	procedure query_circle (c : in pac_keepout_circles.cursor) is begin
		case element (c).filled is
			when NO =>
				-- We draw a normal not-filled circle:
				pac_draw_fab.draw_circle (
					area		=> in_area,
					context		=> context,
					circle		=> element (c),
					filled		=> NO,
					width		=> keepout_line_width,
					height		=> self.frame_height);
				
			when YES =>
				-- We draw a solid filled circle:
				pac_draw_fab.draw_circle (
					area		=> in_area,
					context		=> context,
					circle		=> element (c),
					filled		=> YES,
					width		=> zero,
					height		=> self.frame_height);

		end case;

	end query_circle;

	
	procedure query_polygon (c : in pac_keepout_polygons.cursor) is begin
		pac_draw_fab.draw_polygon (
			area	=> in_area,
			context	=> context,
			polygon	=> element (c),
			filled	=> YES,
			width	=> zero,
			height	=> self.frame_height);

	end query_polygon;

	
	procedure query_cutout (c : in pac_keepout_cutouts.cursor) is begin
		set_color_background (context.cr);
		
		pac_draw_fab.draw_polygon (
			area	=> in_area,
			context	=> context,
			polygon	=> element (c),
			filled	=> YES,
			width	=> zero,
			height	=> self.frame_height);

	end query_cutout;

	
	procedure query_text (c : in pac_texts_fab_with_content.cursor) is 
		use pac_vector_text_lines;
	begin
		draw_text_origin (self, element (c).position, in_area, context);

		-- Set the line width of the vector text:
		set_line_width (context.cr, type_view_coordinate (element (c).line_width));
		
		-- Draw the text:
		pac_draw_fab.draw_vector_text (in_area, context, element (c).vectors,
			element (c).line_width, self.frame_height);
		
	end query_text;

	
	procedure query_items (
		module_name	: in pac_module_name.bounded_string;
		module		: in et_schematic.type_module) 
	is begin
		-- All keepout segments will be drawn with the same color:
		set_color_keepout (context.cr, face);

		cairo.set_line_width (context.cr, type_view_coordinate (keepout_line_width));
		
		case face is
			when TOP =>
				iterate (module.board.keepout.top.lines, query_line'access);
				iterate (module.board.keepout.top.arcs, query_arc'access);
				iterate (module.board.keepout.top.circles, query_circle'access);
				iterate (module.board.keepout.top.polygons, query_polygon'access);
				iterate (module.board.keepout.top.cutouts, query_cutout'access);
				iterate (module.board.keepout.top.texts, query_text'access);

			when BOTTOM =>
				iterate (module.board.keepout.bottom.lines, query_line'access);
				iterate (module.board.keepout.bottom.arcs, query_arc'access);
				iterate (module.board.keepout.bottom.circles, query_circle'access);
				iterate (module.board.keepout.bottom.polygons, query_polygon'access);
				iterate (module.board.keepout.bottom.cutouts, query_cutout'access);
				iterate (module.board.keepout.bottom.texts, query_text'access);
		end case;

	end query_items;
	
begin -- draw_keepout
-- 	put_line ("draw board keepout ...");
	
	pac_generic_modules.query_element (
		position	=> et_canvas_schematic.current_active_module,
		process		=> query_items'access);

	draw_text_being_placed (self, in_area, context, face, LAYER_CAT_KEEPOUT);
	
end draw_keepout;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
