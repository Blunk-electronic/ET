------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW OUTLINE                              --
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
with et_general;				use et_general;
with et_pcb;					use et_pcb;
with et_canvas_primitive_draw_ops;

separate (et_canvas_board)

procedure draw_outline (
	self    : not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context) is
	
	use type_pcb_contour_lines;
	use type_pcb_contour_arcs;
	use type_pcb_contour_circles;
	
	procedure query_line (c : in type_pcb_contour_lines.cursor) is begin
		pac_draw_package.draw_line (
			area		=> in_area,
			context		=> context,
			line		=> element (c),
			height		=> self.frame_height);

	end query_line;

	procedure query_arc (c : in type_pcb_contour_arcs.cursor) is begin
		pac_draw_package.draw_arc (
			area		=> in_area,
			context		=> context,
			arc			=> element (c),
			height		=> self.frame_height);
	end query_arc;

	procedure query_circle (c : in type_pcb_contour_circles.cursor) is begin
		pac_draw_package.draw_circle (
			area		=> in_area,
			context		=> context,
			circle		=> element (c),
			filled		=> NO, -- circles in outline are never filled
			height		=> self.frame_height);
	end query_circle;
	
	procedure query_segments (
		module_name	: in type_module_name.bounded_string;
		module		: in et_schematic.type_module) is
	begin
		-- All outline segments will be drawn with the same line width and color:
		cairo.set_line_width (context.cr, type_view_coordinate (et_packages.pcb_contour_line_width));
		set_color_outline (context.cr);
		
		iterate (module.board.contours.lines, query_line'access);
		iterate (module.board.contours.arcs, query_arc'access);
		iterate (module.board.contours.circles, query_circle'access);

	end query_segments;
	
begin -- draw_outline
-- 	put_line ("draw board outline ...");
	
	et_project.modules.type_modules.query_element (
		position	=> et_canvas_schematic.current_active_module,
		process		=> query_segments'access);
	
end draw_outline;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
