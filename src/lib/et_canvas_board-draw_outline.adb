------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW OUTLINE                              --
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

--with ada.text_io;				use ada.text_io;

with et_pcb_contour;			use et_pcb_contour;

separate (et_canvas_board)

procedure draw_outline (
	self    : not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context) 
is	
	use et_board_shapes_and_text;
	use pac_geometry_2;
	use pac_polygons;
	use pac_polygon_segments;
	
	--use pac_text_fab;
	--use pac_texts_fab_with_content;

	use pac_contour_texts;
	
	procedure query_segment (c : in pac_polygon_segments.cursor) is 
		use pac_draw_fab;
	begin
		case element (c).shape is
			when LINE =>
				draw_line (
					area		=> in_area,
					context		=> context,
					line		=> element (c).segment_line,
					width		=> et_packages.pcb_contour_line_width,
					height		=> self.frame_height);

			when ARC =>
				draw_arc (
					area		=> in_area,
					context		=> context,
					arc			=> element (c).segment_arc,
					width		=> et_packages.pcb_contour_line_width,
					height		=> self.frame_height);
		end case;
	end query_segment;

	
	procedure query_text (c : in pac_contour_texts.cursor) is begin
		draw_text_origin (self, element (c).position, in_area, context);

		-- Set the line width of the vector text:
		set_line_width (context.cr, type_view_coordinate (element (c).line_width));

		-- Draw the text:
		pac_draw_fab.draw_vector_text (in_area, context, element (c).vectors,
			element (c).line_width, self.frame_height);
		
	end query_text;

	
	procedure query_outline_segments (
		module_name	: in pac_module_name.bounded_string;
		module		: in et_schematic.type_module)
	is begin
		if module.board.contours.outline.contours.circular then

			pac_draw_fab.draw_circle (
				area		=> in_area,
				context		=> context,
				circle		=> module.board.contours.outline.contours.circle,
				filled		=> NO, -- circles in outline are never filled
				width		=> et_packages.pcb_contour_line_width,
				height		=> self.frame_height);
			
		else
			iterate (module.board.contours.outline.contours.segments, query_segment'access);
		end if;
		
		iterate (module.board.contours.texts, query_text'access);
	end query_outline_segments;

	
	procedure query_holes (
		module_name	: in pac_module_name.bounded_string;
		module		: in et_schematic.type_module) 
	is
		use et_packages;
		use pac_pcb_cutouts;
		
		procedure query_hole (c : in pac_pcb_cutouts.cursor) is begin
			if element (c).contours.circular then

				pac_draw_fab.draw_circle (
					area		=> in_area,
					context		=> context,
					circle		=> element (c).contours.circle,
					filled		=> NO, -- holes are never filled
					width		=> et_packages.pcb_contour_line_width,
					height		=> self.frame_height);
				
			else
				iterate (element (c).contours.segments, query_segment'access);
			end if;
		end query_hole;
		
	begin
		iterate (module.board.contours.holes, query_hole'access);
	end query_holes;

	
begin -- draw_outline
-- 	put_line ("draw board outline ...");

	-- All outline segments, holes and texts will be 
	-- drawn with the same color:
	set_color_outline (context.cr);

	-- All outline segments and holes be 
	-- drawn with the same line width:
	set_line_width (context.cr, type_view_coordinate (et_packages.pcb_contour_line_width));

	-- The line width of texts is a property of a particular text and is
	-- NOT set here.
	
	pac_generic_modules.query_element (
		position	=> et_canvas_schematic.current_active_module,
		process		=> query_outline_segments'access);

	draw_text_being_placed_in_outline (self, in_area, context);
	
	pac_generic_modules.query_element (
		position	=> et_canvas_schematic.current_active_module,
		process		=> query_holes'access);

	
end draw_outline;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
