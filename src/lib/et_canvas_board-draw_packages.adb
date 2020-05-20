------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW PACKAGES                             --
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
with cairo;						use cairo;
with pango.layout;				use pango.layout;

with et_general;				use et_general;
with et_symbols;
with et_schematic;
use et_schematic.type_nets;

with et_project;				use et_project;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_packages;				use et_packages;
use et_pcb_coordinates.geometry;

with et_pcb;
with et_pcb_stack;				use et_pcb_stack;

with et_display.board;			use et_display.board;
with et_colors;					use et_colors;

with et_canvas_primitive_draw_ops;

separate (et_canvas_board)

procedure draw_packages (
	self    : not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context;
	face	: in type_face) -- the side of the board to be drawn
is

	-- The deepest conductor layer towards bottom is defined by the layer stack:
	bottom_layer : constant type_signal_layer := 
		deepest_conductor_layer (et_canvas_schematic.current_active_module);

	
	procedure draw_package (
		model			: in et_packages.type_package_model_file.bounded_string;
		position		: in et_pcb_coordinates.type_package_position; -- incl. rotation and face
		flip			: in et_pcb.type_flipped;
		placeholders	: in et_packages.type_text_placeholders) is

		use et_packages.pac_shapes;
		use type_packages;

		function flipped return boolean is 
			use et_pcb;
		begin
			if flip = NO then return false;
			else return true;
			end if;
		end flipped;

		-- Destination is the face on which an object is to be drawn.
		-- (We can not assume that all objects of a package are on the same side
		-- of the board.)
		destination : type_face;
		type type_destination_inversed is (INVERSE, NOT_INVERSE);

		-- If the package is flipped, then objects of the former top side
		-- change to the bottom side and vice versa. 
		-- By default set_destination sets the destination side to BOTTOM if the package is flipped.
		procedure set_destination (i : in type_destination_inversed := NOT_INVERSE) is 
			use et_pcb;
		begin
			case flip is
				when YES =>
					case i is
						when INVERSE 		=> destination := TOP;
						when NOT_INVERSE	=> destination := BOTTOM;
					end case;
					
				when NO =>
					case i is
						when INVERSE 		=> destination := BOTTOM;
						when NOT_INVERSE	=> destination := TOP;
					end case;
			end case;
		end set_destination;
		
		-- locate the package model in the package library:
		package_cursor : constant et_packages.type_packages.cursor := locate_package_model (model);
	
		-- SILKSCREEN
		procedure draw_silkscreen is 

			-- LINES
			use type_silk_lines;
			line : type_silk_line;

			procedure draw_line (f : in type_face) is begin
				if silkscreen_enabled (f) then
				
					if f = face then
						if flipped then mirror (line, Y); end if;
						
						rotate_by (line, rot (position));
						move_by (line, type_point (position));

						set_color_silkscreen (context.cr, f);
						set_line_width (context.cr, type_view_coordinate (line.width));
						pac_draw_package.draw_line (in_area, context, line, self.frame_height);
					end if;

				end if;
			end draw_line;
			
			procedure query_line_top (c : in type_silk_lines.cursor) is begin
				line := element (c);
				set_destination;
				draw_line (destination);
			end query_line_top;

			procedure query_line_bottom (c : in type_silk_lines.cursor) is begin
				line := element (c);
				set_destination (INVERSE);
				draw_line (destination);
			end query_line_bottom;

			
			-- ARCS
			use type_silk_arcs;
			arc : type_silk_arc;

			procedure draw_arc (f : in type_face) is begin
				if silkscreen_enabled (f) then
					
					if f = face then
						if flipped then mirror (arc, Y); end if;
						
						rotate_by (arc, rot (position));
						move_by (arc, type_point (position));

						set_color_silkscreen (context.cr, f);
						set_line_width (context.cr, type_view_coordinate (arc.width));
						pac_draw_package.draw_arc (in_area, context, arc, self.frame_height);
					end if;
					
				end if;
			end draw_arc;
			
			procedure query_arc_top (c : in type_silk_arcs.cursor) is begin
				arc := element (c);
				set_destination;
				draw_arc (destination);
			end query_arc_top;

			procedure query_arc_bottom (c : in type_silk_arcs.cursor) is begin
				arc := element (c);
				set_destination (INVERSE);
				draw_arc (destination);
			end query_arc_bottom;

			
			-- CIRCLES
			use type_silk_circles;

			procedure draw_circle (
				circle	: in out type_fillable_circle;
				f 		: in type_face) 
			is begin
				if silkscreen_enabled (f) then
					
					if f = face then
						if flipped then mirror (circle, Y); end if;
						
						rotate_by (circle, rot (position));
						move_by (circle, type_point (position));

						set_color_silkscreen (context.cr, f);

						case circle.filled is
							when NO =>
								set_line_width (context.cr, type_view_coordinate (circle.border_width));
								pac_draw_package.draw_circle (in_area, context, circle, circle.filled, self.frame_height);

							when YES =>
								case circle.fill_style is
									when SOLID =>
										pac_draw_package.draw_circle (in_area, context, circle, circle.filled, self.frame_height);

									when HATCHED => null; -- CS
								end case;
						end case;

					end if;

				end if;
			end draw_circle;
			
			procedure query_circle_top (c : in type_silk_circles.cursor) is 
				circle : type_fillable_circle := element (c);
			begin
				set_destination;
				draw_circle (circle, destination);
			end query_circle_top;

			procedure query_circle_bottom (c : in type_silk_circles.cursor) is 
				circle : type_fillable_circle := element (c);
			begin
				set_destination (INVERSE);
				draw_circle (circle, destination);
			end query_circle_bottom;

			
			-- POLYGONS
			use pac_silk_polygons;

			procedure draw_polygon (
				polygon	: in out et_packages.type_polygon;
				f		: in type_face)
			is begin
				if silkscreen_enabled (f) then
					
					if f = face then
						if flipped then mirror (polygon, Y); end if;
						
						rotate_by (polygon, rot (position));
						move_by (polygon, type_point (position));

						set_color_silkscreen (context.cr, f);

						case polygon.fill_style is
							when SOLID =>
								pac_draw_package.draw_polygon (in_area, context, polygon, YES, self.frame_height);

							when HATCHED =>
								cairo.set_line_width (context.cr, type_view_coordinate (polygon.hatching.border_width));
								pac_draw_package.draw_polygon (in_area, context, polygon, NO, self.frame_height);
								-- CS hatching ?
						end case;
						
					end if;

				end if;
				
			end draw_polygon;
			
			procedure query_polygon_top (c : in pac_silk_polygons.cursor) is
				polygon : et_packages.type_polygon := element (c);
			begin
				set_destination;
				draw_polygon (polygon, destination);
			end query_polygon_top;

			procedure query_polygon_bottom (c : in pac_silk_polygons.cursor) is
				polygon : et_packages.type_polygon := element (c);
			begin
				set_destination (INVERSE);
				draw_polygon (polygon, destination);
			end query_polygon_bottom;


			-- CUTOUTS
			use pac_silk_cutouts;

			procedure draw_cutout (
				cutout	: in out type_cutout_zone;
				f		: in type_face)
			is begin
				if silkscreen_enabled (f) then
					
					if f = face then
						if flipped then mirror (cutout, Y); end if;
						
						rotate_by (cutout, rot (position));
						move_by (cutout, type_point (position));

						set_color_background (context.cr);

						pac_draw_package.draw_polygon (in_area, context, cutout, YES, self.frame_height);
					end if;

				end if;
				
			end draw_cutout;
			
			procedure query_cutout_top (c : in pac_silk_cutouts.cursor) is
				cutout : type_cutout_zone := element (c);
			begin
				set_destination;
				draw_cutout (cutout, destination);
			end query_cutout_top;

			procedure query_cutout_bottom (c : in pac_silk_cutouts.cursor) is
				cutout : type_cutout_zone := element (c);
			begin
				set_destination (INVERSE);
				draw_cutout (cutout, destination);
			end query_cutout_bottom;
			
		begin -- draw_silkscreen
			-- lines
			element (package_cursor).silk_screen.top.lines.iterate (query_line_top'access);
			element (package_cursor).silk_screen.bottom.lines.iterate (query_line_bottom'access);

			-- arcs
			element (package_cursor).silk_screen.top.arcs.iterate (query_arc_top'access);
			element (package_cursor).silk_screen.bottom.arcs.iterate (query_arc_bottom'access);

			-- circles
			element (package_cursor).silk_screen.top.circles.iterate (query_circle_top'access);
			element (package_cursor).silk_screen.bottom.circles.iterate (query_circle_bottom'access);

			-- polygons
			element (package_cursor).silk_screen.top.polygons.iterate (query_polygon_top'access);
			element (package_cursor).silk_screen.bottom.polygons.iterate (query_polygon_bottom'access);

			-- cutouts
			element (package_cursor).silk_screen.top.cutouts.iterate (query_cutout_top'access);
			element (package_cursor).silk_screen.bottom.cutouts.iterate (query_cutout_bottom'access);
			
			-- CS
			-- placeholders
			-- texts		: type_texts_with_content.list;
			
		end draw_silkscreen;

		
		-- ASSY DOC
		procedure draw_assembly_documentation is 

			-- LINES
			use type_doc_lines;
			line : type_doc_line;

			procedure draw_line (f : in type_face) is begin
				if assy_doc_enabled (f) then
				
					if f = face then
						if flipped then mirror (line, Y); end if;
						
						rotate_by (line, rot (position));
						move_by (line, type_point (position));

						set_color_assy_doc (context.cr, f);
						set_line_width (context.cr, type_view_coordinate (line.width));
						pac_draw_package.draw_line (in_area, context, line, self.frame_height);
					end if;

				end if;
			end draw_line;
			
			procedure query_line_top (c : in type_doc_lines.cursor) is begin
				line := element (c);
				set_destination;
				draw_line (destination);
			end query_line_top;

			procedure query_line_bottom (c : in type_doc_lines.cursor) is begin
				line := element (c);
				set_destination (INVERSE);
				draw_line (destination);
			end query_line_bottom;

			
			-- ARCS
			use type_doc_arcs;
			arc : type_doc_arc;

			procedure draw_arc (f : in type_face) is begin
				if assy_doc_enabled (f) then
					
					if f = face then
						if flipped then mirror (arc, Y); end if;
						
						rotate_by (arc, rot (position));
						move_by (arc, type_point (position));

						set_color_assy_doc (context.cr, f);
						set_line_width (context.cr, type_view_coordinate (arc.width));
						pac_draw_package.draw_arc (in_area, context, arc, self.frame_height);
					end if;
					
				end if;
			end draw_arc;
			
			procedure query_arc_top (c : in type_doc_arcs.cursor) is begin
				arc := element (c);
				set_destination;
				draw_arc (destination);
			end query_arc_top;

			procedure query_arc_bottom (c : in type_doc_arcs.cursor) is begin
				arc := element (c);
				set_destination (INVERSE);
				draw_arc (destination);
			end query_arc_bottom;

			
			-- CIRCLES
			use type_doc_circles;

			procedure draw_circle (
				circle	: in out type_fillable_circle;
				f 		: in type_face) 
			is begin
				if assy_doc_enabled (f) then
					
					if f = face then
						if flipped then mirror (circle, Y); end if;
						
						rotate_by (circle, rot (position));
						move_by (circle, type_point (position));

						set_color_assy_doc (context.cr, f);

						case circle.filled is
							when NO =>
								set_line_width (context.cr, type_view_coordinate (circle.border_width));
								pac_draw_package.draw_circle (in_area, context, circle, circle.filled, self.frame_height);

							when YES =>
								case circle.fill_style is
									when SOLID =>
										pac_draw_package.draw_circle (in_area, context, circle, circle.filled, self.frame_height);

									when HATCHED => null; -- CS
								end case;
						end case;

					end if;

				end if;
			end draw_circle;
			
			procedure query_circle_top (c : in type_doc_circles.cursor) is 
				circle : type_fillable_circle := element (c);
			begin
				set_destination;
				draw_circle (circle, destination);
			end query_circle_top;

			procedure query_circle_bottom (c : in type_doc_circles.cursor) is 
				circle : type_fillable_circle := element (c);
			begin
				set_destination (INVERSE);
				draw_circle (circle, destination);
			end query_circle_bottom;

			
			-- POLYGONS
			use pac_doc_polygons;

			procedure draw_polygon (
				polygon	: in out et_packages.type_polygon;
				f		: in type_face)
			is begin
				if assy_doc_enabled (f) then
					
					if f = face then
						if flipped then mirror (polygon, Y); end if;
						
						rotate_by (polygon, rot (position));
						move_by (polygon, type_point (position));

						set_color_assy_doc (context.cr, f);

						case polygon.fill_style is
							when SOLID =>
								pac_draw_package.draw_polygon (in_area, context, polygon, YES, self.frame_height);

							when HATCHED =>
								cairo.set_line_width (context.cr, type_view_coordinate (polygon.hatching.border_width));
								pac_draw_package.draw_polygon (in_area, context, polygon, NO, self.frame_height);
								-- CS hatching ?
						end case;
						
					end if;

				end if;
				
			end draw_polygon;
			
			procedure query_polygon_top (c : in pac_doc_polygons.cursor) is
				polygon : et_packages.type_polygon := element (c);
			begin
				set_destination;
				draw_polygon (polygon, destination);
			end query_polygon_top;

			procedure query_polygon_bottom (c : in pac_doc_polygons.cursor) is
				polygon : et_packages.type_polygon := element (c);
			begin
				set_destination (INVERSE);
				draw_polygon (polygon, destination);
			end query_polygon_bottom;


			-- CUTOUTS
			use pac_doc_cutouts;

			procedure draw_cutout (
				cutout	: in out et_packages.type_cutout_zone;
				f		: in type_face)
			is begin
				if assy_doc_enabled (f) then
					
					if f = face then
						if flipped then mirror (cutout, Y); end if;
						
						rotate_by (cutout, rot (position));
						move_by (cutout, type_point (position));

						set_color_background (context.cr);

						pac_draw_package.draw_polygon (in_area, context, cutout, YES, self.frame_height);
						
					end if;

				end if;
				
			end draw_cutout;
			
			procedure query_cutout_top (c : in pac_doc_cutouts.cursor) is
				cutout : type_cutout_zone := element (c);
			begin
				set_destination;
				draw_cutout (cutout, destination);
			end query_cutout_top;

			procedure query_cutout_bottom (c : in pac_doc_cutouts.cursor) is
				cutout : type_cutout_zone := element (c);
			begin
				set_destination (INVERSE);
				draw_cutout (cutout, destination);
			end query_cutout_bottom;
			
		begin -- draw_assembly_documentation
			-- lines
			element (package_cursor).assembly_documentation.top.lines.iterate (query_line_top'access);
			element (package_cursor).assembly_documentation.bottom.lines.iterate (query_line_bottom'access);

			-- arcs
			element (package_cursor).assembly_documentation.top.arcs.iterate (query_arc_top'access);
			element (package_cursor).assembly_documentation.bottom.arcs.iterate (query_arc_bottom'access);

			-- circles
			element (package_cursor).assembly_documentation.top.circles.iterate (query_circle_top'access);
			element (package_cursor).assembly_documentation.bottom.circles.iterate (query_circle_bottom'access);

			-- polygons
			element (package_cursor).assembly_documentation.top.polygons.iterate (query_polygon_top'access);
			element (package_cursor).assembly_documentation.bottom.polygons.iterate (query_polygon_bottom'access);

			-- cutouts
			element (package_cursor).assembly_documentation.top.cutouts.iterate (query_cutout_top'access);
			element (package_cursor).assembly_documentation.bottom.cutouts.iterate (query_cutout_bottom'access);

			
			-- CS
			-- placeholders
			-- texts		: type_texts_with_content.list;

		end draw_assembly_documentation;

		
		-- KEEPOUT
		procedure draw_keepout is 

			-- LINES
			use type_keepout_lines;
			line : type_keepout_line;

			procedure draw_line (f : in type_face) is begin
				if keepout_enabled (f) then
				
					if f = face then
						if flipped then mirror (line, Y); end if;
						
						rotate_by (line, rot (position));
						move_by (line, type_point (position));

						set_color_keepout (context.cr, f);
						pac_draw_package.draw_line (in_area, context, line, self.frame_height);
					end if;

				end if;
			end draw_line;
			
			procedure query_line_top (c : in type_keepout_lines.cursor) is begin
				line := element (c);
				set_destination;
				draw_line (destination);
			end query_line_top;

			procedure query_line_bottom (c : in type_keepout_lines.cursor) is begin
				line := element (c);
				set_destination (INVERSE);
				draw_line (destination);
			end query_line_bottom;

			
			-- ARCS
			use type_keepout_arcs;
			arc : type_keepout_arc;

			procedure draw_arc (f : in type_face) is begin
				if keepout_enabled (f) then
					
					if f = face then
						if flipped then mirror (arc, Y); end if;
						
						rotate_by (arc, rot (position));
						move_by (arc, type_point (position));

						set_color_keepout (context.cr, f);
						pac_draw_package.draw_arc (in_area, context, arc, self.frame_height);
					end if;
					
				end if;
			end draw_arc;
			
			procedure query_arc_top (c : in type_keepout_arcs.cursor) is begin
				arc := element (c);
				set_destination;
				draw_arc (destination);
			end query_arc_top;

			procedure query_arc_bottom (c : in type_keepout_arcs.cursor) is begin
				arc := element (c);
				set_destination (INVERSE);
				draw_arc (destination);
			end query_arc_bottom;

			
			-- CIRCLES
			use type_keepout_circles;

			procedure draw_circle (
				circle	: in out type_fillable_circle_solid;
				f 		: in type_face) 
			is begin
				if keepout_enabled (f) then
					
					if f = face then
						if flipped then mirror (circle, Y); end if;
						
						rotate_by (circle, rot (position));
						move_by (circle, type_point (position));

						set_color_keepout (context.cr, f);

						pac_draw_package.draw_circle (in_area, context, circle, circle.filled, self.frame_height);
						
					end if;

				end if;
			end draw_circle;
			
			procedure query_circle_top (c : in type_keepout_circles.cursor) is 
				circle : type_fillable_circle_solid := element (c);
			begin
				set_destination;
				draw_circle (circle, destination);
			end query_circle_top;

			procedure query_circle_bottom (c : in type_keepout_circles.cursor) is 
				circle : type_fillable_circle_solid := element (c);
			begin
				set_destination (INVERSE);
				draw_circle (circle, destination);
			end query_circle_bottom;

			
			-- POLYGONS
			use type_keepout_polygons;

			procedure draw_polygon (
				polygon	: in out pac_shapes.type_polygon;
				f		: in type_face)
			is begin
				if keepout_enabled (f) then
					
					if f = face then
						if flipped then mirror (polygon, Y); end if;
						
						rotate_by (polygon, rot (position));
						move_by (polygon, type_point (position));

						set_color_keepout (context.cr, f);

						pac_draw_package.draw_polygon (in_area, context, polygon, YES, self.frame_height);
						
					end if;

				end if;
			end draw_polygon;
			
			procedure query_polygon_top (c : in type_keepout_polygons.cursor) is
				polygon : pac_shapes.type_polygon := element (c);
			begin
				set_destination;
				draw_polygon (polygon, destination);
			end query_polygon_top;

			procedure query_polygon_bottom (c : in type_keepout_polygons.cursor) is
				polygon : pac_shapes.type_polygon := element (c);
			begin
				set_destination (INVERSE);
				draw_polygon (polygon, destination);
			end query_polygon_bottom;


			-- CUTOUTS
			use pac_keepout_cutouts;

			procedure draw_cutout (
				cutout	: in out type_cutout_zone;
				f		: in type_face)
			is begin
				if keepout_enabled (f) then
					
					if f = face then
						if flipped then mirror (cutout, Y); end if;
						
						rotate_by (cutout, rot (position));
						move_by (cutout, type_point (position));

						set_color_background (context.cr);

						pac_draw_package.draw_polygon (in_area, context, cutout, YES, self.frame_height);
						
					end if;

				end if;
				
			end draw_cutout;
			
			procedure query_cutout_top (c : in pac_keepout_cutouts.cursor) is
				cutout : et_packages.type_cutout_zone := element (c);
			begin
				set_destination;
				draw_cutout (cutout, destination);
			end query_cutout_top;

			procedure query_cutout_bottom (c : in pac_keepout_cutouts.cursor) is
				cutout : et_packages.type_cutout_zone := element (c);
			begin
				set_destination (INVERSE);
				draw_cutout (cutout, destination);
			end query_cutout_bottom;
			
		begin -- draw_keepout
			set_line_width (context.cr, type_view_coordinate (keepout_line_width));
			
			-- lines
			element (package_cursor).keepout.top.lines.iterate (query_line_top'access);
			element (package_cursor).keepout.bottom.lines.iterate (query_line_bottom'access);

			-- arcs
			element (package_cursor).keepout.top.arcs.iterate (query_arc_top'access);
			element (package_cursor).keepout.bottom.arcs.iterate (query_arc_bottom'access);

			-- circles
			element (package_cursor).keepout.top.circles.iterate (query_circle_top'access);
			element (package_cursor).keepout.bottom.circles.iterate (query_circle_bottom'access);

			-- polygons
			element (package_cursor).keepout.top.polygons.iterate (query_polygon_top'access);
			element (package_cursor).keepout.bottom.polygons.iterate (query_polygon_bottom'access);

			-- cutouts
			element (package_cursor).keepout.top.cutouts.iterate (query_cutout_top'access);
			element (package_cursor).keepout.bottom.cutouts.iterate (query_cutout_bottom'access);
			
		end draw_keepout;

		
		-- STOP MASK
		procedure draw_stop_mask is 

			-- LINES
			use type_stop_lines;
			line : type_stop_line;

			procedure draw_line (f : in type_face) is begin
				if stop_mask_enabled (f) then
				
					if f = face then
						if flipped then mirror (line, Y); end if;
						
						rotate_by (line, rot (position));
						move_by (line, type_point (position));

						set_color_stop_mask (context.cr, f);
						set_line_width (context.cr, type_view_coordinate (line.width));
						pac_draw_package.draw_line (in_area, context, line, self.frame_height);
					end if;

				end if;
			end draw_line;
			
			procedure query_line_top (c : in type_stop_lines.cursor) is begin
				line := element (c);
				set_destination;
				draw_line (destination);
			end query_line_top;

			procedure query_line_bottom (c : in type_stop_lines.cursor) is begin
				line := element (c);
				set_destination (INVERSE);
				draw_line (destination);
			end query_line_bottom;

			
			-- ARCS
			use type_stop_arcs;
			arc : type_stop_arc;

			procedure draw_arc (f : in type_face) is begin
				if stop_mask_enabled (f) then
					
					if f = face then
						if flipped then mirror (arc, Y); end if;
						
						rotate_by (arc, rot (position));
						move_by (arc, type_point (position));

						set_color_stop_mask (context.cr, f);
						set_line_width (context.cr, type_view_coordinate (line.width));
						pac_draw_package.draw_arc (in_area, context, arc, self.frame_height);
					end if;
					
				end if;
			end draw_arc;
			
			procedure query_arc_top (c : in type_stop_arcs.cursor) is begin
				arc := element (c);
				set_destination;
				draw_arc (destination);
			end query_arc_top;

			procedure query_arc_bottom (c : in type_stop_arcs.cursor) is begin
				arc := element (c);
				set_destination (INVERSE);
				draw_arc (destination);
			end query_arc_bottom;

			
			-- CIRCLES
			use type_stop_circles;

			procedure draw_circle (
				circle	: in out type_fillable_circle;
				f 		: in type_face) 
			is begin
				if stop_mask_enabled (f) then
					
					if f = face then
						if flipped then mirror (circle, Y); end if;
						
						rotate_by (circle, rot (position));
						move_by (circle, type_point (position));

						set_color_stop_mask (context.cr, f);

						case circle.filled is
							when NO =>
								set_line_width (context.cr, type_view_coordinate (circle.border_width));
								pac_draw_package.draw_circle (in_area, context, circle, circle.filled, self.frame_height);

							when YES =>
								case circle.fill_style is
									when SOLID =>
										pac_draw_package.draw_circle (in_area, context, circle, circle.filled, self.frame_height);

									when HATCHED => null; -- CS
								end case;
						end case;
						
					end if;

				end if;
			end draw_circle;
			
			procedure query_circle_top (c : in type_stop_circles.cursor) is 
				circle : type_fillable_circle := element (c);
			begin
				set_destination;
				draw_circle (circle, destination);
			end query_circle_top;

			procedure query_circle_bottom (c : in type_stop_circles.cursor) is 
				circle : type_fillable_circle := element (c);
			begin
				set_destination (INVERSE);
				draw_circle (circle, destination);
			end query_circle_bottom;

			
			-- POLYGONS
			use type_stop_polygons;

			procedure draw_polygon (
				polygon	: in out et_packages.type_polygon;
				f		: in type_face)
			is begin
				if stop_mask_enabled (f) then
					
					if f = face then
						if flipped then mirror (polygon, Y); end if;
						
						rotate_by (polygon, rot (position));
						move_by (polygon, type_point (position));

						set_color_stop_mask (context.cr, f);

						case polygon.fill_style is
							when SOLID =>
								pac_draw_package.draw_polygon (in_area, context, polygon, YES, self.frame_height);

							when HATCHED =>
								cairo.set_line_width (context.cr, type_view_coordinate (polygon.hatching.border_width));
								pac_draw_package.draw_polygon (in_area, context, polygon, NO, self.frame_height);
								-- CS hatching ?
						end case;
						
					end if;

				end if;
			end draw_polygon;
			
			procedure query_polygon_top (c : in type_stop_polygons.cursor) is
				polygon : et_packages.type_polygon := element (c);
			begin
				set_destination;
				draw_polygon (polygon, destination);
			end query_polygon_top;

			procedure query_polygon_bottom (c : in type_stop_polygons.cursor) is
				polygon : et_packages.type_polygon := element (c);
			begin
				set_destination (INVERSE);
				draw_polygon (polygon, destination);
			end query_polygon_bottom;


			-- CUTOUTS
			use pac_stop_cutouts;

			procedure draw_cutout (
				cutout	: in out type_cutout_zone;
				f		: in type_face)
			is begin
				if stop_mask_enabled (f) then
					
					if f = face then
						if flipped then mirror (cutout, Y); end if;
						
						rotate_by (cutout, rot (position));
						move_by (cutout, type_point (position));

						set_color_background (context.cr);

						pac_draw_package.draw_polygon (in_area, context, cutout, YES, self.frame_height);
						
					end if;

				end if;
				
			end draw_cutout;
			
			procedure query_cutout_top (c : in pac_stop_cutouts.cursor) is
				cutout : et_packages.type_cutout_zone := element (c);
			begin
				set_destination;
				draw_cutout (cutout, destination);
			end query_cutout_top;

			procedure query_cutout_bottom (c : in pac_stop_cutouts.cursor) is
				cutout : et_packages.type_cutout_zone := element (c);
			begin
				set_destination (INVERSE);
				draw_cutout (cutout, destination);
			end query_cutout_bottom;
			
		begin -- draw_stop_mask
		
			-- lines
			element (package_cursor).stop_mask.top.lines.iterate (query_line_top'access);
			element (package_cursor).stop_mask.bottom.lines.iterate (query_line_bottom'access);

			-- arcs
			element (package_cursor).stop_mask.top.arcs.iterate (query_arc_top'access);
			element (package_cursor).stop_mask.bottom.arcs.iterate (query_arc_bottom'access);

			-- circles
			element (package_cursor).stop_mask.top.circles.iterate (query_circle_top'access);
			element (package_cursor).stop_mask.bottom.circles.iterate (query_circle_bottom'access);

			-- polygons
			element (package_cursor).stop_mask.top.polygons.iterate (query_polygon_top'access);
			element (package_cursor).stop_mask.bottom.polygons.iterate (query_polygon_bottom'access);

			-- cutouts
			element (package_cursor).stop_mask.top.cutouts.iterate (query_cutout_top'access);
			element (package_cursor).stop_mask.bottom.cutouts.iterate (query_cutout_bottom'access);

			-- CS texts		: type_texts_with_content.list; -- for texts in copper to be exposed
		end draw_stop_mask;

		
		-- STENCIL / SOLDER CREAM MASK
		procedure draw_stencil is 

			-- LINES
			use type_stencil_lines;
			line : type_stencil_line;

			procedure draw_line (f : in type_face) is begin
				if stencil_enabled (f) then
				
					if f = face then
						if flipped then mirror (line, Y); end if;
						
						rotate_by (line, rot (position));
						move_by (line, type_point (position));

						set_color_stencil (context.cr, f);
						set_line_width (context.cr, type_view_coordinate (line.width));
						pac_draw_package.draw_line (in_area, context, line, self.frame_height);
					end if;

				end if;
			end draw_line;
			
			procedure query_line_top (c : in type_stencil_lines.cursor) is begin
				line := element (c);
				set_destination;
				draw_line (destination);
			end query_line_top;

			procedure query_line_bottom (c : in type_stencil_lines.cursor) is begin
				line := element (c);
				set_destination (INVERSE);
				draw_line (destination);
			end query_line_bottom;

			
			-- ARCS
			use type_stencil_arcs;
			arc : type_stencil_arc;

			procedure draw_arc (f : in type_face) is begin
				if stencil_enabled (f) then
					
					if f = face then
						if flipped then mirror (arc, Y); end if;
						
						rotate_by (arc, rot (position));
						move_by (arc, type_point (position));

						set_color_stencil (context.cr, f);
						set_line_width (context.cr, type_view_coordinate (arc.width));
						pac_draw_package.draw_arc (in_area, context, arc, self.frame_height);
					end if;
					
				end if;
			end draw_arc;
			
			procedure query_arc_top (c : in type_stencil_arcs.cursor) is begin
				arc := element (c);
				set_destination;
				draw_arc (destination);
			end query_arc_top;

			procedure query_arc_bottom (c : in type_stencil_arcs.cursor) is begin
				arc := element (c);
				set_destination (INVERSE);
				draw_arc (destination);
			end query_arc_bottom;

			
			-- CIRCLES
			use type_stencil_circles;

			procedure draw_circle (
				circle	: in out type_fillable_circle;
				f 		: in type_face) 
			is begin
				if stencil_enabled (f) then
					
					if f = face then
						if flipped then mirror (circle, Y); end if;
						
						rotate_by (circle, rot (position));
						move_by (circle, type_point (position));

						set_color_stencil (context.cr, f);

						case circle.filled is
							when NO =>
								set_line_width (context.cr, type_view_coordinate (circle.border_width));
								pac_draw_package.draw_circle (in_area, context, circle, circle.filled, self.frame_height);

							when YES =>
								case circle.fill_style is
									when SOLID =>
										pac_draw_package.draw_circle (in_area, context, circle, circle.filled, self.frame_height);

									when HATCHED => null; -- CS
								end case;
						end case;

					end if;

				end if;
			end draw_circle;
			
			procedure query_circle_top (c : in type_stencil_circles.cursor) is 
				circle : type_fillable_circle := element (c);
			begin
				set_destination;
				draw_circle (circle, destination);
			end query_circle_top;

			procedure query_circle_bottom (c : in type_stencil_circles.cursor) is 
				circle : type_fillable_circle := element (c);
			begin
				set_destination (INVERSE);
				draw_circle (circle, destination);
			end query_circle_bottom;

			
			-- POLYGONS
			use type_stencil_polygons;

			procedure draw_polygon (
				polygon	: in out et_packages.type_polygon;
				f		: in type_face)
			is begin
				if stencil_enabled (f) then
					
					if f = face then
						if flipped then mirror (polygon, Y); end if;
						
						rotate_by (polygon, rot (position));
						move_by (polygon, type_point (position));

						set_color_stencil (context.cr, f);

						case polygon.fill_style is
							when SOLID =>
								pac_draw_package.draw_polygon (in_area, context, polygon, YES, self.frame_height);

							when HATCHED =>
								cairo.set_line_width (context.cr, type_view_coordinate (polygon.hatching.border_width));
								pac_draw_package.draw_polygon (in_area, context, polygon, NO, self.frame_height);
								-- CS hatching ?
						end case;
						
					end if;

				end if;
			end draw_polygon;
			
			procedure query_polygon_top (c : in type_stencil_polygons.cursor) is
				polygon : et_packages.type_polygon := element (c);
			begin
				set_destination;
				draw_polygon (polygon, destination);
			end query_polygon_top;

			procedure query_polygon_bottom (c : in type_stencil_polygons.cursor) is
				polygon : et_packages.type_polygon := element (c);
			begin
				set_destination (INVERSE);
				draw_polygon (polygon, destination);
			end query_polygon_bottom;


			-- CUTOUTS
			use pac_stencil_cutouts;

			procedure draw_cutout (
				cutout	: in out type_cutout_zone;
				f		: in type_face)
			is begin
				if stencil_enabled (f) then
					
					if f = face then
						if flipped then mirror (cutout, Y); end if;
						
						rotate_by (cutout, rot (position));
						move_by (cutout, type_point (position));

						set_color_background (context.cr);

						pac_draw_package.draw_polygon (in_area, context, cutout, YES, self.frame_height);
						
					end if;

				end if;
				
			end draw_cutout;
			
			procedure query_cutout_top (c : in pac_stencil_cutouts.cursor) is
				cutout : et_packages.type_cutout_zone := element (c);
			begin
				set_destination;
				draw_cutout (cutout, destination);
			end query_cutout_top;

			procedure query_cutout_bottom (c : in pac_stencil_cutouts.cursor) is
				cutout : et_packages.type_cutout_zone := element (c);
			begin
				set_destination (INVERSE);
				draw_cutout (cutout, destination);
			end query_cutout_bottom;
			
		begin -- draw_stencil
		
			-- lines
			element (package_cursor).stencil.top.lines.iterate (query_line_top'access);
			element (package_cursor).stencil.bottom.lines.iterate (query_line_bottom'access);

			-- arcs
			element (package_cursor).stencil.top.arcs.iterate (query_arc_top'access);
			element (package_cursor).stencil.bottom.arcs.iterate (query_arc_bottom'access);

			-- circles
			element (package_cursor).stencil.top.circles.iterate (query_circle_top'access);
			element (package_cursor).stencil.bottom.circles.iterate (query_circle_bottom'access);

			-- polygons
			element (package_cursor).stencil.top.polygons.iterate (query_polygon_top'access);
			element (package_cursor).stencil.bottom.polygons.iterate (query_polygon_bottom'access);

			-- cutouts
			element (package_cursor).stencil.top.cutouts.iterate (query_cutout_top'access);
			element (package_cursor).stencil.bottom.cutouts.iterate (query_cutout_bottom'access);

		end draw_stencil;

		
		-- ROUTE RESTRICT
		procedure draw_route_restrict is 

			-- LINES
			use type_route_restrict_lines;
			
			procedure query_line (c : in type_route_restrict_lines.cursor) is
				line : type_route_restrict_line := element (c);
			begin
				if flipped then 
					mirror (line, Y);
					mirror (line.layers, bottom_layer);
				end if;

				if route_restrict_layer_enabled (line.layers) then
					
					rotate_by (line, rot (position));
					move_by (line, type_point (position));

					pac_draw_package.draw_line (in_area, context, line, self.frame_height);
				end if;
			end query_line;

			
			-- ARCS
			use type_route_restrict_arcs;
			
			procedure query_arc (c : in type_route_restrict_arcs.cursor) is 
				arc : type_route_restrict_arc := element (c);
			begin
				if flipped then 
					mirror (arc, Y); 
					mirror (arc.layers, bottom_layer);
				end if;

				if route_restrict_layer_enabled (arc.layers) then
					
					rotate_by (arc, rot (position));
					move_by (arc, type_point (position));

					pac_draw_package.draw_arc (in_area, context, arc, self.frame_height);
				end if;
			end query_arc;

			
			-- CIRCLES
			use type_route_restrict_circles;
			
			procedure query_circle (c : in type_route_restrict_circles.cursor) is 
				circle : type_route_restrict_circle := element (c);
			begin
				if flipped then 
					mirror (circle, Y);
					mirror (circle.layers, bottom_layer);
				end if;

				if route_restrict_layer_enabled (circle.layers) then
				
					rotate_by (circle, rot (position));
					move_by (circle, type_point (position));

					pac_draw_package.draw_circle (in_area, context, circle, circle.filled, self.frame_height);
				end if;

			end query_circle;

			
			-- POLYGONS
			use type_route_restrict_polygons;
			
			procedure query_polygon (c : in type_route_restrict_polygons.cursor) is
				polygon : et_packages.type_route_restrict_polygon := element (c);
			begin
				if flipped then 
					mirror (polygon, Y);
					mirror (polygon.layers, bottom_layer);
				end if;

				if route_restrict_layer_enabled (polygon.layers) then
					
					rotate_by (polygon, rot (position));
					move_by (polygon, type_point (position));

					pac_draw_package.draw_polygon (in_area, context, polygon, YES, self.frame_height);
				end if;

			end query_polygon;


			-- CUTOUTS
			use pac_route_restrict_cutouts;
		
			procedure query_cutout (c : in pac_route_restrict_cutouts.cursor) is
				cutout : et_packages.type_route_restrict_cutout := element (c);
			begin
				if flipped then 
					mirror (cutout, Y); 
					mirror (cutout.layers, bottom_layer);
				end if;

				if route_restrict_layer_enabled (cutout.layers) then
					
					rotate_by (cutout, rot (position));
					move_by (cutout, type_point (position));

					set_color_background (context.cr);

					pac_draw_package.draw_polygon (in_area, context, cutout, YES, self.frame_height);
				end if;

			end query_cutout;
			
		begin -- draw_route_restrict
			set_color_route_restrict (context.cr);
			set_line_width (context.cr, type_view_coordinate (route_restrict_line_width));
			
			-- lines
			element (package_cursor).route_restrict.lines.iterate (query_line'access);

			-- arcs
			element (package_cursor).route_restrict.arcs.iterate (query_arc'access);

			-- circles
			element (package_cursor).route_restrict.circles.iterate (query_circle'access);

			-- polygons
			element (package_cursor).route_restrict.polygons.iterate (query_polygon'access);

			-- cutouts
			element (package_cursor).route_restrict.cutouts.iterate (query_cutout'access);

		end draw_route_restrict;

		
		-- VIA RESTRICT
		procedure draw_via_restrict is 

			-- LINES
			use type_via_restrict_lines;
			
			procedure query_line (c : in type_via_restrict_lines.cursor) is
				line : type_via_restrict_line := element (c);
			begin
				if flipped then 
					mirror (line, Y);
					mirror (line.layers, bottom_layer);
				end if;

				if via_restrict_layer_enabled (line.layers) then
					
					rotate_by (line, rot (position));
					move_by (line, type_point (position));

					pac_draw_package.draw_line (in_area, context, line, self.frame_height);
				end if;
			end query_line;

			
			-- ARCS
			use type_via_restrict_arcs;
			
			procedure query_arc (c : in type_via_restrict_arcs.cursor) is 
				arc : type_via_restrict_arc := element (c);
			begin
				if flipped then 
					mirror (arc, Y); 
					mirror (arc.layers, bottom_layer);
				end if;

				if via_restrict_layer_enabled (arc.layers) then
					
					rotate_by (arc, rot (position));
					move_by (arc, type_point (position));

					pac_draw_package.draw_arc (in_area, context, arc, self.frame_height);
				end if;
			end query_arc;

			
			-- CIRCLES
			use type_via_restrict_circles;
			
			procedure query_circle (c : in type_via_restrict_circles.cursor) is 
				circle : type_via_restrict_circle := element (c);
			begin
				if flipped then 
					mirror (circle, Y);
					mirror (circle.layers, bottom_layer);
				end if;

				if via_restrict_layer_enabled (circle.layers) then
				
					rotate_by (circle, rot (position));
					move_by (circle, type_point (position));

					pac_draw_package.draw_circle (in_area, context, circle, circle.filled, self.frame_height);
				end if;

			end query_circle;

			
			-- POLYGONS
			use type_via_restrict_polygons;
			
			procedure query_polygon (c : in type_via_restrict_polygons.cursor) is
				polygon : et_packages.type_via_restrict_polygon := element (c);
			begin
				if flipped then 
					mirror (polygon, Y);
					mirror (polygon.layers, bottom_layer);
				end if;

				if via_restrict_layer_enabled (polygon.layers) then
					
					rotate_by (polygon, rot (position));
					move_by (polygon, type_point (position));

					pac_draw_package.draw_polygon (in_area, context, polygon, YES, self.frame_height);
				end if;

			end query_polygon;


			-- CUTOUTS
			use pac_via_restrict_cutouts;
		
			procedure query_cutout (c : in pac_via_restrict_cutouts.cursor) is
				cutout : et_packages.type_via_restrict_cutout := element (c);
			begin
				if flipped then 
					mirror (cutout, Y); 
					mirror (cutout.layers, bottom_layer);
				end if;

				if via_restrict_layer_enabled (cutout.layers) then
					
					rotate_by (cutout, rot (position));
					move_by (cutout, type_point (position));

					set_color_background (context.cr);

					pac_draw_package.draw_polygon (in_area, context, cutout, YES, self.frame_height);
				end if;

			end query_cutout;
			
		begin -- draw_via_restrict
			set_color_via_restrict (context.cr);
			set_line_width (context.cr, type_view_coordinate (via_restrict_line_width));
			
			-- lines
			element (package_cursor).via_restrict.lines.iterate (query_line'access);

			-- arcs
			element (package_cursor).via_restrict.arcs.iterate (query_arc'access);

			-- circles
			element (package_cursor).via_restrict.circles.iterate (query_circle'access);

			-- polygons
			element (package_cursor).via_restrict.polygons.iterate (query_polygon'access);

			-- cutouts
			element (package_cursor).via_restrict.cutouts.iterate (query_cutout'access);

		end draw_via_restrict;

		
		-- PCB CONTOUR / OUTLINE
		procedure draw_pcb_contour is 

			-- LINES
			use et_packages.type_pcb_contour_lines;
			line : et_packages.type_pcb_contour_line;

			procedure query_line (c : in et_packages.type_pcb_contour_lines.cursor) is
				line : type_pcb_contour_line := element (c);
			begin
				if outline_enabled then
				
					if flipped then mirror (line, Y); end if;
					
					rotate_by (line, rot (position));
					move_by (line, type_point (position));

					pac_draw_package.draw_line (in_area, context, line, self.frame_height);

				end if;		
				
			end query_line;

			
			-- ARCS
			use type_pcb_contour_arcs;
			arc : type_pcb_contour_arc;
			
			procedure query_arc (c : in type_pcb_contour_arcs.cursor) is
				arc : type_pcb_contour_arc := element (c);
			begin
				if outline_enabled then
					
					if flipped then mirror (arc, Y); end if;
					
					rotate_by (arc, rot (position));
					move_by (arc, type_point (position));

					pac_draw_package.draw_arc (in_area, context, arc, self.frame_height);
				end if;

			end query_arc;

			
			-- CIRCLES
			use type_pcb_contour_circles;
			
			procedure query_circle (c : in type_pcb_contour_circles.cursor) is 
				circle : type_pcb_contour_circle := element (c);
			begin
				if outline_enabled then
					
					if flipped then mirror (circle, Y); end if;
						
					rotate_by (circle, rot (position));
					move_by (circle, type_point (position));

					pac_draw_package.draw_circle (in_area, context, circle, NO, self.frame_height);
				end if;

			end query_circle;
			
		begin -- draw_pcb_contour
			set_color_outline (context.cr);
			set_line_width (context.cr, type_view_coordinate (pcb_contour_line_width));
			
			-- lines
			element (package_cursor).pcb_contour.lines.iterate (query_line'access);

			-- arcs
			element (package_cursor).pcb_contour.arcs.iterate (query_arc'access);

			-- circles
			element (package_cursor).pcb_contour.circles.iterate (query_circle'access);

		end draw_pcb_contour;


		-- CONDUCTORS (NON-TERMINAL RELATED, NON-ELECTRICAL !)
		procedure draw_conductors is 

			-- Translates face (TOP/BOTTOM) to conductor layer 1/bottom_layer:
			function face_to_layer (f : in type_face) return type_signal_layer is begin
				case f is
					when TOP => return type_signal_layer'first;
					when BOTTOM => return bottom_layer;
				end case;
			end face_to_layer;

			
			-- LINES
			use type_copper_lines;
			line : type_copper_line;

			procedure draw_line (f : in type_face) is
				ly : constant type_signal_layer := face_to_layer (f);
			begin
				if conductor_enabled (ly) then
				
					if f = face then
						if flipped then mirror (line, Y); end if;
						
						rotate_by (line, rot (position));
						move_by (line, type_point (position));

						set_color_conductor (context.cr, ly);
						set_line_width (context.cr, type_view_coordinate (line.width));
						pac_draw_package.draw_line (in_area, context, line, self.frame_height);
					end if;

				end if;
			end draw_line;
			
			procedure query_line_top (c : in type_copper_lines.cursor) is begin
				line := element (c);
				set_destination;
				draw_line (destination);
			end query_line_top;

			procedure query_line_bottom (c : in type_copper_lines.cursor) is begin
				line := element (c);
				set_destination (INVERSE);
				draw_line (destination);
			end query_line_bottom;

			
			-- ARCS
			use type_copper_arcs;
			arc : type_copper_arc;

			procedure draw_arc (f : in type_face) is
				ly : constant type_signal_layer := face_to_layer (f);
			begin
				if conductor_enabled (ly) then
					
					if f = face then
						if flipped then mirror (arc, Y); end if;
						
						rotate_by (arc, rot (position));
						move_by (arc, type_point (position));

						set_color_conductor (context.cr, ly);
						set_line_width (context.cr, type_view_coordinate (arc.width));
						pac_draw_package.draw_arc (in_area, context, arc, self.frame_height);
					end if;
					
				end if;
			end draw_arc;
			
			procedure query_arc_top (c : in type_copper_arcs.cursor) is begin
				arc := element (c);
				set_destination;
				draw_arc (destination);
			end query_arc_top;

			procedure query_arc_bottom (c : in type_copper_arcs.cursor) is begin
				arc := element (c);
				set_destination (INVERSE);
				draw_arc (destination);
			end query_arc_bottom;

			
			-- CIRCLES
			use pac_copper_circles;

			procedure draw_circle (
				circle	: in out type_copper_circle;
				f 		: in type_face) is
				ly : constant type_signal_layer := face_to_layer (f);
			begin
				if conductor_enabled (ly) then
					
					if f = face then
						if flipped then mirror (circle, Y); end if;
						
						rotate_by (circle, rot (position));
						move_by (circle, type_point (position));

						set_color_conductor (context.cr, ly);

						case circle.filled is
							when NO =>
								set_line_width (context.cr, type_view_coordinate (circle.border_width));
								pac_draw_package.draw_circle (in_area, context, circle, circle.filled, self.frame_height);

							when YES =>
								case circle.fill_style is
									when SOLID =>
										pac_draw_package.draw_circle (in_area, context, circle, circle.filled, self.frame_height);

									when HATCHED => null; -- CS
								end case;
						end case;

					end if;

				end if;
			end draw_circle;
			
			procedure query_circle_top (c : in pac_copper_circles.cursor) is 
				circle : type_copper_circle := element (c);
			begin
				set_destination;
				draw_circle (circle, destination);
			end query_circle_top;

			procedure query_circle_bottom (c : in pac_copper_circles.cursor) is 
				circle : type_copper_circle := element (c);
			begin
				set_destination (INVERSE);
				draw_circle (circle, destination);
			end query_circle_bottom;

			
			-- POLYGONS

			-- solid
			use pac_copper_polygons_solid;

			procedure draw_polygon_solid (
				polygon	: in out et_packages.type_copper_polygon_solid;
				f		: in type_face) is
				ly : constant type_signal_layer := face_to_layer (f);
			begin
				if conductor_enabled (ly) then
					
					if f = face then
						if flipped then mirror (polygon, Y); end if;
						
						rotate_by (polygon, rot (position));
						move_by (polygon, type_point (position));

						set_color_conductor (context.cr, ly);

						pac_draw_package.draw_polygon (in_area, context, polygon, YES, self.frame_height);
-- CS
-- 		easing : type_easing;
-- 		width_min : type_track_width; -- the minimum width
-- 		isolation : type_track_clearance := type_track_clearance'first; 
						
					end if;

				end if;
				
			end draw_polygon_solid;
			
			procedure query_polygon_top_solid (c : in pac_copper_polygons_solid.cursor) is
				polygon : et_packages.type_copper_polygon_solid := element (c);
			begin
				set_destination;
				draw_polygon_solid (polygon, destination);
			end query_polygon_top_solid;

			procedure query_polygon_bottom_solid (c : in pac_copper_polygons_solid.cursor) is
				polygon : et_packages.type_copper_polygon_solid := element (c);
			begin
				set_destination (INVERSE);
				draw_polygon_solid (polygon, destination);
			end query_polygon_bottom_solid;


			-- hatched
			use pac_copper_polygons_hatched;

			procedure draw_polygon_hatched (
				polygon	: in out et_packages.type_copper_polygon_hatched;
				f		: in type_face) is
				ly : constant type_signal_layer := face_to_layer (f);
			begin
				if conductor_enabled (ly) then
					
					if f = face then
						if flipped then mirror (polygon, Y); end if;
						
						rotate_by (polygon, rot (position));
						move_by (polygon, type_point (position));

						set_color_conductor (context.cr, ly);

-- 						pac_draw_package.draw_polygon (in_area, context, polygon, YES, self.frame_height);
-- CS
-- 		easing : type_easing;
-- 		hatching : type_hatching_copper;
-- 		width_min : type_track_width; -- the minimum width
-- 		isolation : type_track_clearance := type_track_clearance'first; 
						
					end if;

				end if;
				
			end draw_polygon_hatched;
			
			procedure query_polygon_top_hatched (c : in pac_copper_polygons_hatched.cursor) is
				polygon : et_packages.type_copper_polygon_hatched := element (c);
			begin
				set_destination;
				draw_polygon_hatched (polygon, destination);
			end query_polygon_top_hatched;

			procedure query_polygon_bottom_hatched (c : in pac_copper_polygons_hatched.cursor) is
				polygon : et_packages.type_copper_polygon_hatched := element (c);
			begin
				set_destination (INVERSE);
				draw_polygon_hatched (polygon, destination);
			end query_polygon_bottom_hatched;

			
			-- CUTOUTS
			use pac_copper_cutouts;

			procedure draw_cutout (
				cutout	: in out et_packages.type_cutout_zone;
				f		: in type_face) is
				ly : constant type_signal_layer := face_to_layer (f);
			begin
				if conductor_enabled (ly) then
					
					if f = face then
						if flipped then mirror (cutout, Y); end if;
						
						rotate_by (cutout, rot (position));
						move_by (cutout, type_point (position));

						set_color_background (context.cr);

						pac_draw_package.draw_polygon (in_area, context, cutout, YES, self.frame_height);
						
					end if;

				end if;
			end draw_cutout;
			
			procedure query_cutout_top (c : in pac_copper_cutouts.cursor) is
				cutout : type_cutout_zone := element (c);
			begin
				set_destination;
				draw_cutout (cutout, destination);
			end query_cutout_top;

			procedure query_cutout_bottom (c : in pac_copper_cutouts.cursor) is
				cutout : type_cutout_zone := element (c);
			begin
				set_destination (INVERSE);
				draw_cutout (cutout, destination);
			end query_cutout_bottom;
			
		begin -- draw_conductors
			-- lines
			element (package_cursor).copper.top.lines.iterate (query_line_top'access);
			element (package_cursor).copper.bottom.lines.iterate (query_line_bottom'access);

			-- arcs
			element (package_cursor).copper.top.arcs.iterate (query_arc_top'access);
			element (package_cursor).copper.bottom.arcs.iterate (query_arc_bottom'access);

			-- circles
			element (package_cursor).copper.top.circles.iterate (query_circle_top'access);
			element (package_cursor).copper.bottom.circles.iterate (query_circle_bottom'access);

			-- polygons solid
			element (package_cursor).copper.top.polygons.solid.iterate (query_polygon_top_solid'access);
			element (package_cursor).copper.bottom.polygons.solid.iterate (query_polygon_bottom_solid'access);

			-- polygons hatched
			element (package_cursor).copper.top.polygons.hatched.iterate (query_polygon_top_hatched'access);
			element (package_cursor).copper.bottom.polygons.hatched.iterate (query_polygon_bottom_hatched'access);

			-- cutouts
			element (package_cursor).copper.top.cutouts.iterate (query_cutout_top'access);
			element (package_cursor).copper.bottom.cutouts.iterate (query_cutout_bottom'access);

			-- CS
			-- placeholders
			-- texts		: type_texts_with_content.list;

		end draw_conductors;


		-- TERMINALS
		procedure draw_terminals is
			use type_terminals;

			procedure query_terminal (c : in type_terminals.cursor) is
				t : type_terminal := element (c);
			begin
-- 				-- name key (c) -- H5, 5, 3
-- 				pac_draw_package.draw_text (
-- 					area		=> in_area,
-- 					context		=> context,
-- 					content		=> to_content (to_string (key (c))),
-- 					size		=> terminal_name_size,
-- 					font		=> terminal_name_font,
-- 					position	=> 
-- 					origin		=> false, -- no origin required
-- 					rotation	=> zero_rotation,
-- 					alignment	=> (others => <>),
-- 					height		=> self.frame_height);
-- 				
				null;

				case t.technology is
					when THT => null;

					when SMT => null;
				end case;
						
			end query_terminal;
			
		begin -- draw_terminals
			element (package_cursor).terminals.iterate (query_terminal'access);
		end draw_terminals;
		
		procedure draw_origin is
			type type_line is new et_packages.pac_shapes.type_line with null record;
			
			line_horizontal : constant type_line := ( -- from left to right
				start_point		=> type_point (set (x => x (position) - origin_half_size, y => y (position))),
				end_point		=> type_point (set (x => x (position) + origin_half_size, y => y (position))));

			line_vertical : constant type_line := ( -- from bottom to top
				start_point		=> type_point (set (x => x (position), y => y (position) - origin_half_size)),
				end_point		=> type_point (set (x => x (position), y => y (position) + origin_half_size)));

		begin -- draw_origin
			if face = get_face (position) then
				if device_origins_enabled (get_face (position)) then

					set_color_origin (context.cr);
					set_line_width (context.cr, type_view_coordinate (origin_line_width));
					pac_draw_package.draw_line (in_area, context, line_horizontal, self.frame_height);
					pac_draw_package.draw_line (in_area, context, line_vertical, self.frame_height);

				end if;
			end if;
		end draw_origin;
		
	begin -- draw_package
		draw_silkscreen;
		draw_assembly_documentation;
		draw_terminals; -- pins, pads, plated millings
		draw_conductors; -- NON-TERMINAL RELATED, NON-ELECTRICAL
		draw_keepout; 
		draw_stop_mask; -- non-terminal related
		draw_stencil; -- non-terminal related
		draw_route_restrict;
		draw_via_restrict;
		draw_pcb_contour;
		
		-- The origin is drawn last so that it obscures other elements of the package:
		draw_origin;
	end draw_package;

	use et_schematic;
	
	procedure query_devices (
		module_name	: in type_module_name.bounded_string;
		module		: in type_module) is
		use et_schematic.type_devices;

		procedure query_device (d : in et_schematic.type_devices.cursor) is
			use et_symbols;
			use et_pcb;
		begin
			if element (d).appearance = PCB then
				
				draw_package (
					model			=> package_model (d), -- libraries/packages/smd/SOT23.pac
					position		=> element (d).position, -- x/y/rotation
					flip			=> element (d).flipped,
					placeholders	=> element (d).text_placeholders);
				
			end if;
		end query_device;
		
	begin -- query_devices
		module.devices.iterate (query_device'access);
	end query_devices;
	
begin -- draw_packages
-- 	put_line ("draw packages ...");
	
	type_modules.query_element (
		position	=> et_canvas_schematic.current_active_module,
		process		=> query_devices'access);

	
	-- CS non-electrical packages ? like fiducials
end draw_packages;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
