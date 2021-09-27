------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW PACKAGES                             --
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

with et_symbols;
with et_devices;

with et_drills;					use et_drills;
with et_packages;				use et_packages;

with et_pcb;
with et_pcb_stack;				use et_pcb_stack;

with et_display.board;			use et_display.board;
with et_colors;					use et_colors;
with et_design_rules;			use et_design_rules;
with et_text;
with et_conductor_text;			use et_conductor_text;

with et_conductor_polygons;		use et_conductor_polygons;
with et_conductor_polygons.packages;	use et_conductor_polygons.packages;

with et_route_restrict;			use et_route_restrict;
with et_via_restrict;			use et_via_restrict;
with et_stop_mask;				use et_stop_mask;
with et_stencil;				use et_stencil;
with et_silkscreen;				use et_silkscreen;
with et_assy_doc;				use et_assy_doc;
with et_keepout;				use et_keepout;
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
		device_name		: in et_devices.type_device_name; -- IC13, C4
		device_value	: in et_devices.pac_device_value.bounded_string; -- SN7400
		device_purpose	: in et_devices.pac_device_purpose.bounded_string; -- brightness control
		model			: in et_packages.pac_package_model_file_name.bounded_string;
		package_position: in et_pcb_coordinates.type_package_position; -- incl. rotation and face
		flip			: in et_pcb.type_flipped;
		placeholders	: in et_packages.type_text_placeholders) -- specified in the board. will override default positions
	is
		-- CS should improve performance:
		-- package_offset : constant type_distance_relative := to_distance_relative (package_position)
		-- use package_offset instead of many calls of to_distance_relative (package_position)
		
		use et_board_shapes_and_text;
		use pac_text_fab;
		use pac_shapes;	

		use pac_packages_lib;

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
		package_cursor : constant et_packages.pac_packages_lib.cursor := locate_package_model (model);

		-- Maps from meaning of given placeholder to text content:
		function to_placeholder_content (ph : in type_text_placeholder)
			return et_text.pac_text_content.bounded_string 
		is
			use et_devices;
			use et_text;
			result : pac_text_content.bounded_string;
		begin
			case ph.meaning is
				when NAME => result := to_content (to_string (device_name));
				when VALUE => result := to_content (to_string (device_value));
				when PURPOSE => result := to_content (to_string (device_purpose));
			end case;
			
			return result;
		end to_placeholder_content;

		
		procedure draw_text_origin (p : in type_point; f : in type_face) is
			line_horizontal : constant type_line := ( -- from left to right
				start_point		=> type_point (set (x => get_x (p) - pac_text_fab.origin_half_size, y => get_y (p))),
				end_point		=> type_point (set (x => get_x (p) + pac_text_fab.origin_half_size, y => get_y (p))));

			line_vertical : constant type_line := ( -- from bottom to top
				start_point		=> type_point (set (x => get_x (p), y => get_y (p) - pac_text_fab.origin_half_size)),
				end_point		=> type_point (set (x => get_x (p), y => get_y (p) + pac_text_fab.origin_half_size)));

		begin -- draw_text_origin
			if device_origins_enabled (f) then

				-- The caller of this procedure has a setting for the color.
				-- So we backup this context setting.
				save (context.cr);
				
				set_color_origin (context.cr);
				set_line_width (context.cr, type_view_coordinate (pac_text_fab.origin_line_width));
				pac_draw_fab.draw_line (in_area, context, line_horizontal, pac_text_fab.origin_line_width, self.frame_height);
				pac_draw_fab.draw_line (in_area, context, line_vertical, pac_text_fab.origin_line_width, self.frame_height);

				-- Restore context setting of caller. See comment above.
				restore (context.cr);
			end if;
		end draw_text_origin;

		
		-- Maps from flip status to mirror status of a vector text:
		function to_mirror (f : in et_pcb.type_flipped) return et_text.type_vector_text_mirrored is
			use et_pcb;
			use et_text;
		begin
			case f is
				when YES => return YES;
				when NO => return NO;
			end case;
		end to_mirror;

		
		procedure draw_text_with_content (
			t : in out type_text_fab_with_content;
			f : in type_face)
		is
			use pac_vector_text_lines;
			vector_text : pac_text_fab.pac_vector_text_lines.list;
		begin

			-- Rotate the position of the text by the rotation of the package.
			-- NOTE: This does not affect the rotation of the text itself.
			rotate_by (t.position, rot (package_position));
			
			if flipped then mirror (t.position, Y); end if;

			-- Move the text by the package position to 
			-- its final position:
			move_by (t.position, to_distance_relative (package_position));

			draw_text_origin (type_point (t.position), f);

			-- Set the line width of the vector text:
			set_line_width (context.cr, type_view_coordinate (t.line_width));

			-- Vectorize the content of the text:
			vector_text := pac_text_fab.vectorize_text (
				content		=> t.content,
				size		=> t.size,
				rotation	=> add (rot (t.position), rot (package_position)),
				position	=> type_point (t.position),
				mirror		=> to_mirror (flip), -- mirror vector text if package is flipped
				line_width	=> t.line_width,
				alignment	=> t.alignment -- right, bottom
				);

			-- Draw the content of the placeholder:
			pac_draw_fab.draw_vector_text (in_area, context, vector_text, t.line_width, self.frame_height);
			
		end draw_text_with_content;

		
		-- SILKSCREEN
		procedure draw_silkscreen is 

			-- LINES
			use pac_silk_lines;
			line : type_silk_line;

			procedure draw_line (f : in type_face) is begin
				if silkscreen_enabled (f) then
				
					if f = face then
						rotate_by (line, rot (package_position));
						
						if flipped then mirror (line, Y); end if;
						
						move_by (line, to_distance_relative (package_position));

						set_color_silkscreen (context.cr, f);
						set_line_width (context.cr, type_view_coordinate (line.width));
						pac_draw_fab.draw_line (in_area, context, line, line.width, self.frame_height);
					end if;

				end if;
			end draw_line;
			
			procedure query_line_top (c : in pac_silk_lines.cursor) is begin
				line := element (c);
				set_destination;
				draw_line (destination);
			end query_line_top;

			procedure query_line_bottom (c : in pac_silk_lines.cursor) is begin
				line := element (c);
				set_destination (INVERSE);
				draw_line (destination);
			end query_line_bottom;

			
			-- ARCS
			use pac_silk_arcs;
			arc : type_silk_arc;

			procedure draw_arc (f : in type_face) is begin
				if silkscreen_enabled (f) then
					
					if f = face then
						rotate_by (arc, rot (package_position));
						
						if flipped then mirror (arc, Y); end if;
						
						move_by (arc, to_distance_relative (package_position));

						set_color_silkscreen (context.cr, f);
						set_line_width (context.cr, type_view_coordinate (arc.width));
						pac_draw_fab.draw_arc (in_area, context, arc, arc.width, self.frame_height);
					end if;
					
				end if;
			end draw_arc;
			
			procedure query_arc_top (c : in pac_silk_arcs.cursor) is begin
				arc := element (c);
				set_destination;
				draw_arc (destination);
			end query_arc_top;

			procedure query_arc_bottom (c : in pac_silk_arcs.cursor) is begin
				arc := element (c);
				set_destination (INVERSE);
				draw_arc (destination);
			end query_arc_bottom;

			
			-- CIRCLES
			use pac_silk_circles;

			procedure draw_circle (
				circle	: in out type_fillable_circle;
				f 		: in type_face) 
			is begin
				if silkscreen_enabled (f) then
					
					if f = face then
						rotate_by (circle, rot (package_position));
						
						if flipped then mirror (circle, Y); end if;
						
						move_by (circle, to_distance_relative (package_position));

						set_color_silkscreen (context.cr, f);

						case circle.filled is
							when NO =>
								set_line_width (context.cr, type_view_coordinate (circle.border_width));
								pac_draw_fab.draw_circle (in_area, context, circle,
										circle.filled, circle.border_width, self.frame_height);

							when YES =>
								case circle.fill_style is
									when SOLID =>
										pac_draw_fab.draw_circle (in_area, context, circle,
											circle.filled, zero, self.frame_height);

									when HATCHED => null; -- CS
								end case;
						end case;

					end if;

				end if;
			end draw_circle;

			
			procedure query_circle_top (c : in pac_silk_circles.cursor) is 
				circle : type_fillable_circle := element (c);
			begin
				set_destination;
				draw_circle (circle, destination);
			end query_circle_top;

			procedure query_circle_bottom (c : in pac_silk_circles.cursor) is 
				circle : type_fillable_circle := element (c);
			begin
				set_destination (INVERSE);
				draw_circle (circle, destination);
			end query_circle_bottom;

			
			-- POLYGONS
			use pac_silk_polygons;

			procedure draw_polygon (
				polygon	: in out type_polygon_non_conductor;
				f		: in type_face)
			is begin
				if silkscreen_enabled (f) then
					
					if f = face then
						rotate_by (polygon, rot (package_position));
						
						if flipped then mirror (polygon, Y); end if;
						
						move_by (polygon, to_distance_relative (package_position));

						set_color_silkscreen (context.cr, f);

						case polygon.fill_style is
							when SOLID =>
								pac_draw_fab.draw_polygon (in_area, context, polygon, YES,
									zero, self.frame_height);

							when HATCHED =>
								set_line_width (context.cr,
									type_view_coordinate (polygon.hatching.border_width));
								
								pac_draw_fab.draw_polygon (in_area, context, polygon, NO,
									polygon.hatching.border_width, self.frame_height);
								-- CS hatching ?
						end case;
						
					end if;

				end if;
				
			end draw_polygon;

			
			procedure query_polygon_top (c : in pac_silk_polygons.cursor) is
				polygon : type_polygon_non_conductor := element (c);
			begin
				set_destination;
				draw_polygon (polygon, destination);
			end query_polygon_top;

			procedure query_polygon_bottom (c : in pac_silk_polygons.cursor) is
				polygon : type_polygon_non_conductor := element (c);
			begin
				set_destination (INVERSE);
				draw_polygon (polygon, destination);
			end query_polygon_bottom;


			-- CUTOUTS
			use pac_silk_cutouts;

			procedure draw_cutout (
				cutout	: in out type_polygon;
				f		: in type_face)
			is begin
				if silkscreen_enabled (f) then
					
					if f = face then
						rotate_by (cutout, rot (package_position));
						
						if flipped then mirror (cutout, Y); end if;
						
						move_by (cutout, to_distance_relative (package_position));

						set_color_background (context.cr);

						pac_draw_fab.draw_polygon (in_area, context, cutout, YES,
							zero, self.frame_height);
					end if;

				end if;
				
			end draw_cutout;

			
			procedure query_cutout_top (c : in pac_silk_cutouts.cursor) is
				cutout : type_polygon := element (c);
			begin
				set_destination;
				draw_cutout (cutout, destination);
			end query_cutout_top;

			procedure query_cutout_bottom (c : in pac_silk_cutouts.cursor) is
				cutout : type_polygon := element (c);
			begin
				set_destination (INVERSE);
				draw_cutout (cutout, destination);
			end query_cutout_bottom;


			-- PLACEHOLDERS
			use pac_text_placeholders;

			procedure draw_placeholder (
				ph	: in out type_text_placeholder;
				f	: in type_face)
			is
				use pac_text_fab.pac_vector_text_lines;
				vector_text : pac_text_fab.pac_vector_text_lines.list;
			begin
				if silkscreen_enabled (f) then
					
					if f = face then

						-- Rotate the position of the placeholder by the rotation of the package.
						-- NOTE: This does not affect the rotation of the placeholder text but only
						-- the rotation about the origin of the package.
						-- If the package has been flipped, then the rotation is counterclockwise.
						if flipped then
							rotate_by (ph.position, - rot (package_position));
						else
							rotate_by (ph.position, rot (package_position));
						end if;

						-- Move the placeholder by the package position to 
						-- its final position:
						move_by (ph.position, to_distance_relative (package_position));

						set_color_silkscreen (context.cr, f);

						draw_text_origin (type_point (ph.position), f);

						-- Set the line width of the vector text:
						set_line_width (context.cr, type_view_coordinate (ph.line_width));

						-- Vectorize the content of the placeholder:
						vector_text := pac_text_fab.vectorize_text (
							content		=> to_placeholder_content (ph), -- map from meaning to content
							size		=> ph.size,
							rotation	=> add (rot (ph.position), rot (package_position)),
							position	=> type_point (ph.position),
							mirror		=> to_mirror (flip), -- mirror vector text if package is flipped
							line_width	=> ph.line_width,
							alignment	=> ph.alignment -- right, bottom
							);

						-- Draw the content of the placeholder:
						pac_draw_fab.draw_vector_text (in_area, context, vector_text,
							ph.line_width, self.frame_height);
						
					end if;

				end if;
			end draw_placeholder;

			
			procedure query_placeholder_top (c : in pac_text_placeholders.cursor) is
				ph : type_text_placeholder := element (c);
			begin
				-- Draw the placeholder only if it has content:
				if not et_text.is_empty (to_placeholder_content (ph)) then
					set_destination;
					draw_placeholder (ph, destination);
				end if;
			end query_placeholder_top;

			procedure query_placeholder_bottom (c : in pac_text_placeholders.cursor) is
				ph : type_text_placeholder := element (c);
			begin
				-- Draw the placeholder only if it has content:
				if not et_text.is_empty (to_placeholder_content (ph)) then
					set_destination (INVERSE);
					draw_placeholder (ph, destination);
				end if;
			end query_placeholder_bottom;

			
			-- TEXTS
			use pac_texts_fab_with_content;
			
			procedure draw_text (
				t	: in out type_text_fab_with_content;
				f	: in type_face) is
			begin
				if silkscreen_enabled (f) then
	
					if f = face then
						set_color_silkscreen (context.cr, f);
						draw_text_with_content (t, f);
					end if;

				end if;
			end draw_text;

			procedure query_text_top (c : in pac_texts_fab_with_content.cursor) is
				t : type_text_fab_with_content := element (c);
			begin
				-- Draw the text only if it has content:
				if not et_text.is_empty (t.content) then
					set_destination;
					draw_text (t, destination);
				end if;
			end query_text_top;

			procedure query_text_bottom (c : in pac_texts_fab_with_content.cursor) is
				t : type_text_fab_with_content := element (c);
			begin
				-- Draw the text only if it has content:
				if not et_text.is_empty (t.content) then
					set_destination (INVERSE);
					draw_text (t, destination);
				end if;
			end query_text_bottom;

			
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
			
			-- placeholders
			placeholders.silk_screen.top.iterate (query_placeholder_top'access);
			placeholders.silk_screen.bottom.iterate (query_placeholder_bottom'access);

			-- texts
			element (package_cursor).silk_screen.top.texts.iterate (query_text_top'access);
			element (package_cursor).silk_screen.bottom.texts.iterate (query_text_bottom'access);
			
		end draw_silkscreen;

		
		-- ASSY DOC
		procedure draw_assembly_documentation is 

			-- LINES
			use pac_doc_lines;
			line : type_doc_line;

			procedure draw_line (f : in type_face) is begin
				if assy_doc_enabled (f) then
				
					if f = face then
						rotate_by (line, rot (package_position));
						
						if flipped then mirror (line, Y); end if;
						
						move_by (line, to_distance_relative (package_position));

						set_color_assy_doc (context.cr, f);
						set_line_width (context.cr, type_view_coordinate (line.width));
						pac_draw_fab.draw_line (in_area, context, line, line.width, self.frame_height);
					end if;

				end if;
			end draw_line;
			
			procedure query_line_top (c : in pac_doc_lines.cursor) is begin
				line := element (c);
				set_destination;
				draw_line (destination);
			end query_line_top;

			procedure query_line_bottom (c : in pac_doc_lines.cursor) is begin
				line := element (c);
				set_destination (INVERSE);
				draw_line (destination);
			end query_line_bottom;

			
			-- ARCS
			use pac_doc_arcs;
			arc : type_doc_arc;

			procedure draw_arc (f : in type_face) is begin
				if assy_doc_enabled (f) then
					
					if f = face then
						rotate_by (arc, rot (package_position));
						
						if flipped then mirror (arc, Y); end if;
						
						move_by (arc, to_distance_relative (package_position));

						set_color_assy_doc (context.cr, f);
						set_line_width (context.cr, type_view_coordinate (arc.width));
						pac_draw_fab.draw_arc (in_area, context, arc, arc.width, self.frame_height);
					end if;
					
				end if;
			end draw_arc;
			
			procedure query_arc_top (c : in pac_doc_arcs.cursor) is begin
				arc := element (c);
				set_destination;
				draw_arc (destination);
			end query_arc_top;

			procedure query_arc_bottom (c : in pac_doc_arcs.cursor) is begin
				arc := element (c);
				set_destination (INVERSE);
				draw_arc (destination);
			end query_arc_bottom;

			
			-- CIRCLES
			use pac_doc_circles;

			procedure draw_circle (
				circle	: in out type_fillable_circle;
				f 		: in type_face) 
			is begin
				if assy_doc_enabled (f) then
					
					if f = face then
						rotate_by (circle, rot (package_position));
						
						if flipped then mirror (circle, Y); end if;
						
						move_by (circle, to_distance_relative (package_position));

						set_color_assy_doc (context.cr, f);

						case circle.filled is
							when NO =>
								set_line_width (context.cr, type_view_coordinate (circle.border_width));
								pac_draw_fab.draw_circle (in_area, context, circle, circle.filled, 
									circle.border_width, self.frame_height);

							when YES =>
								case circle.fill_style is
									when SOLID =>
										pac_draw_fab.draw_circle (in_area, context, circle, circle.filled,
											zero, self.frame_height);

									when HATCHED => null; -- CS
								end case;
						end case;

					end if;

				end if;
			end draw_circle;
			
			procedure query_circle_top (c : in pac_doc_circles.cursor) is 
				circle : type_fillable_circle := element (c);
			begin
				set_destination;
				draw_circle (circle, destination);
			end query_circle_top;

			procedure query_circle_bottom (c : in pac_doc_circles.cursor) is 
				circle : type_fillable_circle := element (c);
			begin
				set_destination (INVERSE);
				draw_circle (circle, destination);
			end query_circle_bottom;

			
			-- POLYGONS
			use pac_doc_polygons;

			procedure draw_polygon (
				polygon	: in out type_polygon_non_conductor;
				f		: in type_face)
			is begin
				if assy_doc_enabled (f) then
					
					if f = face then
						rotate_by (polygon, rot (package_position));
						
						if flipped then mirror (polygon, Y); end if;
						
						move_by (polygon, to_distance_relative (package_position));

						set_color_assy_doc (context.cr, f);

						case polygon.fill_style is
							when SOLID =>
								pac_draw_fab.draw_polygon (in_area, context, polygon, YES,
									zero, self.frame_height);

							when HATCHED =>
								set_line_width (context.cr,
									type_view_coordinate (polygon.hatching.border_width));
								
								pac_draw_fab.draw_polygon (in_area, context, polygon, NO,
									polygon.hatching.border_width, self.frame_height);
								
								-- CS hatching ?
						end case;
						
					end if;

				end if;
				
			end draw_polygon;
			
			procedure query_polygon_top (c : in pac_doc_polygons.cursor) is
				polygon : type_polygon_non_conductor := element (c);
			begin
				set_destination;
				draw_polygon (polygon, destination);
			end query_polygon_top;

			procedure query_polygon_bottom (c : in pac_doc_polygons.cursor) is
				polygon : type_polygon_non_conductor := element (c);
			begin
				set_destination (INVERSE);
				draw_polygon (polygon, destination);
			end query_polygon_bottom;


			-- CUTOUTS
			use pac_doc_cutouts;

			procedure draw_cutout (
				cutout	: in out type_polygon;
				f		: in type_face)
			is begin
				if assy_doc_enabled (f) then
					
					if f = face then
						rotate_by (cutout, rot (package_position));
						
						if flipped then mirror (cutout, Y); end if;
						
						move_by (cutout, to_distance_relative (package_position));

						set_color_background (context.cr);

						pac_draw_fab.draw_polygon (in_area, context, cutout, YES,
							zero, self.frame_height);
						
					end if;

				end if;
				
			end draw_cutout;
			
			procedure query_cutout_top (c : in pac_doc_cutouts.cursor) is
				cutout : type_polygon := element (c);
			begin
				set_destination;
				draw_cutout (cutout, destination);
			end query_cutout_top;

			procedure query_cutout_bottom (c : in pac_doc_cutouts.cursor) is
				cutout : type_polygon := element (c);
			begin
				set_destination (INVERSE);
				draw_cutout (cutout, destination);
			end query_cutout_bottom;

			-- PLACEHOLDERS
			use pac_text_placeholders;

			procedure draw_placeholder (
				ph	: in out type_text_placeholder;
				f	: in type_face) is

				use pac_text_fab.pac_vector_text_lines;
				vector_text : pac_text_fab.pac_vector_text_lines.list;

			begin
				if assy_doc_enabled (f) then
					
					if f = face then

						-- Rotate the position of the placeholder by the rotation of the package.
						-- NOTE: This does not affect the rotation of the placeholder text but only
						-- the rotation about the origin of the package.
						-- If the package has been flipped, then the rotation is counterclockwise.
						if flipped then
							rotate_by (ph.position, - rot (package_position));
						else
							rotate_by (ph.position, rot (package_position));
						end if;

						-- Move the placeholder by the package position to 
						-- its final position:
						move_by (ph.position, to_distance_relative (package_position));

						set_color_assy_doc (context.cr, f);

						draw_text_origin (type_point (ph.position), f);

						-- Set the line width of the vector text:
						set_line_width (context.cr, type_view_coordinate (ph.line_width));

						-- Vectorize the content of the placeholder:
						vector_text := pac_text_fab.vectorize_text (
							content		=> to_placeholder_content (ph), -- map from meaning to content
							size		=> ph.size,
							rotation	=> add (rot (ph.position), rot (package_position)),
							position	=> type_point (ph.position),
							mirror		=> to_mirror (flip), -- mirror vector text if package is flipped
							line_width	=> ph.line_width,
							alignment	=> ph.alignment -- right, bottom
							);

						-- Draw the content of the placeholder:
						pac_draw_fab.draw_vector_text (in_area, context, vector_text,
							ph.line_width, self.frame_height);
						
					end if;

				end if;
			end draw_placeholder;
				
			procedure query_placeholder_top (c : in pac_text_placeholders.cursor) is
				ph : type_text_placeholder := element (c);
			begin
				-- Draw the placeholder only if it has content:
				if not et_text.is_empty (to_placeholder_content (ph)) then
					set_destination;
					draw_placeholder (ph, destination);
				end if;
			end query_placeholder_top;

			procedure query_placeholder_bottom (c : in pac_text_placeholders.cursor) is
				ph : type_text_placeholder := element (c);
			begin
				-- Draw the placeholder only if it has content:
				if not et_text.is_empty (to_placeholder_content (ph)) then
					set_destination (INVERSE);
					draw_placeholder (ph, destination);
				end if;
			end query_placeholder_bottom;


			-- TEXTS
			use pac_texts_fab_with_content;
			
			procedure draw_text (
				t	: in out type_text_fab_with_content;
				f	: in type_face) is
			begin
				if assy_doc_enabled (f) then
	
					if f = face then
						set_color_assy_doc (context.cr, f);
						draw_text_with_content (t, f);
					end if;

				end if;
			end draw_text;

			procedure query_text_top (c : in pac_texts_fab_with_content.cursor) is
				t : type_text_fab_with_content := element (c);
			begin
				set_destination;
				draw_text (t, destination);
			end query_text_top;

			procedure query_text_bottom (c : in pac_texts_fab_with_content.cursor) is
				t : type_text_fab_with_content := element (c);
			begin
				set_destination (INVERSE);
				draw_text (t, destination);
			end query_text_bottom;
			
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

			-- placeholders
			placeholders.assy_doc.top.iterate (query_placeholder_top'access);
			placeholders.assy_doc.bottom.iterate (query_placeholder_bottom'access);
			
			-- texts
			element (package_cursor).assembly_documentation.top.texts.iterate (query_text_top'access);
			element (package_cursor).assembly_documentation.bottom.texts.iterate (query_text_bottom'access);

		end draw_assembly_documentation;

		
		-- KEEPOUT
		procedure draw_keepout is 

			-- LINES
			use pac_keepout_lines;
			line : type_keepout_line;

			procedure draw_line (f : in type_face) is begin
				if keepout_enabled (f) then
				
					if f = face then
						rotate_by (line, rot (package_position));
						
						if flipped then mirror (line, Y); end if;
						
						move_by (line, to_distance_relative (package_position));

						set_color_keepout (context.cr, f);
						pac_draw_fab.draw_line (in_area, context, line, keepout_line_width, self.frame_height);
					end if;

				end if;
			end draw_line;
			
			procedure query_line_top (c : in pac_keepout_lines.cursor) is begin
				line := element (c);
				set_destination;
				draw_line (destination);
			end query_line_top;

			procedure query_line_bottom (c : in pac_keepout_lines.cursor) is begin
				line := element (c);
				set_destination (INVERSE);
				draw_line (destination);
			end query_line_bottom;

			
			-- ARCS
			use pac_keepout_arcs;
			arc : type_keepout_arc;

			procedure draw_arc (f : in type_face) is begin
				if keepout_enabled (f) then
					
					if f = face then
						rotate_by (arc, rot (package_position));
						
						if flipped then mirror (arc, Y); end if;
						
						move_by (arc, to_distance_relative (package_position));

						set_color_keepout (context.cr, f);
						pac_draw_fab.draw_arc (in_area, context, arc, keepout_line_width, self.frame_height);
					end if;
					
				end if;
			end draw_arc;
			
			procedure query_arc_top (c : in pac_keepout_arcs.cursor) is begin
				arc := element (c);
				set_destination;
				draw_arc (destination);
			end query_arc_top;

			procedure query_arc_bottom (c : in pac_keepout_arcs.cursor) is begin
				arc := element (c);
				set_destination (INVERSE);
				draw_arc (destination);
			end query_arc_bottom;

			
			-- CIRCLES
			use pac_keepout_circles;

			procedure draw_circle (
				circle	: in out type_fillable_circle_solid;
				f 		: in type_face) 
			is begin
				if keepout_enabled (f) then
					
					if f = face then
						rotate_by (circle, rot (package_position));
						
						if flipped then mirror (circle, Y); end if;
						
						move_by (circle, to_distance_relative (package_position));

						set_color_keepout (context.cr, f);

						pac_draw_fab.draw_circle (in_area, context, circle, circle.filled,
							keepout_line_width, self.frame_height);
						
					end if;

				end if;
			end draw_circle;
			
			procedure query_circle_top (c : in pac_keepout_circles.cursor) is 
				circle : type_fillable_circle_solid := element (c);
			begin
				set_destination;
				draw_circle (circle, destination);
			end query_circle_top;

			procedure query_circle_bottom (c : in pac_keepout_circles.cursor) is 
				circle : type_fillable_circle_solid := element (c);
			begin
				set_destination (INVERSE);
				draw_circle (circle, destination);
			end query_circle_bottom;

			
			-- POLYGONS
			use pac_keepout_polygons;

			procedure draw_polygon (
				polygon	: in out type_polygon_base'class;
				f		: in type_face)
			is begin
				if keepout_enabled (f) then
					
					if f = face then
						rotate_by (polygon, rot (package_position));
						
						if flipped then mirror (polygon, Y); end if;
						
						move_by (polygon, to_distance_relative (package_position));

						set_color_keepout (context.cr, f);

						pac_draw_fab.draw_polygon (in_area, context, polygon, YES,
							zero, self.frame_height);
						
					end if;

				end if;
			end draw_polygon;
			
			procedure query_polygon_top (c : in pac_keepout_polygons.cursor) is
				polygon : type_keepout_polygon := element (c);
			begin
				set_destination;
				draw_polygon (polygon, destination);
			end query_polygon_top;

			procedure query_polygon_bottom (c : in pac_keepout_polygons.cursor) is
				polygon : type_keepout_polygon := element (c);
			begin
				set_destination (INVERSE);
				draw_polygon (polygon, destination);
			end query_polygon_bottom;


			-- CUTOUTS
			use pac_keepout_cutouts;

			procedure draw_cutout (
				cutout	: in out type_polygon;
				f		: in type_face)
			is begin
				if keepout_enabled (f) then
					
					if f = face then
						rotate_by (cutout, rot (package_position));
						
						if flipped then mirror (cutout, Y); end if;
						
						move_by (cutout, to_distance_relative (package_position));

						set_color_background (context.cr);

						pac_draw_fab.draw_polygon (in_area, context, cutout, YES,
							zero, self.frame_height);
						
					end if;

				end if;
				
			end draw_cutout;
			
			procedure query_cutout_top (c : in pac_keepout_cutouts.cursor) is
				cutout : type_polygon := element (c);
			begin
				set_destination;
				draw_cutout (cutout, destination);
			end query_cutout_top;

			procedure query_cutout_bottom (c : in pac_keepout_cutouts.cursor) is
				cutout : type_polygon := element (c);
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
			use pac_stop_lines;
			line : type_stop_line;

			procedure draw_line (f : in type_face) is begin
				if stop_mask_enabled (f) then
				
					if f = face then
						rotate_by (line, rot (package_position));
						
						if flipped then mirror (line, Y); end if;
						
						move_by (line, to_distance_relative (package_position));

						set_color_stop_mask (context.cr, f, self.scale);
						set_line_width (context.cr, type_view_coordinate (line.width));
						pac_draw_fab.draw_line (in_area, context, line, line.width, self.frame_height);
					end if;

				end if;
			end draw_line;
			
			procedure query_line_top (c : in pac_stop_lines.cursor) is begin
				line := element (c);
				set_destination;
				draw_line (destination);
			end query_line_top;

			procedure query_line_bottom (c : in pac_stop_lines.cursor) is begin
				line := element (c);
				set_destination (INVERSE);
				draw_line (destination);
			end query_line_bottom;

			
			-- ARCS
			use pac_stop_arcs;
			arc : type_stop_arc;

			procedure draw_arc (f : in type_face) is begin
				if stop_mask_enabled (f) then
					
					if f = face then
						rotate_by (arc, rot (package_position));
						
						if flipped then mirror (arc, Y); end if;
						
						move_by (arc, to_distance_relative (package_position));

						set_color_stop_mask (context.cr, f, self.scale);
						set_line_width (context.cr, type_view_coordinate (arc.width));
						pac_draw_fab.draw_arc (in_area, context, arc, arc.width, self.frame_height);
					end if;
					
				end if;
			end draw_arc;
			
			procedure query_arc_top (c : in pac_stop_arcs.cursor) is begin
				arc := element (c);
				set_destination;
				draw_arc (destination);
			end query_arc_top;

			procedure query_arc_bottom (c : in pac_stop_arcs.cursor) is begin
				arc := element (c);
				set_destination (INVERSE);
				draw_arc (destination);
			end query_arc_bottom;

			
			-- CIRCLES
			use pac_stop_circles;

			procedure draw_circle (
				circle	: in out type_fillable_circle;
				f 		: in type_face) 
			is begin
				if stop_mask_enabled (f) then
					
					if f = face then
						rotate_by (circle, rot (package_position));
						
						if flipped then mirror (circle, Y); end if;
						
						move_by (circle, to_distance_relative (package_position));

						set_color_stop_mask (context.cr, f, self.scale);

						case circle.filled is
							when NO =>
								set_line_width (context.cr, type_view_coordinate (circle.border_width));
								pac_draw_fab.draw_circle (in_area, context, circle, circle.filled,
									circle.border_width, self.frame_height);

							when YES =>
								case circle.fill_style is
									when SOLID =>
										pac_draw_fab.draw_circle (in_area, context, circle, circle.filled,
											zero, self.frame_height);

									when HATCHED => null; -- CS
								end case;
						end case;
						
					end if;

				end if;
			end draw_circle;
			
			procedure query_circle_top (c : in pac_stop_circles.cursor) is 
				circle : type_fillable_circle := element (c);
			begin
				set_destination;
				draw_circle (circle, destination);
			end query_circle_top;

			procedure query_circle_bottom (c : in pac_stop_circles.cursor) is 
				circle : type_fillable_circle := element (c);
			begin
				set_destination (INVERSE);
				draw_circle (circle, destination);
			end query_circle_bottom;

			
			-- POLYGONS
			use pac_stop_polygons;

			procedure draw_polygon (
				polygon	: in out type_polygon_non_conductor;
				f		: in type_face)
			is begin
				if stop_mask_enabled (f) then
					
					if f = face then
						rotate_by (polygon, rot (package_position));
						
						if flipped then mirror (polygon, Y); end if;
						
						move_by (polygon, to_distance_relative (package_position));

						set_color_stop_mask (context.cr, f, self.scale);

						case polygon.fill_style is
							when SOLID =>
								pac_draw_fab.draw_polygon (in_area, context, polygon, YES,
									zero, self.frame_height);

							when HATCHED =>
								set_line_width (context.cr,
									type_view_coordinate (polygon.hatching.border_width));
								
								pac_draw_fab.draw_polygon (in_area, context, polygon, NO,
									polygon.hatching.border_width, self.frame_height);
								
								-- CS hatching ?
						end case;
						
					end if;

				end if;
			end draw_polygon;
			
			procedure query_polygon_top (c : in pac_stop_polygons.cursor) is
				polygon : type_polygon_non_conductor := element (c);
			begin
				set_destination;
				draw_polygon (polygon, destination);
			end query_polygon_top;

			procedure query_polygon_bottom (c : in pac_stop_polygons.cursor) is
				polygon : type_polygon_non_conductor := element (c);
			begin
				set_destination (INVERSE);
				draw_polygon (polygon, destination);
			end query_polygon_bottom;


			-- CUTOUTS
			use pac_stop_cutouts;

			procedure draw_cutout (
				cutout	: in out type_polygon;
				f		: in type_face)
			is begin
				if stop_mask_enabled (f) then
					
					if f = face then
						rotate_by (cutout, rot (package_position));
						
						if flipped then mirror (cutout, Y); end if;
						
						move_by (cutout, to_distance_relative (package_position));

						set_color_background (context.cr);

						pac_draw_fab.draw_polygon (in_area, context, cutout, YES,
							zero, self.frame_height);
						
					end if;

				end if;
				
			end draw_cutout;
			
			procedure query_cutout_top (c : in pac_stop_cutouts.cursor) is
				cutout : type_polygon := element (c);
			begin
				set_destination;
				draw_cutout (cutout, destination);
			end query_cutout_top;

			procedure query_cutout_bottom (c : in pac_stop_cutouts.cursor) is
				cutout : type_polygon := element (c);
			begin
				set_destination (INVERSE);
				draw_cutout (cutout, destination);
			end query_cutout_bottom;


			-- TEXTS
			use pac_texts_fab_with_content;
			
			procedure draw_text (
				t	: in out type_text_fab_with_content;
				f	: in type_face) is
			begin
				if stop_mask_enabled (f) then
	
					if f = face then
						set_color_stop_mask (context.cr, f, self.scale);
						draw_text_with_content (t, f);
					end if;

				end if;
			end draw_text;

			procedure query_text_top (c : in pac_texts_fab_with_content.cursor) is
				t : type_text_fab_with_content := element (c);
			begin
				set_destination;
				draw_text (t, destination);
			end query_text_top;

			procedure query_text_bottom (c : in pac_texts_fab_with_content.cursor) is
				t : type_text_fab_with_content := element (c);
			begin
				set_destination (INVERSE);
				draw_text (t, destination);
			end query_text_bottom;
			
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

			-- texts
			element (package_cursor).stop_mask.top.texts.iterate (query_text_top'access);
			element (package_cursor).stop_mask.bottom.texts.iterate (query_text_bottom'access);
			
		end draw_stop_mask;

		
		-- STENCIL / SOLDER CREAM MASK
		procedure draw_stencil is 

			-- LINES
			use pac_stencil_lines;
			line : type_stencil_line;

			procedure draw_line (f : in type_face) is begin
				if stencil_enabled (f) then
				
					if f = face then
						rotate_by (line, rot (package_position));
						
						if flipped then mirror (line, Y); end if;
						
						move_by (line, to_distance_relative (package_position));

						set_color_stencil (context.cr, f, self.scale);
						set_line_width (context.cr, type_view_coordinate (line.width));
						pac_draw_fab.draw_line (in_area, context, line, line.width, self.frame_height);
					end if;

				end if;
			end draw_line;
			
			procedure query_line_top (c : in pac_stencil_lines.cursor) is begin
				line := element (c);
				set_destination;
				draw_line (destination);
			end query_line_top;

			procedure query_line_bottom (c : in pac_stencil_lines.cursor) is begin
				line := element (c);
				set_destination (INVERSE);
				draw_line (destination);
			end query_line_bottom;

			
			-- ARCS
			use pac_stencil_arcs;
			arc : type_stencil_arc;

			procedure draw_arc (f : in type_face) is begin
				if stencil_enabled (f) then
					
					if f = face then
						rotate_by (arc, rot (package_position));
						
						if flipped then mirror (arc, Y); end if;
						
						move_by (arc, to_distance_relative (package_position));

						set_color_stencil (context.cr, f, self.scale);
						set_line_width (context.cr, type_view_coordinate (arc.width));
						pac_draw_fab.draw_arc (in_area, context, arc, arc.width, self.frame_height);
					end if;
					
				end if;
			end draw_arc;
			
			procedure query_arc_top (c : in pac_stencil_arcs.cursor) is begin
				arc := element (c);
				set_destination;
				draw_arc (destination);
			end query_arc_top;

			procedure query_arc_bottom (c : in pac_stencil_arcs.cursor) is begin
				arc := element (c);
				set_destination (INVERSE);
				draw_arc (destination);
			end query_arc_bottom;

			
			-- CIRCLES
			use pac_stencil_circles;

			procedure draw_circle (
				circle	: in out type_fillable_circle;
				f 		: in type_face) 
			is begin
				if stencil_enabled (f) then
					
					if f = face then
						rotate_by (circle, rot (package_position));
						
						if flipped then mirror (circle, Y); end if;
						
						move_by (circle, to_distance_relative (package_position));

						set_color_stencil (context.cr, f, self.scale);

						case circle.filled is
							when NO =>
								set_line_width (context.cr, type_view_coordinate (circle.border_width));
								
								pac_draw_fab.draw_circle (in_area, context, circle, circle.filled, 
									circle.border_width, self.frame_height);

							when YES =>
								case circle.fill_style is
									when SOLID =>
										pac_draw_fab.draw_circle (in_area, context, circle, circle.filled, 
											zero, self.frame_height);

									when HATCHED => null; -- CS
								end case;
						end case;

					end if;

				end if;
			end draw_circle;
			
			procedure query_circle_top (c : in pac_stencil_circles.cursor) is 
				circle : type_fillable_circle := element (c);
			begin
				set_destination;
				draw_circle (circle, destination);
			end query_circle_top;

			procedure query_circle_bottom (c : in pac_stencil_circles.cursor) is 
				circle : type_fillable_circle := element (c);
			begin
				set_destination (INVERSE);
				draw_circle (circle, destination);
			end query_circle_bottom;

			
			-- POLYGONS
			use pac_stencil_polygons;

			procedure draw_polygon (
				polygon	: in out type_polygon_non_conductor;
				f		: in type_face)
			is begin
				if stencil_enabled (f) then
					
					if f = face then
						rotate_by (polygon, rot (package_position));
						
						if flipped then mirror (polygon, Y); end if;
						
						move_by (polygon, to_distance_relative (package_position));

						set_color_stencil (context.cr, f, self.scale);

						case polygon.fill_style is
							when SOLID =>
								pac_draw_fab.draw_polygon (in_area, context, polygon, YES,
									zero, self.frame_height);

							when HATCHED =>
								set_line_width (context.cr,
									type_view_coordinate (polygon.hatching.border_width));

								pac_draw_fab.draw_polygon (in_area, context, polygon, NO,
									polygon.hatching.border_width, self.frame_height);
								
								-- CS hatching ?
						end case;
						
					end if;

				end if;
			end draw_polygon;
			
			procedure query_polygon_top (c : in pac_stencil_polygons.cursor) is
				polygon : type_polygon_non_conductor := element (c);
			begin
				set_destination;
				draw_polygon (polygon, destination);
			end query_polygon_top;

			procedure query_polygon_bottom (c : in pac_stencil_polygons.cursor) is
				polygon : type_polygon_non_conductor := element (c);
			begin
				set_destination (INVERSE);
				draw_polygon (polygon, destination);
			end query_polygon_bottom;


			-- CUTOUTS
			use pac_stencil_cutouts;

			procedure draw_cutout (
				cutout	: in out type_polygon;
				f		: in type_face)
			is begin
				if stencil_enabled (f) then
					
					if f = face then
						rotate_by (cutout, rot (package_position));
						
						if flipped then mirror (cutout, Y); end if;
						
						move_by (cutout, to_distance_relative (package_position));

						set_color_background (context.cr);

						pac_draw_fab.draw_polygon (in_area, context, cutout, YES,
							zero, self.frame_height);
						
					end if;
				end if;				
			end draw_cutout;
			
			procedure query_cutout_top (c : in pac_stencil_cutouts.cursor) is
				cutout : type_polygon := element (c);
			begin
				set_destination;
				draw_cutout (cutout, destination);
			end query_cutout_top;

			procedure query_cutout_bottom (c : in pac_stencil_cutouts.cursor) is
				cutout : type_polygon := element (c);
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
			use pac_route_restrict_lines;			
			line : type_route_restrict_line;

			procedure draw_line (f : in type_face) is begin
				if route_restrict_enabled (f, bottom_layer) then
				
					if f = face then
						rotate_by (line, rot (package_position));
						
						if flipped then mirror (line, Y); end if;
						
						move_by (line, to_distance_relative (package_position));
						pac_draw_fab.draw_line (in_area, context, line, route_restrict_line_width, self.frame_height);
					end if;

				end if;
			end draw_line;
			
			procedure query_line_top (c : in pac_route_restrict_lines.cursor) is begin
				line := element (c);
				set_destination;
				draw_line (destination);
			end query_line_top;

			procedure query_line_bottom (c : in pac_route_restrict_lines.cursor) is begin
				line := element (c);
				set_destination (INVERSE);
				draw_line (destination);
			end query_line_bottom;

			
			-- ARCS
			use pac_route_restrict_arcs;
			arc : type_route_restrict_arc;

			procedure draw_arc (f : in type_face) is begin
				if route_restrict_enabled (f, bottom_layer) then
				
					if f = face then
						rotate_by (arc, rot (package_position));
						
						if flipped then mirror (arc, Y); end if;
						
						move_by (arc, to_distance_relative (package_position));
						pac_draw_fab.draw_arc (in_area, context, arc, route_restrict_line_width, self.frame_height);
					end if;

				end if;
			end draw_arc;
			
			procedure query_arc_top (c : in pac_route_restrict_arcs.cursor) is begin
				arc := element (c);
				set_destination;
				draw_arc (destination);
			end query_arc_top;

			procedure query_arc_bottom (c : in pac_route_restrict_arcs.cursor) is begin
				arc := element (c);
				set_destination (INVERSE);
				draw_arc (destination);
			end query_arc_bottom;

			
			-- CIRCLES
			use pac_route_restrict_circles;
			circle : type_route_restrict_circle;

			procedure draw_circle (f : in type_face) is begin
				if route_restrict_enabled (f, bottom_layer) then
				
					if f = face then
						rotate_by (circle, rot (package_position));
						
						if flipped then mirror (circle, Y); end if;
						
						move_by (circle, to_distance_relative (package_position));
						pac_draw_fab.draw_circle (in_area, context, circle, NO, route_restrict_line_width, self.frame_height);
						-- NO means circle is not filled
					end if;

				end if;
			end draw_circle;
			
			procedure query_circle_top (c : in pac_route_restrict_circles.cursor) is begin
				circle := element (c);
				set_destination;
				draw_circle (destination);
			end query_circle_top;

			procedure query_circle_bottom (c : in pac_route_restrict_circles.cursor) is begin
				circle := element (c);
				set_destination (INVERSE);
				draw_circle (destination);
			end query_circle_bottom;

			
			-- FILL ZONES
			use pac_route_restrict_polygons;
			polygon : type_route_restrict_polygon;

			procedure draw_polygon (f : in type_face) is begin
				if route_restrict_enabled (f, bottom_layer) then
				
					if f = face then
						rotate_by (polygon, rot (package_position));
						
						if flipped then mirror (polygon, Y); end if;
						
						move_by (polygon, to_distance_relative (package_position));
						pac_draw_fab.draw_polygon (in_area, context, polygon, YES, route_restrict_line_width, self.frame_height);
						-- YES means polygon is filled
					end if;

				end if;
			end draw_polygon;
			
			procedure query_polygon_top (c : in pac_route_restrict_polygons.cursor) is begin
				polygon := element (c);
				set_destination;
				draw_polygon (destination);
			end query_polygon_top;

			procedure query_polygon_bottom (c : in pac_route_restrict_polygons.cursor) is begin
				polygon := element (c);
				set_destination (INVERSE);
				draw_polygon (destination);
			end query_polygon_bottom;

			
			-- CUTOUTS
			use pac_route_restrict_cutouts;
			cutout : type_route_restrict_cutout;

			procedure draw_cutout (f : in type_face) is begin
				if route_restrict_enabled (f, bottom_layer) then
				
					if f = face then
						rotate_by (cutout, rot (package_position));
						
						if flipped then mirror (cutout, Y); end if;
						
						move_by (cutout, to_distance_relative (package_position));

						set_color_background (context.cr);
						pac_draw_fab.draw_polygon (in_area, context, cutout, YES, zero, self.frame_height);
						-- YES means cutout is filled, zero means line width
					end if;
				end if;
			end draw_cutout;
			
			procedure query_cutout_top (c : in pac_route_restrict_cutouts.cursor) is begin
				cutout := element (c);
				set_destination;
				draw_cutout (destination);
			end query_cutout_top;

			procedure query_cutout_bottom (c : in pac_route_restrict_cutouts.cursor) is begin
				cutout := element (c);
				set_destination (INVERSE);
				draw_cutout (destination);
			end query_cutout_bottom;


			-- TEXTS
			use pac_conductor_texts_package;
			text : type_conductor_text_package;

			procedure draw_text (f : in type_face) is begin
				if route_restrict_enabled (f, bottom_layer) then
				
					if f = face then
						draw_text_with_content (type_text_fab_with_content (text), f);
					end if;

				end if;
			end draw_text;

			procedure query_text_top (c : in pac_conductor_texts_package.cursor) is begin
				text := element (c);
				set_destination;
				draw_text (destination);
			end query_text_top;

			procedure query_text_bottom (c : in pac_conductor_texts_package.cursor) is begin
				text := element (c);
				set_destination (INVERSE);
				draw_text (destination);
			end query_text_bottom;
			
			
		begin -- draw_route_restrict
			set_color_route_restrict (context.cr);
			set_line_width (context.cr, type_view_coordinate (route_restrict_line_width));
			
			-- lines
			element (package_cursor).route_restrict.top.lines.iterate (query_line_top'access);
			element (package_cursor).route_restrict.bottom.lines.iterate (query_line_bottom'access);
			
			-- arcs
			element (package_cursor).route_restrict.top.arcs.iterate (query_arc_top'access);
			element (package_cursor).route_restrict.bottom.arcs.iterate (query_arc_bottom'access);

			-- circles
			element (package_cursor).route_restrict.top.circles.iterate (query_circle_top'access);
			element (package_cursor).route_restrict.bottom.circles.iterate (query_circle_bottom'access);

			-- polygons
			element (package_cursor).route_restrict.top.polygons.iterate (query_polygon_top'access);
			element (package_cursor).route_restrict.bottom.polygons.iterate (query_polygon_bottom'access);

			-- cutouts
			element (package_cursor).route_restrict.top.cutouts.iterate (query_cutout_top'access);
			element (package_cursor).route_restrict.bottom.cutouts.iterate (query_cutout_bottom'access);

			-- texts
			set_color_route_restrict (context.cr);
			element (package_cursor).route_restrict.top.texts.iterate (query_text_top'access);
			element (package_cursor).route_restrict.bottom.texts.iterate (query_text_bottom'access);			
		end draw_route_restrict;

		
		-- VIA RESTRICT
		procedure draw_via_restrict is 

			-- LINES
			use pac_via_restrict_lines;			
			line : type_via_restrict_line;

			procedure draw_line (f : in type_face) is begin
				if via_restrict_enabled (f, bottom_layer) then
				
					if f = face then
						rotate_by (line, rot (package_position));
						
						if flipped then mirror (line, Y); end if;
						
						move_by (line, to_distance_relative (package_position));
						pac_draw_fab.draw_line (in_area, context, line, via_restrict_line_width, self.frame_height);
					end if;

				end if;
			end draw_line;
			
			procedure query_line_top (c : in pac_via_restrict_lines.cursor) is begin
				line := element (c);
				set_destination;
				draw_line (destination);
			end query_line_top;

			procedure query_line_bottom (c : in pac_via_restrict_lines.cursor) is begin
				line := element (c);
				set_destination (INVERSE);
				draw_line (destination);
			end query_line_bottom;

			
			-- ARCS
			use pac_via_restrict_arcs;
			arc : type_via_restrict_arc;

			procedure draw_arc (f : in type_face) is begin
				if via_restrict_enabled (f, bottom_layer) then
				
					if f = face then
						rotate_by (arc, rot (package_position));
						
						if flipped then mirror (arc, Y); end if;
						
						move_by (arc, to_distance_relative (package_position));
						pac_draw_fab.draw_arc (in_area, context, arc, via_restrict_line_width, self.frame_height);
					end if;

				end if;
			end draw_arc;
			
			procedure query_arc_top (c : in pac_via_restrict_arcs.cursor) is begin
				arc := element (c);
				set_destination;
				draw_arc (destination);
			end query_arc_top;

			procedure query_arc_bottom (c : in pac_via_restrict_arcs.cursor) is begin
				arc := element (c);
				set_destination (INVERSE);
				draw_arc (destination);
			end query_arc_bottom;

			
			-- CIRCLES
			use pac_via_restrict_circles;
			circle : type_via_restrict_circle;

			procedure draw_circle (f : in type_face) is begin
				if via_restrict_enabled (f, bottom_layer) then
				
					if f = face then
						rotate_by (circle, rot (package_position));
						
						if flipped then mirror (circle, Y); end if;
						
						move_by (circle, to_distance_relative (package_position));
						pac_draw_fab.draw_circle (in_area, context, circle, NO, via_restrict_line_width, self.frame_height);
						-- NO means circle is not filled
					end if;

				end if;
			end draw_circle;
			
			procedure query_circle_top (c : in pac_via_restrict_circles.cursor) is begin
				circle := element (c);
				set_destination;
				draw_circle (destination);
			end query_circle_top;

			procedure query_circle_bottom (c : in pac_via_restrict_circles.cursor) is begin
				circle := element (c);
				set_destination (INVERSE);
				draw_circle (destination);
			end query_circle_bottom;

			
			-- FILL ZONES
			use pac_via_restrict_polygons;
			polygon : type_via_restrict_polygon;

			procedure draw_polygon (f : in type_face) is begin
				if via_restrict_enabled (f, bottom_layer) then
				
					if f = face then
						rotate_by (polygon, rot (package_position));
						
						if flipped then mirror (polygon, Y); end if;
						
						move_by (polygon, to_distance_relative (package_position));
						pac_draw_fab.draw_polygon (in_area, context, polygon, YES, via_restrict_line_width, self.frame_height);
						-- YES means polygon is filled
					end if;

				end if;
			end draw_polygon;
			
			procedure query_polygon_top (c : in pac_via_restrict_polygons.cursor) is begin
				polygon := element (c);
				set_destination;
				draw_polygon (destination);
			end query_polygon_top;

			procedure query_polygon_bottom (c : in pac_via_restrict_polygons.cursor) is begin
				polygon := element (c);
				set_destination (INVERSE);
				draw_polygon (destination);
			end query_polygon_bottom;

			
			-- CUTOUTS
			use pac_via_restrict_cutouts;
			cutout : type_via_restrict_cutout;

			procedure draw_cutout (f : in type_face) is begin
				if via_restrict_enabled (f, bottom_layer) then
				
					if f = face then
						rotate_by (cutout, rot (package_position));
						
						if flipped then mirror (cutout, Y); end if;
						
						move_by (cutout, to_distance_relative (package_position));

						set_color_background (context.cr);
						pac_draw_fab.draw_polygon (in_area, context, cutout, YES, zero, self.frame_height);
						-- YES means cutout is filled, zero means line width
					end if;

				end if;
			end draw_cutout;
			
			procedure query_cutout_top (c : in pac_via_restrict_cutouts.cursor) is begin
				cutout := element (c);
				set_destination;
				draw_cutout (destination);
			end query_cutout_top;

			procedure query_cutout_bottom (c : in pac_via_restrict_cutouts.cursor) is begin
				cutout := element (c);
				set_destination (INVERSE);
				draw_cutout (destination);
			end query_cutout_bottom;


			-- TEXTS
			use pac_conductor_texts_package;
			text : type_conductor_text_package;

			procedure draw_text (f : in type_face) is begin
				if via_restrict_enabled (f, bottom_layer) then
				
					if f = face then
						draw_text_with_content (type_text_fab_with_content (text), f);
					end if;

				end if;
			end draw_text;

			procedure query_text_top (c : in pac_conductor_texts_package.cursor) is begin
				text := element (c);
				set_destination;
				draw_text (destination);
			end query_text_top;

			procedure query_text_bottom (c : in pac_conductor_texts_package.cursor) is begin
				text := element (c);
				set_destination (INVERSE);
				draw_text (destination);
			end query_text_bottom;
		
			
		begin -- draw_via_restrict
			set_color_via_restrict (context.cr);
			set_line_width (context.cr, type_view_coordinate (via_restrict_line_width));
			
			-- lines
			element (package_cursor).via_restrict.top.lines.iterate (query_line_top'access);
			element (package_cursor).via_restrict.bottom.lines.iterate (query_line_bottom'access);
			
			-- arcs
			element (package_cursor).via_restrict.top.arcs.iterate (query_arc_top'access);
			element (package_cursor).via_restrict.bottom.arcs.iterate (query_arc_bottom'access);

			-- circles
			element (package_cursor).via_restrict.top.circles.iterate (query_circle_top'access);
			element (package_cursor).via_restrict.bottom.circles.iterate (query_circle_bottom'access);

			-- polygons
			element (package_cursor).via_restrict.top.polygons.iterate (query_polygon_top'access);
			element (package_cursor).via_restrict.bottom.polygons.iterate (query_polygon_bottom'access);

			-- cutouts
			element (package_cursor).via_restrict.top.cutouts.iterate (query_cutout_top'access);
			element (package_cursor).via_restrict.bottom.cutouts.iterate (query_cutout_bottom'access);

			-- texts
			set_color_route_restrict (context.cr);
			element (package_cursor).via_restrict.top.texts.iterate (query_text_top'access);
			element (package_cursor).via_restrict.bottom.texts.iterate (query_text_bottom'access);			
		end draw_via_restrict;

		
		-- PCB HOLE
		procedure draw_pcb_contour is 

			---- LINES
			--use et_packages.pac_pcb_contour_lines;
			--line : et_packages.type_pcb_contour_line;

			--procedure query_line (c : in et_packages.pac_pcb_contour_lines.cursor) is
				--line : type_pcb_contour_line := element (c);
			--begin
				--if outline_enabled then
					--rotate_by (line, rot (package_position));
				
					--if flipped then mirror (line, Y); end if;
					
					--move_by (line, to_distance_relative (package_position));

					--draw_line (in_area, context, line,
						--pcb_contour_line_width, self.frame_height);

				--end if;		
				
			--end query_line;

			
			---- ARCS
			--use pac_pcb_contour_arcs;
			--arc : type_pcb_contour_arc;
			
			--procedure query_arc (c : in pac_pcb_contour_arcs.cursor) is
				--arc : type_pcb_contour_arc := element (c);
			--begin
				--if outline_enabled then
					--rotate_by (arc, rot (package_position));
					
					--if flipped then mirror (arc, Y); end if;
					
					--move_by (arc, to_distance_relative (package_position));

					--draw_arc (in_area, context, arc, 
						--pcb_contour_line_width, self.frame_height);
				--end if;

			--end query_arc;

			
			---- CIRCLES
			--use pac_pcb_contour_circles;
			
			--procedure query_circle (c : in pac_pcb_contour_circles.cursor) is 
				--circle : type_pcb_contour_circle := element (c);
			--begin
				--if outline_enabled then
					--rotate_by (circle, rot (package_position));
					
					--if flipped then mirror (circle, Y); end if;
						
					--move_by (circle, to_distance_relative (package_position));

					--draw_circle (in_area, context, circle, NO,
						--pcb_contour_line_width, self.frame_height);
				--end if;

			--end query_circle;

			use pac_pcb_cutouts;
			use pac_polygon_segments;

			procedure draw_circle (c : in type_circle) is
				circle : type_circle := c;
			begin
				rotate_by (circle, rot (package_position));
				
				if flipped then mirror (circle, Y); end if;
					
				move_by (circle, to_distance_relative (package_position));

				pac_draw_fab.draw_circle (in_area, context, circle, NO,
					pcb_contour_line_width, self.frame_height);

			end draw_circle;

			procedure draw_segment (c : in pac_polygon_segments.cursor) is
				l : type_line;
				a : type_arc;
			begin
				case element (c).shape is
					when LINE =>
						l := element (c).segment_line;
						
						rotate_by (l, rot (package_position));
				
						if flipped then mirror (l, Y); end if;
						
						move_by (l, to_distance_relative (package_position));

						pac_draw_fab.draw_line (in_area, context, l,
							pcb_contour_line_width, self.frame_height);

					when ARC =>
						a := element (c).segment_arc;

						rotate_by (a, rot (package_position));
						
						if flipped then mirror (a, Y); end if;
						
						move_by (a, to_distance_relative (package_position));

						pac_draw_fab.draw_arc (in_area, context, a, 
							pcb_contour_line_width, self.frame_height);
						
				end case;
			end draw_segment;
			
			procedure query_hole (c : in pac_pcb_cutouts.cursor) is
			begin
				if element (c).contours.circular then
					draw_circle (element (c).contours.circle);
				else
					iterate (element (c).contours.segments, draw_segment'access);							 
				end if;
			end query_hole;
			
		begin -- draw_pcb_contour
		
			-- lines
			--element (package_cursor).pcb_contour.lines.iterate (query_line'access);

			-- arcs
			--element (package_cursor).pcb_contour.arcs.iterate (query_arc'access);

			-- circles
			--element (package_cursor).pcb_contour.circles.iterate (query_circle'access);

			if outline_enabled then

				set_color_outline (context.cr);
				set_line_width (context.cr, type_view_coordinate (pcb_contour_line_width));
				
				if not is_empty (element (package_cursor).holes) then
					iterate (element (package_cursor).holes, query_hole'access);
				end if;

			end if;
		end draw_pcb_contour;

		------------------------------------------------------------------

		-- Translates face (TOP/BOTTOM) to conductor layer 1/bottom_layer.
		-- Used by procedure draw_conductors and procedure draw_terminals:
		function face_to_layer (f : in type_face) return type_signal_layer is begin
			case f is
				when TOP => return type_signal_layer'first;
				when BOTTOM => return bottom_layer;
			end case;
		end face_to_layer;

		
		-- CONDUCTORS (NON-TERMINAL RELATED, NON-ELECTRICAL !)
		procedure draw_conductors is 
			use pac_conductor_texts_package;
			use et_conductor_segment;
			
			-- LINES
			use pac_conductor_lines;
			line : type_conductor_line;

			procedure draw_line (f : in type_face) is
				ly : constant type_signal_layer := face_to_layer (f);
			begin
				if conductor_enabled (ly) then
				
					if f = face then
						rotate_by (line, rot (package_position));

						if flipped then mirror (line, Y); end if;
						
						move_by (line, to_distance_relative (package_position));

						set_color_conductor (context.cr, ly);
						set_line_width (context.cr, type_view_coordinate (line.width));
						pac_draw_fab.draw_line (in_area, context, line, line.width, self.frame_height);
					end if;

				end if;
			end draw_line;
			
			procedure query_line_top (c : in pac_conductor_lines.cursor) is begin
				line := element (c);
				set_destination;
				draw_line (destination);
			end query_line_top;

			procedure query_line_bottom (c : in pac_conductor_lines.cursor) is begin
				line := element (c);
				set_destination (INVERSE);
				draw_line (destination);
			end query_line_bottom;

			
			-- ARCS
			use pac_conductor_arcs;
			arc : type_conductor_arc;

			procedure draw_arc (f : in type_face) is
				ly : constant type_signal_layer := face_to_layer (f);
			begin
				if conductor_enabled (ly) then
					
					if f = face then
						rotate_by (arc, rot (package_position));
						
						if flipped then mirror (arc, Y); end if;
						
						move_by (arc, to_distance_relative (package_position));

						set_color_conductor (context.cr, ly);
						set_line_width (context.cr, type_view_coordinate (arc.width));
						pac_draw_fab.draw_arc (in_area, context, arc, arc.width, self.frame_height);
					end if;
					
				end if;
			end draw_arc;
			
			procedure query_arc_top (c : in pac_conductor_arcs.cursor) is begin
				arc := element (c);
				set_destination;
				draw_arc (destination);
			end query_arc_top;

			procedure query_arc_bottom (c : in pac_conductor_arcs.cursor) is begin
				arc := element (c);
				set_destination (INVERSE);
				draw_arc (destination);
			end query_arc_bottom;

			
			-- CIRCLES
			use pac_conductor_circles;

			procedure draw_circle (
				circle	: in out type_conductor_circle;
				f 		: in type_face) is
				ly : constant type_signal_layer := face_to_layer (f);
			begin
				if conductor_enabled (ly) then
					
					if f = face then
						rotate_by (circle, rot (package_position));
						
						if flipped then mirror (circle, Y); end if;
						
						move_by (circle, to_distance_relative (package_position));

						set_color_conductor (context.cr, ly);

						case circle.filled is
							when NO =>
								set_line_width (context.cr, type_view_coordinate (circle.border_width));
								pac_draw_fab.draw_circle (in_area, context, circle, circle.filled,
									circle.border_width, self.frame_height);

							when YES =>
								case circle.fill_style is
									when SOLID =>
										pac_draw_fab.draw_circle (in_area, context, circle, circle.filled,
											zero, self.frame_height);

									when HATCHED => null; -- CS
								end case;
						end case;

					end if;

				end if;
			end draw_circle;
			
			procedure query_circle_top (c : in pac_conductor_circles.cursor) is 
				circle : type_conductor_circle := element (c);
			begin
				set_destination;
				draw_circle (circle, destination);
			end query_circle_top;

			procedure query_circle_bottom (c : in pac_conductor_circles.cursor) is 
				circle : type_conductor_circle := element (c);
			begin
				set_destination (INVERSE);
				draw_circle (circle, destination);
			end query_circle_bottom;

			
			-- POLYGONS

			-- solid
			use pac_conductor_polygons_solid;

			procedure draw_polygon_solid (
				polygon	: in out type_polygon_conductor_solid;
				f		: in type_face) 
			is
				ly : constant type_signal_layer := face_to_layer (f);
			begin
				if conductor_enabled (ly) then
					
					if f = face then
						rotate_by (polygon, rot (package_position));
						
						if flipped then mirror (polygon, Y); end if;
						
						move_by (polygon, to_distance_relative (package_position));

						set_color_conductor (context.cr, ly);

						pac_draw_fab.draw_polygon (in_area, context, polygon, YES,
							zero, self.frame_height);
-- CS
-- 		easing : type_easing;
-- 		width_min : type_track_width; -- the minimum width
-- 		isolation : type_track_clearance := type_track_clearance'first; 
						
					end if;

				end if;
				
			end draw_polygon_solid;
			
			procedure query_polygon_top_solid (
				c : in pac_conductor_polygons_solid.cursor) 
			is
				polygon : type_polygon_conductor_solid := element (c);
			begin
				set_destination;
				draw_polygon_solid (polygon, destination);
			end query_polygon_top_solid;

			procedure query_polygon_bottom_solid (
				c : in pac_conductor_polygons_solid.cursor) 
			is
				polygon : type_polygon_conductor_solid := element (c);
			begin
				set_destination (INVERSE);
				draw_polygon_solid (polygon, destination);
			end query_polygon_bottom_solid;


			-- hatched
			use pac_conductor_polygons_hatched;

			procedure draw_polygon_hatched (
				polygon	: in out type_polygon_conductor_hatched;
				f		: in type_face) 
			is
				ly : constant type_signal_layer := face_to_layer (f);
			begin
				if conductor_enabled (ly) then
					
					if f = face then
						rotate_by (polygon, rot (package_position));
						
						if flipped then mirror (polygon, Y); end if;
						
						move_by (polygon, to_distance_relative (package_position));

						set_color_conductor (context.cr, ly);

-- 						draw_polygon (in_area, context, polygon, YES, self.frame_height);
-- CS
-- 		easing : type_easing;
-- 		hatching : type_hatching_copper;
-- 		width_min : type_track_width; -- the minimum width
-- 		isolation : type_track_clearance := type_track_clearance'first; 
						
					end if;

				end if;
				
			end draw_polygon_hatched;
			
			procedure query_polygon_top_hatched (
				c : in pac_conductor_polygons_hatched.cursor) 
			is
				polygon : type_polygon_conductor_hatched := element (c);
			begin
				set_destination;
				draw_polygon_hatched (polygon, destination);
			end query_polygon_top_hatched;

			
			procedure query_polygon_bottom_hatched (
				c : in pac_conductor_polygons_hatched.cursor) 
			is
				polygon : type_polygon_conductor_hatched := element (c);
			begin
				set_destination (INVERSE);
				draw_polygon_hatched (polygon, destination);
			end query_polygon_bottom_hatched;

			
			-- CUTOUTS
			use packages.pac_conductor_cutouts;

			procedure draw_cutout (
				cutout	: in out type_polygon;
				f		: in type_face) 
			is
				ly : constant type_signal_layer := face_to_layer (f);
			begin
				if conductor_enabled (ly) then
					
					if f = face then
						rotate_by (cutout, rot (package_position));
						
						if flipped then mirror (cutout, Y); end if;
						
						move_by (cutout, to_distance_relative (package_position));

						set_color_background (context.cr);

						pac_draw_fab.draw_polygon (in_area, context, cutout, YES,
							zero, self.frame_height);
						
					end if;

				end if;
			end draw_cutout;
			
			procedure query_cutout_top (
				c : in packages.pac_conductor_cutouts.cursor) 
			is
				cutout : type_polygon := element (c);
			begin
				set_destination;
				draw_cutout (cutout, destination);
			end query_cutout_top;

			
			procedure query_cutout_bottom (
				c : in packages.pac_conductor_cutouts.cursor) 
			is
				cutout : type_polygon := element (c);
			begin
				set_destination (INVERSE);
				draw_cutout (cutout, destination);
			end query_cutout_bottom;


			-- TEXTS

			procedure draw_conductor_text_with_content (
				t : in out type_conductor_text_package;
				f : in type_face)
			is
				use pac_text_fab.pac_vector_text_lines;
				vector_text : pac_text_fab.pac_vector_text_lines.list;
			begin

				-- Rotate the position of the text by the rotation of the package.
				-- NOTE: This does not affect the rotation of the text itself.
				rotate_by (t.position, rot (package_position));
				
				if flipped then mirror (t.position, Y); end if;

				-- Move the text by the package position to 
				-- its final position:
				move_by (t.position, to_distance_relative (package_position));

				draw_text_origin (type_point (t.position), f);

				-- Set the line width of the vector text:
				set_line_width (context.cr, type_view_coordinate (t.line_width));

				-- Vectorize the content of the text:
				vector_text := pac_text_fab.vectorize_text (
					content		=> t.content,
					size		=> t.size,
					rotation	=> add (rot (t.position), rot (package_position)),
					position	=> type_point (t.position),
					mirror		=> to_mirror (flip), -- mirror vector text if package is flipped
					line_width	=> t.line_width,
					alignment	=> t.alignment -- right, bottom
					);

				-- Draw the content of the placeholder:
				pac_draw_fab.draw_vector_text (in_area, context, vector_text, t.line_width, self.frame_height);
				
			end draw_conductor_text_with_content;


			
			use pac_texts_fab_with_content;
			
			procedure draw_text (
				t	: in out type_conductor_text_package;
				f	: in type_face)
			is
				ly : constant type_signal_layer := face_to_layer (f);
			begin
				if conductor_enabled (ly) then
	
					if f = face then
						set_color_conductor (context.cr, ly);
						draw_conductor_text_with_content (t, f);
					end if;

				end if;
			end draw_text;

			procedure query_text_top (c : in pac_conductor_texts_package.cursor) is
				t : type_conductor_text_package := element (c);
			begin
				set_destination;
				draw_text (t, destination);
			end query_text_top;

			procedure query_text_bottom (c : in pac_conductor_texts_package.cursor) is
				t : type_conductor_text_package := element (c);
			begin
				set_destination (INVERSE);
				draw_text (t, destination);
			end query_text_bottom;
			
			
		begin -- draw_conductors
			-- lines
			element (package_cursor).conductors.top.lines.iterate (query_line_top'access);
			element (package_cursor).conductors.bottom.lines.iterate (query_line_bottom'access);

			-- arcs
			element (package_cursor).conductors.top.arcs.iterate (query_arc_top'access);
			element (package_cursor).conductors.bottom.arcs.iterate (query_arc_bottom'access);

			-- circles
			element (package_cursor).conductors.top.circles.iterate (query_circle_top'access);
			element (package_cursor).conductors.bottom.circles.iterate (query_circle_bottom'access);

			-- polygons solid
			element (package_cursor).conductors.top.polygons.solid.iterate (query_polygon_top_solid'access);
			element (package_cursor).conductors.bottom.polygons.solid.iterate (query_polygon_bottom_solid'access);

			-- polygons hatched
			element (package_cursor).conductors.top.polygons.hatched.iterate (query_polygon_top_hatched'access);
			element (package_cursor).conductors.bottom.polygons.hatched.iterate (query_polygon_bottom_hatched'access);

			-- cutouts
			element (package_cursor).conductors.top.cutouts.iterate (query_cutout_top'access);
			element (package_cursor).conductors.bottom.cutouts.iterate (query_cutout_bottom'access);

			-- texts
			element (package_cursor).conductors.top.texts.iterate (query_text_top'access);
			element (package_cursor).conductors.bottom.texts.iterate (query_text_bottom'access);
			
		end draw_conductors;


		-- TERMINALS

		function get_stop_mask_expansion return type_stop_mask_expansion is  -- from DRU
			use et_canvas_schematic;
		begin
			return get_pcb_design_rules (current_active_module).stop_mask.expansion_min;
		end get_stop_mask_expansion;
		
		procedure draw_terminals is
			use type_terminals;

			-- Calculates the final position of the terminal and the 
			-- rotated or mirrored outline.
			procedure move (
				term_pos	: in out type_position; -- terminal position
				outline		: in out type_polygon_base) is
			begin
				-- Rotate the given terminal position by the position of the package:
				rotate_by (term_pos, rot (package_position));

				-- If the package is flipped, then the terminal position
				-- must be mirrored along the Y axis.
				if flipped then mirror (term_pos, Y); end if;
				
				-- Move the given terminal position by the position of the package.
				move_by (term_pos, to_distance_relative (package_position));
				-- The terminal position is now ready for drawing the terminal
				-- name and the pad outline.

				-- The terminal position will later be the offset by which the outline will be moved
				-- to its final place.

				
				if flipped then
					-- The outline must be rotated by the rotation of the package
					-- minus the rotation of the given position itself:
					rotate_by (outline, add (rot (package_position), - rot (term_pos)));

					-- If the package is flipped, then the
					-- given outline (of a pad or a milled hole)
					-- must be mirrored along the Y axis.
					mirror (outline, Y); 
				else				
					-- The outline must be rotated by the rotation of the package
					-- plus the rotation of the given position itself:
					rotate_by (outline, add (rot (package_position), rot (term_pos)));
				end if;
				
				-- Move the outline to its final position:
				move_by (outline, to_distance_relative (term_pos));
			end move;

			procedure draw_tht_pad_with_circular_cutout (
				outer_border	: in type_polygon;
				drill_position	: in type_point;
				drill_size		: in type_drill_size)
			is 
				ib : constant type_circle := (center => drill_position, radius => drill_size * 0.5);
			begin
				set_color_tht_pad (context.cr);

				pac_draw_fab.draw_polygon_with_circular_cutout (
					area			=> in_area,
					context			=> context,
					outer_border	=> outer_border,
					inner_border	=> ib,
					height			=> self.frame_height);

			end draw_tht_pad_with_circular_cutout;

			procedure draw_tht_pad_with_arbitrary_cutout (
				outer_border	: in type_polygon;
				inner_border	: in type_plated_millings)
			is begin
				set_color_tht_pad (context.cr);

				pac_draw_fab.draw_polygon_with_arbitrary_cutout (
					area			=> in_area,
					context			=> context,
					outer_border	=> outer_border,
					inner_border	=> inner_border,
					height			=> self.frame_height);
				
			end draw_tht_pad_with_arbitrary_cutout;

			
			
			procedure query_terminal (c : in type_terminals.cursor) is
				t : constant type_terminal := element (c);

				-- Draws the name of a smt pad.
				procedure draw_name_smt (
					name		: in string;  -- H5, 5, 3
					pad_pos_in	: in type_position)  -- the center of the pad
				is
					use et_text;
				begin
					set_color_terminal_name (context.cr);
					
					pac_draw_fab.draw_text (
						area		=> in_area,
						context		=> context,
						content		=> to_content (name),
						size		=> terminal_name_size,
						font		=> terminal_name_font,
						position	=> type_point (pad_pos_in),
						origin		=> false, -- no origin required
						rotation	=> zero_rotation,
						alignment	=> (center, center),
						height		=> self.frame_height);

				end draw_name_smt;

				
				-- The pad name shall be drawn only once. For this reason we
				-- use this flag. It is set once the name has been drawn the first time.
				name_drawn : boolean := false;

				-- Draws the name of a THT pad if any conductor layer is enabled and
				-- if the name has not been drawn already:
				procedure draw_name_tht (
					name		: in string;  -- H5, 5, 3
					pad_pos_in	: in type_position)  -- the center of the pad
				is
					use et_text;
					pad_pos : type_point := type_point (pad_pos_in);
					
				begin
					if conductors_enabled then
						
						if not name_drawn then
								
							-- Rotate the position of the pad by the rotation of the package:
							rotate_by (pad_pos, rot (package_position));

							-- If the package is flipped, then the terminal position
							-- must be mirrored along the Y axis.
							if flipped then mirror (pad_pos, Y); end if;
							
							-- Move the pad by the position of the package:
							move_by (pad_pos, to_distance_relative (package_position));
							
							set_color_terminal_name (context.cr);
							
							pac_draw_fab.draw_text (
								area		=> in_area,
								context		=> context,
								content		=> to_content (name),
								size		=> terminal_name_size,
								font		=> terminal_name_font,
								position	=> pad_pos,
								origin		=> false, -- no origin required
								rotation	=> zero_rotation,
								alignment	=> (center, center),
								height		=> self.frame_height);

							name_drawn := true;
						end if;
						
					end if;
				end draw_name_tht;

				-- This procedure draws the SMT pad, the stop mask, the stencil and 
				-- the terminal name. The terminal name will be drawn only when
				-- the signal layer is enabled.
				procedure draw_pad_smt (
					name			: in string;  -- H5, 5, 3
					pad_outline_in	: in type_polygon; -- the outline of the solder pad (copper)
					stop_mask_in	: in type_stop_mask_smt; -- the stop mask of the pad
					stencil_in		: in et_terminals.type_stencil; -- the solder cream mask of the pad
					pad_pos_in		: in type_position; -- the center of the pad incl. its rotation
					f				: in type_face) 
				is
					pad_outline : type_polygon := pad_outline_in;
					pad_pos : type_position := pad_pos_in;

					stop_mask_contours	: type_stop_mask_contours;
					stencil_contours	: type_stencil_contours;
					
					ly : constant type_signal_layer := face_to_layer (f);
				begin
					-- We draw only if either the signal layer, the stop mask or the stencil
					-- is enabled. Otherwise nothing will happen here:
					if conductor_enabled (ly) or stop_mask_enabled (f) or stencil_enabled (f) then
						
						if f = face then

							-- Calculate the final position of the terminal and the
							-- rotated or mirrored pad outline.
							move (pad_pos, type_polygon_base (pad_outline));
							
							-- draw the solder pad (conductor material):
							if conductor_enabled (ly) then

								set_color_conductor (context.cr, ly);
								
								pac_draw_fab.draw_polygon (in_area, context, pad_outline, YES,
									zero, self.frame_height);

								-- draw the terminal name
								draw_name_smt (name, pad_pos);
							end if;
							
							-- draw the stop mask
							if stop_mask_enabled (f) then
								
								case stop_mask_in.shape is
									when AS_PAD =>
										-- copy solder pad contours
										stop_mask_contours := (pac_shapes.type_polygon_base (pad_outline) with null record);
										
									when EXPAND_PAD =>
										pad_pos := pad_pos_in;  -- get initial pad position
										
										-- copy solder pad contours and expand according to DRU
										stop_mask_contours := (pac_shapes.type_polygon_base (pad_outline_in) with null record);
										
										offset_polygon (
											polygon		=> stop_mask_contours,
											offset		=> (
												style		=> BY_DISTANCE,
												distance	=> get_stop_mask_expansion)); -- from DRU

										-- compute final position of expanded stop mask opening
										move (pad_pos, type_polygon_base (stop_mask_contours));
										
									when USER_SPECIFIC =>
										-- compute position of user specific stop mask contours:
										pad_pos := pad_pos_in;
										stop_mask_contours := stop_mask_in.contours;
										move (pad_pos, type_polygon_base (stop_mask_contours));
								end case;

								set_color_stop_mask (context.cr, f, self.scale);
								
								pac_draw_fab.draw_polygon (in_area, context, stop_mask_contours, YES,
									zero, self.frame_height);
							end if;
							
							-- draw stencil (or solder paste mask)
							if stencil_enabled (f) then

								case stencil_in.shape is
									
									when AS_PAD =>
										-- copy solder pad contours
										stencil_contours := (pac_shapes.type_polygon_base (pad_outline) with null record);
										
									when SHRINK_PAD =>
										pad_pos := pad_pos_in;  -- get initial pad position

										-- copy solder pad contours and shrink according to shrink_factor
										stencil_contours := (pac_shapes.type_polygon_base (pad_outline_in) with null record);
										
										offset_polygon (
											polygon		=> stencil_contours,
											offset		=> (style => BY_SCALE, scale => stencil_in.shrink_factor));

										-- compute final position of shrinked stencil opening
										move (pad_pos, type_polygon_base (stencil_contours));
										
									when USER_SPECIFIC =>
										-- compute position of user specific stencil contours:
										pad_pos := pad_pos_in; -- get initial pad position
										stencil_contours := stencil_in.contours;
										move (pad_pos, type_polygon_base (stencil_contours));
								end case;

								set_color_stencil (context.cr, f, self.scale);
								
								pac_draw_fab.draw_polygon (in_area, context, stencil_contours, YES,
									zero, self.frame_height);
							end if;

						end if;
					end if;
				end draw_pad_smt;
				
				-- This procedure draws the outer contour of the THT pad and 
				-- th outer contour of the stop mask
				-- in top/bottom signal layer (specified by caller).
				procedure tht_outer_layer (
					pad_outline_in	: in type_polygon; -- the outline of the solder pad
					stop_mask_in	: in et_terminals.type_stop_mask; -- the stop mask in the outer layer
					pad_pos_in		: in type_position; -- the center of the pad incl. its rotation
					f				: in type_face;
					drilled_milled	: in type_terminal_tht_hole;
					drill_size		: in type_drill_size := type_drill_size'first;
					hole_outline_in	: in type_plated_millings := plated_millings_default)
				is
					pad_outline_outer_layer : type_polygon := pad_outline_in;
					pad_pos : type_position := pad_pos_in;

					hole_outline : type_plated_millings := hole_outline_in;
					
					stop_mask_contours : type_stop_mask_contours;
					
					ly : constant type_signal_layer := face_to_layer (f);
				begin
					-- We draw only if either the signal layer or the stop mask
					-- is enabled. Otherwise nothing will happen here:
					if conductor_enabled (ly) or stop_mask_enabled (f) then
						
						if f = face then

							-- Calculate the final position of the terminal and the
							-- rotated or mirrored pad outline.
							move (pad_pos, type_polygon_base (pad_outline_outer_layer));

							-- draw the outer solder pad contour:
							if conductor_enabled (ly) then
								case drilled_milled is
									when DRILLED =>
									
										draw_tht_pad_with_circular_cutout (
											outer_border	=> pad_outline_outer_layer,
											drill_position	=> type_point (pad_pos),
											drill_size		=> drill_size);


									when MILLED =>

										-- Calculate the final position of the milled hole:
										pad_pos := pad_pos_in;
										move (pad_pos, type_polygon_base (hole_outline));
										
										draw_tht_pad_with_arbitrary_cutout (
											outer_border	=> pad_outline_outer_layer,
											inner_border	=> hole_outline);

								end case;
							end if;
							
							-- draw the stop mask
							if stop_mask_enabled (f) then
								
								case stop_mask_in.shape is
									when AS_PAD =>
										-- copy solder pad contours to stop mask:
										stop_mask_contours := (pac_shapes.type_polygon_base (pad_outline_outer_layer) with null record);
										
									when EXPAND_PAD =>
										pad_pos := pad_pos_in;  -- get initial pad position
										
										-- copy solder pad contours and expand according to DRU
										stop_mask_contours := (pac_shapes.type_polygon_base (pad_outline_in) with null record);
										
										offset_polygon (
											polygon		=> stop_mask_contours,
											offset		=> (
												style 		=> BY_DISTANCE,
												distance	=> get_stop_mask_expansion));  -- from DRU

										-- compute final position of expanded stop mask opening
										move (pad_pos, type_polygon_base (stop_mask_contours));
										
									when USER_SPECIFIC =>
										-- compute position of user specific stop mask contours:
										pad_pos := pad_pos_in;
										stop_mask_contours := stop_mask_in.contours;
										move (pad_pos, type_polygon_base (stop_mask_contours));
								end case;

								set_color_stop_mask (context.cr, f, self.scale);

								-- draw the outer contour of the stop mask opening
								pac_draw_fab.draw_polygon (
									area		=> in_area,
									context		=> context,
									polygon		=> stop_mask_contours,
									filled		=> YES,
									width		=> zero,
									height		=> self.frame_height);

							end if;

						end if;

					end if;
				end tht_outer_layer;

				-- This procedure draws the pad contour of a milled THT pad
				-- in an inner conductor layer
				-- if any inner conductor layer is enabled. If no inner conductor
				-- layer is enabled, nothing happens.
				procedure tht_inner_layer_milled (
					hole_outline_in	: in type_plated_millings; -- the countours of the milled hole
					restring_width	: in type_track_width;
					pad_pos_in		: in type_position) -- the center of the pad incl. its rotation
				is
					hole_outline : type_plated_millings := hole_outline_in;
					pad_pos : type_position := pad_pos_in;

					pad_outline_inner_layers : type_polygon;
				begin
					if inner_conductors_enabled (bottom_layer) then
								
						move (pad_pos, type_polygon_base (hole_outline));
						
						-- Compute a polygon that extends the given hole outline by the restring_width:
						pad_outline_inner_layers := (type_polygon_base (hole_outline_in) with null record);
						
						offset_polygon (
							polygon		=> pad_outline_inner_layers, 
							offset		=> (style => BY_DISTANCE, distance => restring_width));

						-- move the conductor frame to its final position:
						pad_pos := pad_pos_in;  -- get initial pad position
						move (pad_pos, type_polygon_base (pad_outline_inner_layers));
						
						draw_tht_pad_with_arbitrary_cutout (
							outer_border	=> pad_outline_inner_layers,
							inner_border	=> hole_outline);

					end if;
				end tht_inner_layer_milled;

				-- This procedure draws the pad contour of a drilled THT pad
				-- in an inner conductor layer
				-- if any inner conductor layer is enabled. If no inner conductor
				-- layer is enabled, nothing happens.
				procedure tht_inner_layer_drilled (
					drill_size	: in type_drill_size;
					restring	: in type_restring_width;
					pad_pos_in	: in type_position) -- the center of the pad incl. its rotation
				is
					pad_pos : type_position := pad_pos_in;

					circle : type_circle;
				begin
					if inner_conductors_enabled (bottom_layer) then
						
						-- Rotate the position of the drill by the rotation of the package:
						rotate_by (pad_pos, rot (package_position));

						if flipped then mirror (pad_pos, Y); end if;

						-- Move the drill by the position of the package:
						move_by (pad_pos, to_distance_relative (package_position));


						-- Build a circle to show the restring of inner layers:
						circle.center := type_point (pad_pos);

						-- set line width and radius:
						--set_line_width (context.cr, type_view_coordinate (restring));
						--circle.radius := (drill_size + restring) * 0.5;

						set_color_tht_pad (context.cr);
						set_line_width (context.cr, type_view_coordinate (zero));

						circle.radius := drill_size * 0.5 + restring;
						
						--draw_circle (in_area, context, circle, NO, self.frame_height);
						pac_draw_fab.draw_circle (in_area, context, circle, YES, zero, self.frame_height);

						
						-- the cutout area must clear out the outer area:
						set_operator (context.cr, CAIRO_OPERATOR_CLEAR);

						circle.radius := drill_size * 0.5;
						pac_draw_fab.draw_circle (in_area, context, circle, YES, zero, self.frame_height);

						-- restore default compositing operator:
						set_operator (context.cr, CAIRO_OPERATOR_OVER);		
					end if;
				end tht_inner_layer_drilled;
				

			begin -- query_terminal
				-- The terminal can be a through-hole type (THT) or a pad for surface mounting (SMT):
				case t.technology is
					
					when THT =>

						-- The pad can have a circular hole or a hole of arbitrary shape:
						case t.tht_hole is

							when DRILLED => -- circlular hole

								-- draw pad outline of top layer:
								set_destination;
								tht_outer_layer (
									pad_outline_in	=> t.pad_shape_tht.top,
									stop_mask_in	=> t.stop_mask_shape_tht.top,
									pad_pos_in		=> t.position,
									f				=> destination,
									drilled_milled	=> t.tht_hole,
									drill_size		=> t.drill_size);

								-- draw pad outline of bottom layer:
								set_destination (INVERSE);
								tht_outer_layer (
									pad_outline_in	=> t.pad_shape_tht.bottom,
									stop_mask_in	=> t.stop_mask_shape_tht.bottom,
									pad_pos_in		=> t.position,
									f				=> destination,
									drilled_milled	=> t.tht_hole,
									drill_size		=> t.drill_size);

								-- draw pad outline of inner layer:
								tht_inner_layer_drilled (
									drill_size		=> t.drill_size,
									restring		=> t.width_inner_layers,
									pad_pos_in		=> t.position);

									draw_name_tht (to_string (key (c)), t.position);

									
							when MILLED => -- arbitrary shape or so called plated millings

								-- draw pad outline of top layer:
								set_destination;
								tht_outer_layer (
									pad_outline_in	=> t.pad_shape_tht.top,
									stop_mask_in	=> t.stop_mask_shape_tht.top,
									pad_pos_in		=> t.position,
									f				=> destination,
									drilled_milled	=> t.tht_hole,
									hole_outline_in	=> t.millings);

								-- draw pad outline of bottom layer:
								set_destination (INVERSE);
								tht_outer_layer (
									pad_outline_in	=> t.pad_shape_tht.bottom,
									stop_mask_in	=> t.stop_mask_shape_tht.bottom,
									pad_pos_in		=> t.position,
									f				=> destination,
									drilled_milled	=> t.tht_hole,
									hole_outline_in	=> t.millings);
								
								-- draw pad outline of inner layer:
								tht_inner_layer_milled (
									hole_outline_in	=> t.millings,
									restring_width	=> t.width_inner_layers,
									pad_pos_in		=> t.position);

								draw_name_tht (to_string (key (c)), t.position);
						end case;
						
					when SMT =>
						case t.face is
							when TOP	=> set_destination;								
							when BOTTOM	=> set_destination (INVERSE);
						end case;

						draw_pad_smt (to_string (key (c)), t.pad_shape_smt, 
							t.stop_mask_shape_smt, t.stencil_shape, t.position, destination);
				end case;
				
			end query_terminal;
			
		begin -- draw_terminals
			element (package_cursor).terminals.iterate (query_terminal'access);
		end draw_terminals;

		
		procedure draw_package_origin is
			line_horizontal : constant type_line := ( -- from left to right
				start_point		=> type_point (set (x => get_x (package_position) - et_packages.origin_half_size, y => get_y (package_position))),
				end_point		=> type_point (set (x => get_x (package_position) + et_packages.origin_half_size, y => get_y (package_position))));

			line_vertical : constant type_line := ( -- from bottom to top
				start_point		=> type_point (set (x => get_x (package_position), y => get_y (package_position) - et_packages.origin_half_size)),
				end_point		=> type_point (set (x => get_x (package_position), y => get_y (package_position) + et_packages.origin_half_size)));

		begin -- draw_package_origin
			if face = get_face (package_position) then
				if device_origins_enabled (get_face (package_position)) then

					set_color_origin (context.cr);
					set_line_width (context.cr, type_view_coordinate (et_packages.origin_line_width));
					pac_draw_fab.draw_line (in_area, context, line_horizontal, et_packages.origin_line_width, self.frame_height);
					pac_draw_fab.draw_line (in_area, context, line_vertical, et_packages.origin_line_width, self.frame_height);

				end if;
			end if;
		end draw_package_origin;
		
	begin -- draw_package
		draw_conductors; -- NON-TERMINAL RELATED, NON-ELECTRICAL
		draw_terminals; -- pins, pads, plated millings
		
		draw_stop_mask; -- non-terminal related
		draw_stencil; -- non-terminal related

		draw_silkscreen;
		draw_assembly_documentation;
		draw_keepout; 

		draw_route_restrict;
		draw_via_restrict;
		
		draw_pcb_contour;
		
		draw_package_origin;
	end draw_package;

	use et_schematic;
	
	procedure query_devices (
		module_name	: in pac_module_name.bounded_string;
		module		: in type_module) 
	is
		use et_schematic.pac_devices_sch;

		procedure query_device (d : in et_schematic.pac_devices_sch.cursor) is
			use et_symbols;
			use et_pcb;
		begin
			if element (d).appearance = PCB then
				
				draw_package (
					device_name			=> key (d), -- R1, IC12
					device_value		=> element (d).value, -- 7400, 100R
					device_purpose		=> element (d).purpose, -- brightness control
					model				=> get_package_model (d), -- libraries/packages/smd/SOT23.pac
					package_position	=> element (d).position, -- x/y/rotation/face
					flip				=> element (d).flipped,

					-- The text placeholders specified in the board override
					-- the default placeholders of the model:
					placeholders		=> element (d).text_placeholders);

			end if;
		end query_device;
		
	begin -- query_devices
		module.devices.iterate (query_device'access);
	end query_devices;

	
	procedure query_devices_non_electric (
		module_name	: in pac_module_name.bounded_string;
		module		: in type_module) is

		use et_schematic.pac_devices_non_electric;
		
		procedure query_device (p : in pac_devices_non_electric.cursor) is 
			use et_devices;
		begin
			draw_package (
				device_name			=> key (p), -- H1, FD2
				package_position	=> element (p).position, -- x/y/rotation/face
				flip				=> element (p).flipped,

				-- The text placeholders specified in the board override
				-- the default placeholders of the model:
				placeholders		=> element (p).text_placeholders, 
				
				model				=> element (p).package_model, -- libraries/packages/smd/SOT23.pac
				device_value		=> to_value (""),
				device_purpose		=> to_purpose (""));

		end query_device;
		
	begin
		module.devices_non_electric.iterate (query_device'access);
	end query_devices_non_electric;

	
begin -- draw_packages
-- 	put_line ("draw packages ...");

	-- draw electric devices
	pac_generic_modules.query_element (
		position	=> et_canvas_schematic.current_active_module,
		process		=> query_devices'access);

	-- draw non-electric devices (like fiducials, mounting holes, ...)
	pac_generic_modules.query_element (
		position	=> et_canvas_schematic.current_active_module,
		process		=> query_devices_non_electric'access);
			
end draw_packages;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
