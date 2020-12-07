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

with et_general;				use et_general;
with et_symbols;
with et_devices;

with et_drills;					use et_drills;
with et_packages;				use et_packages;

with et_pcb;
with et_pcb_stack;				use et_pcb_stack;

with et_display.board;			use et_display.board;
with et_colors;					use et_colors;

with et_text;

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
		device_value	: in et_devices.type_value.bounded_string; -- SN7400
		device_purpose	: in et_devices.type_purpose.bounded_string; -- brightness control
		model			: in et_packages.type_package_model_file.bounded_string;
		package_position: in et_pcb_coordinates.type_package_position; -- incl. rotation and face
		flip			: in et_pcb.type_flipped;
		placeholders	: in et_packages.type_text_placeholders) -- specified in the board. will override default positions
	is

		use et_terminals.pac_shapes;
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

		-- Maps from meaning of given placeholder to text content:
		function to_placeholder_content (ph : in type_text_placeholder)
			return et_text.type_text_content.bounded_string is
			use et_devices;
			use et_text;
			result : type_text_content.bounded_string;
		begin
			case ph.meaning is
				when NAME => result := to_content (to_string (device_name));
				when VALUE => result := to_content (to_string (device_value));
				when PURPOSE => result := to_content (to_string (device_purpose));
			end case;
			
			return result;
		end to_placeholder_content;
			
		procedure draw_text_origin (p : in type_point; f : in type_face) is
			type type_line is new et_terminals.pac_shapes.type_line with null record;
			
			line_horizontal : constant type_line := ( -- from left to right
				start_point		=> type_point (set (x => x (p) - pac_text.origin_half_size, y => y (p))),
				end_point		=> type_point (set (x => x (p) + pac_text.origin_half_size, y => y (p))));

			line_vertical : constant type_line := ( -- from bottom to top
				start_point		=> type_point (set (x => x (p), y => y (p) - pac_text.origin_half_size)),
				end_point		=> type_point (set (x => x (p), y => y (p) + pac_text.origin_half_size)));

		begin -- draw_text_origin
			if device_origins_enabled (f) then

				-- The caller of this procedure has a setting for the color.
				-- So we backup this context setting.
				save (context.cr);
				
				set_color_origin (context.cr);
				set_line_width (context.cr, type_view_coordinate (pac_text.origin_line_width));
				pac_draw_package.draw_line (in_area, context, line_horizontal, self.frame_height);
				pac_draw_package.draw_line (in_area, context, line_vertical, self.frame_height);

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
			t : in out type_text_with_content;
			f : in type_face) is

			use pac_text.pac_vector_text_lines;
			vector_text : pac_text.pac_vector_text_lines.list;
	
		begin

			-- Rotate the position of the text by the rotation of the package.
			-- NOTE: This does not affect the rotation of the text itself.
			rotate_by (t.position, rot (package_position));
			
			if flipped then mirror (t.position, Y); end if;

			-- Move the text by the package position to 
			-- its final position:
			move_by (t.position, type_point (package_position));

			draw_text_origin (type_point (t.position), f);

			-- Set the line width of the vector text:
			set_line_width (context.cr, type_view_coordinate (t.line_width));

			-- Vectorize the content of the text:
			vector_text := pac_text.vectorize (
				content		=> t.content,
				size		=> t.size,
				rotation	=> add (rot (t.position), rot (package_position)),
				position	=> type_point (t.position),
				mirror		=> to_mirror (flip), -- mirror vector text if package is flipped
				line_width	=> t.line_width,
				alignment	=> t.alignment -- right, bottom
				);

			-- Draw the content of the placeholder:
			pac_draw_package.draw_vector_text (in_area, context, vector_text, self.frame_height);
			
		end draw_text_with_content;
		
		-- SILKSCREEN
		procedure draw_silkscreen is 

			-- LINES
			use type_silk_lines;
			line : type_silk_line;

			procedure draw_line (f : in type_face) is begin
				if silkscreen_enabled (f) then
				
					if f = face then
						rotate_by (line, rot (package_position));
						
						if flipped then mirror (line, Y); end if;
						
						move_by (line, type_point (package_position));

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
						rotate_by (arc, rot (package_position));
						
						if flipped then mirror (arc, Y); end if;
						
						move_by (arc, type_point (package_position));

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
						rotate_by (circle, rot (package_position));
						
						if flipped then mirror (circle, Y); end if;
						
						move_by (circle, type_point (package_position));

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
						rotate_by (polygon, rot (package_position));
						
						if flipped then mirror (polygon, Y); end if;
						
						move_by (polygon, type_point (package_position));

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
						rotate_by (cutout, rot (package_position));
						
						if flipped then mirror (cutout, Y); end if;
						
						move_by (cutout, type_point (package_position));

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


			-- PLACEHOLDERS
			use pac_text_placeholders;

			procedure draw_placeholder (
				ph	: in out type_text_placeholder;
				f	: in type_face) is

				use pac_text.pac_vector_text_lines;
				vector_text : pac_text.pac_vector_text_lines.list;

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
						move_by (ph.position, type_point (package_position));

						set_color_silkscreen (context.cr, f);

						draw_text_origin (type_point (ph.position), f);

						-- Set the line width of the vector text:
						set_line_width (context.cr, type_view_coordinate (ph.line_width));

						-- Vectorize the content of the placeholder:
						vector_text := pac_text.vectorize (
							content		=> to_placeholder_content (ph), -- map from meaning to content
							size		=> ph.size,
							rotation	=> add (rot (ph.position), rot (package_position)),
							position	=> type_point (ph.position),
							mirror		=> to_mirror (flip), -- mirror vector text if package is flipped
							line_width	=> ph.line_width,
							alignment	=> ph.alignment -- right, bottom
							);

						-- Draw the content of the placeholder:
						pac_draw_package.draw_vector_text (in_area, context, vector_text, self.frame_height);
						
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
			use type_texts_with_content;
			
			procedure draw_text (
				t	: in out type_text_with_content;
				f	: in type_face) is
			begin
				if silkscreen_enabled (f) then
	
					if f = face then
						set_color_silkscreen (context.cr, f);
						draw_text_with_content (t, f);
					end if;

				end if;
			end draw_text;

			procedure query_text_top (c : in type_texts_with_content.cursor) is
				t : type_text_with_content := element (c);
			begin
				-- Draw the text only if it has content:
				if not et_text.is_empty (t.content) then
					set_destination;
					draw_text (t, destination);
				end if;
			end query_text_top;

			procedure query_text_bottom (c : in type_texts_with_content.cursor) is
				t : type_text_with_content := element (c);
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
			use type_doc_lines;
			line : type_doc_line;

			procedure draw_line (f : in type_face) is begin
				if assy_doc_enabled (f) then
				
					if f = face then
						rotate_by (line, rot (package_position));
						
						if flipped then mirror (line, Y); end if;
						
						move_by (line, type_point (package_position));

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
						rotate_by (arc, rot (package_position));
						
						if flipped then mirror (arc, Y); end if;
						
						move_by (arc, type_point (package_position));

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
						rotate_by (circle, rot (package_position));
						
						if flipped then mirror (circle, Y); end if;
						
						move_by (circle, type_point (package_position));

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
						rotate_by (polygon, rot (package_position));
						
						if flipped then mirror (polygon, Y); end if;
						
						move_by (polygon, type_point (package_position));

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
						rotate_by (cutout, rot (package_position));
						
						if flipped then mirror (cutout, Y); end if;
						
						move_by (cutout, type_point (package_position));

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

			-- PLACEHOLDERS
			use pac_text_placeholders;

			procedure draw_placeholder (
				ph	: in out type_text_placeholder;
				f	: in type_face) is

				use pac_text.pac_vector_text_lines;
				vector_text : pac_text.pac_vector_text_lines.list;

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
						move_by (ph.position, type_point (package_position));

						set_color_assy_doc (context.cr, f);

						draw_text_origin (type_point (ph.position), f);

						-- Set the line width of the vector text:
						set_line_width (context.cr, type_view_coordinate (ph.line_width));

						-- Vectorize the content of the placeholder:
						vector_text := pac_text.vectorize (
							content		=> to_placeholder_content (ph), -- map from meaning to content
							size		=> ph.size,
							rotation	=> add (rot (ph.position), rot (package_position)),
							position	=> type_point (ph.position),
							mirror		=> to_mirror (flip), -- mirror vector text if package is flipped
							line_width	=> ph.line_width,
							alignment	=> ph.alignment -- right, bottom
							);

						-- Draw the content of the placeholder:
						pac_draw_package.draw_vector_text (in_area, context, vector_text, self.frame_height);
						
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
			use type_texts_with_content;
			
			procedure draw_text (
				t	: in out type_text_with_content;
				f	: in type_face) is
			begin
				if assy_doc_enabled (f) then
	
					if f = face then
						set_color_assy_doc (context.cr, f);
						draw_text_with_content (t, f);
					end if;

				end if;
			end draw_text;

			procedure query_text_top (c : in type_texts_with_content.cursor) is
				t : type_text_with_content := element (c);
			begin
				set_destination;
				draw_text (t, destination);
			end query_text_top;

			procedure query_text_bottom (c : in type_texts_with_content.cursor) is
				t : type_text_with_content := element (c);
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
			use type_keepout_lines;
			line : type_keepout_line;

			procedure draw_line (f : in type_face) is begin
				if keepout_enabled (f) then
				
					if f = face then
						rotate_by (line, rot (package_position));
						
						if flipped then mirror (line, Y); end if;
						
						move_by (line, type_point (package_position));

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
						rotate_by (arc, rot (package_position));
						
						if flipped then mirror (arc, Y); end if;
						
						move_by (arc, type_point (package_position));

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
						rotate_by (circle, rot (package_position));
						
						if flipped then mirror (circle, Y); end if;
						
						move_by (circle, type_point (package_position));

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
						rotate_by (polygon, rot (package_position));
						
						if flipped then mirror (polygon, Y); end if;
						
						move_by (polygon, type_point (package_position));

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
						rotate_by (cutout, rot (package_position));
						
						if flipped then mirror (cutout, Y); end if;
						
						move_by (cutout, type_point (package_position));

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
						rotate_by (line, rot (package_position));
						
						if flipped then mirror (line, Y); end if;
						
						move_by (line, type_point (package_position));

						set_color_stop_mask (context.cr, f, self.scale);
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
						rotate_by (arc, rot (package_position));
						
						if flipped then mirror (arc, Y); end if;
						
						move_by (arc, type_point (package_position));

						set_color_stop_mask (context.cr, f, self.scale);
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
						rotate_by (circle, rot (package_position));
						
						if flipped then mirror (circle, Y); end if;
						
						move_by (circle, type_point (package_position));

						set_color_stop_mask (context.cr, f, self.scale);

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
						rotate_by (polygon, rot (package_position));
						
						if flipped then mirror (polygon, Y); end if;
						
						move_by (polygon, type_point (package_position));

						set_color_stop_mask (context.cr, f, self.scale);

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
						rotate_by (cutout, rot (package_position));
						
						if flipped then mirror (cutout, Y); end if;
						
						move_by (cutout, type_point (package_position));

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


			-- TEXTS
			use type_texts_with_content;
			
			procedure draw_text (
				t	: in out type_text_with_content;
				f	: in type_face) is
			begin
				if stop_mask_enabled (f) then
	
					if f = face then
						set_color_stop_mask (context.cr, f, self.scale);
						draw_text_with_content (t, f);
					end if;

				end if;
			end draw_text;

			procedure query_text_top (c : in type_texts_with_content.cursor) is
				t : type_text_with_content := element (c);
			begin
				set_destination;
				draw_text (t, destination);
			end query_text_top;

			procedure query_text_bottom (c : in type_texts_with_content.cursor) is
				t : type_text_with_content := element (c);
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
			use type_stencil_lines;
			line : type_stencil_line;

			procedure draw_line (f : in type_face) is begin
				if stencil_enabled (f) then
				
					if f = face then
						rotate_by (line, rot (package_position));
						
						if flipped then mirror (line, Y); end if;
						
						move_by (line, type_point (package_position));

						set_color_stencil (context.cr, f, self.scale);
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
						rotate_by (arc, rot (package_position));
						
						if flipped then mirror (arc, Y); end if;
						
						move_by (arc, type_point (package_position));

						set_color_stencil (context.cr, f, self.scale);
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
						rotate_by (circle, rot (package_position));
						
						if flipped then mirror (circle, Y); end if;
						
						move_by (circle, type_point (package_position));

						set_color_stencil (context.cr, f, self.scale);

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
						rotate_by (polygon, rot (package_position));
						
						if flipped then mirror (polygon, Y); end if;
						
						move_by (polygon, type_point (package_position));

						set_color_stencil (context.cr, f, self.scale);

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
						rotate_by (cutout, rot (package_position));
						
						if flipped then mirror (cutout, Y); end if;
						
						move_by (cutout, type_point (package_position));

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
					mirror (line.layers, bottom_layer);
				end if;

				if route_restrict_layer_enabled (line.layers) then
					
					rotate_by (line, rot (package_position));

					if flipped then 
						mirror (line, Y);
					end if;
					
					move_by (line, type_point (package_position));

					pac_draw_package.draw_line (in_area, context, line, self.frame_height);
				end if;
			end query_line;

			
			-- ARCS
			use type_route_restrict_arcs;
			
			procedure query_arc (c : in type_route_restrict_arcs.cursor) is 
				arc : type_route_restrict_arc := element (c);
			begin
				if flipped then 
					mirror (arc.layers, bottom_layer);
				end if;

				if route_restrict_layer_enabled (arc.layers) then
					
					rotate_by (arc, rot (package_position));

					if flipped then 
						mirror (arc, Y); 
					end if;

					move_by (arc, type_point (package_position));

					pac_draw_package.draw_arc (in_area, context, arc, self.frame_height);
				end if;
			end query_arc;

			
			-- CIRCLES
			use type_route_restrict_circles;
			
			procedure query_circle (c : in type_route_restrict_circles.cursor) is 
				circle : type_route_restrict_circle := element (c);
			begin
				if flipped then 
					mirror (circle.layers, bottom_layer);
				end if;

				if route_restrict_layer_enabled (circle.layers) then
				
					rotate_by (circle, rot (package_position));

					if flipped then 
						mirror (circle, Y);
					end if;
					
					move_by (circle, type_point (package_position));

					pac_draw_package.draw_circle (in_area, context, circle, circle.filled, self.frame_height);
				end if;

			end query_circle;

			
			-- POLYGONS
			use type_route_restrict_polygons;
			
			procedure query_polygon (c : in type_route_restrict_polygons.cursor) is
				polygon : et_packages.type_route_restrict_polygon := element (c);
			begin
				if flipped then 
					mirror (polygon.layers, bottom_layer);
				end if;

				if route_restrict_layer_enabled (polygon.layers) then
					
					rotate_by (polygon, rot (package_position));

					if flipped then 
						mirror (polygon, Y);
					end if;

					move_by (polygon, type_point (package_position));

					pac_draw_package.draw_polygon (in_area, context, polygon, YES, self.frame_height);
				end if;

			end query_polygon;


			-- CUTOUTS
			use pac_route_restrict_cutouts;
		
			procedure query_cutout (c : in pac_route_restrict_cutouts.cursor) is
				cutout : et_packages.type_route_restrict_cutout := element (c);
			begin
				if flipped then 
					mirror (cutout.layers, bottom_layer);
				end if;

				if route_restrict_layer_enabled (cutout.layers) then
					
					rotate_by (cutout, rot (package_position));

					if flipped then 
						mirror (cutout, Y); 
					end if;

					move_by (cutout, type_point (package_position));

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
					mirror (line.layers, bottom_layer);
				end if;

				if via_restrict_layer_enabled (line.layers) then
					
					rotate_by (line, rot (package_position));

					if flipped then 
						mirror (line, Y);
					end if;

					move_by (line, type_point (package_position));

					pac_draw_package.draw_line (in_area, context, line, self.frame_height);
				end if;
			end query_line;

			
			-- ARCS
			use type_via_restrict_arcs;
			
			procedure query_arc (c : in type_via_restrict_arcs.cursor) is 
				arc : type_via_restrict_arc := element (c);
			begin
				if flipped then 
					mirror (arc.layers, bottom_layer);
				end if;

				if via_restrict_layer_enabled (arc.layers) then
					
					rotate_by (arc, rot (package_position));

					if flipped then 
						mirror (arc, Y); 
					end if;

					move_by (arc, type_point (package_position));

					pac_draw_package.draw_arc (in_area, context, arc, self.frame_height);
				end if;
			end query_arc;

			
			-- CIRCLES
			use type_via_restrict_circles;
			
			procedure query_circle (c : in type_via_restrict_circles.cursor) is 
				circle : type_via_restrict_circle := element (c);
			begin
				if flipped then 
					mirror (circle.layers, bottom_layer);
				end if;

				if via_restrict_layer_enabled (circle.layers) then
				
					rotate_by (circle, rot (package_position));

					if flipped then 
						mirror (circle, Y);
					end if;

					move_by (circle, type_point (package_position));

					pac_draw_package.draw_circle (in_area, context, circle, circle.filled, self.frame_height);
				end if;

			end query_circle;

			
			-- POLYGONS
			use type_via_restrict_polygons;
			
			procedure query_polygon (c : in type_via_restrict_polygons.cursor) is
				polygon : et_packages.type_via_restrict_polygon := element (c);
			begin
				if flipped then 
					mirror (polygon.layers, bottom_layer);
				end if;

				if via_restrict_layer_enabled (polygon.layers) then
					
					rotate_by (polygon, rot (package_position));

					if flipped then 
						mirror (polygon, Y);
					end if;

					move_by (polygon, type_point (package_position));

					pac_draw_package.draw_polygon (in_area, context, polygon, YES, self.frame_height);
				end if;

			end query_polygon;


			-- CUTOUTS
			use pac_via_restrict_cutouts;
		
			procedure query_cutout (c : in pac_via_restrict_cutouts.cursor) is
				cutout : et_packages.type_via_restrict_cutout := element (c);
			begin
				if flipped then 
					mirror (cutout.layers, bottom_layer);
				end if;

				if via_restrict_layer_enabled (cutout.layers) then
					
					rotate_by (cutout, rot (package_position));

					if flipped then 
						mirror (cutout, Y); 
					end if;
					
					move_by (cutout, type_point (package_position));

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
					rotate_by (line, rot (package_position));
				
					if flipped then mirror (line, Y); end if;
					
					move_by (line, type_point (package_position));

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
					rotate_by (arc, rot (package_position));
					
					if flipped then mirror (arc, Y); end if;
					
					move_by (arc, type_point (package_position));

					pac_draw_package.draw_arc (in_area, context, arc, self.frame_height);
				end if;

			end query_arc;

			
			-- CIRCLES
			use type_pcb_contour_circles;
			
			procedure query_circle (c : in type_pcb_contour_circles.cursor) is 
				circle : type_pcb_contour_circle := element (c);
			begin
				if outline_enabled then
					rotate_by (circle, rot (package_position));
					
					if flipped then mirror (circle, Y); end if;
						
					move_by (circle, type_point (package_position));

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
			
			-- LINES
			use type_copper_lines;
			line : type_copper_line;

			procedure draw_line (f : in type_face) is
				ly : constant type_signal_layer := face_to_layer (f);
			begin
				if conductor_enabled (ly) then
				
					if f = face then
						rotate_by (line, rot (package_position));

						if flipped then mirror (line, Y); end if;
						
						move_by (line, type_point (package_position));

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
						rotate_by (arc, rot (package_position));
						
						if flipped then mirror (arc, Y); end if;
						
						move_by (arc, type_point (package_position));

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
						rotate_by (circle, rot (package_position));
						
						if flipped then mirror (circle, Y); end if;
						
						move_by (circle, type_point (package_position));

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
						rotate_by (polygon, rot (package_position));
						
						if flipped then mirror (polygon, Y); end if;
						
						move_by (polygon, type_point (package_position));

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
						rotate_by (polygon, rot (package_position));
						
						if flipped then mirror (polygon, Y); end if;
						
						move_by (polygon, type_point (package_position));

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
						rotate_by (cutout, rot (package_position));
						
						if flipped then mirror (cutout, Y); end if;
						
						move_by (cutout, type_point (package_position));

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


			-- TEXTS
			use type_texts_with_content;
			
			procedure draw_text (
				t	: in out type_text_with_content;
				f	: in type_face) is
				ly : constant type_signal_layer := face_to_layer (f);
			begin
				if conductor_enabled (ly) then
	
					if f = face then
						set_color_conductor (context.cr, ly);
						draw_text_with_content (t, f);
					end if;

				end if;
			end draw_text;

			procedure query_text_top (c : in type_texts_with_content.cursor) is
				t : type_text_with_content := element (c);
			begin
				set_destination;
				draw_text (t, destination);
			end query_text_top;

			procedure query_text_bottom (c : in type_texts_with_content.cursor) is
				t : type_text_with_content := element (c);
			begin
				set_destination (INVERSE);
				draw_text (t, destination);
			end query_text_bottom;
			
			
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

			-- texts
			element (package_cursor).copper.top.texts.iterate (query_text_top'access);
			element (package_cursor).copper.bottom.texts.iterate (query_text_bottom'access);
			
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
				move_by (term_pos, type_point (package_position));
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
				move_by (outline, type_point (term_pos));
			end move;

			procedure draw_name (
				name	: in string;  -- H5, 5, 3
				pos		: in type_position) is -- the center of the pad
				use et_text;
			begin
				set_color_terminal_name (context.cr);

				pac_draw_package.draw_text (
					area		=> in_area,
					context		=> context,
					content		=> to_content (name),
					size		=> terminal_name_size,
					font		=> terminal_name_font,
					position	=> type_point (pos),
					origin		=> false, -- no origin required
					rotation	=> zero_rotation,
					alignment	=> (others => <>),
					height		=> self.frame_height);

			end draw_name;

			-- This procedure draws the SMT pad, the stop mask, the stencil and 
			-- the terminal name. The terminal name will be drawn only when
			-- the signal layer is enabled.
			procedure draw_pad_smt (
				name			: in string;  -- H5, 5, 3
				pad_outline_in	: in type_pad_outline; -- the outline of the solder pad (copper)
				stop_mask_in	: in type_stop_mask_smt; -- the stop mask of the pad
				stencil_in		: in et_terminals.type_stencil; -- the solder cream mask of the pad
				pad_pos_in		: in type_position; -- the center of the pad incl. its rotation
				f				: in type_face) is

				pad_outline : type_pad_outline := pad_outline_in;
				pad_pos : type_position := pad_pos_in;

				stop_mask_contours	: type_stop_mask_contours;
				stencil_contours	: type_stencil_contours;
				
				ly : constant type_signal_layer := face_to_layer (f);

				use pac_draw_package;
			begin
				-- We draw only if either the signal layer, the stop mask or the stencil
				-- is enabled. Otherwise nothing will happen here:
				if conductor_enabled (ly) or stop_mask_enabled (f) or stencil_enabled (f) then
					
					if f = face then

						-- Calculate the final position of the terminal and the
						-- rotated or mirrored pad outline.
						move (pad_pos, type_polygon_base (pad_outline));
						
						-- draw the solder pad (copper):
						if conductor_enabled (ly) then

							set_color_conductor (context.cr, ly);
							draw_polygon (in_area, context, pad_outline, YES, self.frame_height);

							-- draw the terminal name
							draw_name (name, pad_pos);
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
							draw_polygon (in_area, context, stop_mask_contours, YES, self.frame_height);
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
							draw_polygon (in_area, context, stencil_contours, YES, self.frame_height);
						end if;

					end if;
				end if;
			end draw_pad_smt;

			-- This procedure draws the outer "restring" of the THT pad and the stop mask
			-- in an outer signal layer (specified by caller).
			-- The terminal name will be drawn only when the signal layer is enabled.
			procedure draw_pad_tht_outer_layer (
				pad_outline_in	: in type_pad_outline; -- the outline of the solder pad (copper)
				stop_mask_in	: in et_terminals.type_stop_mask; -- the stop mask in the outer layer
				pad_pos_in		: in type_position; -- the center of the pad incl. its rotation
				f				: in type_face) is

				pad_outline : type_pad_outline := pad_outline_in;
				pad_pos : type_position := pad_pos_in;

				stop_mask_contours : type_stop_mask_contours;
				
				ly : constant type_signal_layer := face_to_layer (f);

				use pac_draw_package;
			begin
				-- We draw only if either the signal layer or the stop mask
				-- is enabled. Otherwise nothing will happen here:
				if conductor_enabled (ly) or stop_mask_enabled (f) then
					
					if f = face then

						-- Calculate the final position of the terminal and the
						-- rotated or mirrored pad outline.
						move (pad_pos, type_polygon_base (pad_outline));

						-- draw the solder pad (copper):
						if conductor_enabled (ly) then

							set_color_tht_pad (context.cr);
							draw_polygon (in_area, context, pad_outline, YES, self.frame_height);

						end if;
						
						-- draw the stop mask
						if stop_mask_enabled (f) then
							
							case stop_mask_in.shape is
								when AS_PAD =>
									-- copy solder pad contours to stop mask:
									stop_mask_contours := (pac_shapes.type_polygon_base (pad_outline) with null record);
									
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
							draw_polygon (in_area, context, stop_mask_contours, YES, self.frame_height);
						end if;

					end if;
				end if;
			end draw_pad_tht_outer_layer;

			-- This procedure draws the milled hole (any shape) of a THT pad and the "restring"
			-- of the inner signal layers:
			procedure draw_pad_tht_hole_milled (
				name			: in string;  -- H5, 5, 3
				outline_in		: in type_plated_millings;
				restring_width	: in type_track_width;
				pad_pos_in		: in type_position) is -- the center of the pad incl. its rotation

				hole_outline : type_plated_millings := outline_in;
				pad_pos : type_position := pad_pos_in;

				pad_outline : type_pad_outline;				
			begin
				
				-- We draw the hole only if any conductor layer is enabled.
				-- If no conductor layers are enabled, no hole will be shown.
				if conductors_enabled then
					
					move (pad_pos, type_polygon_base (hole_outline));

					-- Draw the conductor frame ("restring") around the hole if any inner signal layer is enabled:
					if inner_conductors_enabled (bottom_layer) then
						pad_pos := pad_pos_in;  -- get initial pad position
						
						-- Compute a polygon that extends the given hole outline by the restring_width:
						pad_outline := (type_polygon_base (outline_in) with null record);
						
						offset_polygon (
							polygon		=> pad_outline, 
							offset		=> (style => BY_DISTANCE, distance => restring_width));

						-- move the conductor frame to its final position:
						move (pad_pos, type_polygon_base (pad_outline));
						
						-- Draw the conductor frame:
						set_color_tht_pad (context.cr);
						pac_draw_package.draw_polygon (in_area, context, pad_outline, YES, self.frame_height);
					end if;
					
					-- Draw the hole outline:
					set_color_background (context.cr);
					pac_draw_package.draw_polygon (in_area, context, hole_outline, YES, self.frame_height);

					-- draw the terminal name
					draw_name (name, pad_pos);
				end if;
			end draw_pad_tht_hole_milled;

			-- This procedure draws the circular hole of a THT pad and the restring
			-- of the inner signal layers:
			procedure draw_pad_tht_hole_drilled (
				name		: in string;  -- H5, 5, 3
				drill_size	: in type_drill_size;
				restring	: in type_track_width;
				pad_pos_in	: in type_position) is -- the center of the pad incl. its rotation

				pad_pos : type_position := pad_pos_in;

				type type_circle is new et_terminals.pac_shapes.type_circle with null record;
				circle : type_circle;
			begin
				-- We draw the hole only if a conductor layer is enabled.
				-- If no conductor layers are enabled, no hole will be shown.
				if conductors_enabled then
					
					if flipped then 
						mirror (pad_pos, Y);
					end if;
					
					-- Rotate the position of the drill by the rotation of the package:
					rotate_by (pad_pos, rot (package_position));

					-- Move the drill by the position of the package:
					move_by (pad_pos, type_point (package_position));


					circle.center := type_point (pad_pos);

					-- If any inner layer is enabled, build a circle to show the restring 
					-- of inner layers:
					if inner_conductors_enabled (bottom_layer) then
						circle.radius := drill_size * 0.5 + restring;
					
						set_color_tht_pad (context.cr);
						pac_draw_package.draw_circle (in_area, context, circle, YES, self.frame_height);
					end if;
					
					-- Build a black filled circle to show the drill:
					circle.radius := drill_size * 0.5;
					
					set_color_background (context.cr);
					pac_draw_package.draw_circle (in_area, context, circle, YES, self.frame_height);

					-- draw the terminal name
					draw_name (name, pad_pos);
				end if;
			end draw_pad_tht_hole_drilled;
			
			procedure query_terminal (c : in type_terminals.cursor) is
				t : constant type_terminal := element (c);
			begin
				-- The terminal can be a through-hole type (THT) or a pad for surface mounting (SMT):
				case t.technology is
					
					when THT =>
						-- draw pad outline of top layer:
						set_destination;
						draw_pad_tht_outer_layer (t.pad_shape_tht.top, t.stop_mask_shape_tht.top, t.position, destination);

						-- draw pad outline of bottom layer:
						set_destination (INVERSE);
						draw_pad_tht_outer_layer (t.pad_shape_tht.bottom, t.stop_mask_shape_tht.bottom, t.position, destination);

						-- The pad can have a circular hole or a hole of arbitrary shape:
						case t.tht_hole is
							when DRILLED => -- circlular hole
								draw_pad_tht_hole_drilled (to_string (key (c)), t.drill_size, t.width_inner_layers, t.position);
								
							when MILLED => -- arbitrary shape or so called plated millings
								draw_pad_tht_hole_milled (to_string (key (c)), t.millings, t.width_inner_layers, t.position);
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
			type type_line is new et_terminals.pac_shapes.type_line with null record;
			
			line_horizontal : constant type_line := ( -- from left to right
				start_point		=> type_point (set (x => x (package_position) - origin_half_size, y => y (package_position))),
				end_point		=> type_point (set (x => x (package_position) + origin_half_size, y => y (package_position))));

			line_vertical : constant type_line := ( -- from bottom to top
				start_point		=> type_point (set (x => x (package_position), y => y (package_position) - origin_half_size)),
				end_point		=> type_point (set (x => x (package_position), y => y (package_position) + origin_half_size)));

		begin -- draw_package_origin
			if face = get_face (package_position) then
				if device_origins_enabled (get_face (package_position)) then

					set_color_origin (context.cr);
					set_line_width (context.cr, type_view_coordinate (origin_line_width));
					pac_draw_package.draw_line (in_area, context, line_horizontal, self.frame_height);
					pac_draw_package.draw_line (in_area, context, line_vertical, self.frame_height);

				end if;
			end if;
		end draw_package_origin;
		
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
		draw_package_origin;
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
		module_name	: in type_module_name.bounded_string;
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
