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
with et_schematic;				use et_schematic;
use et_schematic.type_nets;

with et_project;				use et_project;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_packages;				use et_packages;
use et_pcb_coordinates.geometry;

with et_pcb;					--use et_pcb;

with et_display.board;			use et_display.board;

with et_canvas_primitive_draw_ops;

separate (et_canvas_board)

procedure draw_packages (
	self    : not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context;
	face	: in type_face) is

	procedure draw_package (
		model			: in et_packages.type_package_model_file.bounded_string;
		position		: in et_pcb_coordinates.type_package_position; -- incl. rotation and face
		flip			: in et_pcb.type_flipped;
		placeholders	: in et_packages.type_text_placeholders) is

		use et_pcb;
		use et_packages.pac_shapes;
		use type_packages;

		function flipped return boolean is begin
			if flip = NO then return false;
			else return true;
			end if;
		end flipped;

		destination : type_face;
		type type_destination_inversed is (INVERSE, NOT_INVERSE);

		procedure set_face (i : in type_destination_inversed := NOT_INVERSE) is begin
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
		end set_face;
		
		-- locate the package model in the package library:
		package_cursor : constant et_packages.type_packages.cursor := locate_package_model (model);
	

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
						stroke (context.cr);
					end if;

				end if;
			end draw_line;
			
			procedure query_line_top (c : in type_silk_lines.cursor) is begin
				line := element (c);
				set_face;
				draw_line (destination);
			end query_line_top;

			procedure query_line_bottom (c : in type_silk_lines.cursor) is begin
				line := element (c);
				set_face (INVERSE);
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
						stroke (context.cr);
					end if;
					
				end if;
			end draw_arc;
			
			procedure query_arc_top (c : in type_silk_arcs.cursor) is begin
				arc := element (c);
				set_face;
				draw_arc (destination);
			end query_arc_top;

			procedure query_arc_bottom (c : in type_silk_arcs.cursor) is begin
				arc := element (c);
				set_face (INVERSE);
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
						
						stroke (context.cr);
					end if;

				end if;
			end draw_circle;
			
			procedure query_circle_top (c : in type_silk_circles.cursor) is 
				circle : type_fillable_circle := element (c);
			begin
				set_face;
				draw_circle (circle, destination);
			end query_circle_top;

			procedure query_circle_bottom (c : in type_silk_circles.cursor) is 
				circle : type_fillable_circle := element (c);
			begin
				set_face (INVERSE);
				draw_circle (circle, destination);
			end query_circle_bottom;

			
			-- POLYGONS
			use pac_silk_polygons;

			procedure draw_polygon (
				polygon	: in out et_packages.type_polygon;
				f		: in type_face)
			is begin
				null;
				null; --: pac_silk_polygons.list;

				if silkscreen_enabled (f) then
					
					if f = face then
-- CS						if flipped then mirror (circle, Y); end if;
						
-- 						rotate_by (circle, rot (position));
-- 						move_by (circle, type_point (position));

						set_color_silkscreen (context.cr, f);

						case polygon.fill_style is
							when SOLID =>
								null;
-- 								set_line_width (context.cr, type_view_coordinate (circle.border_width));
-- 								pac_draw_package.draw_circle (in_area, context, circle, circle.filled, self.frame_height);

							when HATCHED =>
								null;
-- 									pac_draw_package.draw_circle (in_area, context, circle, circle.filled, self.frame_height);
						end case;
						
						stroke (context.cr);
					end if;

				end if;
				
			end draw_polygon;
			
			procedure query_polygon_top (c : in pac_silk_polygons.cursor) is
				polygon : et_packages.type_polygon := element (c);
			begin
				set_face;
				draw_polygon (polygon, destination);
			end query_polygon_top;

			procedure query_polygon_bottom (c : in pac_silk_polygons.cursor) is
				polygon : et_packages.type_polygon := element (c);
			begin
				set_face (INVERSE);
				draw_polygon (polygon, destination);
			end query_polygon_bottom;

			
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


			
			-- CS
			-- placeholders

			
-- 			cutouts 	: pac_silk_cutouts.list;
-- 			texts		: type_texts_with_content.list;
			
		end draw_silkscreen;

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
					stroke (context.cr);

				end if;
			end if;
		end draw_origin;
		
	begin -- draw_package
		draw_silkscreen;
		-- CS draw_assembly_documentation;
		-- CS draw_terminals
		-- CS draw_conductors non-terminal related
		-- CS draw_keepout
		-- CS draw_stop_mask non-terminal related
		-- CS draw_stencil non-terminal related
		-- CS draw_route_restrict
		-- CS draw_via_restrict
		-- CS draw_pcb_contour
		
		-- The origin is drawn last so that it obscures other elements of the package:
		draw_origin;
	end draw_package;
	
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
