------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW PACKAGES                             --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
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
-- To Do:
--
-- 1. The calls of functions like get_conductor_objects or
--    get_assy_doc_objects, get_silkscreen_objects ... should
--    no longer be used. In the past they where intended to move, rotate and
--    mirror the objects in a comfortable way.
--    Reason: All These functions call the function
--    get_package_model in order to map from the schematic device cursor
--    to the package model cursor. This is a waste of time.
--    Instead a single mapping operation from device cursor to
--    package cursor should be used. 
--    In the next step individual primitive objects like lines, arcs, circle
--    should be passed in their original form to primitive draw operations draw_line, draw_arc,
--    draw_circle.
--    See approach in et_canvas_board_2.compute_bounding_box for
--    possible solutions.

with ada.text_io;				use ada.text_io;

with et_mirroring;					use et_mirroring;

with et_symbols;
with et_device_name;
with et_schematic;

with et_drills;						use et_drills;

with et_device_library;
with et_device_model_names;
with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.packages;	use et_device_placeholders.packages;

with et_package_names;
with et_packages;					use et_packages;
with et_package_variant;
with et_terminals;

with et_pcb_contour;
with et_pcb_stack;					use et_pcb_stack;

with et_board_shapes_and_text;

with et_device_query_board;			use et_device_query_board;

with et_modes.board;
with et_canvas_board_devices;
with et_canvas_tool;

with et_display.board;				use et_display.board;
with et_colors;						use et_colors;
with et_colors.board;				use et_colors.board;
with et_design_rules_board;			use et_design_rules_board;
with et_text;
with et_alignment;

with et_conductor_segment;
with et_conductor_text.packages;

with et_fill_zones;					use et_fill_zones;
with et_fill_zones.packages;		use et_fill_zones.packages;

with et_route_restrict;				
with et_route_restrict.packages;

with et_via_restrict;
with et_via_restrict.packages;

with et_stopmask;
with et_stopmask.packages;

with et_stencil;
with et_silkscreen;
with et_assy_doc;
with et_keepout;					

with et_contour_to_polygon;			use et_contour_to_polygon;

with et_board_ops;


separate (et_canvas_board_2)

procedure draw_packages (
	face	: in type_face) -- the side of the board to be drawn
is

	-- The deepest conductor layer towards bottom is defined by the layer stack:
	bottom_layer : constant type_signal_layer := 
		et_board_ops.get_deepest_conductor_layer (active_module);

	
	procedure draw_package (
		electric			: in boolean;
		device_electric		: in et_schematic.pac_devices_sch.cursor;
		device_non_electric	: in et_pcb.pac_devices_non_electric.cursor;					   
		flip				: in type_flipped;
		brightness			: in type_brightness)
	is
		package_position : type_package_position;  -- incl. rotation and face

		use pac_geometry_2;	
		use pac_contours;
		use pac_polygons;
		use pac_offsetting;

		use pac_package_models;

		use pac_draw_contours;
		
		
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
		-- By default set_destination sets the destination side to BOTTOM 
		-- if the package is flipped.
		procedure set_destination (
			i : in type_destination_inversed := NOT_INVERSE) 
		is 
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
		
	
		
		procedure draw_text_origin (
			p : in type_vector_model; 
			f : in type_face) 
		is begin
			if device_origins_enabled (f) then

				-- The caller of this procedure has a setting for the color.
				-- So we backup this context setting.
				-- save (context.cr);
				
				set_color_origin;
				draw_origin ((p, 0.0));
				-- set_line_width (context.cr, type_view_coordinate (pac_text_board.origin_line_width));
				-- draw_line (line_horizontal, pac_text_board.origin_line_width);
				-- draw_line (line_vertical, pac_text_board.origin_line_width);

				-- Restore context setting of caller. See comment above.
				-- restore (context.cr);
			end if;
		end draw_text_origin;
		

		
	-- SILKSCREEN

		-- This procedure draws the silkscreen objects of
		-- the package:
		procedure draw_silkscreen is 
			use et_silkscreen;			
			use pac_silk_lines;
			use pac_silk_arcs;
			use pac_silk_circles;
			use pac_silk_zones;
			use pac_silk_texts;

			face : type_face := TOP;			
			silkscreen_top, silkscreen_bottom : type_silkscreen;

			
			procedure draw is

				procedure query_line (c : in pac_silk_lines.cursor) is 
					line : type_silk_line renames element (c);
				begin
					-- The line has already been moved, flipped and rotated
					-- to the final position. So we do not pass the 
					-- position and rotation of the package.
					-- Likewise there is no need to mirror anything here:
					draw_line (
						line		=> line,
						width		=> line.width,
						do_stroke	=> true);
				end query_line;

				
				procedure query_arc (c : in pac_silk_arcs.cursor) is 
					arc : type_silk_arc renames element (c);
				begin
					-- See comments in procedure query_line.
					draw_arc (
						arc			=> arc,
						width		=> arc.width,
						do_stroke	=> true);
				end query_arc;

				
				procedure query_circle (c : in pac_silk_circles.cursor) is 
					circle : type_silk_circle renames element (c);
				begin
					-- See comments in procedure query_line.
					draw_circle (
						circle		=> circle,
						filled		=> NO,
						width		=> circle.width,
						do_stroke	=> true);
				end query_circle;

				
				procedure query_zone (c : in pac_silk_zones.cursor) is 
					zone : type_silk_zone renames element (c);
				begin
					draw_contour (
						contour	=> zone,
						filled	=> YES,
						width	=> zero);
				end query_zone;

				
				procedure query_text (c : in pac_silk_texts.cursor) is 
					use pac_draw_text;
				begin
					draw_vector_text (element (c));
				end query_text;

				
				face : type_face := TOP;

			begin
				set_color_silkscreen (face, brightness);
				silkscreen_top.lines.iterate (query_line'access);
				silkscreen_top.arcs.iterate (query_arc'access);
				silkscreen_top.circles.iterate (query_circle'access);
				silkscreen_top.zones.iterate (query_zone'access);
				silkscreen_top.texts.iterate (query_text'access);

				face := BOTTOM;
				set_color_silkscreen (face, brightness);
				silkscreen_bottom.lines.iterate (query_line'access);
				silkscreen_bottom.arcs.iterate (query_arc'access);
				silkscreen_bottom.circles.iterate (query_circle'access);
				silkscreen_bottom.zones.iterate (query_zone'access);
				silkscreen_bottom.texts.iterate (query_text'access);
			end draw;
			
			
		begin -- draw_silkscreen
			if electric then
				if silkscreen_enabled (face) then
					silkscreen_top    := get_silkscreen_objects (device_electric, TOP);
				end if;

				face := BOTTOM;
				if silkscreen_enabled (face) then
					silkscreen_bottom := get_silkscreen_objects (device_electric, BOTTOM);
				end if;
				
			else -- non-electrical device
				if silkscreen_enabled (face) then
					silkscreen_top    := get_silkscreen_objects (device_non_electric, TOP);
				end if;

				face := BOTTOM;
				if silkscreen_enabled (face) then
					silkscreen_bottom := get_silkscreen_objects (device_non_electric, BOTTOM);
				end if;
			end if;

			draw;
		end draw_silkscreen;



		
		
	-- ASSEMBLY DOCUMENTATION
		
		procedure draw_assembly_documentation is 
			use et_assy_doc;			
			use pac_doc_lines;
			use pac_doc_arcs;
			use pac_doc_circles;
			use pac_doc_zones;
			use pac_doc_texts;

			face : type_face := TOP;

			doc_top, doc_bottom : type_assy_doc;

			
			procedure draw is

				procedure query_line (c : in pac_doc_lines.cursor) is 
					line : type_doc_line renames element (c);
				begin
					-- The line has already been moved, flipped and rotated
					-- to the final position. So we do not pass the 
					-- position and rotation of the package.
					-- Likewise there is no need to mirror anything here:
					draw_line (
						line		=> line,
						width		=> line.width,
						do_stroke	=> true);

				end query_line;

				
				procedure query_arc (c : in pac_doc_arcs.cursor) is 
					arc : type_doc_arc renames element (c);
				begin
					-- See comments in procedure query_line.
					draw_arc (
						arc			=> arc,
						width		=> arc.width,
						do_stroke	=> true);
				end query_arc;

				
				procedure query_circle (c : in pac_doc_circles.cursor) is 
					circle : type_doc_circle renames element (c);
				begin
					-- See comments in procedure query_line.
					-- put_line ("draw circle");
					
					draw_circle (
						circle		=> circle,
						filled		=> NO,
						width		=> circle.width,
						do_stroke	=> true);
				end query_circle;

			
				procedure query_zone (c : in pac_doc_zones.cursor) is 
					zone : type_doc_zone renames element (c);
				begin
					-- See comments in procedure query_line.
					draw_contour (
						contour	=> zone,
						filled	=> YES,
						width	=> zero);
				end query_zone;

				
				procedure query_text (c : in pac_doc_texts.cursor) is 
					use pac_draw_text;
				begin
					draw_vector_text (element (c));
				end query_text;

				
				face : type_face := TOP;
				
			begin
				set_color_assy_doc (face, brightness);
				doc_top.lines.iterate (query_line'access);
				doc_top.arcs.iterate (query_arc'access);
				doc_top.circles.iterate (query_circle'access);
				doc_top.zones.iterate (query_zone'access);
				doc_top.texts.iterate (query_text'access);

				face := BOTTOM;
				set_color_assy_doc (face, brightness);
				doc_bottom.lines.iterate (query_line'access);
				doc_bottom.arcs.iterate (query_arc'access);
				doc_bottom.circles.iterate (query_circle'access);
				doc_bottom.zones.iterate (query_zone'access);
				doc_bottom.texts.iterate (query_text'access);
			end draw;

			
		begin -- draw_assembly_documentation
			if electric then
				if assy_doc_enabled (face) then
					doc_top    := get_assy_doc_objects (device_electric, TOP);
				end if;

				face := BOTTOM;
				if assy_doc_enabled (face) then
					doc_bottom := get_assy_doc_objects (device_electric, BOTTOM);
				end if;
				
			else -- non-electrical device
				-- put_line ("non electrical device");
				
				if assy_doc_enabled (face) then
					doc_top    := get_assy_doc_objects (device_non_electric, TOP);
				end if;

				face := BOTTOM;
				if assy_doc_enabled (face) then
					doc_bottom := get_assy_doc_objects (device_non_electric, BOTTOM);
				end if;
			end if;

			draw;
		end draw_assembly_documentation;


		
		
	-- KEEPOUT
		
		procedure draw_keepout is 
			use et_keepout;
			use pac_keepout_zones;
			keepout : type_keepout_both_sides;
			face : type_face := TOP;

			
			procedure draw is
				
				procedure query_zone (
					c : pac_keepout_zones.cursor) 
				is begin
					-- The zones have already been moved, flipped and rotated
					-- to the final position. So we do not pass the 
					-- position and rotation of the package.
					-- Likewise there is no need to mirror anything here:
					draw_contour (
						contour	=> element (c),
						filled	=> NO,
						width	=> zero);
					
				end query_zone;
				
			begin
				-- top
				set_color_keepout (TOP, brightness);
				keepout.top.zones.iterate (query_zone'access);
				-- CS cutouts

				-- bottom
				set_color_keepout (BOTTOM, brightness);
				keepout.bottom.zones.iterate (query_zone'access);
				-- CS cutouts
			end draw;

			
		begin -- draw_keepout
			
			if electric then
				if keepout_enabled (face) then
					keepout.top    := get_keepout_objects (device_electric, TOP);
				end if;

				face := BOTTOM;
				if keepout_enabled (face) then
					keepout.bottom := get_keepout_objects (device_electric, BOTTOM);
				end if;
				
			else -- non-electrical device
				if keepout_enabled (face) then
					keepout.top    := get_keepout_objects (device_non_electric, TOP);
				end if;

				face := BOTTOM;
				if keepout_enabled (face) then
					keepout.bottom := get_keepout_objects (device_non_electric, BOTTOM);
				end if;
			end if;

			draw;
		end draw_keepout;


		
	-- STOPMASK
		
		procedure draw_stop_mask is 
			use et_stopmask;
			use et_stopmask.packages;
			
			use pac_stop_lines;
			use pac_stop_arcs;
			use pac_stop_circles;
			use pac_stop_zones;
			use pac_stop_texts;

			face : type_face := TOP;
			stopmask : type_stopmask_both_sides;


			procedure draw is

				procedure query_line (c : in pac_stop_lines.cursor) is
					line : type_stop_line renames element (c);
				begin
					-- The line has already been moved, flipped and rotated
					-- to the final position. So we do not pass the 
					-- position and rotation of the package.
					-- Likewise there is no need to mirror anything here:
					draw_line (
						line		=> line,
						width		=> line.width,
						do_stroke	=> true);
					
				end query_line;

				
				procedure query_arc (c : in pac_stop_arcs.cursor) is
					arc : type_stop_arc renames element (c);
				begin
					-- See comments in procedure query_line.
					draw_arc (
						arc			=> arc,
						width		=> arc.width,
						do_stroke	=> true);
				end query_arc;

				
				procedure query_circle (c : in pac_stop_circles.cursor) is
					circle : type_stop_circle renames element (c);
				begin
					-- See comments in procedure query_line.
					draw_circle (
						circle		=> circle,
						filled		=> NO,
						width		=> circle.width,
						do_stroke	=> true);
				end query_circle;
				
				
				procedure query_zone (c : pac_stop_zones.cursor) is
					zone : type_stop_zone renames element (c);
				begin
					draw_contour (
						contour	=> zone,
						filled	=> YES,
						width	=> zero);
				end query_zone;
				

				procedure query_text (c : pac_stop_texts.cursor) is
					use pac_draw_text;
				begin
					draw_vector_text (element (c));
				end query_text;

				
			begin
				-- top
				set_color_stop_mask (TOP, brightness);
				stopmask.top.lines.iterate (query_line'access);
				stopmask.top.arcs.iterate (query_arc'access);
				stopmask.top.circles.iterate (query_circle'access);
				stopmask.top.zones.iterate (query_zone'access);
				stopmask.top.texts.iterate (query_text'access);

				-- bottom
				set_color_stop_mask (BOTTOM, brightness);
				stopmask.bottom.lines.iterate (query_line'access);
				stopmask.bottom.arcs.iterate (query_arc'access);
				stopmask.bottom.circles.iterate (query_circle'access);
				stopmask.bottom.zones.iterate (query_zone'access);
				stopmask.bottom.texts.iterate (query_text'access);
			end draw;
			
			
		begin -- draw_stop_mask
			if electric then
				if stop_mask_enabled (face) then
					stopmask.top := get_stopmask_objects (device_electric, TOP);
				end if;

				face := BOTTOM;
				if stop_mask_enabled (face) then
					stopmask.bottom := get_stopmask_objects (device_electric, BOTTOM);
				end if;

				
			else -- non-electrical device
				if stop_mask_enabled (face) then
					stopmask.top := get_stopmask_objects (device_non_electric, TOP);
				end if;

				face := BOTTOM;
				if stop_mask_enabled (face) then
					stopmask.bottom := get_stopmask_objects (device_non_electric, BOTTOM);
				end if;
			end if;

			draw;		
		end draw_stop_mask;


		
		
	-- STENCIL / SOLDER CREAM MASK

		procedure draw_stencil is 
			use et_stencil;
			use pac_stencil_lines;
			use pac_stencil_arcs;
			use pac_stencil_circles;
			use pac_stencil_zones;
			stencil : type_stencil_both_sides;
			face : type_face := TOP;


			procedure draw is

				procedure query_line (c : in pac_stencil_lines.cursor) is
					line : type_stencil_line renames element (c);
				begin
					-- The line has already been moved, flipped and rotated
					-- to the final position. So we do not pass the 
					-- position and rotation of the package.
					-- Likewise there is no need to mirror anything here:
					draw_line (
						line		=> line,
						width		=> line.width,
						do_stroke	=> true);
				end query_line;

				
				procedure query_arc (c : in pac_stencil_arcs.cursor) is
					arc : type_stencil_arc renames element (c);
				begin
					-- See comments in procedure query_line.
					draw_arc (
						arc			=> arc,
						width		=> arc.width,
						do_stroke	=> true);
				end query_arc;

				
				procedure query_circle (c : in pac_stencil_circles.cursor) is
					circle : type_stencil_circle renames element (c);
				begin
					-- See comments in procedure query_line.
					draw_circle (
						circle		=> circle,
						filled		=> NO,
						width		=> circle.width,
						do_stroke	=> true);
				end query_circle;
				
				
				procedure query_zone (c : pac_stencil_zones.cursor) is
					zone : type_stencil_zone renames element (c);
				begin
					draw_contour (
						contour	=> zone,
						filled	=> YES,
						width	=> zero);					
				end query_zone;
				
				
			begin
				-- top
				set_color_stencil (TOP, brightness);
				stencil.top.lines.iterate (query_line'access);
				stencil.top.arcs.iterate (query_arc'access);
				stencil.top.circles.iterate (query_circle'access);
				stencil.top.zones.iterate (query_zone'access);

				-- bottom
				set_color_stencil (BOTTOM, brightness);
				stencil.bottom.lines.iterate (query_line'access);
				stencil.bottom.arcs.iterate (query_arc'access);
				stencil.bottom.circles.iterate (query_circle'access);
				stencil.bottom.zones.iterate (query_zone'access);
			end draw;

			
		begin -- draw_stencil
			if electric then
				if stencil_enabled (face) then
					stencil.top := get_stencil_objects (device_electric, TOP);
				end if;

				face := BOTTOM;
				if stencil_enabled (face) then
					stencil.bottom := get_stencil_objects (device_electric, BOTTOM);
				end if;

				
			else -- non-electrical device
				if stencil_enabled (face) then
					stencil.top := get_stencil_objects (device_non_electric, TOP);
				end if;

				face := BOTTOM;
				if stencil_enabled (face) then
					stencil.bottom := get_stencil_objects (device_non_electric, BOTTOM);
				end if;
			end if;

			draw;			
		end draw_stencil;


		

		-- Translates face (TOP/BOTTOM) to conductor layer 1/bottom_layer.
		function face_to_layer (f : in type_face) return type_signal_layer is begin
			case f is
				when TOP => return type_signal_layer'first;
				when BOTTOM => return bottom_layer;
			end case;
		end face_to_layer;



		
	-- ROUTE RESTRICT
		
		procedure draw_route_restrict is 
			use et_route_restrict;
			use et_route_restrict.packages;
			
			use pac_route_restrict_lines;
			use pac_route_restrict_arcs;
			use pac_route_restrict_circles;
			use pac_route_restrict_zones;
			use pac_route_restrict_cutouts;
		
			objects : type_one_side;
			layer : type_signal_layer;			

			
			procedure draw is

				procedure query_line (c : in pac_route_restrict_lines.cursor) is
					line : type_route_restrict_line renames element (c);
				begin
					-- The line has already been moved, flipped and rotated
					-- to the final position. So we do not pass the 
					-- position and rotation of the package.
					-- Likewise there is no need to mirror anything here:
					draw_line (
						line		=> line,
						width		=> zero,
						do_stroke	=> true);
				end query_line;

				
				procedure query_arc (c : in pac_route_restrict_arcs.cursor) is
					arc : type_route_restrict_arc renames element (c);
				begin
					-- See comments in procedure query_line.
					draw_arc (
						arc			=> arc,
						width		=> zero,
						do_stroke	=> true);
				end query_arc;

				
				procedure query_circle (c : in pac_route_restrict_circles.cursor) is
					circle : type_route_restrict_circle renames element (c);
				begin
					-- See comments in procedure query_line.
					draw_circle (
						circle		=> circle,
						filled		=> NO,
						width		=> zero,
						do_stroke	=> true);
				end query_circle;

				
				procedure query_zone (c : in pac_route_restrict_zones.cursor) is
					zone : type_route_restrict_zone renames element (c);
				begin
					draw_contour (
						contour	=> zone,
						filled	=> YES,
						width	=> zero);
				end query_zone;

				
				procedure query_cutout (c : in pac_route_restrict_cutouts.cursor) is
					cutout : type_route_restrict_cutout renames element (c);
				begin					
					draw_contour (
						contour	=> cutout,
						filled	=> NO,
						width	=> zero);
				end query_cutout;

				
			begin
				objects.lines.iterate (query_line'access);
				objects.arcs.iterate (query_arc'access);
				objects.circles.iterate (query_circle'access);

				objects.zones.iterate (query_zone'access);
				objects.cutouts.iterate (query_cutout'access);				
			end draw;
			
			
		begin -- draw_route_restrict
			set_color_route_restrict (brightness);
			-- The color is in all restrict layers the same.

			if electric then
				layer := face_to_layer (TOP);
				if route_restrict_layer_enabled (layer) then
					objects := get_route_restrict_objects (device_electric, OUTER_TOP);
					draw;
				end if;

				layer := face_to_layer (BOTTOM);
				if route_restrict_layer_enabled (layer) then
					objects := get_route_restrict_objects (device_electric, OUTER_BOTTOM);
					draw;
				end if;

			else
				layer := face_to_layer (TOP);
				if route_restrict_layer_enabled (layer) then
					objects := get_route_restrict_objects (device_non_electric, OUTER_TOP);
					draw;
				end if;

				layer := face_to_layer (BOTTOM);
				if route_restrict_layer_enabled (layer) then
					objects := get_route_restrict_objects (device_non_electric, OUTER_BOTTOM);
					draw;
				end if;
			end if;					
		end draw_route_restrict;



		
	-- VIA RESTRICT

		procedure draw_via_restrict is 
			use et_via_restrict;
			use et_via_restrict.packages;

			use pac_via_restrict_zones;
			use pac_via_restrict_cutouts;
			
			objects : type_one_side;
			layer : type_signal_layer;			

			
			procedure draw is

				procedure query_zone (c : in pac_via_restrict_zones.cursor) is
					zone : type_via_restrict_zone renames element (c);
				begin
					draw_contour (
						contour	=> zone,
						filled	=> YES,
						width	=> zero);
				end query_zone;
				

				procedure query_cutout (c : in pac_via_restrict_cutouts.cursor) is
					cutout : type_via_restrict_cutout renames element (c);
				begin					
					draw_contour (
						contour	=> cutout,
						filled	=> NO,
						width	=> zero);
				end query_cutout;
				
			begin
				objects.zones.iterate (query_zone'access);
				objects.cutouts.iterate (query_cutout'access);				
			end draw;
			
			
		begin -- draw_via_restrict
			set_color_via_restrict (brightness);
			-- The color is in all restrict layers the same.
			
			if electric then
				layer := face_to_layer (TOP);
				if via_restrict_layer_enabled (layer) then
					objects := get_via_restrict_objects (device_electric, OUTER_TOP);
					draw;
				end if;

				layer := face_to_layer (BOTTOM);
				if via_restrict_layer_enabled (layer) then
					objects := get_via_restrict_objects (device_electric, OUTER_BOTTOM);
					draw;
				end if;

			else
				layer := face_to_layer (TOP);
				if via_restrict_layer_enabled (layer) then
					objects := get_via_restrict_objects (device_non_electric, OUTER_TOP);
					draw;
				end if;

				layer := face_to_layer (BOTTOM);
				if via_restrict_layer_enabled (layer) then
					objects := get_via_restrict_objects (device_non_electric, OUTER_BOTTOM);
					draw;
				end if;
			end if;					
		end draw_via_restrict;


		
		
	-- PCB HOLES

		procedure draw_holes is 
			use et_pcb_contour;
			use pac_holes;
			
			holes : pac_holes.list;
			
			procedure query_hole (
				c : pac_holes.cursor) 
			is begin
				-- The hole contours have already been moved, 
				-- flipped and rotated
				-- to the final position. So we do not pass the 
				-- position and rotation of the package.
				-- Likewise there is no need to mirror anything here:
				draw_contour (
					contour	=> element (c),
					filled	=> NO,
					width	=> zero);

			end query_hole;
				
			
		begin
			if board_contour_enabled then
				set_color_outline;

				if electric then
					holes := get_holes (device_electric);
				else
					holes := get_holes (device_non_electric);
				end if;		

				holes.iterate (query_hole'access);
			end if;
		end draw_holes;



		
		
	-- CONDUCTORS (NON-TERMINAL RELATED, NON-ELECTRICAL !)
		
		procedure draw_conductors is 
			use et_conductor_text.packages;
			use pac_conductor_texts;
			use et_conductor_segment;
			use pac_conductor_lines;
			use pac_conductor_arcs;
			use pac_conductor_circles;

			objects : type_conductor_objects;
			layer : type_signal_layer;

			
			procedure draw is 

				procedure query_line (c : in pac_conductor_lines.cursor) is
					line : type_conductor_line renames element (c);
				begin
					-- The line has already been moved, flipped and rotated
					-- to the final position. So we do not pass the 
					-- position and rotation of the package.
					-- Likewise there is no need to mirror anything here:
					draw_line (
						line		=> line,
						width		=> line.width,
						do_stroke	=> true);
				end query_line;

				
				procedure query_arc (c : in pac_conductor_arcs.cursor) is
					arc : type_conductor_arc renames element (c);
				begin
					-- See comments in procedure query_line.
					draw_arc (
						arc			=> arc,
						width		=> arc.width,
						do_stroke	=> true);
				end query_arc;

				
				procedure query_circle (c : in pac_conductor_circles.cursor) is
					circle : type_conductor_circle renames element (c);
				begin
					-- See comments in procedure query_line.
					draw_circle (
						circle		=> circle,
						filled		=> NO,
						width		=> circle.width,
						do_stroke	=> true);
				end query_circle;

				
				procedure query_text (c : in pac_conductor_texts.cursor) is
					use pac_draw_text;
				begin
					draw_vector_text (element (c));
				end query_text;

				
			begin
				set_color_conductor (layer, brightness);
				objects.lines.iterate (query_line'access);
				objects.arcs.iterate (query_arc'access);
				objects.circles.iterate (query_circle'access);
				objects.texts.iterate (query_text'access);
			end draw;

			
		begin -- draw_conductors
			if electric then
				layer := face_to_layer (TOP);
				if conductor_enabled (layer) then
					objects := get_conductor_objects (device_electric, OUTER_TOP);
					draw;
				end if;

				layer := face_to_layer (BOTTOM);
				if conductor_enabled (layer) then
					objects := get_conductor_objects (device_electric, OUTER_BOTTOM);
					draw;
				end if;

			else
				layer := face_to_layer (TOP);
				if conductor_enabled (layer) then
					objects := get_conductor_objects (device_non_electric, OUTER_TOP);
					draw;
				end if;

				layer := face_to_layer (BOTTOM);
				if conductor_enabled (layer) then
					objects := get_conductor_objects (device_non_electric, OUTER_BOTTOM);
					draw;
				end if;
			end if;		
		end draw_conductors;


		
		
	-- TERMINALS

		function get_stop_mask_expansion return type_stop_mask_expansion is  -- from DRU
			use et_canvas_schematic_2;
			use et_board_ops;
		begin
			return get_pcb_design_rules (active_module).stop_mask.expansion_min;
		end get_stop_mask_expansion;


		
		-- This procedure draws the terminals of the package.
		-- That is: conductor pads, stopmask, stencil, restring, drills, millings:
		procedure draw_terminals is
			use et_terminals;
			use pac_terminals;
			use et_alignment;

			
			procedure query_terminal (
				c : in pac_terminals.cursor) 
			is
				name : constant string := to_string (key (c)); -- H5, 5, 3
				
				t : type_terminal renames element (c);

				
				-- Draws the name of an smt pad.
				-- The given position is the center of the pad
				-- relative to the origin of the package:
				procedure draw_name_smt (
					pad_position	: in type_position)  -- the center of the pad
				is
					use et_text;

					-- Take a copy of the x/y position of the pad:
					pos_tmp : type_vector_model := pad_position.place;

					use pac_draw_text;
				begin
					set_color_terminal_name (brightness);

					-- Rotate the pad POSITION about the origin
					-- of the package by the rotation of the package:
					rotate_by (pos_tmp, get_rotation (package_position));

					-- If the package is to be flipped then
					-- mirror the pad POSITION along the Y-axis:
					if flipped then
						mirror (pos_tmp, MIRROR_ALONG_Y_AXIS);
					end if;

					-- Now move the pad POSITION by the position
					-- of the package:
					add (pos_tmp, package_position.place);

					-- Draw the pad name at pos_tmp:
					draw_text (
						content		=> to_content (name),
						size		=> terminal_name_size,
						font		=> terminal_name_font,
						anchor		=> pos_tmp,
						origin		=> false, -- no origin required
						rotation	=> zero_rotation,
						alignment	=> (ALIGN_CENTER, ALIGN_CENTER));

						-- CS The rotation should be so that the
						-- name can be read from front and from the right.
					
				end draw_name_smt;

				
				
				-- Draws the name of a THT pad if any conductor layer is enabled 
				procedure draw_name_tht (
					pad_position	: in type_position)  -- the center of the pad
				is
					use et_text;

					-- Take a copy of the x/y position of the pad:
					pos_tmp : type_vector_model := pad_position.place;

					use pac_draw_text;
				begin
					if conductors_enabled then

						set_color_terminal_name (brightness);
						
						-- Rotate the pad POSITION about the origin
						-- of the package by the rotation of the package:
						rotate_by (pos_tmp, get_rotation (package_position));

						-- If the package is flipped, then the terminal position
						-- must be mirrored along the Y axis.
						if flipped then 
							mirror (pos_tmp, MIRROR_ALONG_Y_AXIS); 
						end if;
						
						-- Now move the pad POSITION by the position
						-- of the package:
						add (pos_tmp, package_position.place);
						
						-- Draw the pad name at pos_tmp:							
						draw_text (
							content		=> to_content (name),
							size		=> terminal_name_size,
							font		=> terminal_name_font,
							anchor		=> pos_tmp,
							origin		=> false, -- no origin required
							rotation	=> zero_rotation,
							alignment	=> (ALIGN_CENTER, ALIGN_CENTER));

						-- CS The rotation should be so that the
						-- name can be read from front and from the right.
					end if;
				end draw_name_tht;

				
				-- This procedure draws the SMT pad, the stopmask, the stencil and 
				-- the terminal name. The terminal name will be drawn only if
				-- the signal layer is enabled.
				procedure draw_pad_smt (
					pad_contours	: in type_contour; -- the outline of the solder pad (copper)
					stopmask		: in type_stop_mask_smt; -- the stopmask of the pad
					stencil			: in type_stencil_shape; -- the solder cream mask of the pad

					-- The position of the center of the pad (relative to the package position)
					pad_position	: in type_position; -- incl. pad rotation about itself
					f				: in type_face) 
				is
					use et_board_shapes_and_text;
					
					ly : constant type_signal_layer := face_to_layer (f);

					
					-- Draws the solder pad (conductor material)
					-- and the pad name:
					procedure draw_conductor is begin
						if conductor_enabled (ly) then

							set_color_conductor (ly, brightness);

							if flipped then
								draw_contour (
									contour	=> pad_contours,
									pos		=> get_position (package_position),
									offset	=> pad_position,
									filled	=> YES,
									mirror	=> MIRROR_ALONG_Y_AXIS,
									width	=> zero);

							else
								draw_contour (
									contour	=> pad_contours,
									pos		=> get_position (package_position),
									offset	=> pad_position,
									filled	=> YES,
									width	=> zero);

							end if;
												
							-- draw the terminal name
							draw_name_smt (pad_position);
						end if;
					end draw_conductor;
					

					-- Draws the stopmask of the pad:
					procedure draw_stopmask is 
						stopmask_contours	: type_stop_mask_contours;
						polygon_tmp : type_polygon;
					begin
						if stop_mask_enabled (f) then
							
							case stopmask.shape is
								when AS_PAD =>
									-- Copy pad contours to stopmask without
									-- any modification:
									stopmask_contours := (type_contour (pad_contours) with null record);

									
								when EXPAND_PAD =>
									-- Copy pad contours to stopmask:
									stopmask_contours := (type_contour (pad_contours) with null record);

									-- Now the stopmask must be expanded according to the DRU settings.

									-- Make a temporary polygon from the stopmask contours:
									polygon_tmp := to_polygon (stopmask_contours, fill_tolerance, EXPAND);
									-- CS: expand correct ?

									-- Offset the temporary polygon:
									offset_polygon (
										polygon		=> polygon_tmp,
										offset		=> type_float (get_stop_mask_expansion)); -- from DRU

									-- Convert the temporary polygon back to a contour:
									stopmask_contours := (to_contour (polygon_tmp) with null record);
									
									
								when USER_SPECIFIC =>
									-- Set the stopmask contour as given by the user settings:
									stopmask_contours := stopmask.contours;
							end case;

							
							set_color_stop_mask (f, brightness);

							-- Draw the stopmask contour:
							if flipped then
								
								draw_contour (
									contour	=> stopmask_contours,
									pos		=> get_position (package_position),
									offset	=> pad_position,
									filled	=> YES,
									mirror	=> MIRROR_ALONG_Y_AXIS,
									width	=> zero);

							else

								draw_contour (
									contour	=> stopmask_contours,
									pos		=> get_position (package_position),
									offset	=> pad_position,
									filled	=> YES,
									width	=> zero);

							end if;
							
						end if;
					end draw_stopmask;
					

					-- Draws the stencil (or solder paste mask) of the pad:					
					procedure draw_stencil is 
						stencil_contours : type_stencil_contours;
						polygon_tmp : type_polygon;
					begin
						if stencil_enabled (f) then

							case stencil.shape is
								
								when AS_PAD =>
									-- Copy pad contours to stencil without
									-- any modification:
									stencil_contours := (type_contour (pad_contours) with null record);

									
								when SHRINK_PAD =>
									-- Copy pad contours to stencil:
									stencil_contours := (type_contour (pad_contours) with null record);

									-- Now the stencil must be shrinked according to shrink_factor:
									
									-- Make a temporary polygon from the stencil contour
									polygon_tmp := to_polygon (stencil_contours, fill_tolerance, EXPAND);
									-- CS: expand correct ?
									
									--scale_polygon (
										--polygon		=> stencil_contours,
										--scale		=> stencil.shrink_factor);

									-- Offset the temporary polygon
									offset_polygon (
										polygon		=> polygon_tmp,
										offset		=> type_float (stencil.shrink_factor));

									-- Convert the temporary polygon back to a contour:
									stencil_contours := (to_contour (polygon_tmp) with null record);
									
									
								when USER_SPECIFIC =>
									-- Set the stencil contour as given by the user settings:
									stencil_contours := stencil.contours;
									
							end case;

							
							set_color_stencil (f, brightness);


							-- Draw the stencil contours:
							if flipped then
								
								draw_contour (
									contour	=> stencil_contours,
									pos		=> get_position (package_position),
									offset	=> pad_position,
									filled	=> YES,
									mirror	=> MIRROR_ALONG_Y_AXIS,
									width	=> zero);

							else

								draw_contour (
									contour	=> stencil_contours,
									pos		=> get_position (package_position),
									offset	=> pad_position,
									filled	=> YES,
									width	=> zero);

							end if;
							
						end if;
					end draw_stencil;
					
					
				begin
					-- We draw only if either the signal layer, the stop mask or the stencil
					-- is enabled. Otherwise nothing will happen here:
					if conductor_enabled (ly) or stop_mask_enabled (f) or stencil_enabled (f) then

						if f = face then
							draw_conductor;							
							draw_stopmask;							
							draw_stencil;							
						end if;
					end if;
				end draw_pad_smt;
				
				
				-- This procedure draws the outer contour of the THT pad and 
				-- th outer contour of the stopmask
				-- in top/bottom signal layer (specified by caller).
				procedure tht_outer_layer (
					pad_contours	: in type_contour; -- the outline of the solder pad
					stopmask		: in et_terminals.type_stop_mask; -- the stopmask in the outer layer
					pad_position	: in type_position; -- the center of the pad incl. its rotation
					f				: in type_face;
					drilled_milled	: in type_terminal_tht_hole;
					drill_size		: in type_drill_size := type_drill_size'first;
					hole_contours	: in type_contour := plated_millings_default)
				is
					use et_board_shapes_and_text;					
										
					ly : constant type_signal_layer := face_to_layer (f);

					
					procedure draw_conductor is
						c : type_circle;
					begin
						if conductor_enabled (ly) then
							set_color_tht_pad (brightness);

							--put_line ("draw_conductor");
							
							case drilled_milled is
								when DRILLED =>									
									set_center (c, pad_position.place);
									set_radius (c, drill_size * 0.5);
									
									---put_line ("pad_pos" & to_string (pad_position));

									if flipped then
										draw_contour_with_circular_cutout (
											outer_border	=> pad_contours,
											inner_border	=> c,
											pos				=> get_position (package_position),
											offset			=> pad_position,
											mirror			=> MIRROR_ALONG_Y_AXIS);

									else
										draw_contour_with_circular_cutout (
											outer_border	=> pad_contours,
											inner_border	=> c,
											pos				=> get_position (package_position),
											offset			=> pad_position);
									end if;

									
								when MILLED =>

									if flipped then
										draw_contour_with_arbitrary_cutout (
											outer_border	=> pad_contours,
											inner_border	=> hole_contours,
											pos				=> get_position (package_position),
											offset			=> pad_position,
											mirror			=> MIRROR_ALONG_Y_AXIS);

									else
										draw_contour_with_arbitrary_cutout (
											outer_border	=> pad_contours,
											inner_border	=> hole_contours,
											pos				=> get_position (package_position),
											offset			=> pad_position);
											
									end if;
										
							end case;
						end if;
					end draw_conductor;


					procedure draw_stopmask is
						stopmask_contours : type_stop_mask_contours;
						polygon_tmp : type_polygon;
					begin
						if stop_mask_enabled (f) then

							-- put_line ("draw_stopmask");
							
							case stopmask.shape is
								when AS_PAD =>
									-- Copy solder pad contours to stopmask without
									-- any modifications:
									stopmask_contours := (type_contour (pad_contours) with null record);

									
								when EXPAND_PAD =>
									-- Copy solder pad contour to stopmask:
									stopmask_contours := (type_contour (pad_contours) with null record);

									-- Make a temporary polygon from the stopmask contours:
									polygon_tmp := to_polygon (stopmask_contours, fill_tolerance, EXPAND);
									-- CS: expand correct ?
									
									-- Expand the polygon according to DRU settings:
									offset_polygon (
										polygon		=> polygon_tmp,
										offset		=> type_float (get_stop_mask_expansion));

									-- Convert the temporary polygon back to a contour:
									stopmask_contours := (to_contour (polygon_tmp) with null record);
									
									
								when USER_SPECIFIC =>										
									-- Use the stopmask contours as given by the user:
									stopmask_contours := stopmask.contours;

							end case;

							set_color_stop_mask (f, brightness);
							
							-- Draw the outer contour of the stopmask opening:
							if flipped then
								draw_contour (
									contour		=> stopmask_contours,
									pos			=> get_position (package_position),
									offset		=> pad_position,
									filled		=> YES,
									width		=> zero,
									mirror		=> MIRROR_ALONG_Y_AXIS);
							else
								draw_contour (
									contour		=> stopmask_contours,
									pos			=> get_position (package_position),
									offset		=> pad_position,
									filled		=> YES,
									width		=> zero);
							end if;
						end if;

					end draw_stopmask;

					
				begin
					-- We draw only if either the signal layer or the stop mask
					-- is enabled. Otherwise nothing will happen here:
					if conductor_enabled (ly) or stop_mask_enabled (f) then
						
						if f = face then
							draw_conductor;
							draw_stopmask;
							-- NOTE: THT pads do not have a stencil opening !
						end if;

					end if;
				end tht_outer_layer;

				
				-- This procedure draws the pad contour of a milled THT pad
				-- in an inner conductor layer
				-- if any inner conductor layer is enabled. If no inner conductor
				-- layer is enabled, nothing happens.
				-- The pad contour is derived from the given hole contours:
				procedure tht_inner_layer_milled (
					hole_contours	: in type_contour; -- the contours of the milled hole
					restring_width	: in type_track_width;
					pad_position	: in type_position) -- the center of the pad incl. its rotation
				is
					use et_board_shapes_and_text;
					
					polygon_tmp : type_polygon;
					pad_contours : type_contour;
				begin
					if inner_conductors_enabled (bottom_layer) then

						-- Make a temporary polygon from the hole contours:
						polygon_tmp := to_polygon (hole_contours, fill_tolerance, EXPAND);
						-- CS: expand correct ?

						-- Offset the polygon so that it extends the given hole outline 
						-- by the restring_width:
						offset_polygon (
							polygon		=> polygon_tmp, 
							offset		=> type_float (restring_width));

						-- convert the temporary polygon back to a contour
						pad_contours := to_contour (polygon_tmp);

						if flipped then
							draw_contour_with_arbitrary_cutout (
								outer_border	=> pad_contours,
								inner_border	=> hole_contours,								   
								pos				=> get_position (package_position),
								offset			=> pad_position,
								mirror			=> MIRROR_ALONG_Y_AXIS);
						else
							draw_contour_with_arbitrary_cutout (
								outer_border	=> pad_contours,
								inner_border	=> hole_contours,								   
								pos				=> get_position (package_position),
								offset			=> pad_position);
						end if;
		
					end if;
				end tht_inner_layer_milled;

				
				-- This procedure draws the pad contour of a drilled THT pad
				-- in an inner conductor layer
				-- if any inner conductor layer is enabled. If no inner conductor
				-- layer is enabled, nothing happens.
				procedure tht_inner_layer_drilled (
					drill_size		: in type_drill_size;
					restring		: in type_restring_width;
					pad_position	: in type_position) -- the center of the pad, rotation has no meaning
				is
					circle : type_circle;
					mirror_style : type_mirror := MIRROR_NO;
					
				begin
					if inner_conductors_enabled (bottom_layer) then
						
						-- Build a circle that represents
						-- the restring of inner layers:
						set_center (circle, pad_position.place);
						set_radius (circle, (drill_size + restring) * 0.5);

						if flipped then
							mirror_style := MIRROR_ALONG_Y_AXIS;
						end if;
						
						set_color_tht_pad (brightness);

						-- Draw the restring:
						draw_circle (
							circle		=> circle, 
							pos			=> get_position (package_position), 
							filled		=> NO,
							width		=> restring,
							mirror		=> mirror_style,
							do_stroke	=> true);


						-- CS
						
						-- Draw the hole:
						-- set_color_background;
						
						-- The cutout area must clear out the outer area:
						-- set_operator (context, CAIRO_OPERATOR_CLEAR);

						-- circle.radius := drill_size * 0.5;
      
						-- draw_circle (
						-- 	circle		=> circle, 
						-- 	pos			=> get_position (package_position), 
						-- 	filled		=> YES,
						-- 	width		=> zero,
						-- 	mirror		=> mirror_style,
						-- 	do_stroke	=> true);


						-- restore default compositing operator:
						-- set_operator (context, CAIRO_OPERATOR_OVER);		
					end if;
				end tht_inner_layer_drilled;
				

			begin -- query_terminal

 				-- The terminal can be a through-hole type (THT) or a pad for surface mounting (SMT):
				case t.technology is
					
					when THT =>

						-- The pad can have a circular hole or a hole of arbitrary shape:
						case t.tht_hole is

							when DRILLED => -- circlular hole

								-- Draw top side:
								set_destination;

								-- Draw the conductor shape and the stopmask opening:
								tht_outer_layer (
									pad_contours	=> t.pad_shape_tht.top,
									stopmask		=> t.stop_mask_shape_tht.top,
									pad_position	=> t.position,
									f				=> destination,
									drilled_milled	=> t.tht_hole,
									drill_size		=> t.drill_size);

								
								-- Draw the bottom side:
								set_destination (INVERSE);

								-- Draw the conductor shape and the stopmask opening:
								tht_outer_layer (
									pad_contours	=> t.pad_shape_tht.bottom,
									stopmask		=> t.stop_mask_shape_tht.bottom,
									pad_position	=> t.position,
									f				=> destination,
									drilled_milled	=> t.tht_hole,
									drill_size		=> t.drill_size);

								
								-- draw pad outline of inner layer:
								tht_inner_layer_drilled (
									drill_size		=> t.drill_size,
									restring		=> t.width_inner_layers,
									pad_position	=> t.position);

								
								-- Draw the name of the terminal:
								draw_name_tht (t.position);

									
							when MILLED => -- arbitrary shape or so called plated millings

								-- draw pad outline of top layer:
								set_destination;
								
								tht_outer_layer (
									pad_contours	=> t.pad_shape_tht.top,
									stopmask		=> t.stop_mask_shape_tht.top,
									pad_position	=> t.position,
									f				=> destination,
									drilled_milled	=> t.tht_hole,
									hole_contours	=> t.millings);

								
								-- draw pad outline of bottom layer:
								set_destination (INVERSE);
								
								tht_outer_layer (
									pad_contours	=> t.pad_shape_tht.bottom,
									stopmask		=> t.stop_mask_shape_tht.bottom,
									pad_position	=> t.position,
									f				=> destination,
									drilled_milled	=> t.tht_hole,
									hole_contours	=> t.millings);
								
								-- draw pad outline of inner layer:
								tht_inner_layer_milled (
									hole_contours	=> t.millings,
									restring_width	=> t.width_inner_layers,
									pad_position	=> t.position);

								draw_name_tht (t.position);
						end case;

						
					when SMT =>
						case t.face is
							when TOP	=> set_destination;								
							when BOTTOM	=> set_destination (INVERSE);
						end case;

						draw_pad_smt (t.pad_shape_smt, 
							t.stop_mask_shape_smt, t.stencil_shape, t.position, destination);
				end case;
				
			end query_terminal;


				
			package_cursor : et_packages.pac_package_models.cursor;

			use et_pcb;
			use pac_devices_non_electric;
			use et_schematic;
		begin
			-- locate the package model in the package library:
			if electric then
				package_cursor := get_package_model (device_electric);
			else
				package_cursor := get_package_model (device_non_electric);
			end if;
			
			element (package_cursor).terminals.iterate (query_terminal'access);
		end draw_terminals;


		
		procedure draw_package_origin is begin
			if face = get_face (package_position) then
				if device_origins_enabled (get_face (package_position)) then
					--put_line ("draw origin" & to_string (get_place (package_position));		
					
					set_color_origin (brightness);
					draw_origin ((package_position.place, 0.0));
  
				end if;
			end if;
		end draw_package_origin;

		
		
	begin -- draw_package
		-- put_line ("draw_package");

		if electric then
			package_position := get_position (device_electric);
		else
			package_position := get_position (device_non_electric);
		end if;
		
 		draw_conductors; -- NON-TERMINAL RELATED, NON-ELECTRICAL
		draw_terminals; -- pins, pads, plated millings
		
		draw_stop_mask; -- non-terminal related
		draw_stencil; -- non-terminal related

		draw_silkscreen;
		draw_assembly_documentation;
		draw_keepout; 

		draw_route_restrict;
		draw_via_restrict;
		
		draw_holes;
		
		draw_package_origin;
	end draw_package;



	

	
	use et_schematic;


	-- Draws the packages of electrical devices:
	procedure query_electrical_devices (
		module_name	: in pac_module_name.bounded_string;
		module		: in type_generic_module) 
	is
		use et_modes.board;
		use et_canvas_board_devices;
		use pac_devices_sch;

		
		procedure query_device (
			device_cursor : in pac_devices_sch.cursor) 
		is
			device : type_device_sch renames element (device_cursor);
			use et_pcb;

			brightness : type_brightness := NORMAL;
			
			-- Draws the device package at the position as it is in the board:
			procedure draw_fixed is begin
				draw_package (
					electric			=> true,
					device_electric		=> device_cursor,
					device_non_electric	=> pac_devices_non_electric.no_element,
					flip				=> device.flipped,
					brightness			=> brightness);
			end draw_fixed;


			-- Draws the device packae at the position given by the 
			-- pointer or the cursor:
			procedure draw_being_moved is 
				-- If the device is being moved, then the x/y position
				-- will be overwritten by the position of the mouse or the cursor.

				use et_device_name;

				
				procedure set_position (
					name	: in type_device_name;
					device	: in out type_device_sch)
				is 
					use et_canvas_tool;
				begin
					case object_tool is
						when MOUSE =>
							device.position.place := snap_to_grid (get_mouse_position);

						when KEYBOARD =>
							device.position.place := get_cursor_position;
					end case;
				end set_position;

				-- Create a temporary map of devices. 
				-- This map will contain just a single device:
				m_tmp : pac_devices_sch.map;
				dev_copy : pac_devices_sch.cursor;
			begin
				-- Insert a copy of the candidate device in the temporary map:
				m_tmp.insert (key (device_cursor), element (device_cursor));
				dev_copy := m_tmp.last;

				-- Assign the new position to the copy:
				m_tmp.update_element (dev_copy, set_position'access);

				-- Draw the copy of the candidate device:
				draw_package (
					electric			=> true,
					device_electric		=> dev_copy,
					device_non_electric	=> pac_devices_non_electric.no_element,
					flip				=> device.flipped,
					brightness			=> brightness);

			end draw_being_moved;
			
			
		begin
			-- If the device candidate is selected, then we will
			-- draw it highlighted:
			if is_selected (device_cursor) then
				brightness := BRIGHT;

				if is_moving (device_cursor) then
					draw_being_moved;
				else
					draw_fixed;						
				end if;

			else
				draw_fixed;
			end if;
			
			-- CS live update ratsnest
		end query_device;

		
	begin
		module.devices.iterate (query_device'access);
	end query_electrical_devices;


	

	-- Draws the packages of non-electrical devices:
	procedure query_non_electrical_devices (
		module_name	: in pac_module_name.bounded_string;
		module		: in type_generic_module) 
	is
		use et_pcb;
		use pac_devices_non_electric;


		procedure query_device (
			device_cursor : in pac_devices_non_electric.cursor) 
		is 
			device : type_device_non_electric renames element (device_cursor);

			use et_modes.board;
			use et_canvas_board_devices;
			
			brightness : type_brightness := NORMAL;

			
			-- Draws the device package at the position as it is in the board:
			procedure draw_fixed is 
			begin
				-- put_line (to_string (key (device_cursor)));
				
				draw_package (
					electric			=> false,
					device_electric		=> pac_devices_sch.no_element,
					device_non_electric	=> device_cursor,
					flip				=> device.flipped,
					brightness 			=> brightness);
			end draw_fixed;

			
			-- Draws the device packae at the position given by 
			-- the pointer or the cursor:
			procedure draw_being_moved is 
				-- If the device is being moved, then the x/y position
				-- will be overwritten by the position of the mouse or the cursor.
				
				use et_device_name;
				
				
				procedure set_position (
					name	: in type_device_name;
					device	: in out type_device_non_electric)
				is 
					use et_canvas_tool;
				begin
					case object_tool is
						when MOUSE =>
							device.position.place := snap_to_grid (get_mouse_position);

						when KEYBOARD =>
							device.position.place := get_cursor_position;
					end case;
				end set_position;

				
				-- Create a temporary map of devices. 
				-- This map will contain just a single device:
				m_tmp : pac_devices_non_electric.map;
				
				dev_copy : pac_devices_non_electric.cursor;
			begin
				-- Insert a copy of the candidate device in the temporary map:
				m_tmp.insert (key (device_cursor), element (device_cursor));
				dev_copy := m_tmp.last;

				-- Assign the new position to the copy:
				m_tmp.update_element (dev_copy, set_position'access);

				-- Draw the copy of the candidate device:
				draw_package (
					electric			=> false,
					device_electric		=> pac_devices_sch.no_element,
					device_non_electric	=> dev_copy,
					flip				=> device.flipped,
					brightness			=> brightness);

			end draw_being_moved;

			
		begin
			-- If the device candidate is selected, then we will
			-- draw it highlighted:
			if et_pcb.is_selected (device_cursor) then
				brightness := BRIGHT;

				-- If a move operation is in progress, then the mouse
				case verb is
					when VERB_MOVE =>

						-- If a move operation is in progress, then the mouse
						-- or cursor position overwrites the device position:
						if edit_process_running then
							draw_being_moved;
						else
							draw_fixed;						
						end if;

					-- Other operations leave the device position as it is:
					when others =>
						draw_fixed;
					
				end case;

			else
				draw_fixed;
			end if;		
		end query_device;

		
	begin
		module.devices_non_electric.iterate (query_device'access);
	end query_non_electrical_devices;




	

	package_position_2 : type_package_position;
	
	brightness : type_brightness := NORMAL;
	

	
	procedure draw_package_2 (
		packge 		: in type_package_model; 
		flipped		: in type_flipped)
	is
		-- This flag is set if the package is on the bottom side of the board.
		-- In that case, EVERYTHING (except the origin
		-- and text placeholders) must be mirrored along the Y-axis.
		flip : boolean := false;
		mirror : type_mirror := MIRROR_NO;
		

		
		procedure draw_origin is 

			procedure draw is begin
				--put_line ("draw origin" & to_string (get_place (package_position));				
				set_color_origin (brightness);
				draw_origin ((get_place (package_position_2), 0.0));
			end;

		begin
			if flip then
				if device_origins_enabled (BOTTOM) then
					draw;
				end if;
			else
				if device_origins_enabled (TOP) then
					draw;
				end if;
			end if;
		end draw_origin;


		

		procedure draw_assy is
			use et_assy_doc;
			use pac_doc_lines;

			procedure query_line (c : in pac_doc_lines.cursor) is
				line : type_doc_line renames element (c);
			begin
				draw_line (
					line		=> line,
					pos			=> get_position (package_position_2),		  
					width		=> line.width,
					mirror		=> mirror,
					do_stroke	=> true);
			end query_line;
						
		begin
			if flip then
				if assy_doc_enabled (TOP) then
					set_color_assy_doc (TOP, brightness);
					packge.assy_doc.bottom.lines.iterate (query_line'access);
				end if;

				if assy_doc_enabled (BOTTOM) then
					set_color_assy_doc (BOTTOM, brightness);
					packge.assy_doc.top.lines.iterate (query_line'access);
				end if;

			else
				if assy_doc_enabled (TOP) then
					set_color_assy_doc (TOP, brightness);
					packge.assy_doc.top.lines.iterate (query_line'access);
				end if;

				if assy_doc_enabled (BOTTOM) then
					set_color_assy_doc (BOTTOM, brightness);
					packge.assy_doc.bottom.lines.iterate (query_line'access);
				end if;

			end if;
		end draw_assy;

		


		
		-- This procedure draws the terminals of the package.
		-- It draws:
		-- - the conducting area
		-- - the stopmask opening
		-- - the stencil opening (NOTE: THT pads do not have a stencil opening !)
		-- - the name of the terminal
		procedure draw_terminals is

			use et_terminals;
			use pac_terminals;


			-- Draws a single terminal candidate:
			procedure query_terminal (c : in pac_terminals.cursor) is
				
				-- The name of the terminal (like H5, 5, 3)
				name : constant string := to_string (key (c));
				
				t : type_terminal renames element (c);

				use pac_contours;


			
				-- Translates face (TOP/BOTTOM) to conductor layer 1/bottom_layer.
				function face_to_layer (f : in type_face) return type_signal_layer is begin
					case f is
						when TOP => return type_signal_layer'first;
						when BOTTOM => return bottom_layer;
					end case;
				end face_to_layer;


				

				-- This procedure draws the outer contour of the THT pad:
				procedure tht_outer_layer (
					pad_contours	: in type_contour; -- the outline of the solder pad
					pad_position	: in type_position; -- the center of the pad incl. its rotation
					drilled_milled	: in type_terminal_tht_hole;
					drill_size		: in type_drill_size := type_drill_size'first;
					hole_contours	: in type_contour := plated_millings_default)
				is
					use et_board_shapes_and_text;					

					
					procedure draw_conductor is
						c : type_circle;
						use pac_draw_contours;
					begin
						set_color_tht_pad (brightness);

						--put_line ("draw_conductor");
						
						case drilled_milled is
							when DRILLED =>									
								set_center (c, pad_position.place);
								set_radius (c, drill_size * 0.5);
								
								---put_line ("pad_pos" & to_string (pad_position));

								draw_contour_with_circular_cutout (
									outer_border	=> pad_contours,
									inner_border	=> c,
									pos				=> get_position (package_position_2),
									offset			=> pad_position,
									mirror			=> mirror);

								
							when MILLED =>

								draw_contour_with_arbitrary_cutout (
									outer_border	=> pad_contours,
									inner_border	=> hole_contours,
									pos				=> get_position (package_position_2),
									offset			=> pad_position,
									mirror			=> mirror);

									
						end case;
					end draw_conductor;

					
				begin
					draw_conductor;	
				end tht_outer_layer;

		

				procedure draw_tht_outer_layers_drilled is begin
					-- Draw the conductor shape of outer layers:
					if flip then
						if conductor_enabled (face_to_layer (TOP)) or stop_mask_enabled (TOP) then
						-- CS if conductor_enabled (face_to_layer (TOP)) then ?
							tht_outer_layer (
								pad_contours	=> t.pad_shape_tht.bottom,
								pad_position	=> t.position,
								drilled_milled	=> t.tht_hole,
								drill_size		=> t.drill_size);
						end if;

						if conductor_enabled (face_to_layer (BOTTOM)) or stop_mask_enabled (BOTTOM) then
							tht_outer_layer (
								pad_contours	=> t.pad_shape_tht.top,
								pad_position	=> t.position,
								drilled_milled	=> t.tht_hole,
								drill_size		=> t.drill_size);
						end if;

					else -- no flip
						if conductor_enabled (face_to_layer (TOP)) or stop_mask_enabled (TOP) then
							tht_outer_layer (
								pad_contours	=> t.pad_shape_tht.top,
								pad_position	=> t.position,
								drilled_milled	=> t.tht_hole,
								drill_size		=> t.drill_size);
						end if;

						if conductor_enabled (face_to_layer (BOTTOM)) or stop_mask_enabled (BOTTOM) then
							tht_outer_layer (
								pad_contours	=> t.pad_shape_tht.bottom,
								pad_position	=> t.position,
								drilled_milled	=> t.tht_hole,
								drill_size		=> t.drill_size);
						end if;

					end if;
				end draw_tht_outer_layers_drilled;
				


				-- Draws the conductor shape of outer layers:
				procedure draw_tht_outer_layers_milled is begin
					if flip then
						if conductor_enabled (face_to_layer (TOP)) or stop_mask_enabled (TOP) then
						-- CS if conductor_enabled (face_to_layer (TOP)) then
							tht_outer_layer (
								pad_contours	=> t.pad_shape_tht.bottom,
								pad_position	=> t.position,
								drilled_milled	=> t.tht_hole,
								hole_contours	=> t.millings);
						end if;

						if conductor_enabled (face_to_layer (BOTTOM)) or stop_mask_enabled (BOTTOM) then										
							tht_outer_layer (
								pad_contours	=> t.pad_shape_tht.top,
								pad_position	=> t.position,
								drilled_milled	=> t.tht_hole,
								hole_contours	=> t.millings);

						end if;
						
					else
						if conductor_enabled (face_to_layer (TOP)) or stop_mask_enabled (TOP) then										
							tht_outer_layer (
								pad_contours	=> t.pad_shape_tht.top,
								pad_position	=> t.position,
								drilled_milled	=> t.tht_hole,
								hole_contours	=> t.millings);
						end if;

						if conductor_enabled (face_to_layer (BOTTOM)) or stop_mask_enabled (BOTTOM) then
							tht_outer_layer (
								pad_contours	=> t.pad_shape_tht.bottom,
								pad_position	=> t.position,
								drilled_milled	=> t.tht_hole,
								hole_contours	=> t.millings);
						end if;
					end if;
				end draw_tht_outer_layers_milled;
				
				
				
				-- Draws the pad contour of a drilled THT pad:
				procedure draw_tht_inner_layers_drilled is 

					-- This procedure draws the pad contour of a drilled THT pad
					-- in an inner conductor layer if any inner conductor layer is enabled. 
					-- If no inner conductor layer is enabled, nothing happens.
					procedure do_it (
						drill_size		: in type_drill_size;
						restring		: in type_restring_width;
						pad_position	: in type_position) -- the center of the pad, rotation has no meaning
					is
						circle : type_circle;
						mirror_style : type_mirror := MIRROR_NO;
						
					begin
						if inner_conductors_enabled (bottom_layer) then
							
							-- Build a circle that represents
							-- the restring of inner layers:
							set_center (circle, pad_position.place);
							set_radius (circle, (drill_size + restring) * 0.5);

						
							-- Draw the restring:
							draw_circle (
								circle		=> circle, 
								pos			=> get_position (package_position_2), 
								filled		=> NO,
								width		=> restring,
								mirror		=> mirror,
								do_stroke	=> true);


							-- CS
							
							-- Draw the hole:
							-- set_color_background;
							
							-- The cutout area must clear out the outer area:
							-- set_operator (context, CAIRO_OPERATOR_CLEAR);

							-- circle.radius := drill_size * 0.5;
		
							-- draw_circle (
							-- 	circle		=> circle, 
							-- 	pos			=> get_position (package_position), 
							-- 	filled		=> YES,
							-- 	width		=> zero,
							-- 	mirror		=> mirror_style,
							-- 	do_stroke	=> true);


							-- restore default compositing operator:
							-- set_operator (context, CAIRO_OPERATOR_OVER);		
						end if;
					end do_it;

				begin
					do_it (
						drill_size		=> t.drill_size,
						restring		=> t.width_inner_layers,
						pad_position	=> t.position);
													
				end draw_tht_inner_layers_drilled;



				
				-- Draws the conductor contours of inner layers:
				procedure draw_tht_inner_layers_milled is 

					-- This procedure draws the pad contour of a milled THT pad
					-- in an inner conductor layer
					-- if any inner conductor layer is enabled. If no inner conductor
					-- layer is enabled, nothing happens.
					-- The pad contour is derived from the given hole contours:
					procedure do_it (
						hole_contours	: in type_contour; -- the contours of the milled hole
						restring_width	: in type_track_width;
						pad_position	: in type_position) -- the center of the pad incl. its rotation
					is
						use pac_geometry_2;	
						use pac_contours;
						use pac_polygons;
						use pac_offsetting;

						use pac_draw_contours;
						use et_board_shapes_and_text;
						
						polygon_tmp : type_polygon;
						pad_contours : type_contour;
					begin
						if inner_conductors_enabled (bottom_layer) then

							-- Make a temporary polygon from the hole contours:
							polygon_tmp := to_polygon (hole_contours, fill_tolerance, EXPAND);
							-- CS: expand correct ?

							-- Offset the polygon so that it extends the given hole outline 
							-- by the restring_width:
							offset_polygon (
								polygon		=> polygon_tmp, 
								offset		=> type_float (restring_width));

							-- convert the temporary polygon back to a contour
							pad_contours := to_contour (polygon_tmp);

							draw_contour_with_arbitrary_cutout (
								outer_border	=> pad_contours,
								inner_border	=> hole_contours,								   
								pos				=> get_position (package_position_2),
								offset			=> pad_position,
								mirror			=> mirror);
			
						end if;
					end do_it;


				begin
					do_it (
						hole_contours	=> t.millings,
						restring_width	=> t.width_inner_layers,
						pad_position	=> t.position);
					
				end draw_tht_inner_layers_milled;


				
				
				function get_stop_mask_expansion return type_stop_mask_expansion is  -- from DRU
					use et_canvas_schematic_2;
					use et_board_ops;
				begin
					return get_pcb_design_rules (active_module).stop_mask.expansion_min;
				end get_stop_mask_expansion;

				
				

				

				
				-- Draws the stopmask opening of a THT terminal:
				procedure draw_tht_stop_mask is 

					procedure draw_stopmask (
						pad_contours	: in type_contour; -- the outline of the solder pad
						stopmask		: in et_terminals.type_stop_mask; -- the stopmask in the outer layer
						pad_position	: in type_position) -- the center of the pad incl. its rotation
					is
						
						stopmask_contours : type_stop_mask_contours;
						-- CS initialize (see build_contour)


						-- Builds the stopmask contour from the pad contour:
						procedure build_contour is
							use pac_geometry_2;	
							use pac_contours;
							use pac_polygons;
							use pac_offsetting;

							use et_board_shapes_and_text;

							polygon_tmp : type_polygon;
						begin
							case stopmask.shape is
								when AS_PAD =>
									-- Copy solder pad contours to stopmask without
									-- any modifications:
									stopmask_contours := (type_contour (pad_contours) with null record);

									
								when EXPAND_PAD =>
									-- Copy solder pad contour to stopmask:
									stopmask_contours := (type_contour (pad_contours) with null record);

									-- Make a temporary polygon from the stopmask contours:
									polygon_tmp := to_polygon (stopmask_contours, fill_tolerance, EXPAND);
									-- CS: expand correct ?
									
									-- Expand the polygon according to DRU settings:
									offset_polygon (
										polygon		=> polygon_tmp,
										offset		=> type_float (get_stop_mask_expansion));

									-- Convert the temporary polygon back to a contour:
									stopmask_contours := (to_contour (polygon_tmp) with null record);
									
									
								when USER_SPECIFIC =>										
									-- Use the stopmask contours as given by the user:
									stopmask_contours := stopmask.contours;

							end case;
						end build_contour;

						
					begin
						build_contour;						
						
						-- Draw the outer contour of the stopmask opening:
						pac_draw_contours.draw_contour (
							contour		=> stopmask_contours,
							pos			=> get_position (package_position_2),
							offset		=> pad_position,
							filled		=> YES,
							width		=> zero,
							mirror		=> mirror);

					end draw_stopmask;

					
				begin
					-- put_line ("draw_tht_stop_mask");
					
					if flip then
						if stop_mask_enabled (TOP) then
							set_color_stop_mask (TOP, brightness);
	
							draw_stopmask (
								pad_contours	=> t.pad_shape_tht.bottom,
								stopmask		=> t.stop_mask_shape_tht.bottom,
								pad_position	=> t.position);
						end if;
					
						if stop_mask_enabled (BOTTOM) then
							set_color_stop_mask (BOTTOM, brightness);
	
							draw_stopmask (
								pad_contours	=> t.pad_shape_tht.top,
								stopmask		=> t.stop_mask_shape_tht.top,
								pad_position	=> t.position);
						end if;
								
					else -- not flipped
						if stop_mask_enabled (TOP) then
							set_color_stop_mask (TOP, brightness);
	
							draw_stopmask (
								pad_contours	=> t.pad_shape_tht.top,
								stopmask		=> t.stop_mask_shape_tht.top,
								pad_position	=> t.position);
						end if;
					
						if stop_mask_enabled (BOTTOM) then
							set_color_stop_mask (BOTTOM, brightness);

							draw_stopmask (
								pad_contours	=> t.pad_shape_tht.bottom,
								stopmask		=> t.stop_mask_shape_tht.bottom,
								pad_position	=> t.position);
							
						end if;
					end if;
				end draw_tht_stop_mask;
				


				
				-- Draws the name of a THT pad if any conductor layer is enabled 
				procedure draw_name_tht (
					pad_position : in type_position)  -- the center of the pad
				is
					use et_text;
					use et_alignment;

					-- Take a copy of the x/y position of the pad:
					pos_tmp : type_vector_model := pad_position.place;

					use pac_draw_text;
				begin
					if conductors_enabled then

						set_color_terminal_name (brightness);
						
						-- Rotate the pad POSITION about the origin
						-- of the package by the rotation of the package:
						rotate_by (pos_tmp, get_rotation (package_position_2));

						-- If the package is flipped, then the terminal POSITION
						-- must be mirrored along the Y axis.
						pac_geometry_2.mirror (pos_tmp, mirror); 
						
						-- Now move the pad POSITION by the position
						-- of the package:
						add (pos_tmp, package_position_2.place);
						
						-- Draw the pad name at pos_tmp:							
						draw_text (
							content		=> to_content (name),
							size		=> terminal_name_size,
							font		=> terminal_name_font,
							anchor		=> pos_tmp,
							origin		=> false, -- no origin required
							rotation	=> zero_rotation,
							alignment	=> (ALIGN_CENTER, ALIGN_CENTER));

						-- CS The rotation should be so that the
						-- name can be read from front and from the right.
					end if;
				end draw_name_tht;
				



				-- Draws the name of an smt pad.
				-- The given position is the center of the pad
				-- relative to the origin of the package:
				procedure draw_name_smt (
					pad_position : in type_position)  -- the center of the pad
				is
					use et_text;
					use et_alignment;

					-- Take a copy of the x/y position of the pad:
					pos_tmp : type_vector_model := pad_position.place;

					use pac_draw_text;
				begin
					set_color_terminal_name (brightness);

					-- Rotate the pad POSITION about the origin
					-- of the package by the rotation of the package:
					rotate_by (pos_tmp, get_rotation (package_position_2));

					-- If the package is to be flipped then
					-- mirror the pad POSITION along the Y-axis:
					pac_geometry_2.mirror (pos_tmp, mirror);

					-- Now move the pad POSITION by the position
					-- of the package:
					add (pos_tmp, package_position_2.place);

					-- Draw the pad name at pos_tmp:
					draw_text (
						content		=> to_content (name),
						size		=> terminal_name_size,
						font		=> terminal_name_font,
						anchor		=> pos_tmp,
						origin		=> false, -- no origin required
						rotation	=> zero_rotation,
						alignment	=> (ALIGN_CENTER, ALIGN_CENTER));

						-- CS The rotation should be so that the
						-- name can be read from front and from the right.
					
				end draw_name_smt;
				

				

				-- This procedure draws the SMT pad, the stopmask, the stencil and 
				-- the terminal name. The terminal name will be drawn only if
				-- the signal layer is enabled.
				procedure draw_pad_smt (
					pad_contours	: in type_contour; -- the outline of the solder pad (copper)
					stopmask		: in type_stop_mask_smt; -- the stopmask of the pad
					stencil			: in type_stencil_shape; -- the solder cream mask of the pad

					-- The position of the center of the pad (relative to the package position)
					pad_position	: in type_position; -- incl. pad rotation about itself
					f				: in type_face) -- the face where the pad is
				is
					use pac_draw_contours;
					
	
					
					-- Draws the conductor area of the pad:
					procedure draw_conductor is 

						-- Does the actual drawing of the conductor area:
						procedure do_it is begin
							draw_contour (
								contour	=> pad_contours,
								pos		=> get_position (package_position_2),
								offset	=> pad_position,
								filled	=> YES,
								mirror	=> mirror,
								width	=> zero);
						end do_it;
				

					begin
						if flip then
							if conductor_enabled (face_to_layer (TOP)) then
								set_color_conductor (face_to_layer (TOP), brightness);

								if f = BOTTOM then
									do_it;
								end if;
													
								-- draw the terminal name
								draw_name_smt (pad_position);
							end if;


							if conductor_enabled (face_to_layer (BOTTOM)) then
								set_color_conductor (face_to_layer (BOTTOM), brightness);

								if f = TOP then
									do_it;
								end if;
													
								-- draw the terminal name
								draw_name_smt (pad_position);
							end if;

							
						else -- not flipped

							if conductor_enabled (face_to_layer (TOP)) then
								set_color_conductor (face_to_layer (TOP), brightness);

								if f = TOP then
									do_it;
								end if;
													
								-- draw the terminal name
								draw_name_smt (pad_position);
							end if;


							if conductor_enabled (face_to_layer (BOTTOM)) then
								set_color_conductor (face_to_layer (BOTTOM), brightness);

								if f = BOTTOM then
									do_it;
								end if;
													
								-- draw the terminal name
								draw_name_smt (pad_position);
							end if;
							
						end if;
					end draw_conductor;
					

					
					
					-- Draws the stopmask of the pad:
					procedure draw_stopmask is 
						
						stopmask_contours : type_stop_mask_contours;
						-- CS initialize (see procedure build_contour)
						
						
						-- Builds the stopmask contour from the pad contour:
						procedure build_contour is
							use pac_geometry_2;	
							use pac_contours;
							use pac_polygons;
							use pac_offsetting;

							use et_board_shapes_and_text;

							polygon_tmp : type_polygon;
						begin
							case stopmask.shape is
								when AS_PAD =>
									-- Copy pad contours to stopmask without
									-- any modification:
									stopmask_contours := (type_contour (pad_contours) with null record);

									
								when EXPAND_PAD =>
									-- Copy pad contours to stopmask:
									stopmask_contours := (type_contour (pad_contours) with null record);

									-- Now the stopmask must be expanded according to the DRU settings.

									-- Make a temporary polygon from the stopmask contours:
									polygon_tmp := to_polygon (stopmask_contours, fill_tolerance, EXPAND);
									-- CS: expand correct ?

									-- Offset the temporary polygon:
									offset_polygon (
										polygon		=> polygon_tmp,
										offset		=> type_float (get_stop_mask_expansion)); -- from DRU

									-- Convert the temporary polygon back to a contour:
									stopmask_contours := (to_contour (polygon_tmp) with null record);
									
									
								when USER_SPECIFIC =>
									-- Set the stopmask contour as given by the user settings:
									stopmask_contours := stopmask.contours;
							end case;
						end build_contour;


						
						-- Does the actual drawing of the stopmask contour:
						procedure do_it is begin
							draw_contour (
								contour	=> stopmask_contours,
								pos		=> get_position (package_position_2),
								offset	=> pad_position,
								filled	=> YES,
								mirror	=> mirror,
								width	=> zero);
						end do_it;

						
					begin	
						build_contour;


						if flip then
							if stop_mask_enabled (TOP) then
								set_color_stop_mask (TOP, brightness);

								if f = BOTTOM then
									do_it;
								end if;
							end if;

							
							if stop_mask_enabled (BOTTOM) then
								set_color_stop_mask (BOTTOM, brightness);

								if f = TOP then
									do_it;
								end if;								
							end if;

							
						else -- not flipped
							
							if stop_mask_enabled (TOP) then
								set_color_stop_mask (TOP, brightness);

								if f = TOP then
									do_it;
								end if;								
							end if;

							
							if stop_mask_enabled (BOTTOM) then
								set_color_stop_mask (BOTTOM, brightness);

								if f = BOTTOM then
									do_it;
								end if;								
							end if;
							
						end if;
					end draw_stopmask;


					


					-- Draws the stencil (or solder paste mask) of the pad:					
					procedure draw_stencil is 
						
						stencil_contours : type_stencil_contours;
						-- CS initialize (see build_contour)
						

						-- Builds the stencil contour from the pad contour:
						procedure build_contour is 
							use pac_geometry_2;	
							use pac_contours;
							use pac_polygons;
							use pac_offsetting;
							use et_board_shapes_and_text;

							polygon_tmp : type_polygon;
						begin
							case stencil.shape is
								
								when AS_PAD =>
									-- Copy pad contours to stencil without
									-- any modification:
									stencil_contours := (type_contour (pad_contours) with null record);

									
								when SHRINK_PAD =>
									-- Copy pad contours to stencil:
									stencil_contours := (type_contour (pad_contours) with null record);

									-- Now the stencil must be shrinked according to shrink_factor:
									
									-- Make a temporary polygon from the stencil contour
									polygon_tmp := to_polygon (stencil_contours, fill_tolerance, EXPAND);
									-- CS: expand correct ?
									
									--scale_polygon (
										--polygon		=> stencil_contours,
										--scale		=> stencil.shrink_factor);

									-- Offset the temporary polygon
									offset_polygon (
										polygon		=> polygon_tmp,
										offset		=> type_float (stencil.shrink_factor));

									-- Convert the temporary polygon back to a contour:
									stencil_contours := (to_contour (polygon_tmp) with null record);
									
									
								when USER_SPECIFIC =>
									-- Set the stencil contour as given by the user settings:
									stencil_contours := stencil.contours;
									
							end case;
						end build_contour;
						

						
						-- Does the actual drawing of the contour:
						procedure do_it is begin
							draw_contour (
								contour	=> stencil_contours,
								pos		=> get_position (package_position_2),
								offset	=> pad_position,
								filled	=> YES,
								mirror	=> mirror,
								width	=> zero);
						end do_it;

						
					begin
						build_contour;				

						
						if flip then
							if stencil_enabled (TOP) then
								set_color_stencil (TOP, brightness);

								if f = BOTTOM then
									do_it;
								end if;
							end if;

							
							if stencil_enabled (BOTTOM) then
								set_color_stencil (BOTTOM, brightness);

								if f = TOP then
									do_it;
								end if;								
							end if;

							
						else -- not flipped
							
							if stencil_enabled (TOP) then
								set_color_stencil (TOP, brightness);

								if f = TOP then
									do_it;
								end if;								
							end if;

							
							if stencil_enabled (BOTTOM) then
								set_color_stencil (BOTTOM, brightness);

								if f = BOTTOM then
									do_it;
								end if;								
							end if;
							
						end if;				
					end draw_stencil;
					
					
				begin
					draw_conductor;
					draw_stopmask;
					draw_stencil;
				end draw_pad_smt;


				
				
			begin -- query_terminal

				-- The terminal can be a through-hole type (THT) or a 
				-- pad for surface mounting (SMT):
				case t.technology is
					
					when THT =>

						-- The pad can have a circular hole or a 
						-- hole of arbitrary shape:
						case t.tht_hole is

							when DRILLED => -- circlular hole

								-- Set the color for conductors
								-- of outer an inner layers:
								set_color_tht_pad (brightness);

								draw_tht_outer_layers_drilled;
								draw_tht_inner_layers_drilled;

								draw_tht_stop_mask;

								-- Draw the name of the terminal:
								draw_name_tht (t.position);

									
							when MILLED => -- arbitrary shape of so called "plated millings"

								-- Set the color for conductors
								-- of outer an inner layers:
								set_color_tht_pad (brightness);

								draw_tht_outer_layers_milled;
								draw_tht_inner_layers_milled;
								
								draw_tht_stop_mask;

								-- Draw the name of the terminal:
								draw_name_tht (t.position);
						end case;

						
					when SMT =>

						draw_pad_smt (t.pad_shape_smt, t.stop_mask_shape_smt,
							t.stencil_shape, t.position, t.face);
				end case;
				
			end query_terminal;

			
		begin
			packge.terminals.iterate (query_terminal'access);
		end draw_terminals;

		
		
	begin
		--put_line ("draw_package_2");

		-- Set the "flip" flag if the package is on the backside of the board:
		if get_face (package_position_2) = BOTTOM then
			flip := true;
			mirror := MIRROR_ALONG_Y_AXIS;
		end if;
		
		draw_origin;
		draw_assy;
		
		
--  		draw_conductors; -- NON-TERMINAL RELATED, NON-ELECTRICAL
		draw_terminals; -- pins, pads, plated millings
-- 		
-- 		draw_stop_mask; -- non-terminal related
-- 		draw_stencil; -- non-terminal related
-- 
-- 		draw_assembly_documentation;
-- 		draw_keepout; 
-- 
-- 		draw_route_restrict;
-- 		draw_via_restrict;
-- 		
-- 		draw_holes;
		
	end draw_package_2;


	

	
	use et_device_name;

	


	
	procedure query_electrical_device_2 (
		name	: in type_device_name;
		device	: in type_device_sch)
	is
		-- The cursor to the actual device model:
		use et_device_library;
		device_model_cursor : pac_devices_lib.cursor;

		use et_package_names;
		package_model_name : pac_package_model_file_name.bounded_string;
	begin
		-- put_line ("device " & to_string (name));
		
		-- Get the cursor to the device model:
		device_model_cursor := locate_device (device.model);

		-- Get the name of the package model:
		package_model_name := get_package_model (device_model_cursor, device.variant);

		-- Send the actual package model to the draw procedure:
		draw_package_2 (
			packge	=> pac_package_models.element (package_models, package_model_name),
			flipped	=> device.flipped);
		
	end query_electrical_device_2;



	

	procedure query_non_electrical_device_2 (
		name	: in type_device_name;
		device	: in et_pcb.type_device_non_electric)
	is
		use et_package_names;
		package_model_name : pac_package_model_file_name.bounded_string;
	begin
		-- put_line ("device " & to_string (name));	
		
		-- Send the actual package model to the draw procedure:
		draw_package_2 (
			packge	=> pac_package_models.element (package_models, device.package_model),
			flipped	=> device.flipped);
	end query_non_electrical_device_2;



	

	procedure query_module (
		module_name	: in pac_module_name.bounded_string;
		module		: in type_generic_module)
	is
		debug : boolean := false;
		
		use pac_devices_sch;
		cursor_electrical : pac_devices_sch.cursor := 
			module.devices.first;

		use et_pcb;
		use pac_devices_non_electric;
		cursor_non_electrical : pac_devices_non_electric.cursor := 
			module.devices_non_electric.first;

	begin
		-- Iterate electrical devices:
		if debug then
			put_line (" electrical devices");
		end if;
		
		while has_element (cursor_electrical) loop
			-- If the device is selected then draw it highlighted:
			if is_selected (cursor_electrical) then
				brightness := BRIGHT;
			else
				brightness := NORMAL;
			end if;

			-- Fetch the complete position of the device
			-- (incl. x/y/rotaton/face) from the database:
			package_position_2 := get_position (cursor_electrical);

			if is_moving (cursor_electrical) then
				-- Override package position by tool position:
				package_position_2.place := get_object_tool_position;
			end if;

			query_element (cursor_electrical, query_electrical_device_2'access);
			next (cursor_electrical);
		end loop;


		-- Iterate non-electrical devices:
		if debug then
			put_line (" non electrical devices");
		end if;
		
		while has_element (cursor_non_electrical) loop
			-- If the device is selected then draw it highlighted:
			if is_selected (cursor_non_electrical) then
				brightness := BRIGHT;
			else
				brightness := NORMAL;
			end if;

			-- Fetch the complete position of the device
			-- (incl. x/y/rotaton/face) from the database:
			package_position_2 := get_position (cursor_non_electrical);
			
			if is_moving (cursor_non_electrical) then
				-- Override package position by tool position:
				package_position_2.place := get_object_tool_position;
			end if;
			
			query_element (cursor_non_electrical, query_non_electrical_device_2'access);
			next (cursor_non_electrical);
		end loop;

	end query_module;


	
	
begin -- draw_packages
-- 	put_line ("draw packages ...");

	-- draw electric devices
-- 	pac_generic_modules.query_element (
-- 		position	=> active_module,
-- 		process		=> query_electrical_devices'access);
-- 
-- 	
-- 	-- draw non-electric devices (like fiducials, mounting holes, ...)
-- 	pac_generic_modules.query_element (
-- 		position	=> active_module,
-- 		process		=> query_non_electrical_devices'access);


	pac_generic_modules.query_element (
		position	=> active_module,
		process		=> query_module'access);

	
end draw_packages;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
