------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW PACKAGES                             --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2023                                                --
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

with ada.text_io;				use ada.text_io;

with et_symbols;
with et_devices;

with et_drills;					use et_drills;

with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.packages;	use et_device_placeholders.packages;

with et_packages;				use et_packages;

with et_pcb;
with et_pcb_contour;
with et_pcb_stack;				use et_pcb_stack;

with et_device_query_schematic;		use et_device_query_schematic;
with et_device_query_board;			use et_device_query_board;

with et_display.board;			use et_display.board;
with et_colors;					use et_colors;
with et_design_rules;			use et_design_rules;
with et_text;

with et_conductor_text.packages;

with et_fill_zones;					use et_fill_zones;
with et_fill_zones.packages;		use et_fill_zones.packages;

with et_route_restrict;				
with et_route_restrict.packages;

with et_via_restrict;
with et_via_restrict.packages;

with et_stop_mask;
with et_stop_mask.packages;

with et_stencil;
with et_silkscreen;
with et_assy_doc;
with et_keepout;					

with et_contour_to_polygon;			use et_contour_to_polygon;


separate (et_canvas_board)

procedure draw_packages (
	self    : not null access type_view;
	face	: in type_face) -- the side of the board to be drawn
is

	-- The deepest conductor layer towards bottom is defined by the layer stack:
	bottom_layer : constant type_signal_layer := 
		deepest_conductor_layer (current_active_module);

	
	procedure draw_package (
		electric			: in boolean;
		device_electric		: in et_schematic.pac_devices_sch.cursor;
		device_non_electric	: in et_pcb.pac_devices_non_electric.cursor;					   
		flip				: in et_packages.type_flipped;
		brightness			: in type_brightness)
	is
		package_position : type_package_position;  -- incl. rotation and face

		
		use pac_geometry_2;	
		use pac_contours;
		use pac_polygons;
		use pac_polygon_offsetting;

		use pac_package_models;

		
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
		
	
		
		procedure draw_text_origin (p : in type_point; f : in type_face) is
			line_horizontal : constant type_line_fine := ( -- from left to right
				start_point		=> to_vector (set (x => get_x (p) - pac_text_board.origin_half_size, y => get_y (p))),
				end_point		=> to_vector (set (x => get_x (p) + pac_text_board.origin_half_size, y => get_y (p))));

			line_vertical : constant type_line_fine := ( -- from bottom to top
				start_point		=> to_vector (set (x => get_x (p), y => get_y (p) - pac_text_board.origin_half_size)),
				end_point		=> to_vector (set (x => get_x (p), y => get_y (p) + pac_text_board.origin_half_size)));

		begin -- draw_text_origin
			if device_origins_enabled (f) then

				-- The caller of this procedure has a setting for the color.
				-- So we backup this context setting.
				save (context.cr);
				
				set_color_origin (context.cr);
				set_line_width (context.cr, type_view_coordinate (pac_text_board.origin_line_width));
				draw_line (line_horizontal, pac_text_board.origin_line_width);
				draw_line (line_vertical, pac_text_board.origin_line_width);

				-- Restore context setting of caller. See comment above.
				restore (context.cr);
			end if;
		end draw_text_origin;
		

		
	-- SILKSCREEN
		procedure draw_silkscreen is 
			use et_silkscreen;			
			use pac_silk_lines;
			use pac_silk_arcs;
			use pac_silk_circles;
			use pac_silk_contours;
			use pac_silk_texts;

			face : type_face := TOP;			
			silkscreen_top, silkscreen_bottom : type_silkscreen;

			
			procedure draw is

				procedure query_line (c : in pac_silk_lines.cursor) is 
					line : type_silk_line renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (line.width));
					draw_line (to_line_fine (line), line.width);
				end query_line;

				procedure query_arc (c : in pac_silk_arcs.cursor) is 
					arc : type_silk_arc renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (arc.width));
					draw_arc (to_arc_fine (arc), arc.width);
				end query_arc;

				procedure query_circle (c : in pac_silk_circles.cursor) is 
					circle : type_silk_circle renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (circle.width));
					draw_circle (
						circle	=> circle,
						filled	=> NO,
						width	=> circle.width);
				end query_circle;

				procedure query_contour (c : in pac_silk_contours.cursor) is 
					contour : type_silk_contour renames element (c);
					drawn : boolean := false;
				begin
					set_line_width (context.cr, type_view_coordinate (zero));
					draw_contour (
						contour	=> contour,
						filled	=> YES,
						width	=> zero,
						drawn	=> drawn);
				end query_contour;

				face : type_face := TOP;
				
				procedure query_text (c : in pac_silk_texts.cursor) is 
					text : type_silk_text renames element (c);
				begin
					draw_text_origin (text.position.place, face);
					
					set_line_width (context.cr, type_view_coordinate (text.line_width));
					draw_vector_text (
						text	=> text.vectors,
						width	=> text.line_width);
				end query_text;

				
			begin
				set_color_silkscreen (context.cr, face, brightness);
				silkscreen_top.lines.iterate (query_line'access);
				silkscreen_top.arcs.iterate (query_arc'access);
				silkscreen_top.circles.iterate (query_circle'access);
				silkscreen_top.contours.iterate (query_contour'access);
				silkscreen_top.texts.iterate (query_text'access);

				face := BOTTOM;
				set_color_silkscreen (context.cr, face, brightness);
				silkscreen_bottom.lines.iterate (query_line'access);
				silkscreen_bottom.arcs.iterate (query_arc'access);
				silkscreen_bottom.circles.iterate (query_circle'access);
				silkscreen_bottom.contours.iterate (query_contour'access);
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

		
	-- ASSY DOC
		procedure draw_assembly_documentation is 
			use et_assy_doc;			
			use pac_doc_lines;
			use pac_doc_arcs;
			use pac_doc_circles;
			use pac_doc_contours;
			use pac_doc_texts;

			face : type_face := TOP;

			doc_top, doc_bottom : type_assy_doc;

			
			procedure draw is

				procedure query_line (c : in pac_doc_lines.cursor) is 
					line : type_doc_line renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (line.width));
					draw_line (to_line_fine (line), line.width);
				end query_line;

				procedure query_arc (c : in pac_doc_arcs.cursor) is 
					arc : type_doc_arc renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (arc.width));
					draw_arc (to_arc_fine (arc), arc.width);
				end query_arc;

				procedure query_circle (c : in pac_doc_circles.cursor) is 
					circle : type_doc_circle renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (circle.width));
					draw_circle (
						circle	=> circle,
						filled	=> NO,
						width	=> circle.width);
				end query_circle;

				procedure query_contour (c : in pac_doc_contours.cursor) is 
					contour : type_doc_contour renames element (c);
					drawn : boolean := false;
				begin
					set_line_width (context.cr, type_view_coordinate (zero));
					draw_contour (
						contour	=> contour,
						filled	=> YES,
						width	=> zero,
						drawn	=> drawn);
				end query_contour;

				face : type_face := TOP;
				
				procedure query_text (c : in pac_doc_texts.cursor) is 
					text : type_doc_text renames element (c);
				begin
					draw_text_origin (text.position.place, face);
					
					set_line_width (context.cr, type_view_coordinate (text.line_width));
					draw_vector_text (
						text	=> text.vectors,
						width	=> text.line_width);
				end query_text;
			
				
			begin
				set_color_assy_doc (context.cr, face, brightness);
				doc_top.lines.iterate (query_line'access);
				doc_top.arcs.iterate (query_arc'access);
				doc_top.circles.iterate (query_circle'access);
				doc_top.contours.iterate (query_contour'access);
				doc_top.texts.iterate (query_text'access);

				face := BOTTOM;
				set_color_assy_doc (context.cr, face, brightness);
				doc_bottom.lines.iterate (query_line'access);
				doc_bottom.arcs.iterate (query_arc'access);
				doc_bottom.circles.iterate (query_circle'access);
				doc_bottom.contours.iterate (query_contour'access);
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
				
				procedure query_zone (c : pac_keepout_zones.cursor) is
					drawn : boolean := false;
				begin
					draw_contour (
						contour	=> element (c),
						filled	=> NO,
						width	=> zero,
						drawn	=> drawn);
					
				end query_zone;
				
			begin
				-- top
				set_color_keepout (context.cr, TOP, brightness);
				keepout.top.zones.iterate (query_zone'access);
				-- CS cutouts

				-- bottom
				set_color_keepout (context.cr, BOTTOM, brightness);
				keepout.bottom.zones.iterate (query_zone'access);
				-- CS cutouts
			end draw;

			
		begin -- draw_keepout
			set_line_width (context.cr, type_view_coordinate (keepout_line_width));
			
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


		
	-- STOP MASK
		procedure draw_stop_mask is 
			use et_stop_mask;
			use et_stop_mask.packages;
			
			use pac_stop_lines;
			use pac_stop_arcs;
			use pac_stop_circles;
			use pac_stop_contours;
			use pac_stop_texts;

			face : type_face := TOP;
			stopmask : type_stopmask_both_sides;


			procedure draw is

				procedure query_line (c : in pac_stop_lines.cursor) is
					drawn : boolean := false;
					line : type_stop_line renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (line.width));
					
					draw_line (
						line	=> to_line_fine (line),
						width	=> line.width);
					
				end query_line;

				
				procedure query_arc (c : in pac_stop_arcs.cursor) is
					drawn : boolean := false;
					arc : type_stop_arc renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (arc.width));
					
					draw_arc (
						arc		=> to_arc_fine (arc),
						width	=> arc.width);
					
				end query_arc;

				
				procedure query_circle (c : in pac_stop_circles.cursor) is
					drawn : boolean := false;
					circle : type_stop_circle renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (circle.width));
					
					draw_circle (
						circle	=> circle,
						filled	=> NO,
						width	=> circle.width);
					
				end query_circle;
				
				
				procedure query_contour (c : pac_stop_contours.cursor) is
					drawn : boolean := false;
				begin
					draw_contour (
						contour	=> element (c),
						filled	=> YES,
						width	=> zero,
						drawn	=> drawn);
					
				end query_contour;
				

				procedure query_text (c : pac_stop_texts.cursor) is
					text : type_stop_text renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (text.line_width));
					draw_vector_text (
						text	=> text.vectors,
						width	=> text.line_width);
				end query_text;

				
			begin
				-- top
				set_color_stop_mask (context.cr, TOP, global_scale, brightness);
				stopmask.top.lines.iterate (query_line'access);
				stopmask.top.arcs.iterate (query_arc'access);
				stopmask.top.circles.iterate (query_circle'access);
				stopmask.top.contours.iterate (query_contour'access);
				stopmask.top.texts.iterate (query_text'access);

				-- bottom
				set_color_stop_mask (context.cr, BOTTOM, global_scale, brightness);
				stopmask.bottom.lines.iterate (query_line'access);
				stopmask.bottom.arcs.iterate (query_arc'access);
				stopmask.bottom.circles.iterate (query_circle'access);
				stopmask.bottom.contours.iterate (query_contour'access);
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
			use pac_stencil_contours;
			stencil : type_stencil_both_sides;
			face : type_face := TOP;


			procedure draw is

				procedure query_line (c : in pac_stencil_lines.cursor) is
					drawn : boolean := false;
					line : type_stencil_line renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (line.width));
					
					draw_line (
						line	=> to_line_fine (line),
						width	=> line.width);
					
				end query_line;

				
				procedure query_arc (c : in pac_stencil_arcs.cursor) is
					drawn : boolean := false;
					arc : type_stencil_arc renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (arc.width));
					
					draw_arc (
						arc		=> to_arc_fine (arc),
						width	=> arc.width);
					
				end query_arc;

				
				procedure query_circle (c : in pac_stencil_circles.cursor) is
					drawn : boolean := false;
					circle : type_stencil_circle renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (circle.width));
					
					draw_circle (
						circle	=> circle,
						filled	=> NO,
						width	=> circle.width);
					
				end query_circle;
				
				
				procedure query_contour (c : pac_stencil_contours.cursor) is
					drawn : boolean := false;
				begin
					draw_contour (
						contour	=> element (c),
						filled	=> YES,
						width	=> zero,
						drawn	=> drawn);
					
				end query_contour;
				
				
			begin
				-- top
				set_color_stencil (context.cr, TOP, global_scale, brightness);
				stencil.top.lines.iterate (query_line'access);
				stencil.top.arcs.iterate (query_arc'access);
				stencil.top.circles.iterate (query_circle'access);
				stencil.top.contours.iterate (query_contour'access);

				-- bottom
				set_color_stencil (context.cr, BOTTOM, global_scale, brightness);
				stencil.bottom.lines.iterate (query_line'access);
				stencil.bottom.arcs.iterate (query_arc'access);
				stencil.bottom.circles.iterate (query_circle'access);
				stencil.bottom.contours.iterate (query_contour'access);
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
					set_line_width (context.cr, type_view_coordinate (line.width));
					draw_line (
						line	=> to_line_fine (line),
						width	=> line.width);
				end query_line;

				procedure query_arc (c : in pac_route_restrict_arcs.cursor) is
					arc : type_route_restrict_arc renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (arc.width));
					draw_arc (
						arc		=> to_arc_fine (arc),
						width	=> arc.width);
				end query_arc;

				procedure query_circle (c : in pac_route_restrict_circles.cursor) is
					circle : type_route_restrict_circle renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (circle.width));
					draw_circle (
						circle	=> circle,
						width	=> circle.width, 
						filled	=> NO);
				end query_circle;

				procedure query_zone (c : in pac_route_restrict_zones.cursor) is
					zone : type_route_restrict_zone renames element (c);
					drawn : boolean := false;
				begin
					draw_contour (
						contour	=> zone,
						width	=> route_restrict_line_width, 
						filled	=> YES,
						drawn	=> drawn);
				end query_zone;

				procedure query_cutout (c : in pac_route_restrict_cutouts.cursor) is
					cutout : type_route_restrict_cutout renames element (c);
					drawn : boolean := false;
				begin					
					draw_contour (
						contour	=> cutout,
						width	=> route_restrict_line_width, 
						filled	=> NO,
						drawn	=> drawn);
				end query_cutout;
				
			begin
				objects.lines.iterate (query_line'access);
				objects.arcs.iterate (query_arc'access);
				objects.circles.iterate (query_circle'access);

				set_line_width (context.cr, type_view_coordinate (route_restrict_line_width));
				objects.zones.iterate (query_zone'access);
				objects.cutouts.iterate (query_cutout'access);				
			end draw;
			
			
		begin -- draw_route_restrict
			set_color_route_restrict (context.cr, brightness);
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

			use pac_via_restrict_lines;			
			use pac_via_restrict_arcs;
			use pac_via_restrict_circles;
			use pac_via_restrict_zones;
			use pac_via_restrict_cutouts;
			
			objects : type_one_side;
			layer : type_signal_layer;			

			
			procedure draw is

				procedure query_line (c : in pac_via_restrict_lines.cursor) is
					line : type_via_restrict_line renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (line.width));
					draw_line (
						line	=> to_line_fine (line),
						width	=> line.width);
				end query_line;

				procedure query_arc (c : in pac_via_restrict_arcs.cursor) is
					arc : type_via_restrict_arc renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (arc.width));
					draw_arc (
						arc		=> to_arc_fine (arc),
						width	=> arc.width);
				end query_arc;

				procedure query_circle (c : in pac_via_restrict_circles.cursor) is
					circle : type_via_restrict_circle renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (circle.width));
					draw_circle (
						circle	=> circle,
						width	=> circle.width, 
						filled	=> NO);
				end query_circle;

				procedure query_zone (c : in pac_via_restrict_zones.cursor) is
					zone : type_via_restrict_zone renames element (c);
					drawn : boolean := false;
				begin
					draw_contour (
						contour	=> zone,
						width	=> via_restrict_line_width, 
						filled	=> YES,
						drawn	=> drawn);
				end query_zone;

				procedure query_cutout (c : in pac_via_restrict_cutouts.cursor) is
					cutout : type_via_restrict_cutout renames element (c);
					drawn : boolean := false;
				begin					
					draw_contour (
						contour	=> cutout,
						width	=> via_restrict_line_width, 
						filled	=> NO,
						drawn	=> drawn);
				end query_cutout;
				
			begin
				objects.lines.iterate (query_line'access);
				objects.arcs.iterate (query_arc'access);
				objects.circles.iterate (query_circle'access);

				set_line_width (context.cr, type_view_coordinate (via_restrict_line_width));
				objects.zones.iterate (query_zone'access);
				objects.cutouts.iterate (query_cutout'access);				
			end draw;
			
			
		begin -- draw_via_restrict
			set_color_via_restrict (context.cr, brightness);
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
			
			procedure query_hole (c : pac_holes.cursor) is
				drawn : boolean := false;
			begin
				draw_contour (
					contour	=> element (c),
					filled	=> NO,
					width	=> pcb_contour_line_width,
					drawn	=> drawn);

			end query_hole;
				
			
		begin
			if outline_enabled then

				set_color_outline (context.cr);
				set_line_width (context.cr, type_view_coordinate (pcb_contour_line_width));

				if electric then
					holes := get_holes (device_electric);
				else
					holes := get_holes (device_non_electric);
				end if;		

				holes.iterate (query_hole'access);
			end if;
		end draw_holes;

	------------------------------------------------------------------
		
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
					set_line_width (context.cr, type_view_coordinate (line.width));
					draw_line (
						line	=> to_line_fine (line),
						width	=> line.width);
				end query_line;

				procedure query_arc (c : in pac_conductor_arcs.cursor) is
					arc : type_conductor_arc renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (arc.width));
					draw_arc (
						arc		=> to_arc_fine (arc),
						width	=> arc.width);
				end query_arc;

				procedure query_circle (c : in pac_conductor_circles.cursor) is
					circle : type_conductor_circle renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (circle.width));
					draw_circle (
						circle	=> circle,
						width	=> circle.width, 
						filled	=> NO);
				end query_circle;

				procedure query_text (c : in pac_conductor_texts.cursor) is
					text : et_conductor_text.type_conductor_text renames element (c);
				begin
					set_line_width (context.cr, type_view_coordinate (text.line_width));
					draw_vector_text (
						text	=> text.vectors,
						width	=> text.line_width);
				end query_text;
				
			begin
				set_color_conductor (context.cr, layer, brightness);
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
			use et_canvas_schematic;
		begin
			return get_pcb_design_rules (current_active_module).stop_mask.expansion_min;
		end get_stop_mask_expansion;

		
		procedure draw_terminals is
			use pac_terminals;

			procedure draw_tht_pad_with_circular_cutout (
				outer_border	: in type_contour;
				drill_position	: in type_point;
				drill_size		: in type_drill_size)
			is 
				ib : constant type_circle := (
					center	=> drill_position, 
					radius	=> type_float_positive (drill_size) * 0.5,
					others	=> <>);
			begin
				set_color_tht_pad (context.cr, brightness);

				draw_contour_with_circular_cutout (
					outer_border	=> outer_border,
					inner_border	=> ib);

			end draw_tht_pad_with_circular_cutout;

			
			procedure draw_tht_pad_with_arbitrary_cutout (
				outer_border	: in type_contour;
				inner_border	: in type_contour)
			is begin
				set_color_tht_pad (context.cr, brightness);

				draw_contour_with_arbitrary_cutout (
					outer_border	=> outer_border,
					inner_border	=> inner_border);
				
			end draw_tht_pad_with_arbitrary_cutout;

			
			
			procedure query_terminal (c : in pac_terminals.cursor) is
				t : constant type_terminal := element (c);

				-- Draws the name of a smt pad.
				procedure draw_name_smt (
					name		: in string;  -- H5, 5, 3
					pad_pos_in	: in type_position)  -- the center of the pad
				is
					use et_text;
				begin
					set_color_terminal_name (context.cr, brightness);
					
					draw_text (
						content		=> to_content (name),
						size		=> terminal_name_size,
						font		=> terminal_name_font,
						position	=> pad_pos_in.place,
						origin		=> false, -- no origin required
						rotation	=> zero_rotation,
						alignment	=> (center, center));

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
					pad_pos : type_point := pad_pos_in.place;
					
				begin
					if conductors_enabled then
						
						if not name_drawn then
								
							-- Rotate the position of the pad by the rotation of the package:
							rotate_by (pad_pos, get_rotation (package_position));

							-- If the package is flipped, then the terminal position
							-- must be mirrored along the Y axis.
							if flipped then mirror (pad_pos, Y); end if;
							
							-- Move the pad by the position of the package:
							move_by (pad_pos, to_distance_relative (package_position.place));
							
							set_color_terminal_name (context.cr, brightness);
							
							draw_text (
								content		=> to_content (name),
								size		=> terminal_name_size,
								font		=> terminal_name_font,
								position	=> pad_pos,
								origin		=> false, -- no origin required
								rotation	=> zero_rotation,
								alignment	=> (center, center));

							name_drawn := true;
						end if;
						
					end if;
				end draw_name_tht;

				
				-- This procedure draws the SMT pad, the stop mask, the stencil and 
				-- the terminal name. The terminal name will be drawn only when
				-- the signal layer is enabled.
				procedure draw_pad_smt (
					name			: in string;  -- H5, 5, 3
					pad_outline_in	: in type_contour; -- the outline of the solder pad (copper)
					stop_mask_in	: in type_stop_mask_smt; -- the stop mask of the pad
					stencil_in		: in type_stencil_shape; -- the solder cream mask of the pad
					pad_pos_in		: in type_position; -- the center of the pad incl. its rotation
					f				: in type_face) 
				is
					polygon_tmp : type_polygon;
					
					pad_outline : type_contour := pad_outline_in;
					pad_pos : type_position := pad_pos_in;

					stop_mask_contours	: type_stop_mask_contours;
					stencil_contours	: type_stencil_contours;
					
					ly : constant type_signal_layer := face_to_layer (f);
					drawn : boolean := false;
				begin
					-- We draw only if either the signal layer, the stop mask or the stencil
					-- is enabled. Otherwise nothing will happen here:
					if conductor_enabled (ly) or stop_mask_enabled (f) or stencil_enabled (f) then
						
						if f = face then

							-- Calculate the final position of the terminal and the
							-- rotated or mirrored pad outline.
							move_contours (pad_pos, pad_outline, flip, package_position);
								
							-- draw the solder pad (conductor material):
							if conductor_enabled (ly) then

								set_color_conductor (context.cr, ly, brightness);
								
								draw_contour (
									contour	=> pad_outline,
									filled	=> YES,
									width	=> zero,
									drawn	=> drawn);
								
								-- draw the terminal name
								draw_name_smt (name, pad_pos);
							end if;

							
							-- draw the stop mask
							if stop_mask_enabled (f) then
								
								case stop_mask_in.shape is
									when AS_PAD =>
										-- copy solder pad contours
										stop_mask_contours := (type_contour (pad_outline) with null record);
										
									when EXPAND_PAD =>
										pad_pos := pad_pos_in;  -- get initial pad position
										
										-- copy solder pad contour and expand according to DRU
										stop_mask_contours := (type_contour (pad_outline_in) with null record);

										-- make a temporary polygon from the stop mask contour
										polygon_tmp := to_polygon (stop_mask_contours, fill_tolerance, EXPAND);
										-- CS: expand correct ?

										-- offset the temporary polygon
										offset_polygon (
											polygon		=> polygon_tmp,
											offset		=> type_float (get_stop_mask_expansion)); -- from DRU

										-- convert the temporary polygon back to a contour
										stop_mask_contours := (to_contour (polygon_tmp) with null record);
										
										-- compute final position of expanded stop mask opening
										move_contours (pad_pos, stop_mask_contours, flip, package_position);
										
									when USER_SPECIFIC =>
										-- compute position of user specific stop mask contours:
										pad_pos := pad_pos_in;
										stop_mask_contours := stop_mask_in.contours;
										move_contours (pad_pos, stop_mask_contours, flip, package_position);
								end case;

								set_color_stop_mask (context.cr, f, global_scale, brightness);

								draw_contour (
									contour	=> stop_mask_contours,
									filled	=> YES,
									width	=> zero,
									drawn	=> drawn);
								
							end if;

							
							-- draw stencil (or solder paste mask)
							if stencil_enabled (f) then

								case stencil_in.shape is
									
									when AS_PAD =>
										-- copy solder pad contours
										stencil_contours := (type_contour (pad_outline) with null record);
										
									when SHRINK_PAD =>
										pad_pos := pad_pos_in;  -- get initial pad position

										-- copy solder pad contour and shrink according to shrink_factor
										stencil_contours := (type_contour (pad_outline_in) with null record);

										-- make a temporary polygon from the stencil contour
										polygon_tmp := to_polygon (stencil_contours, fill_tolerance, EXPAND);
										-- CS: expand correct ?
										
										--scale_polygon (
											--polygon		=> stencil_contours,
											--scale		=> stencil_in.shrink_factor);

										-- offset the temporary polygon
										offset_polygon (
											polygon		=> polygon_tmp,
											offset		=> type_float (stencil_in.shrink_factor));

										-- convert the temporary polygon back to a contour
										stencil_contours := (to_contour (polygon_tmp) with null record);
										
										-- compute final position of shrinked stencil opening
										move_contours (pad_pos, stencil_contours, flip, package_position);
										
									when USER_SPECIFIC =>
										-- compute position of user specific stencil contours:
										pad_pos := pad_pos_in; -- get initial pad position
										stencil_contours := stencil_in.contours;
										move_contours (pad_pos, stencil_contours, flip, package_position);
										
								end case;

								
								set_color_stencil (context.cr, f, global_scale, brightness);
								
								draw_contour (
									contour	=> stencil_contours,
									filled	=> YES,
									width	=> zero,
									drawn	=> drawn);

							end if;
						end if;
					end if;
				end draw_pad_smt;
				
				
				-- This procedure draws the outer contour of the THT pad and 
				-- th outer contour of the stop mask
				-- in top/bottom signal layer (specified by caller).
				procedure tht_outer_layer (
					pad_outline_in	: in type_contour; -- the outline of the solder pad
					stop_mask_in	: in et_terminals.type_stop_mask; -- the stop mask in the outer layer
					pad_pos_in		: in type_position; -- the center of the pad incl. its rotation
					f				: in type_face;
					drilled_milled	: in type_terminal_tht_hole;
					drill_size		: in type_drill_size := type_drill_size'first;
					hole_outline_in	: in type_contour := plated_millings_default)
				is
					polygon_tmp : type_polygon;
					
					pad_outline_outer_layer : type_contour := pad_outline_in;
					pad_pos : type_position := pad_pos_in;

					hole_outline : type_contour := hole_outline_in;
					
					stop_mask_contours : type_stop_mask_contours;
					
					ly : constant type_signal_layer := face_to_layer (f);
					drawn : boolean := false;
				begin
					-- We draw only if either the signal layer or the stop mask
					-- is enabled. Otherwise nothing will happen here:
					if conductor_enabled (ly) or stop_mask_enabled (f) then
						
						if f = face then

							-- Calculate the final position of the terminal and the
							-- rotated or mirrored pad outline.
							move_contours (pad_pos, pad_outline_outer_layer, flip, package_position);
							
							-- draw the outer solder pad contour:
							if conductor_enabled (ly) then
								case drilled_milled is
									when DRILLED =>
									
										draw_tht_pad_with_circular_cutout (
											outer_border	=> pad_outline_outer_layer,
											drill_position	=> pad_pos.place,
											drill_size		=> drill_size);


									when MILLED =>

										-- Calculate the final position of the milled hole:
										pad_pos := pad_pos_in;
										move_contours (pad_pos, hole_outline, flip, package_position);
										
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
										stop_mask_contours := (type_contour (pad_outline_outer_layer) with null record);
										
									when EXPAND_PAD =>
										pad_pos := pad_pos_in;  -- get initial pad position
										
										-- copy solder pad contour and expand according to DRU
										stop_mask_contours := (type_contour (pad_outline_in) with null record);

										-- make a temporary polygon from the stop mask contour
										polygon_tmp := to_polygon (stop_mask_contours, fill_tolerance, EXPAND);
										-- CS: expand correct ?
										
										-- offset the temporary polygon										
										offset_polygon (
											polygon		=> polygon_tmp,
											offset		=> type_float (get_stop_mask_expansion));  -- from DRU

										-- convert the temporary polygon back to a contour
										stop_mask_contours := (to_contour (polygon_tmp) with null record);
										
										-- compute final position of expanded stop mask opening
										move_contours (pad_pos, stop_mask_contours, flip, package_position);
										
									when USER_SPECIFIC =>										
										-- compute position of user specific stop mask contours:
										pad_pos := pad_pos_in;
										stop_mask_contours := stop_mask_in.contours;
										move_contours (pad_pos, stop_mask_contours, flip, package_position);
								end case;

								set_color_stop_mask (context.cr, f, global_scale, brightness);
								
								-- draw the outer contour of the stop mask opening
								draw_contour (
									contour		=> stop_mask_contours,
									filled		=> YES,
									width		=> zero,
									drawn		=> drawn);

							end if;

						end if;

					end if;
				end tht_outer_layer;

				
				-- This procedure draws the pad contour of a milled THT pad
				-- in an inner conductor layer
				-- if any inner conductor layer is enabled. If no inner conductor
				-- layer is enabled, nothing happens.
				procedure tht_inner_layer_milled (
					hole_outline_in	: in type_contour; -- the countours of the milled hole
					restring_width	: in type_track_width;
					pad_pos_in		: in type_position) -- the center of the pad incl. its rotation
				is
					polygon_tmp : type_polygon;
					
					hole_outline : type_contour := hole_outline_in;
					pad_pos : type_position := pad_pos_in;

					pad_outline_inner_layers : type_contour;
				begin
					if inner_conductors_enabled (bottom_layer) then
								
						move_contours (pad_pos, hole_outline, flip, package_position);
						
						-- Compute a polygon that extends the given hole outline by the restring_width:
						pad_outline_inner_layers := type_contour (hole_outline_in);

						-- make a temporary polygon from the inner outline
						polygon_tmp := to_polygon (pad_outline_inner_layers, fill_tolerance, EXPAND);
						-- CS: expand correct ?

						-- offset the temporary polygon
						offset_polygon (
							polygon		=> polygon_tmp, 
							offset		=> type_float (restring_width));

						-- move the conductor frame to its final position:
						pad_pos := pad_pos_in;  -- get initial pad position

						-- convert the temporary polygon back to a contour
						pad_outline_inner_layers := to_contour (polygon_tmp);

						-- compute final position of the inner outline
						move_contours (pad_pos, pad_outline_inner_layers, flip, package_position);
						
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
						rotate_by (pad_pos.place, get_rotation (package_position));

						if flipped then mirror (pad_pos.place, Y); end if;

						-- Move the drill by the position of the package:
						move_by (pad_pos.place, to_distance_relative (package_position.place));


						-- Build a circle to show the restring of inner layers:
						circle.center := pad_pos.place;

						-- set line width and radius:
						--set_line_width (context.cr, type_view_coordinate (restring));
						--circle.radius := (drill_size + restring) * 0.5;

						set_color_tht_pad (context.cr, brightness);
						set_line_width (context.cr, type_view_coordinate (zero));

						circle.radius := type_float_positive (drill_size) * 0.5 
										 + type_float_positive (restring);
						
						--draw_circle (in_area, context, circle, NO, self.get_frame_height);
						draw_circle (circle, YES, zero);

						
						-- the cutout area must clear out the outer area:
						set_operator (context.cr, CAIRO_OPERATOR_CLEAR);

						circle.radius := type_float_positive (drill_size) * 0.5;
						draw_circle (circle, YES, zero);

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


				
			package_cursor : et_packages.pac_package_models.cursor;

			use et_schematic;
			use pac_devices_sch;

			use et_pcb;
			use pac_devices_non_electric;
		begin
			-- locate the package model in the package library:
			if electric then
				package_cursor := get_package_model (device_electric);
			else
				package_cursor := get_package_model (element (device_non_electric).package_model);
			end if;
			
			element (package_cursor).terminals.iterate (query_terminal'access);
		end draw_terminals;

		
		procedure draw_package_origin is
			line_horizontal : constant type_line_fine := ( -- from left to right
				start_point		=> to_vector (set (x => get_x (package_position) - et_packages.origin_half_size, y => get_y (package_position))),
				end_point		=> to_vector (set (x => get_x (package_position) + et_packages.origin_half_size, y => get_y (package_position))));

			line_vertical : constant type_line_fine := ( -- from bottom to top
				start_point		=> to_vector (set (x => get_x (package_position), y => get_y (package_position) - et_packages.origin_half_size)),
				end_point		=> to_vector (set (x => get_x (package_position), y => get_y (package_position) + et_packages.origin_half_size)));

		begin -- draw_package_origin
			if face = get_face (package_position) then
				if device_origins_enabled (get_face (package_position)) then

					set_color_origin (context.cr, brightness);
					set_line_width (context.cr, type_view_coordinate (et_packages.origin_line_width));
					draw_line (line_horizontal, et_packages.origin_line_width);
					draw_line (line_vertical, et_packages.origin_line_width);

				end if;
			end if;
		end draw_package_origin;

		
	begin -- draw_package
		--put_line (et_devices.to_string (device_name));

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
		module		: in type_module) 
	is
		use pac_devices_sch;

		
		procedure query_device (device_cursor : in pac_devices_sch.cursor) is
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


			-- Draws the device packae at the position given by the pointer or the cursor:
			procedure draw_being_moved is 
				-- If the device is being moved, then the x/y position
				-- will be overwritten by the position of the mouse or the cursor.

				use et_devices;
				procedure set_position (
					name	: in type_device_name;
					device	: in out type_device_sch)
				is begin
					case preliminary_electrical_device.tool is
						when MOUSE =>
							device.position.place := snap_to_grid (get_mouse_position);

						when KEYBOARD =>
							device.position.place := cursor_main.position;
					end case;
				end set_position;

				-- Create a temporary map of devices. This map will contain just a single device:
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
			if is_real (device_cursor) then

				-- If the device candidate is selected, then we will
				-- draw it highlighted:
				if is_selected (device_cursor) then
					brightness := BRIGHT;

					case verb is
						when VERB_MOVE =>

							-- If a move operation is in progress, then the mouse
							-- or cursor position overwrites the device position:
							if preliminary_electrical_device.ready then
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
				
				-- CS live update ratsnest
				
			end if;
		end query_device;
		
	begin
		module.devices.iterate (query_device'access);
	end query_electrical_devices;


	-- Draws the packages of non-electrical devices:
	procedure query_non_electrical_devices (
		module_name	: in pac_module_name.bounded_string;
		module		: in type_module) 
	is
		use et_pcb;
		use pac_devices_non_electric;


		procedure query_device (device_cursor : in pac_devices_non_electric.cursor) is 
			device : type_device_non_electric renames element (device_cursor);
			use et_devices;
			
			brightness : type_brightness := NORMAL;

			-- Draws the device package at the position as it is in the board:
			procedure draw_fixed is begin
				draw_package (
					electric			=> false,
					device_electric		=> pac_devices_sch.no_element,
					device_non_electric	=> device_cursor,
					flip				=> device.flipped,
					brightness 			=> brightness);
			end draw_fixed;

			
			-- Draws the device packae at the position given by the pointer or the cursor:
			procedure draw_being_moved is 
				-- If the device is being moved, then the x/y position
				-- will be overwritten by the position of the mouse or the cursor.

				use et_devices;
				procedure set_position (
					name	: in type_device_name;
					device	: in out type_device_non_electric)
				is begin
					case preliminary_non_electrical_device.tool is
						when MOUSE =>
							device.position.place := snap_to_grid (get_mouse_position);

						when KEYBOARD =>
							device.position.place := cursor_main.position;
					end case;
				end set_position;

				-- Create a temporary map of devices. This map will contain just a single device:
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
			if is_selected (device_cursor) then
				brightness := BRIGHT;

				-- If a move operation is in progress, then the mouse
				case verb is
					when VERB_MOVE =>

						-- If a move operation is in progress, then the mouse
						-- or cursor position overwrites the device position:
						if preliminary_non_electrical_device.ready then
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

	
begin -- draw_packages
-- 	put_line ("draw packages ...");

	-- draw electric devices
	pac_generic_modules.query_element (
		position	=> current_active_module,
		process		=> query_electrical_devices'access);

	
	-- draw non-electric devices (like fiducials, mounting holes, ...)
	pac_generic_modules.query_element (
		position	=> current_active_module,
		process		=> query_non_electrical_devices'access);
			
end draw_packages;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
