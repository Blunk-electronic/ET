------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             BOARD WRITE                                  --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters;			use ada.characters;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.text_io;				use ada.text_io;

with et_axes;						use et_axes;
with et_coordinates_formatting;		use et_coordinates_formatting;
with et_text;						use et_text;
with et_alignment;					use et_alignment;
with et_exceptions;					use et_exceptions;
with et_keywords;					use et_keywords;
with et_directions;					use et_directions;


package body et_board_write is

	--procedure write_text_properties (t : in et_packages.type_text'class) is
		--use et_packages;
	--begin
---- 		write (keyword => keyword_position, parameters => position (text.position) & 
---- 			space & keyword_rotation & to_string (get_angle (text.position))
---- 			  ); -- position x 0.000 y 5.555 rotation 0.00

		--write (keyword => keyword_position, parameters => position (t.position));
			---- position x 0.000 y 5.555 rotation 0.00
		
		--write (keyword => keyword_size, parameters => to_string (t.size)); -- size 1.000
		
		--write (keyword => keyword_line_width, parameters => to_string (t.line_width));
		--write (keyword => keyword_alignment, parameters =>
			--keyword_horizontal & space & to_string (t.alignment.horizontal) & space &
			--keyword_vertical   & space & to_string (t.alignment.vertical));
		
		---- CS write (keyword => keyword_hidden, parameters => space & to_lower (boolean'image (text.hidden)));
	--end write_text_properties;

	
	procedure write_text_properties (
		t : in type_text_fab'class) 
	is begin
		-- CS rework !
		
-- 		write (keyword => keyword_position, parameters => position (text.position) & 
-- 			space & keyword_rotation & to_string (get_angle (text.position))
-- 			  ); -- position x 0.000 y 5.555 rotation 0.00

		write (keyword => keyword_position, parameters => 
			to_string (get_position (t), FORMAT_2));
			-- position x 0.000 y 5.555 rotation 0.00
		
		write (keyword => keyword_size, parameters => to_string (t.size)); -- size 1.000
		
		write (keyword => keyword_linewidth, parameters => to_string (t.line_width));
		write (keyword => keyword_alignment, parameters =>
			keyword_horizontal & space & to_string (t.alignment.horizontal) & space &
			keyword_vertical   & space & to_string (t.alignment.vertical));

		-- CS use et_alignment.to_string 
	end write_text_properties;

	
	
	procedure write_text_properties_with_face (
		t		: in type_text_fab'class;
		face	: in type_face) 
	is 
		position : type_package_position;
	begin
		-- CS rework !

		-- Assemble the text position:
		set_position (position, get_position (t));
		set_face (position, face);
					  
		write (keyword => keyword_position, parameters => 
			   to_string (position, FORMAT_2)); 
		-- position x 0.000 y 5.555 rotation 0.00 face top

		write (keyword => keyword_size, parameters => to_string (t.size)); -- size 1.000
		
		write (keyword => keyword_linewidth, parameters => to_string (t.line_width));
		write (keyword => keyword_alignment, parameters =>
				keyword_horizontal & space & to_string (t.alignment.horizontal) & space &
				keyword_vertical   & space & to_string (t.alignment.vertical)
			  );

		-- CS use et_alignment.to_string 
		
		-- CS write (keyword => keyword_hidden, parameters => space & to_lower (boolean'image (text.hidden)));
	end write_text_properties_with_face;

	
	
	procedure write_text (cursor : in pac_texts_fab_with_content.cursor) is
		use pac_texts_fab_with_content;
	begin
		text_begin;
		write (keyword => keyword_content, wrap => true,
			parameters => to_string (element (cursor).content));
		write_text_properties (element (cursor));
		text_end;
	end write_text;

	--procedure write_text (cursor : in pac_conductor_texts_package.cursor) is
		--use pac_conductor_texts_package;
	--begin
		--text_begin;
		--write (keyword => keyword_content, wrap => true,
			--parameters => to_string (element (cursor).content));
		---- CS write_text_properties (element (cursor));
		--text_end;
	--end write_text;

	
	procedure write_width (width : in type_track_width) is begin
		write (keyword => keyword_width, parameters => to_string (width));
	end;

	procedure write_fill_linewidth (width : in type_track_width) is begin
		write (keyword => keyword_linewidth, parameters => to_string (width));
	end;

	
	procedure write_line (line : in type_line'class) is begin
		write (keyword => keyword_start, parameters => to_string (get_A (line), FORMAT_2));
		write (keyword => keyword_end  , parameters => to_string (get_B (line), FORMAT_2));
	end write_line;

	
	procedure write_arc (arc : in type_arc'class) is begin
		write (keyword => keyword_center, parameters => to_string (get_center (arc), FORMAT_2));
		write (keyword => keyword_start, parameters => to_string (get_A (arc), FORMAT_2));
		write (keyword => keyword_end, parameters => to_string (get_B (arc), FORMAT_2));
		write (keyword => keyword_direction, parameters => to_string (get_direction (arc)));
	end write_arc;

	
	procedure write_circle (circle : in type_circle'class) is begin
		write (keyword => keyword_center, parameters => to_string (get_center (circle), FORMAT_2));
		write (keyword => keyword_radius, parameters => to_string (get_radius (circle)));
	end write_circle;


	procedure write_spacing (spacing : in type_track_clearance) is
	begin
		write (keyword => keyword_spacing, parameters => to_string (spacing));
	end;

	
	--procedure write_hatching (hatching : in type_hatching) is
	--begin
		--write (keyword => keyword_hatching_line_width  , parameters => to_string (hatching.line_width));
		--write (keyword => keyword_spacing, parameters => to_string (hatching.spacing));
		--write (keyword => keyword_hatching_border_width, parameters => to_string (hatching.border_width));
	--end;

	
	procedure write_easing (easing: in type_easing) is
	begin
		write (keyword => keyword_easing_style, parameters => to_string (easing.style));
		write (keyword => keyword_easing_radius, parameters => to_string (easing.radius));
	end;

	
	procedure write_thermal (thermal : in type_relief_properties) is
		use et_pcb;
	begin
		write (keyword => keyword_pad_technology,   parameters => to_string (thermal.technology));
		write (keyword => keyword_relief_width_min, parameters => to_string (thermal.width_min));
		write (keyword => keyword_relief_gap_max,   parameters => to_string (thermal.gap_max));	
	end;

	
	
	procedure write_isolation (iso : in type_track_clearance) is begin
		write (keyword => keyword_isolation, parameters => to_string (iso));
	end;

	
	procedure write_priority (prio : in type_priority) is
		use et_pcb;
	begin
		write (keyword => keyword_priority , parameters => to_string (prio));
	end;

	
	procedure write_signal_layer (layer : in et_pcb_stack.type_signal_layer) is 
		use et_pcb_stack;
	begin
		write (keyword => keyword_layer, parameters => to_string (layer));
	end;

	
	procedure write_fill_style (fill_style : in type_fill_style) is
	begin
		write (keyword => keyword_fill_style, parameters => to_string (fill_style));
	end;

	
	procedure write_fill_status (filled : in type_filled) is begin
		write (keyword => keyword_filled, parameters => to_string (filled));
	end;

	
	procedure write_pad_connection (connection : in type_pad_connection) is
		use et_pcb;
	begin
		write (keyword => keyword_connection, parameters => to_string (connection));
	end;

	
	procedure write_pad_technology (techno : in type_pad_technology) is
		use et_pcb;
	begin
		write (keyword => keyword_pad_technology, parameters => to_string (techno));
	end;	

	
	procedure write_signal_layers (layers : in et_pcb_stack.type_signal_layers.set) is
		use et_pcb_stack;
	begin
		write (keyword => keyword_layers, parameters => to_string (layers));
	end;

	

	-- CS unify the follwing two procedures write_circle_conductor:
	procedure write_circle_conductor (circle : in et_conductor_segment.type_conductor_circle) is begin
		circle_begin;
		write_circle (circle);
		write (keyword => keyword_width, parameters => to_string (circle.width));
		circle_end;
	end write_circle_conductor;

	
	procedure write_circle_conductor (circle : in et_conductor_segment.boards.type_conductor_circle) is begin
		circle_begin;
		write_circle (circle);
		write (keyword => keyword_width, parameters => to_string (circle.width));
		write_signal_layer (circle.layer);
		circle_end;
	end write_circle_conductor;

	
	procedure write_polygon_segments (
		polygon : in type_contour'class)
	is
		use pac_segments;
		
		procedure query_segment (c : in pac_segments.cursor) is begin
			case element (c).shape is
				
				when LINE =>
					line_begin;
					write_line (element (c).segment_line);
					line_end;

				when ARC =>
					arc_begin;
					write_arc (element (c).segment_arc);
					arc_end;

			end case;
		end query_segment;		

		contours : type_segments := get_segments (polygon);
		
	begin				
		if contours.circular then

			circle_begin;
			write_circle (contours.circle);
			circle_end;

		else
			contours.segments.iterate (query_segment'access);
		end if;
		
	end write_polygon_segments;
	


	
	

	
	
	procedure fill_zone_begin is begin section_mark (section_zone, HEADER); end;
	procedure fill_zone_end   is begin section_mark (section_zone, FOOTER); end;
	procedure cutout_zone_begin is begin section_mark (section_cutout_zone, HEADER); end;
	procedure cutout_zone_end   is begin section_mark (section_cutout_zone, FOOTER); end;
	procedure contours_begin is begin section_mark (section_contours, HEADER); end;
	procedure contours_end   is begin section_mark (section_contours, FOOTER); end;


	
	
-- SILKSCREEN
	procedure write_line (cursor : in pac_silk_lines.cursor) is 
		use pac_silk_lines;
	begin
		line_begin;
		write_line (element (cursor));		
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	
	procedure write_arc (cursor : in pac_silk_arcs.cursor) is 
		use pac_silk_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	
	procedure write_circle (cursor : in pac_silk_circles.cursor) is 
		use pac_silk_circles;
	begin
		circle_begin;
		write_circle (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		circle_end;
	end write_circle;

	
	procedure write_polygon (cursor : in pac_silk_zones.cursor) is 
		use pac_silk_zones;
	begin
		fill_zone_begin;
		contours_begin;		
		write_polygon_segments (type_contour (element (cursor)));
		contours_end;
		fill_zone_end;
	end write_polygon;

	
	
	
-- ASSEMBLY DOCUMENTATION
	procedure write_line (cursor : in pac_doc_lines.cursor) is 
		use pac_doc_lines;
	begin
		line_begin;
		write_line (element (cursor));		
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	procedure write_arc (cursor : in pac_doc_arcs.cursor) is 
		use pac_doc_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	
	procedure write_circle (cursor : in pac_doc_circles.cursor) is
		use pac_doc_circles;
	begin
		circle_begin;
		write_circle (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		circle_end;
	end write_circle;

	
	procedure write_polygon (cursor : in pac_doc_zones.cursor) is 
		use pac_doc_zones;
	begin
		fill_zone_begin;
		contours_begin;		
		write_polygon_segments (element (cursor));
		contours_end;
		fill_zone_end;
	end write_polygon;


	
-- KEEPOUT
	
	procedure write_polygon (cursor : in pac_keepout_zones.cursor) is 
		use pac_keepout_zones;
	begin
		fill_zone_begin;
		contours_begin;
		write_polygon_segments (element (cursor));
		contours_end;
		fill_zone_end;
	end write_polygon;

	
	procedure write_cutout (cursor : in pac_keepout_cutouts.cursor) is 
		use pac_keepout_cutouts;
	begin
		cutout_zone_begin;		
		contours_begin;
		write_polygon_segments (element (cursor));
		contours_end;
		cutout_zone_end;
	end;

	
-- STOP MASK
	procedure write_line (cursor : in pac_stop_lines.cursor) is 
		use pac_stop_lines;
	begin
		line_begin;
		write_line (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	
	procedure write_arc (cursor : in pac_stop_arcs.cursor) is 
		use pac_stop_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	
	procedure write_circle (cursor : in pac_stop_circles.cursor) is 
		use pac_stop_circles;
	begin
		circle_begin;
		write_circle (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		circle_end;
	end write_circle;

	
	procedure write_polygon (cursor : in pac_stop_zones.cursor) is 
		use pac_stop_zones;
	begin
		fill_zone_begin;
		contours_begin;		
		write_polygon_segments (element (cursor));
		contours_end;
		fill_zone_end;
	end write_polygon;

	
	
-- STENCIL (OR SOLDER PASTE MASK)
	
	procedure write_line (cursor : in pac_stencil_lines.cursor) is 
		use pac_stencil_lines;
	begin
		line_begin;
		write_line (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	
	procedure write_arc (cursor : in pac_stencil_arcs.cursor) is 
		use pac_stencil_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	
	procedure write_circle (cursor : in pac_stencil_circles.cursor) is 
		use pac_stencil_circles;
	begin
		circle_begin;
		write_circle (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		circle_end;
	end write_circle;

	
	procedure write_polygon (cursor : in pac_stencil_zones.cursor) is 
		use pac_stencil_zones;
	begin
		fill_zone_begin;
		contours_begin;
		write_polygon_segments (element (cursor));
		contours_end;
		fill_zone_end;
	end write_polygon;


	
	
-- BOARD CONTOUR
	--procedure write_line (cursor : in et_pcb.pac_pcb_contour_lines.cursor) is 
		--use et_pcb;
		--use et_pcb.pac_pcb_contour_lines;
	--begin
		--line_begin;
		--write_line (element (cursor));
		--write (keyword => keyword_locked, parameters => to_string (element (cursor).locked));
		--line_end;
	--end write_line;
	
	--procedure write_arc (cursor : in et_pcb.pac_pcb_contour_arcs.cursor) is 
		--use et_pcb;
		--use et_pcb.pac_pcb_contour_arcs;
	--begin
		--arc_begin;
		--write_arc (element (cursor));
		--write (keyword => keyword_locked, parameters => to_string (element (cursor).locked));
		--arc_end;
	--end write_arc;

	--procedure write_circle (cursor : in et_pcb.pac_pcb_contour_circles.cursor) is 
		--use et_pcb;
		--use et_pcb.pac_pcb_contour_circles;
	--begin
		--circle_begin;
		--write_circle (element (cursor));
		--write (keyword => keyword_locked, parameters => to_string (element (cursor).locked));
		--circle_end;
	--end write_circle;


-- ROUTE RESTRICT
	
	procedure write_line (cursor : in pac_route_restrict_lines.cursor) is 
	begin
		line_begin;
		write_line (element (cursor));
		write_signal_layers (element (cursor).layers);
		line_end;
	end write_line;

	
	procedure write_arc (cursor : in pac_route_restrict_arcs.cursor) is 
	begin
		arc_begin;
		write_arc (element (cursor));		
		write_signal_layers (element (cursor).layers);
		arc_end;
	end write_arc;

	
	procedure write_circle (cursor : in pac_route_restrict_circles.cursor) is 
	begin
		circle_begin;
		write_circle (element (cursor));
		write_signal_layers (element (cursor).layers);
		circle_end;
	end write_circle;

	
	procedure write_contour (cursor : in pac_route_restrict_contours.cursor) is 
	begin
		fill_zone_begin;
		write_signal_layers (element (cursor).layers);

		contours_begin;
		write_polygon_segments (element (cursor));
		contours_end;
		
		fill_zone_end;
	end write_contour;

	
	procedure write_cutout (cursor : in pac_route_restrict_cutouts.cursor) is 
	begin
		cutout_zone_begin;
		write_signal_layers (element (cursor).layers);

		contours_begin;
		write_polygon_segments (element (cursor));
		contours_end;
		
		cutout_zone_end;
	end;


	

-- VIA RESTRICT

	
	procedure write_contour (cursor : in pac_via_restrict_contours.cursor) is 
		use et_pcb_stack;
		use pac_via_restrict_contours;
	begin
		fill_zone_begin;
		write_signal_layers (element (cursor).layers);			

		contours_begin;
		write_polygon_segments (element (cursor));
		contours_end;
		
		fill_zone_end;
	end write_contour;

	
	procedure write_cutout (cursor : in pac_via_restrict_cutouts.cursor) is 
		use pac_via_restrict_cutouts;
	begin
		cutout_zone_begin;
		write_signal_layers (element (cursor).layers);
		
		contours_begin;
		write_polygon_segments (element (cursor));
		contours_end;
		
		cutout_zone_end;
	end;

	
end et_board_write;
