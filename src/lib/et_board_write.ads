------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             BOARD WRITE                                  --
--                                                                          --
--                               S p e c                                    --
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

--   do do:

with et_primitive_objects;		use et_primitive_objects;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_general_rw;				use et_general_rw;
with et_pcb_sides;				use et_pcb_sides;
with et_board_geometry;			use et_board_geometry;
with et_board_coordinates;		use et_board_coordinates;
with et_board_text;				use et_board_text;
with et_drills;					use et_drills;
with et_pcb;
with et_pcb_contour;
with et_pcb_stack;
with et_pcb_signal_layers;		use et_pcb_signal_layers;
with et_design_rules_board;		use et_design_rules_board;
with et_fill_zones;				use et_fill_zones;
with et_fill_zones.boards;		use et_fill_zones.boards;
with et_thermal_relief;			use et_thermal_relief;
--with et_conductor_segment;
with et_conductor_segment.boards;	--use et_conductor_segment.boards;
with et_conductor_text;			use et_conductor_text;
with et_stopmask;				use et_stopmask;
with et_stencil;				use et_stencil;
with et_silkscreen;				use et_silkscreen;
with et_assy_doc;				use et_assy_doc;
with et_keepout;				use et_keepout;
with et_package_sections;		use et_package_sections;

with et_route_restrict.boards;	use et_route_restrict.boards;
with et_via_restrict.boards;	use et_via_restrict.boards;


package et_board_write is

	use pac_geometry_2;
	use pac_contours;
	use pac_text_board;
	


	procedure write_text_properties (
		t : in 	type_text_fab'class);

	
	procedure write_text_properties_with_face (
		t		: in type_text_fab'class;
		face	: in type_face);

	
	procedure write_text (cursor : in pac_texts_fab_with_content.cursor);
	--procedure write_text (cursor : in pac_conductor_texts_package.cursor);
	
	

	
	procedure write_width (width : in type_track_width);
	
	procedure write_fill_linewidth (width : in type_track_width);		

	
	-- writes start and end point of a line
	procedure write_line (line : in type_line'class);

	
	-- writes center, start and end point of an arc
	procedure write_arc (arc : in type_arc'class);

	
	-- writes center and radius of a circle
	procedure write_circle (circle : in type_circle'class);

	procedure write_spacing (spacing : in type_track_clearance);
	--procedure write_hatching (hatching : in type_hatching);
	procedure write_easing (easing: in type_easing);
	procedure write_thermal (thermal : in type_relief_properties);
	procedure write_isolation (iso : in type_track_clearance);
	procedure write_priority (prio : in type_priority);
	procedure write_signal_layer (layer : in type_signal_layer);
	procedure write_fill_style (fill_style : in type_fill_style);
	procedure write_fill_status (filled : in type_filled);
	procedure write_pad_connection (connection : in type_pad_connection);
	procedure write_pad_technology (techno : in type_pad_technology);	
	procedure write_signal_layers (layers : in pac_signal_layers.set);
	procedure write_circle_conductor (circle : in et_conductor_segment.type_conductor_circle);

	-- Writes the properties of a circle in conductor as used in a freetrack:
	procedure write_circle_conductor (circle : in et_conductor_segment.boards.type_conductor_circle);	
	
	
	-- writes the segments of a polygon (lines, arcs or a single circle):
	procedure write_polygon_segments (
		polygon : in type_contour'class);




	procedure fill_zone_begin;
	procedure fill_zone_end;
	procedure cutout_zone_begin;
	procedure cutout_zone_end;
	procedure contours_begin;
	procedure contours_end;

	

-- SILK SCREEN
	procedure write_line (cursor : in pac_silk_lines.cursor);
	procedure write_arc (cursor : in pac_silk_arcs.cursor);
	procedure write_circle (cursor : in pac_silk_circles.cursor);	
	procedure write_polygon (cursor : in pac_silk_zones.cursor);

-- ASSEMBLY DOCUMENTATION
	procedure write_line (cursor : in pac_doc_lines.cursor);
	procedure write_arc (cursor : in pac_doc_arcs.cursor);
	procedure write_circle (cursor : in pac_doc_circles.cursor);	
	procedure write_polygon (cursor : in pac_doc_zones.cursor); -- CS rename to write_zone
	
-- KEEPOUT
	procedure write_polygon (cursor : in pac_keepout_zones.cursor);
	procedure write_cutout (cursor : in pac_keepout_cutouts.cursor);

-- STOPMASK
	procedure write_line (cursor : in pac_stop_lines.cursor);
	procedure write_arc (cursor : in pac_stop_arcs.cursor);
	procedure write_circle (cursor : in pac_stop_circles.cursor);
	procedure write_polygon (cursor : in pac_stop_zones.cursor);

-- STENCIL (OR SOLDER PASTE MASK)
	procedure write_line (cursor : in pac_stencil_lines.cursor);
	procedure write_arc (cursor : in pac_stencil_arcs.cursor);
	procedure write_circle (cursor : in pac_stencil_circles.cursor);	
	procedure write_polygon (cursor : in pac_stencil_zones.cursor);
	

-- ROUTE RESTRICT

	use pac_route_restrict_lines;
	use pac_route_restrict_arcs;
	use pac_route_restrict_circles;
	use pac_route_restrict_contours;
	use pac_route_restrict_cutouts;
	
	procedure write_line (cursor : in pac_route_restrict_lines.cursor);
	procedure write_arc (cursor : in pac_route_restrict_arcs.cursor);
	procedure write_circle (cursor : in pac_route_restrict_circles.cursor);	
	procedure write_contour (cursor : in pac_route_restrict_contours.cursor);
	procedure write_cutout (cursor : in pac_route_restrict_cutouts.cursor);

	
-- VIA RESTRICT

	use pac_via_restrict_contours;
	use pac_via_restrict_cutouts;

	procedure write_contour (cursor : in pac_via_restrict_contours.cursor);
	procedure write_cutout (cursor : in pac_via_restrict_cutouts.cursor);


	
end et_board_write;
