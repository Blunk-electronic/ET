------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               PCB_RW                                     --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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
--	  - separate package related stuff in separate package

with ada.containers;            use ada.containers;

with et_string_processing;
with et_geometry;				use et_geometry;
with et_packages;
with et_pcb;
with et_pcb_coordinates;
with et_pcb_stack;

package pcb_rw is
	
	section_board				: constant string	:= "[BOARD";
	section_top					: constant string	:= "[TOP";
	section_bottom				: constant string	:= "[BOTTOM";
	section_board_layer_stack	: constant string 	:= "[BOARD_LAYER_STACK";
	section_via					: constant string	:= "[VIA";
	
	section_route				: constant string 	:= "[ROUTE";	
	section_silk_screen			: constant string	:= "[SILK_SCREEN";
	section_assembly_doc		: constant string	:= "[ASSEMBLY_DOCUMENTATION";
	section_stencil				: constant string	:= "[STENCIL";
	section_stop_mask			: constant string	:= "[STOP_MASK";
	section_keepout				: constant string	:= "[KEEPOUT";
	section_route_restrict		: constant string	:= "[ROUTE_RESTRICT";
	section_via_restrict		: constant string	:= "[VIA_RESTRICT";
	section_copper				: constant string	:= "[COPPER";
	section_pcb_contours		: constant string	:= "[PCB_CONTOURS_NON_PLATED";
	--section_pcb_contours_plated	: constant string	:= "[PCB_CONTOURS_PLATED"; 
	section_pac_3d_contours		: constant string	:= "[PACKAGE_3D_CONTOURS";

	section_pad_contours_smt	: constant string	:= "[PAD_CONTOURS_SMT";
	section_pad_contours_tht	: constant string	:= "[PAD_CONTOURS_THT";	
	section_pad_millings		: constant string	:= "[MILLINGS";

	section_terminals			: constant string	:= "[TERMINALS";
	section_terminal			: constant string	:= "[TERMINAL";

	
	keyword_description				: constant string := "description";	
	keyword_width 					: constant string := "width";	
	keyword_clearance				: constant string := "clearance";
	keyword_track_width_min			: constant string := "track_width_min";
	keyword_via_drill_min			: constant string := "via_drill_min";
	keyword_via_restring_min		: constant string := "via_restring_min";	
	keyword_micro_via_drill_min		: constant string := "micro_via_drill_min";
	keyword_micro_via_restring_min	: constant string := "micro_via_restring_min";	
	
	
	procedure write_text_properties (t : in et_packages.type_text'class);
	
	procedure write_text_properties_with_face (
		t		: in et_packages.type_text'class;
		face	: in et_pcb_coordinates.type_face);

	procedure write_text (cursor : in et_packages.type_texts_with_content.cursor);
	
	keyword_locked : constant string := "locked"; -- layout related
	
	procedure write_width (width : in et_packages.type_track_width);	

	procedure write_line (line : in et_packages.pac_shapes.type_line'class);
	-- writes start and end point of a line

	procedure write_arc (arc : in et_packages.pac_shapes.type_arc'class);
	-- writes center, start and end point of an arc

	procedure write_circle (circle : in et_packages.pac_shapes.type_circle'class);
	-- writes center and radius of a circle

	
	procedure write_hatching (hatching : in et_packages.type_hatching);
	procedure write_hatching (hatching : in et_packages.type_hatching_copper);
	procedure write_easing (easing: in et_packages.type_easing);
	procedure write_thermal (thermal : in et_pcb.type_thermal);
	procedure write_width_min (width : in et_packages.type_track_width);
	procedure write_isolation (iso : in et_packages.type_track_clearance);
	procedure write_priority (prio : in et_pcb.type_polygon_priority);
	procedure write_signal_layer (layer : in et_pcb_stack.type_signal_layer);
	procedure write_fill_stlye (fill_style : in et_packages.type_fill_style);
	procedure write_fill_status (filled : in type_filled);
	procedure write_pad_connection (connection : in et_pcb.type_polygon_pad_connection);
	procedure write_pad_technology (techno : in et_pcb.type_polygon_pad_technology);	
	procedure write_signal_layers (layers : in et_pcb_stack.type_signal_layers.set);
	procedure write_circle_fillable (circle : in et_packages.type_fillable_circle);
	procedure write_circle_copper (circle : in et_packages.type_copper_circle);
	procedure write_circle_copper (circle : in et_pcb.type_copper_circle);	
	
	
	procedure write_polygon_segments (polygon : in et_packages.pac_shapes.type_polygon_base);
	-- writes the segments of a polygon (lines, arcs and circles)

	
	
	function to_position ( -- CS combine with next function to_position using the tag test ?
	-- Returns a type_point_2d in the the layout.
		line : in et_string_processing.type_fields_of_line; -- "start x 44.5 y 53.5"
		from : in positive)
		return et_pcb_coordinates.geometry.type_point;
		
	function to_position (
	-- Returns a type_position in the layout.
		line : in et_string_processing.type_fields_of_line; -- "x 23 y 0.2 rotation 90.0"
		from : in positive)
		return et_pcb_coordinates.geometry.type_position;

	function position (point : et_pcb_coordinates.geometry.type_point'class) return string;
	
	function to_grid (
		line : in et_string_processing.type_fields_of_line; -- "default x 1 y 1"
		from : in positive)
		return et_pcb_coordinates.geometry.type_grid;

	function to_layers (
	-- Converts a line like "layers 1 4 17" to a set of signal layers.
	-- Issues warning if a layer number occurs more than once.
		line : in et_string_processing.type_fields_of_line) -- layers 1 3 17
		return et_pcb_stack.type_signal_layers.set;	
	


-- BASIC GEOMETRIC OBJECTS USED IN PACKAGES AND BOARDS
	
	type type_board_line is new et_packages.pac_shapes.type_line with null record;

	procedure add_polygon_line (line : in out type_board_line);

	board_line : type_board_line;

	procedure board_reset_line;

	
	type type_board_arc is new et_packages.pac_shapes.type_arc with null record;

	procedure add_polygon_arc (arc : in out type_board_arc);

	board_arc : type_board_arc;

	procedure board_reset_arc;

	
	type type_board_circle is new et_packages.pac_shapes.type_circle with null record;

	procedure add_polygon_circle (circle : in out type_board_circle);
	
	board_circle : type_board_circle;
	procedure board_reset_circle;

	
	procedure read_board_line (line : et_string_processing.type_fields_of_line);
	-- Reads start and end point of the board_line. If the statement is invalid then an error issued.

	function read_board_line (line : et_string_processing.type_fields_of_line) return boolean;
	-- Reads start and end point of the board_line. If the statement is invalid then it returns a false.
	
	procedure read_board_arc (line : et_string_processing.type_fields_of_line);
	-- Reads start and end point of the board_arc. If the statement is invalid then an error issued.

	function read_board_arc (line : et_string_processing.type_fields_of_line) return boolean;
	-- Reads start and end point of the board_arc. If the statement is invalid then it returns a false.
	
	procedure read_board_circle (line : et_string_processing.type_fields_of_line);
	-- Reads start and end point of the board_circle. If the statement is invalid then an error issued.

	function read_board_circle (line : et_string_processing.type_fields_of_line) return boolean;
	-- Reads start and end point of the board_circle. If the statement is invalid then it returns a false.
	
	board_fill_style : et_packages.type_fill_style := et_packages.fill_style_default;
	board_filled : type_filled := filled_default;

	board_hatching : et_packages.type_hatching;
	board_hatching_copper : et_packages.type_hatching_copper;
	board_easing : et_packages.type_easing;

	
	type type_polygon is new et_packages.pac_shapes.type_polygon_base with null record;
	polygon : type_polygon;

	-- Increments polygon.segments_total by 1:
	procedure increment_segment_count;

	-- Returns the total number of segments of polygon:
	function segment_count return et_packages.pac_shapes.type_polygon_segment_count;


	
	polygon_isolation : et_packages.type_track_clearance := et_packages.type_track_clearance'first;
	polygon_width_min : et_packages.type_track_width := et_packages.type_track_width'first;

	-- board relevant only:
	polygon_pad_connection	: et_pcb.type_polygon_pad_connection := et_pcb.type_polygon_pad_connection'first;
	polygon_priority		: et_pcb.type_polygon_priority := et_pcb.type_polygon_priority'first;
	thermal					: et_pcb.type_thermal;
	signal_layer			: et_pcb_stack.type_signal_layer := et_pcb_stack.type_signal_layer'first;

	procedure board_reset_signal_layer;
	
	board_lock_status		: et_pcb.type_locked := et_pcb.NO;

	procedure board_reset_lock_status;
	
	board_line_width : et_packages.type_general_line_width := et_packages.type_general_line_width'first;

	procedure board_reset_line_width;

	-- package and board relevant:	
	procedure board_reset_circle_fillable;

	function to_fillable_circle (
	-- Composes a fillable circle from the given parameters. 
	-- Filled and fill_style are discriminants. Depending on them some parameters
	-- matter or not. See spec for type_fillable_circle.
		circle				: in et_packages.pac_shapes.type_circle;
		filled				: in type_filled;
		fill_style			: in et_packages.type_fill_style;
		circumfence_width	: in et_packages.type_general_line_width;
		hatching			: in et_packages.type_hatching)
		return et_packages.type_fillable_circle;

	
	function board_make_fillable_circle return et_packages.type_fillable_circle;

	function board_make_fillable_circle_solid return et_packages.type_fillable_circle_solid;

	function board_make_copper_circle return et_packages.type_copper_circle;
			

	
	procedure board_reset_polygon;
	-- This procdure resets polygon properties to their defaults.
	-- This procdure is used by both package and board parsing procedures read_package and read_module_file.
	-- Some properties have no meaning in packages as remarked below.

	section_fill_zone	: constant string := "[FILL_ZONE";
	section_cutout_zone	: constant string := "[CUTOUT_ZONE";
	section_contours	: constant string := "[CONTOURS";

	procedure fill_zone_begin;
	procedure fill_zone_end;
	procedure cutout_zone_begin;
	procedure cutout_zone_end;
	procedure contours_begin;
	procedure contours_end;


-- SILK SCREEN
	procedure write_line (cursor : in et_packages.type_silk_lines.cursor);
	procedure write_arc (cursor : in et_packages.type_silk_arcs.cursor);
	procedure write_circle (cursor : in et_packages.type_silk_circles.cursor);	
	procedure write_polygon (cursor : in et_packages.pac_silk_polygons.cursor);
	procedure write_cutout (cursor : in et_packages.pac_silk_cutouts.cursor);

-- ASSEMBLY DOCUMENTATION
	procedure write_line (cursor : in et_packages.type_doc_lines.cursor);
	procedure write_arc (cursor : in et_packages.type_doc_arcs.cursor);
	procedure write_circle (cursor : in et_packages.type_doc_circles.cursor);	
	procedure write_polygon (cursor : in et_packages.pac_doc_polygons.cursor);
	procedure write_cutout (cursor : in et_packages.pac_doc_cutouts.cursor);
	
-- KEEPOUT
	procedure write_line (cursor : in et_packages.type_keepout_lines.cursor);
	procedure write_arc (cursor : in et_packages.type_keepout_arcs.cursor);
	procedure write_circle (cursor : in et_packages.type_keepout_circles.cursor);
	procedure write_polygon (cursor : in et_packages.type_keepout_polygons.cursor);
	procedure write_cutout (cursor : in et_packages.pac_keepout_cutouts.cursor);

-- STOP MASK
	procedure write_line (cursor : in et_packages.type_stop_lines.cursor);
	procedure write_arc (cursor : in et_packages.type_stop_arcs.cursor);
	procedure write_circle (cursor : in et_packages.type_stop_circles.cursor);
	procedure write_polygon (cursor : in et_packages.type_stop_polygons.cursor);
	procedure write_cutout (cursor : in et_packages.pac_stop_cutouts.cursor);

-- STENCIL (OR SOLDER PASTE MASK)
	procedure write_line (cursor : in et_packages.type_stencil_lines.cursor);
	procedure write_arc (cursor : in et_packages.type_stencil_arcs.cursor);
	procedure write_circle (cursor : in et_packages.type_stencil_circles.cursor);	
	procedure write_polygon (cursor : in et_packages.type_stencil_polygons.cursor);
	procedure write_cutout (cursor : in et_packages.pac_stencil_cutouts.cursor);
	
-- ROUTE RESTRICT
	procedure write_line (cursor : in et_packages.type_route_restrict_lines.cursor);
	procedure write_arc (cursor : in et_packages.type_route_restrict_arcs.cursor);
	procedure write_circle (cursor : in et_packages.type_route_restrict_circles.cursor);	
	procedure write_polygon (cursor : in et_packages.type_route_restrict_polygons.cursor);
	procedure write_cutout (cursor : in et_packages.pac_route_restrict_cutouts.cursor);

-- VIA RESTRICT
	procedure write_line (cursor : in et_packages.type_via_restrict_lines.cursor);
	procedure write_arc (cursor : in et_packages.type_via_restrict_arcs.cursor);
	procedure write_circle (cursor : in et_packages.type_via_restrict_circles.cursor);	
	procedure write_polygon (cursor : in et_packages.type_via_restrict_polygons.cursor);
	procedure write_cutout (cursor : in et_packages.pac_via_restrict_cutouts.cursor);
	

-- BOARD CONTOUR
	procedure write_line (cursor : in et_packages.type_pcb_contour_lines.cursor);
	procedure write_arc (cursor : in et_packages.type_pcb_contour_arcs.cursor);
	procedure write_circle (cursor : in et_packages.type_pcb_contour_circles.cursor);	
	procedure write_line (cursor : in et_pcb.type_pcb_contour_lines.cursor);	
	procedure write_arc (cursor : in et_pcb.type_pcb_contour_arcs.cursor);
	procedure write_circle (cursor : in et_pcb.type_pcb_contour_circles.cursor);


	type type_section is ( -- of a package
		SEC_CONTOURS, -- of fill and cutout zones
		SEC_CUTOUT_ZONE,
		SEC_INIT,
		SEC_TOP,
		SEC_BOTTOM,
		SEC_LINE,
		SEC_ARC,
		SEC_CIRCLE,
		SEC_SILK_SCREEN,
		SEC_ASSEMBLY_DOCUMENTATION,
		SEC_KEEPOUT,
		SEC_COPPER,
		SEC_STOP_MASK,
		SEC_STENCIL,
		SEC_ROUTE_RESTRICT,
		SEC_VIA_RESTRICT,
		SEC_PCB_CONTOURS_NON_PLATED,
		SEC_TERMINALS,
		SEC_TERMINAL,
		SEC_PAD_CONTOURS_SMT,
		SEC_PAD_CONTOURS_THT,		
		SEC_MILLINGS,
		SEC_TEXT,
		SEC_PLACEHOLDER,
		SEC_FILL_ZONE,
		SEC_PACKAGE_3D_CONTOURS
		);

	procedure create_package (
	-- Creates a package and stores the package in container et_packages.packages.								 
		package_name 	: in et_packages.type_package_model_file.bounded_string; -- libraries/packages/S_SO14.pac
		appearance		: in et_packages.type_package_appearance;
		log_threshold	: in et_string_processing.type_log_level);

	procedure save_package (
	-- Saves the given package model in a file specified by file_name.							   
		file_name 		: in et_packages.type_package_model_file.bounded_string; -- libraries/packages/S_SO14.pac
		packge			: in et_packages.type_package; -- the actual device model
		log_threshold	: in et_string_processing.type_log_level);
	
	procedure read_package (
	-- Opens the package file and stores the package in container et_packages.packages.
		file_name 		: in et_packages.type_package_model_file.bounded_string; -- libraries/packages/S_SO14.pac
		log_threshold	: in et_string_processing.type_log_level);

	
end pcb_rw;
