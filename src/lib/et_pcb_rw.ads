------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         PCB READ AND WRITE                               --
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

with ada.containers;            use ada.containers;

with et_primitive_objects;		use et_primitive_objects;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_general_rw;				use et_general_rw;
with et_pcb_sides;				use et_pcb_sides;
with et_board_coordinates;		use et_board_coordinates;
with et_board_text;				use et_board_text;
with et_drills;					use et_drills;
with et_pcb;
with et_pcb_contour;
with et_pcb_stack;
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

package et_pcb_rw is

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
	procedure write_signal_layer (layer : in et_pcb_stack.type_signal_layer);
	procedure write_fill_style (fill_style : in type_fill_style);
	procedure write_fill_status (filled : in type_filled);
	procedure write_pad_connection (connection : in type_pad_connection);
	procedure write_pad_technology (techno : in type_pad_technology);	
	procedure write_signal_layers (layers : in et_pcb_stack.type_signal_layers.set);
	procedure write_circle_conductor (circle : in et_conductor_segment.type_conductor_circle);

	-- Writes the properties of a circle in conductor as used in a freetrack:
	procedure write_circle_conductor (circle : in et_conductor_segment.boards.type_conductor_circle);	
	
	
	-- writes the segments of a polygon (lines, arcs or a single circle):
	procedure write_polygon_segments (
		polygon : in type_contour'class);

	
	
	-- Returns a type_point_2d in the the layout.
	function to_position ( -- CS combine with next function to_position using the tag test ?
		line : in type_fields_of_line; -- "start x 44.5 y 53.5"
		from : in type_field_count_positive)
		return type_vector_model;

	
	-- Returns a type_position in the layout.
	function to_position (
		line : in type_fields_of_line; -- "x 23 y 0.2 rotation 90.0"
		from : in type_field_count_positive)
		return type_position;

	
	--function position (point : in type_vector_model'class) return string;
	function position (point : in type_position'class) return string; -- CS rename to to_string


	-- This function processes a line starting 
	-- from a given position and returns a grid spacing.
	-- Since both board and packagel read operations require
	-- this function, it is placed in this package:
	function to_grid_spacing (
		line : in type_fields_of_line; -- spacing x 1 y 1
		from : in type_field_count_positive)
		return type_vector_model;

	
	-- Issues a warning that the given signal layer is deeper than the deepest
	-- signal layer of the pcb stack.
	procedure signal_layer_invalid (
		line			: in type_fields_of_line;
		signal_layer	: in et_pcb_stack.type_signal_layer;
		check_layers	: in et_pcb_stack.type_layer_check);


	
	-- Converts a line like "layers 1 4 17" or "layers [1,3,4-9]" to 
	-- a set of signal layers.
	-- Issues warning if a layer number occurs more than once.
	-- If layer check requested, issues warning if a layer id is greater than the 
	-- deepest layer used (given in argument check_layer).
	function to_layers (
		line 			: in type_fields_of_line; -- layers 1 3 17
		check_layers	: in et_pcb_stack.type_layer_check)
		return et_pcb_stack.type_signal_layers.set;	
	


-- BASIC GEOMETRIC OBJECTS USED IN PACKAGES AND BOARDS
	
	board_line : type_line;
	procedure board_reset_line;
	procedure add_polygon_line (l : in out type_line);

	board_arc : type_arc;
	procedure board_reset_arc;
	procedure add_polygon_arc (a : in out type_arc);

	board_circle : type_circle;
	procedure board_reset_circle;
	procedure add_polygon_circle (c : in out type_circle);
	
	
	-- Reads start and end point of the board_line. 
	-- If the statement is invalid then an error issued.
	procedure read_board_line (
		line : type_fields_of_line);

	-- Reads start and end point of the board_line.
	-- If the statement is invalid then it returns a false.
	function read_board_line (
		line : type_fields_of_line)
		return boolean;

	-- Checks whether start and end point of given arc have same distance from center.
	procedure board_check_arc (
		log_threshold	: in type_log_level);
	
	procedure read_board_arc (line : type_fields_of_line);
	-- Reads start and end point of the board_arc. If the statement is invalid then an error issued.

	function read_board_arc (line : type_fields_of_line) return boolean;
	-- Reads start and end point of the board_arc. If the statement is invalid then it returns a false.
	
	procedure read_board_circle (line : type_fields_of_line);
	-- Reads start and end point of the board_circle. If the statement is invalid then an error issued.

	function read_board_circle (line : type_fields_of_line) return boolean;
	-- Reads start and end point of the board_circle. If the statement is invalid then it returns a false.
	
	board_fill_style : type_fill_style := fill_style_default;
	board_filled : type_filled := filled_default;
	
	fill_spacing : type_track_clearance := type_track_clearance'first; -- CS rename to board_conductor_fill_spacing
	--board_hatching : type_hatching;
	board_easing : type_easing;

	


	-- Ensures that the polygon outline is properly closed. Issues a warning
	-- if it detects gaps.
	procedure check_outline (
		polygon			: in type_contour;
		log_threshold	: in type_log_level);


	-- A temporarily storage place when reading a contour:
	contour : type_contour;
	
	
	polygon_isolation : type_track_clearance := type_track_clearance'first;
	polygon_width_min : type_track_width := type_track_width'first;

	-- board relevant only:
	pad_connection		: type_pad_connection := type_pad_connection'first;
	contour_priority	: type_priority := type_priority'first;
	relief_properties	: type_relief_properties;
	signal_layer		: et_pcb_stack.type_signal_layer := et_pcb_stack.type_signal_layer'first;

	procedure board_reset_signal_layer;


	

	
	
	board_line_width : type_distance_positive := linewidth_fab_min;

	procedure board_reset_line_width;


	
	-- This procdure resets contour properties to their defaults.
	-- This procdure is used by both package and board parsing procedures 
	-- read_package and read_module_file.
	-- Some properties have no meaning in connection with device packages.
	procedure board_reset_contour;

	
	section_zone		: constant string := "[ZONE";
	section_cutout_zone	: constant string := "[CUTOUT_ZONE";
	section_contours	: constant string := "[CONTOURS";

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
	
	
end et_pcb_rw;
