------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              BOARD READ                                  --
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

-- with ada.containers;            use ada.containers;

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

package et_board_read is

	use pac_geometry_2;
	use pac_contours;
	use pac_text_board;
	


	
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

	
end et_board_read;
