------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              MODULE_RW                                   --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

with ada.containers;            use ada.containers;

with et_schematic;
with et_string_processing;
with et_packages;
with et_pcb;
with et_pcb_coordinates;
with et_pcb_stack;

package module_rw is

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

	
	
-- BASIC GEOMETRIC OBJECTS USED IN SYMBOLS AND SCHEMATICS
	schematic_object_filled : et_schematic.shapes.type_filled := et_schematic.shapes.filled_default;		


-- BASIC GEOMETRIC OBJECTS USED IN PACKAGES AND BOARDS
	
	type type_board_line is new et_packages.shapes.type_line with null record;

	procedure add_polygon_line (line : in out type_board_line);

	board_line : type_board_line;

	procedure board_reset_line;

	
	type type_board_arc is new et_packages.shapes.type_arc with null record;

	procedure add_polygon_arc (arc : in out type_board_arc);

	board_arc : type_board_arc;

	procedure board_reset_arc;

	
	type type_board_circle is new et_packages.shapes.type_circle with null record;

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
	board_filled : et_packages.shapes.type_filled := et_packages.shapes.filled_default;

	board_hatching : et_packages.type_hatching;
	board_hatching_copper : et_packages.type_hatching_copper;
	board_easing : et_packages.type_easing;

	
	type type_polygon is new et_packages.shapes.type_polygon_base with null record;
	polygon : type_polygon;
	
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
		circle				: in et_packages.shapes.type_circle;
		filled				: in et_packages.shapes.type_filled;
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

	
end module_rw;
