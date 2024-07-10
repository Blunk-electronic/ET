------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     BOARD OPERATIONS / CONDUCTOR OBJECTS                 --
--                                                                          --
--                               S p e c                                    --
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
--   ToDo: 


with ada.containers;   			       	use ada.containers;
with ada.containers.doubly_linked_lists;


with et_text;
with et_conductor_segment.boards;		use et_conductor_segment.boards;
with et_fill_zones;						use et_fill_zones;
with et_conductor_text.boards;			use et_conductor_text.boards;


package et_board_ops.conductors is

	-- CS rework procedures so that a module cursor
	-- is used instead the module_name.

	use et_board_shapes_and_text;
	use pac_text_board;
	use pac_net_name;


-- TRACKS:
	
	-- Adds a line track segment to the given net in the given module.
	-- The given net must exist:
	procedure add_named_track (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- reset_n
		line			: in type_conductor_line;
		log_threshold	: in type_log_level);


	
	-- Draws a track line. If net_name is empty (default) 
	-- then a freetrack will be drawn.
	procedure draw_track_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string := no_name; -- reset_n
		line			: in type_conductor_line;
		log_threshold	: in type_log_level);

	
	-- Draws a named track line.
	-- Assumes that module_cursor and net_cursor point to existing objects.
	--procedure draw_track_line (
		--module_cursor	: in pac_generic_modules.cursor;
		--net_cursor		: in et_schematic.pac_nets.cursor; -- reset_n
		--line			: in type_conductor_line;
		--log_threshold	: in type_log_level);

	
	-- Draws a track starting at a terminal. The track ends
	-- after the given length in given direction.
	-- If the terminal is a THT type, then the track may start at any signal layer.
	-- If the terminal is a SMT type, then the track may start at either the top or bottom
	-- signal layer. If operator indeed whishes an inner layer a warning is issued.
	procedure draw_track_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
		terminal		: in pac_terminal_name.bounded_string;
		direction		: in type_rotation_model;
		length			: in type_distance_positive;
		log_threshold	: in type_log_level);

	
	-- Draws a track starting at a terminal. The track ends
	-- after the given number of notches along the given axis.
	-- If the terminal is a THT type, then the track may start at any signal layer.
	-- If the terminal is a SMT type, then the track may start at either the top or bottom
	-- signal layer. If operator indeed whishes an inner layer a warning is issued.
	procedure draw_track_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
		terminal		: in pac_terminal_name.bounded_string;
		direction		: in type_rotation_model;
		axis			: in type_axis_2d;
		notches			: in type_grid_notches;
		log_threshold	: in type_log_level);

	
	-- Draws a track starting at a terminal. The track ends at the given point.
	-- If the terminal is a THT type, then the track may start at any signal layer.
	-- If the terminal is a SMT type, then the track may start at either the top or bottom
	-- signal layer. If operator indeed whishes an inner layer a warning is issued.								  
	procedure draw_track_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
		terminal		: in pac_terminal_name.bounded_string;
		end_point		: in type_vector_model;
		log_threshold	: in type_log_level);

	
	-- Draws a track starting at a terminal. The track runs into the 
	-- given direction and ends after the given number of notches along the given axis.
	-- If the terminal is a THT type, then the track may start at any signal layer.
	-- If the terminal is a SMT type, then the track may start at either the top or bottom
	-- signal layer. If operator indeed whishes an inner layer a warning is issued.
	procedure draw_track_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
		terminal		: in pac_terminal_name.bounded_string;
		axis			: in type_axis_2d;
		notches			: in type_grid_notches;
		log_threshold	: in type_log_level);


	

	
	-- If segment lines are connected in a certain area, then
	-- they can be identified by their parent net:
	type type_get_lines_result is record
		net		: pac_net_name.bounded_string;
		line	: type_conductor_line;
	end record;

	package pac_get_lines_result is new doubly_linked_lists (type_get_lines_result);
	
	-- Returns all lines in the given signal layer
	-- in the vicinity of the given point:
	function get_lines (
		module_cursor	: in pac_generic_modules.cursor;
		layer			: in et_pcb_stack.type_signal_layer;
		point			: in type_vector_model;
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_get_lines_result.list;



	-- Modifies that status flag of a line (see package et_object_status):
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		line_cursor		: in pac_conductor_lines.cursor;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);


	
	-- Sets the proposed-flag of all lines which are
	-- in the given zone around the given place.
	procedure propose_lines (
		module_cursor	: in pac_generic_modules.cursor;
		point			: in type_vector_model; -- x/y
		layer			: in et_pcb_stack.type_signal_layer;
		catch_zone		: in type_catch_zone; -- the circular area around the place
		count			: in out natural; -- the number of affected lines
		log_threshold	: in type_log_level);


	-- Clears the proposed-flag and the selected-flag of all lines:
	procedure reset_proposed_lines (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);



	
	type type_line_segment is record
		net_cursor	: pac_nets.cursor;
		line_cursor	: pac_conductor_lines.cursor;
	end record;
	
	-- Returns the first line according to the given flag.
	-- If no line has been found,
	-- then the selector "line" in the return is no_element
	-- and the selector "net" is empty:
	function get_first_line (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_line_segment;


	-- Advances to the next proposed line, starting at
	-- the given line. Traverses through line segments and nets
	-- in a circular manner. If there are no
	-- proposed lines, then line assumes default values (no_element).
	-- If there is only one proposed line, then line is unchanged.
	-- CS last_item indicates that the last line has been reached:
	procedure next_proposed_line (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in out type_line_segment;
		-- CS last_item		: in out boolean;
		log_threshold	: in type_log_level);


	

	-- Moves a line segment.
	-- If the net name is given, then the process consumes
	-- less time. The given net must exist. Otherwise an exception
	-- is raised:
	procedure move_line (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in type_conductor_line;
		point_of_attack	: in type_vector_model;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level;
		net_name		: in pac_net_name.bounded_string := no_name); -- reset_n

	
	
	-- Draws a track arc. If net_name is empty a freetrack will be drawn.
	procedure draw_track_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		arc				: in type_conductor_arc;
		log_threshold	: in type_log_level);

	
	
	-- Rips up the track segment of a net that crosses the given point in given layer.
	-- CS currently rips up the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
	procedure ripup_track_segment (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		point			: in type_vector_model; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level);


	-- Rips up the given line segment in the given net.
	-- If the net or the segment does not exist then
	-- nothing happens and an error message is logged:
	procedure ripup_line_segment (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- reset_n
		line			: in type_conductor_line;
		log_threshold	: in type_log_level);


	-- Rips up all segments of the given net:
	procedure ripup_all_segments (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- reset_n
		log_threshold	: in type_log_level);

	

-- FILL ZONES
	
	procedure place_fill_zone (
		module_cursor	: in pac_generic_modules.cursor;
		zone			: in type_zone'class;
		log_threshold	: in type_log_level;

		-- Net name is relevant if filil zone is part of a route.
		-- The type of the given fill zone is the cirteria:
		net_name		: in pac_net_name.bounded_string := no_name);


	-- Fills fill zones. If nets is empty, then all
	-- zones will be filled (even those who are floating).
	-- If nets contains net names then only the zones of these
	-- nets will be filled:
	procedure fill_zones (
		module_cursor	: in pac_generic_modules.cursor;	
		log_category	: in type_log_category;
		log_threshold	: in type_log_level;
		nets 			: in pac_net_names.list := no_net_names); -- GND, GNDA, P3V3, ...

	
	
-- TEXTS:
	
	-- Places a text in a conductor layer.
	procedure place_text_in_conductor_layer (
		module_cursor	: in pac_generic_modules.cursor;
		signal_layer	: in type_signal_layer;
		text			: in type_text_fab_with_content;
		log_threshold	: in type_log_level);

	
	-- Returns all texts in the vicinity of the given point:
	function get_texts (
		module_cursor	: in pac_generic_modules.cursor;
		point			: in type_vector_model;
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_conductor_texts.list;


	-- Moves a text:
	procedure move_text (
		module_cursor	: in pac_generic_modules.cursor;
		text			: in type_conductor_text;
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model;
		log_threshold	: in type_log_level);

	
end et_board_ops.conductors;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
