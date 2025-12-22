------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     BOARD OPERATIONS / CONDUCTOR OBJECTS                 --
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

with et_conductor_segment.boards;		use et_conductor_segment.boards;
with et_fill_zones.boards;				use et_fill_zones.boards;
with et_conductor_text.boards;			use et_conductor_text.boards;
with et_pcb_placeholders;				use et_pcb_placeholders;
with et_device_name;					use et_device_name;



package et_board_ops.conductors is

	-- CS rework procedures so that a module cursor
	-- is used instead the module_name.

	use et_board_text;
	use pac_text_board;
	use pac_net_name;
	use pac_grid;


-- LINES:
	
	-- Adds a line track segment to the given net in the given module.
	-- The given net must exist:
	procedure add_line (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- reset_n
		line			: in type_conductor_line;
		log_threshold	: in type_log_level);


	
	-- Draws a track line. If net_name is empty (default) 
	-- then a freetrack will be drawn.
	procedure add_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string := et_net_names.no_name; -- reset_n
		line			: in type_conductor_line;
		log_threshold	: in type_log_level);

	
	-- Draws a named track line.
	-- Assumes that module_cursor and net_cursor point to existing objects.
	--procedure add_line (
		--module_cursor	: in pac_generic_modules.cursor;
		--net_cursor		: in et_schematic.pac_nets.cursor; -- reset_n
		--line			: in type_conductor_line;
		--log_threshold	: in type_log_level);

	
	-- Draws a track starting at a terminal. The track ends
	-- after the given length in given direction.
	-- If the terminal is a THT type, then the track may start at any signal layer.
	-- If the terminal is a SMT type, then the track may start at either the top or bottom
	-- signal layer. If operator indeed whishes an inner layer a warning is issued.
	procedure add_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in type_signal_layer;
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
	procedure add_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in type_signal_layer;
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
	procedure add_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in type_signal_layer;
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
	procedure add_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
		terminal		: in pac_terminal_name.bounded_string;
		axis			: in type_axis_2d;
		notches			: in type_grid_notches;
		log_threshold	: in type_log_level);



	-- If line segments of a net are searched, then they can be
	-- identified additionally by the associated net:
	type type_object_line_net is record
		net_cursor	: pac_nets.cursor;
		line_cursor	: pac_conductor_lines.cursor;
	end record;



	-- CS do the same for lines, arcs, circles of freetracks

	-- If floating line segments (of a freetrack) are searched, 
	-- then they can be identified by a cursor:
	type type_object_line_floating is record
		line_cursor	: pac_conductor_lines.cursor;
	end record;

	
	
	package pac_object_lines is new doubly_linked_lists (type_object_line_net);


	-- Modifies the status flag of a line of a net:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in type_object_line_net;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);

	
	-- Modifies the status flag of a floating line:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in type_object_line_floating;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);


	
	-- Returns all lines in the given signal layer
	-- in the vicinity of the given point.
	-- NOTE: This is about line connected with nets:
	function get_lines (
		module_cursor	: in pac_generic_modules.cursor;
		layer			: in type_signal_layer;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_object_lines.list;



	
	-- Returns all line of freetracks 
	-- in the given signal layer
	-- in the vicinity of the given point.
	function get_lines (
		module_cursor	: in pac_generic_modules.cursor;
		layer			: in type_signal_layer;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_conductor_lines.list;

	-- CS do the same for and circles

	

	
	-- Modifies that status flag of a line (see package et_object_status).
	-- If freetracks is false, then only net segments are adressed.
	-- If freetracks is true, then only freetracks are adressed:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		line_cursor		: in pac_conductor_lines.cursor;
		operation		: in type_status_operation;
		freetracks		: in boolean;
		log_threshold	: in type_log_level);


	
	-- Sets the proposed-flag of all lines which are
	-- in the given zone around the given place
	-- If freetracks is false, then only nets are adressed.
	-- If freetracks is true, then only freetracks are adressed:
	-- Adds to count the number of lines that have been found:
	procedure propose_lines (
		module_cursor	: in pac_generic_modules.cursor;
		layer			: in type_signal_layer;
		catch_zone		: in type_catch_zone;
		count			: in out natural; -- the number of affected lines
		freetracks		: in boolean;
		log_threshold	: in type_log_level);


	
	-- Clears the proposed-flag and the selected-flag of all lines.
	-- If freetracks is false, then only nets are adressed.
	-- If freetracks is true, then only freetracks are adressed:
	procedure reset_proposed_lines (
		module_cursor	: in pac_generic_modules.cursor;
		freetracks		: in boolean;							   
		log_threshold	: in type_log_level);



	
	-- Returns the first line according to the given flag.
	-- If no line has been found,
	-- then the selector line_cursor in the return is no_element
	-- and the selector net_cursor is no_element.
	function get_first_line_net (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_line_net;


	-- Returns the first line according to the given flag.
	-- If no line has been found,
	-- then the selector line_cursor in the return is no_element:
	function get_first_line_floating (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_line_floating;

	
	
	-- Advances to the next proposed line, starting at
	-- the given line. Traverses through line segments and nets
	-- in a circular manner. If there are no
	-- proposed lines, then line assumes default values (no_element).
	-- If there is only one proposed line, then line is unchanged.
	-- CS last_item indicates that the last line has been reached.
	-- CS This procedure is currently not used:
	procedure next_proposed_line (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in out type_object_line_net;
		freetracks		: in boolean;
		-- CS last_item		: in out boolean;
		log_threshold	: in type_log_level);


	

	-- Moves a line of a net:
	procedure move_line_net (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in type_object_line_net;
		point_of_attack	: in type_vector_model;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);
	

	-- Moves a floating line:
	procedure move_line_floating (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in type_object_line_floating;
		point_of_attack	: in type_vector_model;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);



	
	-- Deletes the given line segment in the given net.
	-- If the net or the segment does not exist then
	-- nothing happens and an error message is logged:
	procedure delete_line_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- reset_n
		line			: in type_conductor_line;
		log_threshold	: in type_log_level);


	-- Deletes the given freetrack line.
	-- If the line does not exist then
	-- nothing happens:
	procedure delete_line_floating (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in type_conductor_line;
		log_threshold	: in type_log_level);


	

	
-- ARCS:
	
	-- Draws a track arc. If net_name is empty a freetrack will be drawn.
	procedure add_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		arc				: in type_conductor_arc;
		log_threshold	: in type_log_level);

	
	-- If arc segments of a net are searched, then they can be
	-- identified additionally by the associated net:
	type type_object_arc_net is record
		net_cursor	: pac_nets.cursor;
		arc_cursor	: pac_conductor_arcs.cursor;
	end record;


	-- If arc segments (of a freetrack) are searched, then they can be
	-- identified by a cursor:
	type type_object_arc_floating is record
		arc_cursor	: pac_conductor_arcs.cursor;
	end record;



	-- Modifies the status flag of an arc of a net:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		arc				: in type_object_arc_net;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);

	
	-- Modifies the status flag of a floating arc:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		arc				: in type_object_arc_floating;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);



	-- Sets the proposed-flag of all arcs which are
	-- in the given zone around the given place
	-- If freetracks is false, then only nets are adressed.
	-- If freetracks is true, then only freetracks are adressed:
	-- Adds to count the number of arcs that have been found:
	procedure propose_arcs (
		module_cursor	: in pac_generic_modules.cursor;
		layer			: in type_signal_layer;
		catch_zone		: in type_catch_zone;
		count			: in out natural; -- the number of affected arcs
		freetracks		: in boolean;
		log_threshold	: in type_log_level);

	
	
	-- Clears the proposed-flag and the selected-flag of all arcs.
	-- If freetracks is false, then only nets are adressed.
	-- If freetracks is true, then only freetracks are adressed:
	procedure reset_proposed_arcs (
		module_cursor	: in pac_generic_modules.cursor;
		freetracks		: in boolean;							   
		log_threshold	: in type_log_level);



	-- Returns the first arc according to the given flag.
	-- If no arc has been found,
	-- then the selector arc_cursor in the return is no_element
	-- and the selector net_cursor is no_element.
	function get_first_arc_net (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_arc_net;


	-- Returns the first arc according to the given flag.
	-- If no arc has been found,
	-- then the selector arc_cursor in the return is no_element:
	function get_first_arc_floating (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_arc_floating;




	-- Moves a arc of a net:
	procedure move_arc_net (
		module_cursor	: in pac_generic_modules.cursor;
		arc				: in type_object_arc_net;
		point_of_attack	: in type_vector_model;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);
	

	-- Moves a floating arc:
	procedure move_arc_floating (
		module_cursor	: in pac_generic_modules.cursor;
		arc				: in type_object_arc_floating;
		point_of_attack	: in type_vector_model;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);



	-- Deletes the given arc segment in the given net.
	-- If the net or the segment does not exist then
	-- nothing happens and an error message is logged:
	procedure delete_arc_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- reset_n
		arc				: in type_conductor_arc;
		log_threshold	: in type_log_level);


	-- Deletes the given freetrack arc.
	-- If the arc does not exist then
	-- nothing happens:
	procedure delete_arc_floating (
		module_cursor	: in pac_generic_modules.cursor;
		arc				: in type_conductor_arc;
		log_threshold	: in type_log_level);

	
	
-- TRACKS:
	
	-- Deletes the track segment that crosses the given point in given layer.
	-- If a net name is given, then net segments are affected.
	-- If the given name is empty, then only freetrack segments
	-- are targeted.
	-- CS currently deletes the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
	procedure delete_track (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in type_signal_layer;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level);

	


	-- Deletes all segments of the given net:
	procedure ripup_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- reset_n
		log_threshold	: in type_log_level);





-- FILL ZONES


	
	-- This composite type helps to identify a
	-- segment of a zone that is connected with a net:
	type type_object_segment_net (
		fill_style : type_fill_style := SOLID) 
	is record
		segment	: pac_contours.pac_segments.cursor;
		net		: pac_nets.cursor;
		
		case fill_style is
			when SOLID =>
				zone_solid : pac_route_solid.cursor;
				
			when HATCHED =>
				zone_hatched : pac_route_hatched.cursor;
		end case;
	end record;


	
	-- This composite type helps to identify a
	-- segment of a floating zone:
	type type_object_segment_floating (
		fill_style : type_fill_style := SOLID) 
	is record
		segment	: pac_contours.pac_segments.cursor;
		
		case fill_style is
			when SOLID =>
				zone_solid : pac_floating_solid.cursor;
				
			when HATCHED =>
				zone_hatched : pac_floating_hatched.cursor;
		end case;
	end record;



	
	-- Modifies the status flag of segment of a zone
	-- that is connected with a net:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment_net;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);


	-- Modifies the status flag of segment of a
	-- floating zone:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment_floating;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);

	
	
	-- Sets the proposed-flag of all line and arc segments 
	-- of zones which are connected with nets and which are
	-- in the given zone around the given place.
	-- Adds to count the number of segments that have been found:
	procedure propose_segments_net (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		layer			: in type_signal_layer;
		count			: in out natural;
		log_threshold	: in type_log_level);

	
	-- Sets the proposed-flag of all line and arc segments 
	-- of floating zones and which are
	-- in the given zone around the given place.
	-- Adds to count the number of segments that have been found:
	procedure propose_segments_floating (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		layer			: in type_signal_layer;
		count			: in out natural;
		log_threshold	: in type_log_level);


	
	-- Clears the proposed-flag and the selected-flag 
	-- of all line and arc segments of zones which are connected with a net:
	procedure reset_proposed_segments_net (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);


	-- Clears the proposed-flag and the selected-flag 
	-- of all line and arc segments of floating zones:
	procedure reset_proposed_segments_floating (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);


	
	
	-- Returns the first segment of a
	-- connected zone according to the given flag.
	-- If no segment has been found, then the return 
	-- in all components of the return is no_element:
	function get_first_segment_net (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object_segment_net;


	-- Returns the first segment of a
	-- floating zone according to the given flag.
	-- If no segment has been found, then the return 
	-- in all components of the return is no_element:
	function get_first_segment_floating (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object_segment_floating;


	
	-- Moves a contour segment of a connected zone:
	-- CS currently it moves only a single segment.
	-- CS provide parameter for move mode (move attached segments, move whole contour)
	procedure move_segment_net (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment_net;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);

	
	-- Moves a contour segment of a floating zone:
	-- CS currently it moves only a single segment.
	-- CS provide parameter for move mode (move attached segments, move whole contour)
	procedure move_segment_floating (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment_floating;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);

	
	-- Deletes a contour segment of a connected zone:
	procedure delete_segment_net (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment_net;
		log_threshold	: in type_log_level);


	-- Deletes a contour segment of a floating zone:
	procedure delete_segment_floating (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment_floating;
		log_threshold	: in type_log_level);

	
	
-- TEXTS:


	-- Places a text in a conductor layer:
	procedure add_text (
		module_cursor	: in pac_generic_modules.cursor;
		signal_layer	: in type_signal_layer;
		text			: in type_text_fab_with_content;
		log_threshold	: in type_log_level);

	
	
	-- Returns all texts in the vicinity of the given point:
	function get_texts (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_conductor_texts.list;


	-- Moves a text:
	procedure move_text (
		module_cursor	: in pac_generic_modules.cursor;
		text			: in type_conductor_text;
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model;
		log_threshold	: in type_log_level);


	-- This type helps to identify a text by its cursor:
	type type_object_text is record
		cursor	: pac_conductor_texts.cursor := pac_conductor_texts.no_element;
	end record;

	
	-- This procedure sets the status flag of the
	-- given text object:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		text			: in type_object_text;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);


	-- Sets the proposed-flag of all texts which have their
	-- origin (or anchor point) in the given zone around the given place.
	-- Adds to count the number of texts that have been found:
	procedure propose_texts (
		module_cursor	: in pac_generic_modules.cursor;
		layer			: in type_signal_layer;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level);


	procedure move_text (
		module_cursor	: in pac_generic_modules.cursor;
		text			: in type_object_text;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);


	procedure delete_text (
		module_cursor	: in pac_generic_modules.cursor;
		text			: in type_object_text;
		log_threshold	: in type_log_level);



	function get_first_text (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object_text;


	-- Clears the proposed-flag and the selected-flag 
	-- of all texts:
	procedure reset_proposed_texts (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);



	
-- TEXT PLACEHOLDERS:

	-- Places a text placeholder in a conductor layer.
	-- The caller must take care for mirroring the placeholder
	-- in case its signal layer is the bottom of the board:
	procedure add_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		placeholder		: in type_text_placeholder_conductors;
		log_threshold	: in type_log_level);
	

	-- This type helps to identify a text placeholder by its cursor:
	type type_object_placeholder is record
		cursor	: pac_text_placeholders_conductors.cursor := 
			pac_text_placeholders_conductors.no_element;
	end record;
	

	-- This procedure sets the status flag of the
	-- given placeholder object:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		placeholder		: in type_object_placeholder;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);


	-- Sets the proposed-flag of all placeholders which have their
	-- origin (or anchor point) in the given zone around the given place.
	-- Adds to count the number of placeholders that have been found:
	procedure propose_placeholders (
		module_cursor	: in pac_generic_modules.cursor;
		layer			: in type_signal_layer;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level);
	

	procedure move_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		placeholder		: in type_object_placeholder;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);


	procedure delete_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		placeholder		: in type_object_placeholder;
		log_threshold	: in type_log_level);
 
 
 
	function get_first_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object_placeholder;
 
 
	-- Clears the proposed-flag and the selected-flag 
	-- of all placeholders:
	procedure reset_proposed_placeholders (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);
 


	

-- OBJECTS:
	

	-- When objects are handled then we need these
	-- categories in order to store them in indefinite_doubly_linked_lists:
	type type_object_category is (
		CAT_VOID,
		CAT_LINE_NET,
		CAT_ARC_NET,
		CAT_LINE_FLOATING,
		CAT_ARC_FLOATING,
		CAT_ZONE_SEGMENT_NET,
		CAT_ZONE_SEGMENT_FLOATING,
		CAT_TEXT,
		CAT_PLACEHOLDER
		);
	-- CS CAT_CIRCLE

	
	-- This type wraps segments of zones, lines, arcs, circles, 
	-- texts, placeholders into a single type:
	type type_object (cat : type_object_category) is record
		case cat is
			when CAT_VOID => null;
			
			when CAT_ZONE_SEGMENT_NET =>
				segment_net			: type_object_segment_net;
				
			when CAT_ZONE_SEGMENT_FLOATING =>
				segment_floating	: type_object_segment_floating;
				
			when CAT_LINE_NET =>
				line_net			: type_object_line_net;
				
			when CAT_ARC_NET =>
				arc_net				: type_object_arc_net;

			when CAT_LINE_FLOATING =>
				line_floating		: type_object_line_floating;
				
			when CAT_ARC_FLOATING =>
				arc_floating		: type_object_arc_floating;

			when CAT_TEXT =>
				text				: type_object_text;

			when CAT_PLACEHOLDER =>
				placeholder			: type_object_placeholder;
		end case;
	end record;

	package pac_objects is new indefinite_doubly_linked_lists (type_object);


	-- Returns the number of items stored in the given list:
	function get_count (
		objects : in pac_objects.list)
		return natural;
	

	-- Returns the first object (line, arc, circle, zone segment, text,
	-- placeholder) according to the given flag.
	-- If nothing found, then the return is a void object (CAT_VOID):
	function get_first_object (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object;



	-- Collects all objects (lines, arcs, circles, zone segments)
	-- according to the given flag and returns them in a list:
	function get_objects (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return pac_objects.list;



	-- Modifies the status flag of an object:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);


	
	-- Modifies the status flag of an object indicated by a cursor:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object_cursor	: in pac_objects.cursor;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);



	procedure move_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);



	-- CS
	-- Deletes the object that is in the vicinity of the given point.
	-- CS currently deletes the object found. Leaves other objects untouched.
	-- CS a parameter like "all" to delete all objects in the vicinity of point.
	-- procedure delete_object (
	--  module_cursor	: in pac_generic_modules.cursor;
	-- 	layer			: in type_signal_layer;
	-- 	point			: in type_vector_model; -- x/y
	-- 	accuracy		: in type_accuracy;
	-- 	log_threshold	: in type_log_level);



	-- Deletes an object as given by object.
	-- If the object indicates a segment of a net,
	-- then segment will be ripped up according to the
	-- current gloabl ripup_mode:
	procedure delete_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level);


	
	
	
	-- This is a collective procedure that resets
	-- the proposed-flag and the selected-flag 
	-- of texts, lines, arcs, circles and zone segments:
	procedure reset_proposed_objects ( -- CS rename to reset_status_objects
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);

	
end et_board_ops.conductors;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
