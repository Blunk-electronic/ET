------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS SCHEMATIC NETS                            --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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
--                                                                          --
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
-- DESCRIPTION:
-- 

with ada.containers;				use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;

with et_net_names;					use et_net_names;
with et_geometry;					use et_geometry;
with et_general;					use et_general;
with et_canvas_general;				use et_canvas_general;
with et_canvas_schematic;
with et_coordinates;				use et_coordinates;
use et_coordinates.pac_geometry_2;

with et_text;
with et_project.modules;			use et_project.modules;
with et_symbols;
with et_net_labels;					use et_net_labels;
with et_nets;						use et_nets;
with et_schematic;					use et_schematic;
with et_schematic_ops;
with et_schematic_ops.nets;			use et_schematic_ops.nets;
with et_string_processing;			use et_string_processing;
with et_logging;					use et_logging;

package et_canvas_schematic_nets is

	use et_canvas_schematic.pac_canvas;
	
	use et_project.modules.pac_generic_modules;

	use pac_net_name;
	use pac_nets;
	use pac_strands;
	use pac_net_segments;

	-- Whenever a segment is selected via the GUI, we store its
	-- parent net, strand and the segment itself via this type:
	type type_selected_segment is tagged record
		net		: pac_nets.cursor;
		strand	: pac_strands.cursor;
		segment	: pac_net_segments.cursor;
	end record;

	
	-- Deletes a selected segment of a net.
	procedure delete_selected_segment (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		segment			: in type_selected_segment; -- net/strand/segment
		log_threshold	: in type_log_level);

	
	package pac_proposed_segments is new doubly_linked_lists (type_selected_segment);
	use pac_proposed_segments;
	
	-- These variables are used by the GUI when the operator selects a segment:
	proposed_segments	: pac_proposed_segments.list;
	selected_segment	: pac_proposed_segments.cursor;

	
	-- Returns the name of the net of the selected_segment:
	function selected_net return pac_net_name.bounded_string;
	-- CS rename to get_selected_net

	
	-- Returns the position of the strand of the selected segment:
	function get_strand_position return et_coordinates.type_position;

	
	-- Clears the list of proposed segments.
	-- Resets selected_segment to no_element.
	procedure clear_proposed_segments;

	

	
	-- Returns the net name of the first segment in 
	-- given list of net segments.
	-- If the given list is empty then an empty net name will be returned.
	function first_net (
		segments : in pac_proposed_segments.list) 
		return pac_net_name.bounded_string; -- RESET_N, MASTER_CLOCK

	
	-- Returns true if segments contains more than one segment:
	function more_than_one (
		segments : in pac_proposed_segments.list)
		return boolean;

	
	-- Tests if all given segments belong to the same net. 
	-- Returns false if net names differ.
	function all_belong_to_same_net (
		segments	: in pac_proposed_segments.list)
		return boolean;

	
	-- Tests if point sits between start and end point of any of the given segments.
	-- Returns true in that case.
	function between_start_and_end_point_of_sloping_segment (
		point		: in type_point;
		segments	: in pac_proposed_segments.list)
		return boolean;

	
-- 	-- Deletes a selected segment of a net.
-- 	procedure delete_selected_segment (
-- 		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
-- 		segment			: in type_selected_segment; -- net/strand/segment
-- 		log_threshold	: in type_log_level);

	
	-- Collects all net segments in the vicinity of the given point:
	function collect_segments (
		module			: in pac_generic_modules.cursor;
		place			: in et_coordinates.type_position; -- sheet/x/y
		catch_zone		: in type_catch_zone := type_catch_zone'first; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_proposed_segments.list;



	
-- DELETE SEGMENT

	
	status_delete : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to delete net segment." 
		& status_hint_for_abort;

	
	-- Deletes a net segment in the vicinity of given point.
	-- If more than one segment near point found, then it sets the
	-- cursor selected_segment to the first segment and requests
	-- for clarification.
	procedure delete_net_segment (
		point : in type_point);

	
	-- Advances cursor selected_segment to next segment in list proposed_segments.
	procedure clarify_net_segment;

	
	-- Deletes the net segment being pointed at by cursor selected_segment.
	procedure delete_selected_net_segment;


	
-- DRAW NET SEGMENT

	status_draw_net : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& status_set_start_point 
		& status_hint_for_abort;

	
	-- When a net route is being drawn, then this global variable
	-- shall be used:
	type type_net_route is record
		path	: type_path_live;
		name	: pac_net_name.bounded_string := to_net_name ("");
	end record;

	route : type_net_route;

	-- Builds a live net route. This procedure requires to be called twice:
	-- first time for the start and the second time for the end point of the route.
	-- The current bend style in global variable "net_route" is taken into account.
	-- The route may be started and finished with different tools. For example start
	-- with MOUSE and finish with KEYBOARD or vice versa.
	procedure make_net_route (
		tool	: in type_tool;
		point	: in type_point);


	
	-- Resets the components of the net route.
	-- Exception: Leaves the bend style as it is.
	procedure reset_net_route;

	-- Inserts a net segment in the module.
	-- Deduces the name of the net to be extended by the
	-- start or end point of the segment. 
	-- 1. If the segment does not touch any other net, then an anonymous
	--    net name like N$2 will be generated for the new segment. This
	--    would then create a the first strand of net N$2.
	-- 2. If an explicit net name is provided via net_name_given, then:
	--    the new segment will be named after net_name_given.
	-- 3. If an explicit net name is provided via net_name_given and the
	--    net net_name_given already exists, then the new segment will 
	--    extend that net.
	-- 4. The start and end points of the segment to be inserted must
	--    have been validated beforehand  
	--    (via function valid_for_net_segment. see below.).
	-- 5. Calls et_schematic_ops.nets.insert_segment for the final
	--    insertion of the segment in the targeted net.
	procedure insert_net_segment (
		module			: in pac_generic_modules.cursor;
		net_name_given	: in pac_net_name.bounded_string; -- RESET_N
		sheet			: in type_sheet;
		segment			: in type_net_segment;
		log_threshold	: in type_log_level);

	
	-- Returns true if the given point qualifies as start or end point
	-- of a net segment.
	-- The point is considered as suitable if:
	-- - there are no segments at all or
	-- - if all segments there belong to the same net.
	-- If a sloping segment exists there, the return is false.
	function valid_for_net_segment (
		point			: in type_point;
		log_threshold	: in type_log_level)
		return boolean;

	

-- DRAG/MOVE NET SEGMENT

	type type_finalizing_granted is new boolean;

	-- Global information for the GUI when a segment is being
	-- moved or dragged:
	type type_preliminary_segment is record
		-- This flag tells the draw operations to draw the preliminary segment:
		ready					: boolean := false;

		-- This tells the GUI whether the mouse or the
		-- cursor position is to be used when drawing the segment:
		tool					: type_tool := MOUSE;
		
		point_of_attack			: type_point;
		finalizing_granted		: type_finalizing_granted := false;
	end record;

	preliminary_segment : type_preliminary_segment;


	-- This procedure:
	-- - Clears proposed segments (calls clear_proposed_segments).
	-- - resets the preliminary_segment to its default values
	procedure reset_preliminary_segment;

	
	
	status_drag : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to drag net segment." 
		& status_hint_for_abort;
	
	status_move : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to move net segment." 
		& status_hint_for_abort;




	
-- RENAME NET

	status_rename_net_strand : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to rename a single strand." 
		& status_hint_for_abort;	

	status_rename_net_sheet : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to rename all strands on this sheet." 
		& status_hint_for_abort;	

	status_rename_net_everywhere : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to rename all strands on all sheets." 
		& status_hint_for_abort;	

	type type_net_rename is record
		scope		: type_net_scope := SHEET; -- strand, sheet, everywhere
		-- position	: type_point; -- x/y where net segment was selected
	end record;

	net_rename : type_net_rename;
		
	procedure window_set_property;
	--procedure rename_selected_net;


	
	

	-- Assigns the given destination after the drag to the selected segment.
	-- Resets the global variable "segment".
	procedure finalize_drag (
		destination		: in type_point;
		log_threshold	: in type_log_level);
	
	-- Locates all net segments in the vicinity of given point.
	-- If more than one segment near point found, then it sets the
	-- cursor selected_segment to the first segment and requests
	-- for clarification.
	procedure find_segments (point : in type_point);

	procedure drag_segment (
		tool		: in type_tool;
		position	: in type_point);

	
-- NET LABELS

	-- Global information for the GUI when a label is being
	-- placed or moved:
	type type_label is record
		ready				: boolean := false;
		tool				: type_tool := MOUSE;
		appearance			: type_net_label_appearance := SIMPLE;
		rotation_simple		: et_text.type_rotation_documentation := et_text.HORIZONTAL;
		
		-- If we are dealing with a TAG label, then a permission
		-- is required to do the final placement.
		-- For a simple label this flag has no meaning because
		-- a simple net label can be placed anywhere.
		finalizing_granted	: type_finalizing_granted := false;
	end record;

	label : type_label;

	type type_selected_label is new type_selected_segment with record
		label : pac_net_labels.cursor;
	end record;

	package pac_proposed_labels is new indefinite_doubly_linked_lists (type_selected_label);
	use pac_proposed_labels;

	proposed_labels : pac_proposed_labels.list;
	selected_label : pac_proposed_labels.cursor;
	
	--procedure clear_proposed_labels;		
	
	
	-- This procedure:
	-- - Clears list of proposed net labels.
	-- - resets global variable "label" to its default values
	procedure reset_label;

	status_delete_label : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to delete label." 
		& status_hint_for_abort;	
	
	status_place_label : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to place label." 
		& status_hint_for_abort;	
	
	status_place_label_simple : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to place simple label." 
		& status_hint_for_abort;	

	status_place_label_tag : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to place tag label." 
		& status_hint_for_abort;	

	status_move_label : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to move simple label." 
		& status_hint_for_abort;	

	type type_label_category is (SIMPLE, TAG, BOTH);

	function to_string (cat : in type_label_category) return string;
	
	procedure delete_selected_label;

	procedure delete_label (point : in type_point);
	

	procedure clarify_label;
	
	procedure finalize_place_label (
		destination		: in type_point;
		log_threshold	: in type_log_level);

	procedure place_label (
		tool		: in type_tool;
		position	: in type_point);


	
	procedure find_labels (
		point		: in type_point;
		category	: in type_label_category);

	procedure finalize_move_label (
		destination		: in type_point;
		log_threshold	: in type_log_level);

	
	-- NOTE: Only simple labels can be moved.
	-- Tag labels are always attached to a stub
	-- and are moved along when the stub is moved.
	-- This is about simple labels:
	procedure move_label (
		tool		: in type_tool;
		position	: in type_point);


	
	status_show_net : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to select net." 
		& status_hint_for_abort;

	-- Outputs in the status bar some helpful properties
	-- of the selected net:
	procedure show_properties_of_selected_net;

	
end et_canvas_schematic_nets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
