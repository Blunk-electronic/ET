------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS SCHEMATIC NETS                            --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                -- 
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
-- <http://www.gnu.org/licenses/>.   
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

with gdk.event;						use gdk.event;
with gdk.types;						use gdk.types;
with gdk.types.keysyms;				use gdk.types.keysyms;

with gtk.widget;					use gtk.widget;
with gtk.gentry;

with et_canvas_schematic;			use et_canvas_schematic;
with et_net_names;					use et_net_names;
with et_canvas_messages;			use et_canvas_messages;
with et_canvas_tool;				use et_canvas_tool;
with et_sheets;						use et_sheets;
with et_schematic_geometry;			use et_schematic_geometry;
with et_schematic_coordinates;		use et_schematic_coordinates;
use et_schematic_geometry.pac_geometry_2;
use et_schematic_geometry.pac_path_and_bend;

with et_text;
with et_generic_modules;			use et_generic_modules;
with et_net_labels;					use et_net_labels;
with et_nets;						use et_nets;
with et_net_segment;				use et_net_segment;
with et_net_strands;				use et_net_strands;
with et_schematic_ops;
with et_schematic_ops.nets;			use et_schematic_ops.nets;
with et_string_processing;			use et_string_processing;
with et_logging;					use et_logging;

package et_canvas_schematic_nets is

	use pac_generic_modules;
	

	use pac_net_name;
	use pac_nets;
	use pac_strands;
	use pac_net_segments;




	
-- DRAW NET SEGMENT

	status_draw_net : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& status_set_A;
		-- & status_hint_for_abort;

	
	-- Builds a live net path. This procedure requires to be called twice:
	-- first time for the start and the second time for the end point of the path.
	-- The current bend style in preliminary_segment.path is taken into account.
	-- The path may be started and finished with different tools. For example start
	-- with MOUSE and finish with KEYBOARD or vice versa.
	procedure make_path (
		tool	: in type_tool;
		point	: in type_vector_model);


	
	-- Adds a net segment to the module on the given sheet.
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
	--    CS: Probably no longer reqired ?
	-- 5. Calls et_schematic_ops.nets.insert_segment for the final
	--    insertion of the segment in the targeted net.
	procedure add_net_segment (
		module			: in pac_generic_modules.cursor;
		net_name_given	: in pac_net_name.bounded_string; -- RESET_N
		sheet			: in type_sheet;
		segment			: in type_net_segment;
		log_threshold	: in type_log_level);

	


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
		& "to move object." 
		& status_hint_for_abort;



	status_delete : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to delete object." 
		& status_hint_for_abort;

	
	-- status_delete : constant string := 
	-- 	status_click_left 
	-- 	& "or "
	-- 	& status_press_space
	-- 	& "to delete net segment." 
	-- 	& status_hint_for_abort;

	

	
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

	


	
	-- Called when the "on_activate" signal is emitted
	-- (usually when ENTER pressed) by the entry field
	-- for the new name in the rename window:
	procedure cb_rename_new_name_entered (
		self : access gtk.gentry.gtk_entry_record'class);


	-- This procedure is called when the signal "destroy" 
	-- is emitted by the rename window.
	-- This is usually the case when:
	--  1. the operator terminates the rename window by 
	--     clicking the X in the upper right corner of the window.
	--  2. the operator presses the ESC key in the rename window:
	-- The procedure also calls procedure "reset":
	procedure cb_rename_window_destroy (
		window : access gtk_widget_record'class);
	
	
	-- This procedure shows the window where the
	-- operator sees the old name of the targeted object
	-- and where he can enter the new name of the object:
	procedure show_rename_window;
	



	

	-- This procedure is required in order to clarify
	-- which object among the proposed objects is meant.
	-- On every call of this procedure we advance from one
	-- proposed segment to the next in a circular manner
	-- and set it as "selected":
	procedure clarify_object;
	

	
	-- Locates objects in the vicinity of the given point
	-- and sets their proposed-flag.
	-- Only displayed layers are taken into account.
	-- Depending on how many objects have been found, the behaviour is:
	-- - If only one object found, then it is selected automatically.
	-- - If more than one object found, then clarification is requested.
	--   The first object of them is selected.
	procedure find_objects (
		point : in type_vector_model);

	

	procedure drag_object (
		tool	: in type_tool;
		point	: in type_vector_model);



	procedure move_object (
		tool	: in type_tool;
		point	: in type_vector_model);


	procedure delete_object (
		point	: in type_vector_model);

	
	procedure rename_object (
		point	: in type_vector_model);

	
	procedure show_object (
		point	: in type_vector_model);



	
	
-- NET LABELS

	


	
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
		& "to place a net label." 
		& status_hint_for_abort;	
	

	status_place_connector : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to place a net connector." 
		& status_hint_for_abort;	

	
	status_delete_connector : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to delete a net connector." 
		& status_hint_for_abort;	

	
	status_move_label : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to move net label." 
		& status_hint_for_abort;	

	type type_label_category is (SIMPLE, TAG, BOTH);

	function to_string (cat : in type_label_category) return string;
	

	
 
	procedure place_net_label (
		tool	: in type_tool;
		point	: in type_vector_model);



	procedure place_net_connector (
		tool	: in type_tool;
		point	: in type_vector_model);

	

	
	status_show_net : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to select net." 
		& status_hint_for_abort;

	-- Outputs in the status bar some helpful properties
	-- of the selected net:
	-- procedure show_properties_of_selected_net;

	
end et_canvas_schematic_nets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
