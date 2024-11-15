------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD VIAS                                --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                -- 
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
-- DESCRIPTION:
-- 


with glib;								use glib;
with gtk.box;							use gtk.box;
with gtk.combo_box;						use gtk.combo_box;
with gtk.combo_box_text;				use gtk.combo_box_text;
with gtk.label;							use gtk.label;


with et_net_names;						use et_net_names;
with et_canvas;
with et_canvas_tool;					use et_canvas_tool;
with et_canvas_messages;				use et_canvas_messages;
with et_canvas_schematic_2;

with et_pcb_coordinates_2;				use et_pcb_coordinates_2;
use et_pcb_coordinates_2.pac_geometry_2;

with et_drills;							use et_drills;
with et_vias;							use et_vias;
with et_pcb_stack;						use et_pcb_stack;
with et_design_rules_board;				use et_design_rules_board;
with et_pcb;
with et_board_ops.vias;					use et_board_ops.vias;
with et_logging;						use et_logging;

with et_canvas_board_tracks;


package et_canvas_board_vias is

	use pac_net_name;

	-- The via properties bar:
	type type_box_properties is record
		
		-- This flag indicates that the
		-- box is being displayed. 
		-- The purpose of this flag is
		-- to prevent the box from being drawn
		-- multiple times:
		displayed	: boolean := false;
	end record;

	box_properties : type_box_properties;




	box_net_name,
	box_category, box_destination_blind, 
	box_buried_upper, box_buried_lower, box_drill,
	box_restring_inner, box_restring_outer : gtk_vbox;

	label_net_name,
	label_category, label_destination_blind, 
	
	label_buried_upper, label_buried_lower, label_drill,
	label_restring_inner, label_restring_outer : gtk_label;

	cbox_net_name,
	cbox_category, cbox_destination_blind,
	cbox_buried_upper, cbox_buried_lower : gtk_combo_box;
	-- Operator can choose between fixed menu entries.
	
	cbox_drill, cbox_restring_inner, cbox_restring_outer : gtk_combo_box_text;
	-- Operator may enter an additional value in the menu.

	-- These constants define the minimum and maximum of
	-- characters that can be entered in the fields for 
	-- drill size and restring width:
	
	drill_size_length_min : constant gint := 1;
	drill_size_length_max : constant gint := 4; 
	-- CS: adjust if necessary. see parameters of type_drill_size.
	
	restring_size_length_min : constant gint := 1;
	restring_size_length_max : constant gint := 5;
	--CS: adjust if necessary. see parameters of type_restring_width.
	
	-- The spacing between the boxes:
	spacing : constant natural := 5;









	

	
	-- Before placing, moving, deleting or other operations we
	-- collect preliminary information using this type:

	type type_preliminary_via is record
		-- This flag indicates that the via has been
		-- clarified among the proposed vias:
		ready				: boolean := false;

		-- This tells the GUI whether the mouse or the
		-- cursor position is to be used when drawing the via:
		tool				: type_tool := MOUSE;

		-- This is the net name shown inside the via
		-- while it is still being moved:
		net_name			: pac_net_name.bounded_string := no_name;
		
		category			: type_via_category := type_via_category'first;
		drill				: type_drill;

		-- for blind via:
		destination_blind	: type_via_layer := type_via_layer'first;

		restring_inner		: type_restring_width := type_restring_width'first;
		restring_outer		: type_restring_width := type_restring_width'first;

		-- for buried via:
		layers_buried		: type_buried_layers;
	end record;


	-- The place where preliminary information is stored:
	preliminary_via : type_preliminary_via;

	

	-- This procedure initializes the preliminary_via
	-- so that the via properties bar shows the user specific settings
	-- or the values as defined in the DRU data set.
	-- procedure init_preliminary_via;


	-- Builds the box for the via properties and
	-- inserts it below the console.
	-- If the box is already on display, nothing happens.
	procedure show_via_properties;
	

	-- Clears preliminary_via.being_moved and box_properties.displayed.
	-- Removes the via properties bar.
	procedure reset_preliminary_via;

	

	
	use pac_proposed_vias;
	proposed_vias : pac_proposed_vias.list;
	selected_via : pac_proposed_vias.cursor;


	-- Returns true if the given via matches the via indicated
	-- by selected_via:
	function via_is_selected (
		via_cursor	: in pac_vias.cursor;
		net_name	: in pac_net_name.bounded_string)
		return boolean;
	

	-- Clears the list proposed_vias.
	-- Resets selected_via to no_element:
	procedure clear_proposed_vias;
	

	-- Advances cursor selected_via to next via
	-- in list proposed_vias and sets cursor selected_via
	-- to the candidate via:
	procedure select_via;

	
	-- Locates all vias in the vicinity of given point.
	--
	-- If a single via found: 
	-- - The single via gets selected (select_via then points there).
	-- - preliminary_via.ready is set true.
	-- - clarification request is cleared
	--
	-- If more than one via found:
	-- - The first of them is selected (selected_via points to to the first)
	-- - preliminary_via.ready remains false.
	-- - clarification is requested
	procedure find_vias (
		point : in type_vector_model);

	



-- PLACE:

	-- to be output in the status bar:
	status_place_via : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to place via." 
		& status_hint_for_abort;

	
	-- Builds the final via-to-be-placed from the information
	-- provided by preliminary_via.
	-- Places the via at the given point:
	procedure place_via (
		point	: in type_vector_model);


	
-- MOVE:

	status_move_via : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to move via." 
		& status_hint_for_abort;

	
	-- Locates vias in the vicinity of the given point.
	-- Depending on how many vias have been found, the behaviour is:
	-- - If only one via found, then it is selected and 
	--   the flag preliminary_via.ready will be set.
	--   This causes the selected via to be drawn at the tool position.
	-- - If more than one via found, then clarification is requested.
	--   No via will be moved.
	--   The next call of this procedure sets preliminary_via.ready
	--   so that the selected via will be drawn at the tool position.
	--   The next call of this procedure assigns the final position 
	--   to the selected_via:
	procedure move_via (
		tool	: in type_tool;
		point	: in type_vector_model);				   


	
-- DELETE:

	status_delete_via : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to delete via." 
		& status_hint_for_abort;


	-- Locates vias in the vicinity of the given point.
	-- Depending on how many vias have been found, the behaviour is:
	-- - If only one via found, then it is deleted immediately.
	-- - If more than one via found, then clarification is requested.
	--   No via will be deleted.
	--   The next call of this procedure deletes the via indicated
	--   by the cursor selected_via:
	procedure delete_via (
		tool	: in type_tool;
		point	: in type_vector_model);				   

	
	
end et_canvas_board_vias;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
