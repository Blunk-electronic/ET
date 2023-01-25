------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD VIAS                                --
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

with ada.containers;   	         use ada.containers;
with ada.containers.doubly_linked_lists;

with gtk.window;					use gtk.window;
with gtk.box;						use gtk.box;

with et_general;					use et_general;
with et_net_names;					use et_net_names;
with et_canvas_general;				use et_canvas_general;
with et_canvas_schematic;

with et_geometry;					use et_geometry;
with et_pcb_coordinates;			use et_pcb_coordinates;
use et_pcb_coordinates.pac_geometry_2;

with et_drills;						use et_drills;
with et_vias;						use et_vias;
with et_pcb_stack;					use et_pcb_stack;
with et_design_rules;				use et_design_rules;
with et_project.modules;			use et_project.modules;
with et_pcb;
with et_logging;					use et_logging;


package et_canvas_board_vias is

	use pac_net_name;

	-- The via properties bar:
	type type_box_properties is record
		box_main	: gtk_hbox;
		
		-- This flag indicates that the
		-- box is being displayed. 
		-- The purpose of this flag is
		-- to prevent the box from being drawn
		-- multiple times:
		displayed	: boolean := false;
	end record;

	box_properties : type_box_properties;

	
	-- to be output in the status bar:
	status_place_via : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to place via." 
		& status_hint_for_abort;

	status_move_via : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to move via." 
		& status_hint_for_abort;



	
	-- The properties of the via being placed:
	type type_via_place is record
		being_moved			: boolean := false;
		tool				: type_tool := MOUSE;
		
		net					: type_net_indexed; -- net name and index
		
		category			: type_via_category := type_via_category'first;
		drill				: type_drill;

		-- for blind via:
		destination_blind	: type_via_layer := type_via_layer'first;

		restring_inner		: type_restring_width := type_restring_width'first;
		restring_outer		: type_restring_width := type_restring_width'first;

		-- for buried via:
		layers_buried		: type_buried_layers;
	end record;

	via_place : type_via_place;



	type type_selected_via is record
		via	: et_vias.pac_vias.cursor;
	end record;

	package pac_proposed_vias is new doubly_linked_lists (type_selected_via);
	use pac_proposed_vias;

	proposed_vias : pac_proposed_vias.list;
	selected_via : pac_proposed_vias.cursor;


	-- Clears the list proposed_vias.
	-- Resets selected_via to no_element:
	procedure clear_proposed_vias;
	

	-- Collects all vias in the vicinity of the given point:	
	--function collect_vias (
		--module			: in pac_generic_modules.cursor;
		--place			: in type_point; -- x/y
		--catch_zone		: in type_catch_zone; -- the circular area around the place
		--log_threshold	: in type_log_level)
		--return pac_proposed_vias.list;


	-- Advances cursor selected_via to next device
	-- in list proposed_vias:
	procedure clarify_via;

	
	-- Locates all vias in the vicinity of given point.
	-- If more than one via near point found, then it sets the
	-- cursor selected_via to the via and requests
	-- for clarification.
	procedure find_vias (
		point : in type_point);

	
	-- This procedure initializes the variable et_canvas_board_vias.via_place
	-- so that the via properties bar shows the user specific settings
	-- or the values as defined in the DRU data set.
	procedure init_via_place;



	
	-- Builds the final via-to-be-placed from the information
	-- provided by temporarily variable via_place.
	-- Inserts the via in the module.
	procedure place_via (
		destination : in type_point);



	-- Assigns the final position after the move to the selected via.
	-- Resets global variable via_place:
	procedure finalize_move (
		destination		: in type_point;
		log_threshold	: in type_log_level);
	
	procedure move_via (
		tool		: in type_tool;
		position	: in type_point);				   


	
	
	
	-- Clears via_place.being_moved and box_properties.displayed.
	-- Removes the via properties bar.
	procedure reset_via_place;

	
	-- Builds the box for the via properties and
	-- inserts it below the console.
	-- If the box is already on display, nothing happens.
	procedure show_via_properties;
	
end et_canvas_board_vias;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
