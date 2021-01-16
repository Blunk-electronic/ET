------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD VIAS                                --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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

with gtk.window;					use gtk.window;
with gtk.box;						use gtk.box;

with et_general;					use et_general;
with et_nets;						use et_nets;
with et_canvas_general;				use et_canvas_general;
with et_canvas_schematic;

with et_pcb_coordinates;			use et_pcb_coordinates;
use et_pcb_coordinates.pac_geometry_brd;

with et_drills;						use et_drills;
with et_vias;						use et_vias;
with et_pcb_stack;					use et_pcb_stack;
with et_design_rules;				use et_design_rules;
with et_project.modules;			use et_project.modules;
with et_pcb;

with et_string_processing;			use et_string_processing;

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

	-- The properties of the text being placed:
	type type_via_place is record
		being_moved			: boolean := false;

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

	-- Clears via_place.being_moved and box_properties.displayed.
	-- Removes the text properties bar.
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
