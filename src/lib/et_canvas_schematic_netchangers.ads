------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      CANVAS SCHEMATIC / NETCHANGERS                      --
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

with ada.containers;	            use ada.containers;
with ada.containers.doubly_linked_lists;

with gtk.widget;					use gtk.widget;
with gtk.gentry;					use gtk.gentry;

with et_schematic_geometry;			use et_schematic_geometry;
with et_schematic_coordinates;		use et_schematic_coordinates;
use et_schematic_geometry.pac_geometry_2;

with et_generic_modules;			use et_generic_modules;

with et_nets;						use et_nets;
with et_net_segment;				use et_net_segment;

with et_netchangers;				use et_netchangers;
with et_string_processing;			use et_string_processing;
with et_logging;					use et_logging;

with et_canvas_schematic_nets;

with et_canvas_messages;			use et_canvas_messages;
with et_canvas_tool;				use et_canvas_tool;


package et_canvas_schematic_netchangers is

	use pac_generic_modules;



	-- to be output in the status bar:
	status_delete_netchanger : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to delete netchanger." 
		& status_hint_for_abort;

	
	-- to be output in the status bar:
	status_move_netchanger : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to move netchanger."
		& status_hint_for_abort;

	-- to be output in the status bar:
	status_drag_netchanger : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to drag netchanger." 
		& status_hint_for_abort;

	-- to be output in the status bar:
	status_rotate_netchanger : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to rotate netchanger." 
		& status_hint_for_abort;
	


	
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


	procedure move_object (
		tool	: in type_tool;
		point	: in type_vector_model);


	procedure rotate_object (
		point	: in type_vector_model);


	procedure delete_object (
		point	: in type_vector_model);







-- RENAME:
	
	-- Called when the "on_activate" signal is emitted
	-- (usually when ENTER pressed) by the entry field
	-- for the new name in the rename window:
	procedure cb_rename_new_name_entered (
		self : access gtk_entry_record'class);
	

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
	


	
	procedure rename_object (
		point	: in type_vector_model);

	



	
	procedure drag_object (
		tool	: in type_tool;
		point	: in type_vector_model);

	


	
	
	
	-- to be output in the status bar:
	status_add_netchanger : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to add netchanger." 
		& status_hint_for_abort;


	
	-- When a unit is being added this type is required:
	type type_netchanger_being_added is record
		-- The prospective name (like 4, 5, 6, ...) once the 
		-- add operation is complete.
		-- This is relevant for the preview only:
		name_pre	: type_netchanger_id := 1;

		-- The rotation of the netchanger:
		rotation	: type_rotation_0_90 := 0.0;
		
		-- Indicates that the information above is valid:
		valid		: boolean := false;		
	end record;


	-- If a new netchanger is being added, then
	-- all the required preliminary information is stored here:
	netchanger_add : type_netchanger_being_added;


	-- Rotates the netchanger_add by 90 degrees counter-clockwise
	-- if it is valid:	
	procedure rotate_netchanger_add;


	
	-- Resets netchanger_add to its default values:
	procedure reset_netchanger_add;


	
	


	-- This procedure adds a new netchanger
	-- to the drawing. It takes the information stored
	-- in netchanger_add:
	procedure add_netchanger (
		place : in type_vector_model);





	
	-- to be output in the status bar:
	status_copy_netchanger : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to copy a netchanger." 
		& status_hint_for_abort;


	procedure copy_object (
		tool	: in type_tool;
		point	: in type_vector_model);


	
	

	procedure show_object (
		position : in type_vector_model);

	
	status_show_netchanger : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to select netchanger." 
		& status_hint_for_abort;

	
end et_canvas_schematic_netchangers;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
