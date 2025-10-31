------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD DEVICES                             --
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

with et_canvas;
with et_canvas_tool;				use et_canvas_tool;
with et_canvas_messages;			use et_canvas_messages;

with et_pcb_sides;
with et_board_coordinates;			use et_board_coordinates;
with et_board_geometry;				use et_board_geometry;
use et_board_geometry.pac_geometry_2;

with et_pcb;						use et_pcb;
with et_device_model;				use et_device_model;
with et_device_name;				use et_device_name;
with et_logging;					use et_logging;



package et_canvas_board_devices is


	-- This procedure:
	-- - Resets all proposed electrical devices.
	-- - flags object_tool, object_device_name
	procedure reset_preliminary_electrical_device;



	-- This procedure:
	-- - Resets all proposed non-electrical devices.
	-- - flags object_tool, object_device_name
	procedure reset_preliminary_non_electrical_device;




	-- This procedure is required in order to clarify
	-- which object among the proposed objects is meant.
	-- On every call of this procedure we advance from one
	-- proposed object to the next in a circular manner
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

	


	
	
-- MOVE:	

	status_move_device : constant string :=
		status_click_left 
		& "or "
		& status_press_space
		& "to move device." 
		& status_hint_for_abort;

	

	procedure move_object (
		tool	: in type_tool;
		point	: in type_vector_model);




-- RENAME:

	procedure rename_object (
		position : in type_vector_model);

	
	
	status_rename_device : constant string :=
		status_click_left 
		& "or "
		& status_press_space
		& "to rename device." 
		& status_hint_for_abort;





	
	
-- ROTATE:

	procedure rotate_object (
		position : in type_vector_model);

	
	
	status_rotate_device : constant string :=
		status_click_left 
		& "or "
		& status_press_space
		& "to rotate device." 
		& status_hint_for_abort;

	



	

-- FLIP / MIRROR:

	procedure flip_object (
		position : in type_vector_model);


	
	-- to be output in the status bar:
	status_flip_device : constant string :=
		status_click_left 
		& "or "
		& status_press_space
		& "to flip device." 
		& status_hint_for_abort;
	

	

	
-- DELETE:

	status_delete_device : constant string :=
		status_click_left 
		& "or "
		& status_press_space
		& "to delete device." 
		& status_hint_for_abort;



	
	-- NOTE: Electrical devices must be deleted in the schematic !
	-- For this reason there is no procedure here to delete an electrical device.

	procedure delete_object (
		position : in type_vector_model);

	


-- SHOW DEVICE:
	
	procedure show_object (
		position : in type_vector_model);

	
	status_show_device : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to select device." 
		& status_hint_for_abort;

	
	
end et_canvas_board_devices;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
