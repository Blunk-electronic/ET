------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD TRACKS                              --
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

with gtk.list_store;					use gtk.list_store;	

with et_canvas;
with et_canvas_tool;					use et_canvas_tool;
with et_canvas_messages;				use et_canvas_messages;
with et_canvas_board;

with et_primitive_objects;				use et_primitive_objects;
with et_board_geometry;					use et_board_geometry;

with et_conductor_segment.boards;		use et_conductor_segment.boards;

with et_pcb_signal_layers;				use et_pcb_signal_layers;
with et_net_names;						use et_net_names;

with et_board_ops_ratsnest;				use et_board_ops_ratsnest;
with et_ratsnest;						use et_ratsnest;


-- IMPORTANT:
-- This is about drawing conductor segments which are connected
-- with a net.
-- Freetracks in conductor layers are handled in et_canvas_board_lines.


package et_canvas_board_tracks is

	use pac_geometry_2;

	use et_canvas_board.pac_canvas;
	use pac_path_and_bend;

	

	
	
	
	type type_snap_mode is (
		NEAREST_AIRWIRE, -- track starts/ends at the nearest airwire
		NEAREST_OBJECT, -- track starts/ends at the nearest object
		NO_SNAP);

	
	function to_string (
		mode	: in type_snap_mode)
		return string;
	

	snap_mode : type_snap_mode := NEAREST_AIRWIRE;




	


	procedure make_store_for_net_names (
		store : in out gtk_list_store);


	
	-- Displays the track properties:
	procedure show_track_properties;


	-- Advances to the next snap mode:
	procedure next_snap_mode;
	
	
	-- to be output in the status bar:
	status_draw_track : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to draw track." 
		& status_hint_for_abort;



	-- This procedure is required in order to clarify
	-- which object among the proposed objects is meant.
	-- On every call of this procedure we advance from one
	-- proposed airwire to the next in a circular manner
	-- and set it as "selected":
	procedure clarify_airwire;


	-- Builds a live path. This procedure requires to be called twice:
	-- first time for the start and the second time for the end point of the path.
	-- The current bend style of global variable live_path is taken into account.
	-- The path may be started and finished with different tools. For example start
	-- with MOUSE and finish with KEYBOARD or vice versa.
	procedure make_path (
		tool	: in type_tool;
		point	: in type_vector_model);




	status_show_net : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to select net." 
		& status_hint_for_abort;

		
end et_canvas_board_tracks;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
