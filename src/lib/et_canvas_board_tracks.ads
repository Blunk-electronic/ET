------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD TRACS                               --
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

-- with ada.containers;   			       	use ada.containers;
-- with ada.containers.indefinite_doubly_linked_lists;

with gtk.box;							use gtk.box;
with gtk.list_store;					use gtk.list_store;			

with et_canvas_general;					use et_canvas_general;
with et_canvas_board;
with et_pcb_coordinates;				use et_pcb_coordinates;
use et_pcb_coordinates.pac_geometry_2;

with et_board_shapes_and_text;			use et_board_shapes_and_text;

with et_pcb_stack;						use et_pcb_stack;
with et_net_names;						use et_net_names;


package et_canvas_board_tracks is

	use et_canvas_board.pac_canvas;
	

	-- The text properties bar:
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



	
	type type_snap_mode is (
		NEAREST_AIRWIRE, -- track starts/ends at the nearest airwire
		NEAREST_OBJECT, -- track starts/ends at the nearest object
		NO_SNAP);

	
	function to_string (
		mode	: in type_snap_mode)
		return string;
	

	type type_preliminary_track is record
		-- This flag tells the draw operations to draw the preliminary line:
		ready		: boolean := false;

		-- This tells the GUI whether the mouse or the
		-- cursor position is to be used when drawing the line:
		tool		: type_tool := MOUSE;

		net_name		: pac_net_name.bounded_string := no_name;

		signal_layer	: type_signal_layer := signal_layer_default;

		path			: type_path_live;
		width			: type_distance_positive := 0.15;
		snap_mode		: type_snap_mode := NEAREST_AIRWIRE;
	end record;

	-- The place where preliminary information of the line is stored:
	preliminary_track : type_preliminary_track;


	-- Sets the default tool to MOUSE, clears the "ready"-flag
	-- and removes the track property box from the GUI:
	procedure reset_preliminary_track;
	

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




	-- Builds a live path. This procedure requires to be called twice:
	-- first time for the start and the second time for the end point of the path.
	-- The current bend style in preliminary_track.path is taken into account.
	-- The path may be started and finished with different tools. For example start
	-- with MOUSE and finish with KEYBOARD or vice versa.
	procedure make_path (
		tool	: in type_tool;
		point	: in type_point);
	

	
end et_canvas_board_tracks;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16