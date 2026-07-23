------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           MODULE / BOARD                                 --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   to do:
--


with ada.containers; 					use ada.containers;
with ada.containers.doubly_linked_lists;

-- with et_logging;						use et_logging;

with et_board_geometry;					use et_board_geometry;

with et_pcb_stack;						use et_pcb_stack;

with et_drawing_frame;
with et_drawing_frame.board;

with et_route_restrict.boards;			use et_route_restrict.boards;
with et_via_restrict.boards;			use et_via_restrict.boards;
with et_stopmask.board;					use et_stopmask.board;
with et_stencil.board;					use et_stencil.board;
with et_silkscreen.board;				use et_silkscreen.board;
with et_assy_doc.board;					use et_assy_doc.board;
with et_keepout.board;					use et_keepout.board;
with et_board_outline;					use et_board_outline;

with et_conductors_floating_board;		use et_conductors_floating_board;
with et_module_board_user_settings;		use et_module_board_user_settings;

with et_commit;


package et_module_board is


	procedure dummy;
	


	
	-- This is non-electical board stuff:
	type type_board is tagged record
		frame			: et_drawing_frame.board.type_frame_pcb; -- incl. template name
		grid			: pac_grid.type_grid;  -- the drawing grid of the board
		stack			: type_stack;	-- the layer stack
		silkscreen		: type_silkscreen_both_sides;
		assy_doc		: type_assy_doc_both_sides;
		stencil			: type_stencil_both_sides;
		stopmask		: type_stop_mask_both_sides;
		keepout			: type_keepout_both_sides;
		route_restrict	: type_route_restrict;
		via_restrict	: type_via_restrict;

		-- non-electric floating stuff:
		-- (lines, arcs, circles, text, text placeholders, zones):
		conductors_floating	: type_conductors_floating;
		
		board_contour	: type_board_outline; -- outer and inner edges

		user_settings	: type_user_settings;
	end record;


	-- BOARD COMMITS (required for undo/redo operations via the GUI):
	use et_commit;
	
	package pac_board_commit is new pac_commit (type_board);
	use pac_board_commit;
	
	package pac_board_commits is new doubly_linked_lists (
		element_type	=> pac_board_commit.type_commit);

	type type_board_undo_redo_stack is record
		dos		: pac_board_commits.list;
		redos	: pac_board_commits.list;
	end record;

	
end et_module_board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
