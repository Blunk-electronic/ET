------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                                PCB                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                -- 
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
--		- separate in two packages things related to board and device package.


with ada.strings.bounded; 				use ada.strings.bounded;

with ada.containers; 					use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;

with et_logging;						use et_logging;

with et_pcb_sides;						use et_pcb_sides;
with et_board_geometry;					use et_board_geometry;
with et_board_text;						use et_board_text;
with et_text;							use et_text;
with et_drills;							use et_drills;
with et_vias;							use et_vias;
with et_pcb_signal_layers;				use et_pcb_signal_layers;
with et_pcb_stack;						use et_pcb_stack;
with et_drawing_frame;
with et_drawing_frame.board;
with et_design_rules_board;				use et_design_rules_board;

with et_fill_zones;						use et_fill_zones;
with et_fill_zones.boards;				use et_fill_zones.boards;

with et_conductor_segment.boards;		use et_conductor_segment.boards;
with et_conductor_text.boards;			use et_conductor_text.boards;
with et_route_restrict.boards;			use et_route_restrict.boards;
with et_via_restrict.boards;			use et_via_restrict.boards;
with et_stopmask.board;					use et_stopmask.board;
with et_stencil.board;					use et_stencil.board;
with et_silkscreen.board;				use et_silkscreen.board;
with et_assy_doc.board;					use et_assy_doc.board;
with et_keepout;						use et_keepout;
with et_pcb_contour;					use et_pcb_contour;
with et_ratsnest;
with et_commit;
with et_object_status;					use et_object_status;
with et_mirroring;						use et_mirroring;
with et_pcb_placeholders;				use et_pcb_placeholders;


package et_pcb is
	
	use pac_geometry_2;
	use pac_contours;
	use pac_polygons;
	use pac_text_board;


	-- Maps from face to mirror status of a vectorized text.
	-- Use it for non-device related texts and placeholders.
	-- NOTE: Mirroring along X axis is not allowed for vector texts.
	-- So this function returns either MIRROR_NO or MIRROR_ALONG_Y_AXIS:
	function face_to_mirror (
		f : in type_face)
		return type_mirror;

	



	-- Maps from signal layer to mirror status of a vectorized text.
	-- Use it for drawing non-device related texts and placeholders.
	-- So this function returns either MIRROR_NO or MIRROR_ALONG_Y_AXIS:
	function signal_layer_to_mirror (
		current_layer	: in type_signal_layer;
		bottom_layer	: in type_signal_layer)
		return type_mirror;

	

	

	
-- CONTOUR / OUTLINE / HOLES / EDGE CUTS

	-- The board contour consists of the outer edges
	-- and holes:
	type type_board_contour is record
		outline	: type_outer_contour;
		holes	: pac_holes.list;
	end record;

	-- CS
	-- The DRC shall:
	-- - detect gaps in outline
	-- - detect texts inside board area and output an error

	

	


	
	-- Type for NON ELECTRIC !! conductor objects.
	-- All these objects are not connected to any net,
	-- means they are floating.
	-- NON ELECTRIC conductor objects of a pcb may also 
	-- include text placeholders:
	type type_conductors_floating is record
		lines 			: pac_conductor_lines.list;
		arcs			: pac_conductor_arcs.list;
		circles			: pac_conductor_circles.list;

		-- floating fill zones:
		zones			: type_floating;
		-- Useful to catch the liquid solder during wave soldering ?

		-- global cutout areas:
		cutouts			: boards.pac_cutouts.list;
		
		texts			: et_conductor_text.boards.pac_conductor_texts.list;
		placeholders	: pac_text_placeholders_conductors.list;
	end record;




		

	-- In this world, if a package is flipped, then it is
	-- mirrored along the Y-axis.
	-- This function maps from flip status to mirror along y-axis.
	-- If flipped is false, then the return is MIRRROR_NO.
	-- If flipped is true, then the return is MIRROR_ALONG_Y_AXIS:
	function to_mirror_along_y_axis (
		flipped : in type_flipped)
		return type_mirror;

		
	
-- LOGGING PROPERTIES OF OBJECTS

	
	-- Logs the properties of the given line of a route
	procedure route_line_properties (
		cursor			: in pac_conductor_lines.cursor;
		log_threshold 	: in type_log_level);

	
	-- Logs the properties of the given via of a route
	procedure route_via_properties (
		cursor			: in pac_vias.cursor;
		log_threshold 	: in type_log_level);


	-- Logs the properties of the given contour segment:
	procedure pcb_contour_segment_properties (
		cursor			: in pac_segments.cursor;
		log_threshold 	: in type_log_level);

	
	-- Logs the properties of the given contour circle:
	procedure pcb_contour_circle_properties (
		circle			: in type_circle;
		log_threshold 	: in type_log_level);


	
	-- The board origin is positioned x/y away from the lower left
	-- corner of the drawing frame.
	-- Unless specified by operator the board origin default is:
	origin_default : constant type_vector_model := (20.0, 65.0);
	-- CS remove

	
	type type_user_settings is record
		vias		: type_user_settings_vias;
		-- CS auto set drill and track width ?
		
		polygons_conductor	: boards.type_user_settings;

		-- CS polygons_non_conductor
	end record;


	

	
-- BOARD / LAYOUT:

	
	-- This is non-electical board stuff:
	type type_board is tagged record
		frame			: et_drawing_frame.board.type_frame_pcb; -- incl. template name
		grid			: pac_grid.type_grid;  -- the drawing grid of the board
		stack			: et_pcb_stack.type_stack;	-- the layer stack
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
		
		board_contour	: type_board_contour; -- outer and inner edges

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

	
end et_pcb;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
