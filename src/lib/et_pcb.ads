------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                                PCB                                       --
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
with et_pcb_coordinates_2;				use et_pcb_coordinates_2;
with et_board_shapes_and_text;			use et_board_shapes_and_text;
with et_text;							use et_text;
with et_drills;							use et_drills;
with et_vias;							use et_vias;
with et_device_placeholders.packages; -- use et_device_placeholders.packages;
with et_packages;						use et_packages;
with et_package_names;					use et_package_names;
with et_device_name;					use et_device_name;
with et_pcb_stack;						use et_pcb_stack;
with et_frames;
with et_design_rules_board;				use et_design_rules_board;

with et_fill_zones;						use et_fill_zones;
with et_fill_zones.boards;				use et_fill_zones.boards;

with et_conductor_segment.boards;		use et_conductor_segment.boards;
with et_conductor_text.boards;			use et_conductor_text.boards;
with et_route_restrict.boards;			use et_route_restrict.boards;
with et_via_restrict.boards;			use et_via_restrict.boards;
with et_stopmask;						use et_stopmask;
with et_stencil;						use et_stencil;
with et_silkscreen;						use et_silkscreen;
with et_assy_doc;						use et_assy_doc;
with et_keepout;						use et_keepout;
with et_pcb_contour;					use et_pcb_contour;
with et_ratsnest;
with et_commit;
with et_object_status;					use et_object_status;
with et_mirroring;						use et_mirroring;


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
		current_layer	: in et_pcb_stack.type_signal_layer;
		bottom_layer	: in et_pcb_stack.type_signal_layer)
		return type_mirror;

	

	
-- PLACEHOLDERS FOR TEXTS IN CONDUCTOR LAYERS
	
	type type_text_meaning_conductor is (
		COMPANY,
		CUSTOMER,
		PARTCODE,
		DRAWING_NUMBER,
		ASSEMBLY_VARIANT,
		PROJECT, -- CS rename to PROJECT_NAME
		MODULE, -- CS rename to MODULE_NAME
		REVISION, -- CS rename to REVISION_NUMBER
		SIGNAL_LAYER_ID,
		SIGNAL_NAME
		);

	function to_string (meaning : in type_text_meaning_conductor) return string;
	function to_meaning (meaning : in string) return type_text_meaning_conductor;

	
	type type_text_placeholder_conductors is new 
		type_text_fab with 
	record
		meaning : type_text_meaning_conductor := type_text_meaning_conductor'first;

		-- the conductor layer the placeholder is placed in:
		layer	: type_signal_layer := type_signal_layer'first; 
	end record;

	-- There can be lots of placeholders of this kind. So they can be are stored in a list:
	package pac_text_placeholders_conductors is new 
		doubly_linked_lists (type_text_placeholder_conductors);


	
	
-- PLACEHOLDERS FOR TEXTS IN NON-CONDUCTOR LAYERS
		
	subtype type_text_meaning is type_text_meaning_conductor 
		range COMPANY .. REVISION;

	
	type type_text_placeholder is new
		type_text_fab with 
	record
		meaning : type_text_meaning := type_text_meaning'first;
	end record;

	
	package pac_text_placeholders is new doubly_linked_lists (type_text_placeholder);

	
	
-- LOCK STATUS OF AN OBJECT
	type type_locked is (NO, YES);

	lock_status_default : constant type_locked := NO;
	function to_string (locked : in type_locked) return string;
	function to_lock_status (locked : in string) return type_locked;
	

	
-- CONTOUR / OUTLINE / HOLES / EDGE CUTS

	
	type type_pcb_contours is record -- PCB contour defined for the PCB as a whole
		outline	: type_outer_contour;
		holes	: pac_holes.list;
	end record;

	-- CS
	-- The DRC shall:
	-- - detect gaps in outline
	-- - detect texts inside board area and output an error

	

	


	
	-- Type for NON ELECTRIC !! conductor objects:
	-- NON ELECTRIC conductor objects of a pcb may also 
	-- include text placeholders:
	type type_conductors_non_electric is record
		lines 			: pac_conductor_lines.list;
		arcs			: pac_conductor_arcs.list;
		circles			: pac_conductor_circles.list;

		-- floating fill zones:
		fill_zones		: type_floating; 
		-- CS: It is probably no good idea to allow floating conductor polygons.
		-- Useful to catch the liquid solder during wave soldering ?

		-- global cutout areas:
		cutouts			: boards.pac_cutouts.list;
		
		texts			: et_conductor_text.boards.pac_conductor_texts.list;
		placeholders	: pac_text_placeholders_conductors.list;
	end record;



	

	
	-- A fully routed net may consist of these conductor objects:
	
	type type_route is record
		airwires	: et_ratsnest.type_airwires;
		
		lines 		: pac_conductor_lines.list;
		arcs		: pac_conductor_arcs.list;
		-- CS: circles ?
		vias		: pac_vias.list;

		-- fill zones:
		fill_zones	: boards.type_route;

		-- user defined restrictions. currently not supported. CS
		restrict	: et_route_restrict.boards.type_route_restrict;
	end record;
	



	
	-- Stopmask in board may contain placeholders:
	type type_stopmask_board is new et_stopmask.type_stopmask with 
	record
		-- for texts in conductor layers to be exposed
		placeholders : pac_text_placeholders.list;
	end record;


	type type_stop_mask_both_sides is record
		top		: type_stopmask_board;
		bottom	: type_stopmask_board;
	end record;

	


	
	
	
-- SILKSCREEN

	-- For silkscreen objects that do NOT belong to any packages use this type.
	-- Such objects are lines, arcs, circles, contours and 
	-- placeholders for board revision, name, misc ... :
	type type_silkscreen_board is new type_silkscreen with record
		placeholders : pac_text_placeholders.list;
	end record;
		
	-- Because silkscreen is about two sides of the board this 
	-- composite is required:	
	type type_silkscreen_both_sides is record
		top 	: type_silkscreen_board;
		bottom	: type_silkscreen_board;
	end record;


	

-- ASSEMBLY DOCUMENTATION

	-- For assembly documentation objects that do NOT belong to any packages use this type:
	-- Such objects are lines, arcs, circles, contours and 
	-- placeholders for board revision, name, misc ... :
	type type_assy_doc_board is new type_assy_doc with record
		-- Placeholders for revision, board name, misc ... :
		placeholders : pac_text_placeholders.list;
	end record;


	-- Because assembly documentation is about two sides of the board this composite is required:	
	type type_assy_doc_both_sides is record
		top 	: type_assy_doc_board;
		bottom	: type_assy_doc_board;
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


	


-- NON-ELECTICAL DEVICES:

	-- Devices which do not have a counterpart in the schematic 
	-- (like fiducials, mounting holes, ...). They can have
	-- terminals. But the terminals are not connected with any net.
	-- They have names like H1 (hole) or FD (fiducial).
	-- This is NOT about accessories of the module !
	-- These devices do NOT appear in the BOM !
	-- We collect them in an indefinite ordered map.
	
	type type_device_non_electric is record
		position			: et_pcb_coordinates_2.type_package_position; -- incl. rotation and face
		flipped				: type_flipped := flipped_default;
		text_placeholders	: et_device_placeholders.packages.type_text_placeholders;
		package_model		: pac_package_model_file_name.bounded_string; -- ../lbr/packages/fiducial.pac
		-- CS cursor to package model instead ?

		-- CS ?
		--value		: pac_device_value.bounded_string; -- 470R
		--partcode	: pac_device_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
		--purpose		: pac_device_purpose.bounded_string; -- brightness_control

		status : type_object_status;
	end record;

	
	-- CS: this should be a hashed map:
	package pac_devices_non_electric is new ordered_maps (
		key_type		=> type_device_name, -- H1, FD2, ...
		element_type	=> type_device_non_electric);

	use pac_devices_non_electric;
	
	
	-- COMMITS OF NON-ELECTRICAL DEVICES (required for undo/redo operations via the GUI):
	use et_commit;
	
	package pac_non_electrical_device_commit is new pac_commit (pac_devices_non_electric.map);
	use pac_non_electrical_device_commit;
	
	package pac_non_electrical_device_commits is new doubly_linked_lists (
		element_type	=> pac_non_electrical_device_commit.type_commit);

	type type_non_electrical_devices_undo_redo_stack is record
		dos		: pac_non_electrical_device_commits.list;
		redos	: pac_non_electrical_device_commits.list;
	end record;
	


	
	-- Iterates the non-electric devices. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		devices	: in pac_devices_non_electric.map;
		process	: not null access procedure (position : in pac_devices_non_electric.cursor);
		proceed	: not null access boolean);


	

	
-- BOARD / LAYOUT:

	
	-- This is non-electical board stuff:
	type type_board is tagged record
		frame			: et_frames.type_frame_pcb; -- incl. template name
		grid			: pac_grid.type_grid;  -- the drawing grid of the board
		stack			: et_pcb_stack.type_stack;	-- the layer stack
		silk_screen		: type_silkscreen_both_sides; -- CS rename to silkscreen
		assy_doc		: type_assy_doc_both_sides;
		stencil			: type_stencil_both_sides;
		stop_mask		: type_stop_mask_both_sides; -- CS rename to stopmask
		keepout			: type_keepout_both_sides;
		route_restrict	: type_route_restrict;
		via_restrict	: type_via_restrict;

		-- non-electric stuff, incl. floating polygons !
		conductors		: type_conductors_non_electric;
		contours		: type_pcb_contours; -- pcb outline

		user_settings	: type_user_settings;
	end record;


	-- BOARD COMMITS (required for undo/redo operations via the GUI):
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
