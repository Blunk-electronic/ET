------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                                PCB                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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

with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings;				use ada.strings;
with ada.strings.fixed;			use ada.strings.fixed;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;

with et_pcb_coordinates;		use et_pcb_coordinates;
with et_geometry;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_text;
with et_nets;					use et_nets;
with et_drills;					use et_drills;
with et_vias;					use et_vias;
with et_packages;				use et_packages;
with et_devices;
with et_pcb_stack;				use et_pcb_stack;
with et_frames;
with et_design_rules;			use et_design_rules;

with et_conductor_polygons;			use et_conductor_polygons;
with et_conductor_polygons.boards;	use et_conductor_polygons.boards;

with et_conductor_segment.boards;	use et_conductor_segment.boards;
with et_conductor_text.boards;		use et_conductor_text.boards;
with et_route_restrict.boards;		use et_route_restrict.boards;
with et_via_restrict.boards;		use et_via_restrict.boards;
with et_stop_mask.boards;			use et_stop_mask.boards;
with et_stencil.boards;				use et_stencil.boards;
with et_silkscreen.boards;			use et_silkscreen.boards;
with et_assy_doc.boards;			use et_assy_doc.boards;
with et_keepout.boards;				use et_keepout.boards;
with et_pcb_contour;				use et_pcb_contour;

package et_pcb is
	
	use pac_geometry_brd;

	use pac_geometry_2;
	use pac_polygons;
	use pac_text_fab;
	

-- NET CLASSES
	net_class_name_length_max : constant positive := 50;
	package pac_net_class_name is new generic_bounded_length (net_class_name_length_max); -- hi-voltage, si-critical, ...
	use pac_net_class_name;
	
	net_class_name_default : constant pac_net_class_name.bounded_string := pac_net_class_name.to_bounded_string ("default");
	
	function to_string (net_class_name : in pac_net_class_name.bounded_string) return string;
	function to_net_class_name (net_class_name : in string) return pac_net_class_name.bounded_string;
	
	net_class_description_length_max : constant positive := 100;
	package pac_net_class_description is new generic_bounded_length (net_class_description_length_max);

	function to_string (class_description : in pac_net_class_description.bounded_string) return string;
	function to_net_class_description (class_description : in string) return pac_net_class_description.bounded_string;
	
	type type_net_class is tagged record
		description				: pac_net_class_description.bounded_string;

		-- The net class parameters assume default values 
		-- that cause minimal manufacturing costs even if 
		-- no net classes have been defined by the operator:
		clearance				: type_track_clearance := 0.3;
		track_width_min			: type_track_width := 0.3;
		via_drill_min			: type_drill_size := 0.3;
		via_restring_min		: type_restring_width := 0.3;
		micro_via_drill_min		: type_drill_size := type_drill_size'last; -- CS use reasonable default
		micro_via_restring_min	: type_restring_width := type_restring_width'last;  -- CS use reasonable default
	end record;

	package pac_net_classes is new ordered_maps (
		key_type		=> pac_net_class_name.bounded_string,
		element_type	=> type_net_class);
	


	-- Maps from signal layer to mirror status of a vectorized text.
	-- Use it for drawing non-device related texts and placeholders.
	function signal_layer_to_mirror (
		current_layer	: in et_pcb_stack.type_signal_layer;
		bottom_layer	: in et_pcb_stack.type_signal_layer)
		return et_text.type_vector_text_mirrored;

	

	
-- PLACEHOLDERS FOR TEXTS IN CONDUCTOR LAYERS
	
	type type_text_meaning_conductor is (
		COMPANY,
		CUSTOMER,
		PARTCODE,
		DRAWING_NUMBER,
		ASSEMBLY_VARIANT,
		PROJECT,
		MODULE,
		REVISION,
		SIGNAL_LAYER_ID,
		SIGNAL_NAME
		);

	function to_string (meaning : in type_text_meaning_conductor) return string;
	function to_meaning (meaning : in string) return type_text_meaning_conductor;

	
	type type_text_placeholder_conductors is new 
		pac_text_fab.type_text_fab with 
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
		pac_text_fab.type_text_fab with 
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
		outline	: type_polygon;
		holes	: pac_pcb_cutouts.list;
		--texts	: pac_text_fab.pac_texts_fab_with_content.list;
		texts	: pac_contour_texts.list;
	end record;

	-- CS
	-- The DRC shall:
	-- - detect gaps in outline
	-- - detect texts inside board area and output an error

	

	


	
	-- Type for NON ELECTRIC !! conductor objects:
	-- NON ELECTRIC conductor objects of a pcb may also 
	-- include text placeholders:
	type type_conductor_objects is record
		lines 			: pac_conductor_lines.list;
		arcs			: pac_conductor_arcs.list;
		circles			: pac_conductor_circles.list;

		-- floating conductor polygons/fill areas:
		polygons		: type_floating; 
		-- CS: It is probably no good idea to allow floating conductor polygons.

		-- global cutout areas for conductor polygons:
		cutouts			: boards.pac_conductor_cutouts.list;
		
		texts			: et_conductor_text.boards.pac_conductor_texts.list;
		placeholders	: pac_text_placeholders_conductors.list;
	end record;


	
-- Types for ELECTRIC !! conductor objects:


	--type type_airwire is new type_line with null record;

	--type type_airwire_node is new type_point with record
		--processed : boolean := false;
	--end record;

	--package pac_airwire_nodes is new doubly_linked_lists (type_airwire_node);

	

	package pac_airwires is new doubly_linked_lists (type_line);

	-- Returns true if the container airwires contains the given
	-- candidate airwire:
	function contains_airwire (
		airwires	: in pac_airwires.list;
		airwire		: in type_line)
		return boolean;
	
	
	-- Iterates the given list of airwires. Aborts the process when the
	-- proceed-flag goes false:
	procedure iterate (
		airwires	: in pac_airwires.list;
		process		: not null access procedure (position : in pac_airwires.cursor);
		proceed		: not null access boolean);

	
	type type_airwires is record
		lines	: pac_airwires.list;
		hidden	: boolean := false;
	end record;

	status_ratsnest_updated : constant string := "ratsnest updated";

	airwire_line_width : constant type_distance_positive := 0.01;
	
	
	type type_route is record
		airwires	: type_airwires;
		
		lines 		: pac_conductor_lines.list;
		arcs		: pac_conductor_arcs.list;
		-- CS: circles ?
		vias		: pac_vias.list;

		-- fill areas:
		polygons	: boards.type_route;

		-- route specific cutout areas:
		cutouts		: boards.pac_conductor_cutouts.list;
	end record;
	



	
	-- Stop mask in board (may contain placeholders):
	type type_stop_mask 
		is new et_stop_mask.boards.type_stop_mask with 
	record
		-- for texts in conductor layers to be exposed
		placeholders : pac_text_placeholders.list;
	end record;
	-- CS move to et_stop_mask

	type type_stop_mask_both_sides is record
		top		: type_stop_mask;
		bottom	: type_stop_mask;
	end record;

	



-- STENCIL
	-- Stencil has no extensions.
	-- See et_packages.


	
	
	
-- SILK SCREEN
	-- For silk screen objects that do not belong to any packages use this type:
	type type_silk_screen 
		is new et_silkscreen.boards.type_silk_screen with
	record
		-- Placeholders for revision, board name, misc ... :
		placeholders : pac_text_placeholders.list;
	end record;
		

	-- Because silk screen is about two sides of the board this composite is required:	
	type type_silk_screen_both_sides is record
		top 	: type_silk_screen;
		bottom	: type_silk_screen;
	end record;


	

-- ASSEMBLY DOCUMENTATION

	-- For assembly documentation objects that do not belong to any packages use this type:
	type type_assembly_documentation 
		is new et_assy_doc.boards.type_assembly_documentation with
	record
		-- Placeholders for revision, board name, misc ... :
		placeholders : pac_text_placeholders.list;
	end record;


	-- Because assembly documentation is about two sides of the board this composite is required:	
	type type_assembly_documentation_both_sides is record
		top 	: type_assembly_documentation;
		bottom	: type_assembly_documentation;
	end record;


-- KEEPOUT
	-- Keepout has no extensions.
	-- See et_packages.
	
	
-- ROUTE RESTRICT
	-- route restrict has no extensions.
	-- See et_packages.


-- VIA RESTRICT
	-- via restrict has no extensions.
	-- See et_packages.

	



	function package_position (position : in type_package_position) return string;
	-- Returns the coordinates of a package (in a board) as string.



	-- Maps from flip status to mirror status of a 
	-- vector text:
	function to_mirror (
		flipped : in type_flipped) 
		return et_text.type_vector_text_mirrored;

	
	
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
		cursor			: in pac_polygon_segments.cursor;
		log_threshold 	: in type_log_level);

	
	-- Logs the properties of the given contour circle:
	procedure pcb_contour_circle_properties (
		circle			: in type_circle;
		log_threshold 	: in type_log_level);


	
	-- The board origin is positioned x/y away from the lower left
	-- corner of the drawing frame.
	-- Unless specified by operator the board origin default is:
	origin_default : constant type_point := type_point (set (20.0, 65.0));


	
	type type_user_settings is record
		vias		: type_user_settings_vias;
		-- CS auto set drill and track width ?
		
		polygons_conductor	: boards.type_user_settings;

		-- CS polygons_non_conductor
	end record;



	-- Devices which do not have a counterpart in the schematic 
	-- (like fiducials, mounting holes, ...). They can have
	-- terminals. But the terminals are not connected with any net.
	-- They have names like H1 (hole) or FD (fiducial).
	-- This is NOT about accessories of the module !
	-- These devices do NOT appear in the BOM !
	-- We collect them in an indefinite ordered map.
	
	type type_device_non_electric is record
		position			: et_pcb_coordinates.type_package_position; -- incl. rotation and face
		flipped				: type_flipped := flipped_default;
		text_placeholders	: type_text_placeholders;
		package_model		: pac_package_model_file_name.bounded_string; -- ../lbr/packages/fiducial.pac
	end record;

	
	-- CS: this should be a hashed map:
	package pac_devices_non_electric is new ordered_maps (
		key_type		=> et_devices.type_device_name, -- H1, FD2, ...
		"<"				=> et_devices."<",													 
		element_type	=> type_device_non_electric);

	
	-- Iterates the non-electric devices. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		devices	: in pac_devices_non_electric.map;
		process	: not null access procedure (position : in pac_devices_non_electric.cursor);
		proceed	: not null access boolean);

	

	
	
	-- This is general board stuff:
	type type_board is tagged record
		origin			: type_point := origin_default;
		frame			: et_frames.type_frame_pcb; -- incl. template name
		grid			: type_grid;  -- the drawing grid of the board
		stack			: et_pcb_stack.type_stack;	-- the layer stack
		silk_screen		: type_silk_screen_both_sides;
		assy_doc		: type_assembly_documentation_both_sides;
		stencil			: type_stencil_both_sides;
		stop_mask		: type_stop_mask_both_sides;
		keepout			: type_keepout_both_sides;
		route_restrict	: type_route_restrict;
		via_restrict	: type_via_restrict;

		-- non-electric stuff, incl. floating polygons !
		conductors		: type_conductor_objects;
		contours		: type_pcb_contours; -- pcb outline

		user_settings	: type_user_settings;
	end record;

	
end et_pcb;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
