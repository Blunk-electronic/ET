------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                                PCB                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;
with et_string_processing;

with et_pcb_coordinates;		use et_pcb_coordinates;
with et_geometry;
with et_drills;					use et_drills;
with et_packages;				use et_packages;
with et_pcb_stack;				use et_pcb_stack;
with et_frames;

package et_pcb is
	use et_pcb_coordinates.pac_geometry_brd;
	use et_packages.pac_shapes;
	
	-- VIAS
	type type_micro_vias_allowed is (NO, YES);
	function to_micro_vias_allowed (allowed : in string) return type_micro_vias_allowed;
	function to_string (allowed : in type_micro_vias_allowed) return string;
	

	-- NET CLASSES
	net_class_name_length_max : constant positive := 50;
	package type_net_class_name is new generic_bounded_length (net_class_name_length_max); -- hi-voltage, si-critical, ...

	net_class_name_default : constant type_net_class_name.bounded_string := type_net_class_name.to_bounded_string ("default");
	
	function to_string (net_class_name : in type_net_class_name.bounded_string) return string;
	function to_net_class_name (net_class_name : in string) return type_net_class_name.bounded_string;
	
	net_class_description_length_max : constant positive := 100;
	package type_net_class_description is new generic_bounded_length (net_class_description_length_max);

	function to_string (class_description : in type_net_class_description.bounded_string) return string;
	function to_net_class_description (class_description : in string) return type_net_class_description.bounded_string;
	
	type type_net_class is tagged record
		description				: type_net_class_description.bounded_string;

		-- The net class parameters assume default values with the largest/greates structures allowed.
		-- So the manufacturing costs will be at minimum if parameters had not been provided on instanciation.
		clearance				: type_track_clearance := type_track_clearance'last;
		track_width_min			: type_track_width := type_track_width'last;
		via_drill_min			: type_drill_size := type_drill_size'last;
		via_restring_min		: type_restring_width := type_restring_width'last;
		micro_via_drill_min		: type_drill_size := type_drill_size'last;
		micro_via_restring_min	: type_restring_width := type_restring_width'last;
	end record;

	package type_net_classes is new ordered_maps (
		key_type		=> type_net_class_name.bounded_string,
		element_type	=> type_net_class,
		"<"				=> type_net_class_name."<"
		);
	


	
		
-- PLACEHOLDERS FOR TEXTS IN COPPER LAYERS
	type type_text_meaning_copper is (
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

	function to_string (meaning : in type_text_meaning_copper) return string;
	function to_meaning (meaning : in string) return type_text_meaning_copper;
	
	type type_text_placeholder_copper is new type_text with record
		meaning : type_text_meaning_copper;
		layer	: type_signal_layer;	-- the copper layer the placeholder is placed at
	end record;

	-- There can be lots of placeholders of this kind. So they can be are stored in a list:
	package type_text_placeholders_copper is new doubly_linked_lists (type_text_placeholder_copper);


	
	
-- PLACEHOLDERS FOR TEXTS IN NON-COPPER LAYERS
	subtype type_text_meaning is type_text_meaning_copper range COMPANY .. REVISION;
	
	type type_text_placeholder is new type_text with record
		meaning : type_text_meaning;
	end record;

	package pac_text_placeholders is new doubly_linked_lists (type_text_placeholder);

	
	
-- LOCK STATUS OF AN OBJECT
	type type_locked is (NO, YES);

	lock_status_default : constant type_locked := NO;
	function to_string (locked : in type_locked) return string;
	function to_lock_status (locked : in string) return type_locked;
	

	
-- PCB CONTOUR/OUTLINE
	type type_pcb_contour_line is new type_line with record
		locked : type_locked := type_locked'first;
	end record;

	package type_pcb_contour_lines is new doubly_linked_lists (type_pcb_contour_line);

	
	type type_pcb_contour_arc is new type_arc with record
		locked : type_locked := type_locked'first;
	end record;
	package type_pcb_contour_arcs is new doubly_linked_lists (type_pcb_contour_arc);

	
	type type_pcb_contour_circle is new type_circle with record
		locked : type_locked := type_locked'first;
	end record;
	package type_pcb_contour_circles is new doubly_linked_lists (type_pcb_contour_circle);

	
	type type_pcb_contours is record -- PCB contour defined for the PCB as a whole
		lines 	: type_pcb_contour_lines.list;
		arcs	: type_pcb_contour_arcs.list;
		circles	: type_pcb_contour_circles.list;
	end record;


		


	-- COPPER OBJECTS OF A PCB
	-- In a pcb drawing copper objects can be placed at various copper layers.
	-- This requires a layer id for the object.
	type type_copper_line is new et_packages.type_copper_line with record
		layer	: type_signal_layer;
	end record;
	package pac_copper_lines is new doubly_linked_lists (type_copper_line);

	function on_segment (
	-- Returns true if the given point sits on the given line of copper.
		point		: in type_point; -- x/y
		layer		: in type_signal_layer;
		line		: in pac_copper_lines.cursor;
		accuracy	: in type_accuracy)
		return boolean;
	
	type type_copper_arc is new et_packages.type_copper_arc with record
		layer	: type_signal_layer;		
	end record;
	package pac_copper_arcs is new doubly_linked_lists (type_copper_arc);

	function on_segment (
	-- Returns true if the given point sits on the given arc of copper.
		point			: in type_point; -- x/y
		layer			: in type_signal_layer;
		arc				: in pac_copper_arcs.cursor;
		accuracy		: in type_distance)
		return boolean;
	
	type type_copper_circle is new et_packages.type_copper_circle with record
		layer	: type_signal_layer;
	end record;
	package pac_copper_circles is new indefinite_doubly_linked_lists (type_copper_circle);
	


	
	type type_text is new et_packages.type_text_with_content with record
		layer	: type_signal_layer;
	end record;

	package pac_texts is new doubly_linked_lists (type_text);

	-- Cutout-polygons in copper layers:
	type type_copper_cutout is new et_packages.type_cutout_zone with record
		layer 	: type_signal_layer;
	end record;

	package pac_copper_cutouts is new doubly_linked_lists (type_copper_cutout);

	-- Polgon priority: 0 is weakest, 100 ist strongest.
	keyword_priority : constant string := "priority";
	
	polygon_priority_max : constant natural := 100;
	subtype type_polygon_priority is natural range natural'first .. polygon_priority_max;
	function to_string (priority_level : in type_polygon_priority) return string;
	function to_polygon_priority (priority_level : in string) return type_polygon_priority;
	
	-- A floating copper polygon is not connected to any net:
	type type_copper_polygon_floating_solid is new et_packages.type_polygon (fill_style => SOLID) with record
		width_min		: type_track_width; -- the minimum width
		isolation		: type_track_clearance := type_track_clearance'first; -- the space between foreign pads and the polygon
		layer 			: type_signal_layer;
		priority_level	: type_polygon_priority := type_polygon_priority'first;
	end record;

	package pac_copper_polygons_floating_solid is new indefinite_doubly_linked_lists (type_copper_polygon_floating_solid);
	
	type type_copper_polygon_floating_hatched is new et_packages.type_polygon (fill_style => HATCHED) with record
		width_min		: type_track_width; -- the minimum width
		isolation		: type_track_clearance := type_track_clearance'first; -- the space between foreign pads and the polygon
		layer 			: type_signal_layer;
		priority_level	: type_polygon_priority := type_polygon_priority'first;
	end record;

	package pac_copper_polygons_floating_hatched is new indefinite_doubly_linked_lists (type_copper_polygon_floating_hatched);
	
	type type_copper_polygons_floating is record
		solid	: pac_copper_polygons_floating_solid.list;
		hatched	: pac_copper_polygons_floating_hatched.list;
	end record;
	
	
	-- Type for NON ELECTRIC !! conductor objects:
	-- NON ELECTRIC conductor objects of a pcb may also include text placeholders:
	type type_copper is record  -- CS rename to type_conductor
		lines 			: pac_copper_lines.list;
		arcs			: pac_copper_arcs.list;
		circles			: pac_copper_circles.list;

		-- CS: It is probably no good idea to allow floating copper polygons.
		polygons		: type_copper_polygons_floating; 
		cutouts			: pac_copper_cutouts.list;		
		
		texts			: pac_texts.list;
		placeholders	: type_text_placeholders_copper.list;
	end record;

-- Types for ELECTRIC !! conductor objects:

-- VIAS
	keyword_layer_start	: constant string := "layer_start";
	keyword_layer_end	: constant string := "layer_end";		
	
	type type_via is new type_drill with record
		restring_outer	: type_restring_width;	-- restring in outer layers (top/bottom)
		restring_inner	: type_restring_width;	-- restring in inner layers (mostly wider than restring_outer)
		layer_start		: type_signal_layer;
		layer_end		: type_signal_layer;
	end record;

	-- vias are collected in simple lists
	package pac_vias is new doubly_linked_lists (type_via);
	
	-- route (tracks/traces, vias, polgons)

	-- A polygon in a signal layer is usually connected with a THT or SMD pads (or both) via thermals, solid (or not at all).
	-- For this reason we define a controlled type here because some properties may exist (or may not exists) depending
	-- on the kinde of pad_connection:

-- THERMALS
	keyword_thermal_width : constant string := "thermal_width";		
	keyword_thermal_gap : constant string := "thermal_gap";
	
	polygon_thermal_width_min : constant type_track_width := type_track_width'first;
	polygon_thermal_width_max : constant type_track_width := 3.0; -- CS: adjust if nessecariy
	subtype type_polygon_thermal_width is et_pcb_coordinates.type_distance range polygon_thermal_width_min .. polygon_thermal_width_max;

	-- If a terminal is connected/associated with a polyon, this is the space between pad and polygon:
	polygon_thermal_gap_min : constant type_track_clearance := type_track_clearance'first;
	polygon_thermal_gap_max : constant type_track_clearance := 3.0; -- CS: adjust if nessecariy
	subtype type_polygon_thermal_gap is type_track_clearance range polygon_thermal_gap_min .. polygon_thermal_gap_max;


	-- Polygons may be connected with associated pads via thermals, via solid connection or not at all:
	keyword_pad_connection : constant string := "pad_connection";
	type type_polygon_pad_connection is (THERMAL, SOLID);

	function to_string (polygon_pad_connection : in type_polygon_pad_connection) return string;
	function to_pad_connection (connection : in string) return type_polygon_pad_connection;

	
	-- Polygons may be connected with SMT, THT or all pad technologies
	keyword_pad_technology : constant string := "pad_technology";		
	
	type type_polygon_pad_technology is (
		SMT_ONLY,
		THT_ONLY,
		SMT_AND_THT);
	
	function to_string (polygon_pad_technology : in type_polygon_pad_technology) return string;
	function to_pad_technology (technology : in string) return type_polygon_pad_technology;

	type type_thermal is record
		technology	: type_polygon_pad_technology; 	-- whether SMT, THT or both kinds of pads connect with the polygon
		width		: type_polygon_thermal_width; 	-- the width of the spokes
		gap			: type_polygon_thermal_gap;		-- the space between pad and polygon -- CS: rename to thermal_length ?
	end record;
	
	
	type type_copper_polygon_solid (connection : type_polygon_pad_connection) is new
		et_packages.type_copper_polygon_solid with record

		layer 			: type_signal_layer;
		priority_level	: type_polygon_priority := type_polygon_priority'first;
				
		case connection is
			when THERMAL =>
				thermal : type_thermal;

			when SOLID =>
				technology	: type_polygon_pad_technology; -- whether SMT, THT or both kinds of pads connect with the polygon
				-- no need for any kind of thermal parameters

		end case;
				
	end record;

	type type_copper_polygon_hatched (connection : type_polygon_pad_connection) is new
		et_packages.type_copper_polygon_hatched with record

		layer 			: type_signal_layer;
		priority_level	: type_polygon_priority := type_polygon_priority'first;
				
		case connection is
			when THERMAL =>
				thermal : type_thermal;

			when SOLID =>
				technology	: type_polygon_pad_technology; -- whether SMT, THT or both kinds of pads connect with the polygon
				-- no need for any kind of thermal parameters

		end case;
				
	end record;


	
	package pac_signal_polygons_solid is new indefinite_doubly_linked_lists (type_copper_polygon_solid);
	package pac_signal_polygons_hatched is new indefinite_doubly_linked_lists (type_copper_polygon_hatched);	


	type type_signal_polygons is record
		solid	: pac_signal_polygons_solid.list;
		hatched	: pac_signal_polygons_hatched.list;
	end record;
	
	type type_route is record 
		lines 		: pac_copper_lines.list;
		arcs		: pac_copper_arcs.list;
		-- CS: circles ?
		vias		: pac_vias.list;
		polygons_2	: type_signal_polygons;
		cutouts		: pac_copper_cutouts.list;
	end record;
	

	-- Stop mask in board (may contain placeholders):
	type type_stop_mask is new et_packages.type_stop_mask with record
		placeholders : pac_text_placeholders.list; -- for texts in copper to be exposed
	end record;

	type type_stop_mask_both_sides is record
		top		: type_stop_mask;
		bottom	: type_stop_mask;
	end record;

	



-- STENCIL
	-- Stencil has no extensions.
	-- See et_packages.


	
	
	
-- SILK SCREEN
	-- For silk screen objects that do not belong to any packages use this type:
	type type_silk_screen is new type_silk_screen_base with record
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
	type type_assembly_documentation is new type_assembly_documentation_base with record
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


	-- To indicate whether a package has been flipped by the operator:
	type type_flipped is (NO, YES);
	flipped_default : constant type_flipped := NO;

	function to_string (flipped : in type_flipped) return string;
	function to_flipped (flipped : in string) return type_flipped;


	
-- LOGGING PROPERTIES OF OBJECTS
	
	procedure text_copper_properties (
	-- Logs the properties of the given text of copper
		cursor			: in pac_texts.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure route_line_properties (
	-- Logs the properties of the given line of a route
		cursor			: in pac_copper_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure route_via_properties (
	-- Logs the properties of the given via of a route
		cursor			: in pac_vias.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	text_polygon_thermal_width : constant string := "thermal_width";	
	text_polygon_thermal_gap : constant string := "thermal_gap";	
	text_polygon_pad_connection : constant string := "pad_connection";	
	text_polygon_pad_technology : constant string := "connected_with";	
	text_polygon_width_min : constant string := "minimum_width";	
	text_polygon_signal_layer : constant string := "signal_layer";
	
	procedure line_pcb_contour_properties (
	-- Logs the properties of the given line of pcb contour
		cursor			: in type_pcb_contour_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure arc_pcb_contour_properties (
	-- Logs the properties of the given arc of pcb contour
		cursor			: in type_pcb_contour_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure circle_pcb_contour_properties (
	-- Logs the properties of the given circle of pcb contour
		cursor			: in type_pcb_contour_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level);
	

	-- The board origin is positioned x/y away from the lower left
	-- corner of the drawing frame.
	-- Unless specified by operator the board origin default is:
	origin_default : constant type_point := type_point (set (20.0, 50.0));


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
		keepout			: et_packages.type_keepout_both_sides;
		route_restrict	: type_route_restrict;
		via_restrict	: type_via_restrict;
		copper			: type_copper; -- non-electric copper stuff, incl. floating polygons ! CS: rename to conductors
		contours		: type_pcb_contours; -- pcb outline
	end record;

	
end et_pcb;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
