------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                                PCB                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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
with et_libraries;				--use et_libraries;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_geometry;

package et_pcb is
	use geometry;
	
	-- If lines of a file are to be collected we use this simple list:
	package type_lines is new doubly_linked_lists (
		element_type => et_string_processing.type_fields_of_line,
		"=" => et_string_processing.lines_equally);
	
	use et_libraries.type_component_package_name;

	directory_name_length_max : constant positive := 200;
	package type_directory_name is new generic_bounded_length (directory_name_length_max);

	function to_string (directory_name : in type_directory_name.bounded_string) return string;
	-- Converts a directory name to a string.

-- 	function to_directory (directory_name : in string) return type_directory_name.bounded_string;
-- 	-- Converts a string to a type_directory_name.

	signal_layer_top : constant positive := 1;
	signal_layer_bot : constant positive := 100;
	type type_signal_layer is range signal_layer_top .. signal_layer_bot;

	function to_string (signal_layer : in type_signal_layer) return string;
	function to_signal_layer (layer : in string) return type_signal_layer;

	package type_signal_layers is new ordered_sets (type_signal_layer);

	text_size_min : constant type_distance := 1.0;
	text_size_max : constant type_distance := 100.0;
	subtype type_text_size is type_distance range text_size_min .. text_size_max;

	type type_text_dimensions is record
		width	: type_text_size := text_size_min;
		height	: type_text_size := text_size_min;
	end record;

	procedure validate_text_size (size : in type_distance);
	-- Checks whether given text size is in range of type_text_size.

	
	line_width_min : constant type_distance := 0.15;
	line_width_max : constant type_distance := 10.0;
	subtype type_text_line_width is type_distance range line_width_min .. line_width_max;

	procedure validate_text_line_width (width : in type_distance);
	-- Checks whether given line width is in range of type_text_line_width


	subtype type_general_line_width is type_distance range line_width_min .. line_width_max;

	procedure validate_general_line_width (width : in type_distance);
	-- Checks whether given line width is in range of type_general_line_width


	
	-- We fit the via diameter (incl. microvias) in a reasonable range via a subtype:
	drill_size_min : constant type_distance := 0.05;
	drill_size_max : constant type_distance := 10.0;
	subtype type_drill_size is type_distance range drill_size_min .. drill_size_max;

	procedure validate_drill_size (drill : in type_distance);
	-- Checks whether given drill size is in range of type_drill_size

	-- DRILLS
	type type_drill is tagged record
		position	: type_point;
		diameter	: type_drill_size;
		-- CS locked : type_locked;
	end record;

	function to_string (drill : in type_drill) return string;
	-- returns the properties of the given drill as string.
	
	pad_size_min : constant type_distance := 0.05;
	pad_size_max : constant type_distance := 10.0;
	subtype type_pad_size is type_distance range pad_size_min .. pad_size_max;

	procedure validate_pad_size (size : in type_distance);
	-- Checks whether given pad size is in range of type_pad_size


	pad_drill_offset_min : constant type_distance := zero;
	pad_drill_offset_max : constant type_distance := pad_size_max * 0.5;
	subtype type_pad_drill_offset is type_distance range pad_drill_offset_min .. pad_drill_offset_max;
	
	
	
	-- COPPER STRUCTURES GENERAL
	copper_structure_size_min : constant type_distance := 0.05;
	copper_clearance_min : constant type_distance := copper_structure_size_min;


	
	-- SIGNALS
	subtype type_track_clearance is type_distance range copper_clearance_min .. type_distance'last;

	procedure validate_track_clearance (clearance : in type_distance);
	-- Checks whether the given track clearance is in range of type_track_clearance.

	track_width_max : constant type_distance := 100.0;
	subtype type_track_width is type_distance range copper_structure_size_min .. track_width_max;

	procedure validate_track_width (track_width : in type_distance);
	-- Checks whether the given track width is in range of type_track_width.


	-- RESTRING
	restring_width_max : constant type_distance := 5.0;
	subtype type_restring_width is type_distance range copper_structure_size_min .. restring_width_max;

	procedure validate_restring_width (restring_width : in type_distance);
	-- Checks whether the given restring width is in range of type_restring_width.


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
	

	
	-- TEXT IN GENERAL
	type type_text is abstract tagged record
		position	: type_position;
		dimensions	: type_text_dimensions;
		line_width	: type_text_line_width := type_text_line_width'first;
		alignment	: et_libraries.type_text_alignment;
		-- CS locked : type_locked;		
	end record;

	function text_properties (text : in type_text) return string;
	-- Returns the properties of the given text in a long single string.	
	
	
	-- PLACEHOLDERS FOR TEXTS IN A PACKAGE
	type type_text_meaning_package is (REFERENCE, VALUE, PURPOSE);

	function to_string (text_meaning : in type_text_meaning_package) return string;
	function to_text_meaning (text_meaning : in string) return type_text_meaning_package;
	
	type type_text_placeholder_package is new type_text with record
		meaning : type_text_meaning_package := REFERENCE;
	end record;

	-- There can be lots of placeholders of this kind. So they are stored in a list:	
	package type_text_placeholders_package is new doubly_linked_lists (type_text_placeholder_package);

	-- Placeholders for device name (or reference) and value can be placed in
	-- silk screen or assembly documentation only:
	type type_placeholder_package_layer is (SILK_SCREEN, ASSEMBLY_DOCUMENTATION);
	function to_string (layer : in type_placeholder_package_layer) return string;
	function to_layer (layer : in string) return type_placeholder_package_layer;
	
	-- A collection of text placeholders in silk screen and assembly documentation 
	-- modelled by this type. The user is free to change them in the 
	-- layout (position, text size, rotation, line width ...).
	-- Initally, when a device is added to the schematic, these placeholders are 
	-- copies of the placeholders defined in the package model.
	type type_text_placeholders_silk_screen is record
		top		: type_text_placeholders_package.list;
		bottom	: type_text_placeholders_package.list;
	end record;

	type type_text_placeholders_assembly_documentation is record
		top		: type_text_placeholders_package.list;
		bottom	: type_text_placeholders_package.list;
	end record;

	type type_text_placeholders is record
		silk_screen	: type_text_placeholders_silk_screen;
		assy_doc	: type_text_placeholders_assembly_documentation;
	end record;




	
		
	-- PLACEHOLDERS FOR TEXTS IN COPPER LAYERS
	type type_text_meaning_copper is (
		PROJECT_NAME,
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


	
	
	-- PLACEHOLDERS FOR TEXTS IN BOARD DRAWING
	type type_text_meaning_pcb is (
		PROJECT_NAME,
		PROJECT_STATUS,
		DRAWING_NUMBER,
		DRAWING_STATUS,
		PERSON_NAME_DRAWN,
		PERSON_NAME_CHECKED,
		PERSON_NAME_APPROVED,
		DATE_DRAWN,
		DATE_CHECKED,
		DATE_APPROVED,
		CUSTOMER,
		REVISION
		);
		-- CS should be defined via configuration file:

	function to_string (meaning : in type_text_meaning_pcb) return string;
	function to_meaning (meaning : in string) return type_text_meaning_pcb;
	
	-- TEXT PLACEHOLDERS
	type type_text_placeholder_pcb is new type_text with record
		meaning : type_text_meaning_pcb;
	end record;

	package type_text_placeholders_pcb is new doubly_linked_lists (type_text_placeholder_pcb);



	-- TEXTS WITH CONTENT
	type type_text_with_content is new type_text with record
		content : et_libraries.type_text_content.bounded_string;
	end record;

	package type_texts_with_content is new doubly_linked_lists (type_text_with_content);
	

	-- FILL STYLE OF OBJECTS WITH A CLOSED CIRCUMFENCE
	type type_fill_style is (SOLID, HATCHED, CUTOUT);

	text_fill_style : constant string (1..10) := "fill_style";
	text_hatching_line_width	: constant string (1..19) := "hatching_line_width";
	text_hatching_spacing 		: constant string (1..16) := "hatching_spacing";	
	
	function to_string (fill_style : in type_fill_style) return string;
	function to_fill_style (fill_style : in string) return type_fill_style;

	fill_style_hatching_line_width_default	: constant type_distance := 0.3; -- the width of the lines
	fill_style_hatching_spacing_default		: constant type_distance := 2.0; -- the space between the lines

	package shapes is new et_geometry.shapes_2d (
		type_distance	=> et_pcb_coordinates.type_distance,
		type_rotation	=> et_pcb_coordinates.type_rotation,
		geometry		=> et_pcb_coordinates.geometry
		);

	
	-- LINE
	type type_line_2d is abstract tagged record
		start_point 	: type_point;
		end_point   	: type_point;
		-- CS locked : type_locked;
	end record;

-- 	type type_line_3d is abstract tagged record
-- 		start_point 	: type_point_3d;
-- 		end_point   	: type_point_3d;
-- 		-- CS locked : type_locked;
-- 	end record;
	
	-- ARC
	type type_arc_2d is abstract tagged record
		center			: type_point;
		start_point		: type_point;
		end_point		: type_point;
		-- CS locked : type_locked;		
	end record;

-- 	type type_arc_3d is abstract tagged record
-- 		center			: type_point_3d;
-- 		start_point		: type_point_3d;
-- 		end_point		: type_point_3d;
-- 		-- 		angle			: type_angle;
-- 		-- CS locked : type_locked;
-- 	end record;
	
	-- CIRCLE
	type type_circle_2d is abstract tagged record
		center			: type_point;
		radius  		: et_pcb_coordinates.type_distance := zero;
		-- CS locked : type_locked;
	end record;

-- 	type type_circle_3d is abstract tagged record
-- 		center			: type_point_3d;
-- 		radius  		: type_distance;
-- 		-- CS locked : type_locked;		
-- 	end record;

	
	-- POLYGON
	-- Corner points are collected in an ordered set.
	-- This prevents placing two identical points on top of each other.
	package type_polygon_points is new ordered_sets (
		element_type	=> type_point);
-- 		"<"				=> right_point_before_left_2d);

	type type_corner_easing is (NONE, CHAMFER, FILLET);
	
	polygon_easing_radius_max : constant et_pcb_coordinates.type_distance := 1.0;
	subtype type_polygon_easing_radius is et_pcb_coordinates.type_distance range et_pcb_coordinates.type_distance'first .. polygon_easing_radius_max;

	function to_corner_easing (easing : in string) return type_corner_easing;
	function to_string (easing : in type_corner_easing) return string;
	
	type type_polygon is abstract tagged record
		corners				: type_polygon_points.set;
		fill_style			: type_fill_style := SOLID; -- a polygon is always filled
		hatching_line_width	: type_track_width := fill_style_hatching_line_width_default; -- the with of the lines
		hatching_spacing	: type_track_clearance := fill_style_hatching_spacing_default; -- the space between the lines
		corner_easing		: type_corner_easing := NONE;
		easing_radius		: type_polygon_easing_radius := zero; -- center of circle at corner point
		-- CS locked : type_locked;
	end record;

	text_polygon_corner_points : constant string (1..13) := "corner_points";

	
	-- LOCK STATUS OF AN OBJECT
	type type_locked is (NO, YES);

	lock_status_default : constant type_locked := NO;
	function to_string (locked : in type_locked) return string;
	function to_lock_status (locked : in string) return type_locked;
	
	function to_string (line : in type_line_2d) return string;
	-- Returns the start and end point of the given line as string.

	function to_string (arc : in type_arc_2d) return string;
	-- Returns the start, end point and angle of the given arc as string.

	function to_string (circle : in type_circle_2d) return string;
	-- Returns the center and radius of the given circle as string.

	-- CS: functions as above for 3d line, arc, circle
	
	
	-- PCB CONTOUR/OUTLINE
	type type_pcb_contour_line is new type_line_2d with record -- CS: in the future type_line_3d
		locked : type_locked := type_locked'first;
	end record;

	package type_pcb_contour_lines is new doubly_linked_lists (type_pcb_contour_line);

	
	type type_pcb_contour_arc is new type_arc_2d with record  -- CS: in the future type_arc_3d
		locked : type_locked := type_locked'first;
	end record;
	package type_pcb_contour_arcs is new doubly_linked_lists (type_pcb_contour_arc);

	
	type type_pcb_contour_circle is new type_circle_2d with record  -- CS: in the future type_circle_3d
		locked : type_locked := type_locked'first;
	end record;
	package type_pcb_contour_circles is new doubly_linked_lists (type_pcb_contour_circle);

	
	type type_pcb_contour is record -- PCB contour defined for the PCB as a whole
		lines 	: type_pcb_contour_lines.list;
		arcs	: type_pcb_contour_arcs.list;
		circles	: type_pcb_contour_circles.list;
	end record;

	type type_package_pcb_contour is record -- PCB contour as defined by the package
		lines 	: type_pcb_contour_lines.list;
		arcs	: type_pcb_contour_arcs.list;
		circles	: type_pcb_contour_circles.list;
	end record;
	
	type type_package_pcb_contour_plated is record -- plated PCB contour as defined by the package
		lines 	: type_pcb_contour_lines.list;
		arcs	: type_pcb_contour_arcs.list;
		circles	: type_pcb_contour_circles.list;
	end record;

	procedure log_plated_millings (
		millings 		: in type_package_pcb_contour_plated;
		log_threshold	: in et_string_processing.type_log_level);
		
	-- PACKAGE CONTOUR/OUTLINE -- for 3d only
-- 	type type_package_contour_line is new type_line_3d with null record;
-- 	package type_package_contour_lines is new doubly_linked_lists (type_package_contour_line);

-- 	type type_package_contour_arc is new type_arc_3d with null record;
-- 	package type_package_contour_arcs is new doubly_linked_lists (type_package_contour_arc);

-- 	type type_package_contour_circle is new type_circle_3d with null record;
-- 	package type_package_contour_circles is new doubly_linked_lists (type_package_contour_circle);
	
-- 	type type_package_contour is record
-- 		lines 	: type_package_contour_lines.list;
-- 		arcs	: type_package_contour_arcs.list;
-- 		circles	: type_package_contour_circles.list;
-- 	end record;


	

	-- COPPER OBJECTS (NON ELECTRIC !!) OF A PACKAGE
	type type_copper_line is new type_line_2d with record
		width	: type_track_width;
	end record;
	package type_copper_lines is new doubly_linked_lists (type_copper_line);

	type type_copper_arc is new type_arc_2d with record
		width	: type_track_width;
	end record;
	package type_copper_arcs is new doubly_linked_lists (type_copper_arc);

	type type_filled is (YES, NO);
	function to_string (filled : in type_filled) return string;
	function to_filled (filled : in string) return type_filled;

	type type_copper_circle is new type_circle_2d with record
		width				: type_track_width := type_track_width'first;
		filled 				: type_filled := NO;
		fill_style			: type_fill_style := SOLID; -- don't care if filled is false
		hatching_line_width	: type_track_width := fill_style_hatching_line_width_default; -- the with of the lines
		hatching_spacing	: type_track_clearance := fill_style_hatching_spacing_default; -- the space between the lines
	end record;
	package type_copper_circles is new doubly_linked_lists (type_copper_circle);

	-- Polgon priority: 0 is weakest, 100 ist strongest.
	polygon_priority_max : constant natural := 100;
	subtype type_polygon_priority is natural range natural'first .. polygon_priority_max;
	function to_string (priority_level : in type_polygon_priority) return string;
	function to_polygon_priority (priority_level : in string) return type_polygon_priority;

	type type_copper_polygon is new type_polygon with record
		priority_level		: type_polygon_priority := type_polygon_priority'first;
		isolation_gap		: type_track_clearance := type_track_clearance'first; -- the space between foreign pads and the polygon
	end record;

	text_polygon_priority_level	: constant string (1..14) := "priority_level";
	text_polygon_isolation_gap	: constant string (1..13) := "isolation_gap";
	text_polygon_corner_easing	: constant string (1..13) := "corner_easing";
	text_polygon_easing_radius	: constant string (1..13) := "easing_radius";		
	
	package type_copper_polygons is new doubly_linked_lists (type_copper_polygon);
	
	-- Type for NON ELECTRIC !! copper objects of a package:
	type type_copper is record 
		lines 		: type_copper_lines.list;
		arcs		: type_copper_arcs.list;
		circles		: type_copper_circles.list;
		polygons	: type_copper_polygons.list;
		texts		: type_texts_with_content.list;
	end record;
	
	-- since NON ELECTRIC copper objects of a package can be on both sides 
	-- of the board we need this type. There is no reason for NON ELECTRIC 
	-- copper objects in inner layers. So we deal with top and bottom side only:
	type type_copper_package_both_sides is record
		top		: type_copper;
		bottom	: type_copper;
	end record;




	-- COPPER OBJECTS OF A PCB
	-- In a pcb drawing copper objects can be placed at various copper layers.
	-- This requires a layer id for the object.
	type type_copper_line_pcb is new type_copper_line with record
		layer	: type_signal_layer;
	end record;
	package type_copper_lines_pcb is new doubly_linked_lists (type_copper_line_pcb);

	type type_copper_arc_pcb is new type_copper_arc with record
		layer	: type_signal_layer;		
	end record;
	package type_copper_arcs_pcb is new doubly_linked_lists (type_copper_arc_pcb);

	type type_copper_circle_pcb is new type_copper_circle with record
		layer	: type_signal_layer;
	end record;
	package type_copper_circles_pcb is new doubly_linked_lists (type_copper_circle_pcb);

	polygon_thermal_width_min : constant type_track_width := type_track_width'first;
	polygon_thermal_width_max : constant type_track_width := 3.0; -- CS: adjust if nessecariy
	subtype type_polygon_thermal_width is et_pcb_coordinates.type_distance range polygon_thermal_width_min .. polygon_thermal_width_max;

	text_polygon_thermal_width : constant string (1..13) := "thermal_width";		
	
	-- If a terminal is connected/associated with a polyon, this is the space between pad and polygon:
	polygon_thermal_gap_min : constant type_track_clearance := type_track_clearance'first;
	polygon_thermal_gap_max : constant type_track_clearance := 3.0; -- CS: adjust if nessecariy
	subtype type_polygon_thermal_gap is type_track_clearance range polygon_thermal_gap_min .. polygon_thermal_gap_max;

	text_polygon_thermal_gap : constant string (1..11) := "thermal_gap";
	
	-- Polygons may be connected with associated pads via thermals, via solid connection or not at all:
	type type_polygon_pad_connection is (
		THERMAL,
		SOLID,
		NONE);

	text_polygon_pad_connection : constant string (1..14) := "pad_connection";
	
	function to_string (polygon_pad_connection : in type_polygon_pad_connection) return string;
	function to_pad_connection (connection : in string) return type_polygon_pad_connection;

	-- Polygons may be connected with SMT, THT or all pad technologies
	type type_polygon_pad_technology is (
		SMT_ONLY,
		THT_ONLY,
		SMT_AND_THT);

	text_polygon_pad_technology : constant string (1..15) := "connected_width";
	
	function to_string (polygon_pad_technology : in type_polygon_pad_technology) return string;
	function to_pad_technology (technology : in string) return type_polygon_pad_technology;

	-- A polygon in a signal layer is usually connected with a THT or SMD pads (or both) via thermals, solid (or not at all).
	-- For this reason we define a controlled type here because some properties may exist (or may not exists) depending
	-- on the kinde of pad_connection:
	type type_copper_polygon_signal (pad_connection : type_polygon_pad_connection) is new type_copper_polygon with record
		layer 		: type_signal_layer;
		width_min	: type_track_width; -- the minimum width
				
		case pad_connection is
			when THERMAL =>
				thermal_technology	: type_polygon_pad_technology; -- whether SMT, THT or both kinds of pads connect with the polygon
				thermal_width		: type_polygon_thermal_width; -- the thermal width
				thermal_gap			: type_polygon_thermal_gap; -- the space between associated pads and polygon -- CS: rename to thermal_length ?

			when SOLID =>
				solid_technology	: type_polygon_pad_technology; -- whether SMT, THT or both kinds of pads connect with the polygon
				-- no need for any kind of thermal parameters

			when NONE => null;
				-- no more properties required
		end case;
				
	end record;

	text_polygon_signal_layer	: constant string (1..12) := "signal_layer";	
	text_polygon_width_min		: constant string (1..13) := "minimum_width";
	
	package type_copper_polygons_signal is new indefinite_doubly_linked_lists (type_copper_polygon_signal);

	-- A floating copper polygon is not connected to a net:
	type type_copper_polygon_floating is new type_copper_polygon with record
		layer 		: type_signal_layer;
		width_min	: type_track_width; -- the minimum width
	end record;

	package type_copper_polygons_floating is new doubly_linked_lists (type_copper_polygon_floating);


	
	type type_text_with_content_pcb is new type_text_with_content with record
		layer	: type_signal_layer;
	end record;

	package type_texts_with_content_pcb is new doubly_linked_lists (type_text_with_content_pcb);


	
	-- Type for NON ELECTRIC !! copper objects:
	-- NON ELECTRIC copper objects of a pcb may also include text placeholders:
	type type_copper_pcb is record 
		lines 			: type_copper_lines_pcb.list;
		arcs			: type_copper_arcs_pcb.list;
		circles			: type_copper_circles_pcb.list;

		-- CS: It is probably no good idea to allow floating copper polygons.
		polygons		: type_copper_polygons_floating.list; 
		
		texts			: type_texts_with_content_pcb.list;
		placeholders	: type_text_placeholders_copper.list;
	end record;

	-- Types for ELECTRIC !! copper objects:
	-- vias
	type type_via is new type_drill with record
		restring_outer	: type_restring_width;	-- restring in outer layers (top/bottom)
		restring_inner	: type_restring_width;	-- restring in inner layers (mostly wider than restring_outer)
		layer_start		: type_signal_layer;
		layer_end		: type_signal_layer;
	end record;

	-- vias are collected in simple lists
	package type_vias is new doubly_linked_lists (type_via);
	
	-- route (tracks/traces, vias, polgons)
	type type_route is record 
		lines 			: type_copper_lines_pcb.list;
		arcs			: type_copper_arcs_pcb.list;
		vias			: type_vias.list;
		polygons		: type_copper_polygons_signal.list;
	end record;


	-- This circle type is used by silk screen, assembly doc, 
	-- stop mask, stencil, keepout, route restrict, via restrict
	type type_fillable_circle is new type_circle_2d with record
		width				: type_general_line_width := type_general_line_width'first; -- the width of the circumfence		
		filled 				: type_filled := NO;
		fill_style			: type_fill_style := SOLID; -- don't care if filled is false
		hatching_line_width	: type_general_line_width := fill_style_hatching_line_width_default; -- the width of the lines
		hatching_spacing	: type_general_line_width := fill_style_hatching_spacing_default; -- the space between the lines
	end record;
	
	
	-- SOLDER STOP MASK
	type type_stop_line is new type_line_2d with record
		width	: type_general_line_width;
	end record;

	package type_stop_lines is new doubly_linked_lists (type_stop_line);


	type type_stop_arc is new type_arc_2d with record
		width	: type_general_line_width;
	end record;

	package type_stop_arcs is new doubly_linked_lists (type_stop_arc);

	
	
	type type_stop_circle is new type_fillable_circle with null record;
	package type_stop_circles is new doubly_linked_lists (type_stop_circle);


	type type_stop_polygon is new type_polygon with null record;
	
	package type_stop_polygons is new doubly_linked_lists (type_stop_polygon);

	
	-- This is the type for stop mask objects in general:
	type type_stop_mask is tagged record
		lines 		: type_stop_lines.list;
		arcs		: type_stop_arcs.list;
		circles		: type_stop_circles.list;
		polygons	: type_stop_polygons.list;
		texts		: type_texts_with_content.list; -- for texts in copper to be exposed
	end record;

	-- Stop mask of packages (in library):
	type type_stop_mask_both_sides is record
		top		: type_stop_mask;
		bottom	: type_stop_mask;
	end record;

	-- Stop mask in board (may contain placeholders):
	type type_stop_mask_pcb is new type_stop_mask with record
		placeholders : type_text_placeholders_pcb.list; -- for texts in copper to be exposed
	end record;

	type type_stop_mask_pcb_both_sides is record
		top		: type_stop_mask_pcb;
		bottom	: type_stop_mask_pcb;
	end record;

	
	stop_mask_expansion_min : constant et_pcb_coordinates.type_distance := 0.02;
	stop_mask_expansion_max : constant et_pcb_coordinates.type_distance := 0.2;
	subtype type_stop_mask_expansion is et_pcb_coordinates.type_distance range stop_mask_expansion_min .. stop_mask_expansion_max;
	-- see <https://docs.oshpark.com/tips+tricks/stop-mask-expansion/>





	-- SOLDER PASTE STENCIL
	type type_stencil_line is new type_line_2d with record
		width	: type_general_line_width;
	end record;

	package type_stencil_lines is new doubly_linked_lists (type_stencil_line);


	type type_stencil_arc is new type_arc_2d with record
		width	: type_general_line_width;
	end record;

	package type_stencil_arcs is new doubly_linked_lists (type_stencil_arc);

	
	type type_stencil_circle is new type_fillable_circle with null record;
	package type_stencil_circles is new doubly_linked_lists (type_stencil_circle);


	type type_stencil_polygon is new type_polygon with null record; -- fill style hatched does not make sense
	package type_stencil_polygons is new doubly_linked_lists (type_stencil_polygon);
	
	
	-- This is the type for solder paste stencil objects in general:
	type type_stencil is record
		lines 		: type_stencil_lines.list;
		arcs		: type_stencil_arcs.list;
		circles		: type_stencil_circles.list;
		polygons	: type_stencil_polygons.list;
		-- CS: texts ? -- not reasonable and a waste of resources
	end record;

	-- Because stencil is about two sides of the board this composite is required:
	type type_stencil_both_sides is record
		top		: type_stencil;
		bottom	: type_stencil;
	end record;



	
	
	
	-- SILK SCREEN
	type type_silk_line is new type_line_2d with record
		width	: type_general_line_width;
	end record;

	package type_silk_lines is new doubly_linked_lists (type_silk_line);


	type type_silk_arc is new type_arc_2d with record
		width	: type_general_line_width;
	end record;

	package type_silk_arcs is new doubly_linked_lists (type_silk_arc);

	
	type type_silk_circle is new type_fillable_circle with null record;
	package type_silk_circles is new doubly_linked_lists (type_silk_circle);


	type type_silk_polygon is new type_polygon with null record;
	
	package type_silk_polygons is new doubly_linked_lists (type_silk_polygon);
	

	-- This is the base type for silk screen objects in general:
	type type_silk_screen is tagged record
		lines 		: type_silk_lines.list;
		arcs		: type_silk_arcs.list;
		circles		: type_silk_circles.list;
		polygons	: type_silk_polygons.list;
		texts		: type_texts_with_content.list;
	end record;

	-- Silk screen objects of a package (in the library) include placeholders:
	type type_silk_screen_package is new type_silk_screen with record
		placeholders: type_text_placeholders_package.list;
	end record;

	-- Because silk screen is about two sides of the board this composite is required:
	type type_silk_screen_package_both_sides is record
		top		: type_silk_screen_package;
		bottom	: type_silk_screen_package;
	end record;

	-- For silk screen objects that do not belong to any packages use this type:
	type type_silk_screen_pcb is new type_silk_screen with record
		-- Placeholders for revision, board name, misc ... :
		placeholders : type_text_placeholders_pcb.list;
	end record;
		

	-- Because silk screen is about two sides of the board this composite is required:	
	type type_silk_screen_pcb_both_sides is record
		top 	: type_silk_screen_pcb;
		bottom	: type_silk_screen_pcb;
	end record;


	

	-- ASSEMBLY DOCUMENTATION
	type type_doc_line is new type_line_2d with record
		width	: type_general_line_width;
	end record;

	package type_doc_lines is new doubly_linked_lists (type_doc_line);


	type type_doc_arc is new type_arc_2d with record
		width	: type_general_line_width;
	end record;

	package type_doc_arcs is new doubly_linked_lists (type_doc_arc);

	
	type type_doc_circle is new type_fillable_circle with null record;
	package type_doc_circles is new doubly_linked_lists (type_doc_circle);


	type type_doc_polygon is new type_polygon with null record;
	
	package type_doc_polygons is new doubly_linked_lists (type_doc_polygon);

	
	-- This is the base type for assembly documentation objects in general:
	type type_assembly_documentation is tagged record
		lines 		: type_doc_lines.list;
		arcs		: type_doc_arcs.list;
		circles		: type_doc_circles.list;
		polygons	: type_doc_polygons.list;
		texts		: type_texts_with_content.list;
	end record;

	-- Assembly documentation objects of a package (in the library) include placeholders:
	type type_assembly_documentation_package is new type_assembly_documentation with record
		placeholders: type_text_placeholders_package.list;
	end record;

	-- Because assembly documentation is about two sides of the board this composite is required:
	type type_assembly_documentation_package_both_sides is record
		top		: type_assembly_documentation_package;
		bottom	: type_assembly_documentation_package;
	end record;

	-- For assembly documentation objects that do not belong to any packages use this type:
	type type_assembly_documentation_pcb is new type_assembly_documentation with record
		-- Placeholders for revision, board name, misc ... :
		placeholders : type_text_placeholders_pcb.list;
	end record;


	-- Because assembly documentation is about two sides of the board this composite is required:	
	type type_assembly_documentation_pcb_both_sides is record
		top 	: type_assembly_documentation_pcb;
		bottom	: type_assembly_documentation_pcb;
	end record;


	
	
	-- KEEPOUT
	type type_keepout_line is new type_line_2d with record
		width	: type_general_line_width;
	end record;
	
	package type_keepout_lines is new doubly_linked_lists (type_keepout_line);

	type type_keepout_arc is new type_arc_2d with record
		width	: type_general_line_width;
	end record;
		
	package type_keepout_arcs is new doubly_linked_lists (type_keepout_arc);
	
	type type_keepout_circle is new type_fillable_circle with null record;	
	package type_keepout_circles is new doubly_linked_lists (type_keepout_circle);

	type type_keepout_polygon is new type_polygon with null record;
	package type_keepout_polygons is new doubly_linked_lists (type_keepout_polygon);

	
	type type_keepout is record
		lines 		: type_keepout_lines.list;
		arcs		: type_keepout_arcs.list;
		circles		: type_keepout_circles.list;
		polygons	: type_keepout_polygons.list;
		-- CS texts		: type_texts_with_content.list; -- for placement notes ?
	end record;

	type type_keepout_both_sides is record
		top 	: type_keepout;
		bottom	: type_keepout;
	end record;
	


	
	-- ROUTE RESTRICT
	type type_route_restrict_line is new type_line_2d with record
		width	: type_general_line_width; -- CS use subtype for reasonable range
		layers 	: type_signal_layers.set;
	end record;
	
	package type_route_restrict_lines is new doubly_linked_lists (type_route_restrict_line);

	type type_route_restrict_arc is new type_arc_2d with record
		width	: type_general_line_width; -- CS use subtype for reasonable range
		layers 	: type_signal_layers.set;
	end record;
	
	package type_route_restrict_arcs is new doubly_linked_lists (type_route_restrict_arc);
	
	type type_route_restrict_circle is new type_fillable_circle with record
		layers 	: type_signal_layers.set;
	end record;
	package type_route_restrict_circles is new doubly_linked_lists (type_route_restrict_circle);

	type type_route_restrict_polygon is new type_polygon with record
		width	: type_general_line_width; -- CS use subtype for reasonable range
		layers 	: type_signal_layers.set;
	end record;
	package type_route_restrict_polygons is new doubly_linked_lists (type_route_restrict_polygon);

	
	-- this is the base type for route restrict objects
	type type_route_restrict is tagged record
		lines 		: type_route_restrict_lines.list;
		arcs		: type_route_restrict_arcs.list;
		circles		: type_route_restrict_circles.list;
		polygons	: type_route_restrict_polygons.list;
		-- CS texts		: type_texts_with_content.list; -- for routing notes ? mind signal layer !
	end record;
	
	type type_route_restrict_package is new type_route_restrict with null record;
	type type_route_restrict_pcb is new type_route_restrict with null record;



	

	-- VIA RESTRICT
	type type_via_restrict_line is new type_line_2d with record
		width	: type_general_line_width; -- CS use subtype for reasonable range
		layers	: type_signal_layers.set;
	end record;
	
	package type_via_restrict_lines is new doubly_linked_lists (type_via_restrict_line);

	
	type type_via_restrict_arc is new type_arc_2d with record
		width	: type_general_line_width; -- CS use subtype for reasonable range
		layers	: type_signal_layers.set;
	end record;
	
	package type_via_restrict_arcs is new doubly_linked_lists (type_via_restrict_arc);

	
	type type_via_restrict_circle is new type_fillable_circle with record
		layers	: type_signal_layers.set;
	end record;
	
	package type_via_restrict_circles is new doubly_linked_lists (type_via_restrict_circle);

	
	type type_via_restrict_polygon is new type_polygon with record
		width	: type_general_line_width; -- CS use subtype for reasonable range
		layers 	: type_signal_layers.set;
	end record;

	package type_via_restrict_polygons is new doubly_linked_lists (type_via_restrict_polygon);

	
	
	-- this is the base type for via restrict objects
	type type_via_restrict is tagged record
		lines 		: type_via_restrict_lines.list;
		arcs		: type_via_restrict_arcs.list;
		circles		: type_via_restrict_circles.list;
		polygons	: type_via_restrict_polygons.list;
		-- CS texts		: type_texts_with_content.list; -- for via notes ?
	end record;
	
	type type_via_restrict_package is new type_via_restrict with null record;
	type type_via_restrict_pcb is new type_via_restrict with null record;
	



	type type_package_appearance is (
		REAL,	-- packages with x,y,z dimension
		VIRTUAL -- for things that do not have a package (netchangers, testpoints, edge connectors, ...)
		);	

	package_appearance_default : constant type_package_appearance := REAL;
	function to_string (appearance : in type_package_appearance) return string;
	function to_appearance (appearance : in string) return type_package_appearance;
	
	type type_assembly_technology is (
		THT,	-- Through Hole Technology
		SMT		-- Surface Mount Technology
		);

	assembly_technology_default : constant type_assembly_technology := SMT;
	function to_string (technology : in type_assembly_technology) return string;
	function to_assembly_technology (technology : in string) return type_assembly_technology;
	
	type type_solder_paste_status is (NONE, APPLIED);
	solder_paste_status_default : constant type_solder_paste_status := APPLIED;
	function to_string (solder_paste : in type_solder_paste_status) return string;
	function to_solder_paste_status (solder_paste : in string) return type_solder_paste_status;
	
	type type_stop_mask_status is (CLOSED, OPEN);  -- net-ties or netchangers have their pads covered
	stop_mask_status_default : constant type_stop_mask_status := OPEN;
	function to_string (stop_mask : in type_stop_mask_status) return string;
	function to_stop_mask_status (stop_mask : in string) return type_stop_mask_status;
	
	-- A THT terminal may have a drilled or a milled hole (milled hole is also called "plated millings")
	type type_terminal_tht_hole is (DRILLED, MILLED);
	terminal_tht_hole_default : constant type_terminal_tht_hole := DRILLED;
	function to_string (tht_hole : in type_terminal_tht_hole) return string;
	function to_tht_hole (tht_hole : in string) return type_terminal_tht_hole;

	
	-- A pad outline consists of lines, arcs, circles, polygons:
	type type_pad_line is new type_line_2d with null record;
	type type_pad_arc is new type_arc_2d with null record;
	type type_pad_circle is new type_circle_2d with null record;
	type type_pad_polygon is record corners : type_polygon_points.set; end record;

	package type_pad_lines is new doubly_linked_lists (type_pad_line);
	package type_pad_arcs is new doubly_linked_lists (type_pad_arc);
	package type_pad_circles is new doubly_linked_lists (type_pad_circle);
	package type_pad_polygons is new doubly_linked_lists (type_pad_polygon);
	
	type type_pad_outline is record
		lines 		: type_pad_lines.list;
		arcs		: type_pad_arcs.list;
		circles		: type_pad_circles.list;
		polygons	: type_pad_polygons.list; 
	end record;	

	type type_pad_outline_tht is record
		top			: type_pad_outline; -- The pad shape on the top side
		bottom		: type_pad_outline; -- is not nessecarily the same as on the bottom side.
	end record;
	
	type type_terminal (
		technology	: type_assembly_technology; -- smt/tht
		tht_hole	: type_terminal_tht_hole) -- drilled/milled, without meaning if technology is SMT
		is tagged record

			position : type_position; -- drill position or center of pad
			-- The angle is useful for exotic pad contours. The operator would be drawing the 
			-- contour with zero rotation first (which is easier). Then by applying an angle,
			-- the countour would be rotated to its final position.
			
		case technology is
			when THT =>
				pad_shape_tht : type_pad_outline_tht; -- top and bottom shape

				-- This is the width of the copper surrounding the hole in inner layers.
				-- Since the hole can be of any shape we do not speak about restring.
				-- The shape of the copper area around the hole is the same as the shape of the 
				-- hole. No further contours possible.
				width_inner_layers : et_pcb_coordinates.type_distance; -- CS use subtype for reasonable range
				
				case tht_hole is
					when DRILLED =>
						drill_size : type_drill_size;
						
					when MILLED =>
						millings : type_package_pcb_contour_plated;
				end case;
				
			when SMT =>
				pad_shape		: type_pad_outline;
				face			: type_face;
				stop_mask 		: type_stop_mask_status;
				solder_paste	: type_solder_paste_status;
				
		end case;
	end record;
	

	procedure terminal_properties (
	-- Logs the properties of the given terminal.
		terminal		: in et_pcb.type_terminal;
		name			: in et_libraries.type_terminal_name.bounded_string;
		log_threshold 	: in et_string_processing.type_log_level);
	
	package type_terminals is new indefinite_ordered_maps (
		key_type		=> et_libraries.type_terminal_name.bounded_string, -- H7, 14
		element_type	=> type_terminal,
		"<"				=> et_libraries.type_terminal_name."<");


	package_description_length_max : constant positive := 200;
	package type_package_description is new generic_bounded_length (package_description_length_max);

	function to_string (
		description : in type_package_description.bounded_string;
		verbose		: in boolean := false) return string;

	function to_package_description (description : in string) return type_package_description.bounded_string;
	
	package_tags_length_max : constant positive := 200;
	package type_package_tags is new generic_bounded_length (package_tags_length_max);

	function to_string (tags : in type_package_tags.bounded_string) return string;

	function to_package_tags (tags : in string) return type_package_tags.bounded_string;

	
	-- This is the base type of a package:
	type type_package_base (appearance : type_package_appearance) is abstract tagged record
		description				: type_package_description.bounded_string;
		copper					: type_copper_package_both_sides; -- non-electric objects
		keepout 				: type_keepout_both_sides;
		stop_mask				: type_stop_mask_both_sides;
		stencil					: type_stencil_both_sides;

		route_restrict 			: type_route_restrict_package;
		via_restrict 			: type_via_restrict_package;
		-- CS holes

		-- PCB contour or so called "non-plated millings"
		pcb_contour				: type_package_pcb_contour; 

		-- Plated millings. NOTE: NOT FOR SLITTED HOLES ! See type_terminal instead.
		-- CS: currently no need for such things
		--pcb_contour_plated 		: type_package_pcb_contour_plated;
		
		technology				: type_assembly_technology; -- set by majority of terminals
		
		-- Only REAL packages have 3d contours:
		case appearance is
			when REAL =>
				null; -- CS
				--package_contour	: type_package_contour;
			when VIRTUAL =>
				null; -- netchangers, testpoints, ISA-Board edge connectors, ...
		end case;
	end record;

	function package_position (position : in type_package_position) return string;
	-- Returns the coordinates of a package (in a board) as string.


	
	
-- LIBRARIES
	
	-- A package in the library extends the base package type:
	type type_package is new type_package_base with record
		silk_screen				: type_silk_screen_package_both_sides; -- incl. placeholder for reference and purpose
		assembly_documentation	: type_assembly_documentation_package_both_sides; -- incl. placeholder for value
		terminals				: type_terminals.map;
	end record;

	-- packages
	package type_packages is new indefinite_ordered_maps (
		key_type		=> et_libraries.type_package_model_file.bounded_string, -- ../lbr/smd/SO15.pac
		"<"				=> et_libraries.type_package_model_file."<",
		element_type	=> type_package);

	library_file_extension : constant string (1..3) := "pac";

	-- HERE RIG WIDE PACKAGES ARE KEPT:
	packages : type_packages.map;

	function locate_package_model (model_name : in et_libraries.type_package_model_file.bounded_string) -- ../lbr/smd/SO15.pac
	-- Returns a cursor to the given package model.
		return type_packages.cursor;
	
	function is_real (package_name : in et_libraries.type_package_model_file.bounded_string) return boolean;
	-- Returns true if the given package is real (means it has a height).
	
-- PROPERTIES OF OBJECTS IN COPPER (NON ELECTRIC !!)
	procedure line_copper_properties (
	-- Logs the properties of the given line of copper
		face			: in type_face;
		cursor			: in type_copper_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure arc_copper_properties (
	-- Logs the propetype_texts_with_contentrties of the given arc of copper
		face			: in type_face;
		cursor			: in type_copper_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure circle_copper_properties (
	-- Logs the properties of the given circle of copper
		face			: in type_face;
		cursor			: in type_copper_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure text_copper_properties (
	-- Logs the properties of the given text of copper
		cursor			: in type_texts_with_content_pcb.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	

-- PROPERTIES OF ELECTRIC OBJECTS IN SIGNAL LAYERS
	procedure route_line_properties (
	-- Logs the properties of the given line of a route
		cursor			: in type_copper_lines_pcb.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure route_via_properties (
	-- Logs the properties of the given via of a route
		cursor			: in type_vias.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure route_polygon_properties (
	-- Logs the properties of the given polygon of a route
		cursor			: in type_copper_polygons_signal.cursor;
		log_threshold 	: in et_string_processing.type_log_level);
	
	procedure floating_copper_polygon_properties (
	-- Logs the properties of the given floating copper polygon.
		cursor			: in type_copper_polygons_floating.cursor;
		log_threshold 	: in et_string_processing.type_log_level);


	
-- PROPERTIES OF OBJECTS IN SILK SCREEN	
	procedure line_silk_screen_properties (
	-- Logs the properties of the given line of silk screen
		face			: in type_face;
		cursor			: in type_silk_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure arc_silk_screen_properties (
	-- Logs the properties of the given arc of silk screen
		face			: in type_face;
		cursor			: in type_silk_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure circle_silk_screen_properties (
	-- Logs the properties of the given circle of silk screen
		face			: in type_face;
		cursor			: in type_silk_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure placeholder_silk_screen_properties (
	-- Logs the properties of the given silk screen placeholder
		face			: in type_face;
		cursor			: in type_text_placeholders_package.cursor;
		log_threshold 	: in et_string_processing.type_log_level);
	
	procedure text_silk_screen_properties (
	-- Logs the properties of the given silk screen text
		face			: in type_face;
		cursor			: in type_texts_with_content.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	
	
-- PROPERTIES OF OBJECTS IN ASSEMBLY DOCUMENTATION	
	procedure line_assy_doc_properties (
	-- Logs the properties of the given line of assembly documentation
		face			: in type_face;
		cursor			: in type_doc_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure arc_assy_doc_properties (
	-- Logs the properties of the given arc of assembly documentation
		face			: in type_face;
		cursor			: in type_doc_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure circle_assy_doc_properties (
	-- Logs the properties of the given circle of assembly documentation
		face			: in type_face;
		cursor			: in type_doc_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure placeholder_assy_doc_properties (
	-- Logs the properties of the given assembly documentation placeholder
		face			: in type_face;
		cursor			: in type_text_placeholders_package.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure text_assy_doc_properties (
	-- Logs the properties of the given assembly documentation text
		face			: in type_face;
		cursor			: in type_texts_with_content.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	

	
-- PROPERTIES OF OBJECTS IN KEEPOUT	
	procedure line_keepout_properties (
	-- Logs the properties of the given line of keepout
		face			: in type_face;
		cursor			: in type_keepout_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure arc_keepout_properties (
	-- Logs the properties of the given arc of keepout
		face			: in type_face;
		cursor			: in type_keepout_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure circle_keepout_properties (
	-- Logs the properties of the given circle of keepout
		face			: in type_face;
		cursor			: in type_keepout_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level);



-- PROPERTIES OF OBJECTS IN STOP MASK
	procedure arc_stop_mask_properties (
	-- Logs the properties of the given arc of stop mask
		face			: in type_face;
		cursor			: in type_stop_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure circle_stop_mask_properties (
	-- Logs the properties of the given circle of stop mask
		face			: in type_face;
		cursor			: in type_stop_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure line_stop_mask_properties (
	-- Logs the properties of the given line of stop mask
		face			: in type_face;
		cursor			: in type_stop_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure text_stop_mask_properties (
	-- Logs the properties of the given stop mask text
		face			: in type_face;
		cursor			: in type_texts_with_content.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	

-- PROPERTIES OF OBJECTS IN SOLDER PASTE / STENCIL
	procedure arc_stencil_properties (
	-- Logs the properties of the given arc of stencil
		face			: in type_face;
		cursor			: in type_stencil_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure circle_stencil_properties (
	-- Logs the properties of the given circle of stencil
		face			: in type_face;
		cursor			: in type_stencil_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure line_stencil_properties (
	-- Logs the properties of the given line of stencil
		face			: in type_face;
		cursor			: in type_stencil_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level);
	

	
	
-- PROPERTIES OF OBJECTS IN ROUTE RESTRICT	
	procedure line_route_restrict_properties (
	-- Logs the properties of the given line of route restrict
		face			: in type_face;
		cursor			: in type_route_restrict_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure arc_route_restrict_properties (
	-- Logs the properties of the given arc of route restrict
		face			: in type_face;
		cursor			: in type_route_restrict_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level);


	
-- PROPERTIES OF OBJECTS IN VIA RESTRICT		
	procedure line_via_restrict_properties (
	-- Logs the properties of the given line of via restrict
		face			: in type_face;
		cursor			: in type_via_restrict_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure arc_via_restrict_properties (
	-- Logs the properties of the given arc of via restrict
		face			: in type_face;
		cursor			: in type_via_restrict_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level);



-- PROPERTIES OF OBJECTS IN BOARD CONTOUR / OUTLINE / EDGE CUTS
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
	


	



	-- This is general board stuff:
	type type_board is tagged record
		silk_screen		: type_silk_screen_pcb_both_sides;
		assy_doc		: type_assembly_documentation_pcb_both_sides;
		stencil			: type_stencil_both_sides;
		stop_mask		: type_stop_mask_pcb_both_sides;
		keepout			: type_keepout_both_sides;
		route_restrict	: type_route_restrict_pcb;
		via_restrict	: type_via_restrict_pcb;
		copper			: type_copper_pcb; -- non-electric copper stuff, incl. floating polygons !
		contour			: type_pcb_contour;
	end record;

	
end et_pcb;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
