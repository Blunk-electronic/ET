------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             PACKAGES                                     --
--                                                                          --
--                              S p e c                                     --
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
--   to do:


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
with et_pcb_stack;				use et_pcb_stack;

package et_packages is
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

	text_size_min : constant type_distance := 1.0;
	text_size_max : constant type_distance := 100.0;
	subtype type_text_size is type_distance range text_size_min .. text_size_max;

	type type_text_dimensions is record
		width	: type_text_size := text_size_min;
		height	: type_text_size := text_size_min;
	end record;

	procedure validate_text_size (size : in type_distance);
	-- Checks whether given text size is in range of type_text_size.

	keyword_line_width : constant string := "line_width";	
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
	keyword_restring_outer_layers : constant string := "restring_outer_layers";
	keyword_restring_inner_layers : constant string := "restring_inner_layers";		

	restring_width_max : constant type_distance := 5.0;
	subtype type_restring_width is type_distance range copper_structure_size_min .. restring_width_max;

	procedure validate_restring_width (restring_width : in type_distance);
	-- Checks whether the given restring width is in range of type_restring_width.


	
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
	type type_text_meaning_package is (NAME, VALUE, PURPOSE);

	function to_string (text_meaning : in type_text_meaning_package) return string;
	function to_text_meaning (text_meaning : in string) return type_text_meaning_package;
	
	type type_text_placeholder is new type_text with record
		meaning : type_text_meaning_package := NAME;
	end record;

	-- There can be lots of placeholders of this kind. So they are stored in a list:	
	package pac_text_placeholders is new doubly_linked_lists (type_text_placeholder);

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
		top		: pac_text_placeholders.list;
		bottom	: pac_text_placeholders.list;
	end record;

	type type_text_placeholders_assembly_documentation is record
		top		: pac_text_placeholders.list;
		bottom	: pac_text_placeholders.list;
	end record;

	type type_text_placeholders is record -- CS no need any more
		silk_screen	: type_text_placeholders_silk_screen;
		assy_doc	: type_text_placeholders_assembly_documentation;
	end record;


	-- TEXTS WITH CONTENT
	type type_text_with_content is new type_text with record
		content : et_libraries.type_text_content.bounded_string;
	end record;

	package type_texts_with_content is new doubly_linked_lists (type_text_with_content);
	


	


	-- Instantiation of the generic shapes package et_geometry.shapes_2d:
	package shapes is new et_geometry.shapes_2d (geometry => et_pcb_coordinates.geometry);
	use shapes;
	

	-- FILL STYLE OF OBJECTS WITH A CLOSED CIRCUMFENCE		
	keyword_fill_style : constant string := "fill_style";	
	type type_fill_style is (SOLID, HATCHED);
	fill_style_default : constant type_fill_style := SOLID;
	
	function to_string (fill_style : in type_fill_style) return string;
	function to_fill_style (fill_style : in string) return type_fill_style;


	-- HATCHING OF OBJECTS WITH CLOSED CIRCUMFENCE
	keyword_hatching_line_width		: constant string := "hatching_line_width";
	keyword_hatching_line_spacing	: constant string := "hatching_line_spacing";		

	hatching_line_width_default : constant type_distance_positive := 0.2;
	hatching_spacing_default	: constant type_distance_positive := 1.0;
	
	
	type type_hatching is record
		-- the width of the border line
		border_width	: type_distance_positive := 1.0;
		
		-- the with of the lines inside the area:
		width	: type_distance_positive := hatching_line_width_default; -- CS rename to line_width

		-- the space between the lines inside the area:
		spacing	: type_distance_positive := hatching_spacing_default;
	end record;

	
	-- EASING
	keyword_corner_easing : constant string := "corner_easing";
	keyword_easing_radius : constant string := "easing_radius";	

	type type_corner_easing is (NONE, CHAMFER, FILLET);

	function to_corner_easing (easing : in string) return type_corner_easing;
	function to_string (easing : in type_corner_easing) return string;
	
	easing_radius_max : constant type_distance_positive := 100.0;
	subtype type_easing_radius is type_distance_positive range type_distance_positive'first .. easing_radius_max;

	type type_polygon_easing is record -- CS rename to type_easing because it is general
		style	: type_corner_easing := NONE;
		radius	: type_easing_radius := zero; -- center of circle at corner point
	end record;

	
	-- POLYGON
	type type_polygon (fill_style : type_fill_style) is new type_polygon_base with record
		easing : type_polygon_easing;
		
		case fill_style is
			when SOLID		=> null;
			when HATCHED	=> hatching : type_hatching;
		end case;
	end record;

	type type_cutout_zone is new type_polygon_base with record
		easing : type_polygon_easing;
	end record;


	
	-- Corner points are collected in an ordered set.
	-- This prevents placing two identical points on top of each other.
	package type_polygon_points is new ordered_sets (type_point); -- CS remove


	

	type type_copper_line is new type_line with record
		width	: type_track_width;
	end record;
	package type_copper_lines is new doubly_linked_lists (type_copper_line);

	type type_copper_arc is new type_arc with record
		width	: type_track_width;
	end record;
	package type_copper_arcs is new doubly_linked_lists (type_copper_arc);

	type type_copper_circle is new type_circle with record
		width				: type_track_width := type_track_width'first;
		filled 				: type_filled := NO;
		fill_style			: type_fill_style := SOLID; -- don't care if filled is false
		hatching_line_width	: type_track_width := hatching_line_width_default; -- the with of the lines
		hatching_spacing	: type_track_clearance := hatching_spacing_default; -- the space between the lines
	end record;
	package type_copper_circles is new doubly_linked_lists (type_copper_circle);


	-- the space between foreign pads and the polygon outline
	keyword_isolation : constant string := "isolation";

	-- the minimal width of a polygon
	keyword_min_width : constant string := "min_width";
	
	type type_copper_polygon_solid is new type_polygon (fill_style => SOLID) with record
		width_min : type_track_width; -- the minimum width
		isolation : type_track_clearance := type_track_clearance'first; 
	end record;

	package pac_copper_polygons_solid is new doubly_linked_lists (type_copper_polygon_solid);

	type type_copper_polygon_hatched is new type_polygon (fill_style => HATCHED) with record
		width_min : type_track_width; -- the minimum width
		isolation : type_track_clearance := type_track_clearance'first;
	end record;

	package pac_copper_polygons_hatched is new doubly_linked_lists (type_copper_polygon_hatched);

	-- A cutout-polygon used in copper layers:
	package pac_copper_cutouts is new doubly_linked_lists (type_cutout_zone);

	

	


	type type_copper_polygons is record
		solid	: pac_copper_polygons_solid.list;
		hatched	: pac_copper_polygons_hatched.list;
	end record;

		
	
	type type_copper is record 
		lines 		: type_copper_lines.list;
		arcs		: type_copper_arcs.list;
		circles		: type_copper_circles.list;
		polygons	: type_copper_polygons;
		cutouts		: pac_copper_cutouts.list;
		texts		: type_texts_with_content.list;
	end record;
	
	-- since NON ELECTRIC copper objects of a package can be on both sides 
	-- of the board we need this type. There is no reason for NON ELECTRIC 
	-- copper objects in inner layers. So we deal with top and bottom side only:
	type type_copper_both_sides is record
		top		: type_copper;
		bottom	: type_copper;
	end record;





	-- This circle type is used by silk screen, assembly doc, stop mask, stencil
	type type_fillable_circle ( -- CS rework
		filled		: type_filled;
		fill_style	: type_fill_style -- don't care if filled is NO
		)
		is new type_circle with record
		case filled is
			when NO => 
				-- the line width of the circumfence:
				width : type_general_line_width := type_general_line_width'first; -- CS rename to width_circumfence

			when YES =>
				case fill_style is
					when SOLID => null;
					when HATCHED =>
						-- CS use type_hatching here:
						
						-- the width of the circumfence and the hatching lines:
						hatching_line_width	: type_general_line_width := hatching_line_width_default;

						-- the space between the hatching lines:
						hatching_spacing	: type_general_line_width := hatching_spacing_default;
				end case;
				
		end case;
	end record;

	-- CS type_circle_cutout ?
	
	function to_string (circle : in type_fillable_circle) return string;

	-- This circle type is used by keepout, route restrict, via restrict.
	-- The fill style is always solid, hence no discrimintant for fiil style.
	type type_fillable_circle_solid is new type_circle with record
		filled : type_filled;
	end record;

	
-- SOLDER STOP MASK
	type type_stop_line is new type_line with record
		width	: type_general_line_width;
	end record;

	package type_stop_lines is new doubly_linked_lists (type_stop_line);


	type type_stop_arc is new type_arc with record
		width	: type_general_line_width;
	end record;

	package type_stop_arcs is new doubly_linked_lists (type_stop_arc);

	package type_stop_circles is new indefinite_doubly_linked_lists (type_fillable_circle);

	package type_stop_polygons is new indefinite_doubly_linked_lists (type_polygon);
	package pac_stop_cutouts is new doubly_linked_lists (type_cutout_zone);
	
	-- This is the type for stop mask objects in general:
	type type_stop_mask is tagged record
		lines 		: type_stop_lines.list;
		arcs		: type_stop_arcs.list;
		circles		: type_stop_circles.list;
		polygons	: type_stop_polygons.list;
		cutouts		: pac_stop_cutouts.list;
		texts		: type_texts_with_content.list; -- for texts in copper to be exposed
	end record;

	-- Stop mask of packages:
	type type_stop_mask_both_sides is record
		top		: type_stop_mask;
		bottom	: type_stop_mask;
	end record;


	
	stop_mask_expansion_min : constant et_pcb_coordinates.type_distance := 0.02;
	stop_mask_expansion_max : constant et_pcb_coordinates.type_distance := 0.2;
	subtype type_stop_mask_expansion is et_pcb_coordinates.type_distance range stop_mask_expansion_min .. stop_mask_expansion_max;
	-- see <https://docs.oshpark.com/tips+tricks/stop-mask-expansion/>





-- STENCIL
	type type_stencil_line is new type_line with record
		width	: type_general_line_width;
	end record;

	package type_stencil_lines is new doubly_linked_lists (type_stencil_line);


	type type_stencil_arc is new type_arc with record
		width	: type_general_line_width;
	end record;

	package type_stencil_arcs is new doubly_linked_lists (type_stencil_arc);

	package type_stencil_circles is new indefinite_doubly_linked_lists (type_fillable_circle);

	package type_stencil_polygons is new indefinite_doubly_linked_lists (type_polygon);
	package pac_stencil_cutouts is new doubly_linked_lists (type_cutout_zone);	
	
	-- This is the type for solder paste stencil objects in general:
	type type_stencil is record
		lines 		: type_stencil_lines.list;
		arcs		: type_stencil_arcs.list;
		circles		: type_stencil_circles.list;
		polygons	: type_stencil_polygons.list;
		cutouts		: pac_stencil_cutouts.list;
		-- CS: texts ? -- not reasonable and a waste of resources
	end record;

	-- Because stencil is about two sides of the board this composite is required:
	type type_stencil_both_sides is record
		top		: type_stencil;
		bottom	: type_stencil;
	end record;



	
	
	
-- SILK SCREEN
	type type_silk_line is new type_line with record
		width	: type_general_line_width;
	end record;

	package type_silk_lines is new doubly_linked_lists (type_silk_line);


	type type_silk_arc is new type_arc with record
		width	: type_general_line_width;
	end record;

	package type_silk_arcs is new doubly_linked_lists (type_silk_arc);
	
	package type_silk_circles is new indefinite_doubly_linked_lists (type_fillable_circle);

	package pac_silk_polygons is new indefinite_doubly_linked_lists (type_polygon);
	package pac_silk_cutouts is new doubly_linked_lists (type_cutout_zone);	
	

	-- This is the base type for silk screen objects in general:
	type type_silk_screen_base is tagged record
		lines 		: type_silk_lines.list;
		arcs		: type_silk_arcs.list;
		circles		: type_silk_circles.list;
		polygons	: pac_silk_polygons.list;
		cutouts 	: pac_silk_cutouts.list;
		texts		: type_texts_with_content.list;
	end record;

	-- Silk screen objects of a package (in the library) include placeholders:
	type type_silk_screen is new type_silk_screen_base with record
		placeholders: pac_text_placeholders.list;
	end record;

	-- Because silk screen is about two sides of the board this composite is required:
	type type_silk_screen_both_sides is record
		top		: type_silk_screen;
		bottom	: type_silk_screen;
	end record;


	

-- ASSEMBLY DOCUMENTATION
	type type_doc_line is new type_line with record
		width	: type_general_line_width;
	end record;

	package type_doc_lines is new doubly_linked_lists (type_doc_line);


	type type_doc_arc is new type_arc with record
		width	: type_general_line_width;
	end record;

	package type_doc_arcs is new doubly_linked_lists (type_doc_arc);
	
	package type_doc_circles is new indefinite_doubly_linked_lists (type_fillable_circle);
	
	package pac_doc_polygons is new indefinite_doubly_linked_lists (type_polygon);
	package pac_doc_cutouts is new doubly_linked_lists (type_cutout_zone);	
	
	-- This is the base type for assembly documentation objects in general:
	type type_assembly_documentation_base is tagged record
		lines 		: type_doc_lines.list;
		arcs		: type_doc_arcs.list;
		circles		: type_doc_circles.list;
		polygons	: pac_doc_polygons.list;
		cutouts		: pac_doc_cutouts.list;
		texts		: type_texts_with_content.list;
	end record;

	-- Assembly documentation objects of a package include placeholders:
	type type_assembly_documentation is new type_assembly_documentation_base with record
		placeholders: pac_text_placeholders.list;
	end record;

	-- Because assembly documentation is about two sides of the board this composite is required:
	type type_assembly_documentation_both_sides is record
		top		: type_assembly_documentation;
		bottom	: type_assembly_documentation;
	end record;



	
	
-- KEEPOUT
	type type_keepout_line is new type_line with null record;
	
	package type_keepout_lines is new doubly_linked_lists (type_keepout_line);

	type type_keepout_arc is new type_arc with null record;
		
	package type_keepout_arcs is new doubly_linked_lists (type_keepout_arc);
	
	package type_keepout_circles is new doubly_linked_lists (type_fillable_circle_solid);

	package type_keepout_polygons is new doubly_linked_lists (shapes.type_polygon);
	package pac_keepout_cutouts is new doubly_linked_lists (type_cutout_zone);	
	
	type type_keepout is record
		lines 		: type_keepout_lines.list;
		arcs		: type_keepout_arcs.list;
		circles		: type_keepout_circles.list;
		polygons	: type_keepout_polygons.list;
		cutouts 	: pac_keepout_cutouts.list;
		-- CS texts		: type_texts_with_content.list; -- for placement notes ?
	end record;

	type type_keepout_both_sides is record
		top 	: type_keepout;
		bottom	: type_keepout;
	end record;
	


	
-- ROUTE RESTRICT
	
	type type_route_restrict_line is new type_line with record
		layers 	: type_signal_layers.set;
	end record;
	
	package type_route_restrict_lines is new doubly_linked_lists (type_route_restrict_line);

	type type_route_restrict_arc is new type_arc with record
		layers 	: type_signal_layers.set;
	end record;
	
	package type_route_restrict_arcs is new doubly_linked_lists (type_route_restrict_arc);
	
	type type_route_restrict_circle is new type_fillable_circle_solid with record
		layers 	: type_signal_layers.set;
	end record;

	
	package type_route_restrict_circles is new doubly_linked_lists (type_route_restrict_circle);

	type type_route_restrict_polygon is new shapes.type_polygon with record
		layers 	: type_signal_layers.set;
	end record;

	package type_route_restrict_polygons is new doubly_linked_lists (type_route_restrict_polygon);

	type type_route_restrict_cutout is new type_cutout_zone with record
		layers 	: type_signal_layers.set;
	end record;
		
	package pac_route_restrict_cutouts is new doubly_linked_lists (type_route_restrict_cutout);
	
	-- this is the base type for route restrict objects
	type type_route_restrict is tagged record
		lines 		: type_route_restrict_lines.list;
		arcs		: type_route_restrict_arcs.list;
		circles		: type_route_restrict_circles.list;
		polygons	: type_route_restrict_polygons.list;
		cutouts		: pac_route_restrict_cutouts.list;
		-- CS texts		: type_texts_with_content.list; -- for routing notes ? mind signal layer !
	end record;



	

-- VIA RESTRICT
	
	type type_via_restrict_line is new type_line with record
		layers	: type_signal_layers.set;
	end record;
	
	package type_via_restrict_lines is new doubly_linked_lists (type_via_restrict_line);

	
	type type_via_restrict_arc is new type_arc with record
		layers	: type_signal_layers.set;
	end record;
	
	package type_via_restrict_arcs is new doubly_linked_lists (type_via_restrict_arc);

	
	type type_via_restrict_circle is new type_fillable_circle_solid with record
		layers	: type_signal_layers.set;
	end record;
	
	package type_via_restrict_circles is new doubly_linked_lists (type_via_restrict_circle);

	
	type type_via_restrict_polygon is new shapes.type_polygon with record
		layers 	: type_signal_layers.set;
	end record;

	package type_via_restrict_polygons is new doubly_linked_lists (type_via_restrict_polygon);


	type type_via_restrict_cutout is new type_cutout_zone with record
		layers 	: type_signal_layers.set;
	end record;
		
	package pac_via_restrict_cutouts is new doubly_linked_lists (type_via_restrict_cutout);
	
	
	-- this is the base type for via restrict objects
	type type_via_restrict is tagged record
		lines 		: type_via_restrict_lines.list;
		arcs		: type_via_restrict_arcs.list;
		circles		: type_via_restrict_circles.list;
		polygons	: type_via_restrict_polygons.list;
		cutouts		: pac_via_restrict_cutouts.list;
		-- CS texts		: type_texts_with_content.list; -- for via notes ?
	end record;

	
-- PCB CONTOUR
	type type_pcb_contour_line is new type_line with null record;
	package type_pcb_contour_lines is new doubly_linked_lists (type_pcb_contour_line);
	
	type type_pcb_contour_arc is new type_arc with null record;
	package type_pcb_contour_arcs is new doubly_linked_lists (type_pcb_contour_arc);
	
	type type_pcb_contour_circle is new type_circle with null record;
	package type_pcb_contour_circles is new doubly_linked_lists (type_pcb_contour_circle);
	
	type type_pcb_contour is record
		lines 	: type_pcb_contour_lines.list;
		arcs	: type_pcb_contour_arcs.list;
		circles	: type_pcb_contour_circles.list;
	end record;
	
	type type_pcb_contour_plated is record
		lines 	: type_pcb_contour_lines.list;
		arcs	: type_pcb_contour_arcs.list;
		circles	: type_pcb_contour_circles.list;
	end record;
	
	procedure log_plated_millings (
		millings 		: in type_pcb_contour_plated;
		log_threshold	: in et_string_processing.type_log_level);

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

	
	-- A pad outline is a polygon:
	type type_pad_outline is new shapes.type_polygon_base with null record;
	
	type type_pad_outline_tht is record
		top		: type_pad_outline; -- The pad shape on the top side
		bottom	: type_pad_outline; -- is not nessecarily the same as on the bottom side.
	end record;

	keyword_stop_mask		: constant string := "stop_mask";
	keyword_solder_paste	: constant string := "solder_paste";

	keyword_pad_shape			: constant string := "pad_shape";	
	keyword_width_inner_layers	: constant string := "width_inner_layers";
	keyword_assembly_technology	: constant string := "technology";
	keyword_tht_hole			: constant string := "hole";	
	keyword_drill_size			: constant string := "drill_size";
	
	type type_terminal (
		technology	: type_assembly_technology; -- smt/tht
		tht_hole	: type_terminal_tht_hole) -- drilled/milled, without meaning if technology is SMT
		is tagged record

			position : type_position; -- position (x/y) and rotation
			-- For SMT pads this is the geometic center of the pad.
			-- The rotation has no meaning for THT pads with round shape.
			-- The rotation is useful for exotic pad contours. The operator would be drawing the 
			-- contour with zero rotation first (which is easier). Then by applying an angle,
			-- the countour would be rotated to its final position.
			
		case technology is
			when THT =>
				-- The shape of the pad on top and bottom side.
				pad_shape_tht : type_pad_outline_tht; 

				-- This is the width of the copper surrounding the hole in inner layers.
				-- Since the hole can be of any shape we do not speak about restring.
				-- The shape of the copper area around the hole is the same as the shape of the 
				-- hole. No further contours possible.
				width_inner_layers : et_pcb_coordinates.type_distance; -- CS use subtype for reasonable range
				
				case tht_hole is
					when DRILLED =>
						drill_size : type_drill_size;
						
					when MILLED =>
						millings : type_pcb_contour_plated;
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
		terminal		: in type_terminal;
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
		copper					: type_copper_both_sides; -- non-electric objects
		keepout 				: type_keepout_both_sides;
		stop_mask				: type_stop_mask_both_sides;
		stencil					: type_stencil_both_sides;

		route_restrict 			: type_route_restrict;
		via_restrict 			: type_via_restrict;
		-- CS holes

		-- PCB contour or so called "non-plated millings"
		pcb_contour				: type_pcb_contour; 

		-- Plated millings. NOTE: NOT FOR SLITTED HOLES ! See type_terminal instead.
		-- CS: currently no need for such things
		--pcb_contour_plated 		: type_pcb_contour_plated;
		
		technology				: type_assembly_technology; -- set by majority of terminals
		
		-- Only REAL packages have 3d contours:
		case appearance is
			when REAL =>
				null; -- CS
				--package_contour	: type_package_contour;
			when VIRTUAL =>
				null; -- netchangers, testpoints, board edge connectors, ...
		end case;
	end record;

	
	
	-- A package in the library extends the base package type:
	type type_package is new type_package_base with record
		-- CS default for face ?
		silk_screen				: type_silk_screen_both_sides; -- incl. placeholder for reference and purpose
		assembly_documentation	: type_assembly_documentation_both_sides; -- incl. placeholder for value
		terminals				: type_terminals.map;
	end record;

	-- packages
	package type_packages is new indefinite_ordered_maps (
		key_type		=> et_libraries.type_package_model_file.bounded_string, -- ../lbr/smd/SO15.pac
		"<"				=> et_libraries.type_package_model_file."<",
		element_type	=> type_package);

	library_file_extension : constant string := "pac";

	-- HERE RIG WIDE PACKAGES ARE KEPT:
	packages : type_packages.map;

	function locate_package_model (model_name : in et_libraries.type_package_model_file.bounded_string) -- ../lbr/smd/SO15.pac
	-- Returns a cursor to the given package model.
		return type_packages.cursor;
	
	function is_real (package_name : in et_libraries.type_package_model_file.bounded_string) return boolean;
	-- Returns true if the given package is real (means it has a height).

	function terminal_properties (
		cursor		: in type_packages.cursor;
		terminal	: in et_libraries.type_terminal_name.bounded_string)  -- H4, 14
		return type_terminals.cursor;
	-- Returns a cursor to the requested terminal (with all its properties) within the given package model.

	
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
		cursor			: in pac_text_placeholders.cursor;
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
		cursor			: in pac_text_placeholders.cursor;
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

	-- CS procedure circle_route_restrict_properties
	
	
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

	-- CS procedure circle_via_restrict_properties
	

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
	

	
end et_packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
