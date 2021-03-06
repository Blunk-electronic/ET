------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             PACKAGES                                     --
--                                                                          --
--                              S p e c                                     --
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
with et_string_processing;		use et_string_processing;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_geometry;				use et_geometry;
with et_pcb_stack;				use et_pcb_stack;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_drills;					use et_drills;
with et_terminals;				use et_terminals;
with et_text;
with et_design_rules;			use et_design_rules;

with cairo;

package et_packages is
	use pac_geometry_brd;

	use et_board_shapes_and_text.pac_shapes;



-- LAYER CATEGORY

	-- Prefixes before enumeration types prevent clashes with gnat keywords
	-- and package names:	
	layer_category_prefix : constant string := "LAYER_CAT_";

	type type_layer_category is (
		LAYER_CAT_OUTLINE,
		-- CS LAYER_CAT_OUTLINE_TEMPLATE -- CS
									
		-- NON CONDUCTOR LAYERS.
		-- These layers are paired. Means there is a TOP and a BOTTOM:
		LAYER_CAT_SILKSCREEN,
		LAYER_CAT_ASSY,
		LAYER_CAT_KEEPOUT,
		LAYER_CAT_STENCIL,
		LAYER_CAT_STOP,

		-- CONDUCTOR LAYERS.
		-- These layers are numbered:
		LAYER_CAT_CONDUCTOR,
		
		-- NOTE: Restrict layers do not contain any conducting
		-- objects. They are irrelevant for manufacturing.
		-- Since they are of mere supportive nature the are here
		-- regarded as conducor layers.
		-- These layers are numbered:
		LAYER_CAT_ROUTE_RESTRICT,
		LAYER_CAT_VIA_RESTRICT);

	subtype type_layer_category_outline is type_layer_category
		range LAYER_CAT_OUTLINE .. LAYER_CAT_OUTLINE;
	
	subtype type_layer_category_non_conductor is type_layer_category
		range LAYER_CAT_SILKSCREEN .. LAYER_CAT_STOP;

	subtype type_layer_category_conductor is type_layer_category
		range LAYER_CAT_CONDUCTOR .. LAYER_CAT_VIA_RESTRICT;
	
	function to_layer_category (cat : in string) return type_layer_category;
	function to_string (cat : in type_layer_category) return string;



	
	-- A package (or a footprint) is something like "SOT32" or "NDIP14". 
	-- It is a more or less standardized (JEDEC)
	-- designator for the housing or the case of an electronical component.
	-- The package name is independed of
	-- the actual purpose of a device. An LED can have an SOT23 package and
	-- a transistor can also come in an SOT23.

	-- Package names like "SOT23" or "TO220" are stored in bounded strings:
	package_name_characters : character_set := to_set 
		(ranges => (('a','z'),('A','Z'),('0','9'))) 
		or to_set('.')
		or to_set('-')
		or to_set('_'); 

	package_name_length_max : constant positive := 100;
	package pac_package_name is new generic_bounded_length (package_name_length_max);

	function to_string (packge : in pac_package_name.bounded_string) return string;
	-- Returns the given package name as as string.
	-- CS: provide a parameter that turns the preamble on/off

	function to_package_name (package_name : in string) return pac_package_name.bounded_string;
	-- Converts a string to a pac_package_name.
	
	procedure check_package_name_length (packge : in string);
	-- Tests if the given package name is longer than allowed.
	
	procedure check_package_name_characters (
		packge		: in pac_package_name.bounded_string;
		characters	: in character_set := package_name_characters);
	-- Tests if the given package name contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.



	

	package_model_file_name_length_max : constant positive := 300;
	package pac_package_model_file_name is new generic_bounded_length (package_model_file_name_length_max);

	package_model_file_extension : constant string := "pac";
	
	use pac_package_model_file_name;
	
	function to_string (name : in pac_package_model_file_name.bounded_string) return string;
	function to_file_name (name : in string) return pac_package_model_file_name.bounded_string;
	
	
	subtype type_general_line_width is type_distance_positive
		range text_parameters_fab.width_min .. text_parameters_fab.width_max;
	
	-- Checks whether given line width is in range of type_general_line_width
	procedure validate_general_line_width (width : in type_distance);


	
	
-- TEXT
	type type_text is new pac_text_fab.type_text with record
		position	: type_position; -- x/y/rotation
		line_width	: pac_text_fab.type_text_line_width; -- CS default := line_width_default; 
		-- CS locked : type_locked;		
	end record;

	function text_properties (text : in type_text) return string;
	-- Returns the properties of the given text in a long single string.	
	

	
-- PLACEHOLDERS FOR TEXTS
	type type_text_meaning_package is (NAME, VALUE, PURPOSE);

	function to_string (text_meaning : in type_text_meaning_package) return string;
	function to_text_meaning (text_meaning : in string) return type_text_meaning_package;
	
	type type_text_placeholder is new type_text with record
		meaning : type_text_meaning_package := NAME;
	end record;

	-- There can be lots of placeholders of this kind. So they are stored in a list:	
	package pac_text_placeholders is new doubly_linked_lists (type_text_placeholder);

	-- Placeholders for device name and value can be placed in
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

	type type_text_placeholders is record
		silk_screen	: type_text_placeholders_silk_screen;
		assy_doc	: type_text_placeholders_assembly_documentation;
	end record;


	-- TEXTS WITH CONTENT
	type type_text_with_content is new type_text with record
		content : et_text.type_text_content.bounded_string;
	end record;

	package pac_texts_with_content is new doubly_linked_lists (type_text_with_content);
	


	


	

	-- FILL STYLE OF OBJECTS WITH A CLOSED CIRCUMFENCE		
	keyword_fill_style : constant string := "fill_style";	
	type type_fill_style is (SOLID, HATCHED);
	fill_style_default : constant type_fill_style := SOLID;
	
	function to_string (fill_style : in type_fill_style) return string;
	function to_fill_style (fill_style : in string) return type_fill_style;


	-- HATCHING OF OBJECTS WITH CLOSED CIRCUMFENCE
	keyword_hatching_line_width		: constant string := "hatching_line_width";
	keyword_hatching_border_width	: constant string := "hatching_border_width";	
	keyword_hatching_line_spacing	: constant string := "hatching_line_spacing";		

	hatching_line_width_default : constant type_distance_positive := 0.2;
	hatching_spacing_default	: constant type_distance_positive := 1.0;
	
	
	type type_hatching is record
		-- the width of the border line
		border_width : type_distance_positive := 1.0;
		
		-- the with of the lines inside the area:
		line_width : type_distance_positive := hatching_line_width_default;

		-- the space between the lines inside the area:
		spacing	: type_distance_positive := hatching_spacing_default;
	end record;

	type type_conductor_hatching is record
		-- the width of the border line
		border_width : type_track_width := type_track_width'first;
		
		-- the with of the lines inside the area:
		line_width : type_track_width := type_track_width'first;

		-- the space between the lines inside the area:
		spacing	: type_track_clearance := type_track_clearance'first;
	end record;

	
	-- EASING
	keyword_easing_style : constant string := "easing_style";
	keyword_easing_radius : constant string := "easing_radius";	

	type type_easing_style is (NONE, CHAMFER, FILLET);

	function to_easing_style (easing : in string) return type_easing_style;
	function to_string (easing : in type_easing_style) return string;
	
	easing_radius_max : constant type_distance_positive := 100.0;
	subtype type_easing_radius is type_distance_positive range type_distance_positive'first .. easing_radius_max;

	type type_easing is record
		style	: type_easing_style := NONE;
		radius	: type_easing_radius := zero; -- center of circle at corner point
	end record;

	
	-- POLYGON

	-- Polygons in non-conducor layers such as silkscreen, stencil, ...
	type type_polygon_non_conductor (fill_style : type_fill_style) 
	is new type_polygon_base with record
		easing : type_easing;
		
		case fill_style is
			when SOLID		=> null;
			when HATCHED	=> hatching : type_hatching;
		end case;
	end record;

	-- Polygons in conductor layers have a dedicated type for the hatching:
	type type_polygon_conductor (fill_style : type_fill_style) 
	is new type_polygon_base with record

		-- the minimum width:
		width_min : type_track_width := type_track_width'first;

		-- the space between the polygon and foreign conductor objects:
		isolation : type_track_clearance := type_track_clearance'first; 
	
		easing : type_easing;
		
		case fill_style is
			when SOLID		=> null;
			when HATCHED	=> hatching : type_conductor_hatching;
		end case;
	end record;

	

	type type_conductor_line is new type_line with record
		width	: type_track_width;
	end record;

	type type_conductor_line_segment is private;

	function to_string (segment : in type_conductor_line_segment)
		return string;
	
	function to_line_segment (line : in type_conductor_line)
		return type_conductor_line_segment;

	function get_left_edge (segment : in type_conductor_line_segment)
		return type_line;

	function get_right_edge (segment : in type_conductor_line_segment)
		return type_line;

	function get_start_cap (segment : in type_conductor_line_segment)
		return type_arc;

	function get_end_cap (segment : in type_conductor_line_segment)
		return type_arc;


	-- Computes the shortest distance from a point to
	-- a conductor line segment. If the return is negative,
	-- then the point is inside the segment:
	function get_shortest_distance (
		point	: in type_point;
		segment	: in type_conductor_line_segment)
		return type_distance;
	
	
	package pac_conductor_lines is new doubly_linked_lists (type_conductor_line);
	use pac_conductor_lines;

	
	
	
	type type_conductor_arc is new type_arc with record
		width	: type_track_width;
	end record;

	type type_conductor_arc_segment is private;

	function to_string (segment : in type_conductor_arc_segment)
		return string;
	
	function to_arc_segment (arc : in type_conductor_arc)
		return type_conductor_arc_segment;

	function get_inner_edge (segment : in type_conductor_arc_segment)
		return type_arc;

	function get_outer_edge (segment : in type_conductor_arc_segment)
		return type_arc;

	function get_start_cap (segment : in type_conductor_arc_segment)
		return type_arc;

	function get_end_cap (segment : in type_conductor_arc_segment)
		return type_arc;


	-- Computes the shortest distance from a point to
	-- a conductor arc segment. If the return is negative,
	-- then the point is inside the segment:
	function get_shortest_distance (
		point	: in type_point;
		segment	: in type_conductor_arc_segment)
		return type_distance;

	
	package pac_conductor_arcs is new doubly_linked_lists (type_conductor_arc);
	use pac_conductor_arcs;

	
	type type_conductor_circle (
		filled		: type_filled;
		fill_style	: type_fill_style -- don't care if filled is NO
		)
		is new type_circle with record
		case filled is
			when NO => 
				-- the line width of the circumfence:
				border_width : type_track_width := type_track_width'first;

			when YES =>
				case fill_style is
					when SOLID => null;
					when HATCHED =>
						hatching : type_conductor_hatching;
				end case;
				
		end case;
	end record;

	package pac_conductor_circles is new indefinite_doubly_linked_lists (type_conductor_circle);
	use pac_conductor_circles;

	 
	
	-- the space between foreign pads and the polygon outline
	keyword_isolation : constant string := "isolation";

	-- the minimal width of a polygon
	keyword_min_width : constant string := "min_width";


	
	-- SOLID CONDUCTOR POLYGONS
	type type_polygon_conductor_solid 
	is new type_polygon_conductor (fill_style => SOLID) with null record;

	package pac_conductor_polygons_solid is new doubly_linked_lists (type_polygon_conductor_solid);

	
	-- HATCHED CONDUCTOR POLYGONS
	type type_polygon_conductor_hatched
	is new type_polygon_conductor (fill_style => HATCHED) with null record;

	package pac_conductor_polygons_hatched is new doubly_linked_lists (type_polygon_conductor_hatched);


	
	-- A cutout-polygon used in conductor layers:
	package pac_conductor_cutouts is new doubly_linked_lists (type_polygon);

	

	


	type type_conductor_polygons is record
		solid	: pac_conductor_polygons_solid.list;
		hatched	: pac_conductor_polygons_hatched.list;
	end record;

		
	
	type type_conductor_objects is record 
		lines 		: pac_conductor_lines.list;
		arcs		: pac_conductor_arcs.list;
		circles		: pac_conductor_circles.list;
		polygons	: type_conductor_polygons;
		cutouts		: pac_conductor_cutouts.list;
		texts		: pac_texts_with_content.list;
	end record;
	
	-- Since NON ELECTRIC conductor objects of a package can be on both sides 
	-- of the board we need this type. There is no reason for NON ELECTRIC 
	-- conductor objects in inner layers. So we deal with top and bottom side only:
	type type_conductor_objects_both_sides is record
		top		: type_conductor_objects;
		bottom	: type_conductor_objects;
	end record;





	-- This circle type is used by silk screen, assembly doc, stop mask, stencil
	type type_fillable_circle (
		filled		: type_filled;
		fill_style	: type_fill_style -- don't care if filled is NO
		)
		is new type_circle with record
		case filled is
			when NO => 
				-- the line width of the circumfence:
				border_width : type_general_line_width := type_general_line_width'first;

			when YES =>
				case fill_style is
					when SOLID => null;
					when HATCHED =>
						hatching : type_hatching;
				end case;
				
		end case;
	end record;

	-- CS type_circle_cutout ?
	
	function to_string (circle : in type_fillable_circle) return string;

	-- This circle type is used by keepout, route restrict, via restrict.
	-- The fill style is always solid, hence no discrimintant for fiil style.
	-- When drawing, for the width of the border a fixed value will be applied.
	type type_fillable_circle_solid is new type_circle with record
		filled : type_filled;
	end record;



	
-- SOLDER STOP MASK
	
	type type_stop_line is new type_line with record
		width	: type_general_line_width;
	end record;

	package pac_stop_lines is new doubly_linked_lists (type_stop_line);


	type type_stop_arc is new type_arc with record
		width	: type_general_line_width;
	end record;

	package pac_stop_arcs is new doubly_linked_lists (type_stop_arc);

	package pac_stop_circles is new indefinite_doubly_linked_lists (type_fillable_circle);

	package pac_stop_polygons is new indefinite_doubly_linked_lists (type_polygon_non_conductor);
	package pac_stop_cutouts is new doubly_linked_lists (type_polygon);
	
	-- This is the type for stop mask objects in general.
	-- This has nothing to do with the stop mask of pads.
	type type_stop_mask is tagged record
		lines 		: pac_stop_lines.list;
		arcs		: pac_stop_arcs.list;
		circles		: pac_stop_circles.list;
		polygons	: pac_stop_polygons.list;
		cutouts		: pac_stop_cutouts.list;

		-- for texts in conductor layer to be exposed:
		texts		: pac_texts_with_content.list;
	end record;

	-- Stop mask of packages:
	type type_stop_mask_both_sides is record
		top		: type_stop_mask;
		bottom	: type_stop_mask;
	end record;


	





-- STENCIL / SOLDER PASTE MASK
	
	type type_stencil_line is new type_line with record
		width	: type_general_line_width;
	end record;

	package pac_stencil_lines is new doubly_linked_lists (type_stencil_line);


	type type_stencil_arc is new type_arc with record
		width	: type_general_line_width;
	end record;

	package pac_stencil_arcs is new doubly_linked_lists (type_stencil_arc);

	package pac_stencil_circles is new indefinite_doubly_linked_lists (type_fillable_circle);

	package pac_stencil_polygons is new indefinite_doubly_linked_lists (type_polygon_non_conductor);
	package pac_stencil_cutouts is new doubly_linked_lists (type_polygon);	
	
	-- This is the type for solder paste stencil objects in general:
	type type_stencil is record
		lines 		: pac_stencil_lines.list;
		arcs		: pac_stencil_arcs.list;
		circles		: pac_stencil_circles.list;
		polygons	: pac_stencil_polygons.list;
		cutouts		: pac_stencil_cutouts.list;
		
		texts		: pac_texts_with_content.list;
		-- NOTE: Probably not reasonable and a waste of resources.
		-- DRC should output warning if texts in stencil detected.
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

	package pac_silk_lines is new doubly_linked_lists (type_silk_line);  -- CS rename to pac_silk_lines


	type type_silk_arc is new type_arc with record
		width	: type_general_line_width;
	end record;

	package pac_silk_arcs is new doubly_linked_lists (type_silk_arc); -- CS rename to pac_silk_arcs
	
	package pac_silk_circles is new indefinite_doubly_linked_lists (type_fillable_circle); -- CS rename to pac_silk_circles

	package pac_silk_polygons is new indefinite_doubly_linked_lists (type_polygon_non_conductor);
	package pac_silk_cutouts is new doubly_linked_lists (type_polygon);	
	

	-- This is the base type for silk screen objects in general:
	type type_silk_screen_base is tagged record
		lines 		: pac_silk_lines.list;
		arcs		: pac_silk_arcs.list;
		circles		: pac_silk_circles.list;
		polygons	: pac_silk_polygons.list;
		cutouts 	: pac_silk_cutouts.list;
		texts		: pac_texts_with_content.list;
	end record;

	-- Silk screen objects of a package (in the library) include placeholders:
	type type_silk_screen is new type_silk_screen_base with record
		placeholders : pac_text_placeholders.list;
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

	package pac_doc_lines is new doubly_linked_lists (type_doc_line);


	type type_doc_arc is new type_arc with record
		width	: type_general_line_width;
	end record;

	package pac_doc_arcs is new doubly_linked_lists (type_doc_arc);
	
	package pac_doc_circles is new indefinite_doubly_linked_lists (type_fillable_circle);
	
	package pac_doc_polygons is new indefinite_doubly_linked_lists (type_polygon_non_conductor);
	package pac_doc_cutouts is new doubly_linked_lists (type_polygon);	
	
	-- This is the base type for assembly documentation objects in general:
	type type_assembly_documentation_base is tagged record
		lines 		: pac_doc_lines.list;
		arcs		: pac_doc_arcs.list;
		circles		: pac_doc_circles.list;
		polygons	: pac_doc_polygons.list;
		cutouts		: pac_doc_cutouts.list;
		texts		: pac_texts_with_content.list;
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

	-- GUI relevant only: The line width of keepout:
	keepout_line_width : constant type_general_line_width := text_parameters_fab.width_min;

	type type_keepout_line is new type_line with null record;
	package pac_keepout_lines is new doubly_linked_lists (type_keepout_line);

	type type_keepout_arc is new type_arc with null record;
	package pac_keepout_arcs is new doubly_linked_lists (type_keepout_arc);
	
	package pac_keepout_circles is new doubly_linked_lists (type_fillable_circle_solid);

	type type_keepout_polygon is new type_polygon_base with null record;
	package pac_keepout_polygons is new doubly_linked_lists (type_keepout_polygon);
	
	package pac_keepout_cutouts is new doubly_linked_lists (type_polygon);	
	
	type type_keepout is record
		lines 		: pac_keepout_lines.list;
		arcs		: pac_keepout_arcs.list;
		circles		: pac_keepout_circles.list;
		polygons	: pac_keepout_polygons.list;
		cutouts 	: pac_keepout_cutouts.list;
		texts		: pac_texts_with_content.list; -- for notes on placement
	end record;

	type type_keepout_both_sides is record
		top 	: type_keepout;
		bottom	: type_keepout;
	end record;
	


	type type_conductor_text is new type_text_with_content with record
		layer	: type_signal_layer;
	end record;
	
	package pac_conductor_texts is new doubly_linked_lists (type_conductor_text);



	
	
-- ROUTE RESTRICT

	-- GUI relevant only: The line width of route restrict:
	route_restrict_line_width : constant type_general_line_width := text_parameters_fab.width_min;
	
	type type_route_restrict_line is new type_line with record
		layers 	: type_signal_layers.set;
	end record;
	
	package pac_route_restrict_lines is new doubly_linked_lists (type_route_restrict_line);

	type type_route_restrict_arc is new type_arc with record
		layers 	: type_signal_layers.set;
	end record;
	
	package pac_route_restrict_arcs is new doubly_linked_lists (type_route_restrict_arc);
	
	type type_route_restrict_circle is new type_fillable_circle_solid with record
		layers 	: type_signal_layers.set;
	end record;

	
	package pac_route_restrict_circles is new doubly_linked_lists (type_route_restrict_circle);

	type type_route_restrict_polygon is new type_polygon_base with record
		layers 	: type_signal_layers.set;
	end record;

	package pac_route_restrict_polygons is new doubly_linked_lists (type_route_restrict_polygon);

	
	type type_route_restrict_cutout is new type_polygon with record
		layers 	: type_signal_layers.set;
	end record;
		
	package pac_route_restrict_cutouts is new doubly_linked_lists (type_route_restrict_cutout);
	
	-- this is the base type for route restrict objects
	type type_route_restrict is tagged record
		lines 		: pac_route_restrict_lines.list;
		arcs		: pac_route_restrict_arcs.list;
		circles		: pac_route_restrict_circles.list;
		polygons	: pac_route_restrict_polygons.list;
		cutouts		: pac_route_restrict_cutouts.list;
		texts		: pac_conductor_texts.list; -- for notes on routing
	end record;



	
	

-- VIA RESTRICT

	-- GUI relevant only: The line width of via restrict:
	via_restrict_line_width : constant type_general_line_width := text_parameters_fab.width_min;
	
	type type_via_restrict_line is new type_line with record
		layers	: type_signal_layers.set;
	end record;
	
	package pac_via_restrict_lines is new doubly_linked_lists (type_via_restrict_line);

	
	type type_via_restrict_arc is new type_arc with record
		layers	: type_signal_layers.set;
	end record;
	
	package pac_via_restrict_arcs is new doubly_linked_lists (type_via_restrict_arc);

	
	type type_via_restrict_circle is new type_fillable_circle_solid with record
		layers	: type_signal_layers.set;
	end record;
	
	package pac_via_restrict_circles is new doubly_linked_lists (type_via_restrict_circle);

	
	type type_via_restrict_polygon is new type_polygon_base with record
		layers 	: type_signal_layers.set;
	end record;
	
	package pac_via_restrict_polygons is new doubly_linked_lists (type_via_restrict_polygon);


	type type_via_restrict_cutout is new type_polygon with record
		layers 	: type_signal_layers.set;
	end record;
		
	package pac_via_restrict_cutouts is new doubly_linked_lists (type_via_restrict_cutout);
	
	
	-- this is the base type for via restrict objects
	type type_via_restrict is tagged record
		lines 		: pac_via_restrict_lines.list;
		arcs		: pac_via_restrict_arcs.list;
		circles		: pac_via_restrict_circles.list;
		polygons	: pac_via_restrict_polygons.list;
		cutouts		: pac_via_restrict_cutouts.list;
		texts		: pac_conductor_texts.list; -- for notes on via restrict
	end record;



	
-- PCB CUTOUTS OR HOLES

	-- As this is the model of a package, we have only
	-- pcb contours that form holes inside the board area.
	-- There may be multiple cutout areas. Each of them
	-- has a closed circumfence.
	-- So we use the simple polygon type and collect them
	-- in a simple list:
	package pac_pcb_cutouts is new doubly_linked_lists (type_polygon);

	-- GUI relevant only: The line width of contours:
	pcb_contour_line_width : constant type_general_line_width := text_parameters_fab.width_min;

	-- NOTE: There is no reason to allow texts in contours here.
	-- The text would most likely end up somewhere inside the board area. 
	-- This in turn would cause the DRC to output errors.
	

	
	
-- APPEARANCE

	type type_package_appearance is (
		REAL,	-- packages with x,y,z dimension
		VIRTUAL -- for things that do not have a real package 
				-- (like testpoints, edge connectors, mounting holes, fiducials, ...)
		);	

	package_appearance_default : constant type_package_appearance := REAL;
	function to_string (appearance : in type_package_appearance) return string;
	function to_appearance (appearance : in string) return type_package_appearance;
	

-- DESCRIPTION
	
	package_description_length_max : constant positive := 200;
	package pac_package_description is new generic_bounded_length (package_description_length_max);

	function to_string (
		description : in pac_package_description.bounded_string;
		verbose		: in boolean := false) return string;

	function to_package_description (description : in string) 
		return pac_package_description.bounded_string;


	

-- PACKAGE MODEL
	
	-- This is the base type of a package:
	type type_package_base (appearance : type_package_appearance) is abstract tagged record
		description		: pac_package_description.bounded_string;
		conductors		: type_conductor_objects_both_sides; -- non-electric objects
		keepout 		: type_keepout_both_sides;
		stop_mask		: type_stop_mask_both_sides; -- not terminal related
		stencil			: type_stencil_both_sides; -- not terminal related

		route_restrict 	: type_route_restrict;
		via_restrict 	: type_via_restrict;
		
		-- These structures are cutout areas inside the board area:
		holes			: pac_pcb_cutouts.list;
		
		technology			: type_assembly_technology := SMT; -- set by majority of terminals
		
		-- Only REAL packages have 3d contours:
		case appearance is
			when REAL =>
				null; -- CS
				--package_contour	: type_package_contour;
			when VIRTUAL =>
				null; -- fiducials, testpoints, board edge connectors, ...
		end case;
	end record;


	origin_half_size : constant type_distance_positive := 1.0;
	origin_line_width : constant type_distance_positive := 0.01;

	
	-- A package in the library extends the base package type:
	type type_package_lib is new type_package_base with record
		-- CS default for face ?
		silk_screen				: type_silk_screen_both_sides; -- incl. placeholder for name and purpose
		assembly_documentation	: type_assembly_documentation_both_sides; -- incl. placeholder for value
		terminals				: type_terminals.map;
	end record;

	-- CS: this should be a hashed map:
	package pac_packages_lib is new indefinite_ordered_maps (
		key_type		=> pac_package_model_file_name.bounded_string, -- ../lbr/smd/SO15.pac
		element_type	=> type_package_lib);
	
	-- HERE RIG WIDE PACKAGES ARE KEPT:
	packages : pac_packages_lib.map;



	
	function locate_package_model (model_name : in pac_package_model_file_name.bounded_string) -- ../lbr/smd/SO15.pac
	-- Returns a cursor to the given package model.
		return pac_packages_lib.cursor;
	
	function is_real (package_name : in pac_package_model_file_name.bounded_string) return boolean;
	-- Returns true if the given package is real (means it has a height).

	function terminal_properties (
		cursor		: in pac_packages_lib.cursor;
		terminal	: in pac_terminal_name.bounded_string)  -- H4, 14
		return type_terminals.cursor;
	-- Returns a cursor to the requested terminal (with all its properties) within the given package model.

	
	
-- PROPERTIES OF OBJECTS IN CONDUCTOR LAYERS (NON ELECTRIC OBJECTS !!)
	
	procedure line_conductor_properties (
	-- Logs the properties of the given line:
		face			: in type_face;
		cursor			: in pac_conductor_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure arc_conductor_properties (
	-- Logs the properties of the given arc:
		face			: in type_face;
		cursor			: in pac_conductor_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure circle_conductor_properties (
	-- Logs the properties of the given circle:
		face			: in type_face;
		cursor			: in pac_conductor_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level);


	
-- PROPERTIES OF OBJECTS IN SILK SCREEN
	
	procedure line_silk_screen_properties (
	-- Logs the properties of the given line:
		face			: in type_face;
		cursor			: in pac_silk_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure arc_silk_screen_properties (
	-- Logs the properties of the given arc of silk screen
		face			: in type_face;
		cursor			: in pac_silk_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure circle_silk_screen_properties (
	-- Logs the properties of the given circle of silk screen
		face			: in type_face;
		cursor			: in pac_silk_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure placeholder_silk_screen_properties (
	-- Logs the properties of the given silk screen placeholder
		face			: in type_face;
		cursor			: in pac_text_placeholders.cursor;
		log_threshold 	: in et_string_processing.type_log_level);
	
	procedure text_silk_screen_properties (
	-- Logs the properties of the given silk screen text
		face			: in type_face;
		cursor			: in pac_texts_with_content.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	
	
-- PROPERTIES OF OBJECTS IN ASSEMBLY DOCUMENTATION	
	procedure line_assy_doc_properties (
	-- Logs the properties of the given line of assembly documentation
		face			: in type_face;
		cursor			: in pac_doc_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure arc_assy_doc_properties (
	-- Logs the properties of the given arc of assembly documentation
		face			: in type_face;
		cursor			: in pac_doc_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure circle_assy_doc_properties (
	-- Logs the properties of the given circle of assembly documentation
		face			: in type_face;
		cursor			: in pac_doc_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure placeholder_assy_doc_properties (
	-- Logs the properties of the given assembly documentation placeholder
		face			: in type_face;
		cursor			: in pac_text_placeholders.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure text_assy_doc_properties (
	-- Logs the properties of the given assembly documentation text
		face			: in type_face;
		cursor			: in pac_texts_with_content.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	

	
-- PROPERTIES OF OBJECTS IN KEEPOUT	
	procedure line_keepout_properties (
	-- Logs the properties of the given line of keepout
		face			: in type_face;
		cursor			: in pac_keepout_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure arc_keepout_properties (
	-- Logs the properties of the given arc of keepout
		face			: in type_face;
		cursor			: in pac_keepout_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure circle_keepout_properties (
	-- Logs the properties of the given circle of keepout
		face			: in type_face;
		cursor			: in pac_keepout_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level);



-- PROPERTIES OF OBJECTS IN STOP MASK
	procedure arc_stop_mask_properties (
	-- Logs the properties of the given arc of stop mask
		face			: in type_face;
		cursor			: in pac_stop_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure circle_stop_mask_properties (
	-- Logs the properties of the given circle of stop mask
		face			: in type_face;
		cursor			: in pac_stop_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure line_stop_mask_properties (
	-- Logs the properties of the given line of stop mask
		face			: in type_face;
		cursor			: in pac_stop_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure text_stop_mask_properties (
	-- Logs the properties of the given stop mask text
		face			: in type_face;
		cursor			: in pac_texts_with_content.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	

-- PROPERTIES OF OBJECTS IN SOLDER PASTE / STENCIL
	procedure arc_stencil_properties (
	-- Logs the properties of the given arc of stencil
		face			: in type_face;
		cursor			: in pac_stencil_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure circle_stencil_properties (
	-- Logs the properties of the given circle of stencil
		face			: in type_face;
		cursor			: in pac_stencil_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure line_stencil_properties (
	-- Logs the properties of the given line of stencil
		face			: in type_face;
		cursor			: in pac_stencil_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level);
	

	
	
-- PROPERTIES OF OBJECTS IN ROUTE RESTRICT	
	procedure line_route_restrict_properties (
	-- Logs the properties of the given line of route restrict
		face			: in type_face;
		cursor			: in pac_route_restrict_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure arc_route_restrict_properties (
	-- Logs the properties of the given arc of route restrict
		face			: in type_face;
		cursor			: in pac_route_restrict_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	-- CS procedure circle_route_restrict_properties
	
	
-- PROPERTIES OF OBJECTS IN VIA RESTRICT		
	procedure line_via_restrict_properties (
	-- Logs the properties of the given line of via restrict
		face			: in type_face;
		cursor			: in pac_via_restrict_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure arc_via_restrict_properties (
	-- Logs the properties of the given arc of via restrict
		face			: in type_face;
		cursor			: in pac_via_restrict_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	-- CS procedure circle_via_restrict_properties
	

-- PROPERTIES OF OBJECTS IN BOARD CONTOUR / OUTLINE / EDGE CUTS
	--procedure line_pcb_contour_properties (
	---- Logs the properties of the given line of pcb contour
		--cursor			: in pac_pcb_contour_lines.cursor;
		--log_threshold 	: in et_string_processing.type_log_level);

	--procedure arc_pcb_contour_properties (
	---- Logs the properties of the given arc of pcb contour
		--cursor			: in pac_pcb_contour_arcs.cursor;
		--log_threshold 	: in et_string_processing.type_log_level);

	--procedure circle_pcb_contour_properties (
	---- Logs the properties of the given circle of pcb contour
		--cursor			: in pac_pcb_contour_circles.cursor;
		--log_threshold 	: in et_string_processing.type_log_level);
	
private

	type type_conductor_line_segment is record
		left_edge, right_edge : type_line;
		cap_start, cap_end : type_arc;
	end record;

	type type_conductor_arc_segment is record
		inner_edge, outer_edge : type_arc;
		cap_start, cap_end : type_arc;
	end record;

	
end et_packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
