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
with et_conductor_segment;		use et_conductor_segment;
with et_conductor_text;			use et_conductor_text;
with et_route_restrict;			use et_route_restrict;
with et_via_restrict;			use et_via_restrict;
with et_stop_mask;				use et_stop_mask;
with et_stencil;				use et_stencil;
with et_silkscreen;				use et_silkscreen;
with et_assy_doc;				use et_assy_doc;
with et_keepout;				use et_keepout;

with cairo;

package et_packages is
	use pac_geometry_brd;

	use et_board_shapes_and_text.pac_shapes;
	use et_board_shapes_and_text.pac_text_fab;


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
		-- Since they are of mere supportive nature for routing
		-- we regarded them as conductor layers.
		-- These layers are numbered:
		LAYER_CAT_ROUTE_RESTRICT,
		LAYER_CAT_VIA_RESTRICT);

	subtype type_layer_category_outline is type_layer_category
		range LAYER_CAT_OUTLINE .. LAYER_CAT_OUTLINE;
	
	subtype type_layer_category_non_conductor is type_layer_category
		range LAYER_CAT_SILKSCREEN .. LAYER_CAT_STOP;

	subtype type_layer_category_conductor is type_layer_category
		range LAYER_CAT_CONDUCTOR .. LAYER_CAT_VIA_RESTRICT;

	subtype type_layer_category_restrict is type_layer_category
		range LAYER_CAT_ROUTE_RESTRICT .. LAYER_CAT_VIA_RESTRICT;
	
	
	function to_layer_category (cat : in string) return type_layer_category;
	function to_string (cat : in type_layer_category) return string;

	
	-- Maps from face to mirror status of a vectorized text.
	-- Use it for non-device related texts and placeholders.
	function face_to_mirror (f : in type_face) 
		return et_text.type_vector_text_mirrored;


	
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
	
	

	
	
-- TEXT
	--type type_text is new pac_text_fab.type_text with record
		----position	: type_position; -- x/y/rotation
		--line_width	: pac_text_fab.type_text_line_width := pac_text_fab.type_text_line_width'first;
		---- CS locked : type_locked;		
	--end record;
	type type_text is new pac_text_fab.type_text_fab with null record;

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
	--type type_text_with_content is new type_text with record
		--content : et_text.pac_text_content.bounded_string;
		---- CS vectors
	--end record;
	
	--package pac_texts_with_content is new doubly_linked_lists (type_text_with_content);
	use pac_texts_fab_with_content;

	


	use pac_conductor_lines;
	use pac_conductor_arcs;
	use pac_conductor_circles;

	 
	


	
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
		texts		: pac_conductor_texts_package.list;
	end record;
	
	-- Since NON ELECTRIC conductor objects of a package can be on both sides 
	-- of the board we need this type. There is no reason for NON ELECTRIC 
	-- conductor objects in inner layers. So we deal with top and bottom side only:
	type type_conductor_objects_both_sides is record
		top		: type_conductor_objects;
		bottom	: type_conductor_objects;
	end record;





	
	-- Silk screen objects include placeholders:
	type type_silk_screen is new type_silk_screen_base with record
		placeholders : pac_text_placeholders.list;
	end record;

	-- silk screen is about two sides of the board:
	type type_silk_screen_both_sides is record
		top		: type_silk_screen;
		bottom	: type_silk_screen;
	end record;


	-- Assembly documentation includes placeholders:
	type type_assembly_documentation is new type_assembly_documentation_base with record
		placeholders: pac_text_placeholders.list;
	end record;

	-- Because assembly documentation is about two sides of the board this composite is required:
	type type_assembly_documentation_both_sides is record
		top		: type_assembly_documentation;
		bottom	: type_assembly_documentation;
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


	


	-- Logs the properties of the given silk screen placeholder
	procedure placeholder_silk_screen_properties (
		face			: in type_face;
		cursor			: in pac_text_placeholders.cursor;
		log_threshold 	: in et_string_processing.type_log_level);
	

	
	

	-- Logs the properties of the given assembly documentation placeholder
	procedure placeholder_assy_doc_properties (
		face			: in type_face;
		cursor			: in pac_text_placeholders.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	
	
	
	
	

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

	

	
end et_packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
