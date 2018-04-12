------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET PCB                                  --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction side" where things are not
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

with et_string_processing;
with et_libraries;				--use et_libraries;
with et_pcb_coordinates;		use et_pcb_coordinates;

package et_pcb is

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

	package type_signal_layers is new ordered_sets (type_signal_layer);

	text_size_min : constant type_distance := 1.0;
	text_size_max : constant type_distance := 100.0;
	subtype type_text_size is type_distance range text_size_min .. text_size_max;

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


	

	drill_size_min : constant type_distance := 0.05;
	drill_size_max : constant type_distance := 10.0;
	subtype type_drill_size is type_distance range drill_size_min .. drill_size_max;

	procedure validate_drill_size (drill : in type_distance);
	-- Checks whether given drill size is in range of type_drill_size


	pad_size_min : constant type_distance := 0.05;
	pad_size_max : constant type_distance := 10.0;
	subtype type_pad_size is type_distance range pad_size_min .. pad_size_max;

	procedure validate_pad_size (size : in type_distance);
	-- Checks whether given pad size is in range of type_pad_size


	pad_drill_offset_min : constant type_distance := zero_distance;
	pad_drill_offset_max : constant type_distance := pad_size_max * 0.5;
	subtype type_pad_drill_offset is type_distance range pad_drill_offset_min .. pad_drill_offset_max;
	
	
	
	-- COPPER STRUCTURES GENERAL
	copper_structure_size_min : constant type_distance := 0.05;
	copper_clearance_min : constant type_distance := copper_structure_size_min;


	
	-- SIGNALS
	subtype type_signal_clearance is type_distance range copper_clearance_min .. type_distance'last;

	procedure validate_signal_clearance (signal_clearance : in type_distance);
	-- Checks whether the given signal clearance is in range of type_signal_clearance.

	signal_width_max : constant type_distance := 100.0;
	subtype type_signal_width is type_distance range copper_clearance_min .. signal_width_max;

	procedure validate_signal_width (signal_width : in type_distance);
	-- Checks whether the given signal width is in range of type_signal_width.


	-- RESTRING
	restring_width_max : constant type_distance := 5.0;
	subtype type_restring_width is type_distance range copper_structure_size_min .. restring_width_max;

	procedure validate_restring_width (restring_width : in type_distance);
	-- Checks whether the given restring width is in range of type_restring_width.


	

	-- NET CLASSES
	net_class_name_length_max : constant positive := 50;
	package type_net_class_name is new generic_bounded_length (net_class_name_length_max);

	function to_string (net_class_name : in type_net_class_name.bounded_string) return string;
	
	net_class_description_length_max : constant positive := 100;
	package type_net_class_description is new generic_bounded_length (net_class_description_length_max);

	type type_net_class is tagged record
		description				: type_net_class_description.bounded_string;
		clearance				: type_signal_clearance;
		signal_width_min		: type_signal_width;
		via_drill_min			: type_drill_size;
		via_restring_min		: type_restring_width;
		micro_via_drill_min		: type_drill_size;
		micro_via_restring_min	: type_restring_width;
	end record;

	package type_net_classes is new ordered_maps (
		key_type		=> type_net_class_name.bounded_string,
		element_type	=> type_net_class,
		"<"				=> type_net_class_name."<"
		);
	

	
	-- TEXT IN GENERAL
	type type_text is abstract tagged record
		position	: type_point_3d;
		size_x		: type_text_size;
		size_y		: type_text_size;
		width		: type_text_line_width;
		angle		: type_angle;
		alignment	: et_libraries.type_text_alignment;
		hidden		: boolean; -- CS use type with yes or no
	end record;

	function text_properties (text : in type_text) return string;
	-- Returns the properties of the given text in a long single string.	
	
	
	-- PLACEHOLDERS FOR TEXTS IN A PACKAGE
	type type_package_text_meaning is (REFERENCE, VALUE); -- CS: purpose ?

	function to_string (text_meaning : in type_package_text_meaning) return string;
	
	type type_package_text_placeholder is new type_text with record
		meaning : type_package_text_meaning;
	end record;

	package type_package_text_placeholders is new doubly_linked_lists (
		element_type => type_package_text_placeholder);





	-- TEXTS WITH CONTENT
	type type_text_with_content is new type_text with record
		content : et_libraries.type_text_content.bounded_string;
	end record;

	package type_texts_with_content is new doubly_linked_lists (
		element_type => type_text_with_content);
	




	-- PLACEHOLDERS FOR PCB
	-- CS type type_pcb_text_placeholder (meaning : type_pcb_text_meaning) is new type_text with null record;
	-- should be a defined via configuration file:
	-- project, drawing number, drawn, checked, approved, ...

	

	

	-- LINE
	type type_line is abstract tagged record
		start_point 	: type_point_3d;
		end_point   	: type_point_3d;
	end record;

	-- ARC
	type type_arc is abstract tagged record
		center			: type_point_3d;
		start_point		: type_point_3d;
		end_point		: type_point_3d;
-- 		angle			: type_angle;
	end record;

	-- CIRCLE
	type type_circle is abstract tagged record
		center			: type_point_3d;
		radius  		: type_distance;
		-- CS filled : boolean;
	end record;

	type type_locked is (NO, YES);


	
	-- PCB CONTOUR/OUTLINE
	type type_pcb_contour_line is new type_line with record
		locked : type_locked := type_locked'first;
	end record;

	procedure line_pcb_contour_properties (
		pcb_contour_line 	: in type_pcb_contour_line);

	package type_pcb_contour_lines is new doubly_linked_lists (type_pcb_contour_line);

	
	type type_pcb_contour_arc is new type_arc with record
		locked : type_locked := type_locked'first;
	end record;
	package type_pcb_contour_arcs is new doubly_linked_lists (type_pcb_contour_arc);

	
	type type_pcb_contour_circle is new type_circle with record
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
		millings 		: in type_package_pcb_contour_plated);
		
	-- PACKAGE CONTOUR/OUTLINE -- for 3d only
	type type_package_contour_line is new type_line with null record;
	package type_package_contour_lines is new doubly_linked_lists (type_package_contour_line);

	type type_package_contour_arc is new type_arc with null record;
	package type_package_contour_arcs is new doubly_linked_lists (type_package_contour_arc);

	type type_package_contour_circle is new type_circle with null record;
	package type_package_contour_circles is new doubly_linked_lists (type_package_contour_circle);
	
	type type_package_contour is record
		lines 	: type_package_contour_lines.list;
		arcs	: type_package_contour_arcs.list;
		circles	: type_package_contour_circles.list;
	end record;

	function no_contour return type_package_contour;
	-- Returns an empty package contour.


	-- COPPER STRUCTURES (NON ELECTRIC !!)
	type type_copper_line is new type_line with record
		width	: type_general_line_width;
		--layers : type_signal_layer;
	end record;

	package type_copper_lines is new doubly_linked_lists (type_copper_line);


	type type_copper_arc is new type_arc with record
		width	: type_general_line_width;
	end record;

	package type_copper_arcs is new doubly_linked_lists (type_copper_arc);


	type type_copper_circle is new type_circle with record
		width	: type_general_line_width;
	end record;

	package type_copper_circles is new doubly_linked_lists (type_copper_circle);

	-- base type for NON ELECTRIC !! copper objects
	type type_copper is tagged record 
		lines 		: type_copper_lines.list;
		arcs		: type_copper_arcs.list;
		circles		: type_copper_circles.list;
		texts		: type_texts_with_content.list;
		-- CS polygons
	end record;

	-- since NON ELECTRIC copper objects of a package can be on both sides 
	-- of the board we need this type:
	type type_copper_package_both_sides is record
		top		: type_copper;
		bottom	: type_copper;
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

	
	type type_silk_circle is new type_circle with record
		width	: type_general_line_width;
	end record;

	package type_silk_circles is new doubly_linked_lists (type_silk_circle);


	-- This is the base type for silk screen objects in general:
	type type_silk_screen is tagged record
		lines 		: type_silk_lines.list;
		arcs		: type_silk_arcs.list;
		circles		: type_silk_circles.list;
		texts		: type_texts_with_content.list;
	end record;

	-- Silk screen objects of a package include placeholders:
	type type_silk_screen_package is new type_silk_screen with record
		placeholders: type_package_text_placeholders.list;
	end record;

	-- Because silk screen is about two sides of the board this composite is required:
	type type_silk_screen_package_both_sides is record
		top		: type_silk_screen_package;
		bottom	: type_silk_screen_package;
	end record;

	-- For silk screen objects that do not belong to any packages use this type:
	type type_silk_screen_pcb is new type_silk_screen with null record;
		-- CS placeholder for revision, board name, misc ... defined via configuration file

	-- Because silk screen is about two sides of the board this composite is required:	
	type type_silk_screen_pcb_both_sides is record
		top 	: type_silk_screen_pcb;
		bottom	: type_silk_screen_pcb;
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

	
	type type_doc_circle is new type_circle with record
		width	: type_general_line_width;
	end record;

	package type_doc_circles is new doubly_linked_lists (type_doc_circle);

	-- This is the base type for assembly documentation objects in general:
	type type_assembly_documentation is tagged record
		lines 		: type_doc_lines.list;
		arcs		: type_doc_arcs.list;
		circles		: type_doc_circles.list;
		texts		: type_texts_with_content.list;
	end record;

	-- Aassembly documentation objects of a package include placeholders:
	type type_assembly_documentation_package is new type_assembly_documentation with record
		placeholders: type_package_text_placeholders.list;
	end record;

	-- Because assembly documentation is about two sides of the board this composite is required:
	type type_assembly_documentation_package_both_sides is record
		top		: type_assembly_documentation_package;
		bottom	: type_assembly_documentation_package;
	end record;

	-- For assembly documentation objects that do not belong to any packages use this type:
	type type_assembly_documentation_pcb is new type_assembly_documentation with null record;
		-- CS placeholder for revision, board name, misc ... defined via configuration file

	-- Because assembly documentation is about two sides of the board this composite is required:	
	type type_assembly_documentation_pcb_both_sides is record
		top 	: type_assembly_documentation_pcb;
		bottom	: type_assembly_documentation_pcb;
	end record;


	
	
	-- KEEPOUT
	type type_keepout_line is new type_line with null record;
	package type_keepout_lines is new doubly_linked_lists (type_keepout_line);

	type type_keepout_arc is new type_arc with null record;
	package type_keepout_arcs is new doubly_linked_lists (type_keepout_arc);
	
	type type_keepout_circle is new type_circle with null record;
	package type_keepout_circles is new doubly_linked_lists (type_keepout_circle);
	
	type type_keepout is record
		lines 	: type_keepout_lines.list;
		arcs	: type_keepout_arcs.list;
		circles	: type_keepout_circles.list;
		-- CS texts		: type_texts_with_content.list; -- for placement note ?
		-- CS polygons
	end record;

	type type_keepout_package_both_sides is record
		top 	: type_keepout;
		bottom	: type_keepout;
	end record;
	
	type type_keepout_pcb_both_sides is record
		top 	: type_keepout;
		bottom	: type_keepout;
	end record;


	
	-- ROUTE RESTRICT
	type type_route_restrict_line is new type_line with record
		layers : type_signal_layers.set;
	end record;
	
	package type_route_restrict_lines is new doubly_linked_lists (type_route_restrict_line);

	type type_route_restrict_arc is new type_arc with record
		layers : type_signal_layers.set;
	end record;
	
	package type_route_restrict_arcs is new doubly_linked_lists (type_route_restrict_arc);
	
	type type_route_restrict_circle is new type_circle with record
		layers : type_signal_layers.set;
	end record;
	package type_route_restrict_circles is new doubly_linked_lists (type_route_restrict_circle);

	-- this is the base type for route restrict objects
	type type_route_restrict is tagged record
		lines 	: type_route_restrict_lines.list;
		arcs	: type_route_restrict_arcs.list;
		circles	: type_route_restrict_lines.list;
		-- CS polygons
		-- CS texts		: type_texts_with_content.list; -- for routing notes ?		
	end record;
	
	type type_route_restrict_package is new type_route_restrict with null record;
	type type_route_restrict_pcb is new type_route_restrict with null record;



	

	-- VIA RESTRICT
	type type_via_restrict_line is new type_line with record
		layers : type_signal_layers.set;
	end record;
	
	package type_via_restrict_lines is new doubly_linked_lists (type_via_restrict_line);

	type type_via_restrict_arc is new type_arc with record
		layers : type_signal_layers.set;
	end record;
	
	package type_via_restrict_arcs is new doubly_linked_lists (type_via_restrict_arc);
	
	type type_via_restrict_circle is new type_circle with record
		layers : type_signal_layers.set;
	end record;
	
	package type_via_restrict_circles is new doubly_linked_lists (type_via_restrict_circle);

	-- this is the base type for via restrict objects
	type type_via_restrict is tagged record
		lines 	: type_via_restrict_lines.list;
		arcs	: type_via_restrict_arcs.list;
		circles	: type_via_restrict_lines.list;
		-- CS texts		: type_texts_with_content.list; -- for via notes ?
		-- CS polygons
	end record;
	
	type type_via_restrict_package is new type_via_restrict with null record;
	type type_via_restrict_pcb is new type_via_restrict with null record;
	



	type type_package_appearance is (
		REAL,	-- packages with x,y,z dimension
		VIRTUAL -- for things that do not have a package (netchangers, testpoints, ISA-Board edge connectors, ...)
		);	

	function to_string (appearance : in type_package_appearance) return string;
	
	type type_assembly_technology is (
		THT,	-- Through Hole Technology
		SMT		-- Surface Mount Technology
		);

	function to_string (technology : in type_assembly_technology) return string;

	type type_terminal_shape is (CIRCULAR, NON_CIRCULAR);
	
	type type_terminal_shape_tht is (OCTAGON, CIRCULAR, RECTANGLE, LONG, LONG_OFFSET);
	function to_string (shape : in type_terminal_shape_tht) return string;
	
	type type_terminal_shape_smt is (RECTANGLE, CIRCULAR, LONG);
	function to_string (shape : in type_terminal_shape_smt) return string;	

	type type_terminal_solder_paste is (NONE, APPLIED);
	function to_string (solder_paste : in type_terminal_solder_paste) return string;
	
	type type_terminal_stop_mask is (CLOSED, OPEN);
	function to_string (stop_mask : in type_terminal_stop_mask) return string;

	type type_terminal_tht_hole is (DRILLED, MILLED);

	-- This is the base type specification of a terminal:
	type type_terminal_base (
		technology	: type_assembly_technology;
		shape		: type_terminal_shape;
		tht_hole	: type_terminal_tht_hole) -- without meaning if technology is SMT
	is tagged record
		position	: type_terminal_position;

		case technology is
			when THT =>
				width_inner_layers : type_distance; -- CS use subtype for reasonable range
				
				shape_tht 	: type_terminal_shape_tht; -- OCTAGON, CIRCULAR, RECTANGLE, LONG, LONG_OFFSET

				case shape is
					when CIRCULAR =>
						drill_size_cir : type_drill_size;
						offset_x : type_pad_drill_offset; -- CS use a composite type for x/y
						offset_y : type_pad_drill_offset;
						
					WHEN NON_CIRCULAR =>
						size_tht_x, size_tht_y : type_pad_size; -- CS use a composite type for x/y

						case tht_hole is
							when DRILLED =>
								drill_size_dri : type_drill_size;

							when MILLED =>
								millings : type_package_pcb_contour_plated;
						end case;
						
				end case;

				
			when SMT =>
				shape_smt		: type_terminal_shape_smt;
				face			: type_face;
				stop_mask 		: type_terminal_stop_mask;
				solder_paste	: type_terminal_solder_paste;
				case shape is
					when CIRCULAR =>
						null;
						
					WHEN NON_CIRCULAR =>
						size_smt_x, size_smt_y : type_pad_size; -- CS use a composite type for x/y
				end case;

		end case;
	end record;

	package type_terminals is new indefinite_ordered_maps (
		key_type		=> et_libraries.type_terminal_name.bounded_string,
		element_type	=> type_terminal_base,
		"<"				=> et_libraries.type_terminal_name."<");


	package_description_length_max : constant positive := 200;
	package type_package_description is new generic_bounded_length (package_description_length_max);

	function to_string (description : in type_package_description.bounded_string) return string;

	
	package_tags_length_max : constant positive := 200;
	package type_package_tags is new generic_bounded_length (package_tags_length_max);

	function to_string (tags : in type_package_tags.bounded_string) return string;

	
	
	type type_package (appearance : type_package_appearance) is record
		description				: type_package_description.bounded_string;
		copper					: type_copper_package_both_sides;
		silk_screen				: type_silk_screen_package_both_sides; -- incl. placeholder for reference and purpose
		assembly_documentation	: type_assembly_documentation_package_both_sides; -- incl. placeholder for value
		keepout 				: type_keepout_package_both_sides;
		route_restrict 			: type_route_restrict_package;
		via_restrict 			: type_via_restrict_package;
		-- holes
		pcb_contours			: type_package_pcb_contour;
		pcb_contours_plated 	: type_package_pcb_contour_plated;
		terminals				: type_terminals.map;
		timestamp				: et_string_processing.type_timestamp;
		technology				: type_assembly_technology; -- set by majority of terminals
		
		-- Only REAL packages have 3d contours:
		case appearance is
			when REAL =>
				package_contours : type_package_contour;
			when VIRTUAL =>
				null; -- netchangers, testpoints, ISA-Board edge connectors, ...
		end case;
	end record;



	
	package type_packages is new indefinite_ordered_maps (
		key_type 		=> et_libraries.type_component_package_name.bounded_string, -- S_SO14
		element_type 	=> type_package);
	use type_packages;

	package type_libraries is new ordered_maps (
		key_type		=> et_libraries.type_full_library_name.bounded_string, -- projects/lbr/smd_packages.pac
		element_type	=> type_packages.map,
		"<"				=> et_libraries.type_full_library_name."<");

	-- All package models are collected here:
	package_libraries : type_libraries.map;


-- PROPERTIES OF OBJECTS IN COPPER (NON ELECTRIC !!)
	procedure line_copper_properties (
	-- Logs the properties of the given line of copper
		face			: in type_face;
		cursor			: in type_copper_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure arc_copper_properties (
	-- Logs the properties of the given arc of copper
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

	
	
	procedure placeholder_silk_screen_properties (
	-- Logs the properties of the given silk screen placeholder
		face			: in type_face;
		cursor			: in type_package_text_placeholders.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure placeholder_assy_doc_properties (
	-- Logs the properties of the given assembly documentation placeholder
		face			: in type_face;
		cursor			: in type_package_text_placeholders.cursor;
		log_threshold 	: in et_string_processing.type_log_level);
	
	procedure text_silk_screen_properties (
	-- Logs the properties of the given silk screen text
		face			: in type_face;
		cursor			: in type_texts_with_content.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	procedure text_assy_doc_properties (
	-- Logs the properties of the given assembly documentation text
		face			: in type_face;
		cursor			: in type_texts_with_content.cursor;
		log_threshold 	: in et_string_processing.type_log_level);
	
	procedure terminal_properties (
	-- Logs the properties of the terminal indicated by cursor.
		cursor 			: in type_terminals.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	

	function terminal_count (
		library_name		: in et_libraries.type_full_library_name.bounded_string;
		package_name 		: in et_libraries.type_component_package_name.bounded_string)
		return et_libraries.type_terminal_count;

	function terminal_port_map_fits (
	-- Used when terminal_port_maps are to be used for packages.
	-- The given package is specified by the library name and package name.
	-- Returns true if the terminal_port_map fits on the given package.
		library_name		: in et_libraries.type_full_library_name.bounded_string;		-- ../lbr/bel_ic.pretty
		package_name 		: in et_libraries.type_component_package_name.bounded_string;	-- S_SO14
		terminal_port_map	: in et_libraries.type_terminal_port_map.map) 
		return boolean;

		
end et_pcb;

-- Soli Deo Gloria
