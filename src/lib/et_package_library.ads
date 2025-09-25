------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          PACKAGE LIBRARY                                 --
--                                                                          --
--                              S p e c                                     --
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


with ada.containers; 			use ada.containers;
with ada.containers.indefinite_ordered_maps;

with et_mirroring;				use et_mirroring;
with et_pcb_sides;				use et_pcb_sides;
with et_board_geometry;			use et_board_geometry;
with et_board_coordinates;		use et_board_coordinates;
with et_pcb_stack;				use et_pcb_stack;
with et_board_text;				use et_board_text;
with et_drills;					use et_drills;
with et_terminals;				use et_terminals;
with et_text;
with et_design_rules_board;				use et_design_rules_board;
with et_conductor_segment;				use et_conductor_segment;
with et_conductor_text.packages;		use et_conductor_text.packages;
with et_route_restrict.packages;		use et_route_restrict.packages;
with et_via_restrict.packages;			use et_via_restrict.packages;
with et_stopmask.packages;				use et_stopmask.packages;
with et_stencil;						use et_stencil;
with et_silkscreen;						use et_silkscreen;
with et_silkscreen.packages;			use et_silkscreen.packages;
with et_assy_doc.packages;				use et_assy_doc.packages;
with et_keepout;						use et_keepout;
with et_pcb_contour;					use et_pcb_contour;
with et_package_model;					use et_package_model;
with et_package_appearance;				use et_package_appearance;
with et_package_names;					use et_package_names;
with et_package_description;			use et_package_description;
with et_logging;						use et_logging;


package et_package_library is
	
	use pac_geometry_brd;

	use pac_geometry_2;
	use pac_polygons;
	use pac_contours;
	use pac_text_board;

	

	
-- PLACEHOLDERS FOR TEXTS
	--type type_text_meaning_package is (NAME, VALUE, PURPOSE);

	--function to_string (text_meaning : in type_text_meaning_package) return string;
	--function to_text_meaning (text_meaning : in string) return type_text_meaning_package;
	
	--type type_text_placeholder is new type_text_fab with record
		--meaning : type_text_meaning_package := NAME;
	--end record;

	---- There can be lots of placeholders of this kind. So they are stored in a list:	
	--package pac_text_placeholders is new doubly_linked_lists (type_text_placeholder);

	---- Placeholders for device name and value can be placed in
	---- silk screen or assembly documentation only:
	--type type_placeholder_package_layer is (SILK_SCREEN, ASSEMBLY_DOCUMENTATION);
	--function to_string (layer : in type_placeholder_package_layer) return string;
	--function to_layer (layer : in string) return type_placeholder_package_layer;
	
	---- A collection of text placeholders in silk screen and assembly documentation 
	---- modelled by this type. The user is free to change them in the 
	---- layout (position, text size, rotation, line width ...).
	---- Initally, when a device is added to the schematic, these placeholders are 
	---- copies of the placeholders defined in the package model.
	--type type_text_placeholders_silk_screen is record
		--top		: pac_text_placeholders.list;
		--bottom	: pac_text_placeholders.list;
	--end record;

	--type type_text_placeholders_assembly_documentation is record
		--top		: pac_text_placeholders.list;
		--bottom	: pac_text_placeholders.list;
	--end record;

	--type type_text_placeholders is record
		--silk_screen	: type_text_placeholders_silk_screen;
		--assy_doc	: type_text_placeholders_assembly_documentation;
	--end record;

	



		

	
	---- Silkscreen objects include placeholders for device name,
	---- value, purpose:
	--type type_silkscreen_package is new type_silkscreen with record
		--placeholders : pac_text_placeholders.list;
	--end record;

	
	---- Silkscreen is about two sides of the board:
	--type type_silkscreen_both_sides is record
		--top		: type_silkscreen_package;
		--bottom	: type_silkscreen_package;
	--end record;

	

	-- Assembly documentation includes placeholders:
	--type type_assembly_documentation 
		--is new et_assy_doc.packages.type_assembly_documentation with 
	--record
		--placeholders: pac_text_placeholders.list;
	--end record;

	---- Because assembly documentation is about two sides of the board this composite is required:
	--type type_assembly_documentation_both_sides is record
		--top		: type_assembly_documentation;
		--bottom	: type_assembly_documentation;
	--end record;




	use pac_package_model_file_name;
	
	-- CS: this should be a hashed map:
	package pac_package_models is new indefinite_ordered_maps ( -- CS ordered_maps ?
		key_type		=> pac_package_model_file_name.bounded_string, -- ../lbr/smd/SO15.pac
		element_type	=> type_package_model);

	use pac_package_models;


	
	
	-- THIS IS THE RIG WIDE PACKAGE (FOOTPRINT) LIBRARY:
	
	package_models	 : pac_package_models.map;
	-- CS rename to library_electrical_devices



	-- Creates a package and stores the package in container et_packages.packages.								 
	procedure create_package (
		package_name 	: in pac_package_model_file_name.bounded_string; -- libraries/packages/S_SO14.pac
		appearance		: in type_package_appearance;
		log_threshold	: in type_log_level);



	
	-- Returns a cursor to the given package model.
	function get_package_model (
		model_name : in pac_package_model_file_name.bounded_string) -- ../lbr/smd/SO15.pac
		return pac_package_models.cursor;

	
	-- Returns true if the given package is real (means it has a height).
	function is_real (package_name : in pac_package_model_file_name.bounded_string) return boolean;

	
	-- Returns a cursor to the requested terminal (with all its properties) 
	-- within the given package model:
	function get_terminal (
		cursor		: in pac_package_models.cursor;
		terminal	: in pac_terminal_name.bounded_string)  -- H4, 14
		return pac_terminals.cursor;



	

	-- Returns the contours of the terminals of a package.
	-- Adresses only those terminals which are affected by
	-- the given layer category:
	function get_terminal_contours (
		package_cursor	: in pac_package_models.cursor;
		layer_category	: in type_signal_layer_category)
		return pac_contour_list.list;


	-- Returns the non-electric conductor objects of
	-- the given package. Adresses only those objects
	-- which are affected by the given layer category.
	-- Raises exception if layer category INNER is given,
	-- because there are no non-electric objects in inner layers:
	function get_conductor_objects (
		package_cursor	: in pac_package_models.cursor;
		layer_category	: in type_signal_layer_category)
		-- CS layer_category	: in type_signal_layer_category_outer) ?
		return type_conductor_objects;


	-- Returns the contours of route restrict objects of
	-- the given package. Adresses only those objects
	-- which are affected by the given layer category:
	function get_route_restrict_objects (
		package_cursor	: in pac_package_models.cursor;
		layer_category	: in type_signal_layer_category)
		-- CS layer_category	: in type_signal_layer_category_outer) ?
		return et_route_restrict.packages.type_one_side;


	-- Returns the contours of via restrict objects of
	-- the given package. Adresses only those objects
	-- which are affected by the given layer category:
	function get_via_restrict_objects (
		package_cursor	: in pac_package_models.cursor;
		layer_category	: in type_signal_layer_category)
		-- CS layer_category	: in type_signal_layer_category_outer) ?
		return et_via_restrict.packages.type_one_side;

	
	-- Returns the contours of pcb holes of the given package:
	function get_hole_contours (
		package_cursor	: in pac_package_models.cursor)
		return pac_holes.list;


	-- Returns the contours of keepout objects of the given package.
	-- Adresses only those objects affected by the given face:
	function get_keepout_objects (
		package_cursor	: in pac_package_models.cursor;
		face			: in type_face)
		return type_keepout;

	
	-- Returns the contours of stencil objects of the given package.
	-- Adresses only those objects affected by the given face:
	function get_stencil_objects (
		package_cursor	: in pac_package_models.cursor;
		face			: in type_face)
		return et_stencil.type_stencil;


	-- Returns the contours of stopmask objects of the given package.
	-- Adresses only those objects affected by the given face:
	function get_stopmask_objects (
		package_cursor	: in pac_package_models.cursor;
		face			: in type_face)
		return et_stopmask.type_stopmask;


	-- Returns the contours of silkscreen objects (incl. plaaceholders) 
	-- of the given package model.
	-- Adresses only those objects affected by the given face:
	function get_silkscreen_objects (
		package_cursor	: in pac_package_models.cursor;
		face			: in type_face)
		return type_silkscreen_package;


	-- Returns the contours of assembly documentation 
	-- objects (incl. plaaceholders) of the given package model.
	-- Adresses only those objects affected by the given face:
	function get_assy_doc_objects (
		package_cursor	: in pac_package_models.cursor;
		face			: in type_face)
		return type_assy_doc_package;
	
	

	
	

-- PROPERTIES OF OBJECTS IN BOARD CONTOUR / OUTLINE / EDGE CUTS
	--procedure line_pcb_contour_properties (
	---- Logs the properties of the given line of pcb contour
		--cursor			: in pac_pcb_contour_lines.cursor;
		--log_threshold 	: in type_log_level);

	--procedure arc_pcb_contour_properties (
	---- Logs the properties of the given arc of pcb contour
		--cursor			: in pac_pcb_contour_arcs.cursor;
		--log_threshold 	: in type_log_level);

	--procedure circle_pcb_contour_properties (
	---- Logs the properties of the given circle of pcb contour
		--cursor			: in pac_pcb_contour_circles.cursor;
		--log_threshold 	: in type_log_level);

	

	
end et_package_library;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
