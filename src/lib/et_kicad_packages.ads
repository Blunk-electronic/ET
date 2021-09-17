------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      KICAD COMPONENT PACKAGES                            --
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

with ada.text_io;				use ada.text_io;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;

with et_general;				use et_general;
with et_geometry;				use et_geometry;
with et_drills;
with et_terminals;				use et_terminals;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
use et_board_shapes_and_text.pac_shapes;
use et_board_shapes_and_text.pac_text_fab;

with et_packages;
with et_kicad_general;			use et_kicad_general;
with et_import;
with et_pcb_coordinates;		use et_pcb_coordinates;
use et_pcb_coordinates.pac_geometry_brd;

with et_string_processing;		use et_string_processing;
with et_text;					--use et_text;

package et_kicad_packages is

	-- NOTE: this is not a real file extension but just a part of a directory name:
	package_library_directory_extension	: constant string := ".pretty";
	
	package_file_extension				: constant string := "kicad_mod";

	-- These constants are required for directory entry searches:
	package_library_pattern	: constant string := "*" & package_library_directory_extension;
	package_pattern 		: constant string := "*." & package_file_extension;

	-- For the package import we need a special set of layers. 
	type type_layer_abbrevation is (
		EDGE_CUTS,	-- the board outline or contour
		TOP_COPPER, BOT_COPPER,
		TOP_SILK, BOT_SILK,
		TOP_ASSY, BOT_ASSY, -- in kicad this is the fab layer
		TOP_STOP, BOT_STOP, -- solder stop mask
		TOP_PASTE, BOT_PASTE, -- stencil, solder paste, cream
		TOP_KEEP, BOT_KEEP -- in kicad this is the crtyrd layer
		-- CS TOP_GLUE, BOT_GLUE
		);
	
	layer_top_copper			: constant string := "F.Cu";
	layer_bot_copper			: constant string := "B.Cu";
	layer_all_copper			: constant string := "*.Cu";
	
	layer_top_silk_screen		: constant string := "F.SilkS";
	layer_bot_silk_screen		: constant string := "B.SilkS";

	layer_top_assy_doc			: constant string := "F.Fab";
	layer_bot_assy_doc			: constant string := "B.Fab";

	layer_top_keepout			: constant string := "F.CrtYd";
	layer_bot_keepout			: constant string := "B.CrtYd";

	layer_top_stop_mask			: constant string := "F.Mask";
	layer_bot_stop_mask			: constant string := "B.Mask";
	layer_all_stop_mask			: constant string := "*.Mask";
	
	layer_top_solder_paste		: constant string := "F.Paste";
	layer_bot_solder_paste		: constant string := "B.Paste";
	
	keyword_fp_text_reference	: constant string := "reference";
	keyword_fp_text_value		: constant string := "value";
	keyword_fp_text_user		: constant string := "user";
	keyword_fp_text_hide		: constant string := "hide";
	
	placeholder_reference		: constant string := "REF**";

	attribute_technology_smd		: constant string := "smd";
	attribute_technology_virtual	: constant string := "virtual";

	type type_fp_text_meaning is (REFERENCE, VALUE, USER);


	-- LINES, ARCS, CIRCLES
	-- Temporarily we need special types for lines, arcs and circles for the import. 
	-- They are derived from the abstract anchestor types in et_pcb.ads.
	-- Their additional components (width, layer, angle, ...) are later 
	-- copied to the final lines, arcs and circles as specified in et_pcb.ads:
	type type_line is new pac_shapes.type_line with record
		width	: type_text_line_width;
		layer	: type_layer_abbrevation;
	end record;

	type type_arc is new pac_shapes.type_arc with record
		width 	: type_text_line_width;
		angle 	: et_pcb_coordinates.type_rotation;
		layer	: type_layer_abbrevation;
	end record;

	type type_circle is new pac_shapes.type_circle with record -- center and radius incl.
		width 	: type_text_line_width;
		point 	: type_point;
		layer	: type_layer_abbrevation;
	end record;

	
	
	-- This is the base type of a package:
	type type_package is new et_packages.type_package_base with record
		time_stamp : type_timestamp;
	end record;

	type type_package_library is new type_package with record
		silk_screen				: et_packages.type_silk_screen_both_sides; -- incl. placeholder for reference and purpose
		assembly_documentation	: et_packages.type_assembly_documentation_both_sides; -- incl. placeholder for value
		terminals				: et_terminals.type_terminals.map;
	end record;

	
	package_tags_length_max : constant positive := 200;
	package type_package_tags is new generic_bounded_length (package_tags_length_max);

	function to_string (tags : in type_package_tags.bounded_string) return string;

	function to_package_tags (tags : in string) return type_package_tags.bounded_string;

	
	function to_assembly_technology (
		tech : in string) 
		return type_assembly_technology;
	
	type type_pad_shape_tht is (
		CIRCULAR, 
		OVAL,
		RECTANGULAR
		-- CS others ?
		);

	
	function to_string (shape : in type_pad_shape_tht) return string;
	function to_pad_shape_tht (shape : in string) return type_pad_shape_tht;
	
	type type_pad_shape_smt is (
		CIRCULAR, 
		OVAL,
		RECTANGULAR
		-- CS others ?
		);
	
	function to_string (shape : in type_pad_shape_smt) return string;	
	function to_pad_shape_smt (shape : in string) return type_pad_shape_smt;

	-- Converts the given position and dimensions of a circular pad
	-- to a list containing just one circle.
	function to_pad_shape_circle (
		position	: in type_position;
		diameter	: in type_pad_size;
		offset		: in type_distance_relative)	-- the offset of the pad from the center
		return type_polygon;
	
	-- Converts the given position and dimensions of a rectangular pad
	-- to a list with four lines (top, bottom, right, left).
	-- CS: rework as in to_pad_shape_oval
	function to_pad_shape_rectangle (
		center		: in type_position; -- the pad center position (incl. angle)
		size_x		: in type_pad_size;	-- the size in x of the pad
		size_y		: in type_pad_size;	-- the size in y of the pad
		offset		: in type_distance_relative)	-- the offset of the pad from the center
		return type_polygon;
	
	-- Converts the given position and dimensions of an oval pad
	-- to a list with two vertical lines and two arcs (rotation assumed zero).
	function to_pad_shape_oval (
		center	: in type_position;	-- the pad center position (incl. angle)
		size_x	: in type_pad_size;	-- the size in x of the pad
		size_y	: in type_pad_size;	-- the size in y of the pad
		offset	: in type_distance_relative)	-- the offset of the pad from the center
		return type_polygon;
	
	-- slotted holes	
	tht_hole_shape_oval	: constant string := "oval";
	pad_drill_offset	: constant string := "offset";
	type type_tht_hole_shape is (CIRCULAR, OVAL);

	-- "Slotted drills" or "plated millings" for terminals are limited by drill sizes because
	-- the PCB manufacturer starts the milling with a drill.
	subtype type_pad_milling_size is type_distance_positive
		range et_drills.drill_size_min .. et_drills.drill_size_max;
	
	-- Converts the given position and dimensions of a rectangular slotted hole
	-- to a list with four lines (top, bottom, right, left).
	function to_pad_milling_contour (
		center	: in type_position; -- the terminal position (incl. angle, (z axis ignored))
		size_x	: in type_pad_size;	-- the size in x of the hole
		size_y	: in type_pad_size;	-- the size in y of the hole
		offset	: in type_distance_relative) -- the offset of the pad from the center
		return pac_polygon_segments.list;
	
	-- For packages, temporarily this type is required to handle texts in 
	-- silk screen, assembly doc, ...
	-- When inserting the text in the final package, it is decomposed again.
	--type type_text_package is new et_packages.type_text with record
	type type_text_package is new type_text_fab with record
		content	: et_text.pac_text_content.bounded_string;
		layer	: type_layer_abbrevation;
		meaning	: type_fp_text_meaning;
	end record;

	
	directory_name_length_max : constant positive := 200;
	package pac_directory_name is new generic_bounded_length (directory_name_length_max);

	function to_string (directory_name : in pac_directory_name.bounded_string) return string;
	-- Converts a directory name to a string.

-- 	function to_directory (directory_name : in string) return pac_directory_name.bounded_string;
-- 	-- Converts a string to a pac_directory_name.

	
	function to_package_model (
	-- Builds a package model from the given lines.
		file_name		: in string; -- S_0201.kicad_mod
		lines			: in pac_lines_of_file.list;
		log_threshold	: in et_string_processing.type_log_level)
		return type_package_library;
	
	procedure read_libraries ( -- CS rename to read_package_libraries
	-- Reads package libraries.
	-- Create the libraries in container package_libraries. 
	-- The libraries in the container are named like ../lbr/tht_packages/plcc.pretty
		log_threshold 	: in et_string_processing.type_log_level);
	
	-- Lots of packages (in a library) can be collected in a map:
	package type_packages_library is new indefinite_ordered_maps (
		key_type 		=> et_packages.pac_package_name.bounded_string, -- S_SO14, T_0207
		"<"				=> et_packages.pac_package_name."<",
		element_type 	=> type_package_library);
	
	package type_libraries is new ordered_maps ( -- CS rename to pac_package_libraries
		key_type		=> type_package_library_name.bounded_string, -- projects/lbr/smd_packages.pretty
		element_type	=> type_packages_library.map,
		"="				=> type_packages_library."=",
		"<"				=> type_package_library_name."<");
	-- CS the element could be a record consisting of type_packages_library.map, lib_type, options and desrciption
	-- lib_type, options and description are provided in V5 and should be stored here in the future.
	

	-- V4: 
	--	- All package models found in the project libraries are collected here.
	-- V5: 
	--	- After reading the sym-lib-tables and fp-lib-tables empty libraries are created here.
	--	- Procedure read_libraries in turn fills the libraries with content.
	package_libraries : type_libraries.map;

	
	
end et_kicad_packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
