------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET KICAD PCB                            --
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
with et_libraries;
with et_schematic;
with et_general;
with et_pcb;
with et_pcb_coordinates;

package et_kicad_pcb is

	pcb_new_version_4_0_7		: constant string (1..5)	:= "4.0.7";
	pcb_file_format_version_4	: constant string (1..1)	:= "4";
	host_name_pcbnew			: constant string (1..6)	:= "pcbnew";
	
	layer_top_copper			: constant string (1..4)	:= "F.Cu";
	layer_bot_copper			: constant string (1..4)	:= "B.Cu";
	layer_all_copper			: constant string (1..4)	:= "*.Cu";
	
	layer_top_solder_paste		: constant string (1..7)	:= "F.Paste";
	layer_bot_solder_paste		: constant string (1..7)	:= "B.Paste";

	layer_top_stop_mask			: constant string (1..6)	:= "F.Mask";
	layer_bot_stop_mask			: constant string (1..6)	:= "B.Mask";
	layer_all_stop_mask			: constant string (1..6)	:= "*.Mask";

	layer_top_silk_screen		: constant string (1..7)	:= "F.SilkS";
	layer_bot_silk_screen		: constant string (1..7)	:= "B.SilkS";

	layer_top_assy_doc			: constant string (1..5)	:= "F.Fab";
	layer_bot_assy_doc			: constant string (1..5)	:= "B.Fab";

	layer_top_keepout			: constant string (1..7)	:= "F.CrtYd";
	layer_bot_keepout			: constant string (1..7)	:= "B.CrtYd";

	keyword_fp_text_reference	: constant string (1..9)	:= "reference";
	keyword_fp_text_value		: constant string (1..5)	:= "value";
	keyword_fp_text_user		: constant string (1..4)	:= "user";
	keyword_fp_text_hide		: constant string (1..4)	:= "hide";

	placeholder_reference		: constant string (1..5)	:= "REF**";

	attribute_technology_smd		: constant string (1..3)	:= "smd";
	attribute_technology_virtual	: constant string (1..7)	:= "virtual";
	
	type type_fp_text_meaning is (REFERENCE, VALUE, USER);
	
-- 	function to_package_model (
-- 	-- Builds a package model from the given lines.
-- 		package_name	: in et_libraries.type_component_package_name.bounded_string; -- S_SO14
-- 		lines			: in et_pcb.type_lines.list;
-- 		log_threshold	: in et_string_processing.type_log_level)
-- 		return et_pcb.type_package;


	-- For the package import we need a special set of layers. 
	type type_layer is (
		TOP_COPPER, BOT_COPPER,
		TOP_SILK, BOT_SILK,
		TOP_ASSY, BOT_ASSY, -- in kicad this is the fab layer
		TOP_KEEP, BOT_KEEP -- in kicad this is the crtyrd layer
		);

	-- LINES, ARCS, CIRCLES
	-- Temporarily we need special types for lines, arcs and circles for the import. 
	-- They are derived from the abstract anchestor types in et_pcb.ads.
	-- Their additional components (width, layer, angle, ...) are later 
	-- copied to the final lines, arcs and circles as specified in type_package:
	type type_line is new et_pcb.type_line with record
		width	: et_pcb.type_text_line_width;
		layer	: type_layer;
	end record;

	type type_arc is new et_pcb.type_arc with record
		width 	: et_pcb.type_text_line_width;
		angle 	: et_pcb_coordinates.type_angle;
		layer	: type_layer;
	end record;

	type type_circle is new et_pcb.type_circle with record -- center and radius incl.
		width 	: et_pcb.type_text_line_width;
		point 	: et_pcb_coordinates.type_point_3d;
		layer	: type_layer;
	end record;

	

	-- nets are assigned an id:
	net_id_max : constant positive := 1_000_000;
	type type_net_id is range 0..net_id_max;
	type type_net_id_terminal is range 1..net_id_max;
	

	
	-- NET CLASSES
	-- KiCad keeps a list of net names which are in a certain net class.
	package type_nets_of_class is new doubly_linked_lists (
		element_type	=> et_schematic.type_net_name.bounded_string,
		"="				=> et_schematic.type_net_name."=");

	-- The net class type used here extends the basic net class by the list
	-- of net names:
	type type_net_class is new et_pcb.type_net_class with record
		net_names : type_nets_of_class.list;
	end record;

	-- Since there are lots of net classes, they are stored in a map:
	package type_net_classes is new ordered_maps (
		key_type		=> et_pcb.type_net_class_name.bounded_string,
		element_type	=> type_net_class,
		"<"				=> et_pcb.type_net_class_name."<"
		);



	
	
	-- Temporarily this type is required to handle texts in silk screen, assembly doc, ...
	-- When inserting the text in the final package, it is decomposed again.
	type type_package_text is new et_pcb.type_text with record
		content	: et_libraries.type_text_content.bounded_string;
		layer	: type_layer;
		meaning	: type_fp_text_meaning;
	end record;



	procedure read_libraries (
	-- Reads package libraries. Root directory is et_libraries.lib_dir.
	-- The libraries in the container are named after the libraries found in lib_dir.
		log_threshold 	: in et_string_processing.type_log_level);

	layer_id_max : constant positive := 49;
	type type_layer_id is range 0..layer_id_max;



	
	type type_board is record
		paper_size : et_general.type_paper_size;
	end record;

	procedure read_board (
		file_name 		: in string;
		log_threshold	: in et_string_processing.type_log_level);

	
end et_kicad_pcb;

-- Soli Deo Gloria
