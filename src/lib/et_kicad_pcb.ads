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

--   For correct displaying set tab width in your edtior to 4.

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
--with ada.containers.vectors;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_string_processing;
with et_libraries;
with et_schematic;
with et_general;
with et_pcb;
with et_pcb_coordinates;
with et_kicad;

package et_kicad_pcb is

	pcb_new_version_4_0_7		: constant string (1..5)	:= "4.0.7";
	pcb_file_format_version_4	: constant string (1..1)	:= "4";
	host_name_pcbnew			: constant string (1..6)	:= "pcbnew";
	
	layer_top_copper			: constant string (1..4)	:= "F.Cu";
	layer_bot_copper			: constant string (1..4)	:= "B.Cu";
	layer_all_copper			: constant string (1..4)	:= "*.Cu";

	-- NOTE: this is not a real file extension but just a part of a directory name:
	package_library_directory_extension	: constant string (1..7) := ".pretty";
	
	package_file_extension				: constant string (1..9) := "kicad_mod";

	-- These constants are required for directory entry searches:
	package_library_pattern	: constant string (1..8)	:= "*" & package_library_directory_extension;
	package_pattern 		: constant string (1..11)	:= "*." & package_file_extension;
	

	function full_library_name (
		library_name	: in et_libraries.type_library_name.bounded_string; -- bel_logic
		package_name 	: in et_libraries.type_component_package_name.bounded_string; -- S_SO14
		log_threshold	: in et_string_processing.type_log_level)
		return et_libraries.type_full_library_name.bounded_string;
	-- Returns the first library directory (in search_list_project_lib_dirs) that
	-- contains the given package library with the given package.

	
	-- For things in section layers like (0 F.Cu signal) or (49 F.Fab user) we have those specs.
	-- This is board file related.
	layer_id_max : constant positive := 49; -- includes ALL layers (signal and non-signal)
	type type_layer_id is range 0..layer_id_max;

	function to_string (layer : in type_layer_id) return string;
	-- returns the given layer id as string.

	function to_layer_id (layer : in string) return type_layer_id;
	-- Converts a string like B.CU or F.Fab to a kicad layer id (0..49)

	
	layer_top_solder_paste		: constant string (1..7)	:= "F.Paste";
	layer_top_solder_paste_id	: constant type_layer_id	:= 35;
	layer_bot_solder_paste		: constant string (1..7)	:= "B.Paste";
	layer_bot_solder_paste_id	: constant type_layer_id 	:= 34;
	
	layer_top_stop_mask			: constant string (1..6)	:= "F.Mask";
	layer_top_stop_mask_id		: constant type_layer_id 	:= 39;
	layer_bot_stop_mask			: constant string (1..6)	:= "B.Mask";
	layer_bot_stop_mask_id		: constant type_layer_id 	:= 38;	
	layer_all_stop_mask			: constant string (1..6)	:= "*.Mask";

	layer_top_silk_screen		: constant string (1..7)	:= "F.SilkS";
	layer_top_silk_screen_id	: constant type_layer_id 	:= 37;
	layer_bot_silk_screen		: constant string (1..7)	:= "B.SilkS";
	layer_bot_silk_screen_id	: constant type_layer_id 	:= 36;
	
	layer_top_assy_doc			: constant string (1..5)	:= "F.Fab";
	layer_top_assy_doc_id		: constant type_layer_id 	:= 49;
	layer_bot_assy_doc			: constant string (1..5)	:= "B.Fab";
	layer_bot_assy_doc_id		: constant type_layer_id 	:= 48;
	
	layer_top_keepout			: constant string (1..7)	:= "F.CrtYd";
	layer_top_keepout_id		: constant type_layer_id 	:= 47;
	layer_bot_keepout			: constant string (1..7)	:= "B.CrtYd";
	layer_bot_keepout_id		: constant type_layer_id 	:= 46;
	
	layer_edge_cuts				: constant string (1..9)	:= "Edge.Cuts";
	layer_edge_cuts_id			: constant type_layer_id 	:= 44;
	
	-- CS other layers like adhes, eco, margin, ...
	
	keyword_fp_text_reference	: constant string (1..9)	:= "reference";
	keyword_fp_text_value		: constant string (1..5)	:= "value";
	keyword_fp_text_user		: constant string (1..4)	:= "user";
	keyword_fp_text_hide		: constant string (1..4)	:= "hide";

	keyword_fp_text_mirrored	: constant string (1..6)	:= "mirror";
	
	placeholder_reference		: constant string (1..5)	:= "REF**";

	attribute_technology_smd		: constant string (1..3)	:= "smd";
	attribute_technology_virtual	: constant string (1..7)	:= "virtual";
	
	drill_shape_oval	: constant string (1..4) := "oval"; -- used with slotted holes
	pad_drill_offset	: constant string (1..6) := "offset";
	type type_drill_shape is (CIRCULAR, SLOTTED);

	-- "Slotted drills" or "plated millings" for terminals are limited by drill sizes because
	-- the PCB manufacturer starts the milling with a drill.
	subtype type_pad_milling_size is et_pcb_coordinates.type_distance range et_pcb.drill_size_min .. et_pcb.drill_size_max;
	
	type type_fp_text_meaning is (REFERENCE, VALUE, USER);
	
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

	-- LINES, ARCS, CIRCLES
	-- Temporarily we need special types for lines, arcs and circles for the import. 
	-- They are derived from the abstract anchestor types in et_pcb.ads.
	-- Their additional components (width, layer, angle, ...) are later 
	-- copied to the final lines, arcs and circles as specified in et_pcb.ads:
	type type_line is new et_pcb.type_line with record
		width	: et_pcb.type_text_line_width;
		layer	: type_layer_abbrevation;
	end record;

	type type_arc is new et_pcb.type_arc with record
		width 	: et_pcb.type_text_line_width;
		angle 	: et_pcb_coordinates.type_angle;
		layer	: type_layer_abbrevation;
	end record;

	type type_circle is new et_pcb.type_circle with record -- center and radius incl.
		width 	: et_pcb.type_text_line_width;
		point 	: et_pcb_coordinates.type_point_3d;
		layer	: type_layer_abbrevation;
	end record;



	
	-- PLOT PARAMETERS (CAM JOB)
	-- NOTE: plot parameters are directly imprinted in the board. There is no CAM file like in EAGLE.

	-- layerselection
	-- CS meaning not clear yet. for the time being we save the layerselection argument (like 0x00030_80000001)
	-- in a bounded string.
	plot_layer_selection_length_max : constant positive := 20;
	package type_plot_layer_selection_string is new generic_bounded_length (plot_layer_selection_length_max);

	-- usergerberextensions
	type type_plot_user_gerber_extensions is new boolean;
	-- CS functions to_user_gerber_extensions and to_string

	-- excludeedgelayer
	type type_plot_exclude_edge_layer is new boolean;
	-- CS functions to_exclude_edge_layer and to_string

	-- plotframeref
	type type_plot_frame_ref is new boolean;
	-- CS functions to_plot_frame_ref and to_string

	-- viasonmask
	type type_plot_vias_on_mask is new boolean;
	-- CS functions to_vias_on_mask and to_string

	-- mode
	type type_plot_fill_mode is range 1..2; -- 1 filled, 2 sketch

	-- useauxorigin
	type type_plot_use_aux_origin is new boolean;
	-- CS functions to_use_aux_origin and to_string

	-- hpglpennumber
	type type_plot_hpgl_pen_number is range 1..1; -- CS so far nothing more known

	-- hpglpenspeed
	type type_plot_hpgl_pen_speed is range 20..20; -- CS so far nothing more known

	-- hpglpendiameter -- given in mil
	hpgl_pen_diameter_min : constant et_pcb_coordinates.type_distance := 0.1;
	hpgl_pen_diameter_max : constant et_pcb_coordinates.type_distance := 1.0;
	subtype type_plot_hpgl_pen_diameter is et_pcb_coordinates.type_distance 
		range hpgl_pen_diameter_min .. hpgl_pen_diameter_max;

	-- hpglpenoverlay
	plot_hpgl_pen_overlay_min : constant et_pcb_coordinates.type_distance := 0.05; -- CS refine range if required
	plot_hpgl_pen_overlay_max : constant et_pcb_coordinates.type_distance := 0.1;  -- CS refine range if required
	subtype type_plot_hpgl_pen_overlay is et_pcb_coordinates.type_distance
		range plot_hpgl_pen_overlay_min .. plot_hpgl_pen_overlay_max;

	-- psnegative
	type type_plot_ps_negative is new boolean;
	-- CS functions to_ps_negative and to_string

	-- psa4output
	type type_plot_psa_4_output is new boolean;
	-- CS function to_psa_4_output and to_string

	-- plotreference
	type type_plot_reference is new boolean;
	-- CS function to_plot_reference and to_string

	-- plotvalue
	type type_plot_value is new boolean;
	-- CS function to_plot_value and to_string
	
	-- plotinvisibletext
	type type_plot_invisible_text is new boolean;
	-- CS function to_plot_invisible_text and to_string

	-- padsonsilk
	type type_pads_on_silk is new boolean;
	-- CS function to_pads_on_silk and to_string

	-- subtractmaskfromsilk
	type type_plot_subtract_mask_from_silk is new boolean;
	-- CS function to_subtract_mask_from_silk and to_string

	-- outputformat
	type type_plot_output_format is range 0..5; 
		-- 0 hpgl
		-- 1 gerber
		-- 2 postscript
		-- 3 dxf
		-- 4 pdf
		-- 5 svg
	
	-- mirror
	type type_plot_mirror is new boolean;
	-- CS function to_mirror and to_string
	
	-- drillshape. NOTE: DO NOT CONFUSE WITH type_drill_shape (see above)
	type type_plot_drill_shape is range 0..1; -- CS so far nothing more known.
	
	-- scaleselection
	type type_plot_scale_selection is range 1..1; -- CS no far nothing more known

	-- outputdirectory
	plot_output_directory_length_max : constant positive := 200;
	package type_plot_output_directory is new generic_bounded_length (plot_output_directory_length_max);
	function to_plot_output_directory (directory : in string) return type_plot_output_directory.bounded_string;
	function to_string (directory : in type_plot_output_directory.bounded_string) return string;

	type type_plot_setup is record
		layer_selection			: type_plot_layer_selection_string.bounded_string;
		user_gerber_extensions	: type_plot_user_gerber_extensions;
		exclude_edge_layer		: type_plot_exclude_edge_layer;
		line_width				: et_pcb.type_general_line_width;	-- for lines without given width
		frame_ref				: type_plot_frame_ref;
		vias_on_mask			: type_plot_vias_on_mask;
		fill_mode				: type_plot_fill_mode;
		use_aux_origin			: type_plot_use_aux_origin;
		hpgl_pen_number			: type_plot_hpgl_pen_number;
		hpgl_pen_speed			: type_plot_hpgl_pen_speed;
		hpgl_pen_diameter		: type_plot_hpgl_pen_diameter;
		hpgl_pen_overlay		: type_plot_hpgl_pen_overlay;
		ps_negative				: type_plot_ps_negative;
		psa_4_output			: type_plot_psa_4_output;
		reference				: type_plot_reference;
		value					: type_plot_value;
		invisble_text			: type_plot_invisible_text;
		pads_on_silk			: type_pads_on_silk;
		subtract_mask_from_silk	: type_plot_subtract_mask_from_silk;
		output_format			: type_plot_output_format;
		mirror					: type_plot_mirror;
		drill_shape				: type_plot_drill_shape;
		scale_selection			: type_plot_scale_selection;
		output_directory 		: type_plot_output_directory.bounded_string;
	end record;



	
	-- board contours
	-- NOTE: It is not reasonable to draw outlines with a line width other than zero.
	-- Reason: The manufacturer is to cut or mill along these lines and must calculate
	-- the center of the line. However, as kicad allows line with here, it must 
	-- fit in a reasonable range, thus a subtype:
	edge_cut_line_width_min : constant et_pcb_coordinates.type_distance := 0.1;
	edge_cut_line_width_max : constant et_pcb_coordinates.type_distance := 1.0;
	subtype type_edge_cut_line_width is et_pcb_coordinates.type_distance 
		range edge_cut_line_width_min .. edge_cut_line_width_max;

	-- The via diameter is the drill size + 2*restring width. 
	-- We fit the via diameter (incl. microvias) in a reasonable range via a subtype:
	via_diameter_min : constant et_pcb_coordinates.type_distance := 0.1;
	via_diameter_max : constant et_pcb_coordinates.type_distance := 10.0;
	subtype type_via_diameter is et_pcb_coordinates.type_distance
		range via_diameter_min .. via_diameter_max;



	-- BOARD SETUP (DRC)
	-- NOTE: DRC parameters are directly imprinted in the board. There is no DRC file like in EAGLE.

	-- If polygons are allowed to have 45 deg slopes (applies to all polygons)
	type type_zone_45_only is (
		NO, -- outline slope "arbitray" in gui
		YES	-- outline slope "H,V and 45 deg only" in gui
		 );

	-- CS it is not fully clear what aux_axis_origin is good for:
	aux_axis_origin_min : constant et_pcb_coordinates.type_distance := et_pcb_coordinates.zero_distance;
	aux_axis_origin_max : constant et_pcb_coordinates.type_distance := 500.0;
	subtype type_aux_axis_origin is et_pcb_coordinates.type_distance 
		range aux_axis_origin_min .. aux_axis_origin_max;

	-- CS meaning not clear yet
	type type_visible_elements is new string (1..8);

	type type_board_setup is record
		last_trace_width	: et_pcb.type_signal_width;
		trace_clearance		: et_pcb.type_signal_clearance;
		zone_clearance		: et_pcb.type_signal_clearance;
		zone_45_only		: type_zone_45_only;
		trace_min			: et_pcb.type_signal_width;
		segment_width		: et_pcb.type_signal_width;
		edge_width			: type_edge_cut_line_width;
		via_size			: type_via_diameter;	-- regular vias
		via_drill			: et_pcb.type_drill_size;		-- regular vias
		via_min_size		: type_via_diameter;	-- regular vias
		via_min_drill		: et_pcb.type_drill_size;		-- regular vias
		micro_via_size		: type_via_diameter;	-- micro vias
		micro_via_drill		: et_pcb.type_drill_size;		-- micro vias
		micro_vias_allowed	: et_pcb.type_micro_vias_allowed;
		micro_via_min_size	: type_via_diameter;	-- micro vias
		micro_via_min_drill	: et_pcb.type_drill_size;		-- micro vias
		pcb_text_width		: et_pcb.type_text_line_width;	-- all kinds of texts (no matter what layer)
		pcb_text_size_x		: et_pcb.type_text_size;
		pcb_text_size_y		: et_pcb.type_text_size;		
		module_edge_width	: et_pcb.type_general_line_width;
		module_text_size_x	: et_pcb.type_text_size;
		module_text_size_y	: et_pcb.type_text_size;
		module_text_width	: et_pcb.type_text_line_width; -- line width
		pad_size_x			: et_pcb.type_pad_size;
		pad_size_y			: et_pcb.type_pad_size;
		pad_drill			: et_pcb.type_drill_size;
		stop_mask_expansion	: et_pcb.type_stop_mask_expansion;
		aux_axis_origin_x	: type_aux_axis_origin;
		aux_axis_origin_y	: type_aux_axis_origin;
		visible_elements	: type_visible_elements;
	end record;



	
	-- NETLIST ((things like (net 4 /LED_ANODE) ):
	-- NOTE: this has nothing to do with the kicad netlist file !
	net_id_max : constant positive := 1_000_000; -- one million nets should be sufficient
	type type_net_id is range 0..net_id_max; -- used in the "netlist" section
	subtype type_net_id_terminal is type_net_id range 1 .. type_net_id'last; -- used with pads in module section

	function to_net_id (net_id : in string) return type_net_id;
	-- returns the given net id as type_net_id

	function to_string (net_id : in type_net_id) return string;
	-- returns the given net id as string.
	
	type type_netlist_net is record
		id		: type_net_id;
		name	: et_schematic.type_net_name.bounded_string;
	end record;

	-- When nets are collected in an ordered set, the next two functions serve to
	-- detect double usage of net id or net name:
	function right_net_before_left (right, left : in type_netlist_net) return boolean;
	-- Returns true if the right net id comes beforr the left net id AND
	-- if the right net name differs from the left net name.

	function right_net_equals_left (right, left : in type_netlist_net) return boolean;
	-- Returns true if the right net id equals the left net id OR
	-- if the right net name equals the left net name.

	
	-- Nets are collected in an ordered set, that uses the aforementioned two functions:
	package type_netlist is new ordered_sets (
		element_type 	=> type_netlist_net,
		"<"				=> right_net_before_left,
		"="				=> right_net_equals_left);


	-- GENERAL BOARD INFORMATION
	-- The estimation is that the number of links is about 10 times the
	-- number of nets. A link is a connection from one terminal to another.
	general_links_max : constant positive := 10 * net_id_max; 
	type type_general_links is range 0 .. general_links_max;

	-- CS The meaning of no_connects is not clear yet. However, it
	-- must be limited. For the time being the limit equals the number of links:
	general_no_connects_max : constant positive := general_links_max;
	type type_general_no_connects is range 0 .. general_no_connects_max;

	-- The total number of lines, arcs, circles the board outlines consists of:
	type type_general_drawings is new natural; -- no limit because there can be millions

	-- The total number of tracks
	type type_general_tracks is new natural; -- no limit because there can be millions

	-- zones 
	-- CS: meaning not clear yet.
	type type_general_zones is new natural; -- should be limited ot a reasonable value

	-- modules
	-- The total number of component packages.
	general_modules_max : constant positive := 1_000_000; -- CS adjust if nessecary
	type type_general_modules is range 0 .. general_modules_max;

	type type_general_board_info is record
		links		: type_general_links;
		no_connects	: type_general_no_connects;
		area_x1		: et_pcb_coordinates.type_distance; -- CS meaning not clear yet. unit mm ?
		area_y1		: et_pcb_coordinates.type_distance; -- CS meaning not clear yet. unit mm ?
		area_x2		: et_pcb_coordinates.type_distance; -- CS meaning not clear yet. unit mm ?
		area_y2		: et_pcb_coordinates.type_distance; -- CS meaning not clear yet. unit mm ?
		thickness	: et_pcb_coordinates.type_pcb_thickness;
		drawings	: type_general_drawings;
		tracks		: type_general_tracks;
		zones		: type_general_zones;
		modules		: type_general_modules;
		nets		: type_net_id_terminal; -- the total number of nets
	end record;



	
	-- NET CLASSES
	-- NOTE: net class settings are directly imprinted in the board. There is no net class file.
	
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



	
	
	-- For packages, temporarily this type is required to handle texts in 
	-- silk screen, assembly doc, ...
	-- When inserting the text in the final package, it is decomposed again.
	type type_text_package is new et_pcb.type_text with record
		content	: et_libraries.type_text_content.bounded_string;
		layer	: type_layer_abbrevation;
		meaning	: type_fp_text_meaning;
	end record;


	
	-- For the board, temporarily this type is required to handle texts in
	-- copper, silk screen, assembly doc, ...
	-- When inserting the text in the board, it is decomposed again.	
	type type_text_board is new et_pcb.type_text with record
		content	: et_libraries.type_text_content.bounded_string;
		layer	: type_layer_id; -- 0 .. 49 (ALL layers)
	end record;


	
-- LIBRARIES
	-- This is the base type of a package:
	type type_package is new et_pcb.type_package_base with record
		time_stamp : et_kicad.type_timestamp;
	end record;


	type type_package_library is new type_package with record
		silk_screen				: et_pcb.type_silk_screen_package_both_sides; -- incl. placeholder for reference and purpose
		assembly_documentation	: et_pcb.type_assembly_documentation_package_both_sides; -- incl. placeholder for value
		terminals				: et_pcb.type_terminals.map;
	end record;
	
	-- Lots of packages (in a library) can be collected in a map:
	package type_packages_library is new indefinite_ordered_maps (
		key_type 		=> et_libraries.type_component_package_name.bounded_string, -- S_SO14, T_0207
		"<"				=> et_libraries.type_component_package_name."<",
		element_type 	=> type_package_library);
	
	package type_libraries is new ordered_maps (
		key_type		=> et_libraries.type_full_library_name.bounded_string, -- projects/lbr/smd_packages.pretty
		element_type	=> type_packages_library.map,
		"="				=> type_packages_library."=",
		"<"				=> et_libraries.type_full_library_name."<");

	-- V4: All package models found in the project libraries are collected here:
	package_libraries : type_libraries.map;

	
	procedure read_libraries (
	-- Reads package libraries.
	-- Create the libraries in container package_libraries. 
	-- The libraries in the container are named like ../lbr/tht_packages/plcc.pretty
		log_threshold 	: in et_string_processing.type_log_level);



	
	
	layer_name_length_max : constant positive := 9;
	package type_layer_name is new generic_bounded_length (layer_name_length_max); -- B.Cu
	
	function to_layer_name (name : in string) return type_layer_name.bounded_string;
	-- converts a layer name given as string to a bounded string

	type type_layer_meaning is (SIGNAL, USER);
	function to_layer_meaning (meaning : in string) return type_layer_meaning;
	-- converts a layer meaning given as string to a bounded string
	
	type type_layer is record
		name	: type_layer_name.bounded_string;
		meaning	: type_layer_meaning;
	end record;

	package type_layers is new ordered_maps (
		key_type		=> type_layer_id,
		element_type	=> type_layer);




	
	-- Packages (kicad refers to them as modules) as they are listed in the board file are similar to
	-- packages in the libraries. However, there are differences:
	-- - no placeholders for reference and value (here the final component reference and value is)
	-- - x/y position and angle of the package 
	-- - pads with net names

	
	-- In the pcb drawing, a terminal has a net attached. For this reason a
	-- list of terminals is declared here:
	type type_terminal is new et_pcb.type_terminal with record
		net_name : et_schematic.type_net_name.bounded_string;
	end record;

	-- the list of terminals of a package:
	package type_terminals is new indefinite_ordered_maps (
		key_type		=> et_libraries.type_terminal_name.bounded_string,
		element_type	=> type_terminal,
		"<"				=> et_libraries.type_terminal_name."<");



	
	-- A package in a board extends the base package type:
	type type_package_board is new type_package with record
		silk_screen				: et_pcb.type_silk_screen_package_both_sides;
		assembly_documentation	: et_pcb.type_assembly_documentation_package_both_sides;
		terminals				: type_terminals.map; -- terminals with net names
		time_edit				: et_kicad.type_timestamp;
		value					: et_libraries.type_component_value.bounded_string;
		position				: et_pcb_coordinates.type_package_position; -- incl. angle, face
	end record;

	-- Lots of packages (in a board) can be collected in a map:
	package type_packages_board is new indefinite_ordered_maps (
		key_type 		=> et_libraries.type_component_reference, -- IC46
		element_type 	=> type_package_board,
		"<"				=> et_schematic.compare_reference
		);



	
	-- For handling inner signal layers we have a prefix and a suffix
	-- Together with the layer number something like In5.Cu is evaluated.
	layer_inner_prefix			: constant string (1..2)	:= "In";
	layer_inner_suffix			: constant string (1..3)	:= ".Cu";	

	-- The bottom signal layer in kicad is always number 31. Top layer is number 0.
	signal_layer_id_top		: constant type_layer_id := 0;
	signal_layer_id_bottom	: constant type_layer_id := 31;
	subtype type_signal_layer_id is type_layer_id range signal_layer_id_top..signal_layer_id_bottom;
	-- NOTE: On import, the kicad bottom copper layer becomes the ET signal layer 32 !
	-- (NOT et_pcb.type_signal_layer'last !!)
	
	function to_signal_layer_id (layer : in string) return type_signal_layer_id;
	-- Translates a string like F.Cu or In2.Cu or or In15.Cu to a type_signal_layer_id (0..31) -- see spec

	
	-- This is a hex number for lock information or differential signals:
	-- see https://forum.kicad.info/t/meaning-of-segment-status/10912/1
	segment_status_length_max : constant positive := 8;
	package type_segment_status is new generic_bounded_length (segment_status_length_max);

	-- CS function to_segment_status and to_string
	
	type type_segment is new et_pcb.type_copper_line with record
		net_id		: type_net_id;
		layer		: type_signal_layer_id;
		timestamp	: et_kicad.type_timestamp;
		status		: type_segment_status.bounded_string; -- holds lock status and differential status
	end record;

	-- segments are stored in lists
	package type_segments is new doubly_linked_lists (type_segment);


	-- This is a hex number for lock information and other stuff:
	-- see https://forum.kicad.info/t/meaning-of-segment-status/10912/1
	via_status_length_max : constant positive := 8;
	package type_via_status is new generic_bounded_length (via_status_length_max);
	
	type type_via is new et_pcb.type_drill with record
		net_id			: type_net_id;
		diameter_total	: et_pcb_coordinates.type_distance; -- drill + 2 * restring
		layer_start		: type_signal_layer_id;
		layer_end		: type_signal_layer_id;		
		status			: type_via_status.bounded_string;  -- holds lock status and differential status
	end record;
	
	-- vias are stored in lists
	package type_vias is new doubly_linked_lists (type_via);

	-- The polygon hatch style does not have anything to do with real hatch fill patterns.
	-- See <https://forum.kicad.info/t/can-i-do-a-hatched-polygon-on-kicad/2761/13>. It is
	-- about the way the polygon is displayed in the GUI. Likewise, the associated decimal number
	-- (example "(hatch none 0.508)") tells the GUI how to display the polygon.
	type type_polygon_hatch is ( -- "outline style" in gui
		EDGE, -- "hatched" in gui
		NONE, -- "line" in gui
		FULL); -- "fully hatched" in gui

	-- CS: hatch_style and hatch_width are related to the display mode in the GUI.
	-- Currently there is no need to output this stuff:
	-- CS function to_string (hatch_style)
	-- CS function to_hatch_style (hatch_style)
	

	-- Polygons may be connected with THT pads only or all pad technologies
	subtype type_polygon_pad_technology is et_pcb.type_polygon_pad_technology range et_pcb.THT_ONLY .. et_pcb.SMT_AND_THT;

	type type_polygon is new et_pcb.type_copper_polygon with record
		net_name			: et_schematic.type_net_name.bounded_string; -- if name is empty, the polygon is not connected to any net
		net_id				: type_net_id; -- if id is 0, the polygon is not connected to any net
		layer				: type_signal_layer_id;
		timestamp			: et_kicad.type_timestamp;
		gui_hatch_style		: type_polygon_hatch := EDGE;
		gui_hatch_width		: et_pcb_coordinates.type_distance;	-- see spec for type_polygon_hatch. always 0.508. CS use subtype
		min_thickness		: et_pcb.type_signal_width;	-- minimum line width
		filled				: boolean; -- CS probably no need
		fill_mode_segment	: boolean := false; -- true on "segment mode", default -> false on "polygon mode"
		arc_segments		: natural := 0; -- CS subtype ? -- only 16 or 32 allowed
		thermal_gap			: et_pcb.type_polygon_thermal_gap := et_pcb.type_polygon_thermal_gap'first;
		thermal_width		: et_pcb.type_polygon_thermal_width := et_pcb.type_polygon_thermal_width'first; -- spoke width
		pad_technology		: type_polygon_pad_technology := type_polygon_pad_technology'last;
		pad_connection		: et_pcb.type_polygon_pad_connection := et_pcb.type_polygon_pad_connection'first;
	end record;

	package type_polygons is new doubly_linked_lists (type_polygon);
	
	-- This is the data type for the Kicad Board design:
	type type_board is record
		general		: type_general_board_info;
		setup		: type_board_setup; -- DRC stuff
		plot		: type_plot_setup; -- CAM job (there is only one)
		paper_size 	: et_general.type_paper_size;
		layers		: type_layers.map;
		netlist		: type_netlist.set;
		net_classes	: type_net_classes.map;
		packages	: type_packages_board.map;

		silk_screen	: et_pcb.type_silk_screen_pcb_both_sides;
		assy_doc	: et_pcb.type_assembly_documentation_pcb_both_sides;
		stencil		: et_pcb.type_stencil_both_sides;
		stop_mask	: et_pcb.type_stop_mask_both_sides;
		keepout		: et_pcb.type_keepout_both_sides;		
		contour		: et_pcb.type_pcb_contour;
		copper		: et_pcb.type_copper_pcb; -- non-electric. used for texts only
		-- other non-electric graphic objects in signal layers are not allowed in kicad

		-- CS objects in other layers (user defined, glue, ...)

		segments	: type_segments.list;
		vias		: type_vias.list;
		polygons	: type_polygons.list;
	end record;

	procedure read_board (
		file_name 		: in string;
		log_threshold	: in et_string_processing.type_log_level);


	procedure read_boards (log_threshold : in et_string_processing.type_log_level);
	-- Imports layout files. The files to be imported are named after the schematic modules.
	-- The schematic modules are indicated by module_cursor.

	function terminal_count (
		packge : in et_libraries.type_full_library_name.bounded_string) -- ../lbr/bel_ic/S_SO14
		return et_libraries.type_terminal_count;
	
	function terminal_port_map_fits (
	-- Used when terminal_port_maps are to be used for packages.
	-- The given package is specified by the library name and package name.
	-- Returns true if the terminal_port_map fits on the given package.
		library_name		: in et_libraries.type_full_library_name.bounded_string;		-- ../lbr/bel_ic.pretty
		package_name 		: in et_libraries.type_component_package_name.bounded_string;	-- S_SO14
		terminal_port_map	: in et_libraries.type_terminal_port_map.map) 
		return boolean;

-- 	procedure to_native (log_threshold : in et_string_processing.type_log_level);
-- 	-- Converts the packages (from package_libraries) to native packages.
-- 	-- NOTE: Packages of the board (incl. their deviations from the package_libraries) are ignored !

		
	
end et_kicad_pcb;

-- Soli Deo Gloria
