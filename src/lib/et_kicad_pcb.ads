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
--with ada.containers.vectors;
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

	drill_shape_oval	: constant string (1..4) := "oval"; -- used with slotted holes
	pad_drill_offset	: constant string (1..6) := "offset";
	type type_drill_shape is (CIRCULAR, SLOTTED);

	-- "Slotted drills" or "plated millings" for terminals are limited by drill sizes because
	-- the PCB manufacturer starts the milling with a drill.
	subtype type_pad_milling_size is et_pcb_coordinates.type_distance range et_pcb.drill_size_min .. et_pcb.drill_size_max;
	
	type type_fp_text_meaning is (REFERENCE, VALUE, USER);
	
	-- For the package import we need a special set of layers. 
	type type_layer_abbrevation is (
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



	
	-- PLOT PARAMETERS

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
	type type_plot_drill_shape is range 0..0; -- CS so far nothing more known.
	
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



	-- BOARD SETUP
	type type_zone_45_only is (NO, YES);

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
	-- NOTE: this has nothing to do with any kicad netlist file !
	net_id_max : constant positive := 1_000_000; -- one million nets should be sufficient
	type type_net_id is range 0..net_id_max; -- used in the "netlist" section
	type type_net_id_terminal is range 1..net_id_max; -- used with pads in module section

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
		layer	: type_layer_abbrevation;
		meaning	: type_fp_text_meaning;
	end record;



	procedure read_libraries (
	-- Reads package libraries. Root directory is et_libraries.lib_dir.
	-- The libraries in the container are named after the libraries found in lib_dir.
		log_threshold 	: in et_string_processing.type_log_level);



	
	-- For things in section layers like (0 F.Cu signal) or (31 B.Cu signal) we have those specs.
	-- This is board file related.
	layer_id_max : constant positive := 49;
	type type_layer_id is range 0..layer_id_max;
	layer_name_length_max : constant positive := 9;
	package type_layer_name is new generic_bounded_length (layer_name_length_max); -- B.Cu
	function to_layer_name (name : in string) return type_layer_name.bounded_string;
	type type_layer_meaning is (SIGNAL, USER);
	function to_layer_meaning (meaning : in string) return type_layer_meaning;
	
	type type_layer is record
		name	: type_layer_name.bounded_string;
		meaning	: type_layer_meaning;
	end record;

	package type_layers is new ordered_maps (
		key_type		=> type_layer_id,
		element_type	=> type_layer);




	
	-- Packages (modules) as they are listed in the board file are similar to
	-- packages in the libraries. However, there are differences, which requires
	-- a distinct type for them. 
	-- Differences are: 
	-- - no placeholders for reference and value (here the final component reference and value is)
	-- - x/y position and angle of the package 
	-- - pads with net names

	-- silk screen objects without text placeholders:
	type type_silk_screen_package_both_sides is record
		top		: et_pcb.type_silk_screen;
		bottom	: et_pcb.type_silk_screen;
	end record;

	-- assembly documentation objects without text placeholders:
	type type_assembly_documentation_package_both_sides is record
		top		: et_pcb.type_assembly_documentation;
		bottom	: et_pcb.type_assembly_documentation;
	end record;



	
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
	type type_package_board is new et_pcb.type_package with record
		silk_screen				: type_silk_screen_package_both_sides; -- without placeholders
		assembly_documentation	: type_assembly_documentation_package_both_sides; -- without placeholders
		terminals				: type_terminals.map; -- terminals with net names
		time_edit				: et_string_processing.type_timestamp;
		value					: et_libraries.type_component_value.bounded_string;
		position				: et_pcb_coordinates.type_package_position;
	end record;

	-- Lots of packages (in a board) can be collected in a map:
	package type_packages_board is new indefinite_ordered_maps (
		key_type 		=> et_libraries.type_component_reference, -- IC46
		element_type 	=> type_package_board,
		"<"				=> et_schematic.compare_reference
		);



	
	-- This is the data type for the Kicad Board design:
	type type_board is record
		setup		: type_board_setup;
		plot		: type_plot_setup;
		paper_size 	: et_general.type_paper_size;
		layers		: type_layers.map;
		netlist		: type_netlist.set;
		net_classes	: type_net_classes.map;
		packages	: type_packages_board.map;
	end record;

	procedure read_board (
		file_name 		: in string;
		log_threshold	: in et_string_processing.type_log_level);

	
end et_kicad_pcb;

-- Soli Deo Gloria
