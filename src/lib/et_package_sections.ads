------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           PACKAGE SECTIONS                               --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                -- 
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

--   do do:
-- 1. rename this package or merge with et_symbol_sections, et_device_sections
--    and et_module_sections
-- 2. clean up



package et_package_sections is -- CS rename to et_file_sections


	-- Prefixes before enumeration types prevent clashes with gnat keywords
	-- and package names:
	section_prefix : constant string := ("SEC_");

	
	type type_package_section is (  -- CS: sort aphabetically

		SEC_ARC,
		SEC_ASSEMBLY_DOCUMENTATION,
		SEC_ASSEMBLY_VARIANT,
		SEC_ASSEMBLY_VARIANTS,
		
		SEC_BOARD,
		SEC_BOARD_LAYER_STACK,
		SEC_BOTTOM,
		
		SEC_CIRCLE,
		SEC_CONDUCTOR,
		SEC_CONNECTORS,
		SEC_CONTOURS, -- of fill and cutout zones
		SEC_CUTOUT_ZONE,

		SEC_DEVICE,
		SEC_DEVICES,
		SEC_DEVICES_NON_ELECTRIC,
		SEC_DRAW,
		SEC_DRAWING_FRAMES,
		SEC_DRAWING_GRID,

		SEC_FILL_ZONES_CONDUCTOR,
		--SEC_FILL_ZONES_NON_CONDUCTOR, -- CS

		SEC_INIT,		

		SEC_HOLE,
		-- CS SEC_HOLES_PLATED ?

		SEC_JUNCTIONS,
		
		SEC_KEEPOUT,
		
		SEC_LABEL,
		SEC_LABELS,
		SEC_LINE,
		
		SEC_META,
		SEC_MILLINGS,
		
		SEC_NET,
		SEC_NETCHANGER,
		SEC_NETCHANGERS,
		SEC_NETS,
		SEC_NET_CLASS,
		SEC_NET_CLASSES,

		SEC_OUTLINE,

		SEC_PACKAGE,
		SEC_PACKAGE_3D_CONTOURS, -- CS not used
		SEC_PAD_CONTOURS_SMT,
		SEC_PAD_CONTOURS_THT,
		SEC_PCB_CONTOURS_NON_PLATED, -- CS rename to SEC_HOLES
		-- CS SEC_PCB_CONTOUR_PLATED
		SEC_PLACEHOLDER,
		SEC_PLACEHOLDERS,
		SEC_PORT,
		SEC_PORTS,
		SEC_PREFERRED_LIBRARIES,
		
		SEC_ROUTE,
		SEC_ROUTE_RESTRICT,
		SEC_RULES,
		
		SEC_SCHEMATIC,
		SEC_SEGMENT,
		SEC_SEGMENTS,
		SEC_SILKSCREEN,
		SEC_SHEET,
		SEC_SHEET_DESCRIPTIONS,
		SEC_STENCIL,
		SEC_STENCIL_CONTOURS,
		SEC_STOPMASK,
		SEC_STOPMASK_CONTOURS_SMT,
		SEC_STOPMASK_CONTOURS_THT,
		SEC_STRAND,
		SEC_STRANDS,
		SEC_SUBMODULE,
		SEC_SUBMODULES,
		SEC_SYMBOL,
	
		SEC_TERMINAL,
		SEC_TERMINALS,
		SEC_TERMINAL_PORT_MAP,
		SEC_TEXT,
		SEC_TEXTS,
		SEC_TOP,
		
		SEC_UNIT,
		SEC_UNITS,
		SEC_UNITS_EXTERNAL,
		SEC_UNITS_INTERNAL,
		SEC_USER_SETTINGS,
		
		SEC_VARIANT,
		SEC_VARIANTS,		
		SEC_VIA,
		SEC_VIAS,
		SEC_VIA_RESTRICT,
		
		SEC_ZONE
		);


	
	-- Converts a section like SEC_KEEPOUT to a string "keepout".
	function to_string (
		section : in type_package_section) 
		return string;
	

	-- CS use image of type_package_section to compose the
	-- strings below:

-------------------
-- SECTION HEADERS:

	section_begin					: constant string := "BEGIN]";	
	section_end						: constant string := "END]";

	------------------
	
	section_arc						: constant string := "[ARC";
	section_assembly_doc			: constant string := "[ASSEMBLY_DOCUMENTATION";
	section_assembly_variant		: constant string := "[VARIANT";	
	section_assembly_variants		: constant string := "[ASSEMBLY_VARIANTS";

	section_board					: constant string := "[BOARD";
	section_board_layer_stack		: constant string := "[BOARD_LAYER_STACK";
	section_bottom					: constant string := "[BOTTOM";

	section_circle					: constant string := "[CIRCLE";
	section_clearances				: constant string := "[CLEARANCES";
	section_conductor				: constant string := "[CONDUCTOR";
	section_connectors				: constant string := "[CONNECTORS";

	section_contours				: constant string := "[CONTOURS";
	section_cutout_zone				: constant string := "[CUTOUT_ZONE";

	
	section_device					: constant string := "[DEVICE";
	section_devices					: constant string := "[DEVICES";
	section_devices_non_electric	: constant string := "[DEVICES_NON_ELECTRIC";
	section_draw					: constant string := "[DRAW";
	section_drawing_frames			: constant string := "[DRAWING_FRAMES";
	section_drawing_grid			: constant string := "[DRAWING_GRID";
	
	section_fill_zones_conductor	: constant string := "[FILL_ZONES_CONDUCTOR";
	
	section_hole					: constant string := "[HOLE";

	section_junctions				: constant string := "[JUNCTIONS";
	
	section_keepout					: constant string := "[KEEPOUT";

	section_label					: constant string := "[LABEL";
	section_labels					: constant string := "[LABELS";
	section_line					: constant string := "[LINE";
	
	section_meta					: constant string := "[META";
	section_pad_millings			: constant string := "[MILLINGS";

	section_net						: constant string := "[NET";
	section_netchanger				: constant string := "[NETCHANGER";
	section_netchangers				: constant string := "[NETCHANGERS";
	section_nets					: constant string := "[NETS";
	section_net_class				: constant string := "[NET_CLASS";
	section_net_classes				: constant string := "[NET_CLASSES";
	
	section_outline					: constant string := "[OUTLINE";
	-- CS rename to OUTER_CONTOUR

	
	section_package					: constant string := "[PACKAGE";	
	section_pac_3d_contours			: constant string := "[PACKAGE_3D_CONTOURS";

	section_pcb_contours			: constant string := "[PCB_CONTOURS_NON_PLATED";
	-- CS rename to OUTLINE

	section_placeholder				: constant string := "[PLACEHOLDER";
	section_placeholders			: constant string := "[PLACEHOLDERS";	
	
	--section_pcb_contours_plated	: constant string := "[PCB_CONTOURS_PLATED"; 
	section_pad_contours_smt		: constant string := "[PAD_CONTOURS_SMT";
	section_pad_contours_tht		: constant string := "[PAD_CONTOURS_THT";	
	section_port					: constant string := "[PORT";
	section_ports					: constant string := "[PORTS";
	section_preferred_libraries		: constant string := "[PREFERRED_LIBRARIES";
	
	section_restring				: constant string := "[RESTRING";
	section_route					: constant string := "[ROUTE";	
	section_route_restrict			: constant string := "[ROUTE_RESTRICT";
	section_rules					: constant string := "[RULES";
	
	section_schematic				: constant string := "[SCHEMATIC";
	section_segment					: constant string := "[SEGMENT";
	section_segments				: constant string := "[SEGMENTS";
	section_sheet					: constant string := "[SHEET";
	section_sheet_descriptions		: constant string := "[SHEET_DESCRIPTIONS";
	section_silkscreen				: constant string := "[SILKSCREEN";
	section_sizes					: constant string := "[SIZES";
	section_stencil					: constant string := "[STENCIL";
	section_stencil_contours		: constant string := "[STENCIL_CONTOURS";
	section_stopmask				: constant string := "[STOPMASK";
	section_stopmask_contours_tht	: constant string := "[STOPMASK_CONTOURS_THT";
	section_stopmask_contours_smt	: constant string := "[STOPMASK_CONTOURS_SMT";
	section_strand					: constant string := "[STRAND";
	section_strands					: constant string := "[STRANDS";
	section_submodule				: constant string := "[SUBMODULE";
	section_submodules				: constant string := "[SUBMODULES";
	section_symbol					: constant string := "[SYMBOL";
	
	section_terminal				: constant string := "[TERMINAL";
	section_terminals				: constant string := "[TERMINALS";
	section_terminal_port_map		: constant string := "[TERMINAL_PORT_MAP";
	section_text					: constant string := "[TEXT";
	section_texts					: constant string := "[TEXTS";
	
	section_top						: constant string := "[TOP";

	section_unit					: constant string := "[UNIT";
	section_units					: constant string := "[UNITS";
	section_units_internal			: constant string := "[UNITS_INTERNAL";
	section_units_external			: constant string := "[UNITS_EXTERNAL";
	section_user_settings			: constant string := "[USER_SETTINGS";
	
	section_variant					: constant string := "[VARIANT";
	section_variants				: constant string := "[VARIANTS";
	section_via						: constant string := "[VIA";
	section_vias					: constant string := "[VIAS";
	section_via_restrict			: constant string := "[VIA_RESTRICT";

	section_zone					: constant string := "[ZONE";







	

-------------------
	
	function write_top_level_reached return string;
	function write_enter_section return string;
	function write_return_to_section return string;
	function write_missing_begin_end return string;
	function write_section_stack_not_empty return string;
	
	procedure invalid_section;


	


	generic
		max : positive;
		type item is private;
	package stack_lifo is -- CS rename to pac_sections_stack
		procedure push (x : in item);
		procedure pop;
		function pop return item;
		function depth return natural;
		procedure init;
		function empty return boolean;
		function current return item;
		function parent (degree : in natural := 1) return item;
		
	end stack_lifo;

	
end et_package_sections;
