------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           SECTION HEADERS                                --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                -- 
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

--   For correct displaying set tab with in your edtior to 4.

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


package et_section_headers is

	section_assembly_doc			: constant string := "[ASSEMBLY_DOCUMENTATION";
	section_assembly_variant		: constant string := "[VARIANT";	
	section_assembly_variants		: constant string := "[ASSEMBLY_VARIANTS";
	
	section_board					: constant string := "[BOARD";
	section_board_layer_stack		: constant string := "[BOARD_LAYER_STACK";
	section_bottom					: constant string := "[BOTTOM";

	section_clearances				: constant string := "[CLEARANCES";
	section_conductor				: constant string := "[CONDUCTOR";

	section_device					: constant string := "[DEVICE";
	section_devices					: constant string := "[DEVICES";
	section_devices_non_electric	: constant string := "[DEVICES_NON_ELECTRIC";
	section_drawing_frames			: constant string := "[DRAWING_FRAMES";
	
	section_fill_zones_conductor	: constant string := "[FILL_ZONES_CONDUCTOR";
	
	section_hole					: constant string := "[HOLE";
	
	section_keepout					: constant string := "[KEEPOUT";

	section_label					: constant string := "[LABEL";
	section_labels					: constant string := "[LABELS";
	
	section_meta					: constant string := "[META";
	section_pad_millings			: constant string := "[MILLINGS";

	section_net						: constant string := "[NET";
	section_netchanger				: constant string := "[NETCHANGER";
	section_netchangers				: constant string := "[NETCHANGERS";
	section_nets					: constant string := "[NETS";
	section_net_class				: constant string := "[NET_CLASS";
	section_net_classes				: constant string := "[NET_CLASSES";
	
	section_outline					: constant string := "[OUTLINE";
	
	section_pac_3d_contours			: constant string := "[PACKAGE_3D_CONTOURS";
	section_pcb_contours			: constant string := "[PCB_CONTOURS_NON_PLATED";
	--section_pcb_contours_plated	: constant string := "[PCB_CONTOURS_PLATED"; 
	section_pad_contours_smt		: constant string := "[PAD_CONTOURS_SMT";
	section_pad_contours_tht		: constant string := "[PAD_CONTOURS_THT";	
	section_port					: constant string := "[PORT";
	section_ports					: constant string := "[PORTS";
	section_preferred_libraries		: constant string := "[PREFERRED_LIBRARIES";
	
	section_restring				: constant string := "[RESTRING";
	section_route					: constant string := "[ROUTE";	
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
	
	section_terminal				: constant string := "[TERMINAL";
	section_terminals				: constant string := "[TERMINALS";
	section_top						: constant string := "[TOP";

	section_units					: constant string := "[UNITS";
	section_user_settings			: constant string := "[USER_SETTINGS";
	
	section_via						: constant string := "[VIA";
	section_vias					: constant string := "[VIAS";
	
end et_section_headers;
