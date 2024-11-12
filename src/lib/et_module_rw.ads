------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        MODULE READ AND WRITE                             --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 



package et_module_rw is

	
-- SECTION NAMES

	section_net_classes			: constant string := "[NET_CLASSES";
	section_net_class			: constant string := "[NET_CLASS";
	
	section_nets				: constant string := "[NETS";
	section_net					: constant string := "[NET";

	section_strands				: constant string := "[STRANDS";
	section_strand				: constant string := "[STRAND";

	section_segments			: constant string := "[SEGMENTS";
	section_segment				: constant string := "[SEGMENT";

	section_labels				: constant string := "[LABELS";
	section_label				: constant string := "[LABEL";
	
	section_submodules			: constant string := "[SUBMODULES";
	section_submodule			: constant string := "[SUBMODULE";
	section_ports				: constant string := "[PORTS";
	section_port				: constant string := "[PORT";

	section_drawing_frames		: constant string := "[DRAWING_FRAMES";
	section_sheet_descriptions	: constant string := "[SHEET_DESCRIPTIONS";
	section_sheet				: constant string := "[SHEET";
	
	section_schematic			: constant string := "[SCHEMATIC";

	section_devices				: constant string := "[DEVICES";
	section_device				: constant string := "[DEVICE";
	section_devices_non_electric: constant string := "[DEVICES_NON_ELECTRIC";

	section_assembly_variants	: constant string := "[ASSEMBLY_VARIANTS";
	section_assembly_variant	: constant string := "[VARIANT";	
	
	section_netchangers			: constant string := "[NETCHANGERS";
	section_netchanger			: constant string := "[NETCHANGER";

	section_meta				: constant string := "[META";
	section_rules				: constant string := "[RULES";
	
	section_units				: constant string := "[UNITS";

	section_port_begin			: constant string := "[PORT";

	section_preferred_libraries	: constant string := "[PREFERRED_LIBRARIES";

	section_user_settings		: constant string := "[USER_SETTINGS";

	section_outline				: constant string	:= "[OUTLINE";
	
	type type_section is ( -- CS: sort aphabetically
		SEC_DEVICES_NON_ELECTRIC,
		SEC_BOARD_LAYER_STACK,
		SEC_CONTOURS, -- of fill and cutout zones
		SEC_CUTOUT_ZONE,
		SEC_DRAWING_GRID,
		SEC_ZONE,
		SEC_HOLE,
		SEC_INIT,
		SEC_NET_CLASSES,
		SEC_NET_CLASS,
		SEC_NETS,
		SEC_NET,
		SEC_STRANDS,
		SEC_STRAND,
		SEC_SEGMENTS,
		SEC_SEGMENT,
		SEC_LABELS,
		SEC_LABEL,
		SEC_FILL_ZONES_CONDUCTOR,
		--SEC_FILL_ZONES_NON_CONDUCTOR, -- CS
		SEC_PORTS,
		SEC_PORT,
		SEC_ROUTE,
		SEC_LINE,
		SEC_ARC,
		SEC_VIA,
		SEC_VIAS,
		SEC_SUBMODULES,
		SEC_SUBMODULE,
		SEC_SHEET_DESCRIPTIONS,
		SEC_SHEET,
		SEC_DRAWING_FRAMES,
		SEC_SCHEMATIC,
		SEC_BOARD,
		SEC_DEVICES,
		SEC_DEVICE,
		SEC_ASSEMBLY_VARIANTS,
		SEC_ASSEMBLY_VARIANT,
		SEC_NETCHANGERS,
		SEC_NETCHANGER,
		SEC_META,
		SEC_TEXTS,
		SEC_TEXT,
		SEC_UNITS,
		SEC_UNIT,
		SEC_OUTLINE,
		SEC_PACKAGE,
		SEC_PLACEHOLDER,
		SEC_PLACEHOLDERS,
		SEC_PREFERRED_LIBRARIES,
		SEC_SILK_SCREEN,
		SEC_CIRCLE,
		SEC_ASSEMBLY_DOCUMENTATION,
		SEC_KEEPOUT,
		SEC_ROUTE_RESTRICT,
		SEC_RULES,
		SEC_VIA_RESTRICT,
		SEC_STOP_MASK,
		SEC_STENCIL,
		SEC_CONDUCTOR,
		SEC_PCB_CONTOURS_NON_PLATED,
		-- CS SEC_PCB_CONTOUR_PLATED
		SEC_TOP,
		SEC_BOTTOM,
		SEC_USER_SETTINGS
		);

	
	-- Converts a section like SEC_NET to a string "net".
	function to_string (section : in type_section) return string;


	
end et_module_rw;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
