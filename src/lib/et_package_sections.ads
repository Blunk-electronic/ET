------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           PACKAGE SECTIONS                               --
--                                                                          --
--                               S p e c                                    --
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

with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.packages;	use et_device_placeholders.packages;
with et_package_appearance;				use et_package_appearance;
with et_packages;						use et_packages;
with et_fill_zones;						use et_fill_zones;
with et_route_restrict;					use et_route_restrict;
with et_via_restrict;					use et_via_restrict;
with et_pcb_rw.restrict;				use et_pcb_rw.restrict;
with et_package_names;					use et_package_names;


package et_package_sections is

	
	type type_section is (
		SEC_CONDUCTOR,
		SEC_CONTOURS, -- of fill and cutout zones
		SEC_CUTOUT_ZONE,
		SEC_INIT,
		SEC_TOP,
		SEC_BOTTOM,
		SEC_HOLE,
		SEC_LINE,
		SEC_ARC,
		SEC_CIRCLE,
		SEC_SILKSCREEN,
		SEC_ASSEMBLY_DOCUMENTATION,
		SEC_KEEPOUT,
		SEC_STOPMASK,
		SEC_STENCIL,
		SEC_ROUTE_RESTRICT,
		SEC_VIA_RESTRICT,
		SEC_PCB_CONTOURS_NON_PLATED,
		SEC_TERMINALS,
		SEC_TERMINAL,
		SEC_PAD_CONTOURS_SMT,
		SEC_PAD_CONTOURS_THT,
		SEC_STENCIL_CONTOURS,
		SEC_STOPMASK_CONTOURS_SMT,
		SEC_STOPMASK_CONTOURS_THT,
		SEC_MILLINGS,
		SEC_TEXT,
		SEC_PLACEHOLDER,
		SEC_ZONE,
		SEC_PACKAGE_3D_CONTOURS
		);

	
end et_package_sections;
