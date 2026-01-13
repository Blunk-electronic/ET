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

package et_package_sections is -- CS rename to et_file_sections


	-- Prefixes before enumeration types prevent clashes with gnat keywords
	-- and package names:
	section_prefix : constant string := ("SEC_");

	
	type type_package_section is (  -- CS: sort aphabetically
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
		SEC_PCB_CONTOURS_NON_PLATED, -- CS rename to SEC_HOLES
		-- CS SEC_HOLES_PLATED ?
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


	
	-- Converts a section like SEC_KEEPOUT to a string "keepout".
	function to_string (
		section : in type_package_section) 
		return string;
	

	
	section_zone		: constant string := "[ZONE";
	section_cutout_zone	: constant string := "[CUTOUT_ZONE";
	section_contours	: constant string := "[CONTOURS";

	


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
